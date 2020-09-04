# Manipulación de datos
library(dplyr)
library(tidyr)
library(lubridate)

# Data modelling
library(recipes)
library(Metrics)
library(glmnet)
library(caret)
library(MASS)
library(rpart)
library(rpart.plot)
library(e1071)

# Visualización
library(ggplot2)

# lectura de datos
df_train <- read.csv('./data/processed/train_data.csv')
df_test <- read.csv('./data/processed/test_data.csv')
special_dates_train <- read.csv('./data/processed/special_date_monthly_2017.csv')
special_dates_test <- read.csv('./data/processed/special_date_monthly_2018.csv')

df <- rbind(
  df_train,
  df_test
)
special_dates_train <- special_dates_train %>% 
  dplyr::select(PERIODO, MES, DIA, DIA_FESTIVO, FECHA_ESPECIAL, FESTIVO_FECHA_ESPECIAL)
special_dates_test <- special_dates_test %>% 
  dplyr::select(PERIODO, MES, DIA, DIA_FESTIVO, FECHA_ESPECIAL, FESTIVO_FECHA_ESPECIAL)

special_dates <- rbind(
  special_dates_train,
  special_dates_test
)

df <- merge(df, special_dates, by = c("PERIODO", "MES", "DIA"))

df <- df %>%
  mutate(SEMANA=week(FECHA))
special_dates <- special_dates %>% 
  mutate(
    FECHA = as_date(paste(
      PERIODO, 
      formatC(MES, width = 2, flag = 0),
      formatC(DIA, width = 2, flag = 0),
      sep='-'
    ))
  ) %>% 
  mutate(
    SEMANA=week(FECHA)
  )

armado_dataset_semanal <- function(df, objective_var){
  # conjunto de datos semanal por clase
  accidentes_por_clase <- df %>% 
    group_by(PERIODO, SEMANA, CLASE) %>% 
    summarise(n_accidentes=n(), .groups="drop") %>% 
    spread(CLASE, n_accidentes) %>%
    mutate(PERIODO_LEAD=lead(PERIODO), SEMANA_LEAD=lead(SEMANA), rank = 1:length(PERIODO)) %>%
    filter(rank < max(rank)) %>%
    dplyr::select(-c(PERIODO, SEMANA, rank)) 
  
  # conjunto de datos semanal por gravedad
  accidentes_por_gravedad <- df %>% 
    group_by(PERIODO, SEMANA, GRAVEDAD) %>%
    summarise(n_accidentes=n(), .groups="drop") %>% 
    spread(GRAVEDAD, n_accidentes) %>%
    mutate(PERIODO_LEAD=lead(PERIODO), SEMANA_LEAD=lead(SEMANA), rank = 1:length(PERIODO)) %>%
    filter(rank < max(rank)) %>%
    dplyr::select(-c(PERIODO, SEMANA, rank))
  
  # rezagos de la clase
  rezagos_accidentes <- df %>% 
    filter(CLASE == objective_var) %>% 
    group_by(PERIODO, SEMANA) %>%
    summarise(n_accidentes=n(), .groups="drop") %>%
    mutate(
      t_minus_1=lag(n_accidentes, n = 1),
      t_minus_2=lag(n_accidentes, n = 2),
      t_minus_3=lag(n_accidentes, n = 3)
    ) %>% 
    dplyr::select(PERIODO, SEMANA, t_minus_2, t_minus_3) 
  
  # Accidentes en los domingos
  domingos <- df %>% 
    mutate(domingo = ifelse(DIA_NOMBRE == "domingo  ", 1, 0)) %>% 
    group_by(PERIODO, SEMANA) %>% 
    summarise(accidentes_domingo=sum(domingo), .groups="drop") %>% 
    mutate(PERIODO_LEAD=lead(PERIODO), SEMANA_LEAD=lead(SEMANA), rank = 1:length(PERIODO)) %>%
    filter(rank < max(rank)) %>% 
    dplyr::select(-c(PERIODO, SEMANA, rank))
  
  
  # fechas especiales
  fechas_especiales <- special_dates %>% 
    group_by(PERIODO, SEMANA) %>% 
    summarise(
      dias_festivos=sum(DIA_FESTIVO),
      dias_especiales=sum(FECHA_ESPECIAL),
      dias_festivos_especiales=sum(FESTIVO_FECHA_ESPECIAL),
      .groups="drop"
    )
  
  df_processed <- df %>%
    filter(CLASE == objective_var) %>%
    group_by(PERIODO, SEMANA) %>% 
    summarise(
      numero_accidentes = n(),
      .groups="drop"
    ) %>% 
    mutate(
      SEMANA=factor(SEMANA)
    ) %>% 
    merge(accidentes_por_clase,
          by.x = c("PERIODO", "SEMANA"), by.y = c("PERIODO_LEAD", "SEMANA_LEAD"), all.x = T) %>%
    merge(accidentes_por_gravedad, by.x = c("PERIODO", "SEMANA"), by.y = c("PERIODO_LEAD", "SEMANA_LEAD"), all.x = T) %>%
    merge(rezagos_accidentes, by = c("PERIODO", "SEMANA"), all.x = T) %>%
    merge(domingos, by.x = c("PERIODO", "SEMANA"), by.y = c("PERIODO_LEAD", "SEMANA_LEAD"), all.x = T) %>% 
    replace_na(list(incendio=0, muerto=0)) %>%
    filter(!is.na(t_minus_3)) %>%
    merge(fechas_especiales, by = c("PERIODO", "SEMANA")) %>% 
    arrange(PERIODO, SEMANA, .by_group = T)
  
  return(df_processed)
}

df_preprocessed <- armado_dataset_semanal(df=df, objective_var = "choque")
df_choque_train <- df_preprocessed %>% filter(PERIODO!=2018) %>% dplyr::select(-c(PERIODO))
df_choque_test <- df_preprocessed %>% filter(PERIODO==2018) %>% dplyr::select(-c(PERIODO))

model.choque.recipe <- recipe(numero_accidentes ~ ., 
                              data = df_choque_train)
model.choque.steps <- model.choque.recipe %>% 
  step_log(all_outcomes()) %>% 
  step_center(all_predictors(), -SEMANA) %>% 
  step_scale(all_predictors(), -SEMANA)

model.choque.prepared <- prep(model.choque.steps, training = df_choque_train)
df_choque_train <- bake(model.choque.prepared, df_choque_train)
df_choque_test <- bake(model.choque.prepared, df_choque_test)

modelo.choque <- lm(numero_accidentes ~ ., data=df_choque_train)
step.model.choque <- stepAIC(modelo.choque, direction = "both", trace = F, steps = 2000)

model.choque.summary <- summary(step.model.choque)
model.choque.summary

train_lasso_model <- function(df_train, df_test, min_lambda=-2){
  # Construcción del dataset
  x_train <- model.matrix(numero_accidentes~. , df_train)[,-1]
  y_train <- df_train$numero_accidentes
  
  # malla de lambdas a probar
  lambda_seq <- 10^seq(2, min_lambda, by = -.1)
  
  # modelado
  set.seed(42)
  cv.lasso <- cv.glmnet(x_train, y_train,
                        alpha = 1,
                        lambda = lambda_seq, 
                        nfolds = 5)
  
  lasso.model <- glmnet(x_train, y_train, alpha = 1, lambda = cv.lasso$lambda.min)  
  
  y_train_pred <- predict(lasso.model, x_train)
  train_rmse <- rmse(exp(y_train), exp(y_train_pred))
  
  
  x_test <- model.matrix(numero_accidentes~. , df_test)[,-1]
  y_test <- df_test$numero_accidentes
  y_test_pred <- predict(lasso.model, x_test)
  
  test_rmse <- rmse(exp(y_test), exp(y_test_pred))
  
  response=list(
    "training"=list(
      "data"=x_train,
      "response"=y_train,
      "predictions"=y_train_pred,
      "rmse"=train_rmse
    ),
    "validation"=list(
      "data"=x_test,
      "response"=y_test,
      "predictions"=y_test_pred,
      "rmse"=test_rmse
    ),
    "model"=lasso.model
  )
  return(response)
}


lasso.choque.results <- train_lasso_model(df_train = df_choque_train, df_test = df_choque_test)
calendario <- df %>%
  mutate(SEMANA=week(FECHA)) %>%
  dplyr::select(PERIODO, MES,SEMANA) %>%
  distinct()


# Predicciones
predicciones <- df_preprocessed %>%
  dplyr::select(PERIODO, SEMANA)

choque.predicciones <- predict(
  lasso.choque.results$model, 
  rbind(lasso.choque.results$training$data, lasso.choque.results$validation$data)
)

predicciones$accidentes_choque <- exp(
  c(
    lasso.choque.results$training$response,
    lasso.choque.results$validation$response)
)
predicciones$estimaciones_choque <- round(exp(choque.predicciones[,c(1)]))

## clase atropello

df_preprocessed <- armado_dataset_semanal(df=df, objective_var = "atropello")
df_atropello_train <- df_preprocessed %>% filter(PERIODO!=2018) %>% dplyr::select(-c(PERIODO))
df_atropello_test <- df_preprocessed %>% filter(PERIODO==2018) %>% dplyr::select(-c(PERIODO))

model.atropello.recipe <- recipe(numero_accidentes ~ ., 
                                 data = df_atropello_train)
model.atropello.steps <- model.atropello.recipe %>% 
  step_log(all_outcomes()) %>% 
  step_center(all_predictors(), -SEMANA) %>% 
  step_scale(all_predictors(), -SEMANA)

model.atropello.prepared <- prep(model.atropello.steps, training = df_atropello_train)
df_atropello_train <- bake(model.atropello.prepared, df_atropello_train)
df_atropello_test <- bake(model.atropello.prepared, df_atropello_test)

lasso.atropello.results <- train_lasso_model(df_train = df_atropello_train, df_test = df_atropello_test, min_lambda = -3)

### predicciones
atropello.predicciones <- predict(
  lasso.atropello.results$model, 
  rbind(lasso.atropello.results$training$data, lasso.atropello.results$validation$data)
)

predicciones$accidentes_atropello <- exp(
  c(
    lasso.atropello.results$training$response,
    lasso.atropello.results$validation$response)
)
predicciones$estimaciones_atropello <- round(exp(atropello.predicciones[,c(1)]))

## Caida ocupante

df_preprocessed <- armado_dataset_semanal(df=df, objective_var = "caida ocupante")
df_caida_ocupante_train <- df_preprocessed %>% filter(PERIODO!=2018) %>% dplyr::select(-c(PERIODO))
df_caida_ocupante_test <- df_preprocessed %>% filter(PERIODO==2018) %>% dplyr::select(-c(PERIODO))

model.caida_ocupante.recipe <- recipe(numero_accidentes ~ ., 
                                      data = df_caida_ocupante_train)
model.caida_ocupante.steps <- model.caida_ocupante.recipe %>% 
  step_log(all_outcomes()) %>% 
  step_center(all_predictors(), -SEMANA) %>% 
  step_scale(all_predictors(), -SEMANA)

model.caida_ocupante.prepared <- prep(model.caida_ocupante.steps, training = df_caida_ocupante_train)
df_caida_ocupante_train <- bake(model.caida_ocupante.prepared, df_caida_ocupante_train)
df_caida_ocupante_test <- bake(model.caida_ocupante.prepared, df_caida_ocupante_test)

lasso.caida_ocupante.results <- train_lasso_model(df_train = df_caida_ocupante_train, df_test = df_caida_ocupante_test, min_lambda = -3)

### predicciones
caida_ocupante.predicciones <- predict(
  lasso.caida_ocupante.results$model, 
  rbind(lasso.caida_ocupante.results$training$data, lasso.caida_ocupante.results$validation$data)
)

predicciones$accidentes_caida_ocupante <- exp(
  c(
    lasso.caida_ocupante.results$training$response,
    lasso.caida_ocupante.results$validation$response)
)
predicciones$estimaciones_caida_ocupante <- round(exp(caida_ocupante.predicciones[,c(1)]))

## Volcamiento
df_preprocessed <- armado_dataset_semanal(df=df, objective_var = "volcamiento")
df_volcamiento_train <- df_preprocessed %>% filter(PERIODO!=2018) %>% dplyr::select(-c(PERIODO))
df_volcamiento_test <- df_preprocessed %>% filter(PERIODO==2018) %>% dplyr::select(-c(PERIODO))

model.volcamiento.recipe <- recipe(numero_accidentes ~ ., 
                                   data = df_volcamiento_train)
model.volcamiento.steps <- model.volcamiento.recipe %>% 
  step_log(all_outcomes()) %>% 
  step_center(all_predictors(), -SEMANA) %>% 
  step_scale(all_predictors(), -SEMANA)

model.volcamiento.prepared <- prep(model.volcamiento.steps, training = df_volcamiento_train)
df_volcamiento_train <- bake(model.volcamiento.prepared, df_volcamiento_train)
df_volcamiento_test <- bake(model.volcamiento.prepared, df_volcamiento_test)

lasso.volcamiento.results <- train_lasso_model(df_train = df_volcamiento_train, df_test = df_volcamiento_test, min_lambda = -2)

### predicciones
volcamiento.predicciones <- predict(
  lasso.volcamiento.results$model, 
  rbind(lasso.volcamiento.results$training$data, lasso.volcamiento.results$validation$data)
)

predicciones$accidentes_volcamiento <- exp(
  c(
    lasso.volcamiento.results$training$response,
    lasso.volcamiento.results$validation$response)
)
predicciones$estimaciones_volcamiento <- round(exp(volcamiento.predicciones[,c(1)]))

## Clase otros
df_preprocessed <- armado_dataset_semanal(df=df, objective_var = "otro")
df_otro_train <- df_preprocessed %>% filter(PERIODO!=2018) %>% dplyr::select(-c(PERIODO))
df_otro_test <- df_preprocessed %>% filter(PERIODO==2018) %>% dplyr::select(-c(PERIODO))

model.otro.recipe <- recipe(numero_accidentes ~ ., 
                            data = df_otro_train)
model.otro.steps <- model.otro.recipe %>% 
  step_log(all_outcomes()) %>% 
  step_center(all_predictors(), -SEMANA) %>% 
  step_scale(all_predictors(), -SEMANA)

model.otro.prepared <- prep(model.otro.steps, training = df_otro_train)
df_otro_train <- bake(model.otro.prepared, df_otro_train)
df_otro_test <- bake(model.otro.prepared, df_otro_test)

lasso.otro.results <- train_lasso_model(df_train = df_otro_train, df_test = df_otro_test, min_lambda = -2)

### predicciones
otro.predicciones <- predict(
  lasso.otro.results$model, 
  rbind(lasso.otro.results$training$data, lasso.otro.results$validation$data)
)

predicciones$accidentes_otro <- exp(
  c(
    lasso.otro.results$training$response,
    lasso.otro.results$validation$response)
)
predicciones$estimaciones_otro <- round(exp(otro.predicciones[,c(1)]))


write.csv(predicciones, file = "./src/03_app/data_app/predicciones_semanales.csv", row.names = F)

