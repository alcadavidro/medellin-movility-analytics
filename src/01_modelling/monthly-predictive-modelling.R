library(dplyr)
library(tidyr)
library(lubridate)

# Data modelling
library(recipes)
library(Metrics)
library(glmnet)
library(caret)
library(MASS)

# Visualización
library(ggplot2)

# Formato tablas
library(kableExtra)

df_train <- read.csv('../../data/processed/train_data.csv')
df_test <- read.csv('../../data/processed/test_data.csv')
special_dates_train <- read.csv('../../data/processed/special_date_monthly_2017.csv')
special_dates_test <- read.csv('../../data/processed/special_date_monthly_2018.csv')


# dataset
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

# funciones
armado_dataset_mensual <- function(df, objective_var){
  # conjunto de datos mensual por clase
  accidentes_por_clase <- df %>% 
    group_by(PERIODO, MES, CLASE) %>% 
    summarise(n_accidentes=n(), .groups="drop") %>% 
    spread(CLASE, n_accidentes) %>%
    mutate(PERIODO_LEAD=lead(PERIODO), MES_LEAD=lead(MES), rank = 1:length(PERIODO)) %>%
    filter(rank < max(rank)) %>%
    dplyr::select(-c(PERIODO, MES, rank)) 
  
  # conjunto de datos mensual por gravedad
  accidentes_por_gravedad <- df %>% 
    group_by(PERIODO, MES, GRAVEDAD) %>%
    summarise(n_accidentes=n(), .groups="drop") %>% 
    spread(GRAVEDAD, n_accidentes) %>%
    mutate(PERIODO_LEAD=lead(PERIODO), MES_LEAD=lead(MES), rank = 1:length(PERIODO)) %>%
    filter(rank < max(rank)) %>%
    dplyr::select(-c(PERIODO, MES, rank))
  
  # rezagos de la clase
  rezagos_accidentes <- df %>% 
    filter(CLASE == objective_var) %>% 
    group_by(PERIODO, MES) %>%
    summarise(n_accidentes=n(), .groups="drop") %>%
    mutate(
      FECHA=as.Date(paste(PERIODO, formatC(MES, width = 2, flag = "0"), "01", sep='-')),
      t_minus_1=lag(n_accidentes, n = 1),
      t_minus_2=lag(n_accidentes, n = 2)
    ) %>% 
    dplyr::select(PERIODO, MES, t_minus_2) 
  
  # Accidentes en los domingos
  domingos <- df %>% 
    mutate(domingo = ifelse(DIA_NOMBRE == "domingo  ", 1, 0)) %>% 
    group_by(PERIODO, MES) %>% 
    summarise(accidentes_domingo=sum(domingo), .groups="drop") %>% 
    mutate(PERIODO_LEAD=lead(PERIODO), MES_LEAD=lead(MES), rank = 1:length(PERIODO)) %>%
    filter(rank < max(rank)) %>% 
    dplyr::select(-c(PERIODO, MES, rank))
  
  
  # fechas especiales
  fechas_meses <- special_dates %>% 
    group_by(PERIODO, MES) %>% 
    summarise(
      dias_festivos=sum(DIA_FESTIVO),
      dias_especiales=sum(FECHA_ESPECIAL),
      dias_festivos_especiales=sum(FESTIVO_FECHA_ESPECIAL),
      .groups="drop"
    )
  
  df_processed <- df %>%
    filter(CLASE == objective_var) %>%
    group_by(PERIODO, MES) %>% 
    summarise(
      numero_accidentes = n(),
      .groups="drop"
    ) %>% 
    mutate(
      MES=factor(MES)
    ) %>% 
    merge(accidentes_por_clase,
          by.x = c("PERIODO", "MES"), by.y = c("PERIODO_LEAD", "MES_LEAD"), all.x = T) %>%
    replace_na(list(incendio=0)) %>%
    merge(accidentes_por_gravedad, by.x = c("PERIODO", "MES"), by.y = c("PERIODO_LEAD", "MES_LEAD"), all.x = T) %>%
    merge(rezagos_accidentes, by = c("PERIODO", "MES"), all.x = T) %>%
    merge(domingos, by.x = c("PERIODO", "MES"), by.y = c("PERIODO_LEAD", "MES_LEAD"), all.x = T) %>% 
    filter(!is.na(t_minus_2)) %>% 
    merge(fechas_meses, by = c("PERIODO", "MES")) %>% 
    arrange(PERIODO, MES, .by_group = T)
  
  return(df_processed)
}

df_preprocessed <- armado_dataset_mensual(df=df, objective_var = "choque")
df_choque_train <- df_preprocessed %>% filter(PERIODO!=2018) %>% dplyr::select(-c(PERIODO))
df_choque_test <- df_preprocessed %>% filter(PERIODO==2018) %>% dplyr::select(-c(PERIODO))


# df_choque_train <- df_choque_train %>% 
#   mutate(
#     incendio=incendio+1,
#     dias_festivos=dias_festivos+1,
#     dias_especiales=dias_especiales+1,
#     dias_festivos_especiales=dias_festivos_especiales+1
#   )

model.choque.recipe <- recipe(numero_accidentes ~ ., 
                              data = df_choque_train)
model.choque.steps <- model.choque.recipe %>% 
  step_log(all_outcomes()) %>% 
  step_center(all_predictors(), -MES) %>% 
  step_scale(all_predictors(), -MES)

model.choque.prepared <- prep(model.choque.steps, training = df_choque_train)
df_choque_train <- bake(model.choque.prepared, df_choque_train)

df_choque_test <- df_choque_test %>% 
  mutate(
    incendio=incendio+1,
    dias_festivos=dias_festivos+1,
    dias_especiales=dias_especiales+1,
    dias_festivos_especiales=dias_festivos_especiales+1
  )
df_choque_test <- bake(model.choque.prepared, df_choque_test)


modelo.choque <- lm(numero_accidentes ~ ., data=df_choque_train)
step.model.choque <- stepAIC(modelo.choque, direction = "both", trace = F, steps = 2000)

model.choque.summary <- summary(step.model.choque)
model.choque.summary


plot_estimation_errors <- function(y_true, y_pred, title){
  min_value <- min(y_true, y_pred) - min(y_true, y_pred) * 0.1
  max_value <- max(y_true, y_pred) + max(y_true, y_pred) * 0.1
  
  plot(x=y_true,y=y_pred,
       ylab="Predicciones",xlab="Observados",
       xlim=c(min_value, max_value),ylim=c(min_value, max_value),
       las=1, cex=1, pch=16,
       main=title)
  abline(a=0,b=1,lwd=2,col="black", lty=2)
  R_vl<-cor(y_pred, y_true)
  R_vl<-format(R_vl, digits = 3, nsmall = 3)
  rmse_vl<- rmse(actual = y_true, predicted = y_pred)
  rmse_vl<-format(rmse_vl,digits = 3,nsmall = 2)
  grid()
  legend("topleft",legend=paste0(c("Correlación: ","RMSE: "),c(R_vl,rmse_vl)), bty="n")
}


y_pred <- exp(predict(step.model.choque, df_choque_train))
y_true <- exp(df_choque_train$numero_accidentes)

train_rmse <- rmse(y_true, y_pred)

plot_estimation_errors(
  y_true = y_true, 
  y_pred = y_pred, 
  title="Rendimiento de modelo mensual choque ~ Entrenamiento"
)


y_pred <- exp(predict(step.model.choque, df_choque_test))
y_true <- exp(df_choque_test$numero_accidentes)

test_rmse <- rmse(y_true, y_pred)

test_train_ratio <- round(((test_rmse / train_rmse) - 1)*100, 3)

plot_estimation_errors(
  y_true = y_true, 
  y_pred = y_pred, 
  title="Rendimiento de modelo mensual choque ~ Validación"
)


# Construcción del dataset
x_train <- model.matrix(numero_accidentes~. , df_choque_train)[,-1]
y_train <- df_choque_train$numero_accidentes

# malla de lambdas a probar
lambda_seq <- 10^seq(2, -2, by = -.1)

# modelado
set.seed(42)
cv.lasso.choque <- cv.glmnet(x_train, y_train,
                             alpha = 1, lambda = lambda_seq, 
                             nfolds = 5)

lasso.model.choque <- glmnet(x_train, y_train, alpha = 1, lambda = cv.lasso.choque$lambda.min)

y_pred <- predict(lasso.model.choque, x_train)

train_rmse <- rmse(exp(y_train), exp(y_pred))

plot_estimation_errors(
  y_true = exp(y_train), 
  y_pred = exp(y_pred), 
  title="Rendimiento de modelo mensual choque ~ Entrenamiento"
)


x_test <- model.matrix(numero_accidentes~. , df_choque_test)[,-1]
y_test <- df_choque_test$numero_accidentes
y_pred <- predict(lasso.model.choque, x_test)

test_rmse <- rmse(exp(y_test), exp(y_pred))

plot_estimation_errors(
  y_true = exp(y_test), 
  y_pred = exp(y_pred), 
  title="Rendimiento de modelo mensual choque ~ Validación"
)

test_train_ratio <- round((test_rmse / train_rmse -1 )*100, 3) 


prepare_data <- function(df, objective_var){
  # Data preparation
  df_preprocessed <- armado_dataset_mensual(df=df, objective_var = objective_var)
  # Data splitting
  df_tmp_train <- df_preprocessed %>% filter(PERIODO!=2018) %>% dplyr::select(-c(PERIODO))
  df_tmp_test <- df_preprocessed %>% filter(PERIODO==2018) %>% dplyr::select(-c(PERIODO))
  
  # Data preprocess
  df_tmp_train <- df_tmp_train %>% 
    mutate(
      incendio=incendio+1,
      dias_festivos=dias_festivos+1,
      dias_especiales=dias_especiales+1,
      dias_festivos_especiales=dias_festivos_especiales+1
    )
  # Data recipes
  model.tmp.recipe <- recipe(
    numero_accidentes ~ ., 
    data = df_tmp_train
  )
  model.tmp.steps <- model.tmp.recipe %>% 
    step_log(all_outcomes()) %>% 
    step_center(all_predictors(), -MES) %>% 
    step_scale(all_predictors(), -MES)
  
  model.tmp.prepared <- prep(model.tmp.steps, training = df_tmp_train)
  
  # Data pipeline
  df_tmp_train <- bake(model.tmp.prepared, df_tmp_train)
  
  # Applying pipeline to test set
  df_tmp_test <- df_tmp_test %>% 
    mutate(
      incendio=incendio+1,
      dias_festivos=dias_festivos+1,
      dias_especiales=dias_especiales+1,
      dias_festivos_especiales=dias_festivos_especiales+1
    )
  df_tmp_test <- bake(model.tmp.prepared, df_tmp_test)  
  
  response = list(
    "training"=df_tmp_train,
    "validation"=df_tmp_test,
    "model_recipe"=list(
      "recipe"=model.tmp.recipe,
      "steps"=model.tmp.steps,
      "prepared"=model.tmp.prepared
    )
  )
  return(response)
}

data.atropello <- prepare_data(df=df, objective_var = "atropello")
df_atropello_train <- data.atropello$training
df_atropello_test <- data.atropello$validation


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

lasso.atropello.results <- train_lasso_model(df_train = df_atropello_train, df_test = df_atropello_test)


plot_estimation_errors(
  y_true = exp(lasso.atropello.results$training$response), 
  y_pred = exp(lasso.atropello.results$training$predictions), 
  title="Rendimiento de modelo mensual atropellos ~ Entrenamiento"
)


plot_estimation_errors(
  y_true = exp(lasso.atropello.results$validation$response), 
  y_pred = exp(lasso.atropello.results$validation$predictions), 
  title="Rendimiento de modelo mensual atropellos ~ Validación"
)

test_train_ratio <- round((lasso.atropello.results$validation$rmse / lasso.atropello.results$training$rmse - 1)*100,3)


data.caida <- prepare_data(df=df, objective_var = "caida ocupante")
df_caida_train <- data.caida$training
df_caida_test <- data.caida$validation

lasso.caida.results <- train_lasso_model(df_train = df_caida_train, df_test = df_caida_test)


plot_estimation_errors(
  y_true = exp(lasso.caida.results$training$response), 
  y_pred = exp(lasso.caida.results$training$predictions), 
  title="Rendimiento de modelo mensual caida ocupante ~ Entrenamiento"
)



plot_estimation_errors(
  y_true = exp(lasso.caida.results$validation$response), 
  y_pred = exp(lasso.caida.results$validation$predictions), 
  title="Rendimiento de modelo mensual caida ocupante ~ Validación"
)

test_train_ratio <- round((lasso.caida.results$validation$rmse / lasso.caida.results$training$rmse - 1)*100,3)

data.volcamiento <- prepare_data(df=df, objective_var = "volcamiento")
df_volcamiento_train <- data.volcamiento$training
df_volcamiento_test <- data.volcamiento$validation

lasso.volcamiento.results <- train_lasso_model(df_train = df_volcamiento_train, df_test = df_volcamiento_test, min_lambda = -3)

plot_estimation_errors(
  y_true = exp(lasso.volcamiento.results$training$response), 
  y_pred = exp(lasso.volcamiento.results$training$predictions), 
  title="Rendimiento de modelo mensual volcamientos ~ Entrenamiento"
)

plot_estimation_errors(
  y_true = exp(lasso.volcamiento.results$validation$response), 
  y_pred = exp(lasso.volcamiento.results$validation$predictions), 
  title="Rendimiento de modelo mensual volcamiento ~ Validación"
)

test_train_ratio <- round((lasso.volcamiento.results$validation$rmse / lasso.volcamiento.results$training$rmse - 1)*100,3)


data.otros <- prepare_data(df=df, objective_var =  "otro")
df_otros_train <- data.otros$training
df_otros_test <- data.otros$validation

lasso.otros.results <- train_lasso_model(df_train = df_otros_train, df_test = df_otros_test, min_lambda = -3)

plot_estimation_errors(
  y_true = exp(lasso.otros.results$training$response), 
  y_pred = exp(lasso.otros.results$training$predictions), 
  title="Rendimiento de modelo mensual otros accidentes ~ Entrenamiento"
)


plot_estimation_errors(
  y_true = exp(lasso.otros.results$validation$response), 
  y_pred = exp(lasso.otros.results$validation$predictions), 
  title="Rendimiento de modelo mensual otros accidentes ~ Validación"
)

test_train_ratio <- round((lasso.otros.results$validation$rmse / lasso.otros.results$training$rmse - 1)*100,3)
