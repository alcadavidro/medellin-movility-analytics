# Manipulación de datos
library(dplyr)
library(tidyr)
library(lubridate)

# Modelado de datos
library(recipes)
library(Metrics)
library(glmnet)
library(caret)
library(MASS)

# Visualización
library(ggplot2)


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
) %>% distinct()

df <- merge(df, special_dates, by = c("PERIODO", "MES", "DIA"))


armado_dataset_diario <- function(df, objective_var){
  # conjunto de datos diario por clase
  accidentes_por_clase <- df %>% 
    group_by(PERIODO, MES, DIA, CLASE) %>% 
    summarise(n_accidentes=n(), .groups="drop") %>% 
    spread(CLASE, n_accidentes) %>%
    mutate(PERIODO_LEAD=lead(PERIODO), MES_LEAD=lead(MES), DIA_LEAD=lead(DIA), rank = 1:length(PERIODO)) %>%
    filter(rank < max(rank)) %>%
    dplyr::select(-c(PERIODO, MES, DIA, rank)) 
  
  # conjunto de datos dia por gravedad
  accidentes_por_gravedad <- df %>% 
    group_by(PERIODO, MES, DIA, GRAVEDAD) %>%
    summarise(n_accidentes=n(), .groups="drop") %>% 
    spread(GRAVEDAD, n_accidentes) %>%
    mutate(PERIODO_LEAD=lead(PERIODO), MES_LEAD=lead(MES), DIA_LEAD=lead(DIA), rank = 1:length(PERIODO)) %>%
    filter(rank < max(rank)) %>%
    dplyr::select(-c(PERIODO, MES, DIA, rank))
  
  accidentes_por_hora <- df %>% mutate(HORA=hour(FECHA)) %>%
    mutate(
      temprano=ifelse(HORA %in% c(1,2,3,4,5,6), 1, 0),
      temprano_trabajo=ifelse(HORA %in% c(7,8,9,10,11,12), 1,0),
      almuerzo=ifelse(HORA == 13, 1, 0),
      tarde=ifelse(HORA %in% c(14,15,16,17,18), 1, 0),
      noche=ifelse(HORA %in% c(19, 20, 21, 22, 23, 24), 1,0)
    ) %>% 
    group_by(PERIODO, MES, DIA) %>% 
    summarise(
      accidentes_temprano=sum(temprano),
      accidentes_temprano_trabajo=sum(temprano_trabajo),
      accidentes_almuerzo=sum(almuerzo),
      accidentes_tarde=sum(tarde),
      accidentes_noche=sum(noche),
      .groups="drop"
    ) %>% 
    mutate(PERIODO_LEAD=lead(PERIODO), MES_LEAD=lead(MES), DIA_LEAD=lead(DIA), rank = 1:length(PERIODO)) %>%
    filter(rank < max(rank)) %>%
    dplyr::select(-c(PERIODO, MES, DIA, rank))
  
  # rezagos de la clase
  rezagos_accidentes <- df %>% 
    filter(CLASE == objective_var) %>% 
    group_by(PERIODO, MES, DIA) %>%
    summarise(n_accidentes=n(), .groups="drop") %>%
    mutate(
      FECHA=as.Date(paste(PERIODO, formatC(MES, width = 2, flag = "0"), formatC(DIA, width = 2, flag = "0"), sep='-')),
      t_minus_1=lag(n_accidentes, n = 1),
      t_minus_2=lag(n_accidentes, n = 2),
      t_minus_3=lag(n_accidentes, n = 3),
      t_minus_4=lag(n_accidentes, n = 4),
      t_minus_5=lag(n_accidentes, n = 5),
      t_minus_6=lag(n_accidentes, n = 6),
    ) %>% 
    dplyr::select(PERIODO, MES, DIA, t_minus_2, t_minus_3, t_minus_4, t_minus_5, t_minus_6) 
  
  # Accidentes en los domingos
  domingos <- df %>% 
    mutate(domingo = ifelse(DIA_NOMBRE == "domingo  ", 1, 0)) %>% 
    group_by(PERIODO, MES, DIA) %>% 
    summarise(accidentes_domingo=sum(domingo), .groups="drop") %>% 
    mutate(PERIODO_LEAD=lead(PERIODO), MES_LEAD=lead(MES), DIA_LEAD=lead(DIA), rank = 1:length(PERIODO)) %>%
    filter(rank < max(rank)) %>% 
    dplyr::select(-c(PERIODO, MES, DIA, rank)) 
  
  # Union del df
  df_processed <- df %>%
    filter(CLASE == objective_var) %>%
    group_by(PERIODO, MES, DIA) %>% 
    summarise(
      numero_accidentes = n(),
      .groups="drop"
    ) %>% 
    mutate(
      DIA=factor(DIA),
      MES=factor(MES)
    ) %>%
    merge(accidentes_por_clase,
          by.x = c("PERIODO", "MES", "DIA"), by.y = c("PERIODO_LEAD", "MES_LEAD", "DIA_LEAD"), all.x = T) %>%
    merge(accidentes_por_gravedad, 
          by.x = c("PERIODO", "MES", "DIA"), by.y = c("PERIODO_LEAD", "MES_LEAD", "DIA_LEAD"), all.x = T) %>%
    merge(rezagos_accidentes,
          by.x = c("PERIODO", "MES", "DIA"), by.y = c("PERIODO", "MES", "DIA"), all.x = T) %>% 
    merge(domingos, 
          by.x = c("PERIODO", "MES", "DIA"), by.y = c("PERIODO_LEAD", "MES_LEAD", "DIA_LEAD"), all.x = T) %>%
    merge(special_dates,
          by = c("PERIODO", "MES", "DIA")) %>% 
    merge(accidentes_por_hora,
          by.x = c("PERIODO", "MES", "DIA"), by.y = c("PERIODO_LEAD", "MES_LEAD", "DIA_LEAD"), all.x = T) %>%
    mutate(
      DIA_FESTIVO=factor(DIA_FESTIVO),
      FECHA_ESPECIAL=factor(FECHA_ESPECIAL),
      FESTIVO_FECHA_ESPECIAL=factor(FESTIVO_FECHA_ESPECIAL)
    ) %>% 
    arrange(PERIODO, MES, DIA, .by_group = T) %>% 
    slice(-1:-6) %>% 
    # filter(!is.na(t_minus_6)) %>% 
    replace_na(list(incendio=0, muerto=0, volcamiento=0)) %>% 
    mutate(
      DIA_SEMANA = factor(wday(
        as_date(
          paste(
            PERIODO, formatC(MES, width = 2, flag = 0), formatC(DIA, width = 2, flag = 0), sep='-')
        )
      )
      )
    )
  
  return(df_processed)
}


## Modelo de choque

df_preprocessed <- armado_dataset_diario(df=df, objective_var = "choque")
df_choque_train <- df_preprocessed %>% filter(PERIODO!=2018) %>% dplyr::select(-c(PERIODO, DIA))
df_choque_test <- df_preprocessed %>% filter(PERIODO==2018) %>% dplyr::select(-c(PERIODO, DIA))

model.choque.recipe <- recipe(numero_accidentes ~ ., 
                              data = df_choque_train)
model.choque.steps <- model.choque.recipe %>% 
  step_log(all_outcomes()) %>%
  step_center(all_predictors(), -MES, -DIA_SEMANA, -DIA_FESTIVO, -FECHA_ESPECIAL, -FESTIVO_FECHA_ESPECIAL) %>% 
  step_scale(all_predictors(), -MES, -DIA_SEMANA, -DIA_FESTIVO, -FECHA_ESPECIAL, -FESTIVO_FECHA_ESPECIAL)

model.choque.prepared <- prep(model.choque.steps, training = df_choque_train)
df_choque_train <- bake(model.choque.prepared, df_choque_train)
df_choque_test <- bake(model.choque.prepared, df_choque_test)

modelo.choque <- lm(numero_accidentes ~ ., data=df_choque_train)
step.model.choque <- stepAIC(modelo.choque, direction = "both", trace = F, steps = 2000)

model.choque.summary <- summary(step.model.choque)
model.choque.summary

### predicciones

predicciones <- df_preprocessed %>% dplyr::select(PERIODO, MES, DIA)
predicciones$accidentes_choque <- exp(c(df_choque_train$numero_accidentes, df_choque_test$numero_accidentes))
predicciones$estimaciones_choque <- round(
  exp(
    predict(
      step.model.choque, rbind(df_choque_train, df_choque_test)
    )
  )
)

## Modelo de atropello

df_preprocessed <- armado_dataset_diario(df=df, objective_var = "atropello")
df_atropello_train <- df_preprocessed %>% filter(PERIODO!=2018) %>% dplyr::select(-c(PERIODO, DIA))
df_atropello_test <- df_preprocessed %>% filter(PERIODO==2018) %>% dplyr::select(-c(PERIODO, DIA))

model.atropello.recipe <- recipe(numero_accidentes ~ ., 
                                 data = df_atropello_train)
model.atropello.steps <- model.atropello.recipe %>% 
  step_log(all_outcomes()) %>%
  step_center(all_predictors(), -MES, -DIA_SEMANA, -DIA_FESTIVO, -FECHA_ESPECIAL, -FESTIVO_FECHA_ESPECIAL) %>% 
  step_scale(all_predictors(), -MES, -DIA_SEMANA, -DIA_FESTIVO, -FECHA_ESPECIAL, -FESTIVO_FECHA_ESPECIAL)

model.atropello.prepared <- prep(model.atropello.steps, training = df_atropello_train)
df_atropello_train <- bake(model.atropello.prepared, df_atropello_train)
df_atropello_test <- bake(model.atropello.prepared, df_atropello_test)

modelo.atropello <- lm(numero_accidentes ~ ., data=df_atropello_train)
step.model.atropello <- stepAIC(modelo.atropello, direction = "both", trace = F, steps = 2000)

model.atropello.summary <- summary(step.model.atropello)
model.atropello.summary

### predicciones

predicciones$accidentes_atropello <- exp(c(df_atropello_train$numero_accidentes, df_atropello_test$numero_accidentes))
predicciones$estimaciones_atropello <- round(
  exp(
    predict(
      step.model.atropello, rbind(df_atropello_train, df_atropello_test)
    )
  )
)

## Modelo de caida ocupante

df_preprocessed <- armado_dataset_diario(df=df, objective_var = "caida ocupante")
df_caida_ocupante_train <- df_preprocessed %>% filter(PERIODO!=2018) %>% dplyr::select(-c(PERIODO, DIA))
df_caida_ocupante_test <- df_preprocessed %>% filter(PERIODO==2018) %>% dplyr::select(-c(PERIODO, DIA))

model.caida_ocupante.recipe <- recipe(numero_accidentes ~ ., 
                                      data = df_caida_ocupante_train)
model.caida_ocupante.steps <- model.caida_ocupante.recipe %>% 
  step_log(all_outcomes()) %>%
  step_center(all_predictors(), -MES, -DIA_SEMANA, -DIA_FESTIVO, -FECHA_ESPECIAL, -FESTIVO_FECHA_ESPECIAL) %>% 
  step_scale(all_predictors(), -MES, -DIA_SEMANA, -DIA_FESTIVO, -FECHA_ESPECIAL, -FESTIVO_FECHA_ESPECIAL)

model.caida_ocupante.prepared <- prep(model.caida_ocupante.steps, training = df_caida_ocupante_train)
df_caida_ocupante_train <- bake(model.caida_ocupante.prepared, df_caida_ocupante_train)
df_caida_ocupante_test <- bake(model.caida_ocupante.prepared, df_caida_ocupante_test)

modelo.caida_ocupante <- lm(numero_accidentes ~ ., data=df_caida_ocupante_train)
step.model.caida_ocupante <- stepAIC(modelo.caida_ocupante, direction = "both", trace = F, steps = 2000)

model.caida_ocupante.summary <- summary(step.model.caida_ocupante)
model.caida_ocupante.summary

### predicciones
predicciones$accidentes_caida_ocupante <- exp(c(df_caida_ocupante_train$numero_accidentes, df_caida_ocupante_test$numero_accidentes))
predicciones$estimaciones_caida_ocupante <- round(
  exp(
    predict(
      step.model.caida_ocupante, rbind(df_caida_ocupante_train, df_caida_ocupante_test)
    )
  )
)


## Modelo de volcamiento

df_preprocessed <- armado_dataset_diario(df=df, objective_var = "volcamiento")
df_preprocessed <- predicciones %>% dplyr::select(PERIODO, MES, DIA) %>% merge(df_preprocessed, all.x=T)
df_volcamiento_train <- df_preprocessed %>% filter(PERIODO!=2018) %>% dplyr::select(-c(PERIODO, DIA))
df_volcamiento_test <- df_preprocessed %>% filter(PERIODO==2018) %>% dplyr::select(-c(PERIODO, DIA))

model.volcamiento.recipe <- recipe(numero_accidentes ~ ., 
                                   data = df_volcamiento_train)
model.volcamiento.steps <- model.volcamiento.recipe %>% 
  step_log(all_outcomes()) %>%
  step_center(all_predictors(), -MES, -DIA_SEMANA, -DIA_FESTIVO, -FECHA_ESPECIAL, -FESTIVO_FECHA_ESPECIAL) %>% 
  step_scale(all_predictors(), -MES, -DIA_SEMANA, -DIA_FESTIVO, -FECHA_ESPECIAL, -FESTIVO_FECHA_ESPECIAL)

model.volcamiento.prepared <- prep(model.volcamiento.steps, training = df_volcamiento_train)
df_volcamiento_train <- bake(model.volcamiento.prepared, df_volcamiento_train)
df_volcamiento_test <- bake(model.volcamiento.prepared, df_volcamiento_test)

modelo.volcamiento <- lm(numero_accidentes ~ ., data=df_volcamiento_train)
step.model.volcamiento <- stepAIC(modelo.volcamiento, direction = "both", trace = F, steps = 2000)

model.volcamiento.summary <- summary(step.model.volcamiento)
model.volcamiento.summary



### predicciones
predicciones$accidentes_volcamiento <- exp(c(df_volcamiento_train$numero_accidentes, df_volcamiento_test$numero_accidentes))
predicciones$estimaciones_volcamiento <- round(
  exp(
    predict(
      step.model.volcamiento, rbind(df_volcamiento_train, df_volcamiento_test)
    )
  )
)

predicciones <- predicciones %>%
  replace_na(list(accidentes_volcamiento=0, estimaciones_volcamiento=0)) 

## Modelo otro
df_preprocessed <- armado_dataset_diario(df=df, objective_var = "otro")
df_otro_train <- df_preprocessed %>% filter(PERIODO!=2018) %>% dplyr::select(-c(PERIODO, DIA))
df_otro_test <- df_preprocessed %>% filter(PERIODO==2018) %>% dplyr::select(-c(PERIODO, DIA))

model.otro.recipe <- recipe(numero_accidentes ~ ., 
                            data = df_otro_train)
model.otro.steps <- model.otro.recipe %>% 
  step_log(all_outcomes()) %>%
  step_center(all_predictors(), -MES, -DIA_SEMANA, -DIA_FESTIVO, -FECHA_ESPECIAL, -FESTIVO_FECHA_ESPECIAL) %>% 
  step_scale(all_predictors(), -MES, -DIA_SEMANA, -DIA_FESTIVO, -FECHA_ESPECIAL, -FESTIVO_FECHA_ESPECIAL)

model.otro.prepared <- prep(model.otro.steps, training = df_otro_train)
df_otro_train <- bake(model.otro.prepared, df_otro_train)
df_otro_test <- bake(model.otro.prepared, df_otro_test)

modelo.otro <- lm(numero_accidentes ~ ., data=df_otro_train)
step.model.otro <- stepAIC(modelo.otro, direction = "both", trace = F, steps = 2000)

model.otro.summary <- summary(step.model.otro)
model.otro.summary

### predicciones
predicciones$accidentes_otro <- exp(c(df_otro_train$numero_accidentes, df_otro_test$numero_accidentes))
predicciones$estimaciones_otro <- round(
  exp(
    predict(
      step.model.otro, rbind(df_otro_train, df_otro_test)
    )
  )
)

write.csv(predicciones, file = "./src/03_app/data_app/predicciones_diarias.csv", row.names = F)
