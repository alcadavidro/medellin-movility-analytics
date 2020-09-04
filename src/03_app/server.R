library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(leaflet)
library(readr)
library(tidyr)
library(DT)

# Cargado de datos


df <- rbind(
  read.csv('./data_app/processed/train_data.csv'),
  read.csv('./data_app/processed/test_data.csv')
)

# --------------- funciones para capa de visualización -----------------------
graph_number_classes <- function(start_date, end_date, nivel="mes"){
  
  start_date = as_date(start_date)
  end_date = as_date(end_date)
  
  if (nivel == "periodo"){
    tmp_df <- df %>%
      filter(FECHA>=start_date, FECHA<=end_date) %>% 
      group_by(PERIODO) %>% 
      summarise(total=n(), .groups="drop")
    
    ggplot(tmp_df, aes(x=factor(PERIODO), y=total)) +
      geom_bar(stat="identity", fill="steelblue")+
      theme_classic() + 
      ggtitle("Accidentes por año") + 
      labs(y="Número accidentes", x="Año") + 
      theme(plot.title = element_text(hjust = 0.5), 
            axis.text.x = element_text(size=12),
            axis.title.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title.y = element_text(size=12))
    
  } else if (nivel == "mes"){
    tmp_df <- df %>%
      filter(FECHA>=start_date, FECHA<=end_date) %>% 
      group_by(PERIODO, MES) %>% 
      summarise(total=n(), .groups="drop")
    
    ggplot(tmp_df) +
      geom_line(aes(x=MES, y=total, colour=factor(PERIODO))) + 
      scale_x_discrete(name ="Mes", limits=factor(1:12)) + 
      theme_classic() + 
      ggtitle("Accidentes por año") + 
      labs(colour="Año", y="Número accidentes", title="Número de accidentes por año y mes") + 
      theme(plot.title = element_text(hjust = 0.5), 
            axis.text.x = element_text(size=12),
            axis.title.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title.y = element_text(size=12))
  } else {
    tmp_df <- df %>%
      filter(FECHA>=start_date, FECHA<=end_date) %>% 
      group_by(MES, DIA_NOMBRE) %>% 
      summarise(total=n(), .groups="drop")
    
    ggplot(tmp_df) +
      geom_bar(aes(x=DIA_NOMBRE, y=total, group=factor(MES), color=factor(MES)),
               stat = "identity") + 
      facet_wrap(~MES) +
      labs(x='Periodo del accidente',
           title = "Comportamiento de accidentes por mes y día de la semana",
           color="Mes") + 
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 45, vjust = 0.7))
  }
}

plot_map <- function(year=2014){
  df %>%
    filter(PERIODO==year) %>%
    mutate(lng=LONGITUD, lat=LATITUD) %>% 
    dplyr::select(lng, lat) %>% 
    leaflet() %>%
    addTiles() %>%
    addMarkers(
      clusterOptions = markerClusterOptions(),
    )
}

plot_by_cat_var <- function(start_date, end_date, cat_var){
  var <- enquo(cat_var)
  tmp_df <- df %>% 
    filter(FECHA >= start_date, FECHA <= end_date) %>% 
    group_by(!! var) %>% 
    summarise(
      total=n(),
      .groups="drop"
    )
  ggplot(tmp_df) +
    geom_bar(aes_(x=var, y=~total, fill=var), stat = "identity") + 
    labs(y='Numero de accidentes', 
         title = paste('Frecuencia por', quo_name(var), sep=" ")) + 
    theme_classic() +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_text(size=12, vjust = 0.7, angle = 90),
          axis.text.y = element_text(size=12),
          plot.title = element_text(hjust = 0.5))
  
}

plot_heat_map <- function(start_date, end_date, x, y){
  x_var = enquo(x)
  y_var = enquo(y)
  df %>%
    filter(FECHA >= start_date, FECHA <= end_date) %>% 
    group_by(!! x_var, !! y_var) %>%
    summarise(
      n_accidentes=n(),
      .groups="drop"
    ) %>% 
    ggplot(aes_(x_var, y_var, fill= ~n_accidentes)) +
    geom_tile() + 
    scale_fill_gradient() + 
    theme_classic() + 
    labs(fill="accidentes") +
    theme(axis.text.x = element_text(size=12, vjust = 0.7, angle = 90),
          axis.text.y = element_text(size=12))
}


# --------------- funciones para capa de predicción -----------------------

predicciones_mensuales <- read_csv("./data_app/predicciones_mensuales.csv")
predicciones_mensuales <- predicciones_mensuales %>% 
  mutate(FECHA=as_date(FECHA))
predicciones_semanales <- read_csv("./data_app/predicciones_semanales.csv")

predicciones_diarias <- read_csv("./data_app/predicciones_diarias.csv")
predicciones_diarias <- predicciones_diarias %>%
  mutate(FECHA=paste(PERIODO, formatC(MES, width = 2, flag = 0), formatC(DIA, width = 2, flag = ), sep='-')) %>% 
  dplyr::select(-c(PERIODO, DIA, MES)) %>% 
  mutate(FECHA=as_date(FECHA))

plot_weekly_predictions <- function(start_date, end_date, objective_var){
  start_date <- as_date(start_date)
  end_date <- as_date(end_date)
  if (objective_var == "choque"){
    predicciones_semanales %>% 
      filter(
        PERIODO>=year(start_date),
        PERIODO<=year(end_date),
        SEMANA>=week(start_date),
        SEMANA<=week(end_date)
      ) %>% 
      dplyr::select(PERIODO, SEMANA, estimaciones_choque, accidentes_choque) %>% 
      gather(key="variable", value="value", -c(SEMANA,PERIODO)) %>% 
      ggplot(aes(x=SEMANA, y=value)) +
      geom_line(aes(colour=factor(PERIODO), linetype=factor(variable))) + 
      theme_classic() + 
      ggtitle("Accidentes por mensuales de choque") + 
      labs(y="Número accidentes", x="Semana", colour="PERIODO", linetype="tipo de estimación") + 
      theme(plot.title = element_text(hjust = 0.5), 
            axis.text.x = element_text(size=12),
            axis.title.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title.y = element_text(size=12))
      
  }else if (objective_var == "atropello"){
    predicciones_semanales %>% 
      filter(
        PERIODO>=year(start_date),
        PERIODO<=year(end_date),
        SEMANA>=week(start_date),
        SEMANA<=week(end_date)
      ) %>% 
      dplyr::select(PERIODO, SEMANA, estimaciones_atropello, accidentes_atropello) %>% 
      gather(key="variable", value="value", -c(SEMANA,PERIODO)) %>% 
      ggplot(aes(x=SEMANA, y=value)) +
      geom_line(aes(colour=factor(PERIODO), linetype=factor(variable))) + 
      theme_classic() + 
      ggtitle("Accidentes por mensuales de atropello") + 
      labs(y="Número accidentes", x="Semana", colour="PERIODO", linetype="tipo de estimación") + 
      theme(plot.title = element_text(hjust = 0.5), 
            axis.text.x = element_text(size=12),
            axis.title.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title.y = element_text(size=12))
  }else if (objective_var == "caida ocupante"){
    predicciones_semanales %>% 
      filter(
        PERIODO>=year(start_date),
        PERIODO<=year(end_date),
        SEMANA>=week(start_date),
        SEMANA<=week(end_date)
      ) %>% 
      dplyr::select(PERIODO, SEMANA, estimaciones_caida_ocupante, accidentes_caida_ocupante) %>% 
      gather(key="variable", value="value", -c(SEMANA,PERIODO)) %>% 
      ggplot(aes(x=SEMANA, y=value)) +
      geom_line(aes(colour=factor(PERIODO), linetype=factor(variable))) + 
      theme_classic() + 
      ggtitle("Accidentes por mensuales de caida ocupante") + 
      labs(y="Número accidentes", x="Semana", colour="PERIODO", linetype="tipo de estimación") + 
      theme(plot.title = element_text(hjust = 0.5), 
            axis.text.x = element_text(size=12),
            axis.title.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title.y = element_text(size=12))
    
  }else if (objective_var == "volcamiento"){
    predicciones_semanales %>% 
      filter(
        PERIODO>=year(start_date),
        PERIODO<=year(end_date),
        SEMANA>=week(start_date),
        SEMANA<=week(end_date)
      ) %>% 
      dplyr::select(PERIODO, SEMANA, estimaciones_volcamiento, accidentes_volcamiento) %>% 
      gather(key="variable", value="value", -c(SEMANA,PERIODO)) %>% 
      ggplot(aes(x=SEMANA, y=value)) +
      geom_line(aes(colour=factor(PERIODO), linetype=factor(variable))) + 
      theme_classic() + 
      ggtitle("Accidentes por mensuales de volcamiento") + 
      labs(y="Número accidentes", x="Semana", colour="PERIODO", linetype="tipo de estimación") + 
      theme(plot.title = element_text(hjust = 0.5), 
            axis.text.x = element_text(size=12),
            axis.title.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title.y = element_text(size=12))
  }else{
    predicciones_semanales %>% 
      filter(
        PERIODO>=year(start_date),
        PERIODO<=year(end_date),
        SEMANA>=week(start_date),
        SEMANA<=week(end_date)
      ) %>% 
      dplyr::select(PERIODO, SEMANA, estimaciones_otro, accidentes_otro) %>% 
      gather(key="variable", value="value", -c(SEMANA,PERIODO)) %>% 
      ggplot(aes(x=SEMANA, y=value)) +
      geom_line(aes(colour=factor(PERIODO), linetype=factor(variable))) + 
      theme_classic() + 
      ggtitle("Accidentes por mensuales de otra clase") + 
      labs(y="Número accidentes", x="Semana", colour="PERIODO", linetype="tipo de estimación") + 
      theme(plot.title = element_text(hjust = 0.5), 
            axis.text.x = element_text(size=12),
            axis.title.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title.y = element_text(size=12))
  }
}

plot_monthly_predictions <- function(start_date, end_date, objective_var){
  start_date <- as_date(start_date)
  end_date <- as_date(end_date)
  if (objective_var == "choque"){
    
    predicciones_mensuales %>% 
      filter(
        year(FECHA)>=year(start_date),
        year(FECHA)<=year(end_date),
        month(FECHA)>=month(start_date),
        month(FECHA)<=month(end_date)
        ) %>% 
      dplyr::select(FECHA, estimaciones_choque, accidentes_choque) %>% 
      gather(key="variable", value="value", -FECHA) %>% 
      ggplot() +
      geom_line(aes(x=FECHA, y=value, colour=variable)) + 
      theme_classic() + 
      ggtitle("Accidentes por mensuales de choque") + 
      labs(y="Número accidentes", x="Periodo", colour="tipo de estimación") + 
      theme(plot.title = element_text(hjust = 0.5), 
            axis.text.x = element_text(size=12),
            axis.title.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title.y = element_text(size=12))
    
  }else if (objective_var == "atropello"){
    
    predicciones_mensuales %>% 
      filter(
        year(FECHA)>=year(start_date),
        year(FECHA)<=year(end_date),
        month(FECHA)>=month(start_date),
        month(FECHA)<=month(end_date)
      ) %>% 
      dplyr::select(FECHA, estimaciones_atropello, accidentes_atropello) %>% 
      gather(key="variable", value="value", -FECHA) %>% 
      ggplot() +
      geom_line(aes(x=FECHA, y=value, colour=variable)) + 
      theme_classic() + 
      ggtitle("Accidentes por mensuales de atropello") + 
      labs(y="Número accidentes", x="Periodo", colour="tipo de estimación") + 
      theme(plot.title = element_text(hjust = 0.5), 
            axis.text.x = element_text(size=12),
            axis.title.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title.y = element_text(size=12))
    
  }else if (objective_var == "caida ocupante"){
    
    predicciones_mensuales %>% 
      filter(
        year(FECHA)>=year(start_date),
        year(FECHA)<=year(end_date),
        month(FECHA)>=month(start_date),
        month(FECHA)<=month(end_date)
      ) %>% 
      dplyr::select(FECHA, estimaciones_caida_ocupante, accidentes_caida_ocupante) %>% 
      gather(key="variable", value="value", -FECHA) %>% 
      ggplot() +
      geom_line(aes(x=FECHA, y=value, colour=variable)) + 
      theme_classic() + 
      ggtitle("Accidentes por mensuales de caida_ocupante") + 
      labs(y="Número accidentes", x="Periodo", colour="tipo de estimación") + 
      theme(plot.title = element_text(hjust = 0.5), 
            axis.text.x = element_text(size=12),
            axis.title.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title.y = element_text(size=12))
    
  }else if (objective_var == "volcamiento"){
    
    predicciones_mensuales %>% 
      filter(
        year(FECHA)>=year(start_date),
        year(FECHA)<=year(end_date),
        month(FECHA)>=month(start_date),
        month(FECHA)<=month(end_date)
      ) %>% 
      dplyr::select(FECHA, estimaciones_volcamiento, accidentes_volcamiento) %>% 
      gather(key="variable", value="value", -FECHA) %>% 
      ggplot() +
      geom_line(aes(x=FECHA, y=value, colour=variable)) + 
      theme_classic() + 
      ggtitle("Accidentes por mensuales de volcamiento") + 
      labs(y="Número accidentes", x="Periodo", colour="tipo de estimación") + 
      theme(plot.title = element_text(hjust = 0.5), 
            axis.text.x = element_text(size=12),
            axis.title.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title.y = element_text(size=12))
    
  }else{
    
    predicciones_mensuales %>% 
      filter(
        year(FECHA)>=year(start_date),
        year(FECHA)<=year(end_date),
        month(FECHA)>=month(start_date),
        month(FECHA)<=month(end_date)
      ) %>% 
      dplyr::select(FECHA, estimaciones_otros, accidentes_otros) %>% 
      gather(key="variable", value="value", -FECHA) %>% 
      ggplot() +
      geom_line(aes(x=FECHA, y=value, colour=variable)) + 
      theme_classic() + 
      ggtitle("Accidentes por mensuales de otra clase") + 
      labs(y="Número accidentes", x="Periodo", colour="tipo de estimación") + 
      theme(plot.title = element_text(hjust = 0.5), 
            axis.text.x = element_text(size=12),
            axis.title.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title.y = element_text(size=12))
    
  }
}

plot_daily_predictions <- function(start_date, end_date, objective_var){
  start_date <- as_date(start_date)
  end_date <- as_date(end_date)
  if (objective_var == "choque"){
    
    predicciones_diarias %>% 
      filter(
        FECHA>=start_date,
        FECHA<=end_date
      ) %>% 
      dplyr::select(FECHA, accidentes_choque, estimaciones_choque) %>% 
      gather(key="variable", value="value", -FECHA) %>% 
      ggplot(aes(x=FECHA, y=value)) +
      geom_line(aes(colour=factor(variable))) + 
      theme_classic() + 
      ggtitle("Accidentes diarios de choques") + 
      labs(y="Número accidentes", x="Periodo", colour="tipo de estimación") + 
      theme(plot.title = element_text(hjust = 0.5), 
            axis.text.x = element_text(size=12),
            axis.title.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title.y = element_text(size=12))
    
  }else if (objective_var == "atropello"){
    
    predicciones_diarias %>% 
      filter(
        FECHA>=start_date,
        FECHA<=end_date
      ) %>% 
      dplyr::select(FECHA, accidentes_atropello, estimaciones_atropello) %>% 
      gather(key="variable", value="value", -FECHA) %>% 
      ggplot(aes(x=FECHA, y=value)) +
      geom_line(aes(colour=factor(variable))) + 
      theme_classic() + 
      ggtitle("Accidentes diarios de atropellos") + 
      labs(y="Número accidentes", x="Periodo", colour="tipo de estimación") + 
      theme(plot.title = element_text(hjust = 0.5), 
            axis.text.x = element_text(size=12),
            axis.title.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title.y = element_text(size=12))
    
  }else if (objective_var == "caida ocupante"){
    
    predicciones_diarias %>% 
      filter(
        FECHA>=start_date,
        FECHA<=end_date
      ) %>% 
      dplyr::select(FECHA, accidentes_caida_ocupante, estimaciones_caida_ocupante) %>% 
      gather(key="variable", value="value", -FECHA) %>% 
      ggplot(aes(x=FECHA, y=value)) +
      geom_line(aes(colour=factor(variable))) + 
      theme_classic() + 
      ggtitle("Accidentes diarios de caida del ocupante") + 
      labs(y="Número accidentes", x="Periodo", colour="tipo de estimación") + 
      theme(plot.title = element_text(hjust = 0.5), 
            axis.text.x = element_text(size=12),
            axis.title.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title.y = element_text(size=12))
    
  }else if (objective_var == "volcamiento"){
    
    predicciones_diarias %>% 
      filter(
        FECHA>=start_date,
        FECHA<=end_date
      ) %>% 
      dplyr::select(FECHA, accidentes_volcamiento, estimaciones_volcamiento) %>% 
      gather(key="variable", value="value", -FECHA) %>% 
      ggplot(aes(x=FECHA, y=value)) +
      geom_line(aes(colour=factor(variable))) + 
      theme_classic() + 
      ggtitle("Accidentes diarios de volcamientos") + 
      labs(y="Número accidentes", x="Periodo", colour="tipo de estimación") + 
      theme(plot.title = element_text(hjust = 0.5), 
            axis.text.x = element_text(size=12),
            axis.title.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title.y = element_text(size=12))
    
  }else{
    
    predicciones_diarias %>% 
      filter(
        FECHA>=start_date,
        FECHA<=end_date
      ) %>% 
      dplyr::select(FECHA, accidentes_otro, estimaciones_otro) %>% 
      gather(key="variable", value="value", -FECHA) %>% 
      ggplot(aes(x=FECHA, y=value)) +
      geom_line(aes(colour=factor(variable))) + 
      theme_classic() + 
      ggtitle("Accidentes diarios de otra clase") + 
      labs(y="Número accidentes", x="Periodo", colour="tipo de estimación") + 
      theme(plot.title = element_text(hjust = 0.5), 
            axis.text.x = element_text(size=12),
            axis.title.x = element_text(size=12),
            axis.text.y = element_text(size=12),
            axis.title.y = element_text(size=12))
    
  }
}


# --------------- funciones para capa de agrupación -----------------------

cluster_2014 <- read_csv("./data_app/cluster_2014.csv")
cluster_2015 <- read_csv("./data_app/cluster_2015.csv")
cluster_2016 <- read_csv("./data_app/cluster_2016.csv")
cluster_2017 <- read_csv("./data_app/cluster_2017.csv")
cluster_2018 <- read_csv("./data_app/cluster_2018.csv")

create_cluster_data_map <- function(cluster_df, periodo){
  cluster_map_data <- df %>% 
    filter(BARRIO!="", PERIODO==periodo) %>% 
    group_by(BARRIO) %>% 
    summarise(
      lat=mean(LATITUD),
      lng=mean(LONGITUD),
      .groups="drop"
    ) %>% 
    merge(cluster_df %>% dplyr::select(BARRIO, cluster))
  return(cluster_map_data)
}

cluster_14_map <- create_cluster_data_map(cluster_df = cluster_2014, periodo = 2014)
cluster_15_map <- create_cluster_data_map(cluster_df = cluster_2015, periodo = 2015)
cluster_16_map <- create_cluster_data_map(cluster_df = cluster_2016, periodo = 2016)
cluster_17_map <- create_cluster_data_map(cluster_df = cluster_2017, periodo = 2017)
cluster_18_map <- create_cluster_data_map(cluster_df = cluster_2018, periodo = 2018)


plot_cluster_map <- function(periodo){
  if (periodo == 2014){
    cluster_map_data <- cluster_14_map %>% 
      mutate(
        cluster_colors=as.character(ifelse(cluster==3, "red", 
                                           ifelse(cluster==4, "orange", 
                                                  ifelse(cluster==1, "yellow", "green"))))
      )
  } else if (periodo == 2015){
    cluster_map_data <- cluster_15_map %>% 
      mutate(
        cluster_colors=as.character(ifelse(cluster==3, "red", 
                                           ifelse(cluster==4, "orange", 
                                                  ifelse(cluster==1, "yellow", "green"))))
      )
  } else if (periodo == 2016){
    cluster_map_data <- cluster_16_map %>% 
      mutate(
        cluster_colors=as.character(ifelse(cluster==3, "red", 
                                           ifelse(cluster==4, "orange", 
                                                  ifelse(cluster==1, "yellow", "green"))))
      )
  } else if (periodo == 2017){
    cluster_map_data <- cluster_17_map %>% 
      mutate(
        cluster_colors=as.character(
          ifelse(
            cluster==3, "red", 
            ifelse(cluster==1, "orange",
                   "green")
          )
        )
      )
  } else {
    cluster_map_data <- cluster_18_map %>% 
      mutate(
        cluster_colors=as.character(
          ifelse(cluster==3, "red", 
            ifelse(cluster==4, "orange",
              ifelse(cluster==1, "yellow","green")
            )
          )
        )
      )
    
  }
  leaflet(cluster_map_data) %>% addTiles() %>%
    addCircleMarkers(
      color = ~cluster_colors,
      stroke = FALSE, 
      fillOpacity = 0.5,
      lng = ~lng, lat = ~lat,
      label = ~as.character(BARRIO)
    )
  
}

statistics_per_neighborhood <- function(periodo, barrio){
  tmp <- df %>%
    filter(PERIODO==periodo, BARRIO==tolower(barrio)) %>% 
    group_by(BARRIO) %>% 
    summarise(
      accidentes_periodo=n(),
      muertos=sum(ifelse(GRAVEDAD=="muerto", 1, 0)),
      heridos=sum(ifelse(GRAVEDAD=="herido", 1, 0)),
      solo_danos=sum(ifelse(GRAVEDAD=="solo danos", 1, 0)),
      choques=sum(ifelse(CLASE=="choque", 1, 0)),
      otros_accidentes=sum(ifelse(CLASE=="otro", 1, 0)),
      atropellos=sum(ifelse(CLASE=="atropello", 1, 0)),
      caidas_ocupante=sum(ifelse(CLASE=="caida ocupante", 1, 0)),
      volcamientos=sum(ifelse(CLASE=="volcamiento", 1, 0)),
      incendios=sum(ifelse(CLASE=="incendio", 1, 0)),
      .groups="drop"
    )
  return(tmp)
}

statistics_per_cluser <- function(periodo, barrio){

  if (periodo == 2014) {
    cluster_ <- cluster_2014 %>% filter(BARRIO==tolower(barrio))
    cluster_ <- cluster_$cluster
    
    cluster_info <- df %>%
      filter(PERIODO==periodo) %>% 
      merge(cluster_2014, by="BARRIO", all.x=T) %>% 
      filter(cluster==cluster_)
    
  } else if (periodo==2015){
    cluster_ <- cluster_2015 %>% filter(BARRIO==tolower(barrio))
    cluster_ <- cluster_$cluster
    
    cluster_info <- df %>%
      filter(PERIODO==periodo) %>% 
      merge(cluster_2015, by="BARRIO", all.x=T) %>% 
      filter(cluster==cluster_)
    
  } else if (periodo==2016){
    cluster_ <- cluster_2016 %>% filter(BARRIO==tolower(barrio))
    cluster_ <- cluster_$cluster
    
    cluster_info <- df %>%
      filter(PERIODO==periodo) %>% 
      merge(cluster_2016, by="BARRIO", all.x=T) %>% 
      filter(cluster==cluster_)
    
  } else if (periodo==2017){
    cluster_ <- cluster_2017 %>% filter(BARRIO==tolower(barrio))
    cluster_ <- cluster_$cluster
    
    cluster_info <- df %>%
      filter(PERIODO==periodo) %>% 
      merge(cluster_2017, by="BARRIO", all.x=T) %>% 
      filter(cluster==cluster_)
    
  } else {
    cluster_ <- cluster_2018 %>% filter(BARRIO==tolower(barrio))
    cluster_ <- cluster_$cluster
    
    cluster_info <- df %>%
      filter(PERIODO==periodo) %>% 
      merge(cluster_2018, by="BARRIO", all.x=T) %>% 
      filter(cluster==cluster_)
    
  }

  cluster_data <- cluster_info %>%
    group_by(cluster) %>%
    summarise(
      accidentes_periodo=n(),
      muertos=sum(ifelse(GRAVEDAD=="muerto", 1, 0)),
      heridos=sum(ifelse(GRAVEDAD=="herido", 1, 0)),
      solo_danos=sum(ifelse(GRAVEDAD=="solo danos", 1, 0)),
      choques=sum(ifelse(CLASE=="choque", 1, 0)),
      otros_accidentes=sum(ifelse(CLASE=="otro", 1, 0)),
      atropellos=sum(ifelse(CLASE=="atropello", 1, 0)),
      caidas_ocupante=sum(ifelse(CLASE=="caida ocupante", 1, 0)),
      volcamientos=sum(ifelse(CLASE=="volcamiento", 1, 0)),
      incendios=sum(ifelse(CLASE=="incendio", 1, 0)),
      .groups="drop"
    )

  return(cluster_data)
}


  function(input, output, session) {
  
  # --------------- Tab de visualización ----------------------------------
  
  output_total_periodo <- eventReactive(input$renderButton, {
    graph_number_classes(
      start_date = input$inDateRange[1], 
      end_date = input$inDateRange[2],
      nivel = "periodo")} , ignoreNULL = FALSE)
  
  output_total_mes <- eventReactive(input$renderButton, {
    graph_number_classes(
      start_date = input$inDateRange[1], 
      end_date = input$inDateRange[2],
      nivel = "mes")    
  }, ignoreNULL = FALSE)
  
  output_total_dia <- eventReactive(input$renderButton, {
    graph_number_classes(
      start_date = input$inDateRange[1], 
      end_date = input$inDateRange[2],
      nivel = "dia")
  }, ignoreNULL = FALSE)
  
  output_plot_map <- eventReactive(input$mapButton, {
    plot_map(year = input$periodMap)
  }, ignoreNULL = FALSE)
  
  output_title_map <- eventReactive(input$mapButton, {
    paste("Localización de los accidentes en Medellín por año", input$periodMap)
  }, ignoreNULL = FALSE)
  
  output_class_freq <- eventReactive(input$renderButton, {
    plot_by_cat_var(start_date = input$inDateRange[1], end_date = input$inDateRange[2], cat_var = CLASE)
  }, ignoreNULL = FALSE)
  
  output_grav_freq <- eventReactive(input$renderButton, {
    plot_by_cat_var(start_date = input$inDateRange[1], end_date = input$inDateRange[2], cat_var = GRAVEDAD)
  }, ignoreNULL = FALSE)
  
  output_diseno_freq <- eventReactive(input$renderButton, {
    plot_by_cat_var(start_date = input$inDateRange[1], end_date = input$inDateRange[2], cat_var = DISENO)
  }, ignoreNULL = FALSE)
  
  output_heat_clase_grav <- eventReactive(input$renderButton, {
    plot_heat_map(start_date = input$inDateRange[1], end_date = input$inDateRange[2], x = CLASE, y = GRAVEDAD)
  }, ignoreNULL = FALSE)
  
  output_heat_clase_diseno <- eventReactive(input$renderButton, {
    plot_heat_map(start_date = input$inDateRange[1], end_date = input$inDateRange[2], x = CLASE, y = DISENO)
  }, ignoreNULL = FALSE)
  
  output_heat_diseno_grav <- eventReactive(input$renderButton, {
    plot_heat_map(start_date = input$inDateRange[1], end_date = input$inDateRange[2], x = DISENO, y = GRAVEDAD)
  }, ignoreNULL = FALSE)
  
  
  # renderización de las gráficas
  
  output$total_periodo <- renderPlot({
    output_total_periodo()
    })
  
  output$total_mes <- renderPlot({
    output_total_mes()
  })
  
  output$total_dia <- renderPlot({
    output_total_dia()
  })
  
  output$medellin_map <- renderLeaflet({
    output_plot_map()
  })
  
  output$clase_accidente <- renderPlot({
    output_class_freq()
  })
  
  output$gravedad <- renderPlot({
    output_grav_freq()
  })
  
  output$infra_vial <- renderPlot({
    output_diseno_freq()
  })
  
  output$mapTitle <- renderText({
    output_title_map()
  })
  
  output$heat_clase_grav <- renderPlot({
    output_heat_clase_grav()
  })
  
  output$heat_clase_infra <- renderPlot({
    output_heat_clase_diseno()
  })
  
  output$hear_grav_infra <- renderPlot({
    output_heat_diseno_grav()
  })
  
  
  # --------------- Tab de predicción ----------------------------------
  
  # Mensuales
  output_pred_men_choque <- eventReactive(input$predictButton, {
    plot_monthly_predictions(
      start_date = input$inDateRangePred[1],
      end_date = input$inDateRangePred[2],
      objective_var = "choque")
    } , ignoreNULL = FALSE)
  
  output_pred_men_atropello <- eventReactive(input$predictButton, {
    plot_monthly_predictions(
      start_date = input$inDateRangePred[1],
      end_date = input$inDateRangePred[2],
      objective_var = "atropello")
  } , ignoreNULL = FALSE)
  
  output_pred_men_caida <- eventReactive(input$predictButton, {
    plot_monthly_predictions(
      start_date = input$inDateRangePred[1],
      end_date = input$inDateRangePred[2],
      objective_var = "caida ocupante")
  } , ignoreNULL = FALSE)
  
  output_pred_men_volcamiento <- eventReactive(input$predictButton, {
    plot_monthly_predictions(
      start_date = input$inDateRangePred[1],
      end_date = input$inDateRangePred[2],
      objective_var = "volcamiento")
  } , ignoreNULL = FALSE)
  
  output_pred_men_otro <- eventReactive(input$predictButton, {
    plot_monthly_predictions(
      start_date = input$inDateRangePred[1],
      end_date = input$inDateRangePred[2],
      objective_var = "otros")
  } , ignoreNULL = FALSE)
  
  # renderización de las gráficas mensuales
  
  output$pred_mes_choque <- renderPlot({
    output_pred_men_choque()
  })
  
  output$pred_mes_atrop <- renderPlot({
    output_pred_men_atropello()
  })
  
  output$pred_mes_caida <- renderPlot({
    output_pred_men_caida()
  })
  
  output$pred_mes_volc <- renderPlot({
    output_pred_men_volcamiento()
  })
  
  output$pred_mes_otro <- renderPlot({
    output_pred_men_otro()
  })
  
  # Semanales
  output_pred_week_choque <- eventReactive(input$predictButton, {
    plot_weekly_predictions(
      start_date = input$inDateRangePred[1],
      end_date = input$inDateRangePred[2],
      objective_var = "choque")
  } , ignoreNULL = FALSE)
  
  output_pred_week_atropello <- eventReactive(input$predictButton, {
    plot_weekly_predictions(
      start_date = input$inDateRangePred[1],
      end_date = input$inDateRangePred[2],
      objective_var = "atropello")
  } , ignoreNULL = FALSE)
  
  output_pred_week_caida <- eventReactive(input$predictButton, {
    plot_weekly_predictions(
      start_date = input$inDateRangePred[1],
      end_date = input$inDateRangePred[2],
      objective_var = "caida ocupante")
  } , ignoreNULL = FALSE)
  
  output_pred_week_volcamiento <- eventReactive(input$predictButton, {
    plot_weekly_predictions(
      start_date = input$inDateRangePred[1],
      end_date = input$inDateRangePred[2],
      objective_var = "volcamiento")
  } , ignoreNULL = FALSE)
  
  output_pred_week_otro <- eventReactive(input$predictButton, {
    plot_weekly_predictions(
      start_date = input$inDateRangePred[1],
      end_date = input$inDateRangePred[2],
      objective_var = "otros")
  } , ignoreNULL = FALSE)
  
  # renderización de las gráficas semanales
  
  output$pred_week_choque <- renderPlot({
    output_pred_week_choque()
  })
  
  output$pred_week_atrop <- renderPlot({
    output_pred_week_atropello()
  })
  
  output$pred_week_caida <- renderPlot({
    output_pred_week_caida()
  })
  
  output$pred_week_volc <- renderPlot({
    output_pred_week_volcamiento()
  })
  
  output$pred_week_otro <- renderPlot({
    output_pred_week_otro()
  })
  
  # Diarias
  
  output_pred_daily_choque <- eventReactive(input$predictDailyButton, {
    plot_daily_predictions(
      start_date = input$inDateRangePredDaily[1],
      end_date = input$inDateRangePredDaily[2],
      objective_var = "choque")
  } , ignoreNULL = FALSE)
  
  output_pred_daily_atropello <- eventReactive(input$predictDailyButton, {
    plot_daily_predictions(
      start_date = input$inDateRangePredDaily[1],
      end_date = input$inDateRangePredDaily[2],
      objective_var = "atropello")
  } , ignoreNULL = FALSE)
  
  output_pred_daily_caida <- eventReactive(input$predictDailyButton, {
    plot_daily_predictions(
      start_date = input$inDateRangePredDaily[1],
      end_date = input$inDateRangePredDaily[2],
      objective_var = "caida ocupante")
  } , ignoreNULL = FALSE)
  
  output_pred_daily_volcamiento <- eventReactive(input$predictDailyButton, {
    plot_daily_predictions(
      start_date = input$inDateRangePredDaily[1],
      end_date = input$inDateRangePredDaily[2],
      objective_var = "volcamiento")
  } , ignoreNULL = FALSE)
  
  output_pred_daily_otro <- eventReactive(input$predictDailyButton, {
    plot_daily_predictions(
      start_date = input$inDateRangePredDaily[1],
      end_date = input$inDateRangePredDaily[2],
      objective_var = "otros")
  } , ignoreNULL = FALSE)
  
  # renderización de las gráficas diarias
  
  output$pred_day_choque <- renderPlot({
    output_pred_daily_choque()
  })
  
  output$pred_day_atrop <- renderPlot({
    output_pred_daily_atropello()
  })
  
  output$pred_day_caida <- renderPlot({
    output_pred_daily_caida()
  })
  
  output$pred_day_volc <- renderPlot({
    output_pred_daily_volcamiento()
  })
  
  output$pred_day_otro <- renderPlot({
    output_pred_daily_otro()
  })
  
  
  # --------------- Tab de agrupación ----------------------------------
  
  output_plot_cluster_map <- eventReactive(input$neighborhoodMap, {
    plot_cluster_map(periodo = input$selectYear)
  }, ignoreNULL = FALSE)
  
  output_cluster_title_map <- eventReactive(input$neighborhoodMap, {
    paste("Clusters de barrios de Medellín para el año", input$selectYear)
  }, ignoreNULL = FALSE)
  
  output$cluster_map <- renderLeaflet({
    output_plot_cluster_map()
  })
  
  output$clusterMapTitle <- renderText({
    output_cluster_title_map()
  })
  
  output_title_single_table <- eventReactive(input$neighborhoodData, {
    paste("Información del barrio", input$neighborhood, "para el año", input$selectYearNeighborhood)
  }, ignoreNULL = FALSE)
  
  output$neighborhoodInfo <- renderText({
    output_title_single_table()
  })
  
  output_single_table <- eventReactive(input$neighborhoodData, {
    DT::datatable(statistics_per_neighborhood(
      periodo = input$selectYearNeighborhood,
      barrio = input$neighborhood
      ))
  })
  
  output$neighborhoodTable <- DT::renderDataTable({
    output_single_table()
  })
  
  
  ## cluster table
  
  output_title_cluster_table <- eventReactive(input$neighborhoodMap, {
    paste("Información del cluster para el año", input$selectYearNeighborhood)
  }, ignoreNULL = FALSE)
  
  output$clusterInfo <- renderText({
    output_title_cluster_table()
  })
  
  output_cluster_table <- eventReactive(input$neighborhoodData, {
    DT::datatable(statistics_per_cluser(
      periodo = input$selectYearNeighborhood,
      barrio = input$neighborhood
    ))
  })
  
  output$clusterTable <- DT::renderDataTable({
    output_cluster_table()
  })
  
  
}