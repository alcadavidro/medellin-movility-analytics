library(shiny)
library(markdown)
library(leaflet)
library(DT)

neighborhoods <- read.csv('./data_app/neighborhoods.csv')
barrios <- toupper(neighborhoods$BARRIO)

shinyUI(
  navbarPage(
    theme = "cerulean",
    title = 'Accidentalidad en Medellín',
    # Introducción
    tabPanel(
      "Introducción",
      mainPanel(
        includeMarkdown("intro.md")
      )
    ),
    tabPanel('Visualización de datos',
             fluidPage(
               titlePanel("Exploración de datos de accidentalidad por ventana de tiempo"),
               fluidRow(
                 
                 column(6, 
                        wellPanel(h5("Seleccione una ventana de tiempo del 2014-01-01 al 2018-12-31"),
                                  dateRangeInput("inDateRange", "Rango de fechas:",
                                                 start = "2014-01-01",
                                                 end = "2018-12-31",
                                                 min = "2014-01-01",
                                                 max = "2018-12-31"),
                                  actionButton("renderButton", "Render"),
                                  p("Presionar el botón de Render para aplicar cambios de rangos de fechas en las visualizaciones")
                        )
                 ),
                 column(
                   6,
                   wellPanel(h5("Año a mostrar accidentes por geolocalizción"),
                             numericInput("periodMap", label = "Año", value = 2014, min=2014, max=2018, step=1), 
                             actionButton("mapButton", "Plot Map"),
                             p("Presionar el botón de Plot Map para presentar puntos de accidentes en el mapa por año")
                   )
                 )
                 ,
                 fluidRow(
                   column(6, plotOutput("total_periodo")),
                   column(6, plotOutput("total_mes"))
                 ),
                 fluidRow(
                   column(12, plotOutput("total_dia"))
                 ),
                 fluidRow(
                   column(4, plotOutput("clase_accidente")),
                   column(4, plotOutput("gravedad")),
                   column(4, plotOutput("infra_vial"))
                 ),
                 fluidRow(
                   column(4, plotOutput("heat_clase_grav")),
                   column(4, plotOutput("heat_clase_infra")),
                   column(4, plotOutput("hear_grav_infra")),
                 ),
                 fluidRow(
                   h4(textOutput("mapTitle"), align = "center"), 
                   column(12, leafletOutput("medellin_map"))
                 )
                 
               )
             )
    ),
    tabPanel('Modelos predictivos', 
             fluidPage(
               titlePanel("Prediccion de accidentalidad por ventana de tiempo"),
               fluidRow(
                 
                 column(12, 
                        wellPanel(h5("Seleccione una ventana de tiempo del 2014-01-01 al 2018-12-31"),
                                  dateRangeInput("inDateRangePred", "Rango de fechas:",
                                                 start = "2014-01-01",
                                                 end = "2018-12-31",
                                                 min = "2014-01-01",
                                                 max = "2018-12-31"),
                                  actionButton("predictButton", "Predict"),
                                  p("Presionar el botón de Predict para predecir por el rango de fechas seleccionados")
                        )
                 )
                 ,
                 h3("Modelos de predicción de accidentalidad Mensual", align="center"),
                 fluidRow(
                   column(6, plotOutput("pred_mes_choque")),
                   column(6, plotOutput("pred_mes_atrop")),
                 ),
                 fluidRow(
                   column(4, plotOutput("pred_mes_caida")),
                   column(4, plotOutput("pred_mes_volc")),
                   column(4, plotOutput("pred_mes_otro"))
                 ),
                 h3("Modelos de predicción de accidentalidad Semanal", align="center"),
                 fluidRow(
                   column(6, plotOutput("pred_week_choque")),
                   column(6, plotOutput("pred_week_atrop")),
                 ),
                 fluidRow(
                   column(4, plotOutput("pred_week_caida")),
                   column(4, plotOutput("pred_week_volc")),
                   column(4, plotOutput("pred_week_otro"))
                 ),
                 h3("Modelos de predicción de accidentalidad Diaria", align="center"),
                 fluidRow(
                   column(12, 
                          wellPanel(h5("Seleccione una ventana de tiempo del 2014-01-01 al 2018-12-31. Se recomienda usar 30 días"),
                                    dateRangeInput("inDateRangePredDaily", "Rango de fechas:",
                                                   start = "2015-01-01",
                                                   end = "2015-02-02",
                                                   min = "2014-01-01",
                                                   max = "2018-12-31"),
                                    actionButton("predictDailyButton", "Predict"),
                                    p("Presionar el botón de Predict para predecir por el rango de fechas seleccionados")
                          )
                   ),
                   fluidRow(
                     column(6, plotOutput("pred_day_choque")),
                     column(6, plotOutput("pred_day_atrop"))
                   ),
                   fluidRow(
                     column(4, plotOutput("pred_day_caida")),
                     column(4, plotOutput("pred_day_volc")),
                     column(4, plotOutput("pred_day_otro"))
                   )
                 )
               )
             )
    ),
    tabPanel('Agrupamiento', 
             fluidPage(
               titlePanel("Agrupamiento de accidentalidad por barrios"),
               fluidRow(
                 
                 column(6, 
                        wellPanel(h5("Seleccione un Barrio para visualizar"),
                                  selectInput("neighborhood", "Seleccione el barrio", barrios, "solid"),
                                  selectInput("selectYearNeighborhood",
                                              "Seleccione un periodo para ver del barrio",
                                              c(2014, 2015, 2016, 2017, 2018),
                                              "solid"),
                                  actionButton("neighborhoodData", "Show data"),
                        )
                 ),
                 column(6, 
                        wellPanel(h5("Seleccione un periodo para clusterizar"),
                                  selectInput("selectYear",
                                              "Seleccione un periodo",
                                              c(2014, 2015, 2016, 2017, 2018),
                                              "solid"),
                                  actionButton("neighborhoodMap", "Plot map"),
                        )
                 ),
                 fluidRow(
                   h4(textOutput("clusterMapTitle"), align = "center"), 
                   column(12, leafletOutput("cluster_map"))
                 ),
                 fluidRow(
                   h4(textOutput("neighborhoodInfo"), align = "center"),
                   column(12, DT::dataTableOutput("neighborhoodTable"))
                 ),
                 fluidRow(
                   h4(textOutput("clusterInfo"), align = "center"),
                   column(12, DT::dataTableOutput("clusterTable"))
                 )
               )
             )
    )
  )
)