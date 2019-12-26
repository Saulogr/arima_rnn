library(shiny)
library(tidyverse)
library(forecast)
library(shinythemes)
ui = fluidPage(theme = shinytheme("simplex"), headerPanel = h1("Previsões de séries temporais"),
    
                     # Barra de comandos
                     sidebarPanel(
                       h4(strong("Importação dos dados")),
                       hr(),
                         fileInput("ts",
                                   "Selecione o arquivo",
                                   accept = ".csv"),
                       checkboxInput("header",
                                     "A tabela possui cabeçalho",
                                     value = TRUE),
                       radioButtons("delim",
                                    "Delimitador",
                                    choices = c("Ponto e vírgula"= ";",
                                                "Vírgula"=  ",",
                                                "Espaço em branco"= " "),
                                    selected = ";"),
                       h4(strong("Configuração da série temporal")),
                       tags$hr(),
                       uiOutput("ColumnSelector") ,
                         numericInput("period", 
                                      "Qual a frequência da série?",
                                      value = 1,
                                      min = 0),
                                                 
                           dateRangeInput("intervalo",
                                          "Data inicial e final da série",
                                          format = "dd-mm-yyyy",
                                          language = "pt",
                                          min = "1900-01-01")
                                          
                         
                     ),
                     
                     # Painel principal
                     mainPanel(
                       # Dividindo em objetos verticais
                        h3(p(strong("Série temporal selecionada"))),
                         plotOutput("plotts"),
                        # Plotando o resumo da série
                        h3(p(strong("Série ajustada"))),
                        tableOutput("resumo.ts"),
                         # Plotando o arima
                        h3(p(strong("Modelo ARIMA"))),
                        hr(),
                        code(verbatimTextOutput("arima.ts"))
                       
                        
                     )
                     
                     
                     
                     )
