library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)
library(shinythemes)
library(tidyverse)
library(forecast)

# Banco de dados com as credenciais
credentials = data.frame(
  username_id = c("", "myuser1"),
  passod   = sapply(c("", "mypass1"), password_store),
  permission  = c("basic", "advanced"),
  stringsAsFactors = F
)

# Criação da interface

# Tela de login
loginpage =
  div(
    id = "loginpage",
    style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
    wellPanel(
      tags$h2("Login", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
      textInput(
        "userName",
        placeholder = "Username",
        label = tagList(icon("user"), "Usuário")
      ),
      passwordInput(
        "passwd",
        placeholder = "Password",
        label = tagList(icon("unlock-alt"), "Senha")
      ),
      br(),
      div(
        style = "text-align: center;",
        actionButton(
          "login",
          "ENTRAR",
          style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"
        ),
        br(),
        br(),
        code(textOutput("avisologin")),
        br(),
        br()
      )
    )
  )

# Página de importação de ajuste da série temporal
importpage = dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(
    # Comandos para importação dos dados da série temporal
    h4("Importação dos dados", style = "color: white; font-size: 18px; font-weight: 600"),
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
    br(),
    h4("Configuração da série temporal", style = "color: white; font-size: 18px; font-weight: 600"),
    tags$hr(),
    uiOutput("ColumnSelector") ,
    numericInput("period", 
                 "Qual a frequência da série?",
                 value = 12,
                 min = 0),
    
    dateRangeInput("intervalo",
                   "Data inicial e final da série",
                   format = "dd-mm-yyyy",
                   language = "pt",
                   min = "1900-01-01",
                   start = "01-01-2003",
                   end = "30-04-2019",
                   startview = "year")
  ),
  dashboardBody(
    # Dividindo em objetos verticais
    h3(p(strong("Série temporal selecionada"))),
    plotOutput("plotts"),
    # Plotando o resumo da série
    h3(p(strong("Série ajustada"))),
    tableOutput("resumo.ts")
  )
)


# Página de ajsute do arima
arimapage = 
  dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(
      h4("Ajuste do modelo ARIMA", style = "color: white; font-size: 18px; font-weight: 600"),
      tags$hr(),
      radioButtons("selectarima",
                   "Qual abordagem quer utilizar?",
                   choices = c("Ajuste automático"= "autoarima",
                               "Ajustar manualmente"=  "manualarima",
                               "Fazer upload do modelo já ajustado"= "uploadarima"),
                   selected = "autoarima"),
      tags$hr(),
      uiOutput("arimaui"),
      tags$hr()
    ),
    dashboardBody(
      h5("Resumo do modelo ajustado"),
      verbatimTextOutput("resumoarima"),
      h5("Diagnóstico do modelo ajustado"),
      plotOutput("tsdiag")
    )
  )

# Gerar o objeto de interface
ui = navbarPage(
  "",
  theme = shinytheme("simplex"),
  id = "tabs",
  tabPanel("Login",
           loginpage),
  tabPanel(
    "Importação", importpage),
  tabPanel(
    "Arima",arimapage),
  tabPanel(
    "Rede Neural",
    dashboardPage(
      dashboardHeader(disable = TRUE),
      dashboardSidebar(),
      dashboardBody("Essa é a página de ajuste da rede neural")
    )
    
  ),
  tabPanel(
    "Resultados",
    dashboardPage(
      dashboardHeader(disable = TRUE),
      dashboardSidebar(),
      dashboardBody("Essa é a página de resultados")
    )
    
  )
)

server <- function(input, output, session) {
  # Verificação do login
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if (length(which(credentials$username_id == Username)) == 1) {
            pasmatch  <-
              credentials["passod"][which(credentials$username_id == Username), ]
            pasverify <- password_verify(pasmatch, Password)
            if (pasverify) {
              USER$login <- TRUE
            } else {
              output$avisologin = renderText({
                "Senha ou usuário incorreto"
              })
              
            }
          } else {
            output$avisologin = renderText({
              "Senha ou usuário incorreto"
            })
            
          }
        }
      }
    }
  })
  
  # Habilitando as abas
  observe({
    if (USER$login == TRUE) {
      showTab("tabs", "Importação")
      showTab("tabs", "Ajuste")
      showTab("tabs", "Arima")
      showTab("tabs", "Rede Neural")
      showTab("tabs", "Resultados")
      hideTab("tabs", "Login")
    } else {
      hideTab("tabs", "Importação")
      hideTab("tabs", "Ajuste")
      hideTab("tabs", "Arima")
      hideTab("tabs", "Rede Neural")
      hideTab("tabs", "Resultados")
      showTab("tabs", "Login")
    }
  })
  
  # Botão de Logout
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(
      a(icon("fa fa-sign-out"), "Logout",
        href = "javascript:window.location.reload(true)"),
      class = "dropdown",
      style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;"
    )
  })
  
  # Selecionado o banco de dados de forma reativa
  datasetInput <- reactive({
    arquivo = input$ts
    if (is.null(arquivo)){
      return(NULL) 
    } else {
      read.csv(input$ts$datapath, header = input$header,
               sep = input$delim)
    }
  })
  
  # Selecionado a série temporal que será usada de forma dinâmica
  serieTemporal = reactive({
    if (is.null(datasetInput())){
      return(NULL) 
    } else {
      # Extraindo o dia inicial
      dia.in =as.numeric(gsub("^([0-9]*)-([0-9]*)-([0-9]*)$",
                              "\\3", input$intervalo[1]))
      # Extraindo o mes inicial
      mes.in =as.numeric(gsub("^([0-9]*)-([0-9]*)-([0-9]*)$",
                              "\\2", input$intervalo[1]))
      # Extraindo o ano inicial
      ano.in =as.numeric(gsub("^([0-9]*)-([0-9]*)-([0-9]*)$",
                              "\\1", input$intervalo[1]))
      # Extraindo o dia final
      dia.out = as.numeric(gsub("^([0-9]*)-([0-9]*)-([0-9]*)$",
                                "\\3", input$intervalo[2]))
      # Extraindo o mes inicial
      mes.out =as.numeric(gsub("^([0-9]*)-([0-9]*)-([0-9]*)$",
                               "\\2", input$intervalo[2]))
      # Extraindo o ano final
      ano.out =as.numeric(gsub("^([0-9]*)-([0-9]*)-([0-9]*)$",
                               "\\1", input$intervalo[2]))
      ts(select(datasetInput(), input$SelectedColumn), 
         start = c(ano.in, mes.in, dia.in),
         end = c(ano.out, mes.out, dia.out),
         frequency = input$period)
    }
  })
  
  # Resumindo a série temporal
  output$resumo.ts = renderTable({
    if (is.null(datasetInput())) {
      return(NULL)
    } else {
    Month <-  factor(cycle(serieTemporal()),
                     levels = 1:12, labels = month.abb)
    data.frame(tapply(serieTemporal(),
                      list(year = floor(time(serieTemporal())),
                           month = Month), c))
    }
  })
  
  # Gerando a UI de forma dinâmica
  output$ColumnSelector <- renderUI({
    selectInput("SelectedColumn",
                "Em qual coluna está a série", 
                choices = colnames(datasetInput()))
  })
  
  # Plotando o o gráfico da série temporal
  output$plotts = renderPlot({
    
    if (is.null(datasetInput())){
      return(NULL) 
    } else {
      # Extraindo o ano inicial
      
      try(ggplot(serieTemporal(), aes(x, y))+
        geom_line(color = "#E7B800", size = 1)+
        theme_minimal()+
        theme(panel.border = element_rect(fill = NA),
              axis.title = element_text(face = "bold",
                                        size = 12))+
        labs(x = "Período",
             y = "Total"), silent = TRUE)
      
      
    }
    
  })


  # Gerando a ui para o arima
  output$arimaui = renderUI({
    if (input$selectarima == "autoarima"){
      return(NULL)
    } else if (input$selectarima == "manualarima"){
      tagList(
        sliderInput("p", "Coeficiente P do (AR)IMA: AR",min = 0, max = 10, value = 0),
        sliderInput("d", "Coeficiente D AR(I)MA: I",min = 0, max = 10, value = 0),
        sliderInput("q", "Coeficiente Q do ARI(MA): MA",min = 0, max = 10, value = 0))
    }else {
      fileInput("modelarima", "Selecione o modelo .Rdata",accept = ".Rdata")
    }
  })
  # Ajustando o ARIMA para a série
  arima.ts = reactive({
    if (input$selectarima == "autoarima"){
      auto.arima(serieTemporal())
    } else if (input$selectarima == "manualarima"){
      Arima(serieTemporal(), order = c(input$p, input$d, input$q))
    }else {
      readRDS(input$modelarima$datapath)
    }
    
  })
  
  # Gerando um resumo do modelo Arima
  output$resumoarima = renderPrint({
    if (is.null(serieTemporal())){
      return("Nenhuma série selecionada")
    } else {
      arima.ts()
    }
  })
  
  # Gerando gráfico do diagnóstico
  output$tsdiag = renderPlot({
    if (is.null(serieTemporal())){
      return(NULL) 
    } else {
      try(tsdiag(arima.ts()), silent = FALSE)
    }
  })

}

shinyApp(ui, server)