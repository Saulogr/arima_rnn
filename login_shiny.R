library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)
library(shinythemes)

# Main login screen
loginpage <-
  div(
    id = "loginpage",
    style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
    wellPanel(
      tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
      textInput(
        "userName",
        placeholder = "Username",
        label = tagList(icon("user"), "Username")
      ),
      passwordInput(
        "passwd",
        placeholder = "Password",
        label = tagList(icon("unlock-alt"), "Password")
      ),
      br(),
      div(
        style = "text-align: center;",
        actionButton(
          "login",
          "SIGN IN",
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

credentials = data.frame(
  username_id = c("myuser", "myuser1"),
  passod   = sapply(c("mypass", "mypass1"), password_store),
  permission  = c("basic", "advanced"),
  stringsAsFactors = F
)

# Criação da interface
ui = navbarPage(
  "",
  theme = shinytheme("simplex"),
  id = "tabs",
  tabPanel("Login",
           loginpage),
  tabPanel(
    "Importação",
    dashboardPage(
      dashboardHeader(disable = TRUE),
      dashboardSidebar(),
      dashboardBody(
        "Essa é a página de importação dos dados",
        verbatimTextOutput("status")
      )
    )
    
  ),
  tabPanel(
    "Ajuste",
    dashboardPage(
      dashboardHeader(disable = TRUE),
      dashboardSidebar(),
      dashboardBody("Essa é a página de ajsute da Série temporal")
    )
    
  ),
  tabPanel(
    "Arima",
    dashboardPage(
      dashboardHeader(disable = TRUE),
      dashboardSidebar(),
      dashboardBody("Essa é a página de ajsute do ARIMA")
    )
    
  ),
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
  
  
  output$results <-  DT::renderDataTable({
    datatable(iris, options = list(autoWidth = TRUE,
                                   searching = FALSE))
  })
  
  output$status = renderPrint(USER$login)
  
}

shinyApp(ui, server)