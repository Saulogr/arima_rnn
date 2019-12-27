library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(sodium)

# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     br(),
                     tags$code("Username: myuser  Password: mypass"),
                     br(),
                     tags$code("Username: myuser1  Password: mypass1")
                   ))
)

credentials = data.frame(
  username_id = c("myuser", "myuser1"),
  passod   = sapply(c("mypass", "mypass1"),password_store),
  permission  = c("basic", "advanced"), 
  stringsAsFactors = F
)

# Criação da interface
ui = navbarPage("Modelo Híbrido", theme = shinytheme("simplex"), id = "tabs",
                tabPanel("Login",
                        loginpage
                ),
                tabPanel("Importação",
                         dashboardPage(
                           dashboardHeader(disable = TRUE),
                           dashboardSidebar(
                             checkboxInput("hide.ajuste", "Esconder aba ajuste")
                           ),
                           dashboardBody("Essa é a página de importação dos dados",
                             verbatimTextOutput("status")
                           )
                         )
                         
                ),
                tabPanel("Ajuste",
                         dashboardPage(
                           dashboardHeader(disable = TRUE),
                           dashboardSidebar(),
                           dashboardBody("Essa é a página de ajsute da Série temporal")
                         )
                         
                ),
                tabPanel("Arima",
                         dashboardPage(
                           dashboardHeader(disable = TRUE),
                           dashboardSidebar(),
                           dashboardBody("Essa é a página de ajsute do ARIMA")
                         )
                         
                ),
                tabPanel("Rede Neural",
                         dashboardPage(
                           dashboardHeader(disable = TRUE),
                           dashboardSidebar(),
                           dashboardBody("Essa é a página de ajuste da rede neural")
                         )
                         
                ),
                tabPanel("Resultados",
                         dashboardPage(
                           dashboardHeader(disable = TRUE),
                           dashboardSidebar(),
                           dashboardBody("Essa é a página de resultados")
                         )
                         
                ) 
)

server <- function(input, output, session) {
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  # Habilitando as abas
  observe({
    if(USER$login == TRUE){
      showTab("tabs", "Importação")
      showTab("tabs", "Ajuste")
      showTab("tabs", "Arima")
      showTab("tabs", "Rede Neural")
      showTab("tabs", "Resultados")
          } else {
      hideTab("tabs", "Importação")
      hideTab("tabs", "Ajuste")
      hideTab("tabs", "Arima")
      hideTab("tabs", "Rede Neural")
      hideTab("tabs", "Resultados")
    }
  })
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  

  output$results <-  DT::renderDataTable({
    datatable(iris, options = list(autoWidth = TRUE,
                                   searching = FALSE))
  })
  
  output$status = renderPrint(
    USER$login
  )
  
}

runApp(list(ui = ui, server = server), launch.browser = TRUE)