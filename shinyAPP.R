library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinythemes)

ui = navbarPage("Modelo Híbrido", theme = shinytheme("simplex"), id = "tabs",
                tabPanel("Importação",
                         dashboardPage(
                           dashboardHeader(disable = TRUE),
                           dashboardSidebar(
                             checkboxInput("hide.ajuste", "Esconder aba ajuste")
                           ),
                           dashboardBody(
                             verbatimTextOutput("status")
                           )
                         )
                         
                ),
                tabPanel("Ajuste",
                         dashboardPage(
                           dashboardHeader(disable = TRUE),
                           dashboardSidebar(),
                           dashboardBody()
                         )
                         
                ) 
)
    
server = function(input, output) {

  # Criando expressão que armazena o valor do checkbox
  statusCheckBox = reactive({
    if (input$hide.ajuste == TRUE){
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  
  output$status = renderPrint(
    statusCheckBox()
  )

  # Acultando as abas 
  observe({
    if(statusCheckBox() == TRUE){
      hideTab("tabs", "Ajuste")
    } else {
      showTab("tabs", "Ajuste")
    }
  })
  
  
}

shinyApp(ui, server)
