library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinythemes)

ui = navbarPage("Modelo Híbrido",
                tabPanel("Importação",
                         dashboardPage(
                             dashboardHeader(disable = TRUE),
                             dashboardSidebar(),
                             dashboardBody()
                         )

                ) 
    )
    
server = function(input, output) {
    
}

shinyApp(ui, server)
