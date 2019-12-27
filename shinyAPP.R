library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinythemes)

ui = navbarPage("Modelo Híbrido", theme = shinytheme("simplex"),
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
