library(shiny)

server <- function(input, output, session) {
  observeEvent(input$demarrer, {
    updateNavbarPage(session, "navbar", selected = "selection")
  })
}