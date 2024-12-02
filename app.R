#this is a basic template that we can work from, it can be edited as we need
library(shiny)

ui <- fluidPage(
  titlePanel("DIG Trial Data Exploration"),
  sidebarLayout(
    sidebarPanel(
      #inputs here
    ),
    mainPanel(
      #outputs here
    )
  )
)

server <- function(input, output) {
  #server workings here
}

shinyApp(ui = ui, server = server)