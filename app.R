#this is a basic template that we can work from, it can be edited as we need

if (!require("shiny")) install.packages("shiny")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("DynNom")) install.packages("DynNom")
if (!require("shinydashboard")) install.packages("shinydashboard")
library(tidyverse)
library(shiny)



ui <- fluidPage(
  titlePanel("DIG Trial Data Exploration"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "TRTMT", label = "Treatment Group:", choices = c("Placebo", "Treatment"), multiple = FALSE),
      radioButtons(inputId = "SEX", label = "Select Sex:", choices = c("Male", "Female"), inline = T)
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