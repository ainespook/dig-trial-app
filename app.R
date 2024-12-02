#this is a basic template that we can work from, it can be edited as we need

if (!require("shiny")) install.packages("shiny")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("palmerpenguins")) install.packages("palmerpenguins")
if (!require("DynNom")) install.packages("DynNom")
if (!require("shinydashboard")) install.packages("shinydashboard")
library(tidyverse)
library(shiny)
library(palmerpenguins)


ui <- fluidPage(
  titlePanel("DIG Trial Data Exploration"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "TRTMT", label = "Treatment Group:", choices = c("Placebo", "Treatment"), multiple = FALSE)
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