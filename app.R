#this is a basic template that we can work from, it can be edited as we need

install.packages("table1")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("forcats")
install.packages("shinydashboard")
library(forcats)
library(readr)
library(dplyr)
library(table1)
library(ggplot2)
library(tidyverse)
library(shiny)
library(shinydashboard)
```


DIG.df <- read.csv("data/DIG.csv") %>%
  mutate(
    TRTMT = fct_recode(as.factor(TRTMT), Placebo = "0", Treatment = "1"),
    SEX = fct_recode(as.factor(SEX), Male = "1", Female = "2"),
    HYPERTEN = fct_recode(as.factor(HYPERTEN), No = "0", Yes = "1"),
    CVD = fct_recode(as.factor(CVD), No = "0", Yes = "1"),
    WHF = fct_recode(as.factor(WHF), No = "0", Yes = "1"),
    HOSP = fct_recode(as.factor(HOSP), No = "0", Yes = "1"),
    DEATH = fct_recode(as.factor(DEATH), Alive = "0", Death = "1"),
    AGE = as.numeric(AGE),
    BMI = as.numeric(BMI)
  )

ui <- dashboardPage(
  dashboardHeader(title = tags$div(
    "Digitalis Investigation Group",
    style = "font-size: 24px; color: White; text-align: center; width: 100%;"),
    titleWidth = "100%"),
  dashboardSidebar(checkboxGroupInput(inputId = "sex", label = "Select Sex:", choices = c("Male", "Female"), selected = c("Male", "Female")),
                   sliderInput("age_range", "Select Age Range:", min = min(DIG.df$AGE, na.rm = TRUE), max = max(DIG.df$AGE, na.rm = TRUE), value = c(50, 80)),
                   sliderInput("bmi_range", "Select BMI Range:", min = min(DIG.df$BMI, na.rm = TRUE), max = max(DIG.df$BMI, na.rm = TRUE), value = c(20, 30)),
                   checkboxGroupInput(inputId = "hypertension", label = "Hypertension:", choices = c("Yes", "No"), selected = c("Yes", "No")),
                   checkboxGroupInput(inputId = "cvd", label = "Cardiovascular Disease:", choices = c("Yes", "No"), selected = c("Yes", "No")),
                   checkboxGroupInput(inputId = "hospitalization", label = "Hospitalization:", choices = c("Yes", "No"), selected = c("Yes", "No")),
                   checkboxGroupInput(inputId = "whf", label = "Worsening Heart Failure:", choices = c("Yes", "No"), selected = c("Yes", "No"))
  ),
  dashboardBody(tabBox(width = 12,id = "tabs",
                       tabPanel("Mortality Rate Plot",
                                fluidRow(
                                  box(width = 12,title = div("Mortality Rate by Treatment Group",style = "text-align: center; line-height: 30px; color: white;"),
                                      collapsible = TRUE,status = "success",solidHeader = TRUE,plotOutput("plot1"))
                                )),
                       tabPanel("Filtered Data",
                                fluidRow(
                                  box(width = 12,title = tags$div("Filtered Data Table",style = "text-align: center; line-height: 30px; color: white;"),
                                      collapsible = TRUE,status = "success",solidHeader = TRUE,dataTableOutput("table1"))
                                ))
  )
  ),
  skin = "green")
server <- function(input, output) {
  filtered_data <- reactive({
    DIG.df %>%
      filter(SEX %in% input$sex) %>%
      filter(AGE >= input$age_range[1] & AGE <= input$age_range[2]) %>%
      filter(BMI >= input$bmi_range[1] & BMI <= input$bmi_range[2]) %>%
      filter(HYPERTEN %in% input$hypertension) %>%
      filter(CVD %in% input$cvd) %>%
      filter(HOSP %in% input$hospitalization) %>%
      filter(WHF %in% input$whf)
  })
  output$plot1 <- renderPlot({
    filtered_data() %>%
      group_by(TRTMT) %>%
      summarise(Mortality_Rate = mean(as.numeric(DEATH == "Death"), na.rm = TRUE)) %>%
      ggplot(aes(x = TRTMT, y = Mortality_Rate, fill = TRTMT)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 1, width = 0.5) +
      geom_text(aes(label = scales::percent(Mortality_Rate, accuracy = 0.1)), 
                vjust = -0.3, size = 8) +
      scale_fill_manual(values = c("black", "skyblue")) +
      theme_classic() +
      labs(x = "Treatment Group",y = "Mortality Rate",fill = "Treatment Group") +
      theme(
        plot.title = element_text(color = "black", size = 20, face = "bold")
      ) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1))
  })
  
  output$table1 <- renderDataTable({
    filtered_data() %>%
      select(ID, TRTMT, AGE, SEX, BMI, KLEVEL, CREAT, DIABP, SYSBP, 
             HYPERTEN, CVD, WHF, DIG, HOSP, HOSPDAYS, DEATH, DEATHDAY)
  })
}
shinyApp(ui = ui, server)
