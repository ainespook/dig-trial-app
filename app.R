
library(rsconnect)
library(plotly)
library(forcats)
library(readr)
library(dplyr)
library(table1)
library(ggplot2)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(Stat2Data)

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
    BMI = as.numeric(BMI),
    DEATHDAY = as.numeric(DEATHDAY)
  )

ui <- dashboardPage(
  dashboardHeader(title = tags$div("Digitalis Investigation Group",
                                   style = "font-size: 24px; color: white; text-align: center; width: 100%;"),
                  titleWidth = "100%"),
  dashboardSidebar(
    checkboxGroupInput(inputId = "sex", label = "Select Sex:", choices = c("Male", "Female"), selected = c("Male", "Female")),
    checkboxGroupInput(inputId = "TRTMT", label = "Select Group:", choices = c("Placebo", "Treatment"), selected = c("Placebo", "Treatment")),
    sliderInput("age_range", "Select Age Range:", min = min(DIG.df$AGE, na.rm = TRUE), 
                max = max(DIG.df$AGE, na.rm = TRUE), 
                value = c(min(DIG.df$AGE, na.rm = TRUE), max(DIG.df$AGE, na.rm = TRUE))),
    sliderInput("bmi_range", "Select BMI Range:", min = min(DIG.df$BMI, na.rm = TRUE), 
                max = max(DIG.df$BMI, na.rm = TRUE), value = c(min(DIG.df$BMI, na.rm = TRUE), max(DIG.df$BMI, na.rm = TRUE))),
    checkboxGroupInput(inputId = "hypertension", label = "Hypertension:", choices = c("Yes", "No"), selected = c("Yes", "No")),
    checkboxGroupInput(inputId = "cvd", label = "Cardiovascular Disease:", choices = c("Yes", "No"), selected = c("Yes", "No")),
    checkboxGroupInput(inputId = "hospitalization", label = "Hospitalization:", choices = c("Yes", "No"), selected = c("Yes", "No")),
    checkboxGroupInput(inputId = "whf", label = "Worsening Heart Failure:", choices = c("Yes", "No"), selected = c("Yes", "No"))
  ),
  
  dashboardBody(tabBox(width = 12,id = "tabs",
                       tabPanel("DIG Information",
                                fluidRow(
                                  box(width = 12,title = tags$div("DIG Trial Information",style = "text-align: center; font-weight: bold; color: white;"),
                                      collapsible = TRUE,status = "success",solidHeader = TRUE,
                                      tags$div(
                                        style = "padding: 15px;",
                                        tags$p(
                                          "The DIG (Digitalis Investigation Group) Trial was a randomized, double-blind, multicenter trial with more than 300 centers in the United States and Canada participating. 
                The purpose of the trial was to examine the safety and efficacy of Digoxin in treating patients with congestive heart failure in sinus rhythm."
                                        ),
                                        tags$p(
                                          "Digitalis was introduced clinically more than 200 years ago and has since become a commonly prescribed medication for the treatment of heart failure; however, there was considerable uncertainty surrounding its safety and efficacy."
                                        ),
                                        tags$p(
                                          "Small trials indicated that Digoxin alleviated some of the symptoms of heart failure, prolonged exercise tolerance, and generally improved the quality of patients’ lives. 
                Unfortunately, these trials were generally small and although they did focus on the effect of treatment on patients’ relief from heart failure symptoms and quality of life, they failed to address the effect of treatment on cardiovascular outcomes."
                                        ),
                                        tags$p(
                                          "Questions about the safety of Digoxin were also a concern. Digoxin toxicity is uncommon in small trials with careful surveillance; however, the long-term effects of therapeutic levels of Digoxin were less clear."
                                        )
                                      )
                                  )
                                )
                       ),
                       tabPanel(
                         "Scatter Plot",
                         fluidRow(
                           box(width = 12,title = tags$div("Scatter Plot",
                                                           style = "text-align: center; color: white;"
                           ),
                           collapsible = TRUE,status = "success",solidHeader = TRUE,
                           selectInput("y_var", "Select Y-Axis Variable:", 
                                       choices = c("BMI", "SYSBP", "DIABP", "KLEVEL"), selected = "BMI"),
                           plotlyOutput("scatter_plot")
                           )
                         )
                       ),
                       tabPanel(
                         "Mortality Rate",
                         fluidRow(
                           box(width = 12,title = tags$div("Mortality Rate by Treatment Group",
                                                           style = "text-align: center; line-height: 30px; color: white;"
                           ),
                           collapsible = TRUE,status = "success",solidHeader = TRUE,plotOutput("plot1")
                           )
                         )
                       ),
                       tabPanel(
                         "Data",
                         fluidRow(
                           box(width = 12,title = tags$div("Data Table",
                                                           style = "text-align: center; line-height: 30px; color: white;"
                           ),
                           collapsible = TRUE,status = "success",solidHeader = TRUE,dataTableOutput("table1")
                           )
                         )
                       )
  )
  ),
  skin = "green"
)

server <- function(input, output) {
  filtered_data <- reactive({
    DIG.df %>%
      filter(SEX %in% input$sex) %>%
      filter(AGE >= input$age_range[1] & AGE <= input$age_range[2]) %>%
      filter(BMI >= input$bmi_range[1] & BMI <= input$bmi_range[2]) %>%
      filter(HYPERTEN %in% input$hypertension) %>%
      filter(CVD %in% input$cvd) %>%
      filter(HOSP %in% input$hospitalization) %>%
      filter(WHF %in% input$whf) %>%
      filter(TRTMT %in% input$TRTMT)
  })
  
  output$scatter_plot <- renderPlotly({
    data <- filtered_data()
    p <- ggplot(data, aes(x = AGE, y = .data[[input$y_var]], color = TRTMT)) +
      geom_point(alpha = 0.7, size = 1.5) +
      scale_color_manual(values = c("Placebo" = "turquoise", "Treatment" = "salmon")) +
      theme_classic() +
      labs(x = "Age", y = input$y_var, color = "Group", title = paste("Scatter Plot of", input$y_var, "vs Age"))
    ggplotly(p)
  })
  
  output$plot1 <- renderPlot({
    data <- filtered_data()
    total_participants <- data %>%
      group_by(TRTMT) %>%
      summarize(total_in_group = n_distinct(ID)) %>%
      ungroup()
    mortality_data <- data %>%
      filter(DEATH == "Death") %>%
      mutate(time_interval = floor(DEATHDAY / 30)) %>%
      group_by(TRTMT, time_interval) %>%
      summarize(
        deaths_in_month = n()
      ) %>%
      left_join(total_participants, by = "TRTMT") %>%
      mutate(
        mortality_rate = (deaths_in_month / total_in_group) * 100
      ) %>%
      ungroup()
    
    ggplot(mortality_data, aes(x = time_interval, y = mortality_rate, color = TRTMT, group = TRTMT)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(
        x = "Months",y = "Mortality Rate (%)",color = "Treatment Group") +
      theme_classic(base_size = 15) +
      scale_color_manual(values = c("Placebo" = "red", "Treatment" = "black"))
  })
  
  
  output$table1 <- renderDataTable({
    filtered_data() %>%
      select(
        ID, TRTMT, AGE, SEX, BMI, KLEVEL, CREAT, DIABP, SYSBP,
        HYPERTEN, CVD, WHF, DIG, HOSP, HOSPDAYS, DEATH, DEATHDAY
      )
  }, options = list(pageLength = 10,lengthMenu = c(5, 10, 25, 50),scrollX = TRUE,autoWidth = TRUE
  ))
}

shinyApp(ui = ui, server = server)