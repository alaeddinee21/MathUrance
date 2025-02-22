library(shiny)
library(tidymodels)
library(tidyverse)
library(lubridate)
library(shinythemes)  # For additional themes

ui <- fluidPage(
  theme = shinytheme("flatly"),  # Choose a modern theme
  titlePanel(
    div(
      style = "display: flex; align-items: center; justify-content: space-between; padding: 20px; background: linear-gradient(120deg, #fdfbfb 0%, #ebedee 100%); border-radius: 10px;",
      div(
        style = "display: flex; align-items: center;",
        img(src = "https://media.licdn.com/dms/image/v2/D4E22AQHZLGYwc8CJyQ/feedshare-shrink_2048_1536/B4EZTtdoubHgAo-/0/1739150757196?e=1743033600&v=beta&t=nYO3eJWEf70Y8LIjSu7O0Y-q3UtMziMHH5o9Rp9i-SI", 
            height = 60, style = "margin-right: 20px;"),  # Left logo
        div(
          h1("Insurance Claim Predictor", style = "color: #2c3e50; font-weight: bold; margin: 0;"),
          h4("Actuarial Forecasting Tool", style = "color: #7f8c8d; margin: 0;")
        )
      ),
      img(src = "https://www.atlas-mag.net/sites/default/files/images/AtlasMagazine_2024-05-No211/Images/caat.jpg", height = 60, style = "margin-left: 20px;")  # Right logo
    )
  ),
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #f8f9fa; border-radius: 8px; padding: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
      tags$div(
        style = "margin-bottom: 25px;",
        icon("building", class = "fa-regular", style = "color: #3498db; margin-right: 10px;"),
        selectInput("sous_branche", "Select Insurance Branch:",
                    choices = c("Responsabilité Civile", 
                                "Incendie", 
                                "Risque simple", 
                                "CAT-NAT"),
                    width = "100%")
      ),
      tags$div(
        style = "margin-bottom: 25px;",
        icon("calendar", class = "fa-regular", style = "color: #e74c3c; margin-right: 10px;"),
        numericInput("year", "Claim Year:", 
                     value = year(Sys.Date()), 
                     min = 2000, max = year(Sys.Date()),
                     width = "100%")
      ),
      tags$div(
        style = "margin-bottom: 30px;",
        icon("chart-line", class = "fa-solid", style = "color: #2ecc71; margin-right: 10px;"),
        numericInput("dev_year", "Development Year (1-10):", 
                     value = 1, min = 1, max = 10,
                     width = "100%")
      ),
      actionButton("predict", "Generate Prediction", 
                   class = "btn-primary btn-block",
                   icon = icon("rocket", class = "fa-solid"))
    ),
    mainPanel(
      style = "padding: 20px; background-color: white; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
      tags$div(
        style = "margin-bottom: 30px;",
        h3("Prediction Results:", style = "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),
        div(
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 6px; min-height: 120px;",
          verbatimTextOutput("prediction", placeholder = TRUE) %>%
            tagAppendAttributes(style = "color: #2ecc71; font-size: 1.2em; font-weight: bold;")
        )
      ),
      tags$div(
        h3("Model Performance Metrics:", style = "color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),
        div(
          style = "overflow-x: auto;",
          tableOutput("metrics") %>%
            tagAppendAttributes(class = "table table-striped table-hover")
        )
      )
    )
  ),
  tags$footer(
    style = "text-align: center; padding: 15px; color: #7f8c8d; margin-top: 30px;",
    tags$div(
      style = "display: inline-block;",
      tags$img(src = "https://www.r-project.org/logo/Rlogo.png", 
               height = "40px", 
               style = "margin-right: 15px; vertical-align: middle;"),
      tags$span("Powered by R Shiny | Actuarial Science Toolkit", 
                style = "vertical-align: middle;")
    )
  )
)

server <- function(input, output) {
  # Load models
  models <- list(
    "Responsabilité Civile" = readRDS("responsabilite_model.rds"),
    "Incendie" = readRDS("incendie_model.rds"),
    "Risque simple" = readRDS("risk_model.rds"),
    "CAT-NAT" = readRDS("cat_model.rds")
  )
  
  # Metrics data (replace with your actual metrics)
  metrics_data <- tibble(
    category = c("Responsabilité Civile","Incendie","Risque simple","CAT-NAT"),
    MSE = c(3.394658e+15, 2.249107e+17, 4.148632e+15	, 1.361506e+04	)  # Replace with real values
  )
  
  observeEvent(input$predict, {
    req(input$sous_branche, input$year, input$dev_year)
    
    new_data <- tibble(
      Year = input$year,
      `Development Year` = input$dev_year
    )
    
    model <- models[[input$sous_branche]]
    pred <- predict(model, new_data)
    claim_amount <- exp(pred$.pred)
    
    output$prediction <- renderPrint({
      glue::glue("Predicted Claim Amount for {input$sous_branche}:
                 Year: {input$year}
                 Development Year: {input$dev_year}
                 Amount: {round(claim_amount, 2)} DZD")
    })
  })
  
  output$metrics <- renderTable({
    metrics_data %>% 
      filter(category == input$sous_branche) %>% 
      select(MSE)
  })
}

shinyApp(ui, server)