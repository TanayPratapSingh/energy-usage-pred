library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(readr)
library(ranger)  # for predict()

# Load cleaned data and model
sample_df <- readRDS("sample_df_small.rds")
final_rf_model <- readRDS("final_rf_model_fast_clean.rds")

# Source plotting function
source("05_EDA.R")

# ---- UI ----
ui <- fluidPage(
  
  titlePanel("July Energy Demand Forecasting – eSC Energy"),
  
  fluidRow(
    
    # ---- Left Panel: Graphs and Data Table ----
    column(
      width = 6,
      h3("Explore Energy Usage Patterns"),
      
      selectInput(
        "selected_plot",
        "Select a Graph:",
        choices = c(
          "",
          "Energy Usage vs House Size (sqft)",
          "Energy Usage vs Income",
          "Energy Usage by Heating Type"
        ),
        selected = ""
      ),
      
      plotOutput("dynamic_plot", height = "400px"),
      uiOutput("plot_description"),
      
      br(),
      h4("View Sample Data"),
      numericInput("n_rows", "Number of rows to display:", value = 5, min = 1, max = nrow(sample_df)),
      dataTableOutput("data_preview")
    ),
    
    # ---- Right Panel: Prediction ----
    column(
      width = 6,
      h3("Energy Usage Prediction – July"),
      
      numericInput("cooling_setpoint", "Cooling Setpoint (°F)(60–85):", value = 72, min = 60, max = 85),
      numericInput("house_size", "House Size (sqft)(500–5000):", value = 2000, min = 500, max = 5000),
      numericInput("occupants", "Number of Occupants (1–10):", value = 3, min = 1, max = 10),
      numericInput("income", "Household Income ($)(10K–200K):", value = 50000, min = 10000, max = 200000),
      numericInput("temp", "Outside Temperature (°F)(70–110):", value = 85, min = 70, max = 110),
      
      br(),
      actionButton("predictBtn", "Predict Energy Usage"),
      br(), br(),
      
      h4("Predicted Energy Usage for July (kWh):"),
      verbatimTextOutput("prediction_output"),
      helpText("This value represents the predicted average **daily** energy usage (in kWh) for a home in July, based on the input values provided above.")
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  
  # ---- Graph Output ----
  output$dynamic_plot <- renderPlot({
    req(input$selected_plot)
    
    if (input$selected_plot == "Energy Usage vs House Size (sqft)") {
      plot_energy_relationship(sample_df, "in.sqft", "scatter")
      
    } else if (input$selected_plot == "Energy Usage vs Income") {
      plot_energy_relationship(sample_df, "in.income", "scatter")
      
    } else if (input$selected_plot == "Energy Usage by Heating Type") {
      plot_energy_relationship(sample_df, "in.hvac_heating_type", "box")
    }
  })
  
  # ---- Graph Descriptions ----
  output$plot_description <- renderUI({
    desc <- switch(input$selected_plot,
                   "Energy Usage vs House Size (sqft)" = "This scatter plot shows how total energy usage changes with home size (sqft). Larger homes tend to use more energy.",
                   "Energy Usage vs Income" = "This plot explores how energy usage varies by household income level.",
                   "Energy Usage by Heating Type" = "This boxplot compares energy usage by type of heating system, highlighting efficiency differences.",
                   NULL
    )
    if (!is.null(desc)) helpText(desc)
  })
  
  # ---- Data Table Output ----
  output$data_preview <- DT::renderDataTable({
    head(sample_df, input$n_rows)
  })
  
  # ---- Prediction Logic ----
  observeEvent(input$predictBtn, {
    new_data <- data.frame(
      in.cooling_setpoint = input$cooling_setpoint,
      in.sqft = input$house_size,
      in.occupants = input$occupants,
      in.income = input$income,
      temperature_c = input$temp
    )
    
    pred <- predict(final_rf_model, data = new_data)$predictions
    
    output$prediction_output <- renderText({
      paste0(round(pred, 2), " kWh")
    })
  })
}


shinyApp(ui = ui, server = server)
