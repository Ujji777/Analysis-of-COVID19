# Load required libraries
library(shiny)
library(tidyverse)
library(caret)
library(e1071)
library(plotly)


has_covid <- ifelse(temperature > 99 | cough_severity > 4, 1, 0)

# Create a dataframe
covid_data <- data.frame(Age = age, Temperature = temperature, Cough_Severity = cough_severity, Has_COVID = has_covid)

# Define UI
ui <- fluidPage(
  tags$style(type = "text/css", "body { background-color: black; color: white; }"),
  tags$style(type="text/css", "body {background-image: url('https://images.moneycontrol.com/static-mcnews/2021/04/coronavirus.jpg?impolicy%3Dwebsite%26width%3D1600%26height%3D900&imgrefurl=https://www.moneycontrol.com/news/trends/current-affairs/10-things-to-know-about-the-new-emerging-covid-variant-pirola-11295861.html&h=900&w=1600&tbnid=T4usGEJpjVctrM&tbnh=168&tbnw=300&usg=AI4_-kTQnbf6IQUigxr0gzDinnhGZRgMlQ&vet=1&docid=bbYmohL9gE3YpM'); background-size: cover;}"),
  
  titlePanel("COVID-19 PREDICTION APP"),
  
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        numericInput("age", "Select Age:", value = 40),
        numericInput("temperature", "Select Temperature:", value = 98.6),
        sliderInput("cough_severity", "Select Cough Severity:", min = 1, max = 10, value = 3),
        actionButton("predictButton", "Predict", style = "color: black; background-color: #008000;"),
        
        # Dropdown button for selecting graph type
        selectInput("graphType", "Select Graph Type:",
                    choices = c("Scatter Plot", "Bar Plot"),
                    selected = "Scatter Plot")
      ),
      wellPanel(
        p("Welcome to the COVID-19 Prediction App! This tool utilizes machine learning to predict your COVID-19 status based on input parameters such as age, temperature, and cough severity. Simply select your age, temperature, and rate your cough severity using the provided inputs. After that, click the 'Predict' button to get your result.

The scatter plot and bar plot below visualize the underlying data. The scatter plot shows the distribution of age, temperature, and cough severity, with different colors indicating the COVID-19 status. The bar plot displays the count of individuals with and without COVID-19.

Feel free to explore different scenarios and see how the model predicts the COVID-19 status. It's designed for educational purposes and to showcase the functionality of Shiny apps and machine learning integration."),
        style = "background-color: black;"
      )
    ),
    mainPanel(
      wellPanel(
        uiOutput("selectedPlot")
      ),
      textOutput("predictionText")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Reactive expression to store the model
  model <- reactive({
    svm(Has_COVID ~ ., data = covid_data)
  })
  
  # Predictions based on user input
  predictions <- eventReactive(input$predictButton, {
    new_data <- data.frame(
      Age = input$age,
      Temperature = input$temperature,
      Cough_Severity = input$cough_severity
    )
    
    predicted_value <- as.numeric(predict(model(), new_data))
    
    if (predicted_value >= 0.4) {
      return("Positive")
    } else {
      return("Negative")
    }
  })
  
  # Dynamic UI for selected plot
  output$selectedPlot <- renderUI({
    graph_type <- input$graphType
    
    if (graph_type == "Scatter Plot") {
      plotlyOutput("scatterPlot")
    } else if (graph_type == "Bar Plot") {
      plotlyOutput("barPlot")
    }
  })
  
  # Display predictions
  output$predictionText <- renderText({
    paste("Predicted COVID-19 Status:", predictions())
  })
  
  # Scatter plot
  output$scatterPlot <- renderPlotly({
    plot_ly(data = covid_data, x = ~Age, y = ~Temperature, z = ~Cough_Severity, color = ~factor(Has_COVID),
            type = "scatter3d", mode = "markers",
            marker = list(size = 5, opacity = 0.7),
            colors = c('#1f78b4', '#fe7f0e')) %>%
      layout(scene = list(backgroundcolor = "rgb(240,240,240)"))
  })
  
  # Bar plot
  output$barPlot <- renderPlotly({
    bar_data <- covid_data %>% group_by(Has_COVID) %>% summarize(count = n())
    plot_ly(data = bar_data, x = ~factor(Has_COVID), y = ~count, type = "bar",
            marker = list(color = c('#1f78b4', '#fe7f0e'))) %>%
      layout(scene = list(backgroundcolor = "rgb(240,240,240)"))
  })
}

# Run the application
shinyApp(ui,server)