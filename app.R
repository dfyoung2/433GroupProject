library(shiny)
source("C:\\stat 433\\433GroupProject\\Final Project\\diabetes data visualization.R")
source("C:\\stat 433\\433GroupProject\\Final Project\\predictive models.R")

ui <- fluidPage(
  titlePanel("Analysis of Fasting Plasma Glucose"),
  
  sidebarLayout(
    sidebarPanel(
      
      helpText("Select a metric to display its relation to Fasting Plasma Glucose."),
      #Selection input for varying levels of exercise
      selectInput("selected.var", 
                  label = "Metric",
                  choices = c("BMI", 
                              "Waist-Hip Ratio",
                              "Systolic Blood Pressure",
                              "Daily Fiber Intake"),
                  selected = "BMI"),
      
      #Selection Input for predictive models
      selectInput("Predictability", label = "Select a predicitability chart",
                   choices = c("BMI", "Waist-hip Ratio"),
                  selected = "BMI")
    ),
    
    mainPanel(plotOutput("selected.var"), plotOutput("Predictability"))
  )
)

# Server logic ----
server <- function(input, output) {
  output$selected.var <- renderPlot({
    metric <- switch(input$selected.var,
                       "BMI" = BMI_plot,
                       "Waist-Hip Ratio" = WHR_plot,
                       "Systolic Blood Pressure" = BPSY_plot,
                       "Daily Fiber Intake" = fiber_plot)
    metric 
  })
  output$Predictability <- renderPlot({
    prediction <- switch(input$Predictability, 
                         "BMI" = BMI_predict,
                         "Waist-hip Ratio" = WHR_predict)
    prediction 
  })

}

# Run app ----
shinyApp(ui, server)