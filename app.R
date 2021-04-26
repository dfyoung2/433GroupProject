library(shiny)
source("C:\\stat 433\\433GroupProject\\nutrition.R")
source("C:\\stat 433\\433GroupProject\\nutrition database\\nutrition database\\part 3.R")

ui <- fluidPage(
  titlePanel("Nutrition Info"),
  
  sidebarLayout(
    sidebarPanel(
      
      #Help text to help user understand what each level of exercise means
      helpText("Sedentary: little to no exercise + work a desk job"),
      helpText("Lightly Active: light exercise 1-3 days per week"),
      
      helpText("Moderately Active: moderate exercise 3-5 days per week"),
      
      helpText("Very Active: heavy exercise 6-7 days per week"),
      helpText("Extremely Active: very heavy exercise, hard labor job, training 2x per day"),
      
      #Selection input for varying levels of exercise
      selectInput("activity.level", 
                  label = "Choose your rate of exercise",
                  choices = c("Sedentary", "Lightly Active", "Moderately Active",
                              "Very Active", "Extremely Active"),
                  selected = "Sedentary"),
      
      #Numeric input for the user's weight in pounds
      numericInput("weight", 
                  label = "Select your weight (pounds)", value = 150,
                  min = 0, max = 400, step = 1),

      #Numeric input of the user's abdomen(waist) size in centimeters                  
      numericInput("abdomen.size",
                   label = "Select your waist size (centimeters, multiply waist circumference, in inches by 2.54)",
                   value = 30, min = 10, max = 100, step = 1),
      
      #Radio button input so the user can select their goal of fat loss, weight maintenance, or muscle gain
      selectInput("goal", label = "What's your goal?",
                   choices = c("fat loss", "weight maintenance", "muscle gain"))
    ),
    
    mainPanel(textOutput("responses"))
  )
)

# Server logic ----
server <- function(input, output) {
  output$responses <- renderText({
    activity <- switch(input$activity.level,
                       "Sedentary" = 1.2,
                       "Lightly Active" = 1.375,
                       "Moderately Active" = 1.55,
                       "Very Active" = 1.725,
                       "Extremely Active" = 1.9)
    objective <- switch(input$goal, 
                     "fat loss" = 1,
                     "weight maintenance" = 2,
                     "muscle gain" = 3)
    
    n.info <- nutrition(input$weight, input$abdomen.size, activity, objective)
    
    paste("BF%: ",n.info[1],
           "  LBM: ",n.info[2], 
           "  BMR: ",n.info[3],
           "  TDEE: ",n.info[4],
           "  Calories:", n.info[5],
           "  Protein: ",n.info[6], "grams",
           "  Carbs: ",n.info[7], "grams",
           "  Fat: ",n.info[8], "grams")
  })
}

# Run app ----
shinyApp(ui, server)