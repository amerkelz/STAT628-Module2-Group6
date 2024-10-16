library(shiny)
library(bslib)
library(ggplot2)
library(tidyr)
library(dplyr)

# Define UI for app ----
ui <- page_fluid(
  titlePanel("Adult Male Body Fat Estimator - STAT 628 Group 6"),
  fluidRow(
    card(
      card_header("About this Page"),
      helpText(
        "This is a body fat estimator developed by STAT 628 students at UW-Madison. The intent of this tool is to estimate body fat percentage of an adult male person based on certain inputs.",
        br(),
        "Please note that the data set used to develop this model was limited, and results may be inaccurate, especially for extreme measurements."
        
      )
      
    )
  ),
  fluidRow(
    column(4,
        card(
        numericInput(
          inputId = "var1",
          label = "Abdomen Size (cm)",
          min = 40,
          max = 200,
          value = 95
          ),
        textOutput(outputId = 'var1Warning')
        ),
        card(
          numericInput(
          inputId = "var2",
          label = "Weight (kg)",
          min = 40,
          max = 220,
          value = 80
          ),
          textOutput(outputId = 'var2Warning')
        ),
        card(
          card_header("Your Body Fat Estimate:"),
          textOutput(outputId = "BodyFatEst")
        )
      ),
    column(8,
           card(
             plotOutput(outputId = "BodyFatPlot"),
           )
    )
  ),
  fluidRow(
    card(
      card_header("About this Model"),
      helpText("This body fat percentage model was developed using data from 252 men. It is a linear model with 2 factors: abdomen circumference (cm) and weight (kg)",
               br(),
               "Equation:",
               br(),
               "Body Fat % = -36.92 +0.88*Abdomen -0.31*Weight")
    ),
    card(
      card_header("References"),
      helpText(
        "The body fat category ranges in the contour plot are from the American Council on Exercise (https://www.msn.com/en-us/health/wellness/body-fat-percentage-charting-averages-in-men-and-women/ar-BB1iVeuN)",
        br(),
        "Data to develop this model was provided by Prof. Kang for the Fall 2024 STAT 628 course.")
    ),
    card(
      card_header("Contact Information"),
      helpText(
        "The model developers can be contacted via email at: amerkelz@wisc.edu; zhang2873@wisc.edu; cjiang232@wisc.edu")
      )
  )
)

server <- function(input, output) {
  #display warning text is var1, var2 inputs outside 'normal' ranges
  warningText1 <- reactive({ifelse((input$var1 <= 60)|(input$var1 > 150), 'OUTSIDE TYPICAL RANGE: Check your abdomen measurement', '')})
  warningText2 <- reactive({ifelse((input$var2 <= 50)|(input$var2 > 175), 'OUTSIDE TYPICAL RANGE: Check your weight measurement','')})
  output$var1Warning <- renderText({warningText1()})
  output$var2Warning <- renderText({warningText2()})
  
  #input data for body fat prediction and contour plot
  data_clean2 <- read.csv("data_clean_2_for_shiny.csv",sep=";",dec=",")
  data <- data_clean2
  model <- lm(BODYFAT ~ ABDOMEN + WEIGHT_kg, data = data)
  
  #calculate and display body fat percentage estimate
  bodyFat <- reactive({round(model$coefficients[1] + model$coefficients[2]*input$var1 + model$coefficients[3]*input$var2, 1)})
  output$BodyFatEst <- renderText({
    paste(bodyFat(), "%", sep="")
  })
  
  # Create grid data for plotting
  grid_data <- expand.grid(
    WEIGHT_kg = seq(min(data$WEIGHT_kg), max(data$WEIGHT_kg), length.out = 100),
    ABDOMEN = seq(min(data$ABDOMEN), max(data$ABDOMEN), length.out = 100)
  )
  
  # Predict body fat values for the grid
  grid_data$predicted_body_fat <- model$coefficients[1]+model$coefficients[2]*grid_data$ABDOMEN+model$coefficients[3]*grid_data$WEIGHT_kg #predict(model, newdata = grid_data)
  grid_data <- grid_data %>%
    mutate(body_fat_category = case_when(
      predicted_body_fat > 25 & predicted_body_fat < 60 ~ "Above Average/Obese (>25%)",
      predicted_body_fat >= 18 & predicted_body_fat <= 25 ~ "Average (18-24%)",
      predicted_body_fat >= 14 & predicted_body_fat < 18 ~ "Fit (14-17%)",
      predicted_body_fat >= 6 & predicted_body_fat < 14 ~ "Athlete (6-13%)",
      predicted_body_fat >= 2 & predicted_body_fat < 6 ~ "Essential Fat Only (2-5%)",
      predicted_body_fat < 2 | predicted_body_fat >= 50 ~ "Check measurements"  # Include an additional category if needed
    ))
  
  output$BodyFatPlot <- renderPlot({
    
    ggplot(grid_data, aes(y = WEIGHT_kg, x = ABDOMEN)) +
      geom_tile(aes(fill = body_fat_category)) +  
      geom_contour(aes(z = predicted_body_fat), color = "black", size = 0.5) +  
      scale_fill_manual(values = c("Check measurements"= "darkgrey" ,
                                   "Essential Fat Only (2-5%)" = "orange", 
                                   "Athlete (6-13%)" = "limegreen", 
                                   "Fit (14-17%)" = "forestgreen",
                                   "Average (18-24%)" = "gold",
                                   "Above Average/Obese (>25%)" = 'red'),
                        breaks = c("Check measurements", "Essential Fat Only (2-5%)", "Athlete (6-13%)", "Fit (14-17%)","Average (18-24%)","Above Average/Obese (>25%)"),
                        name = "Body Fat Category") +
      labs(title = "Estimated Body Fat Percentage",
           y = "Weight (kg)",
           x = "Abdomen (cm)") +
      theme_minimal() +
      theme(
        axis.title.y = element_text(angle = 360, vjust = 0.5, hjust = 0.5),  # rotate y axis label
        legend.text=element_text(size=14),
        legend.title=element_text(size=12)
      )+ geom_point(aes(x=input$var1,y=input$var2),size=6,color='black') + geom_text(aes(x=input$var1,y=input$var2),label='Your Body Fat %',color='black',nudge_x = 7.5,nudge_y= -3.5)
  })
}

shinyApp(ui = ui, server = server) 

