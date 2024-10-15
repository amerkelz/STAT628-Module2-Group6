library(shiny)
library(bslib)
library(ggplot2)
library(tidyr)
library(dplyr)
# Define UI for app ----
ui <- page_fluid(
  titlePanel("Male Body Fat Estimator - STAT 628 Group 6"),
  fluidRow(
    card(
      card_header("About this Page"),
      helpText(
        "This is a body fat estimator developed by STAT 628 students at UW-Madison. 
        The intent of this tool is to estimate bodyfat percentage of a male person based on certain inputs. Some reference ranges are also provided for convenience."
      )
      
    )
  ),
  fluidRow(
    column(5,
        card(
        numericInput(
          inputId = "var1",
          label = "Abdomen Size (cm)",
          min = 40,
          max = 200,
          value = 100
          ),
        textOutput(outputId = 'var1Warning')
        ),
        card(
          numericInput(
          inputId = "var2",
          label = "Weight (kg)",
          min = 40,
          max = 220,
          value = 90
          ),
          textOutput(outputId = 'var2Warning')
        ),
        card(
          card_header("Your Body Fat Estimate:"),
          textOutput(outputId = "BodyFatEst")
        )
      ),
    column(7,
           card(
             card_header("Body Fat Ranges"),
             plotOutput(outputId = "BodyFatPlot"),
             card_footer("Reference Ranges from American Council on Exercise (ACE)")
           )
    )
  ),
  fluidRow(
    card(
      card_header("About this Model"),
      helpText("add model description here")
    ),
    card(
      card_header("References"),
      helpText(
        "The body fat ranges table: American Council on Exercise (https://www.msn.com/en-us/health/wellness/body-fat-percentage-charting-averages-in-men-and-women/ar-BB1iVeuN)")
    )
  )
)

server <- function(input, output) {
  #display warning text is var1, var2 inputs outside 'normal' ranges
  warningText1 <- reactive({ifelse((input$var1 <= 60)|(input$var1 > 150), 'OUTSIDE NORMAL RANGE: Check your abdomen measurement', '')})
  warningText2 <- reactive({ifelse((input$var2 <= 50)|(input$var2 > 175), 'OUTSIDE NORMAL RANGE: Check your weight measurement','')})
  output$var1Warning <- renderText({warningText1()})
  output$var2Warning <- renderText({warningText2()})
  
  #calculate and display body fat percentage estimate
  bodyFat <- reactive({input$var1 + input$var2})
  output$BodyFatEst <- renderText({
    paste(input$var1,"+",input$var2,"=", bodyFat())
  })
  
  #display contour plot
  data_clean2 <- read.csv("data_clean_2.csv",sep=";",dec=",")
  
  data <- data_clean2
  model <- lm(BODYFAT ~ WEIGHT_kg + ABDOMEN, data = data)
  # Create grid data for plotting
  grid_data <- expand.grid(
    WEIGHT_kg = seq(min(data$WEIGHT_kg), max(data$WEIGHT_kg), length.out = 100),
    ABDOMEN = seq(min(data$ABDOMEN), max(data$ABDOMEN), length.out = 100)
  )
  
  # Predict body fat values for the grid
  grid_data$predicted_body_fat <- predict(model, newdata = grid_data)
  grid_data <- grid_data %>%
    mutate(body_fat_category = case_when(
      predicted_body_fat > 25 ~ "Obese (>25%)",
      predicted_body_fat >= 18 & predicted_body_fat <= 25 ~ "Average (18-24%)",
      predicted_body_fat >= 14 & predicted_body_fat < 18 ~ "Fit (14-17%)",
      predicted_body_fat >= 6 & predicted_body_fat < 14 ~ "Athlete (6-13%)",
      predicted_body_fat >= 2 & predicted_body_fat < 6 ~ "Essential Fat Only (2-5%)",
      predicted_body_fat < 2 ~ "Impossible - check measurements (<2%)"  # Include an additional category if needed
    ))
  
  output$BodyFatPlot <- renderPlot({
    
    ggplot(grid_data, aes(x = ABDOMEN, y = WEIGHT_kg)) +
      geom_tile(aes(fill = body_fat_category)) +  
      geom_contour(aes(z = predicted_body_fat), color = "black", size = 0.5) +  
      scale_fill_manual(values = c("Impossible - check measurements (<2%)"= "darkgrey" ,
                                   "Essential Fat Only (2-5%)" = "orange", 
                                   "Athlete (6-13%)" = "limegreen", 
                                   "Fit (14-17%)" = "forestgreen",
                                   "Average (18-24%)" = "gold",
                                   "Obese (>25%)" = 'red'),
                        breaks = c("Impossible - check measurements (<2%)", "Essential Fat Only (2-5%)", "Athlete (6-13%)", "Fit (14-17%)","Average (18-24%)","Obese (>25%)"),
                        name = "Body Fat Category") +
      labs(title = "Estimated Body Fat Percentage",
           x = "Abdomen (cm)",
           y = "Weight (kg)") +
      theme_minimal() +
      theme(
        axis.title.y = element_text(angle = 360, vjust = 0.5, hjust = 0.5)  # rotate y axis label
      )+ geom_point(aes(x=input$var1,input$var2),size=6,color='black') + geom_text(aes(x=input$var1,input$var2),label='Your Body Fat %',color='black',nudge_x = 7,nudge_y= -3)
  })
}

shinyApp(ui = ui, server = server) 

