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
          label = "Factor1",
          min = 1,
          max = 100,
          value = 80
          ),
        textOutput(outputId = 'var1Warning')
        ),
        card(
          numericInput(
          inputId = "var2",
          label = "Factor2",
          min = 1,
          max = 50,
          value = 20
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
  warningText1 <- reactive({ifelse((input$var1 <= 10)|(input$var1 > 40), 'WARNING: Var1 outside normal range', '')})
  warningText2 <- reactive({ifelse((input$var2 <= 10)|(input$var2 > 40), 'WARNING: Var2 outside normal range','')})
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
  model <- lm(BODYFAT ~ WRIST + ABDOMEN, data = data)
  # Create grid data for plotting
  grid_data <- expand.grid(
    WRIST = seq(min(data$WRIST), max(data$WRIST), length.out = 100),
    ABDOMEN = seq(min(data$ABDOMEN), max(data$ABDOMEN), length.out = 100)
  )
  
  # Predict body fat values for the grid
  grid_data$predicted_body_fat <- predict(model, newdata = grid_data)
  grid_data <- grid_data %>%
    mutate(body_fat_category = case_when(
      predicted_body_fat > 20 ~ "Obese (>20%)",
      predicted_body_fat >= 13 & predicted_body_fat <= 20 ~ "Standard (13%-20%)",
      predicted_body_fat >= 5 & predicted_body_fat < 13 ~ "Lean (5%-13%)",
      TRUE ~ "Underweight (<5%)"  # Include an additional category if needed
    ))
  
  output$BodyFatPlot <- renderPlot({
    
    ggplot(grid_data, aes(x = ABDOMEN, y = WRIST)) +
      geom_tile(aes(fill = body_fat_category)) +  
      geom_contour(aes(z = predicted_body_fat), color = "black", size = 0.5) +  
      scale_fill_manual(values = c("Lean (5%-13%)" = "blue", 
                                   "Standard (13%-20%)" = "purple", 
                                   "Obese (>20%)" = "red"),
                        name = "Body Fat Category") +
      labs(title = "Estimated Body Fat Percentage",
           x = "Abdomen(cm)",
           y = "Wrist  (cm)") +
      theme_minimal() +
      theme(
        axis.title.y = element_text(angle = 360, vjust = 0.5, hjust = 0.5)  # rotate y axis lable
      ) + geom_point(aes(x=input$var1,input$var2,size=20,color='white'))
  })
}

shinyApp(ui = ui, server = server) 

