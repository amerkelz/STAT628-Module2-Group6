library(shiny)
library(bslib)
library(ggplot2)

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
          max = 50,
          value = 30
          ),
        textOutput(outputId = 'var1Warning')
        ),
        card(
          numericInput(
          inputId = "var2",
          label = "Factor2",
          min = 1,
          max = 50,
          value = 30
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
  output$BodyFatPlot <- renderPlot({
    set.seed(1)
    df <- data.frame(x=rnorm(200),y=rnorm(200))
    ggplot(df, aes(x=x,y=y)) + geom_density2d_filled()
  })
}

shinyApp(ui = ui, server = server) 

