
# 02-two-outputs.R
# ~ 01:00:00
#- copy the code from the `02-two-outputs.R` file (in the `two-outputs` code chunk below)
#- add a `textInput` widget that allows the user to change the title of the histogram (following code in `01-two-inputs.R`).  Update the code in the server() function appropriately.  Run the app to make sure it works as you expect.
#- update the layout of the app to use a `navlistPanel` structure (following the code in `06-navlist.R`).  Hint: put `navlistPanel` around the output objects only.
library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "num", 
              label = "Choose a number", 
              value = 25, min = 1, max = 100),
  textInput(inputId = "title", 
            label = "Write a title",
            value = "Histogram of Random Normal Values"),
  #Adding the panels and actions buttons
  navlistPanel(              
    tabPanel(title = "Normal data",
             plotOutput("norm"),
             actionButton("renorm", "Resample")
    ),
    tabPanel(title = "Uniform data",
             plotOutput("unif"),
             actionButton("reunif", "Resample")
    ),
    tabPanel(title = "Chi Squared data",
             plotOutput("chisq"),
             actionButton("rechisq", "Resample")
    )
  ),
  verbatimTextOutput("stats")
)

server <- function(input, output) {
  #defining reactive values
  rv <- reactiveValues(
    norm = rnorm(25), 
    unif = runif(25),
    chisq = rchisq(25, 2))
  
  observeEvent(input$renorm, { rv$norm <- rnorm(input$num) })
  observeEvent(input$reunif, { rv$unif <- runif(input$num) })
  #chisq needs degrees of freedom
  observeEvent(input$rechisq, { rv$chisq <- rchisq(input$num, 2) })
  #output plots
  output$norm <- renderPlot({
    hist(rv$norm, main = input$title)
  })
  output$unif <- renderPlot({
    hist(rv$unif, main = input$title)
  })
  output$chisq <- renderPlot({
    hist(rv$chisq, main = input$title)
  })
  output$stats <- renderPrint({
    summary(rnorm(input$num))
  })
}

shinyApp(ui = ui, server = server)
