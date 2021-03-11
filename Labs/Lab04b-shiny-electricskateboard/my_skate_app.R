library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggrepel)
library(fivethirtyeight)

###############
# import data #
###############
candy <- fivethirtyeight::candy_rankings

#############################################################
# define choice values and labels for widgets (user inputs) #
#############################################################
# define vectors for choice values and labels 
# can then refer to them in server as well (not just in defining widgets)

# for TAB 1 (HISTOGRAM) widgets: 
# for selectInput, 'choices' object should be a NAMED LIST
bar_choice_values <- c("chocolate","fruity","caramel","peanutyalmondy","nougat", "crispedricewafer","hard","bar","pluribus")
bar_choice_names <- c("Chocolate","Fruit","Caramel","Peanut/Almond", "Nougat", "Cripsed/Rice Wafer","Hard","Bar","Pluribus")
names(bar_choice_values) <- bar_choice_names


# for TAB 2 (SCATTERPLOT) widgets: 
# for radio button in scatterplot tab
scatter_choice_values <- c("sugarpercent", "pricepercent", "winpercent")
scatter_choice_names <- c("Sugar%", "Price%", "Win%")
names(scatter_choice_values) <- scatter_choice_names

# for selectizeInput choices for competitor name name, pull directly from data
name_choices <- unique(candy$competitorname)

# for TAB 3 (TABLE) widgets: 
# for selectizeInput choices for competitor name, pull directly from data
brand_choices <- unique(candy$competitorname)

############
#    ui    #
############
ui <- navbarPage(
  
  title="Different Candy",
  #This is for the bar graph, we can change the variable on x-axis
  tabPanel(
    title = "Bar Chart",
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "barvar"
                    , label = "Choose a variable of interest to plot:"
                    , choices = bar_choice_values
                    , selected = "chocolate")
      ),
      mainPanel(
        plotOutput(outputId = "bar")
      )
    )
  ),
  #This chunk is for the scatter plot
   tabPanel(
    title = "Scatterplot",
    
     sidebarLayout(
      
      sidebarPanel(
        radioButtons(inputId = "x_axis"
                      , label = "Graph points by:"
                      , choices = scatter_choice_values
                      , selected = "sugarpercent"),
         selectizeInput(inputId = "id_name"
                     , label = "Identify candy(s) in the scatterplot:"
                     , choices = name_choices
                     , selected = NULL
                     , multiple = TRUE)
       ),
       mainPanel(
         plotOutput(outputId = "scatter")
       )
     )
   ),
  #This chunk is for the table
  tabPanel(
    title = "Table",
    
     sidebarLayout(
       sidebarPanel(
         selectizeInput(inputId = "brand"
                     , label = "Choose one or more brands:"
                     , choices = brand_choices
                     , selected = "100 Grand"
                     , multiple = TRUE)
       ),
       mainPanel(
         DT::dataTableOutput(outputId = "table")
       )
     )
  )
)

############
# server   #
############
server <- function(input,output){
  
  # TAB 1: Bar chart
  data_for_bar <- reactive({
    data <- candy
  })
  
  output$bar <- renderPlot({
    ggplot(data = data_for_bar(), aes_string(x = input$barvar)) +
      geom_bar(color = "#2c7fb8", fill = "#7fcdbb", alpha = 0.7) +
      labs(x = bar_choice_names[bar_choice_values == input$barvar]
           , y = "Number of Candies")
  })
  
  # TAB 2: SCATTERPLOT 
   output$scatter <- renderPlot({
     candy %>%
     ggplot(aes_string(x=input$x_axis, y="winpercent")) +
       geom_point(color = "#2c7fb8") +
       labs(x = scatter_choice_names[scatter_choice_values == input$x_axis] 
            , y = "Win Percentage"
            , title = "Candy Competition", subtitle = "538") +
       geom_label_repel(data = filter(candy, competitorname %in% input$id_name)
                        , aes(label = competitorname), show.legend = FALSE)
     })
  
  # TAB 3: TABLE
   data_for_table <- reactive({
     data <- filter(candy, competitorname %in% input$brand)
   })
  
   output$table <- DT::renderDataTable({ 
     data_for_table()
  })
  
}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)

# Your turn.  Copy this code as a template into a new app.R file (WITHIN A FOLDER
# named something different than your other Shiny app folders).  Then, either 
# (1) update this template to still explore the skateboards dataset, but with
#     different app functionality (e.g. different widgets, variables, layout, theme...); 
#   OR
# (2) use this as a template to create a Shiny app for a different dataset 
#     from the fivethirtyeight package:
#     either candy_rankings (candy characteristics and popularity)
#            hate_crimes (hate crimes in US states, 2010-2015)
#            mad_men (tv performers and their post-show career), 
#            ncaa_w_bball_tourney (women's NCAA div 1 basketball tournament, 1982-2018), 
#         or nfl_suspensions (NFL suspensions, 1946-2014)
#      these five datasets are part of the fivethirtyeight package
#      and their variable definitions are included in pdfs posted to the Moodle course page
