#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)

# Import data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
week8_tbl <- readRDS("week8_shiny.rds")

ui <- fluidPage(
  
  titlePanel("Week 8 Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      # Creates a toggle the user can use to select which rows to display based on participant's gender
      radioButtons(inputId = "genderSelect", label = "Gender", choices = c("Male", "Female", "All"), selected = "All"),
      # Creates a toggle the user can use to show or hide the error band
      radioButtons(inputId = "displayErrorBand", label = "Error Band", choices = c("Display Error Band" = "Y", "Suppress Error Band" = "N"), selected =  "Y"),
      # Creates a toggle the user can use to show or hide points for users who completed the assessment before July 1, 2017
      radioButtons(inputId = "showEarlyParticipants", label = "Show participants who completed assessment before July 1, 2017?", choices = c("Yes" = "Y", "No" = "N"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      # Displays the "scatterOSL" plot created in the server code
      plotOutput("scatterOLS")
    )
  )
)

server <- function(input, output) {
  output$scatterOLS <- renderPlot({
    # Check the input "genderSelect" defined in the UI code to determine whether any rows should be filtered based on gender
    if (input$genderSelect == "All") {
      filtered_tbl <- week8_tbl
    } else {
      filtered_tbl <- week8_tbl %>% filter(gender == input$genderSelect)
    }
    
    # Determine if the error band should be displayed based on the "displayErrorBand" input from the ui code
    displayErrorBand <- input$displayErrorBand == "Y"
    
    # Determine if early participants (completed assessment prior to July 1, 2017) should be filtered
    if (input$showEarlyParticipants == "N") {
      filtered_tbl <- filtered_tbl %>% filter(timeStart >= as.POSIXct("2017-07-01"))
    }
    
    # Display the plot with the settings determined above
    filtered_tbl %>%
      ggplot(mapping = aes(x = x_bar, y = y_bar)) +
      geom_point() +
      geom_jitter() +
      geom_smooth(method = "lm", se = displayErrorBand, fill = "purple")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)