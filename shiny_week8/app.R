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

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Week 8 Data"),
  
  # Sidebar with a slider input for number of bins 
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

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)