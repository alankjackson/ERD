#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(tidyverse)
library(stringr)
library(leaflet)
library(shiny)

path <- "/home/ajackson/Dropbox/Rprojects/ERD/FEMA/"
DataLocation <- "https://www.ajackson.org/ERD/FEMA/"

# Google_notes <- "https://docs.google.com/document???????????????????"

#########    for testing locally
Local_test <- TRUE


if ( Local_test ) {
  df_grp <- readRDS(paste0(path, "Block_group_data_AL.rds")) 
} else {
  z <- url(paste0(DataLocation, "Block_group_data_AL.rds"), method="libcurl")
  df_grp <- readRDS(z)
  close(z)
}

#######################################################
# Functions
#######################################################

# define js function for opening urls in new tab/window
js_code <- "
shinyjs.browseURL = function(url) {
  window.open(url,'_blank');
}
"

#######################################################
# Define UI for application that draws a histogram
#######################################################
ui <- fluidPage(
  
  # set up shiny js to be able to call our browseURL function
  useShinyjs(),
  extendShinyjs(text = js_code, functions = 'browseURL'),
  
    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

#######################################################
# Define server logic required to draw a histogram
#######################################################
server <- function(input, output) {
  
  # js$browseURL(i)
  
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
