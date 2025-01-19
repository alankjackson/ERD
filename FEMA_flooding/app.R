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
  df_grp <- readRDS(paste0(path, "FEMA_extra_AL.rds")) 
} else {
  z <- url(paste0(DataLocation, "FEMA_extra_AL.rds"), method="libcurl")
  df_grp <- readRDS(z)
  close(z)
}

State_df <- as_tibble(cbind(state.name, state.abb))
State_df <- rename(State_df, c(States=state.name, State_abbr=state.abb))

#   Round up to the nearest 5
print("max poverty")
Max_poverty <- 5*round((max(df_grp$Pct_poverty, na.rm=TRUE)+2.5)/5)
Max_claims <- 25*round((max(df_grp$Num_Claims, na.rm=TRUE)+12.5)/25)
Max_house <- 0.1*round((max(df_grp$ClaimsPerHousehold, na.rm=TRUE)+0.05)/0.1)
print(paste("Max poverty", Max_poverty))

Row_diff <- NULL  # Row deselected from table

#######################################################
# Functions
#######################################################

# define js function for opening urls in new tab/window
js_code <- "
shinyjs.browseURL = function(url) {
  window.open(url,'_blank');
}
"
  
####    Read in a state

read_State <- function(State) {
  i <- which(State_df$States==State)
  print(paste0("Read in ", "FEMA_extra_",State_df[i,]$State_abbr,".rds"))
  if ( Local_test ) {
    #     Read block-group data off disk
    foo <- readRDS(paste0(path,
                          paste0("FEMA_extra_", State_df[i,]$State_abbr,".rds")))
  } else {
    z <- url(paste0(DataLocation, "FEMA_extra_", 
                    State_df[i,]$State_abbr,".rds"), 
             method="libcurl")
    foo <- readRDS(z)
    close(z)
  }
  foo
}

########   Draw Block Group

Draw_blkgrp <- function(dataset, Grp_data, pal, input){
  print("---- draw groups")#
  print(input$Block_var)
  leafletProxy("map", data=dataset) %>% 
    clearShapes() %>%
    clearControls() %>%
    addPolygons(  # block groups
      group = "Block Grps",
      layerId = ~censusBlockGroupFips,
      opacity = 0.8,
      color="black",
      weight = 1,
      fillOpacity = 0.4,
      fillColor = ~pal(Pct_poverty),
      popup = paste(
        "Block Group:", Grp_data$censusBlockGroupFips, "<br>",
        "Pop at risk:", Grp_data$Pop_acs, "<br>",
        "Homes at risk:", Grp_data$Households, "<br>",
        "Claims Own, Rent, Mobile:", Grp_data$Num_Claims_Own,",",
                                     Grp_data$Num_Claims_Rent,",",
                                     Grp_data$Num_Claims_Mobile,"<br>",
        "Claims per household:", Grp_data$ClaimsPerHousehold, "<br>",
        "Number of Floods:", Grp_data$Num_dates, "<br>",
        "% in poverty:", Grp_data$Pct_poverty
      )
    ) %>% 
    addLegend(
      position = "topright",
      pal=pal,
      values = Grp_data[[input$Block_var]],
      title=input$Block_var
    )  
}

####  Draw dots

Draw_dots <- function(dataset) {
  centroids <- sf::st_centroid(dataset)
  leafletProxy("map", data=centroids) %>% 
    clearMarkers() %>%
    addCircleMarkers(
      radius=1,
      color="black"
    )
}

####    Create URL for google map

make_URL <- function(MAPID, input){
  
  center <- input$map_center
  zoom <- input$map_zoom
  return(paste0("https://www.google.com/maps/@?api=1&map_action=map&center=",
                center[["lat"]], "%2C1", 
                center[["lng"]], "&zoom=", zoom))
}

#######################################################
# UI 
#######################################################
ui <- fluidPage(
  
  # set up shiny js to be able to call our browseURL function
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = js_code, functions = 'browseURL'),
  
    # Application title
    titlePanel("FEMA flooding data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput('State', 'Choose a state', state.name, selected="Alabama"),
          
          # HTML("<hr>"),
          
          sliderInput('Pct_poverty', '> % in Poverty', 
                      min=5, max=Max_poverty,
                      value=10, 
                      step=5, round=0),
          
          # HTML("<br>"),
          
          sliderInput('Min_Claims', 'Number of Claims', 
                      min=25, max=Max_claims,
                      value=50, 
                      step=25, round=0),
          
          # HTML("<br>"),
          
          sliderInput('Min_house', 'Claims per Household', 
                      min=0, max=Max_house,
                      value=0.1, 
                      step=0.1, round=1),
          
          #   choose variable to color blocks with
          radioButtons("Block_var", "Color Blocks by:",
                       c("% Poverty"="Pct_poverty",
                         "Num of Claims"="Num_Claims",
                         "Claims per House"="ClaimsPerHousehold")),
          
          HTML("<hr>"),
          
          actionButton(
            "Google",
            "Open Google Maps"
          )
        ),

        # Show a map
        mainPanel(
           leafletOutput("map")
        )
    ),
          textOutput("selected_var")
)

#######################################################
# server 
#######################################################
server <- function(input, output, session) {

  ##################################
  #####   Load in a new state
  ##################################
  
  df_grp <- reactive({
    print("============   read in grp data ==============")
    print(paste("State1=", input$State))
    read_State(input$State)
  })
  
##################################
#####   Original map - redo on state change
##################################
  
  observe({
    print("original map")
    Bbox <- sf::st_bbox(df_grp())
    #   Build empty master map by state
    output$map <- leaflet::renderLeaflet({
      print("--5--")
      leaflet() %>% addTiles() %>%
        fitBounds(Bbox[["xmin"]], Bbox[["ymin"]],
                  Bbox[["xmax"]], Bbox[["ymax"]])
    })
  })

  ##################################
  #####   Subset data
  ##################################
  
  dataset_grp <- reactive({
    # print(paste("--1--", input$Pct_poverty))
    foo <- df_grp() %>% 
      filter(Pct_poverty>=input$Pct_poverty) %>% 
      filter(ClaimsPerHousehold>=input$Min_house) %>% 
      filter(Num_Claims>=input$Min_Claims)
    print(paste("--1.1--", nrow(foo)))
    updateSliderInput(session, "Pct_poverty", 
                      min = 5,
                      max = max(5*round((max(foo$Pct_poverty, na.rm=TRUE)+2.5)/5),15),
                      step = 5)
    updateSliderInput(session, "Min_claims", 
                      min = 25,
                      max = max(25*round((max(foo$Num_Claims, na.rm=TRUE)+12.5)/25),25),
                      step = 25)
    updateSliderInput(session, "Min_house", 
                      min = 0,
                      max = max(0.1*round((max(foo$ClaimsPerHousehold, na.rm=TRUE)+0.05)/0.1),0),
                      step = 0.1)
    foo
  })
  
  Grp_data <- reactive({
    print("--2--")
    dataset_grp() %>% sf::st_drop_geometry()
  })
  
  ##################################
  #       Build map
  ##################################
  
  #######################
  #     Draw Block Group polys
  #######################
  
  observe({
    print("--6--")
    #   Reset color scale
    if (nrow(Grp_data())==0) {return()}
    
    # pal <- colorNumeric("YlOrBr",
    #                     # c(5,
    #                     # max(Grp_data()[["Pct_poverty"]], na.rm=TRUE)),
    #                     c(input$Pct_poverty, 
    #                       max(Grp_data()[["Pct_poverty"]], na.rm=TRUE)),
    #                     na.color = "transparent")
    pal <- colorNumeric("YlOrBr",
                        c(min(Grp_data()[[input$Block_var]], na.rm=TRUE),
                          max(Grp_data()[[input$Block_var]], na.rm=TRUE)),
                        na.color = "transparent")
    Draw_blkgrp(dataset_grp(), Grp_data(), pal, input)
    Draw_dots(dataset_grp())
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
