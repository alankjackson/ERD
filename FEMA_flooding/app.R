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
library(bslib)

path <- "/home/ajackson/Dropbox/Rprojects/ERD/FEMA/"
DataLocation <- "https://www.ajackson.org/ERD/FEMA/"

# Google_notes <- "https://docs.google.com/document???????????????????"

#########    for testing locally
Local_test <- FALSE


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
  Events <- str_replace_all(Grp_data$floodEvent, ", ", "<br>")
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
      fillColor = pal(Grp_data[[input$Block_var]]),
      popup = paste(
        "<div class='leaflet-popup-scrolled' style='max-width:150px;max-height:200px'>",
        "Block Group:", Grp_data$censusBlockGroupFips, "<br>",
        "Pop at risk:", Grp_data$Pop_acs, "<br>",
        "Homes at risk:", Grp_data$Households, "<br>",
        "Claims Own, Rent, Mobile:", Grp_data$Num_Claims_Own,",",
                                     Grp_data$Num_Claims_Rent,",",
                                     Grp_data$Num_Claims_Mobile,"<br>",
        "Claims per house:", Grp_data$ClaimsPerHousehold, "<br>",
        "Number of Floods:", Grp_data$Num_dates, "<br>",
        "% in poverty:", Grp_data$Pct_poverty, "<br>",
        "<b>Flood events:</b><br>", Events, "</div>"
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

####    Draw histograms

Draw_plots <- function(Grp_data, output) {
  p1 <- Grp_data %>% 
    sf::st_drop_geometry() %>% 
    filter(Num_Claims>100) %>% 
    ggplot(aes(x=Num_Claims)) +
    geom_histogram() +
    labs(title="Number of Claims per Census Blk-Grp",
         x="Number of Claims",
         y="Blk Grps")
  
  p2 <- Grp_data %>% 
    sf::st_drop_geometry() %>% 
    filter(ClaimsPerHousehold>0.2) %>% 
    ggplot(aes(x=ClaimsPerHousehold)) +
    geom_histogram() +
    labs(title="Number of Claims per Household",
         x="Number of Claims per Household",
         y="Blk Grps")
  
  output$plot <- renderPlot({
    gridExtra::grid.arrange(p1, p2, 
                            top="Claims>100, Claims/House>0.2")
  })
}

####    Create URL for google map

# make_URL <- function(MAPID, input){
make_URL <- function(Select_grp, dataset_grp, input){
  
  # if (!is.null(Select_grp)) {
  #   print(paste("make_URL 1"))
  #   Poly <- dataset_grp[dataset_grp$censusBlockGroupFips==Select_grp,]
  #   Poly <- dePop(Poly)
  #   foo <- as_tibble(sf::st_coordinates(Poly))  
  #   foo <- foo[1:nrow(foo)-1,] # drop last row (duplicated coordinate)
  #   BegX <- foo[1,]$X
  #   BegY <- foo[1,]$Y
  #   EndX <- foo[nrow(foo),]$X
  #   EndY <- foo[nrow(foo),]$Y
  #   
  #   foo <- foo %>% unite("Z", Y:X, sep=",") %>% 
  #     select(Z)
  #   foo <- paste(unlist(as.list(foo)), collapse="|")
  #   URL <- paste0("https://www.google.com/maps/dir/",
  #                 BegY, ",", BegX, "/", EndY, ",", EndX, "/?waypoints=",
  #                 foo
  #   )
  #   return(URL)
  # } else { 
    print(paste("make_URL 2"))
    center <- input$map_center
    zoom <- input$map_zoom
    return(paste0("https://www.google.com/maps/@?api=1&map_action=map&center=",
                  center[["lat"]], "%2C", 
                  center[["lng"]], "&zoom=", zoom))
  # }
}

####   simplify a polygon to less than 20 or thereabouts vertices

dePop <- function(Single_poly, Max_vertices=20){
  vertices <- nrow(sf::st_coordinates(Single_poly)) 
  New_poly <- Single_poly
  Tolerance <- 50
  while(vertices>Max_vertices) {
    New_poly <- sf::st_simplify(Single_poly, dTolerance = Tolerance)
    vertices <- nrow(sf::st_coordinates(New_poly))
    print(paste("Tolerance, Vertices", Tolerance, vertices))
    Tolerance <- Tolerance + 50
  }
  return(New_poly)
}

#######################################################
# UI 
#######################################################
# ui <- fluidPage(
ui <- page_fluid(
  
  # set up shiny js to be able to call our browseURL function
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = js_code, functions = 'browseURL'),
  
  #   Update fonts
  tags$style(type='text/css', ".selectize-input { 
             font-size: 12px; line-height: 12px;} 
             .selectize-dropdown { 
             font-size: 12px; line-height: 12px; }
             .control-label { 
             font-size: 12px; line-height: 12px; }
             .radio { 
             font-size: 12px; line-height: 12px; }
             .btn { 
             font-size: 12px; line-height: 12px; }
             "),
  
    # Application title
    titlePanel("FEMA flooding data"),

    # Sidebar with a slider input for number of bins 
    # sidebarLayout(
    layout_columns(
      card(# left top card
        # sidebarPanel(
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
          
          # HTML("<hr>"),
          
          actionButton(
            "Google",
            "Open Google Maps"
          )
          
        # )
      ),

      card(
        # Show a map
        # mainPanel(
           leafletOutput("map")
        # )
      ),
    col_widths = c(3,9)
    ),
    layout_columns(
      card(
           plotOutput("plot")
           ),
      card(
        DT::dataTableOutput('data')
           ),
      col_widths = c(3,9)
    )
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
    print(paste(input$Block_var, max(Grp_data()[[input$Block_var]])))
    #   Reset color scale
    if (nrow(Grp_data())==0) {return()}
    
    pal <- colorNumeric("YlOrBr",
                        c(min(Grp_data()[[input$Block_var]], na.rm=TRUE),
                          max(Grp_data()[[input$Block_var]], na.rm=TRUE)),
                        na.color = "transparent")
    Draw_blkgrp(dataset_grp(), Grp_data(), pal, input)
    Draw_dots(dataset_grp())
    Draw_plots(df_grp(), output)
  })

  #####################
  #   Open a google map
  #####################
  
  observeEvent(input$Google, {
    center <- input$map_center
    zoom <- input$map_zoom
    url <- make_URL(Select_grp(), dataset_grp(), input)
    print(paste("Google:", center["lat"], center["lng"], zoom))
    print(url)
    # url <- paste0("https://www.google.com/maps/@?api=1&map_action=map&center=",
                  # center["lat"], "%2C", 
                  # center["lng"], "&zoom=", zoom)
    # "https://www.google.com/maps/search/?api=1&query=markers:path:37.7833,-122.4167|37.7833,-122.4000|37.7900,-122.4000|37.7900,-122.4167"
    shinyjs::js$browseURL(url)
  })

  #####################
  #   Select Block Group with mouse
  #####################
  
  Select_grp <- reactive({ #    Select a group with mouse
    print("Reactive select group")
    event <- input$map_shape_click
    print(paste("Blk_grp:", event$id))
    event$id
  })
  #####################
  #   Render the table
  #####################
  
  output$data <- DT::renderDataTable({
    print("--10--")
    dataset_grp() %>% sf::st_drop_geometry()} %>% 
      select(censusBlockGroupFips, Pop_acs, ClaimsPerHousehold, Pct_poverty,
             Num_Claims_Own, Num_Claims_Rent),
    colnames=c("Block Grp", 
               "Pop", 
               "Claims House", 
               "% Poverty", 
               "Owner Claims",
               "Renter Claims")
)

#############    Table selection controls
Sel <- reactive({!is.null(input$data_rows_selected)}) # is there a selection?

Row_list <-reactiveValues(rows=list()) # what rows are selected?

observeEvent(input$data_rows_selected, ignoreNULL = FALSE, {
  Row_list$rows <- input$data_rows_selected
}) #END OBSERVE EVENT


#####################
#   Highlight and remove highlights for polygons from table
#####################

observeEvent(Row_list$rows, ignoreNULL = FALSE, {
  print(paste("--9--", Row_list$rows))
  Row_dropped <- NULL
  Row_added <-   setdiff(Row_list$rows, Row_list$oldrows) # find new row
  Row_dropped <- setdiff(Row_list$oldrows, Row_list$rows) # find deleted row
  rows <- Row_list$rows
  Row_list$oldrows <- rows
  if (Sel()) { # at least one row is selected
    leafletProxy("map") %>%
      addPolylines(data=dataset_grp()[Row_added,],
                   color="red",
                   layerId=paste0(dataset_grp()[Row_added,]$censusBlockGroupFips, "_highlight"),
                   weight=4)
  }
  if (!is.null(Row_dropped) & length(Row_dropped)>0){ # a row was unselected
    leafletProxy("map") %>%
      removeShape(layerId = paste0(dataset_grp()[Row_dropped,]$censusBlockGroupFips,
                                   "_highlight"))
  }
  if(!Sel() &  !is.null(Row_dropped)){ # the final row was unselected
    leafletProxy("map") %>%
      removeShape(layerId = paste0(dataset_grp()[Row_list$oldrows,]$censusBlockGroupFips,
                                   "_highlight"))
  }
  
  
}) #END OBSERVE EVENT

}
# Run the application 
shinyApp(ui = ui, server = server)
