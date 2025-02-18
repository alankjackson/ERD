#
#   Tool for searching the country for poor areas subject to frequent flooding
#
#

library(tidyverse)
library(stringr)
library(leaflet)
library(leafpop)
library(shiny)
library(bslib) # for page layout

path <- "/home/ajackson/Dropbox/Rprojects/ERD/Data/Final_FEMA_Flood_Data/"
DataLocation <- "https://www.ajackson.org/ERD/FEMA/"

# Google_notes <- "https://docs.google.com/document???????????????????"

#########    for testing locally
Local_test <- FALSE


if ( Local_test ) {
  df_grp <- readRDS(paste0(path, "FEMA_AL_plot.rds")) 
} else {
  z <- url(paste0(DataLocation, "FEMA_AL_plot.rds"), method="libcurl")
  df_grp <- readRDS(z)
  close(z)
}

State_df <- as_tibble(cbind(state.name, state.abb))
State_df <- rename(State_df, c(States=state.name, State_abbr=state.abb))

#   Round up to the nearest 5
print("max poverty")
Max_poverty <- 5*round((max(df_grp$Pct_poverty, na.rm=TRUE)+2.5)/5)
# Max_claims <- 25*round((max(df_grp$Num_Claims_Primary, na.rm=TRUE)+12.5)/25)
# Max_house <- 0.1*round((max(df_grp$ClaimsPerHousehold, na.rm=TRUE)+0.05)/0.1)
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
  print(paste0("Read in ", "FEMA_",State_df[i,]$State_abbr,"_plot.rds"))
  if ( Local_test ) {
    #     Read block-group data off disk
    foo <- readRDS(paste0(path,
                          paste0("FEMA_", State_df[i,]$State_abbr,"_plot.rds")))
  } else {
    z <- url(paste0(DataLocation, "FEMA_", 
                    State_df[i,]$State_abbr,"_plot.rds"), 
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
  Events <- str_replace_all(Grp_data$floodEvents, ", ", "<br>")
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
      options = pathOptions(pane = "Polys"),
      popup = paste(
        "<div class='leaflet-popup-scrolled' style='max-width:150px;max-height:200px'>",
        "Block Group:", Grp_data$censusBlockGroupFips, "<br>",
        "Pop at risk:", Grp_data$Pop_acs, "<br>",
        "Homes at risk:", Grp_data$Households, "<br>",
        "<table> <tr>",
        "<th>Own</th> <th>Rent</th> <th>Mobile</th> </tr><tr>",   
        "<td>",Grp_data$Num_Claims_Own_Primary,"</td>",
        "<td>",Grp_data$Num_Claims_Rent_Primary,"</td>",
        "<td>",Grp_data$Num_Claims_Mobile_Primary,"</td></tr></table>",
        "Claims per house:", Grp_data$ClaimsPerHousehold, "<br>",
        "Number of Floods:", Grp_data$Num_dates_Primary, "<br>",
        "% in poverty:", Grp_data$Pct_poverty, "<br>",
        "Vuln Index:", Grp_data$Vuln_Index, "<br>",
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

Draw_dots <- function(dataset, drawme) {
  print(paste("-- Draw_dots", drawme))
  centroids <- sf::st_centroid(dataset)
  print("-- Draw_dots 2")
  p2 <- as.list(NULL)
  p2 <- lapply(1:nrow(dataset), function(i) {
    p2[[i]] <- dataset[i,]$Plot
  })
  print("-- Draw_dots 3")
  foo <- leafletProxy("map", data=centroids) %>% 
    clearMarkers() #%>%
  print("-- Draw_dots 3.1")
  foo <- foo %>% 
    addCircleMarkers(
      group = paste0("pnt", drawme),
      options = pathOptions(pane = "Dots"),
      radius=1,
      color="black"
    ) #%>% 
  print("-- Draw_dots 3.2")
  foo <- foo %>% 
    addPopupGraphs(p2, group = paste0("pnt", drawme))
  print("-- Draw_dots 4")
  foo
}

### Popup based on the dot clicked
# makePopupPlot <- function (BlkGrp, dataset) {
#   plot <- dataset %>% 
#     sf::st_drop_geometry() %>% 
#     filter(censusBlockGroupFips==BlkGrp) %>% 
#     select(floodEvents, floodNum) %>% 
#     mutate(floodEvents=stringr::str_split(floodEvents, ", "),
#            floodNum=stringr::str_split(floodNum, ", ")) %>% 
#     pivot_longer(everything(),
#                  values_to = "Values",
#                  names_to = "Names") %>% 
#     unnest(Values) %>% 
#     group_by(Names) %>% 
#     mutate(id = row_number()) %>% 
#     ungroup() %>% 
#     pivot_wider(id_cols=id,
#                 names_from = "Names",
#                 values_from = "Values") %>% 
#     mutate(floodNum=as.numeric(floodNum)) %>% 
#     mutate(floodEvents = fct_inorder(floodEvents)) %>%
#     select(-id) %>% 
#     ggplot(aes(x=floodEvents, y=floodNum)) +
#     geom_col() +
#     coord_flip() +
#     labs(title=paste("Events in Blk Grp"),
#          y="Number of Flood Claims",
#          x="Flood Name")
#   
#   return(plot)
# }

####    Draw histograms

# Draw_plots <- function(Grp_data, output) {
#   print("draw plots")
#   p1 <- Grp_data %>% 
#     sf::st_drop_geometry() %>% 
#     filter(Num_Claims_Primary>100) %>% 
#     ggplot(aes(x=Num_Claims_Primary)) +
#     geom_histogram() +
#     labs(title="# of Claims/Blk-Grp",
#          x="# of Primary Claims",
#          y="Blk Grps")
#   
#   p2 <- Grp_data %>% 
#     sf::st_drop_geometry() %>% 
#     filter(ClaimsPerHousehold>0.2) %>% 
#     ggplot(aes(x=ClaimsPerHousehold)) +
#     geom_histogram() +
#     labs(title="# of Claims/House",
#          x="# of Primary Claims/House",
#          y="Blk Grps")
#   
#   output$plot <- renderPlot({
#     gridExtra::grid.arrange(p1, p2, 
#                             top = grid::textGrob(
#                               "Claims>100, Claims/House>0.2",
#                               # gp = gpar(fontfamily = "HersheySerif" , fontsize = 9),
#                               gp = grid::gpar(fontsize = 10)
#                             ))
#                             # top="Claims>100, Claims/House>0.2")
#   })
# }

####    Create URL for google map

make_URL <- function(input){
  
  print(paste("make_URL 2"))
  center <- input$map_center
  zoom <- input$map_zoom
  return(paste0("https://www.google.com/maps/@?api=1&map_action=map&center=",
                center[["lat"]], "%2C", 
                center[["lng"]], "&zoom=", zoom))
}

####    Create URL for FEMA flood map

make_FEMA <- function(input){
  # https://msc.fema.gov/portal/search?AddressQuery=%20-92.41567874975726%2C42.54086937934763
  print(paste("make_URL 3"))
  center <- input$map_center
  zoom <- input$map_zoom
  return(paste0("https://msc.fema.gov/portal/search?AddressQuery=",
                center[["lng"]], "%2C", 
                center[["lat"]]))
}

####    Calculate a zoom level

make_Zoom <- function(Poly){
  bbox <- sf::st_bbox(Poly)
  extent <- max(abs(bbox[[1]]-bbox[[3]]), abs(bbox[[2]]-bbox[[4]]))
  print(paste("make_Zoom", extent, sqrt(6/extent)))
  return(round(log(8000/extent)))
}

####    Find the table row corresponding to a Blk-grp

find_Row <- function(Blk_grp, Grp_data){
  print(paste("find_Row", Blk_grp))
  grep(Blk_grp, Grp_data$censusBlockGroupFips)
}

####    Build PDF report

make_Pdf <- function(filename, input){
  # pdf(file = filename)
  # plot(cars)
  # dev.off()  
  
  # Set up parameters to pass to Rmd document
  params <- list(n = input$slider)
  
  # Knit the document, passing in the `params` list, and eval it in a
  # child of the global environment (this isolates the code in the document
  # from the code in this app).
  rmarkdown::render(tempReport, output_file = filename,
                    params = params,
                    envir = new.env(parent = globalenv())
  )
}

####    Expand a bounding box
#   Expand box by 20% to give a little extra room
expand_box <- function(bbox, pct=0.2){
  Dx <- (bbox[["xmax"]]-bbox[["xmin"]])*pct
  Dy <- (bbox[["ymax"]]-bbox[["ymin"]])*pct
  bbox["xmin"] <- bbox["xmin"] - Dx
  bbox["xmax"] <- bbox["xmax"] + Dx
  bbox["ymin"] <- bbox["ymin"] - Dy
  bbox["ymax"] <- bbox["ymax"] + Dy
  return(bbox)
}

#######################################################
# UI 
#######################################################
ui <- fluidPage(
# ui <- page_fluid(
  
  # set up shiny js to be able to call our browseURL function
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = js_code, functions = 'browseURL'),
  
  #   Update fonts (shrink to gain more space)
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
    sidebarLayout(
    # layout_columns(
      # card(# left top card
        sidebarPanel(
        #   Alaska has no block groups that qualify
          selectInput('State', 'Choose a state', state.name[-2], selected="Alabama"),
          
          sliderInput('Pct_poverty', '> % in Poverty', 
                      min=25, max=Max_poverty,
                      value=25, 
                      step=5, round=0),
          
          # sliderInput('Min_house', 'Claims per Household', 
          #             min=0, max=Max_house,
          #             value=0.1, 
          #             step=0.1, round=1),
          
          #   choose variable to color blocks with
          radioButtons("Block_var", "Color Blocks by:",
                       c("% Poverty"="Pct_poverty",
                         "Vulnerability"="Vuln_Index",
                         "Num of Claims"="Num_Claims_Primary",
                         "Claims per House"="ClaimsPerHousehold")),
          
          ####    Google and FEMA buttons
          # HTML("<hr>"),
          splitLayout(cellWidths = c("50%", "50%"),
          column(1,
          actionButton(
            "Google",
            "Google"
            # width="50%"
          )),
          column(6,
          actionButton(
            "FEMA",
            "FEMA"
            # width="50%"
          ))),
          
          #####   Download PDF
          
          textInput("pdflabel", "Label for the PDF", "---"),
          div(id="dwnbutton", 
            downloadButton("pdfButton", "Download PDF")
          )
      ),
  mainPanel(
    # tabsetPanel(type = "tabs", id="inTabset",
    tabsetPanel(id="inTabset",
                tabPanel("Map", value="map", leafletOutput("map")),
                tabPanel("Table", value="table", DT::dataTableOutput("data"))
      )
    )
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
                  Bbox[["xmax"]], Bbox[["ymax"]]) %>% 
        addMapPane("Polys", zIndex = 410) %>% # shown below Dots               
        addMapPane("Dots", zIndex = 420) # shown above Polys               
    })
  })

  ##################################
  #####   Subset data
  ##################################
  
  dataset_grp <- reactive({
    # print(paste("--1--", input$Pct_poverty))
    foo <- df_grp() %>% 
      filter(Pct_poverty>=input$Pct_poverty)# %>% 
      # filter(ClaimsPerHousehold>=input$Min_house)# %>% 
      # filter(Num_Claims_Primary>=input$Min_Claims)
    print(paste("--1.1--", nrow(foo)))
    updateSliderInput(session, "Pct_poverty", 
                      min = 25,
                      max = max(5*round((max(foo$Pct_poverty, na.rm=TRUE)+2.5)/5),15),
                      step = 5)
    # updateSliderInput(session, "Min_claims", 
    #                   min = 0,
    #                   max = max(25*round((max(foo$Num_Claims_Primary, na.rm=TRUE)+12.5)/25),25),
    #                   step = 25)
    # updateSliderInput(session, "Min_house", 
    #                   min = 0,
    #                   max = max(0.1*round((max(foo$ClaimsPerHousehold, na.rm=TRUE)+0.05)/0.1),0),
    #                   step = 0.1)
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
    #   leafpop wants group to change on redraw
    # drawme <- stringr::str_remove_all(now(), "-|:|\\s|\\.")
    print(paste(input$Block_var, max(Grp_data()[[input$Block_var]])))
    #   Reset color scale
    if (nrow(Grp_data())==0) {return()}
    
    pal <- colorNumeric("YlOrBr",
                        c(min(Grp_data()[[input$Block_var]], na.rm=TRUE),
                          max(Grp_data()[[input$Block_var]], na.rm=TRUE)),
                        na.color = "transparent")
    Draw_blkgrp(dataset_grp(), Grp_data(), pal, input)
    # Draw_dots(dataset_grp(), drawme)
    # Draw_plots(df_grp(), output)
  })
  
  observe({
    print("--6.5--")
    #   leafpop wants group to change on redraw
    drawme <- stringr::str_remove_all(now(), "-|:|\\s|\\.")
    if (nrow(Grp_data())==0) {return()}
    
    Draw_dots(dataset_grp(), drawme)
    # Draw_plots(df_grp(), output)
  })

  #####################
  #   Open a google map
  #####################
  
  observeEvent(input$Google, {
    center <- input$map_center
    zoom <- input$map_zoom
    url <- make_URL(input)
    print(paste("Google:", center["lat"], center["lng"], zoom))
    print(url)
    shinyjs::js$browseURL(url)
  })

  #####################
  #   Open a FEMA map
  #####################
  
  observeEvent(input$FEMA, {
    center <- input$map_center
    zoom <- input$map_zoom
    url <- make_FEMA(input)
    print(paste("FEMA:", center["lat"], center["lng"], zoom))
    print(url)
    shinyjs::js$browseURL(url)
  })

  #####################
  #   Select Block Group with mouse
  #####################
  
  Select_grp <- observe({ #    Select a group with mouse
    print("Reactive select group")
    event <- input$map_shape_click
    print(paste("Blk_grp:", event$id))
    print(paste("proxy event"))
    req(event$id)
    proxy %>% DT::selectRows(as.numeric(find_Row(event$id, Grp_data())))
    event$id
  })

  #####################
  #   Select Block Group center with mouse
  #####################
  
  Select_grp2 <- observe({ #    Select a group with mouse
    print("Reactive select group2")
    event <- input$map_marker_click
    print(paste("Blk_grp:", event$id))
    print(paste("proxy event"))
    req(event$id)
    event$id
  })

  #####################
  #   Generate & Download PDF
  #####################
  
  observeEvent(Sel(), {
    if (Sel()) {
      shinyjs::enable("pdfButton")
      shinyjs::runjs("$('#dwnbutton').removeAttr('title');")
    } else {
      shinyjs::disable("pdfButton")
      shinyjs::runjs("$('#dwnbutton').attr('title', 'Select a Block Group');") 
    }
  })
  
  output$pdfButton <- downloadHandler("FEMA_flooding.pdf", function(theFile) {
    # Here, your pdf-generator is provided with the "filename" that is used
    # to provide the file for the user.
      make_Pdf(theFile, input)
  })
  
  
  #####################
  #   Render the table
  #####################
  
  output$data <- DT::renderDataTable({
    print("--10--")
    dataset_grp() %>% sf::st_drop_geometry()} %>%
      select(censusBlockGroupFips, Pop_acs, ClaimsPerHousehold, Pct_poverty,
             Vuln_Index, Num_Claims_Own_Primary, Num_dates_Primary),
    colnames=c("Blk Grp", 
               "Pop", 
               "Clms Hse", 
               "% Pov", 
               "Vuln",
               "Own Clms",
               "# Evnt"),
    selection = 'single'
)
  
proxy <- DT::dataTableProxy('data')

#############    Table selection controls

# observe(event$id, {
#   print(paste("proxy event", Select_grp()))
#   proxy %>% selectRows(as.numeric(input$rows))
# })

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
    LongLat <- sf::st_coordinates(sf::st_centroid(dataset_grp()[Row_added,]))
    Zoom <- make_Zoom(dataset_grp()[Row_added,])
    updateTabsetPanel(session, "inTabset", selected="map") # switch to map tab
    leafletProxy("map") %>%
      addPolylines(data=dataset_grp()[Row_added,],
                   color="red",
                   layerId=paste0(dataset_grp()[Row_added,]$censusBlockGroupFips, "_highlight"),
                   weight=4) %>% 
      setView(LongLat[[1]], LongLat[[2]], zoom=Zoom)
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
