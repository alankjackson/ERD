#
#   Tool for searching the country for poor areas subject to frequent flooding
#
#

library(tidyverse)
library(stringr)
library(leaflet)
library(leafpop)
library(shiny)
library(gt)
library(webshot2)
library(pagedown)
library(curl)
# library(bslib) # for page layout

path <- "/home/ajackson/Dropbox/Rprojects/ERD/Data/Final_FEMA_Flood_Data/"
DataLocation <- "https://www.ajackson.org/ERD/FEMA/"

googlecrs <- "EPSG:4326"

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

###############################################################
####    Build PDF report
###############################################################

make_Pdf <- function(filename, Blk_grp, dataset_grp, input){
  print("----  make_Pdf")
  FEMA_data <- dataset_grp %>% filter(censusBlockGroupFips==Blk_grp)
  print("make Pdf: 2")
  County_code <- stringr::str_sub(FEMA_data$censusBlockGroupFips, start=3, end=5)
  state <- FEMA_data$State
  comment <-  input$pdflabel
  County <- tigris::list_counties(state) %>% 
    filter(county_code==County_code)
  Tempdir <- tempdir()
  
  Title <-paste("Report for Block Group", Blk_grp, "\n# in", 
                County$county, "county,", state)
  Intro <- paste0("Data from FEMA, as of 3 Jan 2025, insurance claims since 1978. ",
                  "This block group has a poverty level of ",
                  FEMA_data$Pct_poverty, "% ",
                  "and a per capita income of ", scales::dollar(FEMA_data$Per_cap_in), ".\n\n",
                  "There have been ", FEMA_data$Num_dates_Primary, " flood events since 1978."
  )  
  
  #####################   Make claims table
  Table <-
    FEMA_data %>% sf::st_drop_geometry() %>% 
    select(Claims_Owner=Num_Claims_Own_Primary, 
           Claims_Renter=Num_Claims_Rent_Primary,
           Claims_Total=Num_Claims_Primary, Pop_Owner=Owners,
           Pop_Renter=Renters, Pop_Total=Pop_acs, House_Owner=OwnersH,
           Claims_Vacation=Num_Claims_Vacation,
           Claims_Mobile=Num_Claims_Mobile_Primary, Pop_Mobile=Mobile,
           House_Renter=RentersH, House_Total=Households) %>% 
    pivot_longer(everything(),
                 values_to="Values",
                 names_sep="_",
                 names_to=c("Category", "Subset")) %>% 
    pivot_wider(names_from=Category,
                values_from = Values) %>% 
    mutate(charOrdered = fct_relevel(Subset, c('Owner', 'Renter', 
                                               'Mobile', 'Vacation', 
                                               'Total'))) %>% 
    arrange(charOrdered) %>%
    select(-charOrdered) %>% 
    gt() %>% 
    tab_header(title="Claims Data") %>% 
    cols_label(
      Subset="Status",
      Claims="# Claims",
      Pop="Population",
      House="Households"
    ) %>% 
    tab_style(
      style = list(cell_fill(color = "lightblue")),
      locations = cells_body(columns = everything(), rows = 5)
    ) %>% 
    tab_footnote(
      footnote=md("*FEMA data*")
    )
  
  gtsave(Table, paste0(Tempdir, "/Table.png"))
  table_png <- paste0(Tempdir, "/Table.png")
  #####################   End of Make claims table
  
  #####################   Make vulnerability table
  Table2 <-
    FEMA_data %>% sf::st_drop_geometry() %>% 
    select(Socio_Econ, Ethnicity, Housing, Household_Ages, Vuln_Index, 
           Per_cap_in, Med_ageE
           ) %>% 
    gt() %>% 
    tab_header(title="Vulnerability Data") %>% 
    cols_label(
      Socio_Econ="Socio-Economic",
      Ethnicity="Ethnicity",
      Housing="Housing",
      Household_Ages="Age",
      Vuln_Index="Overall Index",
      Per_cap_in="Per capita income",
      Med_ageE="Median age"
    ) %>% 
    tab_spanner(
      label="Vulnerability Indicies",
      columns=c(Socio_Econ, Ethnicity, Housing, Household_Ages, Vuln_Index)
    ) %>% 
    fmt_currency(
      columns=c(6)
    ) %>% 
    tab_footnote(
      footnote=md("*Census and CDC data*")
    )
  
  gtsave(Table2, paste0(Tempdir, "/Table2.png"))
  table2_png <- paste0(Tempdir, "/Table2.png")
  #####################   End of Make vulnerability table
  
  #####################   Make Map
  
  Bbox <- sf::st_bbox(FEMA_data)
  # Make a bounding box off-center to accomodate index map
  Bbox_X <- expand_box(Bbox, pct=0.2, offset=2)
  # Get state outline
  Inset_map <- tigris::states() %>% 
    filter(STUSPS==state) %>% 
    sf::st_transform(crs=googlecrs) %>% 
    ggplot() +
    geom_sf() +
    geom_sf(data=sf::st_centroid(FEMA_data), color="red", size=3) +
    theme(axis.text.x=element_blank(), 
          axis.ticks.x=element_blank(), 
          axis.text.y=element_blank(), 
          axis.ticks.y=element_blank()) 
  
  basemap <- maptiles::get_tiles(Bbox_X, provider = "OpenStreetMap", crop = TRUE)
  Main_map <- ggplot() +
    tidyterra::geom_spatraster_rgb(data = basemap) +
    geom_sf(data = FEMA_data, color = "red", fill=NA, linewidth=1, linetype="dashed")
  
  Final_map <- cowplot::ggdraw() +
    cowplot::draw_plot(Main_map) +
    cowplot::draw_plot(Inset_map, x=0.75, y=0.2, width=0.2, height=0.2)
  
  ggsave(filename = "/Final_map.png", 
         path = Tempdir,
         plot = Final_map,
         width = 7.05, 
         # height = 4,
         dpi = 150)
  
  map_png <- paste0(Tempdir,"/Final_map.png")
  #####################   End of Make Map
  
  #####################   Make plot of event vs number of claims
  
  foo <- FEMA_data %>% 
    sf::st_drop_geometry() %>% 
    select(floodEvents, floodNum) %>% 
    mutate(floodEvents=stringr::str_split(floodEvents, ", "),
           floodNum=stringr::str_split(floodNum, ", ")) %>% 
    pivot_longer(everything(),
                 values_to = "Values",
                 # names_pattern = "Events|Num",
                 names_to = "Names") %>% 
    unnest(Values) %>% 
    group_by(Names) %>% 
    mutate(id = row_number()) %>% 
    ungroup() %>% 
    pivot_wider(id_cols=id,
                names_from = "Names",
                values_from = "Values") %>% 
    mutate(floodNum=as.numeric(floodNum)) %>% 
    mutate(floodEvents = fct_inorder(floodEvents)) %>%
    select(-id)
  
  Events <- foo %>% 
    ggplot(aes(x=floodEvents, y=floodNum)) +
    geom_col() +
    coord_flip() +
    labs(title=paste("Events in Blk Grp", Blk_grp),
         y="Number of Flood Claims",
         x="Flood Name")
  
  ggsave(filename = "/Events.png", 
         path = Tempdir,
         plot = Events,
         width = 7.05, 
         # height = 4,
         dpi = 150)
  
  events_png <- paste0(Tempdir,"/Events.png")
  
  #####################   End of Make plot of event vs number of claims
  
  #################### make report
  
  tempReport <- c(
    "---",
    'title: "FEMA Flood Insurance History report"',
    "output: pdf_document",
    "format: pdf",
    "params:",
    "  Title: NA",
    "  Intro: NA",
    "  comment: NA",
    "  map_png: NA",
    "  table_png: NA",
    "  table2_png: NA",
    "  events_png: NA",
    "---",
    "# `r params$Title`",
    " ",
    "`r params$Intro`",
    " ",
    "`r params$comment`",
    " ",
    "![Map](`r params$map_png`)",
    " ",
    "![Claims](`r params$table_png`)",
    " ",
    "![Vulnerability](`r params$table2_png`)",
    " ",
    "![Flood History](`r params$events_png`)"
  )
  
  
  # Set up parameters to pass to Rmd document
  
  Params <- list(Title=Title, Intro=Intro, comment=comment,
                 map_png=map_png, table_png=table_png, table2_png=table2_png, 
                 events_png=events_png)
  
  fileConn<-file(paste0(Tempdir, "/tempReport.rmd"))
  writeLines(tempReport, fileConn)
  close(fileConn)
  
  options(tinytex.verbose = TRUE)
  my_pdf <-  rmarkdown::render(paste0(Tempdir, "/tempReport.rmd"), 
                            output_file = filename,
                            params = Params,
                            envir = new.env(parent = globalenv())
  )
}

###############################################################
#           end build pdf report
###############################################################

####    Expand a bounding box
#   Expand box by 20% to give a little extra room
expand_box <- function(bbox, pct=0.2, offset=1){
  Dx <- (bbox[["xmax"]]-bbox[["xmin"]])*pct
  Dy <- (bbox[["ymax"]]-bbox[["ymin"]])*pct
  bbox["xmin"] <- bbox["xmin"] - Dx
  bbox["xmax"] <- bbox["xmax"] + Dx*offset
  bbox["ymin"] <- bbox["ymin"] - Dy*offset
  bbox["ymax"] <- bbox["ymax"] + Dy
  return(bbox)
}

#######################################################
# UI 
#######################################################
ui <- fluidPage(
  
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

    sidebarLayout(
    # layout_columns(
        sidebarPanel(
        #   Alaska has no block groups that qualify
          selectInput('State', 'Choose a state', state.name[-2], selected="Alabama"),
          
          HTML("<br/>"),
          
          sliderInput('Pct_poverty', '> % in Poverty', 
                      min=25, max=Max_poverty,
                      value=25, 
                      step=5, round=0),
          HTML("<br/>"),
          
          #   choose variable to color blocks with
          radioButtons("Block_var", "Color Blocks by:",
                       c("% Poverty"="Pct_poverty",
                         "Vulnerability"="Vuln_Index",
                         "Num of Claims"="Num_Claims_Primary",
                         "Claims per House"="ClaimsPerHousehold")),
          HTML("<br/>"),
          HTML("<br/>"),
          
          ####    Google and FEMA buttons
          splitLayout(cellWidths = c("50%", "50%"),
          column(1,
          actionButton(
            "Google",
            "Google"
          )),
          column(6,
          actionButton(
            "FEMA",
            "FEMA"
          ))),
          HTML("<br/>"),
          HTML("<br/>"),
          
          #####   Download PDF
          
          textInput("pdflabel", "Label for the PDF", "---"),
            downloadButton("FEMA_flooding", "Download PDF")
      ),
  mainPanel(
    tabsetPanel(id="inTabset",
                tabPanel("Map", value="map", leafletOutput("map", height="80vh")),
                tabPanel("Table", value="table", DT::dataTableOutput("data"))
      )
    )
  )
)

#######################################################
# server 
#######################################################
server <- function(input, output, session) {
  
  #   Set reactive value for storing chosen block group
  
  Blk_grp <- reactiveVal()

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
    print(paste("--1.1--", nrow(foo)))
    updateSliderInput(session, "Pct_poverty", 
                      min = 25,
                      max = max(5*round((max(foo$Pct_poverty, na.rm=TRUE)+2.5)/5),15),
                      step = 5)
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
    print(paste(input$Block_var, max(Grp_data()[[input$Block_var]])))
    #   Reset color scale
    if (nrow(Grp_data())==0) {return()}
    
    pal <- colorNumeric("YlOrBr",
                        c(min(Grp_data()[[input$Block_var]], na.rm=TRUE),
                          max(Grp_data()[[input$Block_var]], na.rm=TRUE)),
                        na.color = "transparent")
    Draw_blkgrp(dataset_grp(), Grp_data(), pal, input)
  })
  
  observe({
    print("--6.5--")
    #   leafpop wants group to change on redraw
    drawme <- stringr::str_remove_all(now(), "-|:|\\s|\\.")
    if (nrow(Grp_data())==0) {return()}
    
    Draw_dots(dataset_grp(), drawme)
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
    Blk_grp(event$id) # store selected block group
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
  
  output$FEMA_flooding <- downloadHandler(
    filename = "FEMA_flooding.pdf",
    content = function(file) {
      make_Pdf(file, Blk_grp(), dataset_grp(), input)
    }
  )
  
  
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
    Blk_grp(dataset_grp()[Row_added,]$censusBlockGroupFips) # store chosen blkgrp
    print(paste("---> pick in table", Blk_grp()))
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
