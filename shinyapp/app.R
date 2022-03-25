# NC Bird Atlas Shiny App
# v0.2
# 03/18/2022
# Scott K. Anderson
# https://github.com/skaclmbr


if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(mongolite)) install.packages("mongolite", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
#if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")

#libraries for spp data
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(grid)) install.packages("grid", repos = "http://cran.us.r-project.org")
if(!require(gridBase)) install.packages("gridBase", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")

#get functions from other files
source("blocks.r")
source("utils.r") #utilities file
source("spp.r") #species function file

# MAP CONSTANTS
nc_center_lat = 35.5
nc_center_lng = -79.2
nc_center_zoom = 7
nc_block_zoom = 13
ncba_blue = "#2a3b4d"

# SETUP FILES
# basemap = leaflet(ebd_data) %>% setView(lng = -78.6778808, lat = 35.7667941, zoom = 12) %>% addTiles() %>% addProviderTiles(providers$CartoDB.Positron) %>% addCircles()
current_block = ""



# Define UI for miles per gallon app ----
ui <- bootstrapPage(
  # titlePanel("NC Bird Atlas Explorer"),
  navbarPage(
    theme = shinytheme("flatly"), collapsible=TRUE,
    # theme = shinytheme("cosmo"), collapsible=TRUE,
    HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">NC Bird Atlas Data Explorer</a>'), id="nav",
    windowTitle = "NCBA Explorer",
    tags$head(includeCSS("styles.css")),
    tabPanel("Blocks",
      div(class="col-md-2 panel sidebar", id = "block_controls",

          # span(tags$i(h6("Checklists submitted to the NC Bird Atlas.")), style="color:#045a8d"),
          # h4("Map Controls"),
          h3("Block Explorer", class="tab-control-title"),
          tags$p("Summary statistics page for block-level data."),
          div(class="tab-control-group",
            h4("Checklists"),
            # checkboxInput("show_checklists","Display Checklists", FALSE ),
            prettySwitch("show_checklists","Display Checklists", value=TRUE ),
            prettySwitch("portal_records","Portal Records Only", FALSE )
          ),
          div(class="tab-control-group",
            h4("Priority Block"),
            htmlOutput("selected_block", inline=FALSE)
          ),
          # div(class="tab-control-group",
          #   h4("Date Range"),
          #   dateRangeInput("date_range", ""),
          #   sliderInput("month_range", "", min=1, max=12, value= c(1,12))
          #   # htmlOutput("selected_block", inline=FALSE)
          # )
        ),
        div(class="col-md-10",
          leafletOutput("mymap")
        ),
        div(class="col-md-3 panel",
          h4("Block Stats (placeholder)"),
          h5("Total Hours"),
          htmlOutput("65"),
          h5("Portal Hours"),
          htmlOutput("32")
        ),
        div(class="col-md-3 panel",
          h4("Block Hours"),
          plotOutput("blockhours")
        ),
        div(class="col-md-3 panel",
          h4("Species Accumulation"),
          plotOutput("spp_accumulation")
        )

    ),
    tabPanel("Species",
      div(class="container-fluid", tags$head(includeCSS("styles.css")),
        div(class="col-md-3",
          selectInput("spp_select", h3("Species"),
          choices = species_list, selected=" ")
        ),
        div(class="col-md-9",
          plotOutput("spp_breedingbox_plot"),
          div(tableOutput("breeding_code_legend"), style="font-size:60%")
          # plotOutput("spp_coords_plot"),
          # plotOutput("spp_starttimes_plot"),
          # plotOutput("spp_traveldist_plot"),
          # plotOutput("spp_mineffort_plot"),
          # plotOutput("spp_localitytype_plot")
      )
    )
    ),
    tabPanel("About",
      tags$div(
        tags$h4("NC Bird Atlas Data Explorer"),
        p("This site is a work in progress, designed to provide access to data collected through the North Carolina Bird Atlas. Data submitted to eBird is updated on a monthly basis."),
        tags$br(),
        tags$img(src = "ncba_logo_blue_halo_final.png", width = "150px", height = "75px")

        )
    )
  )

)

# Define server logic to plot various variables against mpg ----
server <- function(input, output, session) {


  ########################################################################################
  ########################################################################################
  # BLOCK TAB

  ########################################################################################
  # CHECKLISTS
  ## reactive listener for show checklist checkboxInput
  # show_checklists = reactive({
  #   paste(input$show_checklists)
  # })
  #
  # ## reactive listener for portal checkboxInput
  # show_portal_only = reactive({
  #   paste(input$portal_records)
  # })

  checklist_events <- reactive({
    list(input$show_checklists, input$portal_records, current_block_r())
  })

  ########################################################################################
  # BLOCKS
  # reactive listener for block select
  #
  current_block_r <- reactive({
    # get(input$block_select)

    geojson_info <- input$mymap_geojson_click
    paste(geojson_info$properties$ID_NCBA_BLOCK)

  })

  #label for current selected block
  output$selected_block <-renderText({
    req(current_block_r())
    paste(current_block_r())
  })

  #block hours summary plot
  output$blockhours <- renderPlot({
    # ggplot2(get_block_hours(current_block_r())$Value)
    # ggplot(get_block_hours(current_block_r()), aes(YEAR_MONTH, Value, color="#2a3b4d"))
    # ggplot(data=get_block_hours("RALEIGH_EAST-SE")) + geom_bar(mapping = aes(YEAR_MONTH, Value, color="#2a3b4d"))
    req(current_block_r())
    ggplot(data=get_block_hours(current_block_r()),aes(YEAR_MONTH, Value)) + geom_col(fill=ncba_blue)+ guides(x = guide_axis(angle = 90)) + ylab("Hours") + xlab("Year-Month")
  })

  output$spp_accumulation <- renderPlot({
    req(current_block_r())

    plot_spp_accumulation(current_block_r())

    #figure out how to get other data from this summary/analysis!
    # spp_acc_info <- plot_spp_accumulation(current_block_r())
    # spp_acc_info[1]
  })

  ########################################################################################
  # MAP
  # renders basemap on leaflet
  output$mymap <- renderLeaflet({
    leaflet() %>%
      setView(lng = nc_center_lng, lat = nc_center_lat, zoom = nc_center_zoom) %>%
      # addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addGeoJSON(priority_block_geojson, weight= 3, color=ncba_blue, fill = FALSE)
  })

  observeEvent(current_block_r(), {
      req(current_block_r())
      block_info <- filter(block_data, ID_NCBA_BLOCK==current_block_r())
      block_lat <- block_info$SE_Y + ((block_info$NW_Y - block_info$SE_Y)/2)
      block_lng <- block_info$SE_X - ((block_info$SE_X - block_info$NW_X)/2)

      leafletProxy("mymap", session) %>%
        setView(lat = block_lat, lng = block_lng , zoom = nc_block_zoom)

  })

  # show checklists on the map
  observeEvent(checklist_events(), {
    req(current_block_r())
    if (input$show_checklists){
      # check to make sure records returned!
      checklists <- get_block_checklists(current_block_r(),input$portal_records)
      if (length(checklists) > 0){
        leafletProxy("mymap") %>%
          clearMarkers() %>%
          clearShapes() %>%
          addCircles(data=checklists, color=ncba_blue)
      }
    } else {

      leafletProxy("mymap") %>%
        clearMarkers() %>%
        clearShapes()

    }
  })
  # plots checklists on map
  # observeEvent(input$portal_records,{
  #   leafletProxy("mymap") %>%
  #     clearMarkers() %>%
  #     clearShapes() %>%
  #     addCircles(data=reactive_portal())
  #
  # })

  ########################################################################################
  ########################################################################################
  # SPECIES TAB

  #######################################################
  # Species info

  current_spp_r <- reactive({
    # get(input$block_select)
    current_spp <- input$spp_select
  })

output$breeding_code_legend <- renderTable(
  breeding_codes_key,
  striped = TRUE,
  spacing = "xs"
)

output$spp_breedingbox_plot <- renderPlot({

  # PLOT BREEDING CODES ----------------------------------------------------------
  lump <- list(S = c("S", "S7", "M"), O = c("", "F", "O", "NC"))
  no_plot_codes <- NULL
  out_pdf <- NULL
  spp <- current_spp_r()
  query <- str_interp('{"OBSERVATIONS.COMMON_NAME":"${spp}"}')
  filter <- str_interp('{"OBSERVATION_DATE":1, "OBSERVATIONS.BREEDING_CODE":1, "OBSERVATIONS.COMMON_NAME":1}')
  ebird <- get_spp_obs(spp, filter)

  grid::current.viewport()
  breeding_boxplot(spp, ebird, pallet="Paired", out_pdf=NULL, no_plot_codes=no_plot_codes, lump=lump, drop=TRUE)
})

#
# # SUMMARIZE START TIMES --------------------------------------------------------
# plot(start_time_boxplot(ebird))

# # PLOT COORDINATES OF RECORDS --------------------------------------------------
# coords.plot <- plot_checklists_coords(ebird)
# plot(coords.plot)
#
# # SUMMARIZE TRAVEL DISTANCE ----------------------------------------------------
# plot(effort_distance_boxplot(ebird))
#
# # SUMMARIZE MINUTES EFFORT --------------------------------------------------------
# plot(duration_minutes_boxplot(ebird))
#
# # LOCALITY TYPE BREAKDOWN ------------------------------------------------------
# plot(locality_type_pie(ebird))



}

shinyApp(ui, server)
