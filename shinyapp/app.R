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
checklists_found = 0

# SETUP FILES
# basemap = leaflet(ebd_data) %>% setView(lng = -78.6778808, lat = 35.7667941, zoom = 12) %>% addTiles() %>% addProviderTiles(providers$CartoDB.Positron) %>% addCircles()
current_block = ""

sd <- get_safe_dates()


# Define UI for miles per gallon app ----
ui <- bootstrapPage(
  # titlePanel("NC Bird Atlas Explorer"),
  navbarPage(
    theme = shinytheme("flatly"), collapsible=TRUE,
    # theme = shinytheme("cosmo"), collapsible=TRUE,
    HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">NC Bird AtlasCache Explorer</a>'), id="nav",
    windowTitle = "Quackalacky",
    tags$head(includeCSS("styles.css")),
    tabPanel("Blocks",
      div(class="col-md-2 panel sidebar", id = "block_controls",

          # span(tags$i(h6("Checklists submitted to the NC Bird Atlas.")), style="color:#045a8d"),
          # h4("Map Controls"),
          h3("Block Explorer", class="tab-control-title"),
          tags$p("Summary statistics page for block-level data."),
          div(class="tab-control-group",
            h4("Priority Block"),
            htmlOutput("selected_block", inline=FALSE),
            htmlOutput("checklist_counter")
          ),
          div(class="tab-control-group",
            h4("Checklists"),
            # checkboxInput("show_checklists","Display Checklists", FALSE ),
            # prettySwitch("show_checklists","Display Checklists", value=TRUE ),
            prettySwitch("portal_records","Portal Records Only", FALSE ),
  #           h5("Month Range"),
  #           selectInput("start_month", label=NULL,
  # choices = list("Jan" = 1, "Feb" = 2, "Mar" = 3, "Apr" = 4, "May" = 5, "Jun" = 6, "Jul" = 7, "Aug" = 8, "Sep" = 9, "Oct" = 10, "Nov" = 11, "Dec" = 12),
  # selected = 1),
  #           selectInput("end_month",label = NULL,
  # choices = list("Jan" = 1, "Feb" = 2, "Mar" = 3, "Apr" = 4, "May" = 5, "Jun" = 6, "Jul" = 7, "Aug" = 8, "Sep" = 9, "Oct" = 10, "Nov" = 11, "Dec" = 12),
  # selected = 12)
          )
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
          h4("Block Statistics"),
          # h5("Breeding"),
          htmlOutput("block_breeding_stats")
          #should include all the requirements for completing - color coded if hit metric or not - also build/require block_status table in AtlasCache
          # h5("Non-Breeding"),
          # htmlOutput("block_nonbreeding_stats")
        ),
        div(class="col-md-3 panel",
          h4("Block Hours"),
          plotOutput("blockhours")
        ),
        div(class="col-md-3 panel",
          h4("Species Accumulation"),
          plotOutput("spp_accumulation")
        ),
        div(class="col-md-3 panel",
          h4("Species"),
          # div(tableOutput("spp_observed"), style="font-size:85%; height:442.995px; overflow-y:scroll;")
          dataTableOutput("spp_observed"),
          downloadButton("download_spplist", "Download")

        )
        # ,
        # div(class="col-md-12 panel",
        #   h4("Test Panel"),
        #   div(tableOutput("testing_output"), style="font-size:60%")
        # )

    ),
    tabPanel("Species",
      div(class="container-fluid", tags$head(includeCSS("styles.css")),
        div(class="col-md-3",
          # selectInput("spp_select", h3("Species"),
          selectizeInput("spp_select", h3("Species"),
          choices = species_list, options=list(
            placeholder = 'Select species',
            onInitialize = I('function() {this.setValue(""); }')
          ))
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
        tags$p("This site is a work in progress, designed to provide access to data collected through the North Carolina Bird Atlas. Data submitted to eBird is updated on a monthly basis."),
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

  #reactive function that listens for all sidget changes
  checklist_events <- reactive({
    # list(input$show_checklists, input$portal_records, current_block_r())
    list(input$portal_records, current_block_r())
  })

  criteria_changes <- reactive({
    # add other criteria here
    list(input$portal_records)
  })

  checklist_count <- reactive({
    unique_sei <- unique(current_block_ebd()$SAMPLING_EVENT_IDENTIFIER)
    length(unique_sei)
  })

  checklist_filtered_count <- reactive({
    unique_sei <- unique(current_block_ebd_filtered()$SAMPLING_EVENT_IDENTIFIER)
    length(unique_sei)
  })

  ########################################################################################
  # BLOCKS
  # reactive listener for block select
  #
  # current block changes when map is clicked
  current_block_r <- reactive({
    print("map clicked")
    blockmap_info <- input$mymap_shape_click
    print(blockmap_info$id)
    paste(blockmap_info$id)

  })

  # retrieves current block records when current_block_r() changes
  current_block_ebd <- reactive({
    req(current_block_r())
    print("retrieving current block data")
    #build query string from parameters
    #philosophy:
    #   - if block changes, rerun query to retrieve from AtlasCache (this function)
    #     - this returns a flattened dataset, with one row per checklist-species combination
    #   - if other parameters change, filter existing data with "current_block_ebd_filtered"
    #     - current_block_ebd_checklistsonly_filtered returns checklist level data

    cblock <- current_block_r()
    q <- str_interp('{"ID_NCBA_BLOCK":"${cblock}"}')

    get_ebd_data(q, "{}") #get all fields

  })

  # Applies filter WHEN CRITERIA CHANGES
  current_block_ebd_filtered <- reactive({
    req(current_block_ebd())

    #make sure there are records to filter!
    validate(
      need(checklist_count()>0,"")
    )
    print("applying filters to block records")

    # current_block_ebd()

      current_block_ebd() %>%
        filter(if(input$portal_records) PROJECT_CODE == "EBIRD_ATL_NC" else TRUE)

  })

  # UPDAATE CHECKLIST COUNT
  output$checklist_counter <- renderUI({
    req(current_block_ebd(), current_block_ebd_filtered(), checklist_count(), checklist_filtered_count())
    all <- paste(checklist_count(), " Total Checklists Found")
    filtered <- paste(checklist_filtered_count(), " Filtered Checklists Found")

    HTML(paste(all, filtered, sep='<br/>'))

  })

  # FILTERS BLOCK RECORDS WHEN CRITERIA CHANGES - RETURNS CHECKLIST LEVEL DATA
  current_block_ebd_checklistsonly_filtered <- reactive({

    req(current_block_ebd(), current_block_ebd_filtered())

    current_block_ebd_filtered() %>%
      filter(CATEGORY == "species") %>% # make sure only species counted
      group_by(SAMPLING_EVENT_IDENTIFIER) %>%
      mutate(SPP_COUNT = unique(GLOBAL_UNIQUE_IDENTIFIER)) %>%
      select(ALL_SPECIES_REPORTED,ATLAS_BLOCK,BCR_CODE,COUNTRY,COUNTRY_CODE,COUNTY,COUNTY_CODE,DURATION_MINUTES,EFFORT_AREA_HA,EFFORT_DISTANCE_KM,GROUP_IDENTIFIER,IBA_CODE,ID_BLOCK_CODE,ID_NCBA_BLOCK,LAST_EDITED_DATE,LATITUDE,LOCALITY,LOCALITY_ID,LOCALITY_TYPE,LONGITUDE,MONTH,NUMBER_OBSERVERS,OBSERVATION_DATE,OBSERVER_ID,PRIORITY_BLOCK,PROJECT_CODE,PROTOCOL_CODE,PROTOCOL_TYPE,SAMPLING_EVENT_IDENTIFIER,STATE,STATE_CODE,TIME_OBSERVATIONS_STARTED,TRIP_COMMENTS,USFWS_CODE,YEAR)
      # summarise(spp_count = unique(GLOBAL_UNIQUE_IDENTIFIER), .groups = 'drop')
      # filter(if(input$portal_records) PROJECT_CODE == "EBIRD_ATL_NC" else TRUE)
      # ADD ADDITIONAL FILTERS HERE AS NEEDED

  })
  # POPULATE LABEL FOR CURRENT BLOCK
  output$selected_block <-renderText({
    req(current_block_r())
    blockname <- current_block_r()

    strHTML <- str_interp('<strong>${blockname}</strong>')
    HTML(strHTML)

    # paste(current_block_r())
  })

  ## TESTING - table output
  # output$testing_output <- renderTable({
  #
  #
  #   paste(select(current_block_ebd_checklistsonly_filtered()),
  #   striped = TRUE,
  #   spacing = "xs")
  # })


  # BREEDING STATS
  output$block_breeding_stats <- renderUI({
    req(current_block_ebd(), current_block_ebd_filtered())

    #ensure records returned
    validate(
      need(current_block_ebd(), "No checklists submitted.")
    )

    sa_list <- spp_accumulation_results()$spp_unique

    spp_total <- nrow(sa_list["spp"])
    confirmed_total <- nrow(filter(sa_list, bcat == "C4" ))
    if ((spp_total*0.5)<confirmed_total) {
      confirmed_class = "success"
    } else {
      confirmed_class = "failed"
    }

    #add conditional formatting if criteria met
    num_spp_total <- paste("Species: ", nrow(sa_list["spp"]) )
    num_breed_confirm <- paste("Confirmed (C4):<span class='",confirmed_class, "'>", confirmed_total, "</span>")
    num_breed_prob <- paste("Probable (C3):", nrow(filter(sa_list, bcat == "C3" )))
    num_breed_poss <- paste("Possible (C2):", nrow(filter(sa_list, bcat == "C2" )))
    num_breed_hours <- paste("Hours:", format(spp_accumulation_results()$hrs_total, trim=TRUE, digits=1))

    HTML(paste(num_spp_total, num_breed_confirm, num_breed_prob, num_breed_poss, num_breed_hours, sep='<br/>'))
  })


  # DISPLAY BLOCK HOURS SUMMARY PLOT
  output$blockhours <- renderPlot({
    # ggplot2(get_block_hours(current_block_r())$Value)
    # ggplot(get_block_hours(current_block_r()), aes(YEAR_MONTH, Value, color="#2a3b4d"))
    # ggplot(data=get_block_hours("RALEIGH_EAST-SE")) + geom_bar(mapping = aes(YEAR_MONTH, Value, color="#2a3b4d"))
    req(current_block_r())
    ggplot(data=get_block_hours(current_block_r()),aes(YEAR_MONTH, Value)) + geom_col(fill=ncba_blue)+ guides(x = guide_axis(angle = 90)) + ylab("Hours") + xlab("Year-Month")
  })

  # DISPLAY SPECIES ACCUMULATION PLOT
  spp_accumulation_results <- reactive({
    req(current_block_ebd(), current_block_ebd_filtered())
    # pass only those columns needed
    sa <- filter(current_block_ebd_filtered(), CATEGORY == "species")[c("SAMPLING_EVENT_IDENTIFIER", "OBSERVATION_DATE", "DURATION_MINUTES", "BREEDING_CODE", "BREEDING_CATEGORY", "COMMON_NAME", "CATEGORY")]

    plot_spp_accumulation(sa)

  })

  output$spp_accumulation <- renderPlot({
    req(spp_accumulation_results())
    spp_accumulation_results()$plot

  })

  # DISPLAY SPECIES LIST
  output$spp_observed <- renderDataTable(
    spp_accumulation_results()$spp_unique[c("spp","bcat")], options=list(pageLength=5, autoWidth = TRUE)
  )

  output$download_spplist <- downloadHandler(
    filename = function() {
      paste("spp_list", ".csv", sep = "")
    },
    content = function(file) (
      write.csv(spp_accumulation_results()$spp_unique, file, row.names = TRUE)
    )
  )


  ########################################################################################
  # MAP
  # SETUP LEAFLET MAP, RENDER BASEMAP
  output$mymap <- renderLeaflet({

    #setup block geojson layer
    print("starting map, adding blocks")

    leaflet() %>%
      setView(lng = nc_center_lng, lat = nc_center_lat, zoom = nc_center_zoom) %>%
      # addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      # addGeoJSON(priority_block_geojson, weight= 1, color=ncba_blue, opacity=0.6, fillColor='#777777', fillOpacity = 0.05, fill = TRUE)
      addRectangles(data = priority_block_data, layerId = ~ ID_NCBA_BLOCK, lng1 = ~ NW_X, lat1 = ~ NW_Y, lng2 = ~ SE_X, lat2 = ~ SE_Y, weight= 1, color=ncba_blue, opacity=0.6, fillColor='#777777', fillOpacity = 0.05, fill = TRUE, label = ~ ID_NCBA_BLOCK)
      # addRectangles(data = priority_block_data, layerId = ~ ID_NCBA_BLOCK, lng1 = ~ NW_X, lat1 = ~ NW_Y, lng2 = ~ SE_X, lat2 = ~ SE_Y, weight= 1, color=ncba_blue, opacity=0.6, fillColor='#777777', fillOpacity = 0.05, fill = TRUE, label = ~ ID_NCBA_BLOCK , labelOptions = labelOptions(noHide = T, textOnly = TRUE, offset(c(-30, 30)),
      #   style = list(
      #     "color" = "#444444",
      #     "font-size" = "8px"
      #   )))
  })

  # ZOOM MAP TO SELECTED BLOCK
  observeEvent(current_block_r(), {
      req(current_block_r())
      block_info <- filter(block_data, ID_NCBA_BLOCK==current_block_r())

      #calculate the center of the block
      block_center_lat <- block_info$SE_Y + ((block_info$NW_Y - block_info$SE_Y)/2)
      block_center_lng <- block_info$SE_X - ((block_info$SE_X - block_info$NW_X)/2)

      leafletProxy("mymap", session) %>%
        setView(lat = block_center_lat, lng = block_center_lng , zoom = nc_block_zoom)
  })

  # popup menu on hover over checklist


  # DISPLAY CHECKLISTS ON THE MAP
  observeEvent(current_block_ebd_checklistsonly_filtered(), {
    # req(current_block_r())
    req(current_block_ebd_checklistsonly_filtered())
    # check to make sure records returned!
    # checklists <- get_block_checklists(current_block_r(),input$portal_records)
    checklists <- current_block_ebd_checklistsonly_filtered()[c("LATITUDE","LONGITUDE", "SAMPLING_EVENT_IDENTIFIER", "LOCALITY_ID", "LOCALITY", "OBSERVATION_DATE")]
    if (length(checklists) > 0){
      leafletProxy("mymap") %>%
        clearMarkers() %>%
        # clearShapes() %>%
        addCircleMarkers( data = checklists, lat = ~ LATITUDE, lng = ~ LONGITUDE, radius = 5, color=ncba_blue, stroke=FALSE, fillOpacity = 0.6, label = sprintf("<strong>%s</strong><br/>%s<br/>%s",checklists$SAMPLING_EVENT_IDENTIFIER, checklists$LOCALITY, checklists$OBSERVATION_DATE) %>% lapply(htmltools::HTML) )
        # addMarkers(data=checklists, layerId = paste("checklist",~ SAMPLING_EVENT_IDENTIFIER), lat = ~ LATITUDE, lng = ~ LONGITUDE, color=ncba_blue,
        #   label = sprintf("<strong>%s</strong><br/>%s<br/>%s",checklists$SAMPLING_EVENT_IDENTIFIER, checklists$LOCALITY, checklists$OBSERVATION_DATE) %>% lapply(htmltools::HTML),
        #   labelOptions = labelOptions(
        #    style = list("font-weight" = "normal", padding = "3px 8px", "color" = ncba_blue),
        #    textsize = "0.8rem", direction = "auto")
        #   )
    }

  })

  ########################################################################################
  ########################################################################################
  # SPECIES TAB

  #######################################################
  # Species info

  # current_spp_r <- reactive({
  #   # get(input$block_select)
  #   current_spp <- input$spp_select
  # })

output$breeding_code_legend <- renderTable(
  breeding_codes_key,
  striped = TRUE,
  spacing = "xs"
)

output$spp_breedingbox_plot <- renderPlot({

  # check to make sure species is selected
  validate(
    need(input$spp_select, 'select a species from the list')
  )
  # PLOT BREEDING CODES ----------------------------------------------------------
  lump <- list(S = c("S", "S7", "M"), O = c("", "F", "O", "NC"))
  no_plot_codes <- NULL
  out_pdf <- NULL
  spp <- input$spp_select
  # query <- str_interp('{"OBSERVATIONS.COMMON_NAME":"${spp}"}')
  filter <- str_interp('{"OBSERVATION_DATE":1, "OBSERVATIONS.BREEDING_CODE":1, "OBSERVATIONS.COMMON_NAME":1}')
  print("get ebd records")
  ebird <- get_spp_obs(spp, filter)

  print("ebd records retrieved, plotting data")
  # grid::current.viewport()
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
