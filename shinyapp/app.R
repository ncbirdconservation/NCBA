# NC Bird Atlas Shiny App
# v0.2
# 12/08/2022
# Scott K. Anderson, Elsa Chen, Nathan Tarr, Scott Pearson
# https://github.com/nmtarr/NCBA/shinyapp


if(!require(shiny)) install.packages(
  "shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages(
  "shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages(
  "tidyverse", repos = "http://cran.us.r-project.org")
if(!require(mongolite)) install.packages(
  "mongolite", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages(
  "dplyr", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages(
  "leaflet", repos = "http://cran.us.r-project.org")
if(!require(htmltools)) install.packages(
  "htmltools", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages(
  "shinythemes", repos = "http://cran.us.r-project.org")
# if(!require(waffle)) install.packages(
#   "waffle", repos = "https://cinc.rud.is")

#adds functions for tooltips
if(!require(shinyBS)) install.packages(
  "shinyBS", repos = "http://cran.us.r-project.org")
# if(!require(geojsonio)) install.packages(
#   "geojsonio", repos = "http://cran.us.r-project.org")

#libraries for spp data
if(!require(lubridate)) install.packages(
  "lubridate", repos = "http://cran.us.r-project.org")
if(!require(grid)) install.packages(
  "grid", repos = "http://cran.us.r-project.org")
if(!require(gridBase)) install.packages(
  "gridBase", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages(
  "RColorBrewer", repos = "http://cran.us.r-project.org")

#get functions from other files
source("blocks.r")
source("utils.r") #utilities file
source("spp.r") #species function file
source("blkcumhrfunc.r") #block hour graph
source("ncba_functions_shiny.r")

# MAP CONSTANTS
nc_center_lat = 35.5
nc_center_lng = -79.2
nc_center_zoom = 7
nc_block_zoom = 13
ncba_blue = "#2a3b4d"
checklists_found = 0
ncba_failed = "#DB504A"
ncba_success = "#43AA8B"
ncba_white = "#ffffff"

# SETUP FILES
current_block = ""

sd <- get_safe_dates()


# Define UI for Quackalacky Data app -----------------------------------------
ui <- bootstrapPage(
  # titlePanel("NC Bird Atlas Explorer"),
  navbarPage(
    theme = shinytheme("flatly"), collapsible=TRUE,
    # theme = shinytheme("cosmo"), collapsible=TRUE,
    HTML(
      paste0(
        '<a style="text-decoration:none;cursor:default;color:#FFFFFF;"',
        ' class="active" href="#">NC Bird AtlasCache Explorer</a>'
        )
      ),
    id="nav",
    windowTitle = "NCBA Block Explorer",
    tags$head(includeCSS("styles.css")),
    tags$head(tags$link(rel="icon", href="/input_data/ncba_blue_wbnu.ico")),
    tabPanel("Blocks",
      div(class="col-md-2 panel sidebar", id = "block_controls",

          # span(
          #   tags$i(h6("Checklists submitted to the NC Bird Atlas.")),
          #   style="color:#045a8d"
          #   ),
          # h4("Map Controls"),
          h3("Block Explorer", class="tab-control-title"),
          tags$p(
            "Summary statistics page for block-level data.",
            class="desc-text"
            ),
          div(class="tab-control-group",
            selectInput(inputId = "APBlock", #name of input
            label = "Selected Priority Block", #label displayed in ui
            choices = c(
              as.character(unique(priority_block_data$ID_NCBA_BLOCK)),
              "NONE"),
            # calls unique values from the ID_NCBA_BLOCK column
            # in the previously created table
            selected = "NONE"),
            htmlOutput("checklist_counter")
          ),
          div(class="tab-control-group",
           prettySwitch("portal_records","Portal Records Only", TRUE ),
            radioButtons("season_radio",label = h4("Season"),
              choices = list(
                "All Records" = "All",
                "Breeding" = "Breeding",
                "Non-Breeding" = "Non-Breeding",
                "Custom" = "Custom"),
              selected = "All"),
           conditionalPanel(condition = "input.season_radio == 'Custom'",
                            selectInput(
                              "month_range",
                              "Selected Months",
                              choices = c(1,2,3,4,5,6,7,8,9,10,11,12),
                              selected = c(1,2,3,4,5,6,7,8,9,10,11,12),
                              multiple = TRUE)
           ),
              # bsTooltip(
              #   "season_radio",
              #   paste0("Seasons calculated from species-specific",
              #   " safe dates (where available)."),
              #   "right", options = list(container="body"))
          )
        ),
        div(class="col-md-10 panel",
          leafletOutput("mymap")
        ),
        div(class="col-md-3 panel",
          h4("Block Statistics"),
          # h5("Breeding"),
          htmlOutput("block_breeding_stats"),
          downloadButton("download_block_checklists", "Download Checklists")
          #should include all the requirements for completing
          # color coded if hit metric or not
          # also build/require block_status table in AtlasCache

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
        div(class="col-md-6 panel",
          h4("Species"),
          # div(
          #   tableOutput("spp_observed"),
          #   style="font-size:85%; height:442.995px; overflow-y:scroll;")
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
    tabPanel("Species Map",

        div(class = "col-md-2",
          div(selectizeInput("sppmap_select", h3("Species"),
          choices = species_list, options = list(
            placeholder = 'Select species',
            onInitialize = I('function() {this.setValue(""); }')
          ))),
          div(tableOutput("block_breedcode_table"))
        ),
        div(class = "col-md-10",
            id = "spp-block-map",
            leafletOutput("mysppmap")
          )
    ),
    tabPanel("About",
      tags$div(
        tags$h4("NC Bird Atlas Data Explorer"),
        tags$p(
          paste0("This site is a work in progress, designed to provide access",
          " to data collected through the North Carolina Bird Atlas. ",
          "Data submitted to eBird is updated on a monthly basis.")),
        tags$br(),
        tags$img(
          src = "ncba_logo_blue_halo_final.png",
          width = "150px",
          height = "75px")

        )
    )
  )

)

# Define server logic to plot various variables against mpg
server <- function(input, output, session) {


## BLOCK TAB  ----------------------------------------------------

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
  # checklist_events <- reactive({
  #   # list(input$show_checklists, input$portal_records, current_block_r())
  #   list(input$portal_records, current_block_r(), input$season_radio)
  # })

  # listens for changes in the checklist filters:
  #   portal_records and season_radio
  # criteria_changes <- reactive({
  #   # add other criteria here
  #   list(input$portal_records, input$season_radio)
  # })

  # listens for changes in the checklist filters: 
  #   portal_records and season_radio
  # used to ensure there is at least one record to query
  checklist_count <- reactive({
    unique_sei <- unique(current_block_ebd()$SAMPLING_EVENT_IDENTIFIER)
    length(unique_sei)
  })

  checklist_filtered_count <- reactive({
    unique_sei <- 
      unique(current_block_ebd_filtered()$SAMPLING_EVENT_IDENTIFIER)
    length(unique_sei)
  })

  ### BLOCK TAB BLOCKS ----------------------------------------------------
  # reactive listener for block select
  #
  # Creating a reactive value so it can be updated outside of current_block_r()
  # I think current_block_r() could just be totally replaced with rv_block$id?

  rv_block <- reactiveValues(chosen=NULL, id =NULL)

  # grab id from map click
  observeEvent (input$mymap_shape_click, {
    blockmap_info <- input$mymap_shape_click
    blockmap_info_id <- input$mymap_shape_click$id

    rv_block$chosen <- blockmap_info
    rv_block$id <-  blockmap_info_id
  })

  # current block changes when map is clicked
  current_block_r <- reactive({
    print("map clicked")
    print(rv_block$id)
    paste(rv_block$id)
  })

  # when map block clicked, update select input drop down list
  # and when clicking current block it doesn't clear the name from the drop down list
  observeEvent(input$mymap_shape_click, {
    print("Updating drop down list to match clicked block")
    click <- input$mymap_shape_click
    if(click$id %in% input$APBlock & click$id != input$APBlock)
      selected = input$APBlock[input$APBlock != click$id]
    else
      selected = c(input$APBlock, click$id)
    updateSelectInput(session, "APBlock",
                      selected = selected)
  })

  #### React to Change of Selected Block in Drop down list instead of click,
  ## a bit redundant with the one above but I am not sure how to combine them?
  observeEvent(input$APBlock, {
    print("Block selected from drop down list")
    blockmap_list_id <- input$APBlock
    if(input$APBlock == "NONE")
      rv_block$id = NULL
    else
      rv_block$id = blockmap_list_id
  })

  # retrieves current block records when current_block_r() changes
  current_block_ebd <- reactive({
    req(current_block_r())
    print("retrieving current block data")
    #build query string from parameters
    #philosophy:
    #   - if block changes, rerun query to retrieve from
    #       AtlasCache (this function)
    #       - this returns a flattened dataset,
    #           with one row per checklist-species combination
    #   - if other parameters change,
    #       filter existing data with "current_block_ebd_filtered"
    #   - current_block_ebd_checklistsonly_filtered
    #       returns checklist level data

    cblock <- rv_block$id

    q <- str_interp('{"ID_NCBA_BLOCK":"${cblock}"}')

    get_ebd_data(q, "{}") #get all fields

  })


  # Applies filter WHEN CRITERIA CHANGES
  current_block_ebd_filtered <- reactive({
    req(current_block_ebd())
    print(head(current_block_ebd()$EBD_NOCTURNAL))
    #make sure there are records to filter!
    validate(
      need(checklist_count()>0,"")
    )
    print("applying filters to block records")

    # current_block_ebd()

      current_block_ebd() %>%
        filter(
          if(input$portal_records)
          PROJECT_CODE == "EBIRD_ATL_NC"
          else TRUE) %>%
        filter(
          if (input$season_radio == "Breeding")
            MONTH %in% c("4","5","6","7","8")
           else if (input$season_radio == "Non-Breeding") 
              MONTH %in% c("1","2","11","12")
              else if (input$season_radio == "Custom")
             MONTH %in% input$month_range
          else TRUE
        )
          # if(input$season_radio != "All")
          # SEASON == input$season_radio
          # else TRUE)

  })


  # UPDATE CHECKLIST COUNT
  output$checklist_counter <- renderUI({
    req(
      current_block_ebd(),
      current_block_ebd_filtered(),
      checklist_count(),
      checklist_filtered_count())

    all <- paste(checklist_count(), " Total Checklists Found")

    filtered <- paste(
      checklist_filtered_count(),
      " Filtered Checklists Found")

    HTML(paste(all, filtered, sep='<br/>'))

  })

  #### FILTERS BLOCK RECORDS WHEN CRITERIA CHANGES
  #     RETURNS CHECKLIST LEVEL DATA ------
  current_block_ebd_checklistsonly_filtered <- reactive({

    req(current_block_ebd(), current_block_ebd_filtered())

    current_block_ebd_filtered() %>%
      filter(CATEGORY == "species") %>% # make sure only species counted
      group_by(SAMPLING_EVENT_IDENTIFIER) %>%  # nolint
      mutate(SPP_COUNT = length(unique(GLOBAL_UNIQUE_IDENTIFIER))) %>%
      ungroup(SAMPLING_EVENT_IDENTIFIER) %>%
      distinct(SAMPLING_EVENT_IDENTIFIER, .keep_all = TRUE) %>%
      select(ALL_SPECIES_REPORTED, SPP_COUNT, ATLAS_BLOCK,BCR_CODE,COUNTRY, # nolint
        COUNTRY_CODE,COUNTY,COUNTY_CODE,DURATION_MINUTES,EFFORT_AREA_HA, # nolint
        EFFORT_DISTANCE_KM,GROUP_IDENTIFIER,IBA_CODE,ID_BLOCK_CODE, # nolint
        ID_NCBA_BLOCK,LAST_EDITED_DATE,LATITUDE,LOCALITY,LOCALITY_ID, # nolint
        LOCALITY_TYPE,LONGITUDE,MONTH,NUMBER_OBSERVERS,OBSERVATION_DATE, # nolint
        OBSERVER_ID,PRIORITY_BLOCK,PROJECT_CODE,PROTOCOL_CODE,PROTOCOL_TYPE, # nolint
        SAMPLING_EVENT_IDENTIFIER,STATE,STATE_CODE,TIME_OBSERVATIONS_STARTED, # nolint
        TRIP_COMMENTS,USFWS_CODE,YEAR,EBD_NOCTURNAL) %>% # nolint
      # ADD ADDITIONAL FILTERS HERE AS NEEDED
      # create a column with the html link https://ebird.org/checklist/SID
      mutate(
        link = paste(htmlEscape("https://ebird.org/checklist"),
        SAMPLING_EVENT_IDENTIFIER, sep = "/")) %>%
      # then a column with the HTML code for part of the popup label
      mutate(
        ebird_link = paste0("",'<a style="font-weight:bold" href="',
          link,
          '"target="_blank">',htmlEscape(SAMPLING_EVENT_IDENTIFIER),
          '</a> <br>',"Date: ", htmlEscape(OBSERVATION_DATE), "<br>",
          "Start Time: ", htmlEscape(TIME_OBSERVATIONS_STARTED), "<br>",
          "Length (Minutes): ", htmlEscape(DURATION_MINUTES), "<br>",
          "Distance (km): ", htmlEscape(EFFORT_DISTANCE_KM), "<br>"
          )
        )
  })




  # # POPULATE LABEL FOR CURRENT BLOCK
  # output$selected_block <-renderText({
  #   req(current_block_r())
  #   blockname <- current_block_r()
  #
  #   strHTML <- str_interp('<strong>${blockname}</strong>')
  #   HTML(strHTML)

    # paste(current_block_r())
  # })
  output$download_block_checklists <- downloadHandler(
    filename = function() {
      p <- ""
      if (input$portal_records){ p <- "portal"}

      paste(
        current_block_r(),
        input$season_radio,p,
        "blockchecklists.csv",
        sep = "_")
      # paste("test.RData")
    },
    content = function(file) (
      # save(current_block_ebd_checklistsonly_filtered(), file)
      # save(current_block_ebd_checklistsonly_filtered(), file)
      # load(file, verbose=T)
      write.csv(
        current_block_ebd_checklistsonly_filtered(),
        file,
        row.names = TRUE)
    )

  )

### For downloading testing datasets when conditions change (Deprecated?) ####
# observeEvent(current_block_ebd_checklistsonly_filtered(),{
#   # req(current_block_ebd_checklistsonly_filtered())
#   print("saving data to file...")
#   recs <- current_block_ebd_checklistsonly_filtered()
#   p <- ""
#   if (input$portal_records){ p <- "portal"}
#
#   fn <- paste(current_block_r(), input$season_radio,p,
#     "blockchecklists.RData", sep = "_")
#   save(recs, file=fn, compress="gzip")
#   print("successfullly saved data")
# })

  ## BREEDING STATS ----------------------------------------------------
  output$block_breeding_stats <- renderUI({
    req(current_block_ebd(), current_block_ebd_filtered())
    print("rendering block stats")
    #ensure records returned
    validate(
      need(current_block_ebd(), "No checklists submitted.")
    )


    ### Block Species ----------------------------------------------------
    sa_list <- spp_accumulation_results()$spp_unique

    spp_total <- nrow(sa_list["spp"])
    # confirmed_total <- nrow(filter(sa_list, bcat == "C4" ))
    # if ((spp_total*0.5)<confirmed_total) {
    #   confirmed_class = "success"
    # } else {
    #   confirmed_class = "failed"
    # }

    # add conditional formatting if criteria met
    num_spp_total = paste("Species: ", nrow(sa_list["spp"]) )
    print(num_spp_total)
    num_c = nrow(filter(sa_list, bcat == "C4" ))
    num_r = nrow(filter(sa_list, bcat == "C3"))
    num_p = nrow(filter(sa_list, bcat == "C2"))
    num_coded = num_c + num_r + num_p
    print(num_coded)
    num_coded <- num_coded %>% replace(is.na(.),0)
    # num_o = num_spp_total - num_coded
    pct_c = (num_c/num_coded)*100
    pct_r = (num_r/num_coded)*100
    pct_p = (num_p/num_coded)*100
    print(pct_c)
    print(pct_r)
    print(pct_p)
    # leg_p = paste0('Possible (', pct_p , '%)')
    # leg_r = paste0('Probable (', pct_r , '%)')
    # leg_c = paste0('Confirmed (', pct_c , '%)')

    num_breed_confirm <- paste(
      "Confirmed (C4):", num_c,  " (", format(pct_c, digits=1), "%)")
    num_breed_prob <- paste(
      "Probable (C3):", num_r, " (", format(pct_r,digits=1) , "%)")
    num_breed_poss <- paste(
      "Possible (C2):", num_p, " (", format(pct_p,digits=1), "%)")
    
    spp_crp <- c('Possible' = num_p, 'Probable' = num_r, 'Confirmed' = num_c)
    # waf <- waffle(
    #       spp_crp,
    #       rows = 5,
    #       size = 1,
    #       colors = c('#BF78EB',alpha('#7E2AB3', 1/3),'#300C56'),
    #       title = "Species Breeding Status",
    #       legend_pos = "bottom"
    #   )
    # print(waf)

    ### Block Hours ----------------------------------------------------
    diurnal_hours <- block_hrs_results()$total_hr - block_hrs_results()$noc_hr

    diurnal_hours_target <- 20
    if (input$season_radio == "Non-Breeding") {
      diurnal_hours_target <- 10
    }

    diurnal_hours_class <- "failed"
    if (diurnal_hours >= diurnal_hours_target) {
      diurnal_hours_class <- "success"
    }

    nocturnal_hours_class <- "failed"
    if (block_hrs_results()$noc_hr >= 2) {
      nocturnal_hours_class <- "success"
    }

    num_diurnal_hours <- paste(
      "Diurnal:<span class='",diurnal_hours_class, "'>",
      format(diurnal_hours, trim=TRUE, digits=1),
      " hrs</span>")
    num_nocturnal_hours <- paste(
      "Nocturnal:<span class='", nocturnal_hours_class, "'>",
      format(block_hrs_results()$noc_hr, trim=TRUE, digits=1),
      " hrs</span>")
    num_total_hours <- paste(
      "Total:<span class=''>",
      format(block_hrs_results()$total_hr, trim=TRUE, digits=1),
      " hrs</span>")

    print("troubleshooting duplicate block stats:")
    # print("block hrs results:total_hr", 
    #   format(block_hrs_results()$total_hr, trim=TRUE, digits=1))
    # print(block_hrs_results())

    # HTML(paste(
    #   num_spp_total, num_breed_confirm, num_breed_prob, num_breed_poss,
    #   num_breed_hours, sep='<br/>'))
    HTML(paste(
      "<h4>Species</h4>",
      num_spp_total,
      num_breed_confirm,
      num_breed_prob,
      num_breed_poss,
      "<h4>Hours</h4>",
      num_diurnal_hours,
      num_nocturnal_hours,
      num_total_hours,
      sep='<br/>'))

  })


  #### DISPLAY BLOCK HOURS SUMMARY PLOT ------
  block_hrs_results <- reactive({
    req(current_block_ebd_checklistsonly_filtered())
    print("running block hrs results")
    block_hrs(current_block_ebd_checklistsonly_filtered())

  })
  output$blockhours <- renderPlot({
    req(current_block_ebd_checklistsonly_filtered())
    block_hrs_results()$hr_plot
  })

  #### DISPLAY SPECIES ACCUMULATION PLOT ------
  spp_accumulation_results <- reactive({
    req(current_block_ebd(), current_block_ebd_filtered())
    # pass only those columns needed
    sa <- filter(
      current_block_ebd_filtered(),
      CATEGORY == "species"
      )[c("SAMPLING_EVENT_IDENTIFIER", "OBSERVATION_DATE", "DURATION_MINUTES",
      "BREEDING_CODE", "BREEDING_CATEGORY", "COMMON_NAME", "CATEGORY")]

    cblock <- current_block_r()

    ## Retrieves appropriate data through aggregation query
    ## _id = combination of block name and species
    ## spp = species common name
    ## maxBC = maximum Breeding Code
    pipeline <- str_interp(c(
      '[{"$match": {"ID_NCBA_BLOCK": "${cblock}"}},',
      '{"$unwind": {"path": "$OBSERVATIONS"}}, ',
      '{"$match": {"OBSERVATIONS.CATEGORY": "species"}},',
      '{"$group": {"_id": {"blockName": "$ID_NCBA_BLOCK",',
      '"spp": "$OBSERVATIONS.COMMON_NAME"},',
      '"maxBC": {"$max": "$OBSERVATIONS.BREEDING_CATEGORY"}}}]'
      ))

    print(pipeline)
    # pipeline <- str_interp(
    #   paste0('[{"$match": {"ID_NCBA_BLOCK": "${cblock}"}},',
    #   '{"$unwind": {"path": "$OBSERVATIONS"}},',
    #   '{"$group": {"_id": {"blockName": "$ID_NCBA_BLOCK",',
    #   '"spp": "$OBSERVATIONS.COMMON_NAME"},',
    #   '"maxBC": {"$max": "$OBSERVATIONS.BREEDING_CATEGORY"}}}]')
    #   )

    # pipeline <- str_interp(
    #   paste0(
    #     '[{"$match": {"ID_NCBA_BLOCK": "${cblock}"}},',
    #     '{"$unwind": {"path": "$OBSERVATIONS"}},',
    #     '{"$group":',
    #     '{"_id": {"block": "$ID_BLOCK_CODE",',
    #     '"blockName": "$ID_NCBA_BLOCK","spp": "$OBSERVATIONS.COMMON_NAME",',
    #     '"taxon": "$OBSERVATIONS.SCIENTIFIC_NAME"},',
    #     '"maxBC": {"$max": "$OBSERVATIONS.BREEDING_CATEGORY"},',
    #     '"bcList": {"$push": "$OBSERVATIONS.BREEDING_CATEGORY"}}}]')
    #     )

    spp_bcs <- aggregate_ebd_data(pipeline)
    plot_spp_accumulation(sa, spp_bcs)

  })

  output$spp_accumulation <- renderPlot({
    req(spp_accumulation_results())
    # print(spp_accumulation_results()$spp_acc_data)
    spp_accumulation_results()$plot

  })

  #### DISPLAY SPECIES LIST ------
  output$spp_observed <- renderDataTable(
    spp_accumulation_results()$spp_unique[
      c("spp","bcat")],
      options=list(pageLength=5, autoWidth = TRUE)
  )

  output$download_spplist <- downloadHandler(
    filename = function() {
      paste("spp_list", ".csv", sep = "")
    },
    content = function(file) (
      write.csv(spp_accumulation_results()$spp_unique, file, row.names = TRUE)
    )
  )



  ## MAP  ----------------------------------------------------
  ### SETUP LEAFLET MAP, RENDER BASEMAP ------
  output$mymap <- renderLeaflet({

    #setup block geojson layer
    print("starting map, adding blocks")

    leaflet() %>%
      setView(
        lng = nc_center_lng,
        lat = nc_center_lat,
        zoom = nc_center_zoom) %>%
      # addTiles() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      # addProviderTiles(providers$CartoDB.Positron) %>%
      # addGeoJSON(
        # priority_block_geojson,
        # weight= 1,
        # color=ncba_blue,
        # opacity=0.6,
        # fillColor="#777777',
        # fillOpacity = 0.05,
        # fill = TRUE
        # )

      addRectangles(
        data = priority_block_data,
        layerId = ~ ID_NCBA_BLOCK,
        lng1 = ~ NW_X,
        lat1 = ~ NW_Y,
        lng2 = ~ SE_X,
        lat2 = ~ SE_Y,
        weight= 2,
        color=ncba_white,
        opacity = 0.9,
        fillColor='#777777',
        fillOpacity = 0.05,
        fill = TRUE,
        label = ~ ID_NCBA_BLOCK
        )
      # addRectangles(
      #   data = priority_block_data,
      #   layerId = ~ ID_NCBA_BLOCK,
      #   lng1 = ~ NW_X,
      #   lat1 = ~ NW_Y,
      #   lng2 = ~ SE_X,
      #   lat2 = ~ SE_Y,
      #   weight= 1,
      #   color=ncba_blue,
      #   opacity=0.6,
      #   fillColor='#777777',
      #   fillOpacity = 0.05,
      #   fill = TRUE,
      #   label = ~ ID_NCBA_BLOCK,
      #   labelOptions = labelOptions(
      #     noHide = T,
      #     textOnly = TRUE,
      #     offset(c(-30, 30)),
      #   style = list(
      #     "color" = "#444444",
      #     "font-size" = "8px"
      #   ))
      #   )
  })

  ### ZOOM MAP TO SELECTED BLOCK ------
  observeEvent(current_block_r(), {
      req(current_block_r())
      block_info <- filter(block_data, ID_NCBA_BLOCK==current_block_r())

      #calculate the center of the block
      block_center_lat <- block_info$SE_Y +
        ((block_info$NW_Y - block_info$SE_Y)/2)
      block_center_lng <- block_info$SE_X - 
        ((block_info$SE_X - block_info$NW_X)/2)

      leafletProxy("mymap", session) %>%
        setView(
          lat = block_center_lat,
          lng = block_center_lng,
          zoom = nc_block_zoom
          )
  })

   ## SPECIES MAP  ----------------------------------------------------
  ### SETUP LEAFLET MAP, RENDER BASEMAP ------
  output$mysppmap <- renderLeaflet({

    #setup block geojson layer
    print("starting map, adding blocks")

    leaflet() %>%
      setView(
        lng = -79.0,
        lat = 35.7,
        zoom = 7) %>%
      # addTiles() %>%
      addProviderTiles(providers$Esri.WorldTopoMap)
      # addProviderTiles(providers$Esri.NatGeoWorldMap) %>%

      # addRectangles(
      #   data = priority_block_data,
      #   layerId = ~ ID_NCBA_BLOCK,
      #   lng1 = ~ NW_X,
      #   lat1 = ~ NW_Y,
      #   lng2 = ~ SE_X,
      #   lat2 = ~ SE_Y,
      #   weight= 2,
      #   color=ncba_blue,
      #   opacity = 0.9,
      #   fillColor='#777777',
      #   fillOpacity = 0.05,
      #   fill = TRUE,
      #   label = ~ ID_NCBA_BLOCK
      #   )

  })
  ### ADD SYMBOLOGY FOR SELECTED SPECIES ------
  sppblock_data <- reactive({
      get_spp_by_block(input$sppmap_select)
  })

  observeEvent(
    sppblock_data(),
    {
      print("retrieving spp block data")
      sppblockmap_data <- sppblock_data()
      print("spp block data retrieved")
      # colnames(sppblockmap_data)[1] = "ID_NCBA_BLOCK"
      # print(head(sppblockmap_data))

      if (nrow(sppblockmap_data) > 0){
      # print(head(priority_block_data))
      print (paste0("spp by block len = ",nrow(sppblockmap_data)))
      spp_blocks <- merge(
        priority_block_data,
        sppblockmap_data,
        by = "ID_NCBA_BLOCK"
        )
      # print(head(spp_blocks))
      print(paste0("spp block len = ",nrow(spp_blocks)))


      spp_blocks <- mutate(
        spp_blocks,
        breedcat = case_when(
          bc == "C4" ~ "C4 Confirmed",
          bc == "C3" ~ "C3 Probable",
          bc == "C2" ~ "C2 Possible",
          TRUE ~ "C1 Observed"
        )
      )

      spp_blocks <- mutate(
        spp_blocks,
        blocklink = sprintf('https://ebird.org/atlasnc/block/%s', spp_blocks$ID_BLOCK_CODE)
      )
      print(table(t(spp_blocks$breedcat)))
      output$block_breedcode_table <- renderTable(table(spp_blocks$breedcat))

      # breedcat_order <- factor(
      #   spp_blocks$breedcat,
      #   labels = c('Confirmed', 'Probable', 'Possible','Observed')
      #   )
      # print(breedcat_order)
      
      block_colors <- colorFactor(
        palette = brewer.pal(4, 'Purples'),
        # palette = 'Blues',
        # levels = c('Confirmed', 'Probable', 'Possible','Observed'),
        # levels = breedcat_order,
        # ordered = FALSE
        domain = spp_blocks$breedcat
        )

      print("adding blocks to map")
      leafletProxy("mysppmap") %>%
      clearShapes() %>%
      clearControls() %>%
      addRectangles(
        data = priority_block_data,
        layerId = ~ ID_BLOCK_CODE,
        lng1 = ~ NW_X,
        lat1 = ~ NW_Y,
        lng2 = ~ SE_X,
        lat2 = ~ SE_Y,
        weight= 0.5,
        color=ncba_blue,
        opacity = 0.25,
        fill = FALSE,
        label = priority_block_data$ID_NCBA_BLOCK
        ) %>%
      addRectangles(
        data = spp_blocks,
        layerId = ~ ID_NCBA_BLOCK,
        lng1 = ~ NW_X,
        lat1 = ~ NW_Y,
        lng2 = ~ SE_X,
        lat2 = ~ SE_Y,
        weight = 0.5,
        color = '#000000',
        # opacity = 0,
        fillColor= ~block_colors(breedcat),
        fillOpacity = 0.8,
        fill = TRUE,
        label = sprintf(
          "<strong><a href = '%s'>%s</a></strong><br/>%s",
          spp_blocks$blocklink,
          spp_blocks$ID_NCBA_BLOCK,
          spp_blocks$breedcat
          ) %>%
          lapply(htmltools::HTML)
        ) %>%
      addLegend(
        "bottomright",
        pal = block_colors,
        values = spp_blocks$breedcat,
        labFormat = labelFormat(
          transform = function(x) sort(x, decreasing = FALSE)
        ),
        title = "Breeding Category",
        opacity = 1
        ) 
        }
    }
  )


### DISPLAY BLOCKS

 # popup menu on hover over checklist


  ### DISPLAY CHECKLISTS ON THE MAP ------
  observeEvent(
    current_block_ebd_checklistsonly_filtered(),
    {
      req(current_block_ebd_checklistsonly_filtered())

      # check to make sure records returned!
      checklists <-
        current_block_ebd_checklistsonly_filtered()[
          c("LATITUDE","LONGITUDE", "SAMPLING_EVENT_IDENTIFIER", "LOCALITY_ID",
          "LOCALITY", "OBSERVATION_DATE", "ebird_link")]
      if (length(checklists) > 0){
        leafletProxy("mymap") %>%
        clearMarkerClusters() %>%
        # clearShapes() %>%
        addCircleMarkers(
          data = checklists,
          lat = ~ LATITUDE,
          lng = ~ LONGITUDE,
          radius = 5,
          clusterOptions = markerClusterOptions(
            maxClusterRadius = 10,
            spiderfyDistanceMultiplier = 2),
          color = ncba_blue,
          opacity = 1,
          stroke = TRUE,
          weight = 1.8,
          fillColor = ncba_white,
          fillOpacity = 0.6,
          label = sprintf(
            "<strong>%s</strong><br/>%s<br/>%s",
            checklists$SAMPLING_EVENT_IDENTIFIER,
            checklists$LOCALITY,
            checklists$OBSERVATION_DATE) %>%
          lapply(htmltools::HTML),
          popup = ~ebird_link)
      }
    }
  )



  # SPECIES TAB  ----------------------------------------------------

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
  ### PLOT BREEDING CODES ------------------------------------------------
  lump <- list(S = c("S", "S7", "M"), O = c("", "F", "O", "NC"))
  no_plot_codes <- NULL
  out_pdf <- NULL
  spp <- input$spp_select
  print(spp)

  #this pipeline pulls from the ebd_observations collection
  # slower, but needs less pre-compilation (like spp_summaries)
  pipeline  <- sprintf(
    '[{
      "$match": {"COMMON_NAME": "%s"}
      },
      {"$group":
          {"_id": {
              "bcode": "$BREEDING_CODE",
              "odate": "$OBSERVATION_DATE"
            },
            "cname": {
              "$first": "$COMMON_NAME"
            }
          }
      },
      {
        "$project": {
          "_id": "$cname",
          "COMMON_NAME": "$cname",
          "OBSERVATION_DATE": "$_id.odate",
          "BREEDING_CODE": "$_id.bcode"
        }
      }
    ]' ,
      spp)

  # print(pipeline)
  ebird <- aggregate_spp_data(pipeline)

  print("aggregate pipeline completed.")
  # print(ebird)
  ##### END EXPERIMENT

  print("ebd records retrieved, plotting data")
  # grid::current.viewport()
  breeding_boxplot(
    spp,
    ebird,
    pallet="Paired",
    out_pdf=NULL,
    no_plot_codes=no_plot_codes,
    lump=lump,
    drop=TRUE)
})

#
# ## SUMMARIZE START TIMES --------------------------------------------------
# plot(start_time_boxplot(ebird))

# ## PLOT COORDINATES OF RECORDS --------------------------------------------
# coords.plot <- plot_checklists_coords(ebird)
# plot(coords.plot)
#
# ## SUMMARIZE TRAVEL DISTANCE ----------------------------------------------
# plot(effort_distance_boxplot(ebird))
#
# ## SUMMARIZE MINUTES EFFORT -----------------------------------------------
# plot(duration_minutes_boxplot(ebird))
#
# ## LOCALITY TYPE BREAKDOWN ------------------------------------------------
# plot(locality_type_pie(ebird))



}

shinyApp(ui, server)
