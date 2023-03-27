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
if(!require(leaflegend)) install.packages(
    "leaflegend", repos = "http://cran.us.r-project.org")
if(!require(htmltools)) install.packages(
  "htmltools", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages(
  "shinythemes", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages(
  "shinydashboard", repos = "http://cran.us.r-project.org")


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
    header = fluidRow(tags$h5(tags$p(
paste0("This site is a work in progress, designed to provide access",
                                     " to data collected through the North Carolina Bird Atlas. ",
                                     "Data submitted to eBird is updated on a monthly basis."))),
                      align = "center"),
    tags$head(includeCSS("styles.css")),
    tags$head(tags$link(rel="icon", href="/input_data/ncba_blue_wbnu.ico")),
    tags$head(tags$script(HTML('
        var fakeClick = function(tabName) {
          var dropdownList = document.getElementsByTagName("a");
          for (var i = 0; i < dropdownList.length; i++) {
            var link = dropdownList[i];
            if(link.getAttribute("data-value") == tabName) {
              link.click();
            };
          }
        };
      '))),
    tabPanel("Blocks",
             id = "BlocksTab", value = "BlocksTab",
      div(class="col-md-2 panel sidebar", id = "blockcontrols",
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
              uiOutput("APBlock"),
            # selectInput(inputId = "APBlock", #name of input
            # label = "Selected Priority Block", #label displayed in ui
            # choices = c(
            #   as.character(unique(priority_block_data$ID_NCBA_BLOCK)),
            #   "NONE"),
            # # calls unique values from the ID_NCBA_BLOCK column
            # # in the previously created table
            # selected = "NONE"),
            htmlOutput("checklist_counter")
          ),
          div(class="tab-control-group",
           prettySwitch("portal_records","Portal Records Only", FALSE ),
            radioButtons("season_radio",label = h4("Season"),
              choices = list(
                "All Records" = "All",
                "Breeding" = "Breeding",
                "Non-Breeding" = "Non-Breeding",
                "Custom" = "Custom"),
              selected = "All"),
           conditionalPanel(condition = "input.season_radio == 'Custom'",
                            selectInput("month_range", "Selected Months", choices = c(1,2,3,4,5,6,7,8,9,10,11,12),
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
        div(class="col-md-10",
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
    # tabPanel("About",
    #   tags$div(
    #     tags$h4("NC Bird Atlas Data Explorer"),
    #     tags$p(
    #       paste0("This site is a work in progress, designed to provide access",
    #       " to data collected through the North Carolina Bird Atlas. ",
    #       "Data submitted to eBird is updated on a monthly basis.")),
    #     tags$br(),
    #     tags$img(
    #       src = "ncba_logo_blue_halo_final.png",
    #       width = "150px",
    #       height = "75px")
    # 
    #     )
    # ),
    tabPanel("Effort Map",
             # actionButton("link_to_BlocksTab", "Go  to Blocks Tab"),
             div(leafletOutput("Effortmap",height="70vh")),
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

  ### Object for drop down list for block selection
  output$APBlock <- renderUI({
    selectInput(inputId = "APBlock", #name of input
                label = "Selected Priority Block", #label displayed in ui
                choices = c(
                  as.character(unique(priority_block_data$ID_NCBA_BLOCK)),
                  "NONE"),
                # calls unique values from the ID_NCBA_BLOCK column
                # in the previously created table
                selected = "NONE")
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
    confirmed_total <- nrow(filter(sa_list, bcat == "C4" ))
    if ((spp_total*0.5)<confirmed_total) {
      confirmed_class = "success"
    } else {
      confirmed_class = "failed"
    }

    # add conditional formatting if criteria met
    num_spp_total <- paste("Species: ", nrow(sa_list["spp"]) )
    num_breed_confirm <- paste(
      "Confirmed (C4):<span class='", confirmed_class, "'>",
      confirmed_total, "</span>")
    num_breed_prob <- paste(
      "Probable (C3):", nrow(filter(sa_list, bcat == "C3" )))
    num_breed_poss <- paste(
      "Possible (C2):", nrow(filter(sa_list, bcat == "C2" )))

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
        # addMarkers(data=checklists,
        # layerId = paste("checklist",~ SAMPLING_EVENT_IDENTIFIER),
        #   lat = ~ LATITUDE,
        #   lng = ~ LONGITUDE,
        #   color=ncba_blue
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

  ##### SPP DATA RETRIEVAL WORKS, BUT IS SLOW
  # query <- str_interp('{"OBSERVATIONS.COMMON_NAME":"${spp}"}')
  # filter <- str_interp(c('{"OBSERVATION_DATE":1,',
  # ' "OBSERVATIONS.BREEDING_CODE":1,',
  # '"OBSERVATIONS.COMMON_NAME":1}'))
  # print("get ebd records")
  # ebird <- get_spp_obs(spp, filter)
  # print(ebird)
  # print("get ebd records completed")
  ##### END WORKING SPP DATA RETRIEVAL

  ##### EXPERIMENT TO RETRIEVE VIA AGGREGATION STRING
  #faster, but still slow
  pipeline <- 
    str_interp(c('[{ "$match": { "OBSERVATIONS.COMMON_NAME": "${spp}" } },',
    ' { "$unwind": { "path": "$OBSERVATIONS" } }, ',
    '{ "$project": { "COMMON_NAME": "$OBSERVATIONS.COMMON_NAME", ',
    '"BREEDING_CODE": "$OBSERVATIONS.BREEDING_CODE", ',
    '"OBSERVATION_DATE": 1 } }, { "$match" : { "COMMON_NAME": "${spp}"}}, ',
    '{ "$lookup": { "from": "safe_dates", "localField": "COMMON_NAME", ',
    '"foreignField": "COMMON_NAME", "as": "SPP_SAFE_DATES" } }, ',
    '{ "$unwind": { "path": "$SPP_SAFE_DATES" } }, ',
    '{ "$project": { "COMMON_NAME": 1, "BREEDING_CODE": 1, ',
      '"OBSERVATION_DATE": 1, "SEASON": { "$cond": { "if": ',
      '{ "$and": [ { "$gte": [ "$OBSERVATION_DATE", ',
      '"$SPP_SAFE_DATES.B_SAFE_START_DATE" ] }, ',
      '{ "$lte": [ "$OBSERVATION_DATE", ',
      '"$SPP_SAFE_DATES.B_SAFE_END_DATE" ] } ] }, ',
      '"then": "Breeding", "else": "Non-Breeding" } } } }]'))
  print(pipeline)

  ebird <- aggregate_ebd_data(pipeline)
  print("aggregate pipeline completed.")
  print(ebird)
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

#### EFFORT MAP TAB  ----------------------------------------------------
  ### SETUP LEAFLET MAP, RENDER BASEMAP 
  output$Effortmap <- renderLeaflet({
    
    #setup block geojson layer
    print("starting effort map, adding blocks")
    
    leaflet() %>%
      setView(
        lng = nc_center_lng,
        lat = nc_center_lat,
        zoom = nc_center_zoom) %>%
      
      ### Base Groups (Satellite Imagery and Blocks )
      addProviderTiles("CartoDB.DarkMatterNoLabels",
                       options = providerTileOptions(opacity = 1)) %>%
      
      addRectangles(
        data = pb_map,
        layerId = ~ ID_NCBA_BLOCK,
        lng1 = ~ NW_X,
        lat1 = ~ NW_Y,
        lng2 = ~ SE_X,
        lat2 = ~ SE_Y,
        stroke = TRUE,
        weight = 2.5,
        color=~breedingpal(portal_breeding_hrsDiurnal),
        # fillColor= ~binpal(breeding_hrsDiurnal),
        # fillOpacity = 0.8,
        fill = FALSE,
        label = ~paste ("<strong>", ID_NCBA_BLOCK, "</strong><br>Breeding Diurnal Hours:", signif(portal_breeding_hrsDiurnal,2)) %>% lapply(htmltools::HTML),
        group = "Breeding Diurnal Hours") %>% 
      
      ### Overlay Groups 
      ## Nocturnal Hours - Circle Outlines
      addCircles(data = pb_map,
                 lng = ~ SE_X, lat = ~ centr_y,
                 radius = 400,
                 color = ~binpalnight(portal_breeding_hrsNocturnal),
                 stroke = TRUE,
                 weight = 2,
                 fill = FALSE,
                 label = ~paste ("<strong>", ID_NCBA_BLOCK, "</strong><br>Nocturnal Breeding Hours:", signif(portal_breeding_hrsNocturnal,2)) %>% lapply(htmltools::HTML),
                 group = "Breeding Nocturnal Hours") %>% 
      ## Number of Confirmed Species, Custom Crow Icon from Font Awesome
      addMarkers(data = pb_map,
                 lng = ~centr_x, lat = ~centr_y,
                 icon = ~confirm_icons[confirm_colors],
                 label = ~paste ("<strong>",ID_NCBA_BLOCK, "<br>% Confirmed:", PctConfirm,
                                 "</strong><br> # Confirmed:", portal_breeding_sppCountConfirmed,
                                 "<br> # Probable:", portal_breeding_sppCountProbable,
                                 "<br> # Possible:", portal_breeding_sppCountPossible) %>% lapply(htmltools::HTML),
                 group = "% Species Confirmed") %>% 
      
      ## Wintering Diurnal Hours - Filled Circles
      addCircles(data = pb_map,
        lng = ~ centr_x,
        lat = ~ NW_Y,
        radius = 600,
        stroke = FALSE,
        fill = TRUE,
        fillOpacity = 1,
        fillColor= ~winterbinpal(portal_wintering_hrsDiurnal),
        label = ~paste ("<strong>", ID_NCBA_BLOCK, "</strong><br>Wintering Diurnal Hours:", signif(portal_wintering_hrsDiurnal,2)) %>% lapply(htmltools::HTML),
        group = "Wintering Diurnal Hours") %>% 
      
      # Layers Control (Making Icons as Overlay Layers)
      addLayersControl(data = pb_map,
        baseGroups = c("Breeding Diurnal Hours"),
        overlayGroups = c("Wintering Diurnal Hours", "% Species Confirmed", "Breeding Nocturnal Hours"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      # Hide Nocturnal Hours and Confirmed Species Icons upon Map Start
      hideGroup(c("Wintering Diurnal Hours", "% Species Confirmed", "Breeding Nocturnal Hours")) %>% 
      
      # Legends for Diurnal Hours, Nocturnal Hours, and Confirmed Species
      addLegendImage(images = diurnal_symbols, labels = c('0', '≤5', '5-10', '10-15', '15-20', '≥20'), 
      orientation = 'vertical', title = 'Breeding Diurnal Hours', position = 'topleft', width = 16.44, height = 16.44,
      labelStyle = "font-size: 14px; vertical-align: center;") %>% 
      addLegendImage(images = winter_diurnal_symbols, labels = c('0', '≤2.5', '2.5-5.0', '5.0-7.5', '7.5-10', '≥10'), 
                     orientation = 'vertical', title = 'Wintering Diurnal Hours', position = 'topleft', width = 16.44, height = 16.44,
                     labelStyle = "font-size: 14px; vertical-align: center;") %>% 
      addLegendImage(images = breeding_nocturnal_symbols, labels = c('0', '≤0.5', '0.5-1.0', '1.0-1.5', '1.5-2.0', '≥2.0'), 
                     orientation = 'vertical', title = 'Breeding Nocturnal Hours', position = 'topright', width = 16.44, height = 16.44,
                     labelStyle = "font-size: 14px; vertical-align: center;") %>% 
      addLegendImage(images = c("input_data/crow_red.png","input_data/crow_yellow.png", "input_data/crow_green.png", "input_data/crow_teal.png",
                                "input_data/crow_blue.png", "input_data/crow_purple.png"),
                     labels = c("1-5","5-10","10-15","15-20", "20-25",">25"),
                     labelStyle = "font-size: 14px; vertical-align: center;",
                     title = '% Species Confirmed',
                     orientation = "vertical",
                     width = 13.32,
                     height = 16.44,
                     position = "bottomright",
                     group = "Confirmed Species Legend")
    }) 
  
  ### Merging Block Shapes for mapping and Summary Table
  pb_map <- merge(priority_block_data, blocksum, all = TRUE)
  
  ## Convert percentages to numbers instead of decimals
  pb_map <- pb_map %>% 
    mutate(PctConfirm = signif(portal_breeding_sppPctConfirmed*100,4))

  ### Centroids of Priority Blocks
  centr_x <- (pb_map$NW_X + pb_map$SE_X)/2
  centr_y <- (pb_map$NW_Y + pb_map$SE_Y)/2
  mutate(pb_map, centr_x)
  mutate(pb_map, centr_y)
  
  ### Custom Crow icon is from Font Awesome (https://fontawesome.com/icons)
  ### "fa-solid fa-crow"

  ## Make a list of icons. We'll index into it based on name.
  confirm_icons <- iconList(
    crow_grey = makeIcon(iconUrl = "input_data/crow_grey.svg",
                       iconWidth = 4.16, iconHeight = 3.33, iconAnchorX = 6, iconAnchorY = 1),
    crow_red = makeIcon(iconUrl = "input_data/crow_red.svg",
                        iconWidth = 8.33, iconHeight = 6.66, iconAnchorX = 6, iconAnchorY = 1),
    crow_yellow = makeIcon(iconUrl = "input_data/crow_yellow.svg",
                           iconWidth = 8.33, iconHeight = 6.66, iconAnchorX = 6, iconAnchorY = 1),
    crow_green = makeIcon(iconUrl = "input_data/crow_green.svg",
                          iconWidth = 8.33, iconHeight = 6.66, iconAnchorX = 6, iconAnchorY = 1),
    crow_teal = makeIcon(iconUrl = "input_data/crow_teal.svg",
                         iconWidth = 8.33, iconHeight = 6.66, iconAnchorX = 6, iconAnchorY = 1),
    crow_blue = makeIcon(iconUrl = "input_data/crow_blue.svg",
                         iconWidth = 8.33, iconHeight = 6.66, iconAnchorX = 6, iconAnchorY = 1),
    crow_purple = makeIcon(iconUrl = "input_data/crow_purple.svg",
                           iconWidth = 8.33, iconHeight = 6.66, iconAnchorX = 6, iconAnchorY = 1))
  
  # add "confirm_color" column as a variable : this will be associated to the icons' list
  pb_map <- pb_map %>%
    mutate(confirm_colors = case_when(
      PctConfirm == 0 ~ "crow_grey",
      PctConfirm <= 5 ~ "crow_red",
      PctConfirm <= 10 ~ "crow_yellow",
      PctConfirm <= 15 ~ "crow_green",
      PctConfirm <= 20 ~ "crow_teal",
      PctConfirm <= 25 ~ "crow_blue",
      PctConfirm > 25 ~ "crow_purple"))
  
  ### palette for Diurnal Hours
  ### Grey = #808080FF, Yellow = #FDE725FF, Green = #5DC863FF, Teal = #21908DFF,  Blue = #3B528BFF, Purple = #440154FF
           
  breedingpal <- colorBin("viridis", pb_map$portal_breeding_hrsDiurnal, bins = c(0.1,5,10,15,20,Inf), reverse = TRUE)
  breedingpalnum <- colorNumeric("viridis", pb_map$portal_breeding_hrsDiurnal, reverse = TRUE)

  
  winterbinpal <- colorBin("viridis", pb_map$portal_wintering_hrsDiurnal, bins = c(0.1,2.5,5,7.5,10,Inf), reverse = TRUE)
  
  ### palette for Nocturnal Hours
  binpalnight <- colorBin("viridis", pb_map$portal_breeding_hrsNocturnal, bins = c(0.1,0.5,1,1.5,2,Inf), reverse = TRUE)
  
  
  ### legend components for breeding diurnal hours
  diurnal_shapes <- c('rect','rect','rect','rect','rect','rect')
  diurnal_colors <- c("#808080FF", "#FDE725FF", "#5DC863FF", "#21908DFF", "#3B528BFF", " #440154FF")
  diurnal_symbols <- Map(f = makeSymbol, shape = diurnal_shapes, fillColor = diurnal_colors, 
                                        color = diurnal_colors, opacity = 1, fillOpacity = 0, width = 20,
                                        `stroke-width` = 2)
  
  ### legend for winter diurnal hours
  winter_shapes <- c('circle','circle','circle','circle','circle','circle')
  winter_diurnal_symbols <- Map(f = makeSymbol, shape = winter_shapes, fillColor = diurnal_colors, 
                         color = diurnal_colors, opacity = 1, fillOpacity = 1, width = 20,
                         `stroke-width` = 2)
  
  ### legend for breeding nocturnal symbols
  winter_shapes <- c('circle','circle','circle','circle','circle','circle')
  breeding_nocturnal_symbols <- Map(f = makeSymbol, shape = winter_shapes, fillColor = diurnal_colors, 
                                color = diurnal_colors, opacity = 1, fillOpacity = 0, width = 20,
                                `stroke-width` = 2)
  
 

  ### Change to Block Tab when Clicking a Block in Effort Map
  ### 

}  
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





shinyApp(ui, server)
