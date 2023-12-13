# NC Bird Atlas Explorer Tool
Shiny App implementation to display data from the North Carolina Bird Atlas.


## Utilities (utils.r)
Stores functions to be used elsewhere

### MongoDB
The bulk of the NCBA data is stored in a MongoDB Atlas implementation.

Functions:
- get_ebd_data(query, filter)
- get_spp_data()

## Blocks
Diplays block level information.

## Species
Displays species level information.

## ShinyApp Reactive Data Request Structure
This is partial - needs more work...

### criteria_changes - NOT USED?

- NOT USED?
- listens for changes in the checklist filters: portal_records and season_radio
- trigger: input$portal_records, input$season_radio

### current_block_r

- returns: rv_blocks#id - current block id (triggered when map clicked)

### current_block_ebd

- requires: current_block_r

### checklist_count

- listens for changes in the checklist filters: portal_records and season_radio
- trigger: unique_sei (unique sampling event identifier
- returns: number of records

### checklist_filtered_count

- trigger: unique_sei (unique sampling event identifier)
- sets: unique_sei (unique sampling event identifier)
- returns: number of records

## Other resources

Several resources proved invaluable when building this app, including:
- [NC Bird Atlas](https://ncbirdatlas.org) eBird portal;
- [NCBA Dashboard](https://dashboard.ncbirdatlas.org) MongoDB dashboard;

# Git workflow

Development of the shiny app (in the shinyapp folder), and the "development" folder requires us to keep better branch organization. RMarkdown development will be in the "species-stats" branch, while shiny app development will be in the "shiny-dev" branch. When a change is made, the branch should be merged with the "develop" branch. Once both branches are up-to-date and functioning, the "develop" branch can be merged with the master.

master -- develop -- species-stats
                  -- shiny-dev

"species-stats": Development of Rmarkdown scripts and updating the ncba_functions. These edits should be merged with the develop branch. Workflow:

    git pull origin develop
    git checkout shiny-dev
    git merge develop
    # do editing
    git add .
    git commit -m "making changes to species-stats"
    git checkout develop
    git merge species-stats


"shiny-dev": Development of the shiny app and incorporating species-stats changes. Workflow:
 
    git pull origin develop
    git checkout shiny-dev
    git merge develop
    # do editing
    git add .
    git commit -m "making changes to shiny-dev"
    git checkout develop
    git merge shiny-dev

## Authors
Scott Anderson, Bird Conservation Biologist, NC Wildlife Resources Commission

Nathan Tarr, NCSU/USGS

Dr. Scott Pearson

Elsa Chen

## Contact
scott.anderson@ncwildlife.org
