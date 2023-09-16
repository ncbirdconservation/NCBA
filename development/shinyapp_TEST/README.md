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



## Authors
Scott Anderson, Bird Conservation Biologist, NC Wildlife Resources Commission

Nathan Tarr, NCSU/USGS

Dr. Scott Pearson

Elsa Chen

## Contact
scott.anderson@ncwildlife.org
