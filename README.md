# NC Bird Atlas

Contributions by Nathan Tarr, Scott Anderson, Scott Pearson, John Carpenter, and Elsa Chen.

[Project Page](https://github.com/ncbirdconservation/NCBA/projects/1)

## Purpose
This repository houses code to support the NC Bird Atlas.  It contains functions for accessing and summarizing NCBA or EBD data and template scripts (.R or .Rmd files) that demonstrate their usage.  Code that runs the NCBA Data Explorer is also stored in the repository.

## Status
Development

## Updates
- 9/21/23 moved repository to ncbirdconservation

## NCBA Functions 
| __Function__ | __Description__ |
| ------------ | --------------- |
| blocks_needed | Returns a data frame of blocks with geometries where the species was predicted to occur but has not been observed. |
| blocks_observed_in | Returns a data frame of blocks where the species was observed. |
| block_predicted_spp | Returns predicted summer and winter species lists. |
| block_spp_lists | Generates a list of species lists by breeding category for a specified block. |
| breeding_boxplot | Produces a boxplot of breeding codes over calendar day. |
| breeding_codes | Returns either a full or nested list of all breeding codes. |
| breeding_map | Generates an interactive map of records with hyperlinks to checklist webpages. |
| calculate_breeding_dates | Calculates start and end day of year for breeding in North Carolina. |
| complete_checklist_table | Returns a summary table of completeness (all species counted). |
| connect_ncba_db | Connects to the NCBA MongoDB database (AtlasCache). |
| counties_NC | Reads in a counties data frame with spatial attributes. |
| duration_distance_table | Returns a summary table of checklist duration and distance. |
| duration_minutes_boxplot | Describe the distribution of effort_minutes values as a box plot. |
| EBD_fields | Returns a list of fields present in an EBD download. |
| effort_distance_boxplot | Describes the distribution of effort_distance_km values as a boxplot. |
| get_blocks | Returns a data frame of blocks with or without geometries. |
| get_breeding_dates | Gets the start and end day of year for breeding from the Atlas Cache.  Species-specific. |
| get_breeding_records | Gets records with breeding and behavior codes of interest. |
| get_checklists | Get a data frame of checklists from the AtlasCache or an eBird data download. |
| get_observations | Returns a data frame of species observations. |
| get_predicted_presence | Returns a data frame of blocks where the species was predicted to occur. |
| highest_category | Returns a data frame of the highest breeding code reported in each block with geometries. |
| locality_type_pie | Describe the locality types present as a pie chart: whether checklists are for hotspots, personal locations, etc. |
| map_needed_highest | Returns a ggplot2 map of where breeding observations are needed and the highest reported breeding category. |
| map_records | Draws an interactive map of record localities with uncertainty buffers and hyperlinks to eBird webpage. |
| nonEBD_fields | Returns a list of NCBA only fields that are in the Atlas Cache EBD collection. |
| observer_complete_by_priority | Returns a data frame summarizing an observers effort by checklist completeness and block type.  Reported values can be number of checklists or number of blocks. |
| observer_priority_by_breeding | Returns a table with species or block tallies by block type and breeding  category. |
| protocol_table | Returns a summary table of protocol type values from a data frame. |
| protocol_type_pie | Describe the protocol types present as a pie chart: whether checklists are traveling, stationary, etc. |
| records_as_sf | Create new simple features collection (spatial data frame) of records (checklists or observations).  Output can be plotted or used as input for other functions. |
| spp_count_summary | Returns a table summarizing species count by breeding category.|
| start_time_boxplot | Describe the distribution of checklist start times as a box plot. |
| summarize_duration | Returns a data frame that summarizes hours of effort within a block. |
| to_EBD_format | Reformat columns to match that of the EBD. |
| year_bar | Summarize how many checklists were reported each year. |

## Organization
Template scripts and other documents should remain organized into the following folders:

__archive__ -- outdated and irrelevant code that could be a useful reference for something in the future.

__block_assessments__ -- descriptive analyses of individual blocks or groups of blocks, including priority blocks.

__data_review__ -- QAQC code.

__development__ -- scripts for testing and developing functions and other code.

__effort_summaries__ -- summaries of eBird sampling data (eBird checklists) and NCBA data at the survey level.

__help__ -- documents and code for reference and learning.

__resources__ -- contains an R script with the core functions, demos, and reports.

__shinyapp__ -- code that supports the online data explorer (https://ncbirdconservation.shinyapps.io/shinyapp/).

__single-use_scripts__ -- scripts that complete one-time tasks, such as downloading or processing something.

__species_summaries__ -- summaries of species data; individual species or species groups.
 
## Installation
Use of the code requires a config file and access to the Atlas Cache requires special permission.  Contact Scott Anderson with inquiries.

## Usage
To access functions, set your working directory to "../NCBA/resources" (the path to your copy of the repo) and then run 'source("ncba_functions.R")'.

To use a (template) script, copy it to a working directory (project folder) on your machine, edit paths and variables where necessary to reflect your computing environment, and run in RStudio.

## Contributing
See CONTRIBUTING.md file for instructions and guidelines for making contributions and please be sure to follow the CODE_OF_CONDUCT.md.