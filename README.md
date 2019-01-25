# GAP Range Evaluation With Occurrence Records

## Purpose
The Gap Analysis Project developed range maps for 1,590 terrestrial vertebrate species by attributing a 12 digit hydrologic unit code vector data layer with each species' status regarding known presence, seasonal presence, use for reproduction, and origin (i.e., is it native?).  

The abundance of wildlife occurrence datasets that are currently accessible represent an opportunity to validate the GAP range data.  However, this issue task is more complex than it may seem at first consideration given errors and uncertainties in occurrence data and the absence of absence records.  This repository is a draft framework for partially validating GAP range maps with occurrence data that is widely available through API's.  

Many projects have used these occurrence data for species distribution modeling.  This effort is different because it treats the data more conservatively and aims to create simpler summaries of data with well-documented filters.

## Criteria
This framework is designed to meet several important criteria.
* Automation -- the volume of data and species involved necessitates the processes be automated. Automation also reduces subjectivity in decision making, enables thorough documentation, and ensures repeatability.

* Transparency through documentation -- summaries and models using  empirical data still involve subjectivity in the form of choices regarding parameters and rules for handling/filtering/cleaning data.  This framework is meant to document those choices.  It identifies several filter criteria that are especially relevant to species-occurrence records.
  * Occurrence year -- species' distributions and taxonomy can change over time.
  * Occurence month -- especially relevant regarding migratory species.
  * Coordinate uncertainty and issues -- this varies immensely among records and some records have known issues that limit their value.
  * Detection distance -- different species may be sampled differently in ways that introduce location uncertainty.  Small mammals that are trapped can confidently be assigned to the trap's location, but loud-singing birds detected in an auditory survey could be hundreds of meters away from the observer and thus, the coordinate of the record.  
  * Misidentifications -- even professionals are not perfect, so citizen scientists surely mistakenly identify species.  Presence-only data do not directly record absence, so false-positives are the issue here.  False-positives have the ability to expand and distort range delineations and falsely validate GAP range maps.  A simple way to account for them is to employ a threshold number of records for a region (i.e., a HUC) before the species is determined to be present there.  I use the term 'pad' for this threshold here.
  * Occurrence records are polygons -- although records are recorded as x,y coordinates, the coordinate uncertainty and detection distance issues described above require that they be treated as circles with centers at the x,y coordinate and radius equal to the detection distance plus the coordinate uncertainty.  Buffering points accounts for this but creates an issue regarding the overlap of occurrence circles with huc boundaries; when a circle is not completely contained by a huc, which huc should it be attributed to?  The answer is that the analyst has to decide and that decision is a matter of probability.  There is a 100% chance that the species occurred somewhere within the circle, so the probability it was in a particular huc equals the proportion of the circle that falls in that huc.  Therefore, setting an a priori tolerance for error is required.  If you set your tolerance to 10%, then the occurrence record will only be attributed to a huc if over 90% of the circle occurs within it.  
  * Occurrence data sets are dynamic -- some datasets enable data contributors to go back and edit attributes of occurrence records.  In addition, historic records may be added that change the set of records associated with a past time period.  That is to say that a query of years past run today may be different than the same query run tomorrow.  This represents a challenge for provenance.  The method I employed to handle this is to document data request parameters, post-request filter sets, and range map evaluation criteria as uniquely identifiable objects that are stored and documented in a database ('rng_eval_params.sqlite').  Records that pass through filters are also stored with geometry in a separate sqlite database.
  * Species concepts are dynamic -- taxonomic classifications are constantly being scrutinized and are revised annually.  As a result, different projects may not have used different concepts for the same species name.  In many cases, this is not a big problem because the species is unique and easily identifiable and taxonomic changes regard names only.  More problematic are cases where genetic studies have identified species that are nearly identical physiologically and were once considered a single species.  Such cases often reveal geographic patterns in species occurrence that may be used by some as a basis for identification of individuals in the field (e.g., a bird watcher decides an individual's identity in part based on which species is supposed to occur in the area or a species' range is revised and eBird changes records of the old species concept from the area where it is now known not to occur to the correct species).  Dynamic species-concepts are a daunting challenge, but can hopefully be handled with scrutinizing taxonomic crosswalks.  In this framework, species-concepts are documented in the rng_eval_params.sqlite database with columns for the years the concept was valid and a geometry column where a polygon of potential occurrence could be recorded.  This whole topic needs more work though.

* High confidence -- criteria and filters can be set in this framework that produce high confidence in results, whether it's evaluation of the GAP range maps or summaries of records.

* Open source -- processes are coded in Python 3 and use sqlite3, which comes with Python 3, for spatial queries.

## Inputs
Data is gathered from catalogs and databases through API's so there are few inputs.  However, the rng_eval_params.sqlite database is needed, which includes tables for species-concepts, data request parameters, post-request filtering criteria, and range evaluation criteria.  Additionally, the GAP 12 digit HUC ancillary layer is needed, and although it is available on ScienceBase.gov as a geodatabase, a different format is needed.  Hopefully a shapefile version can be uploaded there eventually.

GBIF is currently the only dataset currently used but others can/will be added later including eBird.

## Outputs
On a per-species basis
* A database of filtered species-occurrence records.
* A database of GAP range evaluation information from which an updated range evaluation shapefile can be created.
* A csv file of updated range data with columns for evaluation and validation information as well as rows for HUCs with occurrences that were omitted by GAP.
* A map of occurrence polygons.
* Seasonal maps of occurrence polygons for migratory species.

## Constraints
None at this time

## Dependencies
Python 3 and numerous packages including sqlite3 with the spatialite extension.  An environment can be created from the .yml file included in this repository.

## Code
All code is included in this repository.  
