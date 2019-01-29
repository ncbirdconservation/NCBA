# Demonstration of a GAP Range Map Evaluation Framework
This notebook details a framework for evaluating GAP range maps with occurrence data retrieved from databases such as GBIF via APIs.  It details the major steps and features of the framework for a single species: the yellow-billed cuckoo.  The primary results are some maps for visualization, columns added to a GAP range data .csv file that is downloaded from ScienceBase, and documentation of decisions and archiving of data used.  

See the README.md file in this repository for more information.

There are a few major steps.  

## 1. Range Evaluation Parameter Database
This process requires some decision making about how to filter the records and other things.  Such decisions are documented in a database that can be queried in the other steps and referred to later for reference.

## 2. Species Concepts
Taxonomic classifications are periodically revised,  creating the potential for disagreement among organizations/efforts/databases about what a species name actually refers to.  That disagreement needs to be assessed and resolved before retrievals occurrence records can be fully trusted.

## 3. Retrieve Species Occurrence Records
Occurrence records can be accessed through API's, filtered, and saved in a database.  Filtering can happen during the request for records or after they have been received.  The precision of occurrence records varies due to coordinate uncertainty and detection distances, so points must be represented as circles (i.e., buffered)

## 4. Evaluate GAP Known Range Data
GAP ranges exist in table form in a database and on ScienceBase.  Ranges can be compared to occurrence circles to find HUCs where GAP was correct about species' presence and where it was wrong about absence.  The results of those comparisons can be saved in columns in the range tables.
----------------------------------------

## 1. Range Evaluation Parameter Database


## 2. Species Concepts


## 3. Retrieve Species Occurrence Records


## 4. Evaluate GAP Known Range Data
