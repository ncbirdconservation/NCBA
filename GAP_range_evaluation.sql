/*
Description: This code uses occurrence data collected with
'occurrence_records_summaries.py' to evaluate the GAP range map for a species.
A table is created for the GAP range and columns reporting the results of
evaluation and validation are populated after evaluating spatial relationships
of occurrence records (circles) and GAP range.

The results of this code are new columns in the GAP range table (in the db
created for work in this repository) and a range shapefile.

The primary use of code like this would be range evaluation and revision.

Unresolved issues:
1. How can overlap be handled?  As is, occurrence circles overlaping huc
   boundaries are omitted.  Set a tolerable level of spatial error?
2. Can the final shapefile be dissolved?
   Code to try:
   """Select f.field1 as field1, st_unaryunion(st_collect(f.geometry)) as geometry
   From tableA as f
   Group by field1;"""
3. Can the runtime be improved with spatial indexing?  Minimum bounding rectangle?
4. ".import" has to be worked around when this goes into python.
5. Locations of huc and GAP range files.
6. Documenting evaluation parameters (spatial error allowed, spatial relationship
   rules.)
*/

SELECT load_extension('mod_spatialite');
/*#############################################################################
                                 Load Tables
 ############################################################################*/
/* Add the hucs shapefile to the db. */
SELECT ImportSHP('/users/nmtarr/data/SHUCS', 'shucs', 'utf-8', 102008, 'geom_102008',
                 'HUC12RNG', 'POLYGON');

/* Load the GAP range csv, filter out some columns, rename others */
.mode csv
.import /users/nmtarr/Documents/ranges/indata/bYBCUx_CONUS_Range_2001v1.csv sp_range
.headers on
ALTER TABLE sp_range RENAME TO garb;
CREATE TABLE sp_range AS
                      SELECT strHUC12RNG,
                             intGapOrigin AS intGAPOrigin,
                             intGapPres AS intGAPPresence,
                             intGapRepro AS intGAPReproduction,
                             intGapSeas AS intGAPSeason,
                             Origin AS strGAPOrigin,
                             Presence AS strGAPPresence,
                             Reproduction AS strGAPReproduction,
                             Season AS strGAPSeason
                      FROM garb;
DROP TABLE garb;


/*#############################################################################
                             Assess Agreement
 ############################################################################*/
/*
Add a column to store gbif evaluation result - meaning, does the huc
completely contain an occurrence circle?  Circles that overlap huc boundaries
are left out of this, but could possibly be added later
*/

/*############################################  Does HUC contain an occurrence?
#############################################################################*/
ALTER TABLE sp_range ADD COLUMN eval_gbif1 INTEGER;

/*  Make table of hucs that contain an occurrence circle */
CREATE TABLE blue AS
              SELECT shucs.HUC12RNG, shucs.geom_102008,
              FROM shucs, occs
              WHERE Contains(shucs.geom_102008, occs.circle_albers);

/*  Record in sp_range that gap and gbif agreed on species presence */
UPDATE sp_range
SET eval_gbif1 = 1
WHERE EXISTS (SELECT HUC12RNG FROM blue WHERE HUC12RNG = sp_range.strHUC12RNG);

/*  Find hucs that contained gbif occurrences, but were not in gaprange and
insert them into sp_range as new records */
INSERT INTO sp_range (strHUC12RNG, eval_gbif1)
SELECT blue.HUC12RNG, 0
FROM blue LEFT JOIN sp_range ON sp_range.strHUC12RNG = blue.HUC12RNG
WHERE sp_range.strHUC12RNG IS NULL;

/*  For new records, put zeros in GAP range attribute fields  */
UPDATE sp_range
SET intGAPOrigin = 0,
    intGAPPresence = 0,
    intGAPReproduction = 0,
    intGAPSeason = 0
WHERE eval_gbif1 = 0;

/*  Populated a validation column.  If an evaluation supports the GAP ranges
then it is validated */
ALTER TABLE sp_range ADD COLUMN validated_presence INTEGER NOT NULL DEFAULT 0;

UPDATE sp_range
SET validated_presence = 1
WHERE eval_gbif1 = 1;


/*######################################  How many occurrences inside each huc?
#############################################################################*/
/*  How many occurrences in each huc that had an occurrence? */
/*
SELECT g.plgnID AS "plgn_ID",
   AVG(s.pollution) AS "Average Pollution",
   MAX(s.pollution) AS "Maximum Pollution",
   COUNT(*) AS "Number of Sensors"
FROM sensors AS s JOIN SHAPE1 AS g
ON contains(g.geometry, s.geometry)
GROUP BY g.plgnID*/
ALTER TABLE sp_range ADD COLUMN eval_gbif1_cnt INTEGER DEFAULT 0;

UPDATE sp_range
SET eval_gbif1_cnt = (SELECT COUNT(geom_102008)
                      FROM blue
                      WHERE HUC12RNG = sp_range.strHUC12RNG
                      GROUP BY HUC12RNG);


/*#############################################################################
                               Export Table and Map
 ############################################################################*/
/*  Create a version of sp_range with geometry  */
CREATE TABLE sp_geom AS
              SELECT sp_range.*, shucs.geom_102008
              FROM sp_range LEFT JOIN shucs ON sp_range.strHUC12RNG = shucs.HUC12RNG;
SELECT RecoverGeometryColumn('sp_geom', 'geom_102008', 102008, 'POLYGON');

/* Export maps */
SELECT ExportSHP('sp_geom', 'geom_102008',
                 '/users/nmtarr/documents/ranges/bYBCUx_CONUS_Range_2001v1_eval', 'utf-8');

/* Export csv */
.output /users/nmtarr/documents/ranges/bYBCUx_CONUS_Range_2001v1_eval.csv
SELECT * FROM sp_range;
