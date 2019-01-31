/*
This code must be run in the sqlite3 command line interface until packaged
within python code.

Uses occurrence data collected with 'retrieve_occurrences.py' to evaluate the
GAP range map for a species.  A table is created for the GAP range and columns
reporting the results of evaluation and validation are populated after
evaluating spatial relationships of occurrence records (circles) and GAP range.

The results of this code are new columns in the GAP range table (in the db
created for work in this repository) and a range shapefile.

The primary use of code like this would be range evaluation and revision.

Unresolved issues:
1. Can the runtime be improved with spatial indexing?  Minimum bounding rectangle?
2. ".import" has to be worked around when this goes into python.
3. Locations of huc files. -- can sciencebase be used?
4. Add month and year filters - they are currently hard-coded but need to come
   from queries of rng_eval_params.evaluations.
5. Condition data used on the parameters, such as filter_sets in the evaluations
   table.
6. Draw from config file when possible.
*/

.headers on
.mode csv
ATTACH DATABASE '/users/nmtarr/documents/ranges/outputs/bybcux0_occurrences.sqlite'
                AS occs;

ATTACH DATABASE '/users/nmtarr/documents/ranges/inputs/rng_eval_params.sqlite'
                AS params;

SELECT load_extension('mod_spatialite');

/*#############################################################################
                             Assess Agreement
 ############################################################################*/

 /*#########################  Which HUCs contain an occurrence?
 #############################################################*/
/*  Intersect occurrence circles with hucs */
CREATE TABLE green AS
              SELECT shucs.HUC12RNG, ox.occ_id,
              CastToMultiPolygon(Intersection(shucs.geom_102008,
                                              ox.circle_albers)) AS geom_102008
              FROM shucs, occs.occurrences AS ox
              WHERE Intersects(shucs.geom_102008, ox.circle_albers)
                AND Cast(strftime('%m', ox.occurrenceDate) AS INTEGER) IN
                (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
                AND Cast(strftime('%Y', ox.occurrenceDate) AS INTEGER) IN (1990,
                    1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
                    2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
                    2011, 2012, 2013, 2014, 2015);

SELECT RecoverGeometryColumn('green', 'geom_102008', 102008, 'MULTIPOLYGON',
                             'XY');

/* In light of the error tolerance for the species, which occurrences can
   be attributed to a huc?  */
CREATE TABLE orange AS
  SELECT green.HUC12RNG, green.occ_id,
         100 * (Area(green.geom_102008) / Area(ox.circle_albers))
            AS proportion_circle
  FROM green
       LEFT JOIN occs.occurrences AS ox
       ON green.occ_id = ox.occ_id
  WHERE proportion_circle BETWEEN (100 - (SELECT error_tolerance
                                          FROM params.evaluations
                                          WHERE id= 'eval_gbif1'))
                                  AND 100;

/*  How many occurrences in each huc that had an occurrence? */
ALTER TABLE sp_range ADD COLUMN eval_gbif1_cnt INTEGER;

UPDATE sp_range
SET eval_gbif1_cnt = (SELECT COUNT(occ_id)
                      FROM orange
                      WHERE HUC12RNG = sp_range.strHUC12RNG
                      GROUP BY HUC12RNG);


/*  Find hucs that contained gbif occurrences, but were not in gaprange and
insert them into sp_range as new records.  Record the occurrence count */
INSERT INTO sp_range (strHUC12RNG, eval_gbif1_cnt)
            SELECT orange.HUC12RNG, COUNT(occ_id)
            FROM orange LEFT JOIN sp_range ON sp_range.strHUC12RNG = orange.HUC12RNG
            WHERE sp_range.strHUC12RNG IS NULL
            GROUP BY orange.HUC12RNG;


/*############################  Does HUC contain an occurrence?
#############################################################*/
ALTER TABLE sp_range ADD COLUMN eval_gbif1 INTEGER;

/*  Record in sp_range that gap and gbif agreed on species presence, in light
of the pad for the species. */
UPDATE sp_range
SET eval_gbif1 = 1
WHERE eval_gbif1_cnt >= (SELECT pad
                        FROM params.evaluations
                        WHERE id = 'eval_gbif1');


/*  For new records, put zeros in GAP range attribute fields  */
UPDATE sp_range
SET intGAPOrigin = 0,
    intGAPPresence = 0,
    intGAPReproduction = 0,
    intGAPSeason = 0,
    eval_gbif1 = 0
WHERE eval_gbif1_cnt >= 0 AND intGAPOrigin IS NULL;


/*###########################################  Validaton column
#############################################################*/
/*  Populate a validation column.  If an evaluation supports the GAP ranges
then it is validated */
ALTER TABLE sp_range ADD COLUMN validated_presence INTEGER NOT NULL DEFAULT 0;

UPDATE sp_range
SET validated_presence = 1
WHERE eval_gbif1 = 1;


/*#############################################################################
                               Export Table and Map
 ############################################################################*/
/*  Create a version of sp_range with geometry  */
CREATE TABLE new_range AS
              SELECT sp_range.*, Transform(shucs.geom_102008, 4326) AS geom_4326
              FROM sp_range LEFT JOIN shucs ON sp_range.strHUC12RNG = shucs.HUC12RNG;

SELECT RecoverGeometryColumn('new_range', 'geom_4326', 4326, 'POLYGON', 'XY');

SELECT ExportSHP('new_range', 'geom_4326',
                 '/users/nmtarr/documents/ranges/outputs/bYBCUx_CONUS_Range_2001v1_eval',
                 'utf-8');

/* Make a shapefile of evaluation results */
CREATE TABLE eval_gbif1 AS
              SELECT strHUC12RNG, eval_gbif1, geom_4326
              FROM new_range
              WHERE eval_gbif1 >= 0;

SELECT RecoverGeometryColumn('eval_gbif1', 'geom_4326', 4326, 'POLYGON', 'XY');

SELECT ExportSHP('eval_gbif1', 'geom_4326',
                 '/users/nmtarr/documents/ranges/outputs/bYBCUx_eval_gbif1',
                 'utf-8');

/* Export csv */
.output /users/nmtarr/documents/ranges/outputs/bYBCUx_CONUS_Range_2001v1_eval.csv
SELECT * FROM sp_range;

/*#############################################################################
                             Clean Up
#############################################################################*/
/* sp_range is no longer needed, use new_range instead */
DROP TABLE sp_range;
DROP TABLE green;
DROP TABLE orange;
