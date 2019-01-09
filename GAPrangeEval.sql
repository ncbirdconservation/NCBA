/*
Use the occurrence data to evaluate the GAP range map for a species
*/

/* Load the GAP range csv, filter out some columns, rename others */
.mode csv
.import /users/nmtarr/Documents/ranges/indata/bYBCUx_CONUS_Range_2001v1.csv GAPrange
.headers on
ALTER TABLE GAPrange RENAME TO garb;
CREATE TABLE GAPrange AS
                      SELECT strHUC12RNG AS HUC12RNG,
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

/* Add a column to store gbif evaluation result - meaning, does the huc
completely contain an occurrence circle?  Circles that overlap huc boundaries
are left out of this, but could possibly be added later */
ALTER TABLE GAPrange ADD COLUMN eval_gbif1 INTEGER;

/*  Select hucs that contain a circle, and insert a '1' in the evaluation
column within GAPrange for them */
CREATE TABLE blue AS
              SELECT shucs.HUC12RNG, shucs.geom_102008
              FROM shucs, occs
              WHERE Contains(shucs.geom_102008, occs.circle_albers);
ALTER TABLE blue ADD COLUMN eval INTEGER ;

UPDATE GAPrange
SET eval_gbif1 = (SELECT eval FROM blue WHERE HUC12RNG = GAPrange.HUC12RNG)
WHERE EXISTS (SELECT eval FROM blue WHERE HUC12RNG = GAPrange.HUC12RNG);

SELECT RecoverGeometryColumn('blue', 'geom_102008', 102008,
                              'POLYGON');

SELECT ExportSHP('blue', 'geom_102008', 'shuctest2', 'utf-8');




/*
Select hucs that overlap with occurrence circles
CREATE TABLE orange AS
              SELECT shucs.HUC12RNG, shucs.geom_102008
              FROM shucs, occs
              WHERE Overlaps(shucs.geom_102008, occs.circle_albers)
                OR Contains(occs.circle_albers, shucs.geom_102008);
SELECT RecoverGeometryColumn('orange', 'geom_102008', 102008,
                              'POLYGON');

SELECT ExportSHP('orange', 'geom_102008', 'shuctest1', 'utf-8');
*/
