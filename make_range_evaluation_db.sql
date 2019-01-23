SELECT load_extension('mod_spatialite');


/*#############################################################################
                                 Load Tables
 ############################################################################*/
/* Add the hucs shapefile to the db. */
SELECT ImportSHP('/users/nmtarr/data/SHUCS', 'shucs', 'utf-8', 102008, 'geom_102008',
                 'HUC12RNG', 'POLYGON');

/* Load the GAP range csv, filter out some columns, rename others */
.import /users/nmtarr/Documents/ranges/inputs/bYBCUx_CONUS_Range_2001v1.csv sp_range
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
