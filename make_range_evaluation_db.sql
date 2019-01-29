.mode csv

ATTACH DATABASE '/users/nmtarr/documents/ranges/outputs/bybcux_range.sqlite'
                AS rangeDB;

SELECT load_extension('mod_spatialite');

SELECT InitSpatialMetadata();

/* Add Albers_Conic_Equal_Area 102008 to the spatial sys ref tables */
SELECT InitSpatialMetaData();
             INSERT into spatial_ref_sys
             (srid, auth_name, auth_srid, proj4text, srtext)
             values (102008, 'ESRI', 102008, '+proj=aea +lat_1=20 +lat_2=60
             +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m
             +no_defs ', 'PROJCS["North_America_Albers_Equal_Area_Conic",
             GEOGCS["GCS_North_American_1983",
             DATUM["North_American_Datum_1983",
             SPHEROID["GRS_1980",6378137,298.257222101]],
             PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],
             PROJECTION["Albers_Conic_Equal_Area"],
             PARAMETER["False_Easting",0],
             PARAMETER["False_Northing",0],
             PARAMETER["longitude_of_center",-96],
             PARAMETER["Standard_Parallel_1",20],
             PARAMETER["Standard_Parallel_2",60],
             PARAMETER["latitude_of_center",40],
             UNIT["Meter",1],AUTHORITY["EPSG","102008"]]');


/*#############################################################################
                                 Load Tables
 ############################################################################*/
/* Add the hucs shapefile to the db. */
SELECT ImportSHP('/users/nmtarr/data/SHUCS', 'shucs', 'utf-8', 102008,
                 'geom_102008', 'HUC12RNG', 'POLYGON');

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
