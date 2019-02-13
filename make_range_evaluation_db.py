"""
Builds an sqlite database in which to store range evaluation information.

shucloc needs to be eventually be replaced wtih ScienceBase download of shucs.
"""
import config
import sqlite3
import pandas as pd
import os

# Get gap id
conn2 = sqlite3.connect(config.inDir + 'parameters.sqlite')
cursor2 = conn2.cursor()
sql_tax = """SELECT gap_id FROM species_concepts
             WHERE species_id = '{0}';""".format(config.sp_id)
gap_id = cursor2.execute(sql_tax).fetchone()[0]
gap_id = gap_id[0] + gap_id[1:5] + gap_id[5]
conn2.close()
del cursor2

# Delete db if it exists
eval_db = config.outDir + gap_id + '_range.sqlite' # Name of range evaluation database.
if os.path.exists(eval_db):
    os.remove(eval_db)

# Create or connect to the database
conn = sqlite3.connect(eval_db)
os.putenv('SPATIALITE_SECURITY', 'relaxed')
conn.enable_load_extension(True)
conn.execute('SELECT load_extension("mod_spatialite")')
cursor = conn.cursor()

shucLoc = '/users/nmtarr/data/SHUCS'

sql="""
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


/* Add the hucs shapefile to the db. */
SELECT ImportSHP('{0}', 'shucs', 'utf-8', 102008,
                 'geom_102008', 'HUC12RNG', 'POLYGON');
""".format(shucLoc)
cursor.executescript(sql)

# Load the GAP range csv, filter out some columns, rename others
csvfile = config.inDir + gap_id + "_CONUS_RANGE_2001v1.csv"
sp_range = pd.read_csv(csvfile)
sp_range.to_sql('sp_range', conn, if_exists='replace', index=False)

sql2="""
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
"""
cursor.executescript(sql2)
