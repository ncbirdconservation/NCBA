#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jan 3 08:08:59 2019

author: nmtarr

Description: Draft workflow for creating spatial data summaries to support
range delineation.  This script will eventually be placed in a jupyter
notebook or separate top-level scripts.

Functionality:
Identifying a species concept to investigate
Retrieving and filtering occurrence records, saving them in a database
Generating shapefiles of occurrence records by month as well as concave hull
polygons around recordsself.

Outputs:
Maps and spatially-explicit tables within a database.

Uses:
Provide resources to consult when creating expert drawn range mapsself.
Investigation of data availability and agreement.

Unresolved issues:
1.  Downloading from Sciencebase with code.
2.  Maximize filtering.
3.  Develop taxonomy section.
4.  Archiving and documenting data and process.
5.  Incorporate detection distance.
6.  Print maps into console.
7.  Incorporate allowable spatial error, which would vary by species.
"""
import pandas as pd
pd.set_option('display.width', 1000)
%matplotlib inline
import matplotlib.pyplot as plt
from mpl_toolkits.basemap import Basemap
import matplotlib.gridspec as gridspec
import numpy as np
import sqlite3
import os
os.chdir('/')
os.chdir(workDir)


#############################################################################
#                               Configuration
#############################################################################
species = 'Coccyzus americanus' # Yellow-billed Cuckoo
sp_id = 'bYBCUx0'
sp_gbif_key = 2496287
sp_TSN = 177831
srn = 102008 # Albers Equal Area spatial reference ID
workDir = '/Users/nmtarr/Documents/RANGES'
data_dir = '/Users/nmtarr/Documents/Ranges/InData'
SRID_dict = {'WGS84': 4326, 'AlbersNAD83': 102008}


#############################################################################
#                       Create Occurrence Database
#############################################################################
"""
Description: Create a database for storing occurrence and species-concept
data.  Needs to have spatial querying functionality.
"""
# Delete the database if it already exists
file1 = workDir + '/Occurrences.sqlite'
if os.path.exists(file1):
    os.remove(file1)

# Create or connect to the database
conn = sqlite3.connect('occurrences.sqlite')
os.putenv('SPATIALITE_SECURITY', 'relaxed')
conn.enable_load_extension(True)
conn.execute('SELECT load_extension("mod_spatialite")')
cursor = conn.cursor()

# Make database spatial and add the spatial reference system that GAP used
conn.executescript('''SELECT InitSpatialMetaData();
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
             UNIT["Meter",1],AUTHORITY["EPSG","102008"]]');''')

################################################################# Create tables
###############################################################################
sql_cdb = """
/* Create a taxa table for species-concepts */
CREATE TABLE IF NOT EXISTS taxa (
                    species_id TEXT NOT NULL PRIMARY KEY UNIQUE,
                    fws_id TEXT,
                    gap_code VARCHAR(5),
                    itis_tsn TEXT,
                    gbif_id TEXT,
                    bcb_id TEXT,
                    common_name TEXT,
                    scientific_name TEXT,
                    start_date TEXT,
                    end_date TEXT)
                    WITHOUT ROWID;

/* Create a table for occurrence records, WITH GEOMETRY */
CREATE TABLE IF NOT EXISTS occs (
        occ_id INTEGER NOT NULL PRIMARY KEY UNIQUE,
        species_id INTEGER NOT NULL,
        source TEXT NOT NULL,
        source_sp_id TEXT NOT NULL,
        coordinateUncertaintyInMeters INTEGER,
        occurrenceDate TEXT,
        occurrenceYear TEXT,
        occurrenceMonth TEXT,
        retrievalDate TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
        individualCount INTEGER DEFAULT 1,
            FOREIGN KEY (species_id) REFERENCES taxa(species_id)
            ON UPDATE RESTRICT
            ON DELETE NO ACTION);
SELECT AddGeometryColumn('occs', 'geom_4326', 4326, 'POINT', 'XY');

/* Make a table for storing range maps for unique species-time period
combinations, WITH GEOMETRY */
CREATE TABLE rangemaps (
             rmap_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
             species_id TEXT NOT NULL,
             period TEXT NOT NULL);
SELECT AddGeometryColumn('rangemaps', 'range', 102008, 'MULTIPOLYGON',
                          'XY');
SELECT AddGeometryColumn('rangemaps', 'circles', 102008, 'MULTIPOLYGON',
                          'XY');
"""
cursor.executescript(sql_cdb)

#############################################################################
#                            SOLVE TAXONOMY
#############################################################################
"""
Description:  Data retrieved from datasets is for specific species-concepts,
which change over time, sometimes with a spatial component (e.g., changes in
range delination of closely related species or subspecies).  Retrieval of data
for the wrong species-concept would introduce error.  Therefore, the first
step is to sort out species concepts of different datasets to identify concepts
to include.

For this project/effort, individual species-concepts are identified,
crosswalked to concepts from various datasets, and stored in a table within
occurrences.sqlite database.
"""
from pygbif import species
from pprint import pprint

#df = pd.DataFrame(columns=['fws_id', 'gap_code', 'itis_tsn', 'gbif_id',
#                           'bcb_id', 'common_name', 'scientific_name',
#                           'start_date', 'end_date'])
#df.index.name='uid'

# Solve taxonomy
name_dict = species.name_backbone(species)
pprint(name_dict)
#df.loc[1000, 'gbif_key'] = int(name_dict['speciesKey'])

# Connect to or create db
sql1 = """
        INSERT INTO taxa
        (species_id, gap_code, gbif_id, common_name, scientific_name)
        VALUES ('bYBCUx0', 'bYBCUx', 2496287, 'Yellow-billed Cuckoo',
                'Coccyzus americanus');
        """
cursor.execute(sql1)
sql2 = """SELECT * FROM taxa;"""
cursor.execute(sql2).fetchall()


#############################################################################
#                            Get GBIF Records
#############################################################################
"""
Description: Retrieve GBIF records for a species and save appropriate
attributes in the occurrence db.
"""
from pygbif import occurrences

sp = species
gbif_key = sp_gbif_key

############################# RETRIEVE RECORDS
# Up-front filters are an opportunity to lighten the load from the start.
latRange = '27,41'
lonRange = '-91,-75'
years = '1970,2018'
months = '1,12'
geoIssue = False
coordinate = True
continent= 'north_america'

# First, find out how many records there are that meet criteria
occ_search = occurrences.search(gbif_key,
                          year=years,
                          month=months,
                          decimelLatitude=latRange,
                          decimelLongitude=lonRange,
                          hasGeospatialIssue=geoIssue,
                          hasCoordinate=coordinate,
                          continent=continent)
occ_count=occ_search['count']
print('{0} records exist'.format(occ_count))

# Get occurrences in batches, saving into master list
alloccs = []
batches = range(0, occ_count, 300)
for i in batches:
    occ_json = occurrences.search(gbif_key,
                              limit=300,
                              offset=i,
                              year=years,
                              month=months,
                              decimelLatitude=latRange,
                              decimelLongitude=lonRange,
                              hasGeospatialIssue=geoIssue,
                              hasCoordinate=coordinate,
                              continent=continent)
    occs = occ_json['results']
    alloccs = alloccs + occs
    print(len(alloccs))

# Pull out relevant attributes from occurrence dictionaries.  Filtering
# will be performed with info from these keys.
keykeys = ['basisOfRecord', 'individualCount', 'acceptedTaxonKey',
           'scientificName', 'acceptedScientificName','taxonomicStatus',
           'decimalLongitude', 'decimalLatitude',
           'coordinateUncertaintyInMeters', 'year',
           'month', 'day', 'eventDate', 'issues','geodeticDatum',
           'gbifID', 'type', 'preparations', 'occurrenceStatus',
           'georeferenceProtocol', 'georeferenceVerificationStatus',
           'occurrenceID']
alloccs2 = []
for x in alloccs:
    alloccs2.append(dict((y,x[y]) for y in x if y in keykeys))

##################################################################  FILTER MORE
###############################################################################
# Remove if no coordinate uncertainty
alloccs3 = [x for x in alloccs2
            if 'coordinateUncertaintyInMeters' in x.keys()]

#  WHAT ELSE CAN WE DO ??????...

###############################################################  INSERT INTO DB
###############################################################################
# Insert the records
for x in alloccs3:
    insert1 = []
    insert1.append((x['gbifID'], 'bYBCUx', 'gbif', x['acceptedTaxonKey'],
            x['coordinateUncertaintyInMeters'], x['eventDate'],
            x['year'], x['month']))

    insert1 = tuple(insert1)[0]

    sql1 = """INSERT INTO occs ('occ_id', 'species_id', 'source',
                            'source_sp_id', 'coordinateUncertaintyInMeters',
                            'occurrenceDate','occurrenceYear',
                            'occurrenceMonth', 'geom_4326')
                VALUES {0}, GeomFromText('POINT({1} {2})',
                                            {3}))""".format(str(insert1)[:-1],
                x['decimalLongitude'], x['decimalLatitude'],
                SRID_dict[x['geodeticDatum']])
    cursor.executescript(sql1)

# Update the individual count when it exists
for e in alloccs3:
    if 'individualCount' in e.keys():
        print(e)
        sql2 = """UPDATE occs
            SET individualCount = {0}
            WHERE occ_id = {1};""".format(e['individualCount'], e['gbifID'])
        cursor.execute(sql2)

################################################################  BUFFER POINTS
###############################################################################
# Buffer the x,y locations with the coordinate uncertainty
# in order to create circles.  Create versions in albers and wgs84.  The
# wgs84 version will be used in plotting with Basemap.
sql_buf = """
/* Transform to albers (102008) and apply buffer */
ALTER TABLE occs ADD COLUMN circle_albers BLOB;

UPDATE occs SET circle_albers = Buffer(Transform(geom_4326, 102008),
                                coordinateUncertaintyInMeters);

/* The new geometry column needs an entry in the geometry columns table CAN THIS BE DONE WITH RecoverGeometryColumn???*/
INSERT INTO geometry_columns (f_table_name, f_geometry_column,
                              geometry_type, coord_dimension,
                              srid, spatial_index_enabled)
            VALUES ('occs', 'circle_albers', 3, 2, 102008, 0);

/* Apply buffer */
ALTER TABLE occs ADD COLUMN circle_wgs84 BLOB;

UPDATE occs SET circle_wgs84 = Transform(circle_albers, 4326);

SELECT RecoverGeometryColumn('occs', 'circle_wgs84', 4326, 'POLYGON', 'XY');

/* Export the results to a shapefile */
SELECT ExportSHP('occs', 'circle_wgs84',
                 '/users/nmtarr/documents/ranges/ybcu_circles', 'utf-8');
"""
cursor.executescript(sql_buf)
conn.commit()


##################################################################  EXPORT MAPS
###############################################################################
# Export occurrence 'points' as a shapefile (all seasons)
cursor.execute("""SELECT ExportSHP('occs', 'geom_4326', 'occ_points',
                                   'utf-8');""")

# Display

m = Basemap(ax=plt.subplot(G[0, 1:]), projection='aea', resolution='c',
                llcrnrlat=20,urcrnrlat=55,
                llcrnrlon=-126,urcrnrlon=-66,
                lat_ts=20,)
m.drawcoastlines()
m.drawstates()
m.drawcountries()
m.fillcontinents(color='green',lake_color='aqua')
m.drawmapboundary(fill_color='aqua')

# plot





# Make occurrence shapefiles for each month
month_dict = {'january': 1, 'february':2, 'march':3, 'april':4, 'may':5,
              'june':6, 'july':7, 'august':8, 'september':9, 'october':10,
              'november':11, 'december':12}
for month in month_dict.keys():
    print(month)
    try:
        sql4 = """
        /* Create tables for each month and export as shapefiles. */

        INSERT INTO rangemaps (species_id, period, range, circles)
                        SELECT species_id, '{0}',
                        ConcaveHull(CastToMultiPolygon(GUnion(circle_albers))),
                        CastToMultiPolygon(GUnion(circle_albers))
                        FROM occs
                        WHERE occurrenceMonth = {1};

        /* Pull out the period for mapping */
        CREATE TABLE temp1 AS SELECT * FROM rangemaps
                        WHERE period='{0}';
        SELECT RecoverGeometryColumn('temp1', 'range', 102008, 'MULTIPOLYGON',
                                     'XY');
        SELECT RecoverGeometryColumn('temp1', 'circles', 102008, 'MULTIPOLYGON',
                                     'XY');
        /* Export shapefiles */
        SELECT ExportSHP('temp1', 'range', '{0}_rng', 'utf-8');

        SELECT ExportSHP('temp1', 'circles', '{0}_occs', 'utf-8');

        DROP TABLE temp1;

        """.format(month, month_dict[month])
        cursor.executescript(sql4)
    except:
        print(Exception)

# Make range shapefiles for each season
period_dict = {"summer": '(5,6,7,8)', "winter": '(11,12,1,2)',
               "spring": '(3,4,5)', "fall": '(8,9,10,11)',
               "yearly": '(1,2,3,4,5,6,7,8,9,10,11,12)'}
for period in period_dict:
    print(period)
    try:
        sql_season = """
            /*  Insert a record for a range map, created by making polygons into
            a multipolygon geometry and then calculating the concave hull.
            Also, insert circles into a column for provenance */
            INSERT INTO rangemaps (species_id, period, range, circles)
                    SELECT species_id, '{0}',
                    ConcaveHull(CastToMultiPolygon(GUnion(circle_albers))),
                    CastToMultiPolygon(GUnion(circle_albers))
                    FROM occs
                    WHERE occurrenceMonth IN {1};

            /* Pull out the period for mapping */
            CREATE TABLE temp2 AS SELECT * FROM rangemaps
                            WHERE period='{0}';
            SELECT RecoverGeometryColumn('temp2', 'range', 102008,
                                         'MULTIPOLYGON',
                                         'XY');
            SELECT RecoverGeometryColumn('temp2', 'circles', 102008,
                                         'MULTIPOLYGON',
                                         'XY');

            SELECT ExportSHP('temp2', 'range', '{0}_rng', 'utf-8');

            SELECT ExportSHP('temp2', 'circles', '{0}_occs', 'utf-8');

            DROP TABLE temp2;
        """.format(period, period_dict[period])
        cursor.executescript(sql_season)
    except:
        print(Exception)
conn.commit()


# Print maps

###########################################################  GET THE GAP RANGES
###########################################################  FROM SCIENCEBASE

conn.commit()
conn.close()
