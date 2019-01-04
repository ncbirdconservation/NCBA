#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jan  3 08:08:59 2019

@author: nmtarr

Description: draft workflow for creating spatial data summaries to support
range delineation.  This script will eventually be placed in a jupyter
notebook or separate top-level scripts.

1. Create HUC Database
2. Create Occurrence Database
3. Solve Taxonomy
4. Get GBIF Records

"""
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

import sqlite3
import os
os.chdir('/')
os.chdir(workDir)

file1 = workDir + '/Occurrences.sqlite'
if os.path.exists(file1):
    os.remove(file1)

############################################################ Create db 
######################################################################
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

######################################################## Create tables
######################################################################
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

/* Create a table for occurrence records */
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

/* Add a geometry column for the occurrence points with WGS84 SR */
SELECT AddGeometryColumn ('occs', 'geom_4326', 4326, 'POINT', 'XY'); 
"""
cursor.executescript(sql_cdb)

######################################################### Add GAP hucs
######################################################################
sqlHUC = """
/* Add the hucs shapefile to the db. */
SELECT ImportSHP('InData/SHUCS', 'shucs', 'utf-8', 102008, 'geom_102008', 
                 'HUC12RNG', 'POLYGON');
"""
cursor.executescript(sqlHUC)


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
import sqlite3
import os

sp = species

#df = pd.DataFrame(columns=['fws_id', 'gap_code', 'itis_tsn', 'gbif_id', 
#                           'bcb_id', 'common_name', 'scientific_name', 
#                           'start_date', 'end_date'])
#df.index.name='uid'

# Solve taxonomy
name_dict = species.name_backbone(sp)
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
import sqlite3
import os

sp = species
gbif_key = sp_gbif_key

############################# RETRIEVE RECORDS
# First pass filters
latRange = '27,41'
lonRange = '-91,-75'
years = '1998,2018'
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

#############################################################  FILTER MORE
#############################################################
# Remove if no coordinate uncertainty
alloccs3 = [x for x in alloccs2 
            if 'coordinateUncertaintyInMeters' in x.keys()]

#...

###########################################################  INSERT INTO DB
###########################################################

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
                VALUES {0}, GeomFromText('POINT({1} {2})', {3}))""".format(str(insert1)[:-1], 
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


#############################################################  EXPORT
#############################################################

sql4 = """
        CREATE VIEW ybcu_june
        AS 
        SELECT * FROM occs WHERE occurrenceMonth = 6;
        SELECT * FROM ybcu_june;        
    """
cursor.executescript(sql4)

cursor.execute("""SELECT ExportSHP('occs', 'geom_4326', 'ybcu2', 'utf-8');""")


conn.commit()
conn.close()  