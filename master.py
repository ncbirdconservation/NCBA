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
workDir = 'Users/nmtarr/Documents/RANGES'
data_dir = 'Users/nmtarr/Documents/Ranges/InData'
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
os.chdir('Users/nmtarr/Code/Ranger')

file1 = 'Occurrences.sqlite'
if os.path.exists(file1):
    os.remove(file1)

############################################################ Create db 
######################################################################
os.chdir('/')
os.chdir(workDir)
conn = sqlite3.connect('occurrences.sqlite')
os.putenv('SPATIALITE_SECURITY', 'relaxed')
conn.enable_load_extension(True)
conn.execute('SELECT load_extension("mod_spatialite")')
cursor = conn.cursor()

# Make database spatial
conn.execute('SELECT InitSpatialMetaData();')

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
        decimalLongitude VARCHAR NOT NULL,
        decimalLatitude VARCHAR NOT NULL, 
        geodeticDatum TEXT NOT NULL,
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
SELECT AddGeometryColumn ('occs_geo', 'geom_4326', 4326, 'POINT', 'XY'); 
        """
cursor.executescript(sql_cdb)

######################################################### Add GAP hucs
######################################################################
sqlHUC = """
/* Add the hucs shapefile to the db. */
SELECT ImportSHP('InData/SHUCS', 'shucs_albers', 'utf-8', 102008, 'geometry', 
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
os.chdir('/')
os.chdir(workDir)

# Insert the records 
insert1 = []
for x in alloccs3:
    insert1.append((x['gbifID'], 'bYBCUx', 'gbif', x['acceptedTaxonKey'], 
            x['decimalLongitude'], x['decimalLatitude'], x['geodeticDatum'],
            x['coordinateUncertaintyInMeters'], x['eventDate'],
            x['year'], x['month']))
insert1 = tuple(insert1)

sql1 = """INSERT INTO occs (
                            'occ_id', 'species_id', 'source',
                            'source_sp_id', 'decimalLongitude',
                            'decimalLatitude', 'geodeticDatum',
                            'coordinateUncertaintyInMeters', 
                            'occurrenceDate','occurrenceYear', 
                            'occurrenceMonth')
                            VALUES {0}""".format(str(insert1)[1:-1])
cursor.execute(sql1)

# Update the individual count when it exists
for e in alloccs3:
    if 'individualCount' in e.keys():
        print(e)
        sql2 = """UPDATE occs
            SET individualCount = {0}
            WHERE occ_id = {1};""".format(e['individualCount'], e['gbifID'])
        cursor.execute(sql2)
   
############################################################  Geometry
############################################################       
# Geometry needs to be added to the occurrence records, but they may not all 
# have the same spatial reference.  Sort this out; initially, GBIF is mostly 
# WGS84 so just use those records.
sql_geo = """
CREATE TABLE IF NOT EXISTS occs_geo AS 
                        SELECT * FROM occs 
                        WHERE geodeticDatum = 'WGS84';



ALTER TABLE occs_geo ADD COLUMN geom_102008;
UPDATE occs_geo SET geom_102008=TRANSFORM(geom_4326, 102008);
"""
cursor.executescript(sql_geo)

'''
#############################################################  EXPORT
#############################################################

sql4 = """
        SELECT ExportSHP('occs', 'geom', 'cuckoccurence', 'utf-8');
        
    """
cursor.execute(sql4)
'''



#############################################################################
#                           Get GAP Range Data
#############################################################################



#os.putenv('SPATIALITE_SECURITY', '')  Remove the environment variable, somehow


conn.commit()
conn.close()  