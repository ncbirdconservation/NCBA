#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jan  3 08:08:59 2019

@author: nmtarr

Description: draft workflow for creating spatial data summaries to support
range delineation.  This script will eventually be placed in a jupyter
notebook or separate top-level scripts.

"""
#############################################################################
#                            Create HUC Database
#############################################################################
"""
Description: Create a database for the GAP hucs and GAP ranges.  This data 
will be used for spatial summaries of occurrence records.
"""

import sqlite3
import os

data_dir = 'Users/nmtarr/Documents/Ranges/InData'

os.chdir('/Users/nmtarr/Documents/RANGES')

conn = sqlite3.connect("GAPhuc.sqlite")
conn.enable_load_extension(True)
conn.load_extension("mod_spatialite")

curs = conn.cursor()

conn.commit()
conn.close()


'''
# Make table of hucs
sql1 = 'CREATE TABLE hucs
        


# Make table of GBIF occurrences
sql2 = 'CREATE TABLE occu



# Make table of GAP hucs

'''







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
import config

srn = config.epsg

# Connect to or create db
os.chdir('/')
os.chdir(config.workDir)
conn = sqlite3.connect('occurrences.sqlite')
cursor = conn.cursor()

# Make database spatial
conn.enable_load_extension(True)
conn.execute('SELECT load_extension("mod_spatialite")')
conn.execute('SELECT InitSpatialMetaData();')

#### Create taxa table
sql_cdb = """
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
    
        """
#SELECT AddGeometryColumn ('occs', 'Geometry', 4326, 'POINT', 'XY');

cursor.executescript(sql_cdb)
conn.commit()
conn.close()


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
import pandas as pd
from pygbif import species
import config
from pprint import pprint
import sqlite3
import os

sp = config.species

#df = pd.DataFrame(columns=['fws_id', 'gap_code', 'itis_tsn', 'gbif_id', 
#                           'bcb_id', 'common_name', 'scientific_name', 
#                           'start_date', 'end_date'])
#df.index.name='uid'

# Solve taxonomy
name_dict = species.name_backbone(sp)
pprint(name_dict)
#df.loc[1000, 'gbif_key'] = int(name_dict['speciesKey'])

# Connect to or create db
os.chdir('/')
os.chdir(config.workDir)
conn = sqlite3.connect('occurrences.sqlite')
cursor = conn.cursor()
sql1 = """
        INSERT INTO taxa 
        (species_id, gap_code, gbif_id, common_name, scientific_name) 
        VALUES ('bYBCUx0', 'bYBCUx', 2496287, 'Yellow-billed Cuckoo', 
                'Coccyzus americanus');
        """
cursor.execute(sql1)
sql2 = """SELECT * FROM taxa;"""
cursor.execute(sql2).fetchall()
conn.commit()
conn.close()


#############################################################################
#                           Get GBIF Occurrences
#############################################################################
"""
Description: Retrieve GBIF records for a species and save appropriate 
attributes in the occurrence db.  
"""
import config
from pygbif import occurrences
import sqlite3
import os

sp = config.species
gbif_key = config.sp_gbif_key

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
os.chdir(config.workDir)

##### ---- TEMP put a record in taxa table
#conn = sqlite3.connect('occurrences.sqlite')
#cursor = conn.cursor()
#sql-1 = """
#        INSERT INTO taxa 
#        (species_id, gap_code, gbif_id, common_name, scientific_name) 
#        VALUES ('bYBCUx0', 'bYBCUx', 2496287, 'Yellow-billed Cuckoo', 
#                'Coccyzus americanus');
#        """
#cursor.execute(sql-1)
#conn.commit()
#conn.close()

# Insert the records 
insert1 = []
for x in alloccs3:
    insert1.append((x['gbifID'], 'bYBCUx', 'gbif', x['acceptedTaxonKey'], 
            x['decimalLongitude'], x['decimalLatitude'], x['geodeticDatum'],
            x['coordinateUncertaintyInMeters'], x['eventDate'],
            x['year'], x['month']))
insert1 = tuple(insert1)

conn = sqlite3.connect('occurrences.sqlite')
cursor = conn.cursor()
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
'''
#############################################################  EXPORT
#############################################################
conn.enable_load_extension(True)
conn.execute('SELECT load_extension("mod_spatialite")')
sql4 = """
        SELECT ExportSHP('occs', 'geom', 'cuckoccurence', 'utf-8');
        
    """
cursor.execute(sql4)


conn.commit()
conn.close()
'''   