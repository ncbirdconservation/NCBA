#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Dec 19 20:31:49 2018

@author: nmtarr

Description: Retrieve GBIF records for a species and save appropriate attributes
in the occurrence db.  
"""

import config
from pprint import pprint
from pygbif import species
from pygbif import occurrences
import pandas as pd
import sqlite3
import os

sp = config.species
gbif_key = config.sp_gbif_key


############################################################# RETRIEVE RECORDS
#############################################################
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

conn.commit()
conn.close()
            