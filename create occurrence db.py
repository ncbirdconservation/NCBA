#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Dec 19 20:19:02 2018

@author: nmtarr

Description: Create a database for storing occurrence data.  Needs to have 
spatialite functionality.
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
sql1 = """
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

cursor.executescript(sql1)
conn.commit()
conn.close()