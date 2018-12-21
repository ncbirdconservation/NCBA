#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Dec 19 20:19:02 2018

@author: nmtarr
"""
import sys
sys.path.append('Users/nmtarr/Code/ranger/')
import config
import sqlite3
import os

srn = config.epsg

# Connect to or create db
os.chdir('/')
os.chdir(config.workDir)
conn = sqlite3.connect('occurrences.sqlite')
cursor = conn.cursor()

sql1 = """
CREATE TABLE IF NOT EXISTS taxa (
                    species_id INTEGER NOT NULL PRIMARY KEY UNIQUE,
                    fws_id TEXT,
                    gap_code VARCHAR(5),
                    itis_tsn TEXT,
                    gbif_id TEXT,
                    bcb_id TEXT,
                    common_name TEXT,
                    scientific_name TEXT,
                    start_date TEXT, 
                    end_date TEXT)
    WITHOUT ROWID;"""
cursor.execute(sql1)

sql2 = """CREATE TABLE IF NOT EXISTS occs (
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
            ON DELETE NO ACTION)
    WITHOUT ROWID;"""
cursor.execute(sql2)

conn.commit()
conn.close()
