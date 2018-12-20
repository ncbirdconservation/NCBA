#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Dec 19 18:29:57 2018

@author: nmtarr

Environment: ranger

Description: Create a database for the GAP hucs and GAP ranges

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
