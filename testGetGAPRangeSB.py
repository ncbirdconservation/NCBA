#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jan 16 11:31:29 2019

@author: nmtarr
"""
import sciencebasepy
import os
import pprint

sp_id = 'bybcux0'
summary_name = 'cuckoo'
gbif_req_id = 'r001'
gbif_filter_id = 'f001'

workDir = '/Users/nmtarr/Documents/RANGES/'
codeDir = '/Users/nmtarr/Code/Ranger/'
inDir = workDir + 'Inputs/'
outDir = workDir + 'Outputs/'


gap_id = 'bybcux'.upper()
sb = sciencebasepy.SbSession()

# Search
item_search = 'bYBCUx_CONUS_2001v1 Range Map'
items = sb.find_items_by_any_text(item_search)

# Get a public item.  No need to log in.
item_json = sb.get_item(items['items'][0]['id'])
print("Public Item: " + str(item_json))


ret = sb.get_item_files(item, inDir)

    

    

