#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jan 16 11:31:29 2019

@author: nmtarr

!!!!!!! Not working
"""
import sciencebasepy
import os
import pprint

sp_id = 'bybcux0'
summary_name = 'cuckoo'
gbif_req_id = 'r001'
gbif_filter_id = 'f001'
gap_id = 'bybcux'

workDir = '/Users/nmtarr/Documents/RANGES/'
codeDir = '/Users/nmtarr/Code/Ranger/'
inDir = workDir + 'Inputs/'
outDir = workDir + 'Outputs/'

########################################################
sb = sciencebasepy.SbSession()

# Search for gap range item in ScienceBase
gap_id = gap_id[0] + gap_id[1:5].upper() + gap_id[5]
item_search = '{0}_CONUS_2001v1 Range Map'.format(gap_id)
items = sb.find_items_by_any_text(item_search)

# Get a public item.  No need to log in.
item_json = sb.get_item(items['items'][0]['id'])
ret = sb.get_item_files(items['items'][0]['id'], inDir)

    

    

