#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jan 14 14:26:24 2019

@author: nmtarr

Description: Species-concepts change over time, sometimes with a spatial 
component (e.g., changes in range delination of closely related species or 
subspecies).  Retrieval of data for the wrong species-concept would introduce 
error.  Therefore, the first step is to sort out species concepts of different
datasets to identify concepts that can be investigated.

For this project/effort, individual species-concepts are identified,
crosswalked to concepts from various datasets, and stored in a table within
requests.sqlite database.
"""


