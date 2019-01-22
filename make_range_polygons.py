#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Jan 15 11:03:56 2019

@author: nmtarr

Description: Use occurrence polygons to evaluate GAP range maps.

TO DO:
1. max_error_meters -> error_tolerance
2  remove pad?
3. Only make month and season polygons if species is migratory.
"""
#############################################################################
#                               Configuration
#############################################################################
sp_id = 'bybcux0'
summary_name = 'cuckoo'
gbif_req_id = 'r001'
gbif_filter_id = 'f001'
max_coordUncertainty = 10000
year_range = (1980,2018)

workDir = '/Users/nmtarr/Documents/RANGES/'
codeDir = '/Users/nmtarr/Code/Ranger/'
inDir = workDir + 'Inputs/'
outDir = workDir + 'Outputs/'
# Used in file names for output.
SRID_dict = {'WGS84': 4326, 'AlbersNAD83': 102008}


#############################################################################
#                                  Imports
#############################################################################
import pandas as pd
pd.set_option('display.width', 1000)
#%matplotlib inline
import sqlite3
import sciencebasepy
from pygbif import occurrences
import os
os.chdir('/')
os.chdir(codeDir)
import config
import config
import sqlite3
import os


#############################################################################
#                              Species-concept
#############################################################################
os.chdir(codeDir)
# Get species info from requests database
conn2 = sqlite3.connect(inDir + 'requests.sqlite')
cursor2 = conn2.cursor()
sql_tax = """SELECT gbif_id, common_name, scientific_name,
                    error_tolerance, gap_id, pad, migratory
             FROM species_concepts
             WHERE species_id = '{0}';""".format(sp_id)
concept = cursor2.execute(sql_tax).fetchall()[0]
gbif_id = concept[0]
common_name = concept[1]
scientific_name = concept[2]
error_toler = concept[3]
gap_id = concept[4]
pad = concept[5]
migratory = concept[6]


#############################################################################
#                          Connect to Database
#############################################################################
# Delete the database if it already exists
evdb = outDir + 'range_eval.sqlite'
if os.path.exists(evdb):
    os.remove(evdb)

# Create or connect to the database
conn = sqlite3.connect(evdb)
os.putenv('SPATIALITE_SECURITY', 'relaxed')
conn.enable_load_extension(True)
conn.execute('SELECT load_extension("mod_spatialite")')
cursor = conn.cursor()

# Make db spatial
cursor.execute('SELECT InitSpatialMetadata();')

sql_rngy = """
        /* Make a table for storing range maps for unique species-time period
           combinations, needs GEOMETRY */
        CREATE TABLE IF NOT EXISTS range_polygons (
                     rng_polygon_id TEXT NOT NULL PRIMARY KEY,
                     alias TEXT UNIQUE,
                     species_id TEXT NOT NULL,
                     months TEXT,
                     years TEXT,
                     method TEXT,
                     max_uncertainty_meters INTEGER,
                     pad INTEGER,
                     date_created TEXT
                     );
            """
cursor.executescript(sql_rngy)

sql_geom = """SELECT AddGeometryColumn('range_polygons', 'range_4326', 4326,
                         'MULTIPOLYGON', 'XY');

              SELECT AddGeometryColumn('range_polygons', 'occurrences_4326', 4326,
                         'MULTIPOLYGON', 'XY');"""
cursor.executescript(sql_geom)


#############################################################################
#                          Make Some Range Polygons
#############################################################################
# Function for making range_polygons
def MakeConcaveHull(rng_poly_id, alias, sp_id, months, years,
                    max_uncertainty, outDir, export, factor=2,
                    allow_holes=True):
    """
    Function for creating a range polygon entry in range_eval.range_polygons.

    Arguments:
    rng_poly_id -- A unique ID to use for the range map record
    alias -- keyword to use for filenames and shorthand reference to polygon
    sp_id -- species id for this project.  Must be in requests.species_concepts.
    months -- tuple of months to include.  For example: (3,4,5,6,7)
    years -- tuple of start and end years to use.  Format as (1980,2000)
    max_uncertainty -- max coordinate uncertainty to allow when filtering
                            occurrences for use in polygon delineation.
    factor -- factor to use in concave hull; defaults to 2.
    allow_holes -- True or False for holes within the range.
    outDir -- working directory, where to put the output.
    export -- True False whether to create a shapefile version in outDir.
    """
    years2 = str(tuple(range(years[0], years[1])))

    print('SRID being used is 4326')
    sql = """
    /* Attach requests database */
    ATTACH DATABASE '/Users/nmtarr/Documents/RANGES/Inputs/requests.sqlite'
    AS requests;

    /* Attach an occurrences database */
    ATTACH DATABASE '/Users/nmtarr/Documents/RANGES/Outputs/{2}_occurrences.sqlite'
    AS occs;

    /* Create range map for the period. */
    INSERT INTO range_polygons (rng_polygon_id, alias, species_id,
                                months, years,
                                method, date_created, range_4326,
                                occurrences_4326)
                    SELECT '{0}', '{1}', '{2}', '{3}', '{5}',
                           'concave hull_{7}_{8}', date('now'),
                            CASE
                            WHEN (SELECT COUNT(circle_wgs84)
                            FROM occs.occurrences
                            WHERE cast(strftime('%m', occurrenceDate) AS INTEGER) IN {3}
                            AND cast(strftime('%Y', occurrenceDate) AS INTEGER) IN {4}
                            AND coordinateUncertaintyInMeters < {6})
                            > 3 THEN ConcaveHull(CastToMultiPolygon(GUnion(circle_wgs84)),
                                                 {7}, {8})
                            ELSE NULL
                            END range_4326,
                            CastToMultiPolygon(GUnion(circle_wgs84))
                    FROM occs.occurrences
                    WHERE cast(strftime('%m', occurrenceDate) AS INTEGER) IN {3}
                        AND cast(strftime('%Y', occurrenceDate) AS INTEGER) IN {4}
                        AND coordinateUncertaintyInMeters < {6};

    /* Update the range tolerance and pad information */
    UPDATE range_polygons
    SET max_uncertainty_meters = '{6}';

    /* Recover geometry */
    SELECT RecoverGeometryColumn('range_polygons', 'range_4326', 4326,
                                 'MULTIPOLYGON', 'XY');

    SELECT RecoverGeometryColumn('range_polygons', 'occurrences_4326', 4326,
                                 'MULTIPOLYGON', 'XY');

    DETACH DATABASE requests;

    DETACH DATABASE occs;
    """.format(rng_poly_id, alias, sp_id, months, years2, years,
                max_coordUncertainty, factor, allow_holes)

    try:
        conn = sqlite3.connect(evdb)
        os.putenv('SPATIALITE_SECURITY', 'relaxed')
        conn.enable_load_extension(True)
        conn.execute('SELECT load_extension("mod_spatialite")')
        cursor = conn.cursor()
        cursor.executescript(sql)
        del cursor
        conn.close()
    except Exception as e:
        print(e)
        print(sql)

    if export == True:
        sqlExp = """
        /* Pull out the period for mapping */
        CREATE TABLE temp1 AS SELECT * FROM range_polygons
                        WHERE  alias = '{0}';

        SELECT RecoverGeometryColumn('temp1', 'range_4326', 4326,
                                     'MULTIPOLYGON', 'XY');

        SELECT RecoverGeometryColumn('temp1', 'occurrences_4326', 4326,
                                     'MULTIPOLYGON', 'XY');

        /* Export shapefiles */
        SELECT ExportSHP('temp1', 'range_4326', '{1}{0}_range', 'utf-8');

        SELECT ExportSHP('temp1', 'occurrences_4326', '{1}{0}_occs', 'utf-8');

        DROP TABLE temp1;""".format(alias, outDir)

        try:
            conn = sqlite3.connect(evdb)
            os.putenv('SPATIALITE_SECURITY', 'relaxed')
            conn.enable_load_extension(True)
            conn.execute('SELECT load_extension("mod_spatialite")')
            cursor = conn.cursor()
            cursor.executescript(sqlExp)
            conn.close()
        except:
            print(sqlExp)

    return

# Make occurrence shapefiles for each month, if migratory
month_dict = {'january': '(1)', 'february':'(2)', 'march':'(3)', 'april':'(4)',
              'may':'(5)', 'june':'(6)', 'july':'(7)', 'august':'(8)',
              'september':'(9)', 'october':'(10)', 'november':'(11)',
              'december':'(12)'}

if migratory == 1:
    for month in list(month_dict.keys()):
        print(month)
        MakeConcaveHull(rng_poly_id='rng' + month, alias=month, sp_id=sp_id,
                        months=month_dict[month],
                        years=year_range,
                        max_uncertainty=max_coordUncertainty,
                        outDir=outDir, export=True)

# Make range shapefiles for each season, display them too
period_dict = {"summer": '(5,6,7,8)',
               "winter": '(11,12,1,2)',
               "spring": '(3,4,5)',
               "fall": '(8,9,10,11)',
               "yearly": '(1,2,3,4,5,6,7,8,9,10,11,12)'}

if migratory == 1:
    for period in period_dict:
        print(period)
        MakeConcaveHull(rng_poly_id='rng' + period, alias=period, sp_id=sp_id,
                        months=period_dict[period],
                        years=year_range,
                        max_uncertainty=max_coordUncertainty,
                        outDir=outDir, export=True)
else:
    MakeConcaveHull(rng_poly_id='rng' + 'yearly', alias='yearly', sp_id=sp_id,
                    months=period_dict['yearly'],
                    years=year_range,
                    max_uncertainty=max_coordUncertainty,
                    outDir=outDir, export=True)

#############################################################################
#                    Display Seasonal Range Maps
#############################################################################
"""
season_colors = {'fall': 'red', 'winter': 'white', 'summer': 'magenta',
                    'spring': 'blue'}
for period in list(season_colors.keys()):
     title = "Yellow-billed Cuckoo occurrence polygons (1970-2018) - {0}".format(period)

    # Packages needed for plotting
    import matplotlib.pyplot as plt
    from mpl_toolkits.basemap import Basemap
    import numpy as np
    from matplotlib.patches import Polygon
    from matplotlib.collections import PatchCollection
    from matplotlib.patches import PathPatch

    shp1 = {'file': '{0}{1}_range'.format(outDir, period),
            'drawbounds': False, 'linewidth': .5, 'linecolor': 'y',
            'fillcolor': 'y'}

    # Display occurrence polygons
    map_these=[shp1]

    # Basemap
    fig = plt.figure(figsize=(12,8))
    ax = plt.subplot(1,1,1)
    map = Basemap(projection='aea', resolution='i', lon_0=-95.5, lat_0=39.5,
                  height=3400000, width=5000000)
    map.drawcoastlines(color='grey')
    map.drawstates(color='grey')
    map.drawcountries(color='grey')
    map.fillcontinents(color='green',lake_color='aqua')
    map.drawmapboundary(fill_color='aqua')

    for mapfile in map_these:
        # Add shapefiles to the map
        if mapfile['fillcolor'] == None:
            map.readshapefile(mapfile['file'], 'mapfile',
                              drawbounds=mapfile['drawbounds'],
                              linewidth=mapfile['linewidth'],
                              color=mapfile['linecolor'])
        else:
            map.readshapefile(mapfile['file'], 'mapfile',
                      drawbounds=mapfile['drawbounds'])
            # Code for extra formatting -- filling in polygons setting border
            # color
            patches = []
            for info, shape in zip(map.mapfile_info, map.mapfile):
                patches.append(Polygon(np.array(shape), True))
            ax.add_collection(PatchCollection(patches,
                                              facecolor= mapfile['fillcolor'],
                                              edgecolor=mapfile['linecolor'],
                                              linewidths=mapfile['linewidth'],
                                              zorder=2))
    fig.suptitle(title, fontsize=20)
"""

#############################################################################
#                          Insert GAP Range Map
#############################################################################
