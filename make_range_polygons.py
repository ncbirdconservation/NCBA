#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Jan 15 11:03:56 2019

@author: nmtarr

Description: Use occurrence polygons to evaluate GAP range maps.
"""
import config
import sqlite3
import os

workDir = '/Users/nmtarr/Documents/RANGES/'
codeDir = '/Users/nmtarr/Code/Ranger/'
inDir = workDir + 'Inputs/'
outDir = workDir + 'Outputs/'

# Create or connect to the database
conn = sqlite3.connect(outDir + 'range_eval.sqlite')
os.putenv('SPATIALITE_SECURITY', 'relaxed')
conn.enable_load_extension(True)
conn.execute('SELECT load_extension("mod_spatialite")')
cursor = conn.cursor()

sql_rngy = """
        /* Make db spatial
        SELECT InitSpatialMetadata();
        
        /* Make a table for storing range maps for unique species-time period
           combinations, WITH GEOMETRY */ 
        CREATE TABLE IF NOT EXISTS range_polygons (
                     rng_polygon_id TEXT NOT NULL PRIMARY KEY,
                     alias TEXT UNIQUE,
                     species_id TEXT NOT NULL,
                     period TEXT NOT NULL,
                     months TEXT,
                     years TEXT,
                     method TEXT,
                     max_error_meters INTEGER,
                     date_created TEXT,
                     range_4326 BLOB
                     occs_4326 BLOB
                     );
        
        SELECT AddGeometryColumn('range_polygons', 'range_4326', 4326, 
                                 'MULTIPOLYGON', 'XY');
        
        SELECT AddGeometryColumn('range_polygons', 'circles_4326', 4326, 
                                 'MULTIPOLYGON', 'XY');
"""
conn.executescript(sql_rngy)
##################################################  EXPORT MAPS
###############################################################


# Make occurrence shapefiles for each month
month_dict = {'january': 1, 'february':2, 'march':3, 'april':4, 'may':5,
              'june':6, 'july':7, 'august':8, 'september':9, 'october':10,
              'november':11, 'december':12}
for month in month_dict.keys():
    print(month)
    try:
        sql4 = """
        /* Create tables for each month and export as shapefiles. */

        INSERT INTO range_polygons (species_id, period, range, circles)
                        SELECT species_id, '{0}',
                        ConcaveHull(CastToMultiPolygon(GUnion(circle_albers))),
                        CastToMultiPolygon(GUnion(circle_albers))
                        FROM occs
                        WHERE occurrenceMonth = {1};

        /* Pull out the period for mapping */
        CREATE TABLE temp1 AS SELECT * FROM rangemaps
                        WHERE period='{0}';

        SELECT RecoverGeometryColumn('temp1', 'range', 102008, 'MULTIPOLYGON',
                                     'XY');

        SELECT RecoverGeometryColumn('temp1', 'circles', 102008, 'MULTIPOLYGON',
                                     'XY');
        
        /* Transform back to WGS84 so that map can be displayed in ipython */
        ALTER TABLE temp1 ADD COLUMN range_wgs84 blob;

        SELECT RecoverGeometryColumn('temp1', 'range_wgs84', 4326,
                                     'MULTIPOLYGON');
        
        UPDATE temp1 SET range_wgs84 = Transform(range, 4326);

        ALTER TABLE temp1 ADD COLUMN circles_wgs84 blob;

        SELECT RecoverGeometryColumn('temp1', 'circles_wgs84', 4326,
                             'MULTIPOLYGON');

        UPDATE temp1 SET circles_4326 = Transform(circles, 4326);

        /* Export shapefiles */
        SELECT ExportSHP('temp1', 'range_wgs84', '{0}_rng', 'utf-8');

        SELECT ExportSHP('temp1', 'circles_wgs84', '{0}_occs', 'utf-8');

        DROP TABLE temp1;

        """.format(month, month_dict[month])
        cursor.executescript(sql4)
    except:
        print(Exception)

# Make range shapefiles for each season, display them too
period_dict = {"summer": '(5,6,7,8)', "winter": '(11,12,1,2)',
               "spring": '(3,4,5)', "fall": '(8,9,10,11)',
               "yearly": '(1,2,3,4,5,6,7,8,9,10,11,12)'}
for period in period_dict:
    print(period)
    try:
        sql_season = """
            /*  Insert a record for a range map, created by making polygons into
            a multipolygon geometry and then calculating the concave hull.
            Also, insert circles into a column for provenance */
            INSERT INTO rangemaps (species_id, period, range, circles)
                    SELECT species_id, '{0}',
                    ConcaveHull(CastToMultiPolygon(GUnion(circle_albers))),
                    CastToMultiPolygon(GUnion(circle_albers))
                    FROM occs
                    WHERE occurrenceMonth IN {1};

            /* Pull out the period for mapping */
            CREATE TABLE temp2 AS SELECT * FROM rangemaps
                            WHERE period='{0}';

            SELECT RecoverGeometryColumn('temp2', 'range', 102008,
                                         'MULTIPOLYGON',
                                         'XY');

            SELECT RecoverGeometryColumn('temp2', 'circles', 102008,
                                         'MULTIPOLYGON',
                                         'XY');

            /* Transform back to WGS84 so that map can be displayed in ipython */
            ALTER TABLE temp2 ADD COLUMN range_wgs84 blob;

            SELECT RecoverGeometryColumn('temp2', 'range_wgs84', 4326,
                                         'MULTIPOLYGON');

            UPDATE temp2 SET range_wgs84 = Transform(range, 4326);

            ALTER TABLE temp2 ADD COLUMN circles_wgs84 blob;

            SELECT RecoverGeometryColumn('temp2', 'circles_wgs84', 4326,
                                         'MULTIPOLYGON');

            UPDATE temp2 SET circles_wgs84 = Transform(circles, 4326);


            SELECT ExportSHP('temp2', 'range_wgs84', '{0}_rng', 'utf-8');

            SELECT ExportSHP('temp2', 'circles_wgs84', '{0}_occs', 'utf-8');

            DROP TABLE temp2;
        """.format(period, period_dict[period])
        cursor.executescript(sql_season)
    except:
        print(Exception)
conn.commit()


###############################################  DISPLAY MAPS
#############################################################

##########################################################

workDir = '/Users/nmtarr/Documents/RANGES'



season_colors = {'Fall': 'red', 'Winter': 'white', 'Summer': 'magenta',
                    'Spring': 'blue'}
for period in ['Fall', 'Winter', 'Summer', 'Spring']: 
     shp1 = {'file': workDir + '/{0}_rng'.format(period),
                    'drawbounds': True, 'linewidth': 1, 
                    'linecolor': season_colors[period], 
                    'fillcolor': None}
     shp2 = {'file': workDir + '/{0}_occs'.format(period),
                    'drawbounds': True, 'linewidth': .5, 'linecolor': 'k', 
                    'fillcolor': None}
     title = "Yellow-billed Cuckoo occurrence polygons - {0}".format(period)  
     try:
         config.MapPolygonsFromSHP([shp1, shp2], title)
     except:
         print(period + " FAILED !!!!")

###########################################  GET THE GAP RANGES
#############################################  FROM SCIENCEBASE