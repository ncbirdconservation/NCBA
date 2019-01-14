#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jan 3 08:08:59 2019

author: nmtarr

Description: Draft workflow for creating spatial data summaries to support
range delineation.  This script will eventually be placed in a jupyter
notebook or separate top-level scripts.

Functionality:
Identifying a species concept to investigate
Retrieving and filtering occurrence records, saving them in a database
Generating shapefiles of occurrence records by month as well as concave hull
polygons around recordsself.

Outputs:
Maps and spatially-explicit tables within a database.

Uses:
Provide resources to consult when creating expert drawn range mapsself.
Investigation of data availability and agreement.

Unresolved issues:
1.  Downloading from Sciencebase with code.
2.  Maximize filtering.
4.  Archiving and documenting data and process.
5.  Incorporate detection distance.
6.  Can we use EPSG:5070?
7.  Incorporate allowable spatial error, which would vary by species.
"""
import pandas as pd
pd.set_option('display.width', 1000)
#%matplotlib inline
import sqlite3
from pygbif import occurrences
import os
os.chdir('/')


#############################################################################
#                               Configuration
#############################################################################
workDir = '/Users/nmtarr/Documents/RANGES'
os.chdir(workDir)
inDir = workDir + '/Inputs/'
outDir = workDir + '/Outputs/'
SRID_dict = {'WGS84': 4326, 'AlbersNAD83': 102008}
gbif_req_id = 'gbif0001'


#############################################################################
#                              Species-concept
#############################################################################
# Species to investigate
sp_id = 'bybcux0'

# Get species info from requests database
conn2 = sqlite3.connect(inDir + 'requests.sqlite')
cursor2 = conn2.cursor()
sql_tax = """SELECT gbif_id, common_name, scientific_name
             FROM species_concepts
             WHERE species_id = '{0}';""".format(sp_id)
concept = cursor2.execute(sql_tax).fetchall()[0]
gbif_id = concept[0]
common_name = concept[1]
scientific_name = concept[2]


#############################################################################
#                           Create Occurrence Database
#############################################################################
"""
Description: Create a database for storing occurrence and species-concept
data.  Needs to have spatial querying functionality.
"""
# Delete the database if it already exists
spdb = outDir + sp_id + '_occurrencess.sqlite'
if os.path.exists(spdb):
    os.remove(spdb)

# Create or connect to the database
conn = sqlite3.connect(spdb)
os.putenv('SPATIALITE_SECURITY', 'relaxed')
conn.enable_load_extension(True)
conn.execute('SELECT load_extension("mod_spatialite")')
cursor = conn.cursor()

# Make database spatial and add the spatial reference system that GAP used
conn.executescript('''SELECT InitSpatialMetaData();
             INSERT into spatial_ref_sys
             (srid, auth_name, auth_srid, proj4text, srtext)
             values (102008, 'ESRI', 102008, '+proj=aea +lat_1=20 +lat_2=60
             +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m
             +no_defs ', 'PROJCS["North_America_Albers_Equal_Area_Conic",
             GEOGCS["GCS_North_American_1983",
             DATUM["North_American_Datum_1983",
             SPHEROID["GRS_1980",6378137,298.257222101]],
             PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]],
             PROJECTION["Albers_Conic_Equal_Area"],
             PARAMETER["False_Easting",0],
             PARAMETER["False_Northing",0],
             PARAMETER["longitude_of_center",-96],
             PARAMETER["Standard_Parallel_1",20],
             PARAMETER["Standard_Parallel_2",60],
             PARAMETER["latitude_of_center",40],
             UNIT["Meter",1],AUTHORITY["EPSG","102008"]]');''')
conn.commit()

################################################################# Create tables
###############################################################################
sql_cdb = """
/* Create a table for occurrence records, WITH GEOMETRY */
CREATE TABLE IF NOT EXISTS occs (
        occ_id INTEGER NOT NULL PRIMARY KEY UNIQUE,
        species_id INTEGER NOT NULL,
        source TEXT NOT NULL,
        source_sp_id TEXT NOT NULL,
        coordinateUncertaintyInMeters INTEGER,
        occurrenceDate TEXT,
        occurrenceYear TEXT,
        occurrenceMonth TEXT,
        retrievalDate TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
        individualCount INTEGER DEFAULT 1,
            FOREIGN KEY (species_id) REFERENCES taxa(species_id)
            ON UPDATE RESTRICT
            ON DELETE NO ACTION);
SELECT AddGeometryColumn('occs', 'geom_4326', 4326, 'POINT', 'XY');

/* Make a table for storing range maps for unique species-time period
combinations, WITH GEOMETRY */
CREATE TABLE rangemaps (
             rmap_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
             species_id TEXT NOT NULL,
             period TEXT NOT NULL);
SELECT AddGeometryColumn('rangemaps', 'range', 102008, 'MULTIPOLYGON',
                          'XY');
SELECT AddGeometryColumn('rangemaps', 'circles', 102008, 'MULTIPOLYGON',
                          'XY');
"""
cursor.executescript(sql_cdb)


#############################################################################
#                            Get GBIF Records
#############################################################################
"""
Retrieve GBIF records for a species and save appropriate
attributes in the occurrence db.
"""
############################# RETRIEVE REQUEST PARAMETERS
# Up-front filters are an opportunity to lighten the load from the start.
sql_twi = """ SELECT lat_range FROM gbif_requests 
              WHERE request_id = '{0}'""".format(gbif_req_id)
latRange = cursor2.execute(sql_twi).fetchone()[0]

sql_twi = """ SELECT lon_range FROM gbif_requests 
              WHERE request_id = '{0}'""".format(gbif_req_id)
lonRange = cursor2.execute(sql_twi).fetchone()[0]

sql_twi = """ SELECT years_range FROM gbif_requests 
              WHERE request_id = '{0}'""".format(gbif_req_id)
years = cursor2.execute(sql_twi).fetchone()[0]

sql_twi = """ SELECT months_range FROM gbif_requests 
              WHERE request_id = '{0}'""".format(gbif_req_id)
months = cursor2.execute(sql_twi).fetchone()[0]

sql_twi = """ SELECT geoissue FROM gbif_requests 
              WHERE request_id = '{0}'""".format(gbif_req_id)
geoIssue = cursor2.execute(sql_twi).fetchone()[0]

sql_twi = """ SELECT coordinate_issue FROM gbif_requests 
              WHERE request_id = '{0}'""".format(gbif_req_id)
coordinate = cursor2.execute(sql_twi).fetchone()[0]

sql_twi = """ SELECT continent FROM gbif_requests 
              WHERE request_id = '{0}'""".format(gbif_req_id)
continent = cursor2.execute(sql_twi).fetchone()[0]

############################# REQUEST RECORDS ACCORDING TO REQUEST PARAMS
# First, find out how many records there are that meet criteria
occ_search = occurrences.search(gbif_id,
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

##################################################################  FILTER MORE
###############################################################################
# Remove if no coordinate uncertainty
alloccs3 = [x for x in alloccs2
            if 'coordinateUncertaintyInMeters' in x.keys()]

#  WHAT ELSE CAN WE DO ??????...

###############################################################  INSERT INTO DB
###############################################################################
# Insert the records
for x in alloccs3:
    insert1 = []
    insert1.append((x['gbifID'], sp_id, 'gbif', x['acceptedTaxonKey'],
            x['coordinateUncertaintyInMeters'], x['eventDate'],
            x['year'], x['month']))

    insert1 = tuple(insert1)[0]

    sql1 = """INSERT INTO occs ('occ_id', 'species_id', 'source',
                            'source_sp_id', 'coordinateUncertaintyInMeters',
                            'occurrenceDate','occurrenceYear',
                            'occurrenceMonth', 'geom_4326')
                VALUES {0}, GeomFromText('POINT({1} {2})',
                                            {3}))""".format(str(insert1)[:-1],
                x['decimalLongitude'], x['decimalLatitude'],
                SRID_dict[x['geodeticDatum']])
    cursor.executescript(sql1)

# Update the individual count when it exists
for e in alloccs3:
    if 'individualCount' in e.keys():
        print(e)
        sql2 = """UPDATE occs
            SET individualCount = {0}
            WHERE occ_id = {1};""".format(e['individualCount'], e['gbifID'])
        cursor.execute(sql2)

################################################################  BUFFER POINTS
###############################################################################
# Buffer the x,y locations with the coordinate uncertainty
# in order to create circles.  Create versions in albers and wgs84.  The
# wgs84 version will be used in plotting with Basemap.
sql_buf = """
/* Transform to albers (102008) and apply buffer */
ALTER TABLE occs ADD COLUMN circle_albers BLOB;

UPDATE occs SET circle_albers = Buffer(Transform(geom_4326, 102008),
                                coordinateUncertaintyInMeters);

/* The new geometry column needs an entry in the geometry columns table CAN THIS BE DONE WITH RecoverGeometryColumn???*/
INSERT INTO geometry_columns (f_table_name, f_geometry_column,
                              geometry_type, coord_dimension,
                              srid, spatial_index_enabled)
            VALUES ('occs', 'circle_albers', 3, 2, 102008, 0);

/* Transform back to WGS84 so it can be displayed in iPython */
ALTER TABLE occs ADD COLUMN circle_wgs84 BLOB;

UPDATE occs SET circle_wgs84 = Transform(circle_albers, 4326);

SELECT RecoverGeometryColumn('occs', 'circle_wgs84', 4326, 'POLYGON', 'XY');


"""
cursor.executescript(sql_buf)
conn.commit()


##################################################################  EXPORT MAPS
###############################################################################
# Export occurrence circles as a shapefile (all seasons)
cursor.execute("""SELECT ExportSHP('occs', 'circle_wgs84',
                 '{0}{1}_circles', 'utf-8');""".format(outDir, sp_id))

# Export occurrence 'points' as a shapefile (all seasons)
cursor.execute("""SELECT ExportSHP('occs', 'geom_4326', 
                                   '{0}{1}_points', 'utf-8');""".format(outDir,
                                   sp_id))

# Make occurrence shapefiles for each month
month_dict = {'january': 1, 'february':2, 'march':3, 'april':4, 'may':5,
              'june':6, 'july':7, 'august':8, 'september':9, 'october':10,
              'november':11, 'december':12}
for month in month_dict.keys():
    print(month)
    try:
        sql4 = """
        /* Create tables for each month and export as shapefiles. */

        INSERT INTO rangemaps (species_id, period, range, circles)
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


###############################################################  DISPLAY MAPS
#############################################################################
# Define a function for displaying the maps that will be created.
def MapPolygonsFromSHP(map_these, title):
    """
    Displays shapefiles on a simple CONUS basemap.  Maps are plotted in the order
    provided so put the top map last in the listself.

    NOTE: The shapefiles have to be in WGS84 CRS.

    (dict, str) -> displays maps, returns matplotlib.pyplot figure

    Arguments:
    map_these -- list of dictionaries for shapefiles you want to display in 
                CONUS. Each dictionary should have the following format:
                    {'file': '/path/to/your/shapfile', 
                    'linecolor': 'k',
                    'fillcolor': 'k', 
                    'linewidth': 1, 
                    'drawbounds': True}
    title -- title for the map.
    """
    # Packages needed for plotting
    import matplotlib.pyplot as plt
    from mpl_toolkits.basemap import Basemap
    import numpy as np
    from matplotlib.patches import Polygon
    from matplotlib.collections import PatchCollection
    from matplotlib.patches import PathPatch

    # Basemap
    fig = plt.figure(figsize=(12,8))
    ax = plt.subplot(1,1,1)
    map = Basemap(projection='aea', resolution='c', lon_0=-95.5, lat_0=39.5,
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
    
#    # Make a legend
#    handles, labels = plt.gca().get_legend_handles_labels()
#    handles.extend(['mapfile'])  
#    labels.extend(["mapfile"])                     
#    plt.legend(handles=handles, labels=labels)

    fig.suptitle(title, fontsize=20)
    return fig

##########################################################

workDir = '/Users/nmtarr/Documents/RANGES'

shp1 = {'file': '/users/nmtarr/documents/ranges/inputs/ybcu_range',
        'drawbounds': False, 'linewidth': .5, 'linecolor': 'y', 
        'fillcolor': 'y'}

shp2 = {'file': '/users/nmtarr/documents/ranges/ybcu_circles',
        'drawbounds': True, 'linewidth': .5, 'linecolor': 'k', 
        'fillcolor': None}

# Display occurrence polygons
MapPolygonsFromSHP(map_these=[shp1, shp2],
                   title="""Yellow-billed Cuckoo occurrence polygons
                           and and GAP range - any month""")

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
         MapPolygonsFromSHP([shp1, shp2], title)
     except:
         print(period + " FAILED !!!!")

###########################################################  GET THE GAP RANGES
###########################################################  FROM SCIENCEBASE

conn.commit()
conn.close()
