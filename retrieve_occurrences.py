"""
Retrieves GAP range from ScienceBase and occurrence records from APIs. Filters
occurrence records, stores them in a database, buffers the xy points,
and filtering occurrence records, saving them in a database.  Finally, exports
some maps.

To do:
1.  Maximize filtering.
2.  Can we use EPSG:5070?
4.  Account for possiblity of non-4326 occurrence records in gbif? Solution
    would be to transform before entering into database.
"""
import pandas as pd
pd.set_option('display.width', 1000)
import sqlite3
import sciencebasepy
from pygbif import occurrences
import os
os.chdir('/')
import config
import repo_functions as functions
import pprint


#############################################################################
#                              Species-concept
#############################################################################
os.chdir(config.codeDir)
# Get species info from requests database
conn2 = sqlite3.connect(config.inDir + 'rng_eval_params.sqlite')
cursor2 = conn2.cursor()
sql_tax = """SELECT gbif_id, common_name, scientific_name,
                    detection_distance_meters, gap_id
             FROM species_concepts
             WHERE species_id = '{0}';""".format(config.sp_id)
concept = cursor2.execute(sql_tax).fetchall()[0]
gbif_id = concept[0]
common_name = concept[1]
scientific_name = concept[2]
det_dist = concept[3]
gap_id = concept[4]


#############################################################################
#                      GAP Range Data From ScienceBase
#############################################################################
gap_range = functions.download_GAP_range_CONUS2001v1(gap_id, config.inDir)

# Reproject the GAP range to WGS84 for displaying
conn3 = sqlite3.connect(':memory:')
os.putenv('SPATIALITE_SECURITY', 'relaxed')
conn3.enable_load_extension(True)
cursor3 = conn3.cursor()
sql_repro = """
SELECT load_extension('mod_spatialite');

SELECT InitSpatialMetadata();

SELECT ImportSHP('{0}{1}_conus_range_2001v1', 'rng3', 'utf-8', 5070,
                 'geom_5070', 'HUC12RNG', 'MULTIPOLYGON');

CREATE TABLE rng2 AS SELECT HUC12RNG, seasonCode, seasonName,
                            Transform(geom_5070, 4326) AS geom_4326 FROM rng3;

SELECT RecoverGeometryColumn('rng2', 'geom_4326', 4326, 'MULTIPOLYGON', 'XY');

SELECT ExportSHP('rng2', 'geom_4326', '{0}{1}_range_4326', 'utf-8');
""".format(config.inDir, gap_id)

cursor3.executescript(sql_repro)
conn3.close()
del cursor3

gap_range2 = "{0}{1}_range_4326".format(config.inDir, gap_id)


#############################################################################
#                           Create Occurrence Database
#############################################################################
"""
Description: Create a database for storing occurrence and species-concept
data.  Needs to have spatial querying functionality.
"""
spdb = config.spdb
# Delete the database if it already exists
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


################################################# Create tables
###############################################################
sql_cdb = """
        /* Create a table for occurrence records, WITH GEOMETRY */
        CREATE TABLE IF NOT EXISTS occurrences (
                occ_id INTEGER NOT NULL PRIMARY KEY UNIQUE,
                species_id INTEGER NOT NULL,
                source TEXT NOT NULL,
                source_sp_id TEXT NOT NULL,
                request_id TEXT NOT NULL,
                filter_id TEXT NOT NULL,
                coordinateUncertaintyInMeters INTEGER,
                occurrenceDate TEXT,
                retrievalDate TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
                individualCount INTEGER DEFAULT 1,
                detection_distance INTEGER,
                radius_meters INTEGER,
                    FOREIGN KEY (species_id) REFERENCES taxa(species_id)
                    ON UPDATE RESTRICT
                    ON DELETE NO ACTION);

        SELECT AddGeometryColumn('occurrences', 'geom_xy4326', 4326, 'POINT',
                                 'XY');
"""
cursor.executescript(sql_cdb)


#############################################################################
#                              GBIF Records
#############################################################################
"""
Retrieve GBIF records for a species and save appropriate
attributes in the occurrence db.
"""
############################# RETRIEVE REQUEST PARAMETERS
# Up-front filters are an opportunity to lighten the load from the start.
sql_twi = """ SELECT lat_range FROM gbif_requests
              WHERE request_id = '{0}'""".format(config.gbif_req_id)
latRange = cursor2.execute(sql_twi).fetchone()[0]

sql_twi = """ SELECT lon_range FROM gbif_requests
              WHERE request_id = '{0}'""".format(config.gbif_req_id)
lonRange = cursor2.execute(sql_twi).fetchone()[0]

sql_twi = """ SELECT years_range FROM gbif_requests
              WHERE request_id = '{0}'""".format(config.gbif_req_id)
years = cursor2.execute(sql_twi).fetchone()[0]

sql_twi = """ SELECT months_range FROM gbif_requests
              WHERE request_id = '{0}'""".format(config.gbif_req_id)
months = cursor2.execute(sql_twi).fetchone()[0]

sql_twi = """ SELECT geoissue FROM gbif_requests
              WHERE request_id = '{0}'""".format(config.gbif_req_id)
geoIssue = cursor2.execute(sql_twi).fetchone()[0]

sql_twi = """ SELECT coordinate_issue FROM gbif_requests
              WHERE request_id = '{0}'""".format(config.gbif_req_id)
coordinate = cursor2.execute(sql_twi).fetchone()[0]

sql_twi = """ SELECT continent FROM gbif_requests
              WHERE request_id = '{0}'""".format(config.gbif_req_id)
continent = cursor2.execute(sql_twi).fetchone()[0]

#################### REQUEST RECORDS ACCORDING TO REQUEST PARAMS
# First, find out how many records there are that meet criteria
if geoissue == True or geoissue == False:
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
        occ_json = occurrences.search(gbif_id,
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

else:
    occ_search = occurrences.search(gbif_id,
                                    year=years,
                                    month=months,
                                    decimelLatitude=latRange,
                                    decimelLongitude=lonRange,
                                    hasCoordinate=coordinate,
                                    continent=continent)
    occ_count=occ_search['count']
    print('{0} records exist'.format(occ_count))

    # Get occurrences in batches, saving into master list
    alloccs = []
    batches = range(0, occ_count, 300)
    for i in batches:
        occ_json = occurrences.search(gbif_id,
                                      limit=300,
                                      offset=i,
                                      year=years,
                                      month=months,
                                      decimelLatitude=latRange,
                                      decimelLongitude=lonRange,
                                      hasCoordinate=coordinate,
                                      continent=continent)
        occs = occ_json['results']
        alloccs = alloccs + occs


# Save json of keys returned for examination later in notebooks.


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


##################################################  FILTER MORE
###############################################################

#  COORDINATE UNCERTAINTY
sql_green = """SELECT has_coordinate_uncertainty FROM gbif_filters
               WHERE filter_id = '{0}';""".format(config.gbif_filter_id)
filt_coordUncertainty = cursor2.execute(sql_green).fetchone()[0]

if filt_coordUncertainty == 1:
    alloccs3 = [x for x in alloccs2 if 'coordinateUncertaintyInMeters'
                in x.keys()]
if filt_coordUncertainty == 0:
    alloccs3 = alloccs2

# OTHERS?
########################
########################  WHAT ELSE CAN WE DO ??????...
########################  DEVELOP HERE
########################

###############################################  INSERT INTO DB
###############################################################
# Insert the records   !!!! needs to assess if coord uncertainty is present and act accordingly because insert statement depends on if it's present.
for x in alloccs3:
    if filt_coordUncertainty == 1:
        insert1 = []
        insert1.append((x['gbifID'], config.sp_id,
                        'gbif', x['acceptedTaxonKey'],
                        x['coordinateUncertaintyInMeters'], x['eventDate'],
                        config.gbif_req_id, config.gbif_filter_id))
    if filt_coordUncertainty == 0:
        insert1 = []
        insert1.append((x['gbifID'], config.sp_id,
                        'gbif', x['acceptedTaxonKey'],
                        config.default_coordUncertainty, x['eventDate'],
                        config.gbif_req_id, config.gbif_filter_id))
    insert1 = tuple(insert1)[0]

    sql1 = """INSERT INTO occurrences ('occ_id', 'species_id', 'source',
                            'source_sp_id', 'coordinateUncertaintyInMeters',
                            'occurrenceDate', 'request_id', 'filter_id',
                            'geom_xy4326')
                VALUES {0}, GeomFromText('POINT({1} {2})',
                                            {3}))""".format(str(insert1)[:-1],
                x['decimalLongitude'], x['decimalLatitude'],
                config.SRID_dict[x['geodeticDatum']])
    cursor.executescript(sql1)

# Update the individual count when it exists
for e in alloccs3:
    if 'individualCount' in e.keys():
        sql2 = """UPDATE occurrences
            SET individualCount = {0}
            WHERE occ_id = {1};""".format(e['individualCount'], e['gbifID'])
        cursor.execute(sql2)
conn.commit()


#############################################################################
#                            EBIRD Records
#############################################################################

#############################################################################

################################################  BUFFER POINTS
###############################################################
# Buffer the x,y locations with the coordinate uncertainty
# in order to create circles.  Create versions in albers and wgs84.  The
# wgs84 version will be used in plotting with Basemap.  Buffer radius is
# the sum of detectiondistance from requests.species_concepts and
# coordinate uncertainty in meters here.
requestsDB = config.inDir + 'requests.sqlite'
sql_det = """
        ATTACH DATABASE '{0}' AS requests;

        UPDATE occurrences
        SET detection_distance = {1};

        UPDATE occurrences
        SET radius_meters = detection_distance + coordinateUncertaintyInMeters;

        DETACH DATABASE requests;
""".format(requestsDB, det_dist)
cursor.executescript(sql_det)

sql_buf = """
        /* Transform to albers (102008) and apply buffer */
        ALTER TABLE occurrences ADD COLUMN circle_albers BLOB;

        UPDATE occurrences SET circle_albers = Buffer(Transform(geom_xy4326,
                                                                102008),
                                                      radius_meters);

        SELECT RecoverGeometryColumn('occurrences', 'circle_albers', 102008,
                                     'POLYGON', 'XY');

        /* Transform back to WGS84 so it can be displayed in iPython */
        ALTER TABLE occurrences ADD COLUMN circle_wgs84 BLOB;

        UPDATE occurrences SET circle_wgs84 = Transform(circle_albers, 4326);

        SELECT RecoverGeometryColumn('occurrences', 'circle_wgs84', 4326,
                                     'POLYGON', 'XY');
"""
cursor.executescript(sql_buf)


##################################################  EXPORT MAPS
###############################################################
# Export occurrence circles as a shapefile (all seasons)
cursor.execute("""SELECT ExportSHP('occurrences', 'circle_wgs84',
                 '{0}{1}_circles', 'utf-8');""".format(config.outDir,
                                                       config.summary_name))

# Export occurrence 'points' as a shapefile (all seasons)
cursor.execute("""SELECT ExportSHP('occurrences', 'geom_4326',
                  '{0}{1}_points', 'utf-8');""".format(config.outDir,
                                                       config.summary_name))
conn.commit()
conn.close()
conn2.commit()
conn2.close()
