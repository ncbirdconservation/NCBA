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
import json


#############################################################################
#                              Species-concept
#############################################################################
os.chdir(config.codeDir)
# Get species info from requests database
conn2 = sqlite3.connect(config.codeDir + 'parameters.sqlite')
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
try:
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
except:
    print("No GAP range was retrieved.")

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
                occ_id INTEGER NOT NULL PRIMARY KEY,
                species_id INTEGER NOT NULL,
                source TEXT NOT NULL,
                request_id TEXT NOT NULL,
                filter_id TEXT NOT NULL,
                coordinateUncertaintyInMeters INTEGER,
                occurrenceDate TEXT,
                retrievalDate TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP,
                individualCount INTEGER DEFAULT 1,
                generalizations TEXT,
                remarks TEXT,
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
if geoIssue == 'None':
    geoIssue = None

sql_twi = """ SELECT coordinate FROM gbif_requests
              WHERE request_id = '{0}'""".format(config.gbif_req_id)
coordinate = cursor2.execute(sql_twi).fetchone()[0]

sql_twi = """ SELECT continent FROM gbif_requests
              WHERE request_id = '{0}'""".format(config.gbif_req_id)
continent = cursor2.execute(sql_twi).fetchone()[0]
if continent == "None":
    continent = None

#################### REQUEST RECORDS ACCORDING TO REQUEST PARAMS
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
print('{0} records exist with the request parameters'.format(occ_count))

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


######################### CREATE SUMMARY TABLE OF KEYS/FIELDS RETURNED
keys = [list(x.keys()) for x in alloccs]
keys2 = set([])
for x in keys:
    keys2 = keys2 | set(x)
dfK = pd.DataFrame(index=keys2, columns=['included(n)', 'populated(n)'])
dfK['included(n)'] = 0
dfK['populated(n)'] = 0
for t in alloccs:
    for y in t.keys():
        dfK.loc[y, 'included(n)'] += 1
        try:
            int(t[y])
            dfK.loc[y, 'populated(n)'] += 1
        except:
            if t[y] == None:
                pass
            elif len(t[y]) > 0:
                dfK.loc[y, 'populated(n)'] += 1
dfK.sort_index(inplace=True)
dfK.to_sql(name='gbif_fields_returned', con=conn, if_exists='replace')

############################# SAVE SUMMARY OF VALUES RETURNED (REQUEST)
summary = {'datums': ['WGS84'],
           'issues': set([]),
           'bases': [],
           'institutions': [],
           'collections': [],
           'generalizations': set([]),
           'remarks': set([]),
           'establishment': set([]),
           'IDqualifier': set([]),
           'protocols': set([])}

value_summaries = {'bases': {},
                  'datums': {'WGS84': 0},
                  'issues': {},
                  'institutions': {},
                  'collections': {}}

for occdict in alloccs:
    # datums
    if occdict['geodeticDatum'] != 'WGS84':
        summary['datums'] = summary['datums'] + occdict['geodeticDatum']
        if occdict['geodeticDatum'] not in value_summaries['datums'].keys():
            value_summaries['datums'][occdict['geodeticDatum']] = 0
        else:
            value_summaries['datums'][occdict['geodeticDatum']] += 1
    if occdict['geodeticDatum'] == 'WGS84':
        value_summaries['datums']['WGS84'] += 1

    # issues
    summary['issues'] = summary['issues'] | set(occdict['issues'])
    for issue in occdict['issues']:
        if issue not in value_summaries['issues'].keys():
            value_summaries['issues'][issue] = 1
        if issue in value_summaries['issues'].keys():
            value_summaries['issues'][issue] += 1

    # basis or record
    BOR = occdict['basisOfRecord']
    if BOR == "" or BOR == None:
        BOR = 'UNKNOWN'
    summary['bases'] = summary['bases'] + [BOR]

    if BOR in value_summaries['bases'].keys():
        value_summaries['bases'][BOR] += 1
    else:
        value_summaries['bases'][BOR] = 1

    # institution
    try:
        who = occdict['institutionID']
    except:
        try:
            who = occdict['institutionCode']
        except:
            who = 'unknown'

    summary['institutions'] = summary['institutions'] + [who]

    if who in value_summaries['institutions'].keys():
        value_summaries['institutions'][who] += 1
    else:
        value_summaries['institutions'][who] = 1

    # collections
    try:
        co = occdict['collectionCode']
    except:
        co = 'UNKNOWN'

    summary['collections'] = summary['collections'] + [co]
    if co in value_summaries['collections'].keys():
        value_summaries['collections'][who] += 1
    else:
        value_summaries['collections'][who] = 1

    # establishment means
    try:
        est = occdict['establishmentMeans']
        summary['establishment'] = summary['establishment'] | set([est])
    except:
        pass

    # identification qualifier
    try:
        qual = occdict['identificationQualifier']
        summary['IDqualifier'] = summary['IDqualifier'] | set([qual])
    except:
        pass

    # protocols
    try:
        proto = occdict['protocol']
        summary['protocols'] = summary['protocols'] | set([proto])
    except:
        pass
    try:
        samproto = occdict['samplingProtocol']
        summary['protocols'] = summary['protocols'] | set([samproto])
    except:
        pass

# Remove duplicates, make strings for entry into table
cursor.executescript("""CREATE TABLE post_request_attributes (field TEXT, vals TEXT);""")
for x in summary.keys():
    stmt = """INSERT INTO post_request_attributes (field, vals) VALUES ("{0}", "{1}");""".format(x, str(list(set(summary[x]))).replace('"', ''))
    cursor.execute(stmt)


##################################################  FILTER MORE
###############################################################
# Pull out relevant attributes from occurrence dictionaries.  Filtering
# will be performed with info from these keys.
keykeys = ['basisOfRecord', 'individualCount', 'acceptedTaxonKey',
           'scientificName', 'acceptedScientificName','taxonomicStatus',
           'decimalLongitude', 'decimalLatitude',
           'coordinateUncertaintyInMeters', 'year',
           'month', 'day', 'eventDate', 'issues','geodeticDatum',
           'gbifID', 'type', 'preparations', 'occurrenceStatus',
           'georeferenceProtocol', 'georeferenceVerificationStatus',
           'occurrenceID', 'dataGeneralizations', 'eventRemarks', 'locality',
           'locationRemarks', 'occurrenceRemarks', 'collectionCode',
           'protocol', 'samplingProtocol', 'institutionCode']
alloccs2 = []
for x in alloccs:
    alloccs2.append(dict((y,x[y]) for y in x if y in keykeys))

# Combine remarks FIELDS
for x in alloccs2:
    remarks = str()
    try:
        put = x['locality']
        remarks = remarks + "; " + put
    except:
        pass

    try:
        these = x['eventRemarks']
        remarks = remarks + "; " + these
    except:
        pass

    try:
        tog = x['locationRemarks']
        remarks = remarks + "; " + tog
    except:
        pass

    try:
        ether = x['occurrenceRemarks']
        remarks = remarks + ether
    except:
        pass

    try:
        x['remarks'] = remarks
    except Exception as e:
        x['remarks'] = ""

# Identify data generalizations
for x in alloccs2:
    if 'dataGeneralizations' not in x.keys():
        x['dataGeneralizations'] = ""


# HAS COORDINATE UNCERTAINTY
sql_green = """SELECT has_coordinate_uncertainty FROM gbif_filters
               WHERE filter_id = '{0}';""".format(config.gbif_filter_id)
filt_coordUncertainty = cursor2.execute(sql_green).fetchone()[0]

if filt_coordUncertainty == 1:
    alloccs3 = [x for x in alloccs2 if 'coordinateUncertaintyInMeters'
                in x.keys()]
if filt_coordUncertainty == 0:
    alloccs3 = alloccs2
del alloccs2

# MAXIMUM COORDINATE UNCERTAINTY
sql_maxcoord = """SELECT max_coordinate_uncertainty FROM gbif_filters
               WHERE filter_id = '{0}';""".format(config.gbif_filter_id)
filt_maxcoord = cursor2.execute(sql_maxcoord).fetchone()[0]
alloccs4 = []
for x in alloccs3:
    if 'coordinateUncertaintyInMeters' not in x.keys():
        alloccs4.append(x)
    elif x['coordinateUncertaintyInMeters'] <= filt_maxcoord:
        alloccs4.append(x)
    else:
        pass
del alloccs3

# COLLECTION CODES
sql_collection = """SELECT collection_codes_omit FROM gbif_filters
               WHERE filter_id = '{0}';""".format(config.gbif_filter_id)
filt_collection = cursor2.execute(sql_collection).fetchone()[0]
if type(filt_collection) == str:
    filt_collection = list(filt_collection.split(', '))
else:
    filt_collection = []

alloccs5 = []
for x in alloccs4:
    if x['collectionCode'] not in list(filt_collection):
        alloccs5.append(x)
    elif 'collectionCode' not in x.keys():
        alloccs5.append(x)
    else:
        pass
del alloccs4

# INSTITUTIONS
sql_instit = """SELECT institutions_omit FROM gbif_filters
               WHERE filter_id = '{0}';""".format(config.gbif_filter_id)
filt_instit = cursor2.execute(sql_instit).fetchone()[0]
if type(filt_instit) == str:
    filt_instit = list(filt_instit.split(', '))
else:
    filt_instit = []

alloccs6 = []
for x in alloccs5:
    if x['institutionCode'] not in list(filt_instit):
        alloccs6.append(x)
    elif 'institutionCode' not in x.keys():
        alloccs6.append(x)
    else:
        pass
del alloccs5

# BASES
sql_bases = """SELECT bases_omit FROM gbif_filters
               WHERE filter_id = '{0}';""".format(config.gbif_filter_id)
filt_bases = cursor2.execute(sql_bases).fetchone()[0]
if type(filt_bases) == str:
    filt_bases = list(filt_bases.split(', '))
else:
    filt_bases = []

alloccs7 = []
for x in alloccs6:
     if x['basisOfRecord'] not in list(filt_bases):
         alloccs7.append(x)
     elif 'basisOfRecord' not in x.keys():
         alloccs7.append(x)
     else:
         pass
del alloccs6

# PROTOCOLS
sql_protocols = """SELECT protocols_omit FROM gbif_filters
               WHERE filter_id = '{0}';""".format(config.gbif_filter_id)
filt_protocols = cursor2.execute(sql_protocols).fetchone()[0]
if type(filt_protocols) == str:
    filt_protocols = list(filt_protocols).split(', ')
else:
    filt_protocols = []

alloccs8 = []
for x in alloccs7:
    if x['protocol'] not in list(filt_protocols):
        alloccs8.append(x)
    elif 'protocol' not in x.keys():
        alloccs8.append(x)
    else:
        pass
del alloccs7

# SAMPLING PROTOCOL
sql_sampling = """SELECT sampling_protocols_omit FROM gbif_filters
               WHERE filter_id = '{0}';""".format(config.gbif_filter_id)
filt_sampling = cursor2.execute(sql_sampling).fetchone()[0]
if type(filt_sampling) == str:
    filt_sampling = list(filt_sampling.split(', '))

alloccsX = []
for x in alloccs8:
    if 'samplingProtocol' in x.keys() and x['samplingProtocol'] not in list(filt_sampling):
        alloccsX.append(x)
    elif 'samplingProtocol' not in x.keys():
        alloccsX.append(x)
    else:
        pass

############################# SAVE SUMMARY OF VALUES KEPT (FILTER)
summary2 = {'datums': ['WGS84'],
           'issues': set([]),
           'bases': [],
           'institutions': [],
           'collections': [],
           'generalizations': set([]),
           'remarks': set([]),
           'establishment': set([]),
           'IDqualifier': set([]),
           'protocols': set([])}

for occdict in alloccsX:
    # datums
    if occdict['geodeticDatum'] != 'WGS84':
        summary2['datums'] = summary2['datums'] + occdict['geodeticDatum']
    # issues
    summary2['issues'] = summary2['issues'] | set(occdict['issues'])
    # basis or record
    BOR = occdict['basisOfRecord']
    if BOR == "" or BOR == None:
        summary2['bases'] = summary2['bases'] + ["UNKNOWN"]
    else:
        summary2['bases'] = summary2['bases'] + [BOR]
    # institution
    try:
        try:
            who = occdict['institutionID']
            summary2['institutions'] = summary2['institutions'] + [who]
        except:
            who = occdict['institutionCode']
            summary2['institutions'] = summary2['institutions'] + [who]
    except:
        summary2['institutions'] = summary2['institutions'] + ['UNKNOWN']
    # collections
    try:
        co = occdict['collectionCode']
        summary2['collections'] = summary2['collections'] + [co]
    except:
        pass
    # establishment means
    try:
        est = occdict['establishmentMeans']
        summary2['establishment'] = summary2['establishment'] | set([est])
    except:
        pass
    # identification qualifier
    try:
        qual = occdict['identificationQualifier']
        summary2['IDqualifier'] = summary2['IDqualifier'] | set([qual])
    except:
        pass
    # protocols
    try:
        proto = occdict['protocol']
        summary2['protocols'] = summary2['protocols'] | set([proto])
    except:
        pass
    try:
        samproto = occdict['samplingProtocol']
        summary2['protocols'] = summary2['protocols'] | set([samproto])
    except:
        pass

# Remove duplicates, make strings for entry into table
cursor.executescript("""CREATE TABLE post_filter_attributes (field TEXT, vals TEXT);""")
for x in summary2.keys():
    stmt = """INSERT INTO post_filter_attributes (field, vals) VALUES ("{0}", "{1}");""".format(x, str(list(set(summary2[x]))).replace('"', ''))
    cursor.execute(stmt)


###############################################  INSERT INTO DB
###############################################################
# Insert the records   !needs to assess if coord uncertainty is present
# and act accordingly because insert statement depends on if it's present!
for x in alloccsX:
    try:
        if 'coordinateUncertaintyInMeters' in x.keys() and x['coordinateUncertaintyInMeters'] > 0:
            insert1 = []
            insert1.append((x['gbifID'], config.sp_id, 'gbif',
                            x['coordinateUncertaintyInMeters'], x['eventDate'],
                            config.gbif_req_id, config.gbif_filter_id,
                            x['dataGeneralizations'], x['remarks']))
        else:
            insert1 = []
            insert1.append((x['gbifID'], config.sp_id, 'gbif',
                            config.default_coordUncertainty, x['eventDate'],
                            config.gbif_req_id, config.gbif_filter_id,
                            x['dataGeneralizations'], x['remarks']))
        insert1 = tuple(insert1)[0]

        sql1 = """INSERT INTO occurrences ('occ_id', 'species_id', 'source',
                                           'coordinateUncertaintyInMeters',
                                           'occurrenceDate', 'request_id',
                                           'filter_id', 'generalizations',
                                           'remarks', 'geom_xy4326')
                    VALUES {0}, GeomFromText('POINT({1} {2})',
                                                {3}))""".format(str(insert1)[:-1],
                    x['decimalLongitude'], x['decimalLatitude'],
                    config.SRID_dict[x['geodeticDatum']])
        cursor.executescript(sql1)
    except Exception as e:
        print(e)
        print(x)

# Update the individual count when it exists
for e in alloccsX:
    if 'individualCount' in e.keys():
        sql2 = """UPDATE occurrences
            SET individualCount = {0}
            WHERE occ_id = {1};""".format(e['individualCount'], e['gbifID'])
        cursor.execute(sql2)
conn.commit()

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
