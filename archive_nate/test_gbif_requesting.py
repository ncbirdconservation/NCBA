
import pandas as pd
pd.set_option('display.width', 1000)
from pygbif import occurrences
import os
import config
import pprint
import sqlite3


#############################################################################
#                              Species-concept
#############################################################################
os.chdir(config.codeDir)
# Get species info from requests database
conn2 = sqlite3.connect(config.inDir + 'parameters.sqlite')
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

sql_twi = """ SELECT coordinate FROM gbif_requests
              WHERE request_id = '{0}'""".format(config.gbif_req_id)
coordinate = cursor2.execute(sql_twi).fetchone()[0]

sql_twi = """ SELECT continent FROM gbif_requests
              WHERE request_id = '{0}'""".format(config.gbif_req_id)
continent = cursor2.execute(sql_twi).fetchone()[0]


# Save table of keys that were returned
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
print(dfK)

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



#___________________________________8
test_occs = occurrences.search(gbif_id,
                                year=years,
                                month='1,12',
                                decimelLatitude=latRange,
                                decimelLongitude=lonRange,
                                hasGeospatialIssue=geoIssue,
                                hasCoordinate=True,
                                continent=continent)
occ_count=test_occs['count']
print('{0} records exist'.format(occ_count))
