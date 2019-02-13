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

#############################################################################
#                            EBIRD Records
#############################################################################

#############################################################################
