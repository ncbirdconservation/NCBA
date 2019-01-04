SELECT load_extension('mod_spatialite');
INSERT INTO occs ('occ_id', 'species_id', 'source',
                  'source_sp_id', 'coordinateUncertaintyInMeters',
                  'occurrenceDate','occurrenceYear',
                  'occurrenceMonth', 'geom_4326')
                VALUES ('2', 'bYBCUx',
                  'gbif', 2496287, 32.0,
                  '2017-05-06T00:00:00', 2017, 5,
                  GeomFromText('POINT(-83.738608 42.279341)',
                  4326))
