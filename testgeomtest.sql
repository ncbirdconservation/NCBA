CREATE TABLE test_geom2 (
                        id INTEGER NOT NULL
                            PRIMARY KEY AUTOINCREMENT,
                        name TEXT NOT NULL,
                        measured_value DOUBLE NOT NULL);

SELECT AddGeometryColumn('test_geom2', 'the_geom',
                          4326, 'POINT', 'XY');

INSERT INTO test_geom2 (id, name, measured_value, the_geom)
              VALUES (NULL, 'three', 1.23456,
                      GeomFromText('POINT(83.738608 42.279341)', 4326));
