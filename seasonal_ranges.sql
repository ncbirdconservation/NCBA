/* Make a table for storing range maps for unique species-time period
combinations */
CREATE TABLE rangemaps2 (
             rmap_id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,
             species_id TEXT NOT NULL,
             period TEXT NOT NULL);
SELECT AddGeometryColumn('rangemaps2', 'range', 102008, 'MULTIPOLYGON',
                          'XY');
SELECT AddGeometryColumn('rangemaps2', 'circles', 102008,
                          'MULTIPOLYGON', 'XY');

/*  Insert a record for a range map, created by making polygons into
a multipolygon geometry and then calculating the concave hull.
Also, insert circles into a column for provenance */
INSERT INTO rangemaps2 (species_id, period, range, circles)
        SELECT species_id, 'Winter',
        ConcaveHull(CastToMultiPolygon(GUnion(circle_albers))),
        CastToMultiPolygon(GUnion(circle_albers))
        FROM occs
        WHERE occurrenceMonth IN (8,9,10,11);

CREATE TABLE winter AS SELECT * FROM rangemaps2
                        WHERE period='Winter';

SELECT RecoverGeometryColumn('winter', 'range',
                              102008, 'MULTIPOLYGON', 'XY');
SELECT RecoverGeometryColumn('winter', 'circles',
                              102008, 'MULTIPOLYGON', 'XY');

SELECT ExportSHP('winter', 'range', 'winter2_rng',
                 'utf-8');

DROP TABLE winter;
