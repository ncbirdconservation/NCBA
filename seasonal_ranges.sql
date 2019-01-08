/* Make a table for storing range maps for unique species-time period
combinations */
CREATE TABLE rangemaps2 (
             species_id TEXT NOT NULL,
             period TEXT NOT NULL);
SELECT AddGeometryColumn('rangemaps2', 'range', 102008, 'POLYGON',
                          'XY');
SELECT AddGeometryColumn('rangemaps2', 'circles', 102008, 'MULTIPOLYGON',
                          'XY');

/*  Insert a record for a range map, created by making polygons into
a multipolygon geometry and then calculating the concave hull.
Also, insert circles into a column for provenance */
INSERT INTO rangemaps2 (species_id, period, range, circles)
        SELECT species_id, 'Winter',
        ConcaveHull(CastToMultiPolygon(GUnion(circle_albers))),
        CastToMultiPolygon(GUnion(circle_albers))
        FROM occs
        WHERE occurrenceMonth IN (8,9,10,11);

SELECT ExportSHP('rangemaps2', 'range', 'winter2_rng', 'utf-8');
SELECT ExportSHP('rangemaps2', 'circles', 'winter2_occs', 'utf-8');


SELECT species_id, 'oct',
                    ConcaveHull(CastToMultiPolygon(GUnion(circle_albers))),
                    CastToMultiPolygon(GUnion(circle_albers))
                    FROM occs
                    WHERE occurrenceMonth = 10;
