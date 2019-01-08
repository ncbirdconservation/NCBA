/* Create views for each season that can then be used to create
    occurrence-derived range maps. */
CREATE TABLE summer AS
                        SELECT occ_id, species_id, circle_albers
                        FROM occs
                        WHERE occurrenceMonth IN (5, 6, 7);

SELECT RecoverGeometryColumn('summer', 'circle_albers',
                            102008, 'POLYGON', 'XY');

/* Make a table for storing range maps for unique species-time period
combinations */
CREATE TABLE rangemaps (
             species_id TEXT NOT NULL,
             period TEXT NOT NULL);

SELECT AddGeometryColumn('rangemaps', 'range', 102008, 'MULTIPOLYGON',
                          'XY');

/*  Insert a record for a range map, created by making polygons into
a multipolygon geometry and then calculating the concave hull */
SELECT ConcaveHull(CastToMultiPolygon(GUnion(circle_albers)))
FROM occs
WHERE occurrenceMonth IN (5,6,7);

INSERT INTO rangemaps (species_id, period, range)
        SELECT species_id, 'summer',
        ConcaveHull(CastToMultiPolygon(GUnion(circle_albers)))
        FROM occs
        WHERE occurrenceMonth IN (5,6,7);

SELECT ExportSHP('rangemaps', 'range', 'summerrange', 'utf-8');
