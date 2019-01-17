ATTACH DATABASE '/Users/nmtarr/Documents/RANGES/Inputs/requests.sqlite'
AS requests;

/* Attach an occurrences database */
ATTACH DATABASE '/Users/nmtarr/Documents/RANGES/Outputs/bybcux0_occurrences.sqlite'
AS occs;

/* Create range maps for each month. */
INSERT INTO range_polygons (alias, species_id, months, years, method,
                            max_error_meters, pad, date_created, circles)
                SELECT 'bybcux0_april', species_id, 'fall',
                ConcaveHull(CastToMultiPolygon(GUnion(circle_albers))),
                CastToMultiPolygon(GUnion(circle_albers))
                FROM occs.occurrences
                WHERE occurrenceMonth = 4;

alias,




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
