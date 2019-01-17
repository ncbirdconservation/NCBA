ATTACH DATABASE '/Users/nmtarr/Documents/RANGES/Inputs/requests.sqlite'
AS requests;

/* Attach an occurrences database */
ATTACH DATABASE '/Users/nmtarr/Documents/RANGES/Outputs/bybcux0_occurrences.sqlite'
AS occs;

/* Create range maps for the month. */
INSERT INTO range_polygons (alias, species_id, months, years, method,
                            date_created, range_4326, occurrences_4326)
                SELECT 'bybcux0_april', 'bybcux0', '4', '1970-2018',
                'concave hull', date('now'),
                Transform(ConcaveHull(CastToMultiPolygon(GUnion(O.circle_albers))), 4326),
                Transform(CastToMultiPolygon(GUnion(O.circle_albers)), 4326)
                FROM occs.occurrences AS O
                WHERE strftime('%m', occurrenceDate) = 4;

UPDATE range_polygons
SET max_error_meters = (SELECT error_tolerance FROM requests.species_concepts WHERE species_id = 'bybcux0'),
    pad = (SELECT pad FROM requests.species_concepts WHERE species_id = 'bybcux0')
WHERE species_id = 'bybcux0';

/* Pull out the period for mapping */
CREATE TABLE temp1 AS SELECT * FROM range_polygons
                WHERE months ='4';

SELECT RecoverGeometryColumn('temp1', 'range_4326', 4326, 'MULTIPOLYGON',
                             'XY');

SELECT RecoverGeometryColumn('temp1', 'occurrences_4326', 4326, 'MULTIPOLYGON',
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
