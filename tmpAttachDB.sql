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
                ConcaveHull(CastToMultiPolygon(GUnion(O.circle_wgs84))),
                CastToMultiPolygon(GUnion(O.circle_wgs84))
                FROM occs.occurrences AS O
                WHERE strftime('%m', occurrenceDate) = 4;

SELECT RecoverGeometryColumn('range_polygons', 'range_4326', 4326, 'MULTIPOLYGON',
                           'XY');

SELECT RecoverGeometryColumn('range_polygons', 'occurrences_4326', 4326, 'MULTIPOLYGON',
                           'XY');


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

/* Export shapefiles */
SELECT ExportSHP('temp1', 'range_4326',
                 '/users/nmtarr/documents/ranges/outputs/april_rng', 'utf-8');

SELECT ExportSHP('temp1', 'occurrences_4326',
                 '/users/nmtarr/documents/ranges/outputs/april_occs', 'utf-8');

DROP TABLE temp1;

DETACH DATABASE requests;

DETACH DATABASE occs;
