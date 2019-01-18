/* Attach an requests database */
ATTACH DATABASE '/Users/nmtarr/Documents/RANGES/Inputs/requests.sqlite'
AS requests;

/* Attach an occurrences database */
ATTACH DATABASE '/Users/nmtarr/Documents/RANGES/Outputs/bybcux0_occurrences.sqlite'
AS occs;

/* Create range map for the period. */
INSERT INTO range_polygons (rng_polygon_id, alias, species_id, months, years,
                            method, date_created, range_4326, occurrences_4326)
                SELECT 'rng2', 'bybcux0_any2', 'bybcux0', 'any_month',
                       '1970-2018', 'concave hull', date('now'),
                        ConcaveHull(CastToMultiPolygon(GUnion(O.circle_wgs84))),
                        CastToMultiPolygon(GUnion(O.circle_wgs84))
                FROM occs.occurrences AS O
                WHERE cast(strftime('%m', occurrenceDate) AS INT) IN (1,2,3,4,
                                                                      5,6,7,8,
                                                                      9,10,11,
                                                                      12);
/* Update the range tolerance and pad information */
UPDATE range_polygons
SET max_error_meters = (SELECT error_tolerance FROM requests.species_concepts WHERE species_id = 'bybcux0'),
    pad = (SELECT pad FROM requests.species_concepts WHERE species_id = 'bybcux0')
WHERE species_id = 'bybcux0';

/* Recover geometry */
SELECT RecoverGeometryColumn('range_polygons', 'range_4326', 4326, 'MULTIPOLYGON',
                           'XY');

SELECT RecoverGeometryColumn('range_polygons', 'occurrences_4326', 4326, 'MULTIPOLYGON',
                           'XY');


/* Pull out the period for mapping */
CREATE TABLE temp1 AS SELECT * FROM range_polygons
                WHERE  alias = 'bybcux0_any2';

SELECT RecoverGeometryColumn('temp1', 'range_4326', 4326, 'MULTIPOLYGON',
                             'XY');

SELECT RecoverGeometryColumn('temp1', 'occurrences_4326', 4326, 'MULTIPOLYGON',
                             'XY');

/* Export shapefiles */
SELECT ExportSHP('temp1', 'range_4326',
                 '/users/nmtarr/documents/ranges/outputs/bybcux0_any2_range', 'utf-8');

SELECT ExportSHP('temp1', 'occurrences_4326',
                 '/users/nmtarr/documents/ranges/outputs/bybcux0_any2_occs', 'utf-8');

DROP TABLE temp1;

DETACH DATABASE requests;

DETACH DATABASE occs;
