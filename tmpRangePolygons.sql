
/* Attach requests database */
ATTACH DATABASE '/Users/nmtarr/Documents/RANGES/Inputs/requests.sqlite'
AS requests;

/* Attach an occurrences database */
ATTACH DATABASE '/Users/nmtarr/Documents/RANGES/Outputs/bybcux0_occurrences.sqlite'
AS occs;

/* Create range map for the period. */
INSERT INTO range_polygons (rng_polygon_id, alias, species_id,
                            months, years,
                            method, date_created, range_4326,
                            occurrences_4326)
                SELECT 'rngmarch', 'march', 'bybcux0', '(3)', '{6}',
                    'concave hull', date('now'),
                    ConcaveHull(CastToMultiPolygon(GUnion(circle_wgs84))),
                    CastToMultiPolygon(GUnion(circle_wgs84))
                FROM occs.occurrences
                WHERE cast(strftime('%m', occurrenceDate) AS INTEGER) IN (3)
                    AND cast(strftime('%Y', occurrenceDate) AS INTEGER) IN (1980, 1981, 1982, 1983, 1984, 1985,
 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005
, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017);

/* Update the range tolerance and pad information */
UPDATE range_polygons
SET max_error_meters = (SELECT error_tolerance
                        FROM requests.species_concepts
                        WHERE species_id = 'sp_id'),
    pad = (SELECT pad
           FROM requests.species_concepts
           WHERE species_id = 'sp_id')
WHERE species_id = 'sp_id';

/* Recover geometry */
SELECT RecoverGeometryColumn('range_polygons', 'range_4326', 4326,
                             'MULTIPOLYGON', 'XY');

SELECT RecoverGeometryColumn('range_polygons', 'occurrences_4326', 4326,
                             'MULTIPOLYGON', 'XY');

DETACH DATABASE requests;

DETACH DATABASE occs;
