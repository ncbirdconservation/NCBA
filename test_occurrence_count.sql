ATTACH DATABASE '/users/nmtarr/documents/ranges/outputs/bybcux0_occurrences.sqlite'
AS occs;


SELECT 'concave hull', date('now'),
              CASE
              WHEN (SELECT COUNT(circle_wgs84)
                    FROM occs.occurrences
                    WHERE cast(strftime('%m', occurrenceDate) AS INTEGER) IN (11,12,1,2,3,4,5)
                            AND cast(strftime('%Y', occurrenceDate) AS INTEGER) IN (2016, 2017, 2018, 2015, 2014, 2013, 2012, 2011))
                    > 10 THEN ConcaveHull(CastToMultiPolygon(GUnion(circle_wgs84)))
              ELSE 'Insufficient data'
              END range_4326,
              CastToMultiPolygon(GUnion(circle_wgs84))
          FROM occs.occurrences
          WHERE cast(strftime('%m', occurrenceDate) AS INTEGER) IN (11,12,1,2,3,4,5)
                  AND cast(strftime('%Y', occurrenceDate) AS INTEGER) IN (2016, 2017, 2018, 2015, 2014, 2013, 2012, 2011);
