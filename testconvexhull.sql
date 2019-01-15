/*  Test out convex hull functionality */

/*  Make occurrence records in occs into a single multipoint
and save in a new table, then export as shapefile */
CREATE TABLE multi AS
      SELECT 'ybcu' AS species,
             CastToMultipoint(GUnion(geom_4326)) AS points
      FROM occs;

SELECT RecoverGeometryColumn('multi', 'points',
                              4326, 'MULTIPOINT', 'XY');

SELECT ExportSHP('multi', 'points', 'ybcu_multi', 'utf-8');

/* Make the multipoint into a convex hull and export */
CREATE TABLE convex AS
      SELECT 'ybcu' AS species,
      Transform(ConvexHull(points), 102008) AS convexhull
      FROM multi;

SELECT RecoverGeometryColumn('convex', 'convexhull',
                              102008, 'POLYGON', 'XY');

SELECT ExportSHP('convex', 'convexhull', 'ybcu_convex', 'utf-8');

/* Make the multipoint into a concave hull and export */
CREATE TABLE IF NOT EXISTS concave AS
      SELECT 'ybcu' AS species,
      Transform(ConcaveHull(points), 102008) AS concavehull
      FROM multi;

SELECT RecoverGeometryColumn('concave', 'concavehull',
                              102008, 'MULTIPOLYGON', 'XY');

SELECT ExportSHP('concave', 'concavehull', 'ybcu_concave', 'utf-8');

DROP TABLE multi;

DROP TABLE convex;

DROP TABLE concave;
