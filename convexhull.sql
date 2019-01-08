/*  Test out convex hull functionality */

/*  Make occurrence records in occs into a single multipoint
and save in a new table */
CREATE TABLE multi AS
      SELECT 'ybcu' AS species,
      CastToMultipoint(GUnion(geom_4326)) AS points
      FROM occs;

SELECT RecoverGeometryColumn('multi', 'points',
                              4326, 'MULTIPOINT', 'XY');

SELECT ExportSHP('multi', 'points', 'ybcu_multi', 'utf-8');

/* Make the multipoint into a convex hull and export */
CREATE TABLE hull3 AS
      SELECT 'ybcu' AS species,
      Transform(ConvexHull(points), 102008) AS convexhull
      FROM multi;

SELECT RecoverGeometryColumn('hull3', 'convexhull',
                              102008, 'POLYGON', 'XY');

SELECT ExportSHP('hull3', 'convexhull', 'ybcu_hull', 'utf-8');
