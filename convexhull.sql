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
CREATE TABLE hull AS
      SELECT 'ybcu' AS species,
      Transform(ConvexHull(points), 102008) AS convexhull
      FROM multi;

SELECT RecoverGeometryColumn('hull', 'convexhull',
                              102008, 'POLYGON', 'XY');

SELECT ExportSHP('hull', 'convexhull', 'ybcu_hull', 'utf-8');

DROP TABLE multi;

DROP TABLE hull
