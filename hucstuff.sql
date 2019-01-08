/*  Test out stuff with hucs */

/*  Select hucs that overlap with occurrence circles */
CREATE TABLE orange AS
              SELECT shucs.HUC12RNG, shucs.geom_102008
              FROM shucs, occs
              WHERE Overlaps(shucs.geom_102008, occs.circle_albers)
                OR Contains(occs.circle_albers, shucs.geom_102008);
SELECT RecoverGeometryColumn('orange', 'geom_102008', 102008,
                              'POLYGON');

SELECT ExportSHP('orange', 'geom_102008', 'shuctest1', 'utf-8');

/*  Select hucs that contain a circle, they can be used to validate
the range map (assuming total confidence in the points).  What
to do about hucs that contain part of a circle? */
CREATE TABLE blue AS
              SELECT shucs.HUC12RNG, shucs.geom_102008
              FROM shucs, occs
              WHERE Contains(shucs.geom_102008, occs.circle_albers);
SELECT RecoverGeometryColumn('blue', 'geom_102008', 102008,
                              'POLYGON');
SELECT ExportSHP('blue', 'geom_102008', 'shuctest2', 'utf-8');

/* Store records from blue in a table (blue?) that records whether
it agrees with the the GAP range occurrence attribute.  */
