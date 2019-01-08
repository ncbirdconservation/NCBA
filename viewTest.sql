DROP VIEW IF EXISTS ybcu_april;
CREATE VIEW ybcu_april AS
        SELECT * FROM occs WHERE occurrenceMonth = 4;
/*INSERT INTO views_geometry_columns
            (view_name, view_geometry, view_rowid, f_table_name,
              f_geometry_column, read_only)
              VALUES
             ('ybcu_april', 'geom_4326', 'occ_id', 'occs', 'geom_4326',
             1);*/
SELECT ExportSHP('ybcu_april', 'circles_albers', 'ybcu_april',
  'utf-8');
