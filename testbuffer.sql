ALTER TABLE occs ADD COLUMN buff_albers3 BLOB;

UPDATE occs SET buff_albers3 = Buffer(Transform(geom_4326, 102008),
                                coordinateUncertaintyInMeters);

INSERT INTO geometry_columns (f_table_name, f_geometry_column,
                              geometry_type, coord_dimension,
                              srid, spatial_index_enabled)
            VALUES ('occs', 'buff_albers3', 3, 2, 102008, 0);

SELECT ExportSHP('occs', 'buff_albers3', 'ybcu_circle3', 'utf-8');
