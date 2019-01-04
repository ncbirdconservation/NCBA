DROP TABLE ybcu_may
CREATE TABLE ybcu_may
        AS
        SELECT * FROM occs WHERE occurrenceMonth = 5;
        SELECT * FROM ybcu_may;
SELECT RecoverGeometryColumn('ybcu_may', 'geom_4326', 4326, 'POINT', 2);
SELECT ExportSHP('ybcu_may', 'geom_4326', 'ybcu_may', 'utf-8');
