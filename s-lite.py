import sqlite3
conn = sqlite3.connect('occurrences.sqlite')
cursor = conn.cursor()

sql = """
/
*  Tests of how to work with occurrence data tables
Work with occs */

/* 1.  Copy the table  */
CREATE TABLE IF NOT EXISTS copy AS SELECT * FROM occs;

/*  2.  Add a wgs84 geometry column to the copy table*/

/*  3.  Add an albers geometry column to the copy table*/

/*  4.  Filter for records from june in years between 2000 and 2010 */

/*  5.  */



"""
cursor.executescript(sql)
conn.close