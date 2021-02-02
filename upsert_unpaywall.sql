/*PostgreSQL script to upsert unpaywall table (not fast so far since index cannot be removed when upserting)*/
/*Temporary tables for data cleaning*/
CREATE TEMPORARY TABLE unpaywall_copy_2 as (SELECT * FROM oa.unpaywall LIMIT 0);
CREATE TEMPORARY TABLE unpaywall_copy as (SELECT * FROM oa.unpaywall LIMIT 0);
/*Read data into second temp table*/
COPY unpaywall_copy_2 FROM '/home/data_feed.csv'
DELIMITER ' ';
/* Remove dublicates */
INSERT INTO unpaywall_copy(doi, oa_status, version)
SELECT 
    DISTINCT ON (doi) doi,
    oa_status
FROM unpaywall_copy_2; 
/*Remove temporary table*/
DROP TABLE unpaywall_copy_2;
/*Set memory limit*/
SET work_mem = '256MB';
/*Upsert into unpaywall table*/
INSERT INTO oa.unpaywall (doi, oa_status, version)
SELECT doi,oa_status,version 
FROM unpaywall_copy
ON CONFLICT (doi)
DO
   UPDATE SET oa_status = EXCLUDED.oa_status, 
	      version = EXCLUDED.version;
/*Remove temporary table*/
DROP TABLE unpaywall_copy;
/*Reset memory limit*/
SET work_mem TO DEFAULT;
