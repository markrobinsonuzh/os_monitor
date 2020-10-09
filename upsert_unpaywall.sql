

CREATE TEMPORARY TABLE unpaywall_copy_2 as (SELECT * FROM oa.unpaywall LIMIT 0);
CREATE TEMPORARY TABLE unpaywall_copy as (SELECT * FROM oa.unpaywall LIMIT 0);

COPY unpaywall_copy_2 FROM '/home/data_feed.csv'
DELIMITER ' ';
/* Remove dublicates */
INSERT INTO unpaywall_copy(doi, oa_status)
SELECT 
    DISTINCT ON (doi) doi,
    oa_status
FROM unpaywall_copy_2; 

DROP TABLE unpaywall_copy_2;

/*
CREATE INDEX tmp_doi_unp_idx ON unpaywall_copy USING HASH (doi);


SET search_path TO oa, public;

UPDATE unpaywall
SET oa_status = unpaywall_copy.oa_status
FROM unpaywall_copy
WHERE unpaywall.doi = unpaywall_copy.doi;


DROP INDEX doi_unique;
DROP INDEX doi_unp_idx;

INSERT INTO oa.unpaywall(doi,oa_status)
VALUES
	(SELECT *
	FROM   unpaywall_copy l
	WHERE  NOT EXISTS (
   	   SELECT  *
	   FROM   oa.unpaywall
	   WHERE  doi = l.doi
	   );
	)
;

CREATE INDEX doi_unp_idx ON oa.unpaywall USING HASH (doi);
DROP INDEX tmp_doi_unp_idx;
*/

SET work_mem = '256MB';
EXPLAIN ANALYSE INSERT INTO oa.unpaywall (doi, oa_status)
SELECT doi,oa_status 
FROM unpaywall_copy
ON CONFLICT (doi)
DO
   UPDATE SET oa_status = EXCLUDED.oa_status;

DROP TABLE unpaywall_copy;

SET work_mem TO DEFAULT;
