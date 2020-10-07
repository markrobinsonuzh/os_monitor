

CREATE TEMPORARY TABLE unpaywall_copy as (SELECT * FROM oa.unpaywall LIMIT 0);

COPY unpaywall_copy FROM '/home/data_feed.csv'
DELIMITER ' ';


INSERT INTO oa.unpaywall (doi, oa_status)
SELECT doi,oa_status 
FROM unpaywall_copy
ON CONFLICT (doi)
DO
   UPDATE SET oa_status = EXCLUDED.oa_status;

DROP TABLE unpaywall_copy;
