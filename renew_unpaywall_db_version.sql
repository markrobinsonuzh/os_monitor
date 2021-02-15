DROP TABLE oa.unpaywall;

CREATE TABLE oa.unpaywall(
	        doi TEXT,
		oa_status VARCHAR(16),
		version VARCHAR(16),
		firstversion VARCHAR(16)
		);

CREATE TEMPORARY TABLE unpaywall_copy_2 as (SELECT * FROM oa.unpaywall LIMIT 0);
CREATE TEMPORARY TABLE unpaywall_copy as (SELECT * FROM oa.unpaywall LIMIT 0);

COPY unpaywall_copy_2 FROM '/home/snapshot_unpaywall_150221.csv'
DELIMITER ' ';
/* Remove dublicates */
INSERT INTO unpaywall_copy(doi, oa_status, version, firstversion)
SELECT
    DISTINCT ON (doi) doi,
    oa_status,
    version,
    firstversion
FROM unpaywall_copy_2;

DROP TABLE unpaywall_copy_2;


INSERT INTO oa.unpaywall (doi, oa_status, version, firstversion)
SELECT doi,oa_status,version,firstversion
FROM unpaywall_copy;

DROP TABLE unpaywall_copy;

ALTER TABLE oa.unpaywall ADD PRIMARY KEY (doi);

