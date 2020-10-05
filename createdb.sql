CREATE SCHEMA oa;

CREATE TABLE oa.unpaywall(
	doi TEXT PRIMARY KEY,
	oa_status VARCHAR(16) 
);

CREATE UNIQUE INDEX CONCURRENTLY doi_con_idx ON oa.unpaywall(doi);

ALTER TABLE oa.unpaywall DROP CONSTRAINT unpaywall_pkey,
	ADD CONSTRAINT unpaywall_pkey PRIMARY KEY USING INDEX doi_con_idx;

CREATE INDEX doi_unp_idx ON oa.unpaywall USING HASH (doi);
ALTER TABLE oa.unpaywall
ADD unpaywall_pkey PRIMARY KEY USING INDEX doi_unp_idx;



CREATE TABLE oa.authorkeys(
	authorkey_fullname TEXT PRIMARY KEY,
	authorkey TEXT,
	authorname TEXT
);

CREATE TABLE oa.eprints(
	eprintid INT PRIMARY KEY,
	doi TEXT,
	date CHAR(4),
	title TEXT,
	type VARCHAR(255),
	refereed VARCHAR(12),
	institution TEXT,
	oa_status VARCHAR(16),
	published_doc BOOLEAN

);

CREATE TABLE oa.subjects(
	eprintid INT,
	subjects VARCHAR(16),
        name TEXT,
        parent VARCHAR(16),
        parent_name TEXT,
	FOREIGN KEY (eprintid)
		REFERENCES oa.eprints(eprintid)
);

CREATE TABLE oa.authors(
	eprintid INT NOT NULL,
	authorkey_fullname TEXT,
	FOREIGN KEY (authorkey_fullname)
		REFERENCES oa.authorkeys(authorkey_fullname),
	FOREIGN KEY (eprintid)
		REFERENCES oa.eprints(eprintid)
);
