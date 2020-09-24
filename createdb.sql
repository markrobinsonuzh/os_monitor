CREATE SCHEMA oa;

CREATE TABLE oa.unpaywall(
	doi TEXT PRIMARY KEY,
	oa_status VARCHAR(16) 
);

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
