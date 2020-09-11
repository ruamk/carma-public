CREATE TABLE "AutotekaRequest"
  ( id SERIAL PRIMARY KEY
  , ctime timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP
  , mtime timestamp
  , caseId int4 REFERENCES casetbl NOT NULL
  , userId int4 REFERENCES usermetatbl NOT NULL
  , plateNumber text NOT NULL
  , report json
  , error json
  );

CREATE INDEX ON "AutotekaRequest"(caseId);

GRANT ALL ON "AutotekaRequest" TO carma_db_sync;
GRANT ALL ON "AutotekaRequest_id_seq" TO carma_db_sync;

GRANT SELECT ON "AutotekaRequest" TO reportgen;
