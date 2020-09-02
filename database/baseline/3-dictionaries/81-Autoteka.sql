CREATE TABLE "AutotekaRequest"
  ( id SERIAL PRIMARY KEY
  , ctime timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP
  , caseId int4 REFERENCES casetbl NOT NULL
  , userId int4 REFERENCES usermetatbl NOT NULL
  , plateNumber text NOT NULL
  , previewId text
  );

CREATE INDEX ON "AutotekaRequest"(caseId);

GRANT ALL ON "AutotekaRequest" TO carma_db_sync;
GRANT ALL ON "AutotekaRequest_id_seq" TO carma_db_sync;

GRANT SELECT ON "AutotekaRequest" TO reportgen;



CREATE TABLE "AutotekaResponse"
  ( id SERIAL PRIMARY KEY
  , ctime timestamp with time zone NOT NULL DEFAULT CURRENT_TIMESTAMP
  , requestId int4 REFERENCES "AutotekaRequest" NOT NULL
  , caseId int4 REFERENCES casetbl NOT NULL
  , response json NOT NULL
  );

CREATE INDEX ON "AutotekaResponse"(caseId);

GRANT ALL ON "AutotekaResponse" TO carma_db_sync;
GRANT ALL ON "AutotekaResponse_id_seq" TO carma_db_sync;

GRANT SELECT ON "AutotekaResponse" TO reportgen;
