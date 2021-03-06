CREATE TABLE "Part"
  ( id SERIAL PRIMARY KEY
  , parent int4 REFERENCES "System" NOT NULL
  , label text NOT NULL CHECK (label <> '')
  , fdds text
  , UNIQUE (label, parent)
  );

GRANT ALL ON "Part" TO carma_db_sync;
GRANT ALL ON "Part_id_seq" TO carma_db_sync;
