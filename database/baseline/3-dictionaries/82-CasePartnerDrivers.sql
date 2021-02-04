CREATE TABLE IF NOT EXISTS "CasePartnerDrivers"
  ( id SERIAL PRIMARY KEY
  , partner INT REFERENCES usermetatbl(id)
  , phone TEXT NOT NULL UNIQUE -- username
  , name TEXT NOT NULL
  , password TEXT -- sha256
  , plateNum TEXT
  , isActive BOOLEAN NOT NULL DEFAULT FALSE
  , chatId INT UNIQUE
  , cookie TEXT
  , currentServiceId INT -- чтобы быстрее искать текущую заявку для водителя
  );

GRANT ALL ON TABLE "CasePartnerDrivers" TO carma_db_sync;
GRANT ALL ON "CasePartnerDrivers_id_seq" TO carma_db_sync;
