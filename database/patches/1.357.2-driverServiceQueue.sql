BEGIN;

-- проверка внесение существующей услуги, так как на servicetbl.id нельзя сослаться
-- при создании таблицы потому, что servicetbl унаследованная (inherite)
CREATE OR REPLACE FUNCTION driverServiceQueueCheckInsertServiceId()
  RETURNS TRIGGER AS $$
BEGIN
  PERFORM TRUE FROM servicetbl WHERE id = new.serviceId;
  IF FOUND THEN -- если услуга существует
    RETURN NEW;
  ELSE -- если услуги не существует
    RAISE EXCEPTION 'Invalid serviceId %', new.serviceId;
    RETURN NULL;
  END IF;
END;
$$ LANGUAGE PLPGSQL;


-- проверка при попытке поменять serviceId
CREATE OR REPLACE FUNCTION driverServiceQueueCheckUpdateServiceId()
  RETURNS TRIGGER AS $$
BEGIN
  IF new.serviceId <> old.serviceId THEN
    RAISE EXCEPTION 'Unable to change serviceId from % to %', old.serviceId, new.serviceId;
    RETURN NULL;
  ELSE
    RETURN NEW;
  END IF;
END;
$$ LANGUAGE PLPGSQL;

CREATE TABLE driverServiceQueue
  ( id SERIAL PRIMARY KEY
  , driverId INT REFERENCES "CasePartnerDrivers"(id)
  , serviceId INT -- FOREIGN KEY (serviceId) REFERENCES servicetbl(id) replaced with trigger
  , locations PATH
  , timeline TIMESTAMP WITH TIME ZONE []
  , created timestamp with time zone not null default now()
  , notified timestamp with time zone -- время когда было отправлено уведомление
  , isAccepted BOOLEAN --
  , closed timestamp with time zone
  , UNIQUE (driverId, serviceId)
  );

GRANT ALL ON TABLE driverServiceQueue TO carma_db_sync;
GRANT ALL ON TABLE driverServiceQueue_id_seq TO carma_db_sync;

CREATE TRIGGER driverServiceQueueInsertTrigger
  BEFORE INSERT 
  ON driverServiceQueue
  FOR EACH ROW
  EXECUTE PROCEDURE driverServiceQueueCheckInsertServiceId();

CREATE TRIGGER driverServiceQueueCheckUpdateServiceId
  BEFORE UPDATE
  ON driverServiceQueue
  FOR EACH ROW
  EXECUTE PROCEDURE driverServiceQueueCheckUpdateServiceId();

COMMIT;

