BEGIN;

-- проверка внесения существующей услуги, так как на servicetbl.id нельзя ссылаться
-- при создании таблицы потому, что servicetbl унаследованная (inherite)
CREATE OR REPLACE FUNCTION driversPhotosCheckInsert()
  RETURNS TRIGGER AS $$
BEGIN
  PERFORM TRUE FROM servicetbl WHERE id = new.serviceId;
  IF FOUND
  THEN -- если услуга существует
    RETURN NEW;
  ELSE -- если услуги не существует
    RAISE EXCEPTION 'Invalid serviceId %', new.serviceId;
    RETURN NULL;
  END IF;
END;
$$ LANGUAGE PLPGSQL;


-- проверка при попытке поменять serviceId и блокировка изменения serviceId
CREATE OR REPLACE FUNCTION driversPhotosCheckUpdate()
  RETURNS TRIGGER AS $$
BEGIN
  IF new.serviceId = old.serviceId
  THEN
    RETURN NEW;
  ELSE
    RAISE EXCEPTION 'Unable to change serviceId from % to %', old.serviceId, new.serviceId;
    RETURN NULL;
  END IF;
END;
$$ LANGUAGE PLPGSQL;


CREATE TYPE "DriverPhotoType"
  AS ENUM ( 'After'
          , 'Before'
          , 'Difficult'
          , 'Order'
          );


CREATE TABLE driversPhotos
  ( id SERIAL PRIMARY KEY
  , driverId INT REFERENCES "CasePartnerDrivers"(id)
  , serviceId INT NOT NULL
  , coord GEOMETRY(Point, 4326) NOT NULL
  , created TIMESTAMP WITH TIME ZONE NOT NULL
  , photoType "DriverPhotoType" NOT NULL
  , filename text not null
  );

GRANT ALL ON TABLE driversPhotos TO carma_db_sync;
GRANT ALL ON SEQUENCE driversPhotos_id_seq TO carma_db_sync;

CREATE TRIGGER driversPhotosInsertTrigger
  BEFORE INSERT
  ON driversPhotos
  FOR EACH ROW
  EXECUTE PROCEDURE driversPhotosCheckInsert();

CREATE TRIGGER driversPhotosUpdateTrigger
  BEFORE UPDATE
  ON driversPhotos
  FOR EACH ROW
  EXECUTE PROCEDURE driversPhotosCheckUpdate();

COMMIT;
