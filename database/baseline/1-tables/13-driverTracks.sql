-- driverTrack предназначена для временного хранения
-- системы постоянного отслеживания водителей партнёров по заявкам
CREATE TABLE driverTracks
  ( id SERIAL PRIMARY KEY
  , driverId INT REFERENCES "CasePartnerDrivers"(id)
  , created DATE NOT NULL DEFAULT CURRENT_DATE
  , locations PATH
  , timeline TIMESTAMP WITH TIME ZONE []
  , UNIQUE (driverId, created)
  );

GRANT ALL ON driverTracks TO carma_db_sync;

CREATE OR REPLACE FUNCTION
       save_drivers_track (driver INT, lat FLOAT, lon FLOAT)
       RETURNS VOID
AS $$
DECLARE
  existingDriverId INT;
BEGIN
  PERFORM TRUE FROM "CasePartnerDrivers" WHERE id = driver AND isActive;
  IF FOUND THEN
    SELECT id
      FROM driverTracks
      INTO existingDriverId
     WHERE driverId = driver AND created = CURRENT_DATE;
    IF existingDriverId IS NULL THEN
      INSERT INTO driverTracks (driverId, created, locations, timeline)
           VALUES ( driver
                  , CURRENT_DATE
                  , PATH('[(' || lat::text || ',' || lon::text || ')]')
                  , ARRAY[now()]
                  );
    ELSE
      UPDATE driverTracks
         SET locations = locations + PATH('[(' || lat::text || ',' || lon::text || ')]')
           , timeline = ARRAY_APPEND(timeline, now())
       WHERE id = existingDriverId;
    END IF;
  END IF;
  RETURN;
END;
$$ LANGUAGE PLPGSQL;

