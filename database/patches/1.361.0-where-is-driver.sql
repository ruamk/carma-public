BEGIN;

-- If trackLocation contains NULL, then mobile application should not send location
-- otherwise, it contains location update interval in seconds.
-- Invalid interval value (< 30 seconds) should be processed in backend.
ALTER TABLE "CasePartnerDrivers" ADD COLUMN trackLocation INTEGER DEFAULT NULL;

-- Number of days to keep location in database.
-- Invalid values (null or < 1) should be processed in backend.
ALTER TABLE "CasePartnerDrivers" ADD COLUMN locationKeepInterval INTEGER NOT NULL DEFAULT 3;

-- Contains car attributes like: color, car model, etc.
ALTER TABLE "CasePartnerDrivers" ADD COLUMN carInfo JSONB DEFAULT '{}';

COMMIT;

