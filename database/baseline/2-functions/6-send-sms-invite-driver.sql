BEGIN;

CREATE OR REPLACE FUNCTION
  send_sms_invite_driver (driverId integer, message text)
  RETURNS INTEGER
AS $$
DECLARE
  smsId INTEGER;
BEGIN
  INSERT INTO "Sms" (msgtext, phone, status)
    SELECT message, d.phone, 'please-send'
      FROM "CasePartnerDrivers" d
     WHERE d.id = driverId
  RETURNING "Sms".id INTO smsId;
  RETURN smsId;
END;
$$ LANGUAGE plpgsql;

COMMENT ON FUNCTION send_sms_invite_driver(integer, text) IS
  'Inserts SMS into "Sms" table with invitation for driver,'
  ' containing url to mobile application';

COMMIT;
