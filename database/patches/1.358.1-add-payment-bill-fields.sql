-- Rename some fields:
-- bill_billNumber  -> payment_billNumber
-- bill_billingCost -> payment_billingCost
-- bill_billingDate -> payment_billingDate

BEGIN;
ALTER TABLE servicetbl ADD COLUMN IF NOT EXISTS payment_billNumber TEXT;
ALTER TABLE servicetbl ADD COLUMN IF NOT EXISTS payment_billingDate DATE;


DROP TABLE IF EXISTS temp_models;

CREATE TEMPORARY TABLE temp_models(id serial primary key, name text unique not null);
INSERT INTO temp_models (id, name)
  (SELECT id, value
   FROM "CtrModel"
   WHERE value IN
    ( 'AverageCommissioner'
    , 'Bank'
    , 'BikeTowage'
    , 'Consultation'
    , 'Continue'
    , 'DeliverCar'
    , 'DeliverClient'
    , 'DeliverParts'
    , 'Hotel'
    , 'Information'
    , 'LegalAssistance'
    , 'Rent'
    , 'Service'
    , 'SoberDriver'
    , 'Taxi'
    , 'Tech'
    , 'TechInspect'
    , 'Tickets'
    , 'Towage'
    , 'Transportation'
    )
  );
INSERT INTO temp_models (name) VALUES ('Service');


-- payment_billNumber
INSERT INTO "FieldPermission"
  (role, model, field, r, w)
  (SELECT 9, name, 'payment_billNumber', 't', 't'
   FROM temp_models
  );


INSERT INTO "ConstructorFieldOption"
  (model, program, ord, field, label, r, w)
  (SELECT model, program, ord+1
        , 'payment_billNumber', 'Номер счёта'
        , r, w
     FROM "ConstructorFieldOption"
    WHERE field = 'payment_partnerCost'
  );

-- payment_billingDate
INSERT INTO "FieldPermission"
  (role, model, field, r, w)
  (SELECT 9, name, 'payment_billingDate', 't', 't'
   FROM temp_models
  );


INSERT INTO "ConstructorFieldOption"
  (model, program, ord, field, label, r, w)
  (SELECT model, program, ord+1
        , 'payment_billingDate', 'Дата выставления счёта'
        , r, w
     FROM "ConstructorFieldOption"
    WHERE field = 'payment_partnerCost'
  );

-- TODO: во view "Услуги"
-- consultationtbl
-- tech1tbl
COMMIT;


