BEGIN;

ALTER TABLE servicetbl ADD COLUMN IF NOT EXISTS payment_partnerCostTranscript TEXT;
ALTER TABLE servicetbl ADD COLUMN IF NOT EXISTS payment_checkCost DOUBLE PRECISION;
ALTER TABLE servicetbl ADD COLUMN IF NOT EXISTS payment_checkCostTranscript TEXT;
ALTER TABLE servicetbl ADD COLUMN IF NOT EXISTS payment_claimsCost DOUBLE PRECISION;
ALTER TABLE servicetbl ADD COLUMN IF NOT EXISTS payment_claimsCostTranscript TEXT;

-- ###
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

-- payment_partnerCostTranscript
INSERT INTO "FieldPermission"
  (role, model, field, r, w)
  (SELECT 10, name, 'payment_partnerCostTranscript', 't', 't'
   FROM temp_models
  );


INSERT INTO "ConstructorFieldOption"
  (model, program, ord, field, label, r, w)
  (SELECT model, program, ord+1
        , 'payment_partnerCostTranscript', 'Расшифровка стоимости со слов партнёра'
        , r, w
     FROM "ConstructorFieldOption"
    WHERE field = 'payment_partnerCost'
  );

-- payment_checkCost
INSERT INTO "FieldPermission"
  (role, model, field, r, w)
  (SELECT 10, name, 'payment_checkCost', 't', 't'
   FROM temp_models
  );


INSERT INTO "ConstructorFieldOption"
  (model, program, ord, field, label, r, w)
  (SELECT model, program, ord+2
        , 'payment_checkCost', 'Стоимость при закрытии (число)'
        , r, w
     FROM "ConstructorFieldOption"
    WHERE field = 'payment_partnerCost'
  );

-- payment_checkCostTranscript
INSERT INTO "FieldPermission"
  (role, model, field, r, w)
  (SELECT 10, name, 'payment_checkCostTranscript', 't', 't'
   FROM temp_models
  );


INSERT INTO "ConstructorFieldOption"
  (model, program, ord, field, label, r, w)
  (SELECT model, program, ord+3
        , 'payment_checkCostTranscript', 'Расшифровка стоимости при закрытии'
        , r, w
     FROM "ConstructorFieldOption"
    WHERE field = 'payment_partnerCost'
  );

-- payment_claimsCost
INSERT INTO "FieldPermission"
  (role, model, field, r, w)
  (SELECT 10, name, 'payment_claimsCost', 't', 't'
   FROM temp_models
  );


INSERT INTO "ConstructorFieldOption"
  (model, program, ord, field, label, r, w)
  (SELECT model, program, ord+4
        , 'payment_claimsCost', 'Обработка счёта (число)'
        , r, w
     FROM "ConstructorFieldOption"
    WHERE field = 'payment_partnerCost'
  );

-- payment_claimsCostTranscript
INSERT INTO "FieldPermission"
  (role, model, field, r, w)
  (SELECT 10, name, 'payment_claimsCostTranscript', 't', 't'
   FROM temp_models
  );


INSERT INTO "ConstructorFieldOption"
  (model, program, ord, field, label, r, w)
  (SELECT model, program, ord+5
        , 'payment_claimsCostTranscript', 'Расшифровка обработки счёта'
        , r, w
     FROM "ConstructorFieldOption"
    WHERE field = 'payment_partnerCost'
  );



COMMIT;
