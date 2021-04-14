BEGIN;

ALTER TABLE partnertbl ADD COLUMN IF NOT EXISTS isPartner BOOLEAN DEFAULT FALSE;
ALTER TABLE partnertbl ADD COLUMN IF NOT EXISTS legalName TEXT;
ALTER TABLE partnertbl ADD COLUMN IF NOT EXISTS inn TEXT;

INSERT INTO "FieldPermission" (role, model, field, r, w)
  VALUES (1, 'Partner', 'isPartner', 'true', 'false');
INSERT INTO "FieldPermission" (role, model, field, r, w)
  VALUES (1, 'Partner', 'legalName', 'true', 'false');
INSERT INTO "FieldPermission" (role, model, field, r, w)
  VALUES (1, 'Partner', 'inn', 'true', 'false');

INSERT INTO "FieldPermission" (role, model, field, r, w)
  VALUES (3, 'Partner', 'isPartner', 'true', 'true');
INSERT INTO "FieldPermission" (role, model, field, r, w)
  VALUES (3, 'Partner', 'legalName', 'true', 'true');
INSERT INTO "FieldPermission" (role, model, field, r, w)
  VALUES (3, 'Partner', 'inn', 'true', 'true');

COMMIT;
