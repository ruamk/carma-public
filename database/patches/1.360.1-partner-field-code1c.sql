BEGIN;

ALTER TABLE partnertbl ADD COLUMN IF NOT EXISTS code1c TEXT;

INSERT INTO "FieldPermission" (role, model, field, r, w)
  VALUES (1, 'Partner', 'code1c', 'true', 'false');

INSERT INTO "FieldPermission" (role, model, field, r, w)
  VALUES (3, 'Partner', 'code1c', 'true', 'true');

COMMIT;
