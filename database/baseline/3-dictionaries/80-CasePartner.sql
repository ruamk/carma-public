CREATE TABLE IF NOT EXISTS "CasePartner" (
  uid int references snap_auth_user(uid)
, partner int references partnertbl(id)
, unique(uid, partner)
);

GRANT ALL ON TABLE "CasePartner" TO carma_db_sync;
