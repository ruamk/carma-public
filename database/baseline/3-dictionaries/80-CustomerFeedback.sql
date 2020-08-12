
create table "CustomerFeedback"
  ( id serial primary key
  , ctime timestamp with time zone not null default current_timestamp
  , userId int4 not null references usermetatbl
  , caseId int4 not null references casetbl
  , serviceId int4 -- references servicetbl
  -- ^^ foreign keys are not compatible with table inheritance
  , value int4 not null references "Satisfaction"
  , "comment" text not null default ''
  );

create index on "CustomerFeedback" (caseId);
create index on "CustomerFeedback" using brin (ctime);

grant all on "CustomerFeedback" to carma_db_sync;
grant all on "CustomerFeedback_id_seq" to carma_db_sync;
