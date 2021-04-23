drop type if exists "CustomerFeedbackType" cascade;
create type "CustomerFeedbackType" as enum
  ( 'FeedbackRequested'
  , 'MessageSent'
    -- data: {smsId: number}
  , 'FeedbackReceived'
    -- data: {operValue: number. techType: number, comment: string}
  , 'Comment'
    -- data: {comment: string}
  , 'ClaimResolved'
    -- data: {comment: string}
  );

drop table if exists "CustomerFeedback";
create table "CustomerFeedback"
  ( id serial primary key
  , ctime timestamp with time zone not null default current_timestamp
  , userId int4 references usermetatbl
  -- ^^ can be null if event has been initiated automatically
  , caseId int4 not null references casetbl
  , serviceId int4 -- references servicetbl
  -- ^^ foreign keys are not compatible with table inheritance
  , eventType "CustomerFeedbackType" not null
  , urlKey text
  -- ^^ this is not null only for FeedbackRequested & FeedbackReceived
  , data jsonb not null
  );


create index on "CustomerFeedback" (caseId);
create unique index on "CustomerFeedback" (caseId, serviceId)
  where eventType = 'FeedbackRequested';
-- create index on "CustomerFeedback" using brin (ctime);


create or replace function
  create_customer_feedback_request(
    _caseId integer, _serviceId integer, _userId integer)
  returns text
as $$
declare
  _urlKey text;
  _smsId integer;
  _len integer;
begin
  _len := 4;
  loop
    begin
      -- FIXME: prevent duplicate requests (caseId, serviceId)
      _urlKey := random_text(_len);
      insert into "CustomerFeedback"
        (caseId, serviceId, userId, urlKey, eventType, data) values
        (_caseId, _serviceId, _userId, _urlKey, 'FeedbackRequested', '{}');
      exit;
    exception when unique_violation then
      _len := _len + 1;
    end;
  end loop;

  insert into "Sms"(caseRef, msgtext, phone, status)
    select
      _caseId,
      'Пожалуйста оцените нашу работу: https://qc.ruamc.ru#' || _urlKey,
      c.contact_phone1, 'please-send'
      from casetbl c
      where c.id = _caseId
      returning "Sms".id into _smsId;

  insert into "CustomerFeedback"
    (caseId, serviceId, userId, urlKey, eventType, data) values
    (_caseId, _serviceId, _userId, _urlKey,
      'MessageSent',
      ('{"smsId": ' || _smsId || '}')::jsonb
    );

  return _urlKey;
end;
$$ language plpgsql volatile;


create or replace aggregate
  jsonb_object_merge(jsonb)
    ( sfunc='jsonb_concat'
    , stype=jsonb
    , initcond='{}'
  );

create or replace function
  customer_feedback_by_urlkey(_urlKey text)
  returns table (res jsonb)
as $$
  -- FIXME: use ctime of FeedbackRequested to check validity
  select jsonb_object_merge(data)
    from "CustomerFeedback"
    where eventType in ('FeedbackReceived', 'FeedbackRequested')
    group by urlKey
    having urlKey = _urlKey
$$ language sql stable;


create or replace function
  post_customer_feedback(_urlKey text, _data jsonb)
  returns table (res int4)
as $$
  insert into "CustomerFeedback"
    (urlKey, eventType, caseId, serviceId, data)
    select urlKey, 'FeedbackReceived', caseId, serviceId, _data
      from "CustomerFeedback"
      where eventType = 'FeedbackRequested'
        and ctime > now() - interval '25 hours'
        and urlKey = _urlKey
    returning id
$$ language sql stable;


create or replace view "CustomerFeedback_view" as
with events as
  (select caseId, serviceId,
    json_agg(json_strip_nulls(
      json_build_object(
        'ctime', f.ctime,
        'user',
          case when u.id is null
          then null
          else json_build_object(
            'login', u.login,
            'realName', u.realName,
            'id', u.id)
          end,
        'type', f.eventType,
        'operValue', f.data->'operValue',
        'techValue', f.data->'techValue',
        'comment', f.data->'comment'
      )
    ) order by f.ctime asc) as events
    from "CustomerFeedback" f
      left join usermetatbl u on u.id = f.userId
    group by caseId, serviceId
  )

  select
    caseId, serviceId,
    json_build_object(
      'caseId', caseId,
      'program', row_to_json(p.*),
      'service', row_to_json(s.*),
      'events', e.events
    ) as jsn
    from events e
      join lateral
        (select p.label, p.id
          from "Program" p, casetbl c
          where c.id = e.caseId and p.id = c.program) p
        on true
      left join lateral
        (select s.id, st.label as "type", st.id as "typeId"
          from servicetbl s, "ServiceType" st
          where s.id = e.serviceId and st.id = s.type) s
        on true
;



-- grant all on "CustomerFeedback" to customer_feedback_svc;
-- grant all on "CustomerFeedback_id_seq" to customer_feedback_svc;
grant all on "CustomerFeedback" to carma_db_sync;
grant all on "CustomerFeedback_view" to carma_db_sync;
grant all on "CustomerFeedback_id_seq" to carma_db_sync;
