
alter table servicetbl add column partnerIsDone bool not null default false;

insert into "FieldPermission"
  (role, model, field, r, w)
  values
  (1, 'Tech', 'partnerIsDone', 't', 't'),
  (1, 'Towage', 'partnerIsDone', 't', 't');


-- normalize field orders (with a gap, so we multiply row_number by 2)
update "ConstructorFieldOption" a
  set ord = b.ord
  from (
    select
      2 * row_number() over (partition by model, program order by ord) as ord,
      model, program, field
    from "ConstructorFieldOption") as b
  where a.model = b.model
    and a.program = b.program
    and a.field = b.field;


insert into "ConstructorFieldOption"
  (model, program, ord, field, label, r, w)
  (select
      14, -- Tech
      program,
      ord + 1,
      'partnerIsDone',
      'Партнёр сообщил о выполнении услуги',
      't', 't'
    from "ConstructorFieldOption"
    where model=14 and field='partnerWarnedInTime');

insert into "ConstructorFieldOption"
  (model, program, ord, field, label, r, w)
  (select
      17,  -- Towage
      program,
      ord + 1,
      'partnerIsDone',
      'Партнёр сообщил о выполнении услуги',
      't', 't'
    from "ConstructorFieldOption"
    where model=17 and field='partnerWarnedInTime');


-- normalize field orders again by without gaps
update "ConstructorFieldOption" a
  set ord = b.ord
  from (
    select
      row_number() over (partition by model, program order by ord) as ord,
      model, program, field
    from "ConstructorFieldOption") as b
  where a.model = b.model
    and a.program = b.program
    and a.field = b.field;
