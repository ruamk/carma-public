begin;


insert into "FieldPermission"
(role, model, field, r, w)
( select
    role.id, 'Case', 'car_detailsFromAutoteka', true, false
    from "Role" as role where value = 'core'
    limit 1
);


-- add `car_detailsFromAutoteka` after `car_class` field in all the programs
with
  params as (
    select model, program, ord+1 as ord
      from "ConstructorFieldOption" c, "CtrModel" m
      where m.value = 'Case'
        and c.model = m.id
        and field = 'car_class'
  )
  insert into "ConstructorFieldOption"
    (model, program, ord, field, r)
    (select
        model, program, ord, 'car_detailsFromAutoteka', true
      from params
    );


-- update ord = ord + 1 for all the fields after the inserted one
with
  params as (
    select m.id as caseModel, program, ord as prevOrd
      from "ConstructorFieldOption" c, "CtrModel" m
      where m.value = 'Case'
        and c.model = m.id
        and field = 'car_detailsFromAutoteka'
  )
  update "ConstructorFieldOption" c
    set ord = ord + 1
    from params p
    where field <> 'car_detailsFromAutoteka'
      and ord >= prevOrd
      and c.program = p.program
      and model = caseModel;

commit;
