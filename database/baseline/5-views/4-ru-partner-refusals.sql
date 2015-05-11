CREATE VIEW "Отказы партнеров" AS --ОБЪЕДИНЕНИЕ ГОРОДОВ ИЗ "Region" В ОДНУ СТРОКУ ДЛЯ ИЗБЕЖАНИЯ ЗАДВОЕНИЯ СТРОК В ОТЧЕТЕ
WITH cities_regions AS (WITH cit AS
                          (SELECT label,
                                  unnest(cities) AS cities
                           FROM "Region")
                        SELECT array_to_string(array_agg(label), ',') AS regionlist,
                               cities
                        FROM cit
                        GROUP BY cities
                        ORDER BY cities)
SELECT "PartnerCancel".id,
       --ФУНКЦИИ TIME (ВРЕМЯ ПО МОСКВЕ) НЕ РЕАЛИЗУЕТС ВО VIEW, БУДЕТ РЕАЛИЗОВАНА В JASPERSOFT STUDIO (формат DD.MM.YYYY HH24:MI)

       ctime AT TIME ZONE 'Europe/Moscow' AS "Время и дата",
                          casetbl.id AS "Номер кейса",
                          partnertbl.name AS "Партнёр",
                          --СПРАВОЧНИКА dictionaries/PartnerCancelReason.json --РЕАЛИЗОВАН, КАК UNBOUNDED

                          "PartnerRefusalReason".label AS "Причина отказа",
                          --ЗАЧЕМ ДЖОЙН С РЕГИОНОМ?
                                --"Region" AS "Регион".label,

                          "PartnerCancel".comment AS "Комментарий",
                          "City".label AS "Город",
                          "ServiceType".label AS "Услуга",
                          usermetatbl.login AS "Оператор",
                          regionlist AS "Регион",
                          CASE
                              WHEN partnertbl.isDealer THEN 'Да'::text
                              ELSE 'Нет'::text
                          END AS "Отказ дилера?",
                          "Program".label AS "Программа",
                          "SubProgram".label AS "Подпрограмма"
FROM "PartnerCancel"
LEFT JOIN "PartnerRefusalReason" ON "PartnerCancel".partnerCancelReason = "PartnerRefusalReason".id
LEFT JOIN usermetatbl ON "PartnerCancel".owner = usermetatbl.id
LEFT JOIN casetbl ON "PartnerCancel".caseid = casetbl.id
LEFT JOIN "Program" ON casetbl.program = "Program".id
LEFT JOIN "SubProgram" ON "SubProgram".id = casetbl.subprogram
LEFT JOIN partnertbl ON partnertbl.id = "PartnerCancel".partnerid
LEFT JOIN "City" ON casetbl.city = "City".id --РЕАЛИЗАЦИЯ УНИКАЛЬНОЙ СВЯЗКИ В servicetbl(id, type):

LEFT JOIN servicetbl ON ("PartnerCancel".serviceid = servicetbl.id)
LEFT JOIN "ServiceType" ON servicetbl.type = "ServiceType".id --ДЖОЙНИМ РЕЗУЛЬТАТ ПОДЗАПРОСА cities_regions ДЛЯ ВЫБОРА РЕГИОНОВ

LEFT JOIN cities_regions ON "City".id = cities_regions.cities::Integer
ORDER BY "PartnerCancel".ctime ASC,
         casetbl.id ASC;

 GRANT
SELECT ON "Отказы партнеров" TO reportgen;

 GRANT ALL ON "Отказы партнеров" TO analyst;
