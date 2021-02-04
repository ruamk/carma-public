#! /usr/bin/env bash

$PSQL -f baseline/3-dictionaries/80-CasePartner.sql
$PSQL -f baseline/3-dictionaries/82-CasePartnerDrivers.sql
$PSQL -f baseline/1-tables/12-driverServiceQueue.sql
$PSQL -f baseline/2-functions/6-send-sms-invite-driver.sql
