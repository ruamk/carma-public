#!/usr/bin/env bash

$PSQL -c 'DROP FUNCTION IF EXISTS GetPartnerPayment(INT)' # drop old function
$PSQL -f baseline/2-functions/3-get-partner-payment.sql
