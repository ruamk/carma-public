#!/usr/bin/env bash

$PSQL -f baseline/3-dictionaries/80-CustomerFeedback.sql
$PSQL -f baseline/5-views/12-case-history.sql
