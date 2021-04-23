#!/usr/bin/env bash

$PSQL -f baseline/3-dictionaries/84-CustomerFeedback.sql
$PSQL -f baseline/5-views/12-case-history.sql

# bump-bump
