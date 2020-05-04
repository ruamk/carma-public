#!/usr/bin/env bash

$PSQL -c 'create index on "Contract"(fts_key)'
