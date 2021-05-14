#!/bin/bash -e

(
    echo "BEGIN;"
    echo
    for i in $(seq -w 1 15);
    do
        echo "ALTER TABLE towagetbl ADD COLUMN IF NOT EXISTS towcheck$i BOOLEAN;"
        echo "INSERT INTO \"FieldPermission\" (role, model, field, r, w)"
        echo "    VALUES (1, 'Towage', 'towcheck$i', 't', 't');"
        echo "INSERT INTO \"ConstructorFieldOption\" (model, program, ord, field, label, r, w)"
        echo "    (SELECT 17, program, ord+$i, 'towcheck$i', 'towcheck$i', 'f', 'f' "
        echo "        FROM \"ConstructorFieldOption\" WHERE field = 'canNeutral');"
        echo
    done;
    echo
    echo "COMMIT;"
) | $PSQL -f -
