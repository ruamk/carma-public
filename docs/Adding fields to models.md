### What If I Want to Add a Field to Some Form?

Adding a new field is not a simple task as it involves multiple steps and may
silently go wrong at any of them.

First, let's dive a bit into internal workings of data model handling in CaRMa.


## Internals

We have multiple layers of data representation in CaRMa:
  - Tables in PostgreSQL
  - Algebraic Data Types in Haskell code
  - JSON with metadata in the frontend code

# DB Schema and Patches

We tried to store database schema in repo to be able to track changes over time.
Database scripts are split in to two parts:
  - **database/baseline** represents current state of database schema
  - **database/patches** contains series of sql/sh scripts to update schema
    (and sometimes data also).

The system was designed to allow bootstrapping the database from those scripts
but, sadly, this has not been tested for too long.


# Schema Representation in Haskell

Haskell data models are at the core of the backend. They were invented to
provide type safety for the hairy business-logic.

You can get around by walking down the tree from `carma-models/src/Carma/Model`.
There are two particularly interesting models: Case and Services.

**Case** model is so HUGE that it was split in two files:
  - `carma-models/src/Carma/Model/Case/Type.hs` − contains only field
    definitions
  - `carma-models/src/Carma/Model/Case.hs` -- contains meta definitions for
    different model views.

**Service** model (a bunch of models actually). Those are represented using
table inheritance in the PostgreSQL (not sure if it was a mistake or an
enlightment).

  - `carma-models/src/Carma/Model/Service.hs` − common service fields
  - `carma-models/src/Carma/Model/Service/*.hs` − service specific fields

There was also an attempt to migrate to *Database.Persist* but the process stalled
at the very beginning. Some artifacts like
`carma-models/src/Carma/Model/Case/Persistent.hs` remind us about this.


# Data and Metadata in Frontend

To render CRUD forms, frontend has to get model descriptions (meta) from the
backend.


## Adding a field

Now, knowing all the whys, we finally get to the "how" part of the document.

# Update carma-models

Add required field to the corresponding model like this

```
  partnerWarnedInTime :: F (Maybe Bool) "partnerWarnedInTime" "Партнёр предупредил вовремя"
```

Here we have:
  - `partnerWarnedInTime` − field accessor to be used in Haskell code
  - `Maybe Bool` − field type (must be compatible with the field type in
    the database)
  - `"partnerWarnedInTime"` − column name in the database table
  - `"Партнёр предупредил вовремя"` − default field description that can be
    overriden in form constructor (see `ConstructorFieldOption` table)

Update `modelView` if required

```
  TODO: XXX
```

# Update database baseline

Update corresponding table DML in `database/baseline/1-tables` or
`database/baseline/3-dictionaries`.

Suddenly, most important tables like `casetbl` and services are not tracked
in the `database/baseline`.

# Add a database patch

Database patch is a `.sh` or `.sql` file. The file name must start with a fresh
version number.

When adding fields, it is easier to use `.sql` patches (leaving `.sh` for more
complicated cases).

There are several tasks for the patch:
  - add column to the table
  - update `FieldPermissions`
  - update `ConstructorFieldOption` if updated model is managed by constructor

```
ALTER TABLE techtbl ADD COLUMN compl27p1 bool;
INSERT INTO "FieldPermission"
  (role, model, field, r, w)
  VALUES
  (1, 'Tech', 'compl27p1', 't', 't');

INSERT INTO "ConstructorFieldOption"
  (model, program, ord, field, label, r, w)
  (SELECT 14, program, ord + 1, 'compl27p1', 'Двери открываются', 't', 't'
    FROM "ConstructorFieldOption"
    WHERE model=14 AND field='isCountryRide');
```

# Update database baseline version

Don't forget to update `database/baseline/9-version.sql` with the current
schema version.


## Troubleshooting
  - Check user role
  - Check if there are corresponding permission in `FieldPermissions` table


## Advanced topics

- explain how to Group fields like "Client"
- explain View modificators in models
- explain how to support new field types (Field instance)
- Multiple views per model
- Enumerate all (or most common) view-modifiers.
- How constructor works
  - user roles: visibility, write permissions
