Iolaus.Database
===============

For more details, including a tutorial, please see the `example.hs`
file that is part of this distribution.

Running the Example
-------------------

To run the example application you first need to set two environment
variables:

  * `DB_CONN`: The PostgreSQL database connection string.  Create a
    database with any name you like, then set this variable.

  * `iolaus_opaleye_datadir`: The path to the directory containing
    this `README.md` file.  This is so the example application can
    find the schema migration files.


Examples of these environment variables can be found in the
top-level [.envrc](../.envrc) file.
