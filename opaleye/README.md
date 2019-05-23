Iolaus.Opaleye
==============

The goal of this library is to provide a simple wrapper around the
[opaleye](https://hackage.haskell.org/package/opaleye) and
[postgresql-simple](https://hackage.haskell.org/package/postgresql-simple)
packages without leaking a `MonadIO` interface into your application's
monad, all while exposing an mtl + lens style of composing the
major components of an application.

Using this library is fairly straight forward:

  1. Create your monad transformer stack and then make it an instance
     of `AsOpaleyeError`, `HasOpaleye`, and `CanOpaleye`.

  2. Parse a `Config` value from a configuration file.

  3. Call `initOpaleye` to create the `MonadReader` value you`ll need.

  4. Use the query functions inside your transformer stack!

For more details, including a tutorial, please see the `example.hs`
file that is part of this distribution.

Running the Example
-------------------

To run the example application you first need to set two environment
variables:

  * `DB_CONN`: The PostgreSQL database connection string.  Create a
    database with any name you like this set this variable.

  * `iolaus_opaleye_datadir`: The path to the directory containing
    this `README.md` file.  This is so the example application and
    find the schema migration files.


Examples of these environment variables can be found in the
[.envrc](../.envrc) file.
