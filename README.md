# [PostgreSQL Schema][1]

[![Package version][2]][3]
[![Build status][4]][5]
[![Dependency status][6]][7]

PostgreSQL Schema is a database migration tool. Based on [Schema Evolution Manager][8].

### Adding Migrations

The tool `schema-add` takes as parameters a file and a directory
containing migrations. By default, the directory `migrations` in the
current working directory is used. These parameters can be overridden
on the command line:

    $ schema-add --file new.sql --dir scripts

### Applying Migrations

The tool `schema-apply` takes as parameters a database and a directory
containing migrations. By default, the directory `migrations` in the
current working directory is used, and the database specified in
`DATABASE_URL` environment variable is used. These parameters can be
overridden on the command line:

    $ schema-apply --url postgres://user@localhost/database --dir scripts

[1]: https://github.com/mfine/postgresql-schema
[2]: https://img.shields.io/hackage/v/postgresql-schema.svg?style=flat
[3]: https://hackage.haskell.org/package/postgresql-schema
[4]: https://img.shields.io/travis/mfine/postgresql-schema/master.svg?style=flat
[5]: https://travis-ci.org/mfine/postgresql-schema
[6]: https://img.shields.io/hackage-deps/v/postgresql-schema.svg?style=flat
[7]: http://packdeps.haskellers.com/feed?needle=postgresql-schema
[8]: https://github.com/mbryzek/schema-evolution-manager
