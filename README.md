# PG Schema Extractor

Simple schema extractor for PostgreSQL, which runs pure SQL and psql.


## Usage

    psql pg_db_meta.sql

    psql --username=<role> --port=<port> --host=<host> --dbname=<dbname> --file=pg_db_meta.sql

Extract DB schema in pg_db_meta directory.

If directory exists, recreated.

