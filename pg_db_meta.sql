-- Simple schema extractor for PostgreSQL, which runs pure SQL and psql.
-- выгрузка структуры БД в приятном виде по каталогам
-- готовый рабочий скрипт

-- psql --username=<role> --port=<port> --host=<host> --dbname=<dbname> --file=pg_db_meta.sql

\! rm --recursive pg_db_meta


--------------------------------------------------------------------------------
-- пишем таблицы
-- tables

\o /tmp/psql-out.sql

WITH
table_list AS (
SELECT  n.nspname as schema,
        c.relname as name,
        c.oid AS table_oid,
        CASE WHEN n.nspname = 'public' THEN '' 
             ELSE n.nspname || '.' 
        END || c.relname AS table_name,
        CASE WHEN obj_description(c.oid, 'pg_class') IS NULL THEN NULL
             ELSE 'COMMENT ON TABLE  ' || CASE WHEN n.nspname = 'public' THEN '' 
                                               ELSE n.nspname || '.' 
                                          END || c.relname || ' IS ' || quote_nullable(pg_catalog.obj_description(c.oid, 'pg_class')) || ';' 
        END AS table_comment,
        (SELECT  string_agg(
                    CASE WHEN f5[2] = 'arwdDxt' THEN 'GRANT ALL ON ' || CASE WHEN n.nspname = 'public' THEN '' 
                                                                             ELSE n.nspname || '.' 
                                                                        END || c.relname || ' TO ' || f5[1] || ';'
                         ELSE 'GRANT ' || (SELECT string_agg(CASE WHEN t7.f7 = 'a' THEN 'INSERT'
                                                                  WHEN t7.f7 = 'r' THEN 'SELECT'
                                                                  WHEN t7.f7 = 'w' THEN 'UPDATE'
                                                                  WHEN t7.f7 = 'd' THEN 'DELETE'
                                                                  WHEN t7.f7 = 'D' THEN 'TRUNCATE'
                                                                  WHEN t7.f7 = 'x' THEN 'REFERENCES'
                                                                  WHEN t7.f7 = 't' THEN 'TRIGGER'
                                                                  WHEN t7.f7 = 'X' THEN 'EXECUTE'
                                                                  WHEN t7.f7 = 'U' THEN 'USAGE'
                                                                  WHEN t7.f7 = 'C' THEN 'CREATE'
                                                                  WHEN t7.f7 = 'c' THEN 'CONNECT'
                                                                  WHEN t7.f7 = 'T' THEN 'TEMPORARY'
                                                             END, ', ') FROM regexp_split_to_table(f5[2], '\s*') t7(f7))
                        || ' ON ' || CASE WHEN n.nspname = 'public' THEN '' 
                                         ELSE n.nspname || '.' 
                                    END || c.relname || ' TO ' || f5[1] || ';'
                    END, '
' ORDER BY f5[1]) AS f1
            FROM (SELECT  regexp_match(f5::text, '(.+)=(.+)/(.+)') f5
                  FROM UNNEST(c.relacl) tt01(f5)) tt01
            WHERE f5[1] <> f5[3]) AS table_grant
FROM pg_catalog.pg_class c
LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
WHERE c.relkind IN ('r','p','')
  AND n.nspname <> 'pg_catalog'
  AND n.nspname <> 'information_schema'
  AND n.nspname !~ '^pg_toast'
),

column_list AS (
SELECT  '    ' || a.attname || ' ' ||
-- проверяем если вдруг SERIAL
CASE WHEN pg_get_serial_sequence((SELECT (SELECT tt2.nspname 
                                        FROM pg_namespace tt2 
                                        WHERE tt2.oid = tt1.relnamespace) || '.' || tt1.relname 
                                FROM pg_class tt1 
                                WHERE tt1.oid = a.attrelid) , a.attname) IS NOT NULL THEN 
    CASE WHEN lower(pg_catalog.format_type(a.atttypid, a.atttypmod)) = 'integer' THEN 'SERIAL'
         ELSE 'BIGSERIAL'
    END
ELSE
-- Тип колонки
UPPER(
regexp_replace(
regexp_replace(
regexp_replace(pg_catalog.format_type(a.atttypid, a.atttypmod), 'timestamp without time zone', 'TIMESTAMPTZ'),
                                                                'character varying', 'VARCHAR'),
                                                                'double precision', 'FLOAT8'))
|| CASE WHEN a.attnotnull THEN ' NOT NULL' ELSE '' END ||
-- значение по умолчанию
COALESCE(' DEFAULT ' ||
(SELECT substring(pg_catalog.pg_get_expr(d.adbin, d.adrelid) for 128)
 FROM pg_catalog.pg_attrdef d
 WHERE d.adrelid = a.attrelid 
   AND d.adnum = a.attnum 
   AND a.atthasdef), '')
END AS f1,
        a.attrelid AS table_oid,
        a.attnum AS nu,
        CASE WHEN col_description(a.attrelid, a.attnum) IS NULL THEN NULL
             ELSE 'COMMENT ON COLUMN ' ||
                    (SELECT (SELECT CASE WHEN tt2.nspname = 'public' THEN '' 
                                         ELSE tt2.nspname || '.' 
                                    END
                             FROM pg_namespace tt2 
                             WHERE tt2.oid = tt1.relnamespace) || tt1.relname 
                     FROM pg_class tt1 
                     WHERE tt1.oid = a.attrelid) || '.' || a.attname || ' IS ' || quote_nullable(col_description(a.attrelid, a.attnum)) || ';' 
        END AS col_comment
FROM pg_catalog.pg_attribute a
WHERE a.attnum > 0 
  AND NOT a.attisdropped
  AND a.attrelid IN (SELECT table_oid FROM table_list)
),

constraint_list AS (
SELECT  '    CONSTRAINT ' ||
        trim(
        regexp_replace(
        regexp_replace(
        regexp_replace(
        regexp_replace(r.conname || ' ' || pg_catalog.pg_get_constraintdef(r.oid, true), ' REFERENCES ','
        REFERENCES '), '\) ON ', ')
        ON '),
        'ON DELETE NO ACTION', ''),
        'ON UPDATE NO ACTION', '')) AS f1,
        r.conrelid AS table_oid,
        CASE WHEN r.contype = 'p' THEN 10 
             WHEN r.contype = 'u' THEN 20
             WHEN r.contype = 'f' THEN 30
             ELSE 100
        END AS nu
FROM pg_catalog.pg_constraint r
),

index_list AS (
SELECT
regexp_replace(
regexp_replace(
regexp_replace(
regexp_replace(pg_get_indexdef(t1.indexrelid), ' USING btree ', '')
                                             , ' public.', ' ')
                                             , ' ON ', '
    ON '), ' WHERE ', '
    WHERE ') || ';' AS f1,
        t1.indrelid AS table_oid,
        1 AS nu
FROM pg_index t1
LEFT JOIN pg_constraint t2 ON t2.conindid = t1.indexrelid
WHERE t2.oid IS NULL
),

trigger_list AS (
SELECT  regexp_replace(
        regexp_replace(
        regexp_replace(
        regexp_replace(
        regexp_replace(pg_get_triggerdef(t.oid, true), ' BEFORE ', '
    BEFORE '), ' AFTER ', '
    AFTER '), ' ON ', '
    ON '), ' FOR ', '
    FOR '), ' EXECUTE ', '
    EXECUTE ') || ';' AS f1,
        t.tgrelid AS table_oid,
        1 AS nu
FROM pg_catalog.pg_trigger t
WHERE (NOT t.tgisinternal OR (t.tgisinternal AND t.tgenabled = 'D'))
)

SELECT 
'\! mkdir -p pg_db_meta/' || table_list.schema || '/table
'
|| '\o pg_db_meta/' || table_list.schema || '/table/' || table_list.name || '.sql
'
|| 'SELECT $$'
|| '-- DROP TABLE ' || table_list.table_name || ';

CREATE TABLE ' || table_list.table_name || ' ('
|| COALESCE('
' || (SELECT string_agg(column_list.f1, ',
' ORDER BY column_list.nu) 
      FROM column_list 
      WHERE column_list.table_oid = table_list.table_oid), '')
|| COALESCE(',
' || (SELECT string_agg(constraint_list.f1, ',
' ORDER BY constraint_list.nu, constraint_list.f1) 
      FROM constraint_list 
      WHERE constraint_list.table_oid = table_list.table_oid), '')
|| '
);
'
|| COALESCE('
' || table_list.table_comment, '')
|| COALESCE('
' || (SELECT string_agg(col_comment, '
' ORDER BY column_list.nu) 
      FROM column_list 
      WHERE column_list.table_oid = table_list.table_oid), '')
|| COALESCE('


' || table_list.table_grant, '')
|| COALESCE('


' || (SELECT string_agg(index_list.f1, '

' ORDER BY index_list.nu, index_list.f1) 
      FROM index_list 
      WHERE index_list.table_oid = table_list.table_oid), '')
|| COALESCE('


' || (SELECT string_agg(trigger_list.f1, '

' ORDER BY trigger_list.f1) FROM trigger_list WHERE trigger_list.table_oid = table_list.table_oid), '')
|| '
$$;
\o'
FROM table_list
ORDER BY table_list.schema, table_list.name;

-- Выводим по папкам
\pset tuples_only on \pset format unaligned \g /tmp/psql-out.sql \include /tmp/psql-out.sql \pset format aligned \pset tuples_only off


--------------------------------------------------------------------------------
-- пишем отображения
-- views 

\o /tmp/psql-out.sql

SELECT  '\! mkdir -p pg_db_meta/' || n.nspname || '/view
'
|| '\o pg_db_meta/' || n.nspname || '/view/' || c.relname || '.sql
'
|| 'SELECT $$'
||
CASE c.relkind WHEN 'v' THEN 'CREATE OR REPLACE VIEW ' 
                       WHEN 'm' THEN 'CREATE MATERIALIZED VIEW '
        END || CASE WHEN n.nspname = 'public' THEN '' ELSE n.nspname || '.' END || c.relname 
        || ' AS 
' ||
        regexp_replace(
        regexp_replace(
        regexp_replace(
        regexp_replace(
        regexp_replace(
        regexp_replace(
        regexp_replace(
        regexp_replace(pg_get_viewdef(c.oid), ' SELECT ', 'SELECT  ', 'g'),
                                              '   FROM ', 'FROM ', 'g'),
                                              '  WHERE ', 'WHERE ', 'g'),
                                              '  ORDER BY ', 'ORDER BY ', 'g'),
                                              '  GROUP BY ', 'GROUP BY ', 'g'),
                                              '     JOIN ', 'JOIN ', 'g'),
                                              '     LEFT JOIN ', 'LEFT JOIN ', 'g'),
                                              '    ', '        ', 'g')
        || COALESCE('

COMMENT ON ' || CASE c.relkind WHEN 'v' THEN 'VIEW ' 
                       WHEN 'm' THEN 'MATERIALIZED VIEW '
        END || CASE WHEN n.nspname = 'public' THEN '' ELSE n.nspname || '.' END || c.relname || ' IS '
|| quote_nullable(pg_catalog.obj_description(c.oid, 'pg_class')) 
|| CASE WHEN pg_catalog.obj_description(c.oid, 'pg_class') IS NULL THEN NULL ELSE ';' END, '')
|| '
$$;
\o'
FROM pg_catalog.pg_class c
LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace
WHERE c.relkind IN ('v', 'm', '')
  AND n.nspname <> 'pg_catalog'
  AND n.nspname <> 'information_schema'
  AND n.nspname !~ '^pg_toast'
ORDER BY n.nspname, c.relname;

-- Выводим по папкам
\pset tuples_only on \pset format unaligned \g /tmp/psql-out.sql \include /tmp/psql-out.sql \pset format aligned \pset tuples_only off


--------------------------------------------------------------------------------
-- пишем процедуры
-- functions

\o /tmp/psql-out.sql

SELECT  '\! mkdir -p pg_db_meta/' || n.nspname || '/function
'
|| '\o pg_db_meta/' || n.nspname || '/function/' || p.proname || '.sql
'
|| 'SELECT $$'
||

        regexp_replace(
        regexp_replace(
        regexp_replace(
        regexp_replace(
        regexp_replace(
        regexp_replace(
        regexp_replace(
        regexp_replace(pg_get_functiondef(p.oid), ' LANGUAGE ', 'LANGUAGE '),
                                                  ' IMMUTABLE', 'IMMUTABLE'),
                                                  ' STABLE ', 'STABLE '),
                                                  'END \$function\$', 'END
$function$'),
                                                  'AS \$function\$', 'AS
$function$'),
                                                  ' SECURITY ', 'SECURITY '),
-- заменяем табличную часть return
'TABLE.*LANGUAGE',
(SELECT regexp_replace(
        regexp_replace(
        regexp_replace(
        regexp_replace(
        regexp_replace(
        regexp_replace(
        regexp_replace(
        regexp_replace(string_agg(t1.f1, ',
    '), 'TABLE\(', 'TABLE (
    '), '\)', '
)
LANGUAGE'), ' integer', ' INTEGER', 'g')
          , ' text', ' TEXT', 'g')
          , ' character varying', ' VARCHAR', 'g')
          , ' numeric', ' NUMERIC', 'g')
          , ' timestamp without time zone', ' TIMESTAMP', 'g')
          , ' timestamp with time zone', ' TIMESTAMPTZ', 'g')
         FROM regexp_split_to_table(pg_get_function_result(p.oid), ', ') t1(f1)
        )),
-- заменяем входные аргументы
'CREATE .*RETURNS',
'CREATE OR REPLACE FUNCTION ' || n.nspname || '.' || p.proname ||
        (SELECT CASE WHEN string_agg('    ' || f1, ',
') = '    ' THEN '()
' ELSE '(
' || regexp_replace(
     regexp_replace(
     regexp_replace(
     regexp_replace(
     regexp_replace(
     regexp_replace(string_agg('    ' || f1, ',
'), ' integer', ' INTEGER', 'g')
  , ' text', ' TEXT', 'g')
  , ' numeric', ' NUMERIC', 'g')
  , ' timestamp without time zone', ' TIMESTAMP', 'g')
  , ' timestamp with time zone', ' TIMESTAMPTZ', 'g')
  , ' character varying', ' VARCHAR', 'g') || '
) ' END
FROM regexp_split_to_table(pg_get_function_arguments(p.oid), ', ') t1(f1)) || 'RETURNS'
)
|| '
$$;
\o'
FROM pg_catalog.pg_proc p
LEFT JOIN pg_catalog.pg_namespace n ON n.oid = p.pronamespace
WHERE pg_catalog.pg_function_is_visible(p.oid)
  AND n.nspname <> 'pg_catalog'
  AND n.nspname <> 'information_schema'
  AND NOT p.proisagg;

-- Выводим по папкам
\pset tuples_only on \pset format unaligned \g /tmp/psql-out.sql \include /tmp/psql-out.sql \pset format aligned \pset tuples_only off


--------------------------------------------------------------------------------
-- пишем последовательности
-- sequences

\o /tmp/psql-out.sql

SELECT  '\! mkdir -p pg_db_meta/' || t1.schemaname || '/sequence
'
|| '\o pg_db_meta/' || t1.schemaname || '/sequence/sequence.sql
'
|| 'SELECT $$'
||
        string_agg(
        'CREATE SEQUENCE ' || t1.schemaname || '.' || t1.sequencename
        || CASE WHEN t1.increment_by <> 1 THEN ' INCREMENT BY ' || t1.increment_by ELSE '' END
        || CASE WHEN t1.cache_size <> 1 THEN ' CACHE ' || t1.cache_size ELSE '' END
        || ';', '
' ORDER BY t1.schemaname, t1.sequencename)
|| '
$$;
\o'
FROM pg_sequences t1
WHERE t1.schemaname || '.' || t1.sequencename NOT IN (
    SELECT pg_get_serial_sequence(t3.nspname || '.' || t2.relname, t1.attname) AS f1
    FROM pg_attribute t1
    INNER JOIN pg_class t2 ON t2.oid = t1.attrelid
    INNER JOIN pg_namespace t3 ON t3.oid = t2.relnamespace
    WHERE t2.relkind IN ('r')
      AND t3.nspname <> 'pg_catalog'
      AND t3.nspname <> 'information_schema'
      AND t3.nspname !~ '^pg_toast'
      AND t1.attnum > 0
      AND pg_get_serial_sequence(t3.nspname || '.' || t2.relname, t1.attname) IS NOT NULL
)
GROUP BY t1.schemaname
ORDER BY t1.schemaname;

\pset tuples_only on \pset format unaligned \g /tmp/psql-out.sql \include /tmp/psql-out.sql \pset format aligned \pset tuples_only off


--------------------------------------------------------------------------------
-- пишем типы
-- types & domains

\o /tmp/psql-out.sql

SELECT  '\! mkdir -p pg_db_meta/' || n.nspname || '/type
'
|| '\o pg_db_meta/' || n.nspname || '/type/' || t.typname || '.sql
'
|| 'SELECT $$'
||

CASE WHEN t.typtype = 'e' THEN 'CREATE TYPE ' || format_type(t.oid, NULL) || ' AS ENUM (
' || 
(SELECT string_agg('    ' || quote_nullable(tt01.enumlabel), ',
' ORDER BY tt01.enumsortorder)
FROM pg_enum tt01
WHERE tt01.enumtypid = t.oid) || '
);'
WHEN t.typtype = 'd' THEN 'CREATE DOMAIN ' || format_type(t.oid, NULL) || ' AS ' || format_type(t.typbasetype, t.typtypmod)
|| CASE WHEN t.typdefault IS NOT NULL THEN '
    DEFAULT ' || quote_nullable(t.typdefault) ELSE '' END
|| CASE WHEN t.typnotnull THEN '
    NOT NULL ' ELSE '' END
|| COALESCE('
' || array_to_string(ARRAY(SELECT '    CONSTRAINT ' || r.conname || ' ' || pg_get_constraintdef(r.oid, true) FROM pg_constraint r WHERE t.oid = r.contypid), '
'), '')
|| ';' 
WHEN t.typtype = 'c' THEN 'CREATE TYPE ' || CASE WHEN n.nspname <> 'public' THEN n.nspname || '.' ELSE '' END || format_type(t.oid, NULL) || ' AS (
' || 
(SELECT  string_agg(
'    ' || t1.attname || ' '  || 
UPPER(regexp_replace(
      regexp_replace(
      regexp_replace(
      regexp_replace(
      regexp_replace(format_type(t1.atttypid, t1.atttypmod), 'int4', 'INTEGER')
                                                           , 'int8', 'BIGINT')
                                                           , 'timestamp without time zone', 'TIMESTAMP')
                                                           , 'timestamp with time zone', 'TIMESTAMPTZ')
                                                           , 'character varying', 'VARCHAR')), ',
' ORDER BY t1.attnum)
FROM pg_attribute t1
WHERE t1.attrelid = t.typrelid) || '
);'
END || COALESCE('

COMMENT ON TYPE ' || format_type(t.oid, NULL) || ' IS ' || quote_literal(obj_description(t.oid, 'pg_type')) || ';', '')
|| '
$$;
\o'
FROM pg_catalog.pg_type t
LEFT JOIN pg_catalog.pg_namespace n ON n.oid = t.typnamespace
WHERE (t.typrelid = 0 OR (SELECT c.relkind = 'c' FROM pg_catalog.pg_class c WHERE c.oid = t.typrelid))
  AND NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type el WHERE el.oid = t.typelem AND el.typarray = t.oid)
  AND n.nspname <> 'pg_catalog'
  AND n.nspname <> 'information_schema'
ORDER BY n.nspname, format_type(t.oid, NULL);

\pset tuples_only on \pset format unaligned \g /tmp/psql-out.sql \include /tmp/psql-out.sql \pset format aligned \pset tuples_only off


--------------------------------------------------------------------------------
-- пишем схемы
-- schemas

\o /tmp/psql-out.sql

SELECT 
'\! mkdir -p pg_db_meta/' || n.nspname || '
' || '\o pg_db_meta/' || n.nspname || '/schema.sql
'
|| 'SELECT $$'
||

'CREATE SCHEMA ' || n.nspname || ';

' || COALESCE('COMMENT ON SCHEMA ' || n.nspname || ' IS ' || quote_literal(obj_description(n.oid, 'pg_namespace')) || ';

', '')
||
(SELECT  string_agg(
                    'GRANT ' || (SELECT string_agg(CASE WHEN t7.f7 = 'a' THEN 'INSERT'
                                                                  WHEN t7.f7 = 'r' THEN 'SELECT'
                                                                  WHEN t7.f7 = 'w' THEN 'UPDATE'
                                                                  WHEN t7.f7 = 'd' THEN 'DELETE'
                                                                  WHEN t7.f7 = 'D' THEN 'TRUNCATE'
                                                                  WHEN t7.f7 = 'x' THEN 'REFERENCES'
                                                                  WHEN t7.f7 = 't' THEN 'TRIGGER'
                                                                  WHEN t7.f7 = 'X' THEN 'EXECUTE'
                                                                  WHEN t7.f7 = 'U' THEN 'USAGE'
                                                                  WHEN t7.f7 = 'C' THEN 'CREATE'
                                                                  WHEN t7.f7 = 'c' THEN 'CONNECT'
                                                                  WHEN t7.f7 = 'T' THEN 'TEMPORARY'
                                                             END, ', ') FROM regexp_split_to_table(f5[2], '\s*') t7(f7))
                        || ' ON SCHEMA ' || n.nspname || ' TO ' || f5[1] || ';', '
' ORDER BY f5[1]) AS f1
            FROM (SELECT  regexp_match(f5::text, '(.+)=(.+)/(.+)') f5
                  FROM UNNEST(n.nspacl) tt01(f5)) tt01
            WHERE f5[1] <> f5[3])
|| '
$$;
\o'
FROM pg_catalog.pg_namespace n
WHERE n.nspname !~ '^pg_' 
  AND n.nspname <> 'information_schema'
  AND n.nspname <> 'public';

\pset tuples_only on \pset format unaligned \g /tmp/psql-out.sql \include /tmp/psql-out.sql \pset format aligned \pset tuples_only off

\o


