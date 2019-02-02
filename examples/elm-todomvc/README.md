# TodoMVC in Elm with PostgREST

Based on the original [TodoMVC in Elm](http://github.com/evancz/elm-todomvc) used
to demo [elm-drec](http://github.com/s6o/elm-drec) with [PostgREST](https://github.com/PostgREST/postgrest)

All of the Elm code lives in `src/Main.elm` and relies on the [elm/html][html] and [elm/http][http] libraries.

[html]: https://package.elm-lang.org/packages/elm/html/latest
[http]: https://package.elm-lang.org/packages/elm/http/latest

## Setup

### Prerequisites

* Yarn (and Node)
* PostgreSQL

### Get Elm

```bash
yarn install
```

### Configure PostgreSQL database

Start the `psql` interactive terminal from the project directory so that you can
create the database and import the schema with less typing

```bash
psql -U postgres
```

In `psql`

```psql
create database elm_todomvc;
```

the database name needs to match the name configured in `postgrest.conf`

Now switch to your elm_todomvc database and import the schema and initial data:

```psql
\c elm_todomvc
\i ./schema.sql
```

If all went without errors the query

```psql
select * from api.model ;
```

should produce the following output

```psql
elm_todomvc=# select * from api.model ;
                            model
-------------------------------------------------------------
 {"uid": 0, "field": "", "entries": [], "visibility": "All"}
(1 row)
```

Exit `psql`

```psql
\q
```

### Get PostgREST

Download and extract the single binary into the same directory as this README.
[Latest PostgREST](https://github.com/PostgREST/postgrest/releases/latest)

### Configure PostgREST and PostgreSQL database

Check the `postgrest.conf` provided with the project to have the correct PostgreSQL
access credentials.

### Start PostgREST

To start PostgREST in the background with logging run:

```bash
./start-postgrest.sh
```

The resulting log file `postgrest.log` will be created in the project directory.

### Open the application in browser

Run the following command from the root of this project:

```bash
yarn elm reactor
```

Now open [elm-todomvc-postgrest](http://localhost:8000/src/Main.elm) at

```text
http://localhost:8000/src/Main.elm
```

in your browser.
