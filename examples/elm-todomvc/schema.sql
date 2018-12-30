-- elm-drec-todomvc schema for PostgREST

create schema api;

create table api.model (
  uid INTEGER DEFAULT 0
, field TEXT DEFAULT ''
, visibility TEXT DEFAULT 'All'
, entries JSONB DEFAULT '[]'
);

insert into api.model (uid, visibility) values (0 , 'All');

grant all on schema api to anon;
