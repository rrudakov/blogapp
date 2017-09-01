CREATE TABLE users (
       id    	   serial primary key,
       username	   varchar(30) UNIQUE,
       password	   varchar(100) not null,
       created_at  timestamptz not null,
       is_active   bool not null,
       is_admin	   bool not null
);

CREATE TABLE posts (
       id    	   serial primary key,
       author	   int REFERENCES users (id) ON DELETE CASCADE,
       created	   timestamptz not null,
       content	   text
);
