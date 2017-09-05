CREATE TABLE users (
       id    	   serial primary key,
       username	   varchar(30) not null UNIQUE,
       password	   varchar(100) not null,
       created_at  timestamptz not null,
       updated_at  timestamptz,
       is_active   bool not null,
       is_admin	   bool not null
);

CREATE TABLE posts (
       id    	   serial primary key,
       author	   int REFERENCES users (id) ON DELETE CASCADE,
       created_at  timestamptz not null,
       updated_at  timestamptz,
       title	   varchar(100) not null,
       content	   text not null
);
