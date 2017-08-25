CREATE TABLE users (
       id    	   serial primary key,
       username	   varchar(30) UNIQUE,
       password	   varchar(100) not null,
       is_active   bool not null,
       is_admin	   bool not null
);

CREATE TABLE user_profiles (
       user_id		   serial REFERENCES users (id) ON DELETE CASCADE,
       registered	   date not null,
       first_name	   varchar(100),
       last_name	   varchar(100),
       about		   text,
       avatar_link	   varchar(200)
);

CREATE TABLE posts (
       id    	   serial primary key,
       author	   int REFERENCES users (id) ON DELETE CASCADE,
       created	   timestamp not null,
       content	   text
);
