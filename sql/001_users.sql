CREATE TABLE users (
       id    	   serial primary key,
       username	   varchar(30) not null unique,
       password	   varchar(100) not null
);

INSERT INTO users (username, password) VALUES ('admin', 'vbpfynhjg');
