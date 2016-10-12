BEGIN;

CREATE TABLE users
( id        SERIAL       PRIMARY KEY
, username  VARCHAR(255) NOT NULL
, password  VARCHAR(255) NOT NULL
);

INSERT INTO users (username,password) VALUES
  ( 'uname1', 'badpassword' )
, ( 'uname2', 'worsepassword' )
;

COMMIT;
