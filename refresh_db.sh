psql postgres -c 'DROP DATABASE opal';
psql postgres -c 'CREATE DATABASE opal';
psql opal -f setup.sql
