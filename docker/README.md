## Docker - Local Server

# Requirements
Docker needs to be installed on your local machine. Setup instructions can be found here: https://www.docker.com/get-started

## Local Server
Contains an application server and a database server. Suitable for experimenting on your local machine.

# Instructions
```
% cd docker
% docker-compose up
% docker-compose exec jnet lein fetch
% docker-compose restart jnet
```

To stop a running instance, use `docker-compose stop`.

The Jinteki interface should now be available at http://localhost:1042

## Production Server
Contains an application server, database server, and reverse-proxy server. Closer to a production-ready setup.

# Instructions
```
% cd docker
% docker-compose -f docker-compose-production.yml up
% docker-compose exec jnet lein fetch
% docker-compose restart jnet
```

To stop a running instance, use `docker-compose stop`.

The Jinteki interface should now be available at http://localhost:8080

## Data Storage
The database and card images are stored in Docker volumes. The volumes will persist until `docker-compose down` is executed or the volumes are manually deleted.


