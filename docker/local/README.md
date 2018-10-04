## Docker - Local Server

This directory contains the configuration files to generate a Docker container that runs the Jinteki server in a local container. Suitable for experimenting on your local machine.

# Requirements
Docker needs to be installed on your local machine. Setup instructions can be found here: https://www.docker.com/get-started

# Instructions
```
% cd docker/local
% docker-compose up
% docker-compose exec jnet lein fetch
% docker-compose restart jnet
```

To stop a running instance, use `docker-compose stop`.

The Jinteki interface should now be available at http://localhost:1042

# Data Storage
The database and card images are stored in Docker volumes. The volumes will persist until `docker-compose down` is executed or the volumes are manually deleted.


