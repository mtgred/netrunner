services:

  endpoint:
    image: nginx:alpine
    ports:
      - 8042:8000
    volumes:
      - ./docker/prod/nginx.conf:/etc/nginx/nginx.conf:ro
      - {{ resources-folder }}:/usr/share/netrunner
    depends_on:
      - server
    links:
      - server

  server:
    image: {{ image-name }}
    build:
        context: .
        dockerfile: ./docker/prod/Dockerfile
    volumes:
      - "./docker/prod/prod.edn:/opt/netrunner/prod.edn"
    depends_on:
      - database
    links:
      - database:mongo

  database:
    image: {{ image-name-mongodb }}
    container_name: mongodb
    restart: unless-stopped
{{#expose-mongodb}}
    ports:
      - 27017-27019:27017-27019
{{/expose-mongodb}}
    volumes:
      - {{ folder-mongodb }}:/data/db
