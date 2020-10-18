# Developing with Docker

A quick way to get netrunner up and running locally is using the
`docker-compose.yml` and `Dockerfile` in this repo. You'll
need [docker](https://github.com/docker/docker)
and [docker-compose](https://docs.docker.com/compose/). Once you've got those,
just run

    ./compose.sh -b
    
This should start spinning up your own instance of netrunner! Easy.

Note that the first run will probably need to download a bunch of docker images,
which may take quite a while. Subsequent runs should be quite a lot quicker.

The `-b` flag builds the netrunner-node docker image and should only be needed
on the first run.

When you're done, run `docker-compose stop` to stop the running containers or
`docker-compose down` to stop and destroy containers.

## Issues

While it does the job, this set-up isn't perfect. A lot of the issues are to do
with the order that services start in mattering. Here are some specific issues:

  - If the fetch-cards job runs before mongo is fully started up, cards won't be
    correctly imported into the database. Hence the kludgey `sleep 30` in
    `compose.sh`.
  - The lein* services download there dependencies every time they're run.
  - For reasons I don't understand, `docker up lein-netrunner lein-cljs
    stylus-css coffee-server` causes the lein* services to throw an exception,
    but splitting the non-lein services into a separate up command doesn't.
