# Netrunner in the browser

Hosted at [http://www.jinteki.net](http://www.jinteki.net). [Example of gameplay](https://www.youtube.com/watch?v=cnWudnpeY2c).

![screenshot](http://i.imgur.com/xkxOMHc.jpg)

## Card implementation status

[Card rules implementation status](https://docs.google.com/spreadsheets/d/1ICv19cNjSaW9C-DoEEGH3iFt09PBTob4CAutGex0gnE/pubhtml)

## Development

### Quickstart

*(There's a [docker](#using-docker) section down there!)*

Install [Leiningen](https://leiningen.org/),
[NodeJS](https://nodejs.org/en/download/package-manager/#macos) and
[MongoDB](https://docs.mongodb.com/manual/installation/).

This project runs on Java 21. If you're on OSX or Linux, we recommend using
[jenv](https://github.com/jenv/jenv/blob/master/README.md) to manage your java environment.

You can check your setup by running:

    $ lein version
    Leiningen 2.11.2 on Java 21.0.7 OpenJDK 64-Bit Server VM

Your exact version numbers may vary, but we require Java 21+.

Populate the database and create indexes using:

    $ lein fetch [--no-card-images]
    1648 cards imported

    $ lein create-indexes
    Indexes successfully created.

You can optionally pass `--no-card-images` if you don't want to download images from
[NetrunnerDB](https://netrunnerdb.com/), as this takes a while. See `lein fetch help`
for further options.

To install frontend dependencies, run:

    $ npm ci
    added 124 packages, and audited 125 packages in 2s

To compile CSS:

    $ npm run css:build
    compiled resources/public/css/netrunner.css

Optionally you can say `npm run watch:css` to watch for changes and automatically
recompile.

Compile ClojureScript frontend:

    $ npm run cljs:build
    [:app] Compiling ...
    [:app] Build completed. (238 files, 0 compiled, 0 warnings, 22.18s)

Finally, launch the webserver and the Clojure REPL:

    $ lein repl
    dev.user=>

and open [http://localhost:1042/](http://localhost:1042/).

### Using Docker

You'll need to install [Docker](https://docs.docker.com/get-docker/) and [Docker-Compose](https://docs.docker.com/compose/install/). After that, just run `bin/up` in the project directory (or do the GUI-equivalent of this). If this fails because it "couldn't fetch dependencies", try again, it was just a networking error.

It can take a while. You'll see lots of messages, so just wait until you see something like `netrunner-server-1 | nREPL server started on port 44867`. After that, you can visit [http://localhost:1042/](http://localhost:1042/) and the server should be running.

While coding clojure it's important to have a REPL connection going. The server's REPL is configured to always run on port `44867`, so you can connect using `$ bin/repl` (or the program of your preference, like your code editor).

Now, let's populate the database and create indexes with `bin/database-seed`. This will download card images, which can be slow. When its done the server will be restarted. If you ever need to drop the database, run `bin/database-drop`. This will keep the card images you downloaded previously, but you'll need to do `bin/up` and `bin/database-seed` again.

You can access the servers' bash and lein commands individually using `bin/bash` and `bin/lein`.

### Tests

To do testing, you run them inside the container with `bin/test` and `bin/test-focus`.

To run all tests:

    $ bin/test
    3602 tests, 118797 assertions, 0 failures.

To run a single test file:

    $ bin/test-focus game.cards.agendas-test
    285 tests, 9715 assertions, 0 failures.

Or a single test:

    $ bin/test-focus game.cards.agendas-test/fifteen-minutes
    1 tests, 47 assertions, 0 failures.

For more information refer to the [development guide](https://github.com/mtgred/netrunner/wiki/Getting-Started-with-Development).

### Further reading

- [Development Tips and Tricks](https://github.com/mtgred/netrunner/wiki/Development-Tips-and-Tricks)
- [Writing Tests](https://github.com/mtgred/netrunner/wiki/Tests)
- "Profiling Database Queries" in `DEVELOPMENT.md`

## License

Jinteki.net is released under the [MIT License](http://www.opensource.org/licenses/MIT).
