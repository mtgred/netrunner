# Android: Netrunner in the browser

[![Build status](https://circleci.com/gh/mtgred/netrunner/tree/master.svg?style=shield)](https://circleci.com/gh/mtgred/netrunner)

Hosted at [http://www.jinteki.net](http://www.jinteki.net). [Example of gameplay](https://www.youtube.com/watch?v=cnWudnpeY2c).

![screenshot](http://i.imgur.com/xkxOMHc.jpg)

## Card implementation status

[Card rules implementation status](https://docs.google.com/spreadsheets/d/1ICv19cNjSaW9C-DoEEGH3iFt09PBTob4CAutGex0gnE/pubhtml)

## Development

### Quickstart

Install [Leiningen](https://leiningen.org/),
[NodeJS](https://nodejs.org/en/download/package-manager/#macos) and
[MongoDB](https://docs.mongodb.com/manual/installation/).

This project runs on Java 8. If you're on OSX or Linux, we recommend using
[jenv](https://github.com/jenv/jenv/blob/master/README.md) to manage your java environment.

You can check your setup by running:

    $ lein version
    Leiningen 2.9.6 on Java 16.0.1 OpenJDK 64-Bit Server VM

Your exact version numbers below may vary, but we require Java 1.8+.

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

### Tests

To run all tests:

    $ lein test
    Ran 2640 tests containing 44704 assertions.
    0 failures, 0 errors.

To run a single test file:

    $ lein test game.cards.agendas-test
    Ran 216 tests containing 3536 assertions.
    0 failures, 0 errors.

Or a single test:

    $ lein test :only game.cards.agendas-test/fifteen-minutes
    Ran 1 tests containing 29 assertions.
    0 failures, 0 errors.

For more information refer to the [development guide](https://github.com/mtgred/netrunner/wiki/Getting-Started-with-Development).

### Further reading

- [Development Tips and Tricks](https://github.com/mtgred/netrunner/wiki/Development-Tips-and-Tricks)
- [Writing Tests](https://github.com/mtgred/netrunner/wiki/Tests)
- "Profiling Database Queries" in `DEVELOPMENT.md`

## License

Jinteki.net is released under the [MIT License](http://www.opensource.org/licenses/MIT).
