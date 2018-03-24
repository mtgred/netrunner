[![Build status](https://circleci.com/gh/mtgred/netrunner/tree/master.svg?style=shield)](https://circleci.com/gh/mtgred/netrunner)

Play Android: Netrunner in the browser.

## Live server

http://www.jinteki.net

[Gameplay videos](https://www.youtube.com/results?search_query=jinteki.net)

![screenshot](http://i.imgur.com/xkxOMHc.jpg)


## Card implementation status


[Card rules implementation status](https://docs.google.com/spreadsheets/d/1ICv19cNjSaW9C-DoEEGH3iFt09PBTob4CAutGex0gnE/pubhtml)


## Dependencies

* [Leiningen](https://leiningen.org/) (version 2+)
* [MongoDB](https://docs.mongodb.com/manual/administration/install-community/)


## Installation

Install frontend dependencies:

```
$ npm install -g bower
$ npm install -g stylus
$ bower install
```

Launch MongoDB and fetch card data:

```
$ mongod --dbpath data
```
or on windows
```
$ mongod --dbpath .\data\
```
then:
```
$ lein fetch
```

Compile and watch client side ClojureScript files<sup>[1](#footnote_1)</sup>:

```
$ lein figwheel
```

Launch web server:

* As a REPL process (recommended for development):
    ```
    $ lein repl
    ```
* As a standalone process in production mode (must first run `lein uberjar` and `lein cljsbuild once prod`):
    ```
    $ java -jar target/netrunner-standalone.jar
    ```

Open http://localhost:1042/

## Tests

To run all tests:

```
$ lein test
```

To run a single test file:
```
$ lein test game-test.cards.agendas
```

Or a single test:
```
$ lein test :only game-test.cards.agendas/fifteen-minutes
```

For more information refer to the [development guide](https://github.com/mtgred/netrunner/wiki/Getting-Started-with-Development).

## License

Jinteki.net is released under the [MIT License](http://www.opensource.org/licenses/MIT).


<a name="footnote_1">1</a>: This is only necessary the first time you run the project, or if you are working on front end changes.
