[![Build status](https://circleci.com/gh/mtgred/netrunner/tree/master.svg?style=shield)](https://circleci.com/gh/mtgred/netrunner)

Play Android: Netrunner in the browser.

## Live server

http://www.jinteki.net

[Gameplay videos](https://www.youtube.com/results?search_query=jinteki.net)

![screenshot](http://i.imgur.com/xkxOMHc.jpg)


## Card implementation status


[Card rules implementation status](https://docs.google.com/spreadsheets/d/1ICv19cNjSaW9C-DoEEGH3iFt09PBTob4CAutGex0gnE/pubhtml)


## Dependencies

* Leiningen (version 2+)
* MongoDB


## Installation

Install frontend dependencies:

```
$ npm install
```

Launch MongoDB and fetch card data:

```
$ mongod
$ lein fetch
```

Compile and watch client side ClojureScript files:

```
$ lein figwheel
```

Compile server side Clojure files:

```
$ lein uberjar
```

Launch web server:

* As a REPL process (recommended for development):
    ```
    $ lein repl
    ```
* As a standalone process in production mode (must first run `lein cljsbuild once prod`):
    ```
    $ java -jar target/netrunner-standalone.jar
    ```



## Tests

To run all tests:

```
$ lein test
```

To run a single test file:
```
$ lein test game-test.cards.agendas
```


For more information refer to the [development guide](https://github.com/mtgred/netrunner/wiki/Getting-Started-with-Development).

## License

Jinteki.net is released under the [MIT License](http://www.opensource.org/licenses/MIT).
