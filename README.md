Play Android: Netrunner in the browser.

## Live server

http://www.jinteki.net

![screenshot](https://dl.dropboxusercontent.com/u/5601199/screenshot.jpg)


## Development status

The deck builder implements all the deck building constraints. It is optimised for fast deck edition. It is possible for instance to copy & paste a decklist from a forum and it will be parsed.

The implemention of the game rules is in progress. About 75% of the cards are currently automated. For the cards that aren't, it is possible to resolve them manually most of the time.

[Card rules implementation status](https://www.dropbox.com/s/rpkhxafgile5spp/Cards%20status.xlsx).


## Dependencies

* Node.js, Node Package Manager
* Leiningen (version 2+)
* MongoDB
* Coffeescript
* Bower


## Installation

Install Node.js dependencies:

```
$ npm install
```

Install JavaScript dependencies:

```
$ bower install
```

Launch MongoDB and fetch card data:

```
$ mongod
$ cd data
$ coffee fetch.coffee
```

Compile and watch client side Clojurescript files:

```
$ lein cljsbuild auto dev
```

Compile server side Clojure files:

```
$ lein uberjar
```

Launch game server:

```
$ java -jar target/netrunner-0.1.0-SNAPSHOT-standalone.jar
```

Launch the Node server:

```
$ coffee server.coffee
```

## Optional

If you use nREPL, you can launch a Clojurescript Browser REPL:

```
$ lein repl

user> (brepl)
<< started Weasel server on ws://0.0.0.0:9001 >>
Type `:cljs/quit` to stop the ClojureScript REPL
```
