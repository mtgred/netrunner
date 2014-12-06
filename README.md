Play Android: Netrunner in the browser.

## Live server

http://www.jinteki.net

![screenshot](https://dl.dropboxusercontent.com/u/5601199/screenshot.jpg)


## Development status

The chat, deck builder, card browser and game lobby are operational.

The deck builder implements all the deck building constraints. It is optimised for fast deck edition. It is possible for instance to copy & paste a decklist from a forum and it will be parsed.

The implemention of the game rules is in progress. About half of the cards are currently automated. For the cards that aren't, it is possible to resolve them manually most of the time.

Card rules implementation status: https://www.dropbox.com/s/rpkhxafgile5spp/Cards%20status.xlsx


## Dependencies

* Node.js, Node Package Manager
* Leiningen
* MongoDB
* Coffeescript


## Installation

Install Node.js dependencies:

```
$ npm install
```

Install Javascript dependencies:

```
$ node_modules/bower/bin/bower install
```

Launch MongoDB and fetch card data:

```
$ mongod
$ cd data
$ mkdir img
$ coffee fetch.coffee
```

## Usage

To launch the server:

```
$ coffee server.coffee
```

To compile and watch client side Clojurescript files:

```
$ lein cljsbuild auto dev
```

To compile and watch server side Clojurescript files:

```
$ lein cljsbuild auto node
```

To launch a Clojurescript Browser nREPL:

```
$ lein repl

user> (brepl)
<< started Weasel server on ws://0.0.0.0:9001 >>
Type `:cljs/quit` to stop the ClojureScript REPL
```