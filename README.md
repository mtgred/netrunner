Play Android: Netrunner in the browser.

## Live server

http://netrunner-manabase.rhcloud.com/


## Development status

The chat, deck builder, card browser and game lobby are operational. All the deck building rules are implemented the deck builder except for "The Professor" and "Custom Biotics".

The implemention of the game rules is in progress. Some actions such as drawing, taking credit or playing card are implemented but a game can't be completely played yet.

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
$ coffee data/fetch.coffee
```

## Usage

To launch the server:

```
$ coffee server.coffee
```

To compile and watch Clojurescript files:

```
$ lein cljsbuild auto dev
```

To launch a Clojurescript Browser nREPL:

```
$ lein repl

user> (brepl)
<< started Weasel server on ws://0.0.0.0:9001 >>
Type `:cljs/quit` to stop the ClojureScript REPL
```