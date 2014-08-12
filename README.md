## Live server

http://netrunner-manabase.rhcloud.com/

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

To launch a Clojurescript Node.js nREPL:

```
$ lein repl

user> (node-repl)
Type `:cljs/quit` to stop the ClojureScript REPL
```