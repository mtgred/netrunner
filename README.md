## Live server

[http://netrunner-manabase.rhcloud.com/]

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

Launch the server:

```
$ coffee server.coffee
```

Compile and watch Clojurescript files:

```
$ lein cljsbuild auto dev
```

Launch a Browser REPL:

```
$ lein repl

user> (brepl)
<< started Weasel server on ws://0.0.0.0:9001 >>
Type `:cljs/quit` to stop the ClojureScript REPL
nil
```

