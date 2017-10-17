[![Build status](https://circleci.com/gh/rezwits/cardnum/tree/master.svg?style=shield)](https://circleci.com/gh/rezwits/cardnum)

Play Middle-earth CCG in the browser.

## Live server

http://[no current live server]

[Gameplay videos](https://[no current live videos])

![screenshot](resources/public/img/tutorials/Screen-Shot-2017-10-16at7.35.29PM.png)


## Card implementation status


[Card rules implementation status](https://[no current status doc)


## Dependencies

* Node.js, Node Package Manager
* Leiningen (version 2+)
* MongoDB
* Zero MQ


## Installation

Install frontend dependencies:

```
$ npm install
```

Launch MongoDB and fetch card data:

```
$ mongod
$ npm run fetch
```

Compile and watch client side Clojurescript files:

```
$ lein figwheel
```

Compile server side Clojure files:

```
$ lein uberjar
```

Launch game server:

```
$ java -jar target/meccg-standalone.jar
```

Launch the Node server:

```
$ npm start
```

## Tests

To run all tests:

```
$ lein test test.all
```

To run a single test file:
```
$ lein test test.cards.agendas
```


For more information refer to the [development guide](https://github.com/rezwits/cardnum/wiki/Getting-Started-with-Development).

## License

Cardnum.net is released under the [MIT License](http://www.opensource.org/licenses/MIT).

