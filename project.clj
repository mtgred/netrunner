(defproject netrunner "1.0"
  ; the version string gets replaced by the git rev version plugin anyway
  :description "Browser implementation of Android: Netrunner card game."
  :url "https://github.com/mtgred/netrunner"
  :license {:name "The MIT License (MIT)"
            :url "https://opensource.org/licenses/MIT"}

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.229"]
                 [org.clojure/core.async "0.2.391"]
                 [org.zeromq/jeromq "0.3.6"]
                 [cheshire "5.6.3"]
                 [org.omcljs/om "0.9.0"]
                 [sablono "0.3.4"]
                 [environ "1.1.0"]
                 [com.novemberain/monger "3.1.0"]
                 [org.slf4j/slf4j-nop "1.7.21"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [differ "0.3.1"]]

  :profiles {:dev {:dependencies [[figwheel "0.5.8"]
                                  [figwheel-sidecar "0.5.8"]
                                  [com.cemerick/piggieback "0.2.1"]]}}

  ; aot only the namespaces needed for the main game in uberjar
  :aot [game.utils
        game.main
        game.macros
        game.core]
  :main game.main

  :plugins [[lein-cljsbuild "1.1.4"]
            [lein-figwheel "0.5.8"]
            [com.gfredericks/lein-sha-version "0.1.1-p1"]]

  :source-paths ["src/clj" "src/cljs"]

  :jar-name "netrunner.jar"
  :uberjar-name "netrunner-standalone.jar"

  :omit-source true

  :cljsbuild {
    :builds [
      {:id "dev"
       :source-paths ["src/cljs"]
       :compiler {:output-to "resources/public/cljs/app.js"
                  :output-dir "resources/public/cljs"
                  :optimizations :none
                  :source-map true}}
      {:id "prod"
       :source-paths ["src/cljs/netrunner"]
       :compiler {:output-to "resources/public/js/app.js"
                  :output-dir "out"
                  :optimizations :advanced
                  :pretty-print false
                  :externs ["src/cljs/externs/extras.js"
                            "src/cljs/externs/$.js"
                            "src/cljs/externs/howler.js"
                            "src/cljs/externs/io.js"
                            "src/cljs/externs/marked.js"
                            "src/cljs/externs/moment.js"
                            "src/cljs/externs/toastr.js"]}}]}

  :figwheel {:http-server-root "public"
             :server-port 3449
             :css-dirs ["resources/public/css"]})
