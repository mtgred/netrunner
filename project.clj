(defproject netrunner "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-3196"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [org.zeromq/jeromq "0.3.4"]
                 [cheshire "5.4.0"]
                 [org.omcljs/om "0.8.8"]
                 [sablono "0.3.4"]
                 [environ "1.0.0"]
                 [com.novemberain/monger "3.0.0-rc2"]
                 [org.slf4j/slf4j-nop "1.7.12"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [differ "0.2.1"]]

  :profiles {:dev {:dependencies [[figwheel "0.3.7"]]}}

  :main game.main
  :aot :all

  :plugins [[lein-cljsbuild "1.0.6"]
            [lein-figwheel "0.3.7"]]

  :source-paths ["src/clj" "src/cljs"]

  :cljsbuild {
    :builds [
      {:id "dev"
       :source-paths ["src/cljs"]
       :compiler {:output-to "resources/public/cljs/app.js"
                  :output-dir "resources/public/cljs"
                  :optimizations :none
                  :source-map true}},
      {:id "prod"
       :source-paths ["src/cljs/netrunner"]
       :compiler {:output-to "resources/public/js/app.js"
                  :output-dir "out"
                  :optimizations :advanced
                  :pretty-print false
                  :externs ["resources/public/lib/jquery/jquery.min.js"
                            "resources/public/lib/react/react.min.js"
                            "resources/public/lib/moment/min/moment.min.js"
                            "resources/public/lib/bootstrap/dist/js/bootstrap.min.js"
                            "node_modules/socket.io/node_modules/socket.io-client/socket.io.js"]}}]}

  :figwheel {:http-server-root "public"
             :server-port 3449
             :css-dirs ["resources/public/css"]})
