(defproject netrunner "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-2202"]
                 [org.clojure/core.async "0.1.278.0-76b25b-alpha"]
                 [om "0.5.0"]
                 [sablono "0.2.16"]]

  :plugins [[lein-cljsbuild "1.0.3"]]

  :source-paths ["src/cljs"]

  :cljsbuild {
    :builds [
      {:id "dev"
       :source-paths ["src/cljs"]
       :compiler {:output-to "resources/cljs/app.js"
                 :output-dir "resources/cljs"
                 :optimizations :none
                 :source-map true}},
      {:id "prod"
       :source-paths ["src/cljs/netrunner"]
       :compiler {:output-to "resources/js/app.js"
                  :optimizations :advanced
                  :pretty-print false
                  :externs ["resources/lib/react/react.min.js"
                            "resources/lib/bootstrap/dist/js/bootstrap.min.js"
                            "node_modules/socket.io/node_modules/socket.io-client/dist/socket.io.min.js"]}}]})
