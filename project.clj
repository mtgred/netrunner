(defproject netrunner "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2311"]
                 [org.clojure/core.async "0.1.303.0-886421-alpha"]
                 [figwheel "0.1.4-SNAPSHOT"]
                 [om "0.7.1"]
                 [sablono "0.2.20"]]

  :plugins [[lein-cljsbuild "1.0.3"]
            [lein-figwheel "0.1.4-SNAPSHOT"]]

  :source-paths ["src/node" "src/cljs"]

  :cljsbuild {
    :builds [
      {:id "node"
       :source-paths ["src/node"]
       :compiler {:output-to "game.js"
                  :target :nodejs
                  :optimizations :simple
                  :pretty-print true}}
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
