(defproject netrunner "1.0"
  ;; the version string gets replaced by the git rev version plugin anyway
  :description "Browser implementation of Android: Netrunner card game."
  :url "https://github.com/mtgred/netrunner"
  :license {:name "The MIT License (MIT)"
            :url "https://opensource.org/licenses/MIT"}

  :dependencies [[org.clojure/clojure "1.9.0-beta3"]
                 [org.clojure/clojurescript "1.9.229"]
                 [org.clojure/core.async "0.3.443"]
                 [cheshire "5.6.3"]
                 [org.omcljs/om "0.9.0"]
                 [sablono "0.3.4"]
                 [com.novemberain/monger "3.1.0"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [differ "0.3.1"]
                 [com.taoensso/sente "1.11.0"]
                 [ring "1.6.2"]
                 [ring/ring-json "0.4.0"]
                 [compojure "1.6.0"]
                 [hiccup "1.0.5"]
                 ;[org.immutant/web "2.1.9"]
                 [aero "1.1.2"]
                 [buddy/buddy-sign "2.2.0"]
                 [buddy/buddy-auth "1.4.1"]
                 [crypto-password "0.2.0"]
                 [binaryage/devtools "0.9.7"]
                 [digest "1.4.6"]
                 [http-kit "2.2.0"]
                 [org.slf4j/slf4j-nop "1.7.12"]

                 ]

  :profiles {:dev {:dependencies [[figwheel-sidecar "0.5.11"]
                                  [com.cemerick/piggieback "0.2.1"]
                                  [binaryage/devtools "0.9.4"]]
                   :plugins [[lein-figwheel "0.5.11"]]
                   :source-paths ["src/clj" "src/cljs" "src/dev" "src/cljc"]}}

  ;; aot only the namespaces needed for the main game in uberjar, notably ignoring the test namespaces
  :aot [game.utils
        game.main
        game.macros
        game.core
        web.core
        jinteki.utils]
  :main web.core

  :test-paths ["test/clj"]

  :plugins [[lein-cljsbuild "1.1.4"]
            [lein-figwheel "0.5.11"]
            [com.gfredericks/lein-sha-version "0.1.1-p1"]
            [lein-ring "0.9.7"]]

  :ring {:handler web.api/app}

  :source-paths ["src/clj" "src/cljs" "src/cljc"]

  :jar-name "netrunner.jar"
  :uberjar-name "netrunner-standalone.jar"

  :omit-source true

  :cljsbuild {
    :builds [
      {:id "dev"
       :source-paths ["src/cljs" "src/dev" "src/cljc"]
       :figwheel true
       :compiler {:output-to "resources/public/js/app.js"
                  :output-dir "resources/public/cljs"
                  :optimizations :none
                  :source-map-timestamp true
                  :preloads [devmode.core]
                  :external-config {:devtools/config {:features-to-install :all}}}}
      {:id "prod"
       :source-paths ["src/cljs/netrunner" "src/cljc"]
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
             :css-dirs ["resources/public/css"]}

  ;; Set timeout to 2 min to allow for full compilation after a clean.
  :repl-options {:timeout 120000
                 :init-ns web.core
                 ;:init (-main)
                 })
