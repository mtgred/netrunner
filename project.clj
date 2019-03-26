(defproject netrunner "1.0"
  ;; the version string gets replaced by the git rev version plugin anyway
  :description "Browser implementation of Android: Netrunner card game."
  :url "https://github.com/mtgred/netrunner"
  :license {:name "The MIT License (MIT)"
            :url "https://opensource.org/licenses/MIT"}

  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.520"]
                 [org.clojure/core.async "0.4.490"]
                 [org.clojure/tools.cli "0.4.1"]
                 [cheshire "5.8.1"]
                 [danhut/monger "3.1.0"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [differ "0.3.2"]
                 [com.taoensso/sente "1.13.1"]
                 [ring "1.7.1" :exclusions [ring/ring-codec]]
                 [ring/ring-json "0.4.0"]
                 [compojure "1.6.1"]
                 [hiccup "1.0.5"]
                 [aero "1.1.3"]
                 [buddy/buddy-sign "3.0.0"]
                 [buddy/buddy-auth "2.1.0"]
                 [crypto-password "0.2.1"]
                 [binaryage/devtools "0.9.10"]
                 [digest "1.4.8"]
                 [http-kit "2.3.0"]
                 [org.slf4j/slf4j-nop "1.7.26"]
                 [jwarwick/trello "0.3.3" :exclusions [org.clojure/tools.namespace
                                                       org.clojure/core.cache
                                                       org.clojure/data.priority-map]]
                 [clj-time "0.15.1"]
                 [com.draines/postal "2.0.3"]
                 [throttler "1.0.0"]
                 [clj-http "3.9.1"]
                 [reagent "0.8.1"]
                 [eftest "0.5.7"]
                 [cljsjs/react "16.8.3-0"]
                 [cljsjs/react-dom "16.8.3-0"]
                 [org.clojure/tools.analyzer "0.7.0"]
                 [org.clojure/tools.analyzer.jvm "0.7.2"]
                 [org.clojars.frozenlock/reagent-modals "0.2.8"]
                 [hawk "0.2.11"]]

  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-figwheel "0.5.18"]
            [com.gfredericks/lein-sha-version "0.1.1-p1"]
            [lein-ring "0.9.7"]
            [lein-eftest "0.5.2"]
            [lein-exec "0.3.7"]]

  :profiles {:dev {:dependencies [[figwheel-sidecar "0.5.18" :exclusions [args4j]]
                                  [binaryage/devtools "0.9.10"]
                                  [cider/piggieback "0.4.0"]]
                   :plugins [[lein-figwheel "0.5.16"]]
                   :source-paths ["src/clj" "src/cljs" "src/dev" "src/cljc"]}}

  :aliases {"fetch" ["run" "-m" "tasks.fetch/fetch"]
            "dumbrepl" ["trampoline" "run" "-m" "clojure.main/main"]
            "add-art" ["run" "-m" "tasks.altart/add-art"]
            "delete-duplicate-users" ["run" "-m" "tasks.db/delete-duplicate-users"]
            "card-coverage" ["run" "-m" "tasks.cards/test-coverage"]}

  ;; Compilation.
  :source-paths ["src/clj" "src/cljs/nr" "src/cljc"]
  ;; aot only the namespaces needed for the main game in uberjar, notably ignoring the test and (most of the) task namespaces.
  :aot [#"game\.*"
        #"web\.*"
        #"tasks.fetch"
        #"tasks.altart"
        #"jinteki\.*"]
  :jar-name "netrunner.jar"
  :uberjar-name "netrunner-standalone.jar"
  :omit-source true
  :main web.core


  ;; Misc
  :test-paths ["test/clj"]
  :eftest {:report eftest.report.pretty/report}

  :ring {:handler web.api/app}

  :cljsbuild {
    :builds [
      {:id "dev"
       :source-paths ["src/cljs/nr" "src/cljs/dev" "src/cljc"]
       :figwheel true
       :compiler {:output-to "resources/public/cljs/app10.js"
                  :output-dir "resources/public/cljs"
                  :main "dev.nr"
                  :asset-path   "cljs"
                  :optimizations :none
                  :source-map-timestamp true
                  :npm-deps false
                  :external-config {:devtools/config {:features-to-install :all}}}}
      {:id "prod"
       :source-paths ["src/cljs/nr" "src/cljs/prod" "src/cljc"]
       :compiler {:output-to "resources/public/js/app10.js"
                  :output-dir "out"
                  :optimizations :advanced
                  :pretty-print false
                  :npm-deps false
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
  :repl-options {:timeout 180000
                 :init-ns web.core
                 :init (do (use 'web.lobby) (-main "dev"))})
