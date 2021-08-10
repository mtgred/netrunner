(defproject netrunner "1.0"
  ;; the version string gets replaced by the git rev version plugin anyway
  :description "Browser implementation of Android: Netrunner card game."
  :url "https://github.com/mtgred/netrunner"
  :license {:name "The MIT License (MIT)"
            :url "https://opensource.org/licenses/MIT"}

  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.238"]
                 [org.clojure/core.async "0.3.443"]
                 [cheshire "5.6.3"]
                 [stylefruits/gniazdo "1.1.4"]
                 [com.novemberain/monger "3.5.0"]
                 [differ "0.3.3"]
                 [com.taoensso/sente "1.16.0"]
                 [ring "1.7.1"]
                 [ring/ring-defaults "0.3.2"] ; Includes `ring-anti-forgery`, etc.
                 [ring/ring-json "0.4.0"]
                 [compojure "1.6.0"]
                 [hiccup "1.0.5"]
                 [aero "1.1.3"]
                 [buddy/buddy-sign "2.2.0"]
                 [buddy/buddy-auth "1.4.1"]
                 [crypto-password "0.2.0"]
                 [digest "1.4.6"]
                 [http-kit "2.4.0"]
                 [clj-time "0.14.2"]
                 [com.draines/postal "2.0.2"]
                 [throttler "1.0.0"]
                 [clj-http "3.7.0"]
                 [reagent "0.8.1"]
                 [cljsjs/react "16.5.2-0"]
                 [cljsjs/react-dom "16.5.2-0"]
                 [org.clojure/tools.analyzer "0.7.0"]
                 [org.clojure/tools.analyzer.jvm "0.7.2"]
                 [org.clojars.frozenlock/reagent-modals "0.2.8"]
                 [org.clojure/tools.cli "0.4.2"]
                 [hawk "0.2.11"]
                 [danlentz/clj-uuid "0.1.9"]
                 [potemkin "0.4.5"]
                 [cond-plus "1.0.1"]
                 [com.taoensso/tempura "1.2.1"]
                 [org.clojure/data.csv "1.0.0"]
                 [medley "1.3.0"]
                 [org.slf4j/slf4j-nop "1.7.12"]
                 [integrant "0.8.0"]
                 [metosin/malli "0.5.1"]]

  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-figwheel "0.5.16"]
            [lein-ring "0.9.7"]
            [lein-eftest "0.5.8"]
            [lein-exec "0.3.7"]]

  :profiles {:dev {:dependencies [[figwheel-sidecar "0.5.16"]
                                  [binaryage/devtools "0.9.7"]
                                  [cider/piggieback "0.5.2"]]
                   :plugins [[lein-figwheel "0.5.16"]
                             [integrant/repl "0.3.2"]]
                   :source-paths ["src/clj" "src/cljs" "src/cljc"]}}

  :aliases {"fetch" ["run" "-m" "tasks.fetch/command"]
            "dumbrepl" ["trampoline" "run" "-m" "clojure.main/main"]
            "load-generator" ["run" "-m" "tasks.load-generator/command"]
            "delete-duplicate-users" ["run" "-m" "tasks.db/delete-duplicate-users"]
            "update-all-decks" ["run" "-m" "tasks.db/update-all-decks"]
            "add-deck-to-all" ["run" "-m" "tasks.decks/add-for-all-users"]
            "rename-card" ["run" "-m" "tasks.card-rename/command"]
            "card-coverage" ["run" "-m" "tasks.card-coverage/test-coverage"]
            "create-indexes" ["run" "-m" "tasks.index/create-indexes"]
            "drop-indexes" ["run" "-m" "tasks.index/drop-indexes"]
            "create-sample-data" ["run" "-m" "tasks.db/create-sample-data"]
            "get-game-stats" ["run" "-m" "tasks.game-stats/all-games"]
            "get-user-stats" ["run" "-m" "tasks.user-stats/all-users"]
            "get-background-stats" ["run" "-m" "tasks.user-stats/all-backgrounds"]}

  ;; Compilation.
  :source-paths ["src/clj" "src/cljs/nr" "src/cljc"]
  ;; aot only the namespaces needed for the main game in uberjar, notably ignoring the test and (most of the) task namespaces.
  :aot [#"web.core"
        #"tasks.fetch"]
  :jar-name "netrunner.jar"
  :jar-exclusions [#"public/img/cards/*"]
  :uberjar-name "netrunner-standalone.jar"
  :uberjar-exclusions [#"public/img/cards/*"]
  :omit-source true
  :main web.core


  ;; Misc
  :test-paths ["test/clj"]
  :eftest {:report eftest.report.pretty/report
           ; :capture-output? false
           :fail-fast? false}

  :ring {:handler web.api/app}

  :cljsbuild
  {:builds
   [{:id "dev"
     :source-paths ["src/cljs/nr" "src/cljs/dev" "src/cljc"]
     :figwheel true
     :compiler {:output-to "resources/public/cljs/app10.js"
                :output-dir "resources/public/cljs"
                :main "dev.nr"
                :asset-path   "/cljs"
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
                          "src/cljs/externs/moment.js"
                          "src/cljs/externs/toastr.js"]}}]}

  :figwheel {:http-server-root "public"
             :server-port 3449
             :reload-clj-files false
             :css-dirs ["resources/public/css"]}

  ;; Set timeout to 2 min to allow for full compilation after a clean.
  :repl-options {:timeout 180000
                 :init-ns web.core
                 :init (do (use 'web.lobby) (-main "dev"))})
