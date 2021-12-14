(defproject netrunner "1.0"
  ;; the version string gets replaced by the git rev version plugin anyway
  :description "Browser implementation of Android: Netrunner card game."
  :url "https://github.com/mtgred/netrunner"
  :license {:name "The MIT License (MIT)"
            :url "https://opensource.org/licenses/MIT"}
  :min-lein-version "2.7.1"

  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/clojurescript "1.10.773"]
                 [org.clojure/core.async "1.5.644"]
                 [com.taoensso/sente "1.16.2"]
                 [com.taoensso/tempura "1.2.1"]
                 [ring/ring-core "1.9.4"]
                 [ring/ring-devel "1.9.4"]
                 [ring/ring-anti-forgery "1.3.0"]
                 [ring/ring-json "0.5.1"]
                 [puppetlabs/ring-middleware "1.3.1"]
                 [ring-cors "0.1.13"]
                 [compojure "1.6.2"]
                 [hiccup "1.0.5"]
                 [aero "1.1.6"]
                 [cheshire/cheshire "5.10.1"]
                 [stylefruits/gniazdo "1.2.0"]
                 [com.novemberain/monger "3.5.0"]
                 [differ "0.3.3"]
                 [buddy/buddy-sign "3.4.1"]
                 [buddy/buddy-auth "3.0.1"]
                 [crypto-password "0.3.0"]
                 [digest "1.4.10"]
                 [http-kit "2.5.3"]
                 [com.draines/postal "2.0.5"]
                 [throttler "1.0.1"]
                 [clj-http "3.12.3"]
                 [reagent "1.1.0"]
                 [org.clojure/tools.analyzer "1.1.0"]
                 [org.clojure/tools.analyzer.jvm "1.2.1"]
                 [org.clojars.frozenlock/reagent-modals "0.2.8"]
                 [org.clojure/tools.cli "1.0.206"]
                 [danlentz/clj-uuid "0.1.9"]
                 [potemkin "0.4.5"]
                 [cond-plus "1.1.1"]
                 [org.clojure/data.csv "1.0.0"]
                 [medley "1.3.0"]
                 [org.slf4j/slf4j-nop "1.7.32"]
                 [integrant "0.8.0"]
                 [cljc.java-time "0.1.18"]
                 [time-literals "0.1.5"]
                 [metosin/reitit "0.5.15"]
                 [metosin/malli "0.7.0"]]

  :plugins [[lein-eftest "0.5.9"]
            [cider/cider-nrepl "0.27.3"]]

  :profiles {:dev {:dependencies [[binaryage/devtools "1.0.4"]
                                  [cider/piggieback "0.5.3"]
                                  [com.clojure-goes-fast/clj-async-profiler "0.5.1"]
                                  [rewrite-clj "1.0.699-alpha"]
                                  [criterium "0.4.6"]
                                  [integrant/repl "0.3.2"]
                                  [com.bhauman/figwheel-main "0.2.15"]
                                  [com.bhauman/rebel-readline-cljs "0.1.4"]]
                   :source-paths ["dev" "src/clj" "src/cljs" "src/cljc" "test/clj"]
                   :resource-paths ["target"]
                   :clean-targets ^{:protect false} ["target"]
                   :jvm-opts ["-Djdk.attach.allowAttachSelf"
                              "-XX:+UnlockDiagnosticVMOptions"
                              "-XX:-OmitStackTraceInFastThrow"
                              "-XX:+DebugNonSafepoints"]}}

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
            "get-background-stats" ["run" "-m" "tasks.user-stats/all-backgrounds"]
            ;; figwheel-main commands
            "fig"      ["trampoline" "run" "-m" "figwheel.main"]
            "fig:dev"  ["trampoline" "run" "-m" "figwheel.main" "-b" "dev" "-r"]
            "fig:prod" ["run" "-m" "figwheel.main" "-O" "advanced" "-bo" "prod"]
            ; "fig:test"  ["run" "-m" "figwheel.main" "-co" "test.cljs.edn" "-m" "nr.test-runner"]
            }

  ;; Compilation.
  :source-paths ["src/clj" "src/cljs/nr" "src/cljc"]
  ;; aot only the namespaces needed for the main game in uberjar, notably ignoring the test and (most of the) task namespaces.
  :aot [web.core]
  :jar-name "netrunner.jar"
  :jar-exclusions [#"public/img/cards/*"]
  :uberjar-name "netrunner-standalone.jar"
  :uberjar-exclusions [#"public/img/cards/*"]
  :omit-source true
  :main web.core

  ;; Misc
  :test-paths ["test/clj"]
  :eftest {:report eftest.report.pretty/report
           :fail-fast? false}

  ;; Set timeout to 2 min to allow for full compilation after a clean.
  :repl-options {:timeout 180000
                 :init-ns dev.user
                 :init (do (use 'web.core) (go))})
