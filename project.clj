(defproject netrunner "1.0"
  :description "Browser implementation of Netrunner card game."
  :url "https://github.com/mtgred/netrunner"
  :license {:name "The MIT License (MIT)"
            :url "https://opensource.org/licenses/MIT"}
  :min-lein-version "2.7.1"

  :source-paths ["src/clj" "src/cljs" "src/cljc"]
  :test-paths ["test/clj" "test/cljs" "test/cljc"]

  :jar-name "netrunner.jar"
  :jar-exclusions [#"public/img/cards/*"]
  :uberjar-name "netrunner-standalone.jar"
  :uberjar-exclusions [#"public/img/cards/*"]
  :main web.core

  :repl-options {:timeout 180000
                 :init-ns web.dev
                 :init (go)}

  :dependencies [[org.clojure/clojure "1.12.0"]
                 [org.clojure/clojurescript "1.11.132"
                  :exclusions [org.clojure/google-closure-library
                               org.clojure/data.json
                               org.clojure/tools.reader
                               com.cognitect/transit-clj
                               com.cognitect/transit-java]]
                 [com.google.javascript/closure-compiler-unshaded "v20240317"]
                 [org.clojure/core.async "1.7.701"]
                 [com.taoensso/sente "1.19.2" :exclusions [org.clojure/tools.reader]]
                 [ring/ring-core "1.9.4"]
                 [ring/ring-devel "1.9.4" :exclusions [org.clojure/java.classpath]]
                 [ring/ring-anti-forgery "1.3.0"]
                 [ring/ring-json "0.5.1"]
                 [ring-cors "0.1.13"]
                 [compojure "1.6.2"]
                 [hiccup "1.0.5"]
                 [aero "1.1.6"]
                 [cheshire/cheshire "5.10.1"]
                 [stylefruits/gniazdo "1.2.0"]
                 [com.google.guava/guava "31.0.1-jre"]
                 [com.novemberain/monger "3.5.0" :exclusions [com.google.guava/guava]]
                 [differ "0.3.3"]
                 [buddy/buddy-sign "3.4.1"]
                 [buddy/buddy-auth "3.0.1"]
                 [crypto-password "0.3.0"]
                 [digest "1.4.10"]
                 [http-kit "2.7.0"]
                 [com.draines/postal "2.0.5"]
                 [throttler "1.0.1"]
                 [clj-http "3.12.3"]
                 [reagent "1.3.0"]
                 [org.clojure/tools.analyzer "1.1.0"]
                 [org.clojure/tools.analyzer.jvm "1.2.1"]
                 [org.clojars.frozenlock/reagent-modals "0.2.8"]
                 [org.clojure/tools.cli "1.0.206"]
                 [danlentz/clj-uuid "0.1.9"]
                 [potemkin "0.4.5"]
                 [cond-plus "1.1.1"]
                 [org.clojure/data.csv "1.0.0"]
                 [dev.weavejester/medley "1.8.0"]
                 [org.clj-commons/claypoole "1.2.2"]
                 [org.slf4j/slf4j-nop "1.7.32"]
                 [integrant "0.8.0"]
                 [com.widdindustries/cljc.java-time "0.1.21"]
                 [time-literals "0.1.5"]
                 [metosin/reitit "0.7.2"]
                 [org.flatland/ordered "1.15.12"]
                 [io.github.noahtheduke/fluent-clj "0.0.2"]]

  :test-selectors {:default (fn [m] (not (:kaocha/pending m)))}

  :profiles {:dev {:dependencies [[binaryage/devtools "1.0.7"]
                                  [cider/piggieback "0.5.3"]
                                  [com.clojure-goes-fast/clj-async-profiler "0.5.1"]
                                  [rewrite-clj "1.1.45"]
                                  [criterium "0.4.6"]
                                  [instaparse "1.5.0"]
                                  [integrant/repl "0.3.2"]
                                  [lambdaisland/kaocha "1.68.1059"]
                                  [thheller/shadow-cljs "2.28.21"]]
                   :plugins [[lein-eftest "0.6.0"]
                             [cider/cider-nrepl "0.47.1"]]
                   :eftest {:report eftest.report.pretty/report
                            :fail-fast? false}
                   :source-paths ["src/clj" "src/cljs" "src/cljc" "src/css"
                                  "dev/src/clj" "dev/src/cljs"
                                  "test/clj" "test/cljc" "test/cljs"]
                   :resource-paths ["target"]
                   :clean-targets ^{:protect false} ["target"]
                   :jvm-opts ["-Djdk.attach.allowAttachSelf"
                              "-XX:+UnlockDiagnosticVMOptions"
                              "-XX:-OmitStackTraceInFastThrow"
                              "-XX:+DebugNonSafepoints"]}
             :debugger {:jvm-opts ["-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=5010"]}}

  :aliases {"fetch" ^{:doc "Fetch card data and images from github"} ["run" "-m" "tasks.fetch/command"]
            "kaocha" ^{:doc "Run tests with kaocha"} ["run" "-m" "kaocha.runner"]
            "dumbrepl" ["trampoline" "run" "-m" "clojure.main/main"]
            "load-generator" ^{:doc "Performance test lobbies"} ["run" "-m" "tasks.load-generator/command"]
            "delete-duplicate-users" ["run" "-m" "tasks.db/delete-duplicate-users"]
            "update-all-decks" ["run" "-m" "tasks.db/update-all-decks"]
            "update-prizes" ^{:doc "Update prize information (card-backs, etc)"} ["run" "-m" "tasks.update-prizes/command"]
            "update-alt-arts" ^{:doc "Update alt-arts from a private source"} ["run" "-m" "tasks.update-alt-arts/command"]
            "add-deck-to-all" ["run" "-m" "tasks.decks/add-for-all-users"]
            "rename-card" ["run" "-m" "tasks.card-rename/command"]
            "card-coverage" ["run" "-m" "tasks.card-coverage/test-coverage"]
            "create-indexes" ["run" "-m" "tasks.index/create-indexes"]
            "drop-indexes" ["run" "-m" "tasks.index/drop-indexes"]
            "create-sample-data" ["run" "-m" "tasks.db/create-sample-data"]
            "get-game-stats" ["run" "-m" "tasks.game-stats/all-games"]
            "get-user-stats" ["run" "-m" "tasks.user-stats/all-users"]
            "get-background-stats" ["run" "-m" "tasks.user-stats/all-backgrounds"]
            "missing-translations" ^{:doc "Print translations in 'en' that aren't in other languages. Can be passed a language (fr, ko, ...) to filter to that language."} ["run" "-m" "tasks.translations/missing-translations"]
            "undefined-translations" ^{:doc "Find usages of (tr [...]) in code that don't exist in 'en' translation."} ["run" "-m" "tasks.translations/undefined-translations"]
            "unused-translations" ^{:doc "Find entries in 'en' translation that aren't used in code."} ["run" "-m" "tasks.translations/unused-translations"]})
