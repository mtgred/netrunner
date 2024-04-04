(ns user
  (:require
   [clojure.pprint]
   [kaocha.repl]
   [potemkin :refer [import-vars]]
   [web.dev]))

(defn clear-current-ns
  "Removes all refers, all defined vars, and all imports from the current namespace.
  Useful in development when unsure of the state of the current namespace.
  Can be called from anywhere with `(user/clear-current-ns)`."
  []
  (map #(ns-unmap *ns* %) (keys (ns-imports *ns*))))

(import-vars
  [web.dev
   fetch-cards
   go
   halt
   reset
   restart])

(defmacro spy [item]
  `(do (println "SPY:" '~item)
       (let [result# ~item]
         (println "RESULT:" (with-out-str (clojure.pprint/pprint result#)))
         result#)))

(import-vars
  [kaocha.repl
   run])
