(ns dev.user
  (:require
   [integrant.repl :as ig-repl]
   [web.system :as system]))

(ig-repl/set-prep! (fn [] (system/server-config)))

(def go ig-repl/go)
(def halt ig-repl/halt)
(defn restart [] (halt) (go))

(comment
  (go)
  (halt)
  (restart))
