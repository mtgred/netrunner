(ns dev.user
  (:require
   [integrant.repl :as ig-repl]
   [time-literals.read-write :as read-write]
   [web.system :as system]))

(read-write/print-time-literals-clj!)

(ig-repl/set-prep! (fn [] (system/server-config)))

(def go ig-repl/go)
(def halt ig-repl/halt)
(defn restart [] (halt) (go))

(comment
  (go)
  (halt)
  (restart))
