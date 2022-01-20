(ns dev.user
  (:require
   [io.aviso.exception :as exception]
   [integrant.repl :as ig-repl]
   [malli.dev :as mdev]
   [malli.clj-kondo :as mc]
   [web.system :as system]))

(mdev/start!)
(mc/emit!)

(ig-repl/set-prep! (fn [] (system/server-config)))

(def go ig-repl/go)
(def halt ig-repl/halt)
(defn restart [] (halt) (go))

(comment
  (go)
  (halt)
  (restart))

;; Show clojure java calls in stack traces during development
(alter-var-root
  #'exception/*default-frame-rules*
  (constantly
    [[:package #"sun\.reflect.*" :hide]
     [:package "java.lang.reflect" :omit]
     [:name #"speclj\..*" :terminate]
     [:name #"clojure\.main/repl/read-eval-print.*" :terminate]]))
