(ns test.utils
  (:require [monger.core :as mg]
            [monger.collection :as mc]))

(defn load-card [title]
  (let [conn (mg/connect {:host "127.0.0.1" :port 27017})
        db (mg/get-db conn "netrunner")
        card (mc/find-maps db "cards" {:title title})
        ret (first card)]
    (mg/disconnect conn)
    ret))

(defn make-deck [identity deck]
  {:identity identity :deck deck})

(defn qty [card amt]
  {:card (if (string? card) (load-card card) card) :qty amt})

(defn default-corp
  ([] (default-corp [(qty "Hedge Fund" 3)]))
  ([deck] (make-deck "Custom Biotics: Engineered for Success" deck)))

(defn default-runner
  ([] (default-runner [(qty "Sure Gamble" 3)]))
  ([deck] (make-deck "The Professor: Keeper of Knowledge" deck)))