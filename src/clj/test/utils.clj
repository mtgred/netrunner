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

(defn load-cards []
  (let [conn (mg/connect {:host "127.0.0.1" :port 27017})
        db (mg/get-db conn "netrunner")
        cards (mc/find-maps db "cards")
        ret (take 99999 cards)]
    ;; Doing this to materialize the list. I'm sure there's a better way. The take above might be useless.
    (count ret)
    (mg/disconnect conn)
    ret))

(defn qty [card amt]
  {:card (if (string? card) (load-card card) card) :qty amt})

(defn make-deck [identity deck]
  {:identity identity 
   :deck (map #(if (string? %) (qty % 1) %) deck)})

(defn default-corp
  ([] (default-corp [(qty "Hedge Fund" 3)]))
  ([deck] (make-deck "Custom Biotics: Engineered for Success" deck)))

(defn default-runner
  ([] (default-runner [(qty "Sure Gamble" 3)]))
  ([deck] (make-deck "The Professor: Keeper of Knowledge" deck)))