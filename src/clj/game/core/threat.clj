(ns game.core.threat
  (:require
    [clojure.string :as string]
    [game.core.card :refer [corp? in-hand?]]
    [game.core.eid :refer [effect-completed]]
    [game.core.engine :refer [resolve-ability]]
    [game.core.moving :refer [trash-cards]]
    [game.core.say :refer [system-msg]]
    [game.macros :refer [req msg continue-ability]]
    [game.utils :refer [pluralize]]))

(defn threat-level [threshold state]
  ;; does the threat level meet or exceed the given threshold
  (or (<= threshold (get-in @state [:runner :agenda-point]))
      (<= threshold (get-in @state [:corp :agenda-point]))))

(defn get-threat-level [state]
  (max (get-in @state [:runner :agenda-point])
       (get-in @state [:corp :agenda-point])))

(defn threat
  ([threshold accept-ab]
   (threat threshold accept-ab nil))
  ([threshold accept-ab reject-ab]
   {:req (req true)
    :async true
    :effect (req (if (threat-level threshold state)
                   (continue-ability state side accept-ab card targets)
                   (if (nil? reject-ab)
                     (effect-completed state side eid)
                     (continue-ability state side reject-ab card targets))))}))
