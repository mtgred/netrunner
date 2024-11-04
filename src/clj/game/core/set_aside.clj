(ns game.core.set-aside
  (:require
    [game.core.card :refer [in-set-aside?]]
    [game.core.eid :refer [effect-completed]]
    [game.core.moving :refer [move swap-cards]]
    [game.utils :refer [dissoc-in]]
    [clojure.string :as string]))

(defn set-aside
  "move a group of cards to the set-aside zone. Does not call effect-completed on the eid"
  ([state side eid cards] (set-aside state side eid cards true true))
  ([state side eid cards corp-vis runner-vis]
   (swap! state assoc-in [side :set-aside-tracking (:eid eid)] (map :cid cards))
   (mapv #(move state side (assoc % :set-aside-visibility {:corp-can-see corp-vis :runner-can-see runner-vis}
                                  :set-aside-eid eid) :set-aside)
         cards)))

(defn set-aside-for-me
  "sets aside cards visible only to the player setting them aside"
  ([state side eid cards]
   (if (= side :runner)
     (set-aside state side eid cards nil true)
     (set-aside state side eid cards true nil))))

(defn get-set-aside
  "gets all the cards currently set aside in the given players set-aside zone with tracked with this eid"
  [state side eid]
  (let [eid (:eid eid)
        cids (set (get-in @state [side :set-aside-tracking eid]))
        player (get @state side)]
    (->> (:set-aside player)
         (filter #(contains? cids (:cid %)))
         (into []))))

(defn clean-set-aside!
  "cleans stale entries out of the set aside tracker"
  [state side]
  (let [to-clear (filter #(empty? (get-set-aside state side {:eid %})) (keys (get-in @state [side :set-aside-tracking])))]
    (doseq [eid to-clear]
      (swap! state dissoc-in [side :set-aside-tracking eid]))))

;; adds a card into the set aside zone using a given cid (ie add it to the set of cards in a draw)
(defn add-to-set-aside
  "Adds a card into an existing set-aside eid tracker"
  [state side eid card visibility]
  (set-aside state side eid
             (conj (get-set-aside state side eid) card)
             (:corp-can-see visibility) (:runner-can-see visibility)))

(defn swap-set-aside-cards
  "Swaps two cards when one or both aren't installed"
  [state side a b]
  (let [swapped-cards (swap-cards state side a b)
        a-moved (first swapped-cards)
        b-moved (second swapped-cards)]
    (when (in-set-aside? a)
      (add-to-set-aside state side (:set-aside-eid a) b-moved (:set-aside-visibility a)))
    (when (in-set-aside? b)
      (add-to-set-aside state side (:set-aside-eid b) a-moved (:set-aside-visibility b)))))
