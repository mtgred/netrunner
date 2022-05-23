(ns game.core.set-aside
  (:require
    [game.core.eid :refer [effect-completed]]
    [game.core.moving :refer [move]]
    [clojure.string :as string]))

(defn set-aside
  ([state side eid cards] (set-aside state side eid cards true true))
   ;(swap! state assoc-in [side :set-aside-tracking (:eid eid)] (map :cid cards))
   ;(doseq [c cards] (move state side c :set-aside))
   ;(effect-completed state side eid))
  ([state side eid cards corp-vis runner-vis]
   (swap! state assoc-in [side :set-aside-tracking (:eid eid)] (map :cid cards))
   (doseq [c cards]
     (move state side (assoc c :set-aside-visibility
                             {:corp-can-see corp-vis :runner-can-see runner-vis}) :set-aside))))

(defn set-aside-for-me
  ([state side eid cards]
   (if (= side :runner)
     (set-aside state side eid cards nil true)
     (set-aside state side eid cards true nil))))

(defn get-set-aside
  [state side eid]
  (let [eid (:eid eid)
        cids (set (get-in @state [side :set-aside-tracking eid]))
        player (get @state side)]
    (->> (:set-aside player)
         (filter #(contains? cids (:cid %)))
         (into []))))
