(ns game.core.subtypes
  (:require [game.core.board :refer [get-all-cards]]
            [game.core.card :refer [get-card]]
            [game.core.effects :refer [get-effects]]
            [game.core.update :refer [update!]]
            [game.utils :refer [server-card to-keyword]]))

(defn subtypes-for-card
  "Creates a sorted list of subtypes for the card. Returns nil if given a counter or fake agenda."
  [state card]
  (when (:title card)
    (let [printed-subtypes (:subtypes (server-card (:title card)))
          gained-subtypes (flatten (get-effects state nil :gain-subtype card))
          lost-subtypes (flatten (get-effects state nil :lose-subtype card))
          total-gained (frequencies (concat printed-subtypes gained-subtypes))
          total-lost (frequencies lost-subtypes)
          total (reduce
                  (fn [acc [k v]]
                    (let [cur (get acc k 0)
                          total (- cur v)]
                      (if (pos? total)
                        (assoc acc k total)
                        (dissoc acc k))))
                  total-gained
                  total-lost)]
      (into [] (sort (keys total))))))

(defn update-subtypes-for-card
  [state _ card]
  (let [card (get-card state card)
        old-subtypes (:subtypes card)
        new-subtypes (subtypes-for-card state card)
        changed? (not= old-subtypes new-subtypes)]
    (when changed?
      (update! state (to-keyword (:side card)) (assoc card :subtypes new-subtypes)))
    changed?))

(defn update-all-subtypes
  ([state] (update-all-subtypes state nil))
  ([state _]
   (reduce
     (fn [changed? card]
       (or (update-subtypes-for-card state nil card)
           changed?))
     false
     (get-all-cards state))))
