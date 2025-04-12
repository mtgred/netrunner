(ns jinteki.card-backs)

(defonce card-backs
  {;; the traditional card backs we all like
   :ffg-card-back {:description "The standard FFG card backs that were with the game for most of it's life."
                   :name "FFG Card Backs"
                   :file "ffg"}
   :nsg-card-back {:description "The current Null Signal Games card backs."
                   :name "NSG Card Backs"
                   :file "nsg"}

   ;; prizes for tournaments - these are assigned to individual players through the admin or TO panels
   :nbk-deep-dive {:description "You won something! Well done!"
                   :side :runner
                   :prize true
                   :name "Deep Dive (nbk)"
                   :file "nbk-deep-dive"}})

(defn just-prizes []
  (into (sorted-map-by (fn [a b] (compare (:name a) (:name b))))
        (filter (fn [[_ v]] (:prize v)) card-backs)))

(defn card-backs-for-side [side unlocked]
  ;; TODO - system for unlocking these
  (into {} (filter (fn [[k v]]
                     (and
                       ;; it either has no specified side, or matches the input side
                       (or (not (:side v)) (= side (:side v)))
                       ;; it's either not a prize, or it's a prize that we own
                       (or (not (:prize v))
                           (contains? unlocked k))))
                   card-backs)))
