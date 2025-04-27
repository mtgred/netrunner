(ns jinteki.card-backs
  #?(:cljs (:require-macros [jinteki.prizes :refer [load-card-backs]])))

(defonce base-card-backs
  {;; the traditional card backs we all like
   :ffg-card-back {:description "The standard FFG card backs that were with the game for most of it's life."
                   :name "FFG Card Backs"
                   :file "ffg"}
   :nsg-card-back {:description "The current Null Signal Games card backs."
                   :name "NSG Card Backs"
                   :file "nsg"}
   ;; fallbacks, just in case
   :ffg {:file "ffg"}
   :nsg {:file "nsg"}})

#?(:cljs (load-card-backs base-card-backs card-backs)
   :clj (def card-backs base-card-backs))

(defn just-prizes []
  (into {}
        (filter (fn [[_ v]] (:prize v)))
        card-backs))

(defn card-backs-for-side [side unlocked]
  ;; TODO for later - explicitly make the nsg and ffg backs pop up at the top of the list,
  ;; regardless of the sorting used
  (into (sorted-map-by (fn [k1 k2]
                         (compare [(get-in card-backs [k1 :name]) k1]
                                  [(get-in card-backs [k2 :name]) k2])))
        (filter (fn [[k v]]
                     (and
                       ;; it either has no specified side, or matches the input side
                       (or (not (:side v)) (= side (:side v)))
                       ;; it's either not a prize, or it's a prize that we own
                       (or (not (:prize v))
                           (contains? unlocked k))))
                (dissoc card-backs :nsg :ffg "" nil))))
