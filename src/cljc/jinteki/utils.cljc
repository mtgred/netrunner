(ns jinteki.utils)

(def INFINITY 2147483647)


(defn str->int [s]
  #?(:clj  (java.lang.Integer/parseInt s)
     :cljs (js/parseInt s)))

(defn side-from-str [side-str]
  (keyword (.toLowerCase side-str)))

(defn faction-label
  "Returns faction of a card as a lowercase label"
  [card]
  (if (nil? (:faction card))
    "neutral"
    (-> card :faction .toLowerCase (.replace " " "-"))))

(defn other-side [side]
  (cond (= side :corp) :runner
        (= side :runner) :corp))

(defn count-tags
  "Counts number of tags runner has (real + additional)"
  [state]
  (+ (get-in @state [:runner :tag :base] 0)
     (get-in @state [:runner :tag :additional] 0)))

(defn is-tagged?
  "Returns truthy if runner is tagged"
  [state]
  (or (pos? (get-in @state [:runner :tag :is-tagged] 0))
      (pos? (count-tags state))))
