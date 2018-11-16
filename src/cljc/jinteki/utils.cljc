(ns jinteki.utils
  (:require [clojure.string :as s]))

(def INFINITY 2147483647)


(defn str->int [string]
  #?(:clj  (java.lang.Integer/parseInt string)
     :cljs (js/parseInt string)))

(defn side-from-str [side-str]
  (keyword (s/lower-case side-str)))

(defn faction-label
  "Returns faction of a card as a lowercase label"
  [card]
  (if (nil? (:faction card))
    "neutral"
    (-> card :faction s/lower-case (s/replace " " "-"))))

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

(defn has-subtype?
  "Checks if the specified subtype is present in the card, ignoring case."
  [card subtype]
  (letfn [(contains-sub? [card]
            (when-let [subs (:subtype card)]
              (s/includes? (s/lower-case subs) (s/lower-case subtype))))]
    (or (contains-sub? card)
        (contains-sub? (:persistent card)))))

(defn slugify
  "As defined here: https://you.tools/slugify/"
  ([string] (slugify string "-"))
  ([string sep]
   (if-not (string? string) ""
     (as-> string $
       #?(:clj (java.text.Normalizer/normalize $ java.text.Normalizer$Form/NFD)
          :cljs (.normalize $ "NFD"))
       (s/replace $ #"[^\x00-\x7F]+" "")
       (s/lower-case $)
       (s/trim $)
       (s/split $ #"[ \t\n\x0B\f\r!\"#$%&'()*+,-./:;<=>?@\\\[\]^_`{|}~]+")
       (filter seq $)
       (s/join sep $)))))
