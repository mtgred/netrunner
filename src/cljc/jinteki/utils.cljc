(ns jinteki.utils
  (:require [clojure.string :as s]))

(def INFINITY 2147483647)


(defn str->int
  [string]
  #?(:clj (java.lang.Integer/parseInt (re-find #"^\d+" string))
     :cljs (js/parseInt string 10)))

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

(defn count-bad-pub
  "Counts number of bad pub corp has (real + additional)"
  [state]
  (+ (get-in @state [:corp :bad-publicity :base] 0)
     (get-in @state [:corp :bad-publicity :additional] 0)))

(defn has-bad-pub?
  "Returns truthy if corp has any bad publicity"
  [state]
  (pos? (count-bad-pub state)))

(defn count-tags
  "Counts number of tags runner has (real + additional)"
  [state]
  (or (get-in @state [:runner :tag :total]) 0))

(defn count-real-tags
  "Count number of non-additional tags"
  [state]
  (or (get-in @state [:runner :tag :base]) 0))

(defn is-tagged?
  "Returns truthy if runner is tagged"
  [state]
  (or (get-in @state [:runner :tag :is-tagged])
      (pos? (count-tags state))))

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

(defn superuser?
  [user]
  (or (:isadmin user)
      (:ismoderator user)
      (:tournament-organizer user)))

(defn capitalize [string]
  (if (pos? (count string))
    (str (s/upper-case (first string)) (subs string 1))
    ""))

(defn decapitalize [string]
  (if (pos? (count string))
    (str (s/lower-case (first string)) (subs string 1))
    ""))

(defn make-label
  "Looks into an ability for :label, if it doesn't find it, capitalizes :msg instead."
  [ability]
  (capitalize (or (:label ability)
                  (and (string? (:msg ability))
                       (:msg ability))
                  "")))

(defn add-cost-to-label
  [ability]
  (let [label (make-label ability)
        cost-label (:cost-label ability)]
    (cond
      (and (not (s/blank? cost-label))
           (not (s/blank? label)))
      (str cost-label ": " label)
      :else
      label)))
