(ns game.utils
  (:require [clojure.string :refer [split-lines split join]]
            [clojure.stacktrace :refer [print-stack-trace]]
            [jinteki.cards :refer [all-cards]]
            [clj-uuid :as uuid]))

(defn make-cid []
  (uuid/to-string (uuid/v4)))

(defn server-card
  [title]
  (let [card (get @all-cards title)]
    (if (and title card)
      card
      (.println *err* (with-out-str
                        (print-stack-trace
                          (Exception. (str "Tried to select server-card for " title))
                          2500))))))

(defn server-cards
  []
  (vals @jinteki.cards/all-cards))

(defn abs [n] (max n (- n)))

(defn safe-zero?
  "`zero?` throws up on non numbers, so this is a safe version."
  [n]
  ((fnil zero? 1) n))

(defn safe-inc-n
  "Helper function to safely update a value by n. Returns a function to use with `update` / `update-in`"
  [n]
  (partial (fnil + 0 0) n))

(defn sub->0
  "Helper function for use in `update` or `update-in` to subtract for a value, to a minimum of 0."
  [n]
  #(max 0 ((fnil - 0 0) % n)))

(defn remove-once [pred coll]
  (let [[head tail] (split-with (complement pred) coll)]
    (vec (concat head (rest tail)))))

(defn card-is?
  "Checks the property of the card to see if it is equal to the given value,
  as either a string or a keyword"
  [card property value]
  (let [cv (property card)]
    (cond
      (or (keyword? cv)
          (and (string? value)
               (string? cv)))
      (= value cv)

      (and (keyword? value)
           (string? cv))
      (= value (keyword (.toLowerCase cv)))

      :else
      (= value cv))))

(defn zone
  "Associate the specified zone to each item in the collection.
  Zone can be a singleton or a sequential collection"
  [zone coll]
  (let [dest (if (sequential? zone) (vec zone) [zone])]
    (map #(assoc % :zone dest) coll)))

(defn to-keyword [string]
  (cond

    (= "[Credits]" string)
    :credit

    (string? string)
    (keyword (.toLowerCase string))

    :else
    string))

(defn capitalize [string]
  (str (Character/toUpperCase (first string)) (subs string 1)))

(defn decapitalize [string]
  (str (Character/toLowerCase (first string)) (subs string 1)))

(defn vdissoc [v n]
  (vec (concat (subvec v 0 n) (subvec v (inc n)))))

(defn distinct-by [f coll]
  (letfn [(step [xs seen]
            (lazy-seq (when-let [[x & more] (seq xs)]
                        (let [k (f x)]
                          (if (seen k)
                            (step more seen)
                            (cons x (step more (conj seen k))))))))]
    (step coll #{})))

(defn string->num [s]
  (try
    (let [num (bigdec s)]
      (if (and (> num Integer/MIN_VALUE) (< num Integer/MAX_VALUE)) (int num) num))
  (catch Exception e nil)))

(def safe-split (fnil split ""))

(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
  nested structure. keys is a sequence of keys. Any empty maps that result
  will not be present in the new structure."
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))

(defn click-spent?
  "Returns true if player has spent at least one click"
  [side state]
  (case side
    :runner (contains? (into {} (get @state :turn-events)) :runner-spent-click)
    :corp   (contains? (into {} (get @state :turn-events)) :corp-spent-click)))

(defn used-this-turn?
  "Returns true if a card has been used this turn"
  [cid state]
  (contains? (get-in @state [:per-turn]) cid))

(defn cancellable
  "Wraps a vector of prompt choices with a final 'Cancel' option. Optionally sorts the vector alphabetically,
  with Cancel always last."
  ([choices] (cancellable choices false))
  ([choices sorted]
   (if sorted
     (conj (vec (sort-by :title choices)) "Cancel")
     (conj (vec choices) "Cancel"))))

(defn side-str
  "Converts kw into str. If str is passed same str is returned."
  [side]
  (cond
    (= side :corp) "Corp"
    (= side "Corp") "Corp"
    (= side :runner) "Runner"
    (= side "Runner") "Runner"))

(defn same-side?
  "Checks if two supplied sides are the same side. Accepts both keyword and str."
  [side1 side2]
  (= (side-str side1) (side-str side2)))

(defn same-card?
  "Checks if the two cards are the same by :cid. Alternatively specify 1-function to use to check the card"
  ([card1 card2] (same-card? :cid card1 card2))
  ([func card1 card2]
   (let [id1 (func card1)
         id2 (func card2)]
     (and (some? id1)
          (some? id2)
          (= id1 id2)))))

;;; Functions for working with zones.
(defn remote-num->name [num]
  (str "Server " num))

(defn remote->name
  "Converts a remote zone to a string"
  [zone]
  (let [kw (if (keyword? zone) zone (last zone))
        s (str kw)]
    (when (.startsWith s ":remote")
      (let [num (last (split s #":remote"))]
        (remote-num->name num)))))

(defn central->name
  "Converts a central zone keyword to a string."
  [zone]
  (case (if (keyword? zone) zone (last zone))
    (:hand :hq) "HQ"
    (:deck :rd) "R&D"
    (:discard :archives) "Archives"
    nil))

(defn zone->name
  "Converts a zone to a string."
  [zone]
  (or (central->name zone)
      (remote->name zone)))

(defn zone->sort-key [zone]
  (case (if (keyword? zone) zone (last zone))
    :archives -3
    :rd -2
    :hq -1
    (string->num
      (last (safe-split (str zone) #":remote")))))

(defn zones->sorted-names [zones]
  (->> zones (sort-by zone->sort-key) (map zone->name)))

(defn is-remote?
  "Returns true if the zone is for a remote server"
  [zone]
  (not (nil? (remote->name zone))))

(defn is-central?
  "Returns true if the zone is for a central server"
  [zone]
  (not (is-remote? zone)))

(defn is-root?
  "Returns true if the zone is root a central server"
  [zone]
  (and (is-central? (second zone))
       (= :content (last zone)))
)

(defn central->zone
  "Converts a central server keyword like :discard into a corresponding zone vector"
  [zone]
  (case (if (keyword? zone) zone (last zone))
    :discard [:servers :archives]
    :hand [:servers :hq]
    :deck [:servers :rd]
    nil))

(defn type->rig-zone
  "Converts a runner's card type to a vector zone, e.g. 'Program' -> [:rig :program]"
  [type]
  (vec [:rig (-> type .toLowerCase keyword)]))

(defn get-server-type [zone]
  (or (#{:hq :rd :archives} zone) :remote))

(defn combine-subtypes
  "Takes an existing subtype-string, adds in the new-subtypes, and removes
  duplicates if is-distinct is truthy"
  [is-distinct subtype-string & new-subtypes]
  (let [do-distinct #(if is-distinct (distinct %) %)]
    (->> (split (or subtype-string " - ") #" - ")
         (concat new-subtypes)
         do-distinct
         (join " - "))))

(defn remove-subtypes
  "Takes an existing subtype-string and removes all instances of
  subtypes-to-remove"
  [subtype-string & subtypes-to-remove]
  (->> (split (or subtype-string " - ") #" - ")
       (remove #(some #{%} subtypes-to-remove))
       (join " - ")))

(defn remove-subtypes-once
  "Takes an existing subtype-string and removes one instance of
  each subtypes-to-remove"
  [subtype-string & subtypes-to-remove]
  (let [subtypes-to-remove (flatten subtypes-to-remove)
        types (split (or subtype-string " - ") #" - ")
        part (join " - " (remove-once #(= % (first subtypes-to-remove)) types))
        left (rest subtypes-to-remove)]
    (if-not (empty? left)
      (remove-subtypes-once part left)
      part)))

(defn pluralize
  "Makes a string plural based on the number n. Takes specific suffixes for singular and plural cases if necessary."
  ([string n] (pluralize string "s" n))
  ([string suffix n] (pluralize string "" suffix n))
  ([string single-suffix plural-suffix n]
   (if (or (= 1 n)
           (= -1 n))
     (str string single-suffix)
     (str string plural-suffix))))

(defn quantify
  "Ensures the string is correctly pluralized based on the number n."
  ([n string] (str n " " (pluralize string n)))
  ([n string suffix] (str n " " (pluralize string suffix n)))
  ([n string single-suffix plural-suffix]
   (str n " " (pluralize string single-suffix plural-suffix n))))

(defn in-coll?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn positions
  "Returns the positions of elements in coll matching pred"
  [pred coll]
  (keep-indexed (fn [idx x]
                  (when (pred x)
                    idx))
                coll))
