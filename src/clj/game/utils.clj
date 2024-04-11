(ns game.utils
  (:require
    [jinteki.cards :refer [all-cards]]
    [clojure.string :as str]
    [clj-uuid :as uuid]))

(defn make-cid []
  (uuid/to-string (uuid/v4)))

(defn server-card
  ([title] (server-card title true))
  ([title strict?]
   (let [card (get @all-cards title)]
     (cond
       (and title card) card
       (or (= title "Corp Basic Action Card") (= title "Runner Basic Action Card")) {}
       :else (when strict?
               (throw (Exception. (str "Tried to select server-card for " title))))))))

(defn server-cards
  []
  (vals @all-cards))

(defn safe-zero?
  "`zero?` throws up on non numbers, so this is a safe version."
  [n]
  ((fnil zero? 1) n))

(defn remove-once [pred coll]
  (let [[head tail] (split-with (complement pred) coll)]
    (vec (concat head (rest tail)))))

(defn to-keyword [string]
  (cond
    (= "[Credits]" string) :credit
    (string? string) (keyword (str/lower-case string))
    :else string))

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
  (catch Exception _ nil)))

(def safe-split (fnil str/split ""))

(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
  nested structure. keys is a sequence of keys. Any empty maps that result
  will not be present in the new structure."
  [m [k & ks]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))

(defn used-this-turn?
  "Returns true if a card has been used this turn"
  [cid state]
  (contains? (get-in @state [:per-turn]) cid))

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

(defn enumerate-str
  "Joins a collection to a string, seperated by commas and 'and' in front of
  the last item. If collection only has one item, justs returns that item
  without seperators. Returns an empty string if coll is empty."
  [strings]
  (if (<= (count strings) 2)
    (str/join " and " strings)
    (str (apply str (interpose ", " (butlast strings))) ", and " (last strings))))

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
