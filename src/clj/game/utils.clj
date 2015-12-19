(ns game.utils)

(def cid (atom 0))

(defn make-cid []
  (swap! cid inc))

(defn abs [n] (max n (- n)))
  
(defn merge-costs [costs]
  (vec (reduce #(let [key (first %2) value (last %2)]
              (assoc %1 key (+ (or (key %1) 0) value)))
           {} (partition 2 (flatten costs)))))

(defn remove-once [pred coll]
  (let [[head tail] (split-with pred coll)]
    (vec (concat head (rest tail)))))

(defn has? [card property value]
  (when-let [p (property card)]
    (> (.indexOf p value) -1)))

(defn zone [zone coll]
  (let [dest (if (sequential? zone) (vec zone) [zone])]
    (map #(assoc % :zone dest) coll)))

(defn to-keyword [string]
  (if (string? string)
    (keyword (.toLowerCase string))
    string))

(defn side-str [k]
  "Takes a side key and converts it to a string (Runner/Corp)."
  (case k
    "Corp" "Corp"
    "Runner" "Runner"
    :corp "Corp"
    :runner "Runner"
    nil))

(defn side-key [s]
  "Takes a side string and converts it to a key (:runner/:corp)."
  (case s
    "Corp" :corp
    "Runner" :runner
    :corp :corp
    :runner :runner
    nil))


(defn capitalize [string]
  (str (Character/toUpperCase (first string)) (subs string 1)))

(defn costs-to-symbol [costs]
  (clojure.string/join ", " (map #(let [key (first %) value (last %)]
                                   (case key
                                     :credit (str value "[Credits]")
                                     :click (reduce str (for [i (range value)] "[Click]"))
                                     (str value (str key)))) (partition 2 (flatten costs)))))

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

(defn String->Num [s]
  (try
    (bigdec s)
  (catch Exception e nil)))

(def safe-split (fnil clojure.string/split ""))

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

(defn cancellable
  ([choices] (cancellable choices false))
  ([choices sorted]
   (if sorted
     (conj (vec (sort-by :title choices)) "Cancel")
     (conj (vec choices) "Cancel"))))