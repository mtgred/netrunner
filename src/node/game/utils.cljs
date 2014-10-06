(ns game.utils)

(def cid (atom 0))

(defn make-cid []
  (swap! cid inc))

(defn merge-costs [costs]
  (vec (reduce #(let [key (first %2) value (last %2)]
              (assoc %1 key (+ (or (key %1) 0) value )))
           {} (partition 2 (flatten costs)))))

(defn remove-once [pred coll]
  (let [[head tail] (split-with pred coll)]
    (concat head (rest tail))))

(defn has? [card property value]
  (when-let [p (property card)]
    (> (.indexOf p value) -1)))

(defn zone [zone coll]
  (let [dest (if (sequential? zone) zone [zone])]
    (map #(assoc % :zone dest) coll)))

(defn to-keyword [string]
  (keyword (.toLowerCase string)))
