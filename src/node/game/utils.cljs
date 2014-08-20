(ns game.utils)

(defn add-costs [cost & costs]
  (reduce (fn [cost1 cost2]
            (reduce-kv (fn [cost k v]
                         (if (k cost)
                           (update-in cost [k] #(+ % v))
                           (assoc cost k v)))
                       cost1 cost2))
          cost costs))

(defn remove-once [pred coll]
  (let [[head tail] (split-with pred coll)]
    (concat head (rest tail))))

(defn has? [card property value]
  (> (.indexOf (property card) value) -1))

