(ns game.macros)

(defmacro do! [{:keys [cost effect]}]
  `(fn ~['state 'side 'args]
     ~(let [actions (map #(concat [(first %) 'state 'side] (rest %)) effect)]
         (if cost
           `(when ~(concat '(pay state side) cost)
              (do ~@actions))
           `(do ~@actions)))))
