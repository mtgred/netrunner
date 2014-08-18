(ns game.macros)

(defmacro do! [{:keys [cost effect]}]
  `(fn ~['state 'side 'args]
     ~@(map #(concat [(first %) 'state 'side] (rest %)) effect)))
