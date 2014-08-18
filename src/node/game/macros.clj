(ns game.macros)

(defmacro ability [{:keys [cost effect]}]
  `{:cost ~cost
    :effect (fn ~['state 'side 'args]
              ~(let [actions (map #(concat [(first %) 'state 'side] (rest %)) effect)]
                 `(do ~@actions)))})
