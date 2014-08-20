(ns game.macros)

(defmacro effect [& expr]
  `(fn ~['state 'side 'args]
     ~(let [actions (map #(concat [(first %) 'state 'side] (rest %)) expr)]
        `(let ~['grip '(get-in state [:runner :hand])
                'stack '(get-in state [:runner :deck])
                'heap '(get-in state [:runner :discard])
                'hq '(get-in state [:corp :hand])
                'rd '(get-in state [:corp :deck])
                'archive '(get-in state [:corp :discard])
                'rig '(get-in state [:runner :rig])
                'servers '(get-in state [:corp :servers])]
           ~@actions))))
