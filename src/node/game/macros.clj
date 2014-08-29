(ns game.macros)

(defmacro effect [& expr]
  `(fn ~['state 'side 'card]
     ~(let [actions (map #(concat [(first %) 'state 'side] (rest %)) expr)]
        `(let ~['runner '(:runner @state)
                'corp '(:corp @state)]
           ~@actions))))

(defmacro req [& expr]
  `(fn ~['state]
     (let ~['runner '(:runner @state)
            'corp '(:corp @state)]
        ~@expr)))
