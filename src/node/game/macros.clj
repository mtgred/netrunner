(ns game.macros)

(defmacro effect [& expr]
  `(fn ~['state 'side 'card]
     ~(let [actions (map #(if (#{:runner :corp} (second %))
                            (concat [(first %) 'state (second %)] (drop 2 %))
                            (concat [(first %) 'state 'side] (rest %)))
                         expr)]
        `(let ~['runner '(:runner @state)
                'corp '(:corp @state)]
           ~@actions))))

(defmacro req [& expr]
  `(fn ~['state]
     (let ~['runner '(:runner @state)
            'corp '(:corp @state)
            'tagged '(> (get-in @state [:runner :tag]) 0)]
        ~@expr)))
