(ns game.macros)

(defmacro effect [& expr]
  `(fn ~['state 'side 'card]
     ~(let [actions (map #(if (#{:runner :corp} (second %))
                            (concat [(first %) 'state (second %)] (drop 2 %))
                            (concat [(first %) 'state 'side] (rest %)))
                         expr)]
        `(let ~['runner '(:runner @state)
                'corp '(:corp @state)
                'register '(:register @state)]
           ~@actions))))

(defmacro req [& expr]
  `(fn ~['state]
     (let ~['runner '(:runner @state)
            'corp '(:corp @state)
            'register '(:register @state)
            'tagged '(> (get-in @state [:runner :tag]) 0)]
        ~@expr)))

(defmacro msg [& expr]
  `(fn ~['state 'side 'card] (str ~@expr)))
