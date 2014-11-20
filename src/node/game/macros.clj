(ns game.macros)

(defmacro effect [& expr]
  `(fn ~['state 'side 'card 'targets]
     ~(let [actions (map #(if (#{:runner :corp} (second %))
                            (concat [(first %) 'state (second %)] (drop 2 %))
                            (concat [(first %) 'state 'side] (rest %)))
                         expr)]
        `(let ~['runner '(:runner @state)
                'corp '(:corp @state)
                'target '(first targets)
                'register '(:register @state)]
           ~@actions))))

(defmacro req [& expr]
  `(fn ~['state 'side 'card 'targets]
     (let ~['runner '(:runner @state)
            'corp '(:corp @state)
            'run '(:run @state)
            'access '(:access @state)
            'corp-reg '(get-in @state [:corp :register])
            'runner-reg '(get-in @state [:runner :register])
            'target '(first targets)
            'installed '(#{:rig :servers} (first (:zone card)))
            'servers '(concat ["HQ" "R&D" "Archives"]
                              (map #(str "Server " %) (range (count (get-in corp [:servers :remote])))))
            'tagged '(> (get-in @state [:runner :tag]) 0)]
        ~@expr)))

(defmacro msg [& expr]
  `(fn ~['state 'side 'card 'targets]
     (let ~['runner '(:runner @state)
            'corp '(:corp @state)
            'target '(first targets)]
       (str ~@expr))))
