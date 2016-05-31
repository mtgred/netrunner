(ns game.macros)

(defmacro effect [& expr]
  `(fn ~['state 'side 'eid 'card 'targets]
     ~(let [actions (map #(if (#{:runner :corp} (second %))
                            (concat [(first %) 'state (second %)] (drop 2 %))
                            (concat [(first %) 'state 'side] (rest %)))
                         expr)]
        `(let ~['runner '(:runner @state)
                'corp '(:corp @state)
                'corp-reg '(get-in @state [:corp :register])
                'runner-reg '(get-in @state [:runner :register])
                'current-ice '(when-let [run (:run @state)]
                                (when (> (or (:position run) 0) 0) ((:ices run) (dec (:position run)))))
                'target '(first targets)]
           ~@actions))))

(defmacro req [& expr]
  `(fn ~['state 'side 'eid 'card 'targets]
     (let ~['runner '(:runner @state)
            'corp '(:corp @state)
            'run '(:run @state)
            'current-ice '(when (and run (> (or (:position run) 0) 0)) ((:ices run) (dec (:position run))))
            'corp-reg '(get-in @state [:corp :register])
            'runner-reg '(get-in @state [:runner :register])
            'target '(first targets)
            'installed '(#{:rig :servers} (first (:zone card)))
            'remotes '(get-remote-names @state)
            'servers '(concat ["HQ" "R&D" "Archives"] remotes)
            'unprotected '(let [server (second (:zone (if (:host card)
                                                        (get-card state (:host card)) card)))]
                            (empty? (get-in @state [:corp :servers server :ices])))
            'runnable-servers '(let [servers (map zone->name
                                                  (keys (get-in @state [:corp :servers])))
                                     restricted (map zone->name
                                                     (keys (get-in @state [:runner :register :cannot-run-on-server])))]
                                 (remove (set restricted) (set servers)))
            'tagged '(or (> (:tagged runner) 0) (> (:tag runner) 0))
            'has-bad-pub '(or (> (:bad-publicity corp) 0) (> (:has-bad-pub corp) 0))
            'this-server '(let [s (-> card :zone rest butlast)
                                r (:server run)]
                            (and (= (first r) (first s))
                                 (= (last r) (last s))))]
        ~@expr)))

(defmacro msg [& expr]
  `(fn ~['state 'side 'eid 'card 'targets]
     (let ~['runner '(:runner @state)
            'corp '(:corp @state)
            'corp-reg '(get-in @state [:corp :register])
            'runner-reg '(get-in @state [:runner :register])
            'run '(:run @state)
            'current-ice '(when (and run (> (or (:position run) 0) 0)) ((:ices run) (dec (:position run))))
            'target '(first targets)
            'tagged '(or (> (:tagged runner) 0) (> (:tag runner) 0))]
       (str ~@expr))))

(defmacro when-completed
  ([action expr]
   (let [reqmac `(fn [~'state1 ~'side1 ~'eid1 ~'card1 ~'targets1] ~expr)
   ;; this creates a five-argument function to be resolved later,
   ;; without overriding any local variables name state, card, etc.
         th (nth action 3)
         use-eid (and (map? th) (:eid th))]
     `(let [~'new-eid (game.core/make-eid ~'state)]
        (~'register-effect-completed ~'state ~'side ~'new-eid ~'card ~reqmac)
        ~(let [action (concat (take 3 action)
                              (list (if use-eid th 'new-eid))
                              (drop (if use-eid 4 3) action))]
           action)))))

(defmacro final-effect [& expr]
  (macroexpand (apply list `(effect ~@expr ~(list (quote effect-completed) 'eid 'card)))))

(defmacro continue-ability
  [state side ability card targets]
  `(game.core/resolve-ability ~state ~side (assoc ~ability :eid ~'eid) ~card ~targets))