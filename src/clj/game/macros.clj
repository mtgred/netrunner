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
                'corp-reg-last '(get-in @state [:corp :register-last-turn])
                'runner-reg '(get-in @state [:runner :register])
                'runner-reg-last '(get-in @state [:runner :register-last-turn])
                'run-server '(when (:run @state)
                               (get-in @state (concat [:corp :servers] (:server (:run @state)))))
                'run-ices '(:ices run-server)
                'current-ice '(when-let [run-pos (:position (:run @state))]
                                (when (and (pos? run-pos) (<= run-pos (count (:ices run-server))))
                                  (nth (:ices run-server) (dec run-pos))))
                'target '(first targets)]
           ~@actions))))

(defmacro req [& expr]
  `(fn ~['state 'side 'eid 'card 'targets]
     (let ~['runner '(:runner @state)
            'corp '(:corp @state)
            'run '(:run @state)
            'run-server '(when (:run @state)
                           (get-in @state (concat [:corp :servers] (:server (:run @state)))))
            'run-ices '(:ices run-server)
            'run-position '(:position (:run @state))
            'current-ice '(when (and run-position
                                     (pos? run-position)
                                     (<= run-position (count (:ices run-server))))
                            (nth (:ices run-server) (dec run-position)))
            'corp-reg '(get-in @state [:corp :register])
            'corp-reg-last '(get-in @state [:corp :register-last-turn])
            'runner-reg '(get-in @state [:runner :register])
            'runner-reg-last '(get-in @state [:runner :register-last-turn])
            'target '(first targets)
            'installed '(#{:rig :servers} (first (:zone (get-nested-host card))))
            'remotes '(get-remote-names state)
            'servers '(zones->sorted-names (get-zones state))
            'unprotected '(let [server (second (:zone (if (:host card)
                                                        (get-card state (:host card)) card)))]
                            (empty? (get-in @state [:corp :servers server :ices])))
            'runnable-servers '(zones->sorted-names (get-runnable-zones state))
            'hq-runnable '(not (:hq (get-in runner [:register :cannot-run-on-server])))
            'rd-runnable '(not (:rd (get-in runner [:register :cannot-run-on-server])))
            'archives-runnable '(not (:archives (get-in runner [:register :cannot-run-on-server])))
            'tagged '(is-tagged? state)
            'has-bad-pub '(or (pos? (:bad-publicity corp)) (pos? (:has-bad-pub corp)))
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
            'corp-reg-last '(get-in @state [:corp :register-last-turn])
            'runner-reg '(get-in @state [:runner :register])
            'runner-reg-last '(get-in @state [:runner :register-last-turn])
            'run '(:run @state)
            'run-server '(when (:run @state)
                           (get-in @state (concat [:corp :servers] (:server (:run @state)))))
            'run-ices '(:ices run-server)
            'current-ice '(when-let [run-pos (:position (:run @state))]
                            (when (and (pos? run-pos) (<= run-pos (count (:ices run-server))))
                              (nth (:ices run-server) (dec run-pos))))
            'target '(first targets)
            'tagged '(is-tagged? state)]
       (str ~@expr))))

(defmacro wait-for
  ([action & expr]
   (let [reqmac `(fn [~'state1 ~'side1 ~'eid1 ~'card1 ~'target1]
                   (let [~'async-result (:result ~'eid1)]
                     ~@expr))
   ;; this creates a five-argument function to be resolved later,
   ;; without overriding any local variables name state, card, etc.
         totake (if (= 'apply (first action)) 4 3)
         th (nth action totake)]
     `(let [~'use-eid (and (map? ~th) (:eid ~th))
            ~'new-eid (if ~'use-eid ~th (game.core/make-eid ~'state))]
        (~'register-effect-completed ~'state ~'side ~'new-eid ~(when (resolve 'card) ~'card) ~reqmac)
        (if ~'use-eid
          ~(concat (take totake action) (list 'new-eid) (drop (inc totake) action))
          ~(concat (take totake action) (list 'new-eid) (drop totake action)))))))

(defmacro continue-ability
  [state side ability card targets]
  `(game.core/resolve-ability ~state ~side (assoc ~ability :eid ~'eid) ~card ~targets))
