(ns game.macros
  (:require [clojure.tools.analyzer.jvm :as a.j]
            [clojure.tools.analyzer.ast :as ast]))

(defn run-server
  [state]
  (when (:run @state)
    (get-in @state (concat [:corp :servers] (:server (:run @state))))))

(defn run-ices
  [state]
  (:ices (run-server state)))

(defn run-position
  [state]
  (get-in @state [:run :position]))

(def forms
  (->>
    '[runner (:runner @state)
      corp (:corp @state)
      current-run (:run @state)
      run-server (game.macros/run-server state)
      run-ices (game.macros/run-ices state)
      run-position (game.macros/run-position state)
      current-ice (let [position (game.macros/run-position state)
                        ices (game.macros/run-ices state)]
                    (when (and position
                               (pos? position)
                               (<= position (count ices)))
                      (nth ices (dec position))))
      corp-reg (get-in @state [:corp :register])
      corp-reg-last (get-in @state [:corp :register-last-turn])
      runner-reg (get-in @state [:runner :register])
      runner-reg-last (get-in @state [:runner :register-last-turn])
      target (first targets)
      installed (#{:rig :servers} (first (:zone (get-nested-host card))))
      remotes (get-remote-names state)
      servers (zones->sorted-names (get-zones state))
      unprotected (let [server (second (:zone (if (:host card)
                                                (get-card state (:host card)) card)))]
                    (empty? (get-in @state [:corp :servers server :ices])))
      runnable-servers (zones->sorted-names (get-runnable-zones state))
      hq-runnable (not (:hq (get-in (:runner @state) [:register :cannot-run-on-server])))
      rd-runnable (not (:rd (get-in (:runner @state) [:register :cannot-run-on-server])))
      archives-runnable (not (:archives (get-in (:runner @state) [:register :cannot-run-on-server])))
      tagged (is-tagged? state)
      has-bad-pub (or (pos? (:bad-publicity (:corp @state)))
                      (pos? (:has-bad-pub (:corp @state))))
      this-server (let [s (-> card :zone rest butlast)
                        r (:server (:run @state))]
                    (and (= (first r) (first s))
                         (= (last r) (last s))))]
    (partition 2)
    (map (juxt first identity))
    (into {})))

(defn emit-only
  [needed-locals]
  (mapcat identity
          (for [x needed-locals
                :when (contains? forms x)]
            (get forms x))))

;; Taken from https://github.com/Bronsa/tools.analyzer.jvm.deps/commit/8c7c3936e6f73e85f9e7cc122a2142c43d459c12
;; TODO: Switch from inlined function to require'd package when new version drops.
(defn find-undefined-locals
  "Takes a form and returns a set of all the free locals in it"
  [expr]
  (->> (binding [a.j/run-passes identity]
         (a.j/analyze expr (a.j/empty-env)))
       ast/nodes
       (filter (fn [{:keys [op]}] (= op :maybe-class)))
       (map :class)
       (remove (fn [x] (-> x str (.contains "."))))
       (into #{})))

(defmacro req [& expr]
  (let [needed-locals (find-undefined-locals expr)
        nls (emit-only needed-locals)]
    `(fn ~['state 'side 'eid 'card 'targets]
       (let [~@nls]
         ~@expr))))

(defn effect-state-handler
  [expr]
  (for [body expr]
    (if (#{:runner :corp} (second body))
      (concat [(first body) 'state (second body)] (drop 2 body))
      (concat [(first body) 'state 'side] (rest body)))))

(defmacro effect [& expr]
  `(req ~@(effect-state-handler expr)))

(defmacro msg [& expr]
  `(str (req ~@expr)))

(defmacro wait-for
  ([action & expr]
   (let [reqmac `(fn [state# side# eid# card# target#]
                   (let [~'async-result (:result eid#)]
                     ~@expr))
   ;; this creates a five-argument function to be resolved later,
   ;; without overriding any local variables name state, card, etc.
         totake (if (= 'apply (first action)) 4 3)
         th (nth action totake)]
     `(let [~'use-eid# (and (map? ~th) (:eid ~th))
            ~'new-eid# (if ~'use-eid# ~th (game.core/make-eid ~'state))]
        (~'register-effect-completed ~'state ~'side ~'new-eid# ~(when (resolve 'card) ~'card) ~reqmac)
        (if ~'use-eid#
          ~(concat (take totake action) (list 'new-eid#) (drop (inc totake) action))
          ~(concat (take totake action) (list 'new-eid#) (drop totake action)))))))

(defmacro continue-ability
  [state side ability card targets]
  `(game.core/resolve-ability ~state ~side (assoc ~ability :eid ~'eid) ~card ~targets))

(defmacro when-let*
  ([bindings & body]
   (if (seq bindings)
     `(when-let [~(first bindings) ~(second bindings)]
        (when-let* ~(drop 2 bindings) ~@body))
     `(do ~@body))))
