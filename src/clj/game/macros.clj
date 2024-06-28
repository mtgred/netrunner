(ns game.macros
  (:require [clojure.tools.analyzer.jvm :as a.j]
            [clojure.tools.analyzer.ast :as ast]))

(def forms
  (->>
    '[
      runner (:runner @state)
      corp (:corp @state)
      run (:run @state)
      run-server (get-in @state (concat [:corp :servers] (:server (:run @state))))
      run-ices (get-in @state (concat [:corp :servers] (:server (:run @state)) [:ices]))
      run-position (get-in @state [:run :position])
      current-ice (game.core.ice/get-current-ice state)
      corp-reg (get-in @state [:corp :register])
      corp-reg-last (get-in @state [:corp :register-last-turn])
      runner-reg (get-in @state [:runner :register])
      runner-reg-last (get-in @state [:runner :register-last-turn])
      target (let [t (first targets)]
               (if (and (map? t)
                        (contains? t :uuid)
                        (contains? t :value))
                 (:value t)
                 t))
      context (let [t (first targets)]
                (if (and (map? t)
                         (contains? t :uuid)
                         (contains? t :value))
                  (:value t)
                  t))
      installed (#{:rig :servers} (first (game.core.card/get-zone card)))
      remotes (game.core.board/get-remote-names state)
      servers (game.core.servers/zones->sorted-names (game.core.board/get-zones state))
      unprotected (let [server (second (game.core.card/get-zone card))]
                    (empty? (get-in @state [:corp :servers server :ices])))
      runnable-servers (game.core.servers/zones->sorted-names
                         (game.core.runs/get-runnable-zones state side eid card nil))
      hq-runnable (some #{:hq} (game.core.runs/get-runnable-zones state))
      rd-runnable (some #{:rd} (game.core.runs/get-runnable-zones state))
      archives-runnable (some #{:archives} (game.core.runs/get-runnable-zones state))
      tagged (jinteki.utils/is-tagged? state)
      ;; only intended for use in event listeners on (pre-/post-, un-)successful-run or run-ends
      ;; true if the run was initiated by this card
      this-card-run (and (get-in card [:special :run-id])
                         (= (get-in card [:special :run-id])
                            (:run-id (first targets))))
      this-server (let [s (game.core.card/get-zone card)
                        r (:server (:run @state))]
                    (= (second s) (first r)))
      corp-currently-drawing (seq (peek (get-in @state [:corp :register :currently-drawing])))
      runner-currently-drawing (seq (peek (get-in @state [:runner :register :currently-drawing])))
      ]
    (partition 2)
    (map (juxt first identity))
    (into {})))

;; Taken from https://github.com/Bronsa/tools.analyzer.jvm.deps/commit/8c7c3936e6f73e85f9e7cc122a2142c43d459c12
;; TODO: Switch from this inlined function to requiring the right package when the new version drops.
(defn- find-undefined-locals
  "Takes a form and returns a set of all the free locals in it"
  [expr]
  (->> (binding [a.j/run-passes identity]
         (a.j/analyze expr (a.j/empty-env)))
       ast/nodes
       (filter (fn [{:keys [op]}] (= op :maybe-class)))
       (map :class)
       (remove (fn [x] (-> x str (.contains "."))))
       (into #{})))

(defn- emit-only
  [needed-locals]
  (mapcat identity
          (for [x needed-locals
                :when (contains? forms x)]
            (get forms x))))

(defn- effect-state-handler
  [expr]
  (for [body expr]
    (if (#{:runner :corp} (second body))
      (concat [(first body) 'state (second body)] (drop 2 body))
      (concat [(first body) 'state 'side] (rest body)))))

(defmacro req [& expr]
  (let [needed-locals (find-undefined-locals expr)
        nls (emit-only needed-locals)]
    `(fn ~['state 'side 'eid 'card 'targets]
       (assert (or (nil? (:source ~'eid)) (:cid (:source ~'eid)))
               (str ":source should be a card, received: " (:source ~'eid)))
       (let [~@nls]
         ~@expr))))

(defmacro effect [& expr]
  `(req ~@(effect-state-handler expr)))

(defmacro msg [& expr]
  `(req (str ~@expr)))

(defmacro wait-for
  [& body]
  (let [[binds action] (if (vector? (first body))
                         (first body)
                         [[{'async-result :result}] (first body)])
        expr (next body)
        abnormal? (#{'handler 'payable?} (first action))
        to-take (if abnormal? 4 3)
        fn-name (gensym (name (first action)))
        [_ state _ eid?] (if abnormal? (next action) action)]
    `(let [eid?# ~eid?
           use-eid# (and (map? eid?#) (:eid eid?#))
           existing-eid# ~(when (contains? &env 'eid) 'eid)
           new-eid# (if use-eid# eid?# (game.core.eid/make-eid ~state existing-eid#))]
       (game.core.eid/register-effect-completed
         ~state new-eid#
         (fn ~fn-name ~(if (vector? binds) binds [binds])
           ~@expr))
       (if use-eid#
         (~@(take to-take action) new-eid# ~@(drop (inc to-take) action))
         (~@(take to-take action) new-eid# ~@(drop to-take action))))))

(comment
  (macroexpand
    '(wait-for (draw state :corp (make-eid state) 1)
               (system-msg state :corp async-result)
               (effect-completed state :corp eid)))
  (macroexpand
    '(wait-for [{card :result} (draw state :corp (make-eid state) 1)]
               (system-msg state :corp card)
               (effect-completed state :corp eid)))
  )

(defmacro continue-ability
  [state side ability card targets]
  `(let [ability# ~ability
         ability# (if (:eid ability#) ability# (assoc ability# :eid ~'eid))]
     (game.core.engine/resolve-ability ~state ~side ability#  ~card ~targets)))

(defmacro when-let*
  ([bindings & body]
   (if (seq bindings)
     `(when-let [~(first bindings) ~(second bindings)]
        (when-let* ~(drop 2 bindings) ~@body))
     `(do ~@body))))
