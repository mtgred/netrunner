(ns tasks.stress.bot
  "Decision engine for stress-test players. Decides one command at a time from
  the client-visible game state, leaning on the :playable flags, prompt choice
  uuids and :selectable lists the server computes for real clients. It plays
  legally and keeps games progressing; it does not try to play well."
  (:require
    [clojure.string :as str]))

(defn- rand-nth* [^java.util.Random rng coll]
  (when (seq coll)
    (nth (vec coll) (.nextInt rng (count coll)))))

(defn- chance? [^java.util.Random rng p]
  (< (.nextDouble rng) p))

(defn card-ref [card]
  (select-keys card [:cid :zone :side :host :type]))

(defn find-card
  "Finds a full card map by cid anywhere in either player's visible state.
  The :zone guard skips the zoneless card stubs echoed in prompts."
  [state cid]
  (->> [(:corp state) (:runner state)]
       (tree-seq coll? seq)
       (filter #(and (map? %) (= cid (:cid %)) (:zone %)))
       first))

;; prompts

(defn- answer-buttons [prompt rng]
  (when-let [choice (rand-nth* rng (:choices prompt))]
    ["choice" {:eid (:eid prompt) :choice {:uuid (:uuid choice)}}]))

(defn- answer-select [state prompt rng]
  (if-let [card (some->> (:selectable prompt) (rand-nth* rng) (find-card state))]
    ["select" {:card (card-ref card) :eid (:eid prompt)}]
    (answer-buttons prompt rng)))

(defn- answer-prompt [state _side prompt rng]
  (let [choices (:choices prompt)]
    (cond
      (= "mulligan" (:prompt-type prompt))
      (when-let [keep (first (filter #(= "Keep" (:value %)) choices))]
        ["choice" {:eid (:eid prompt) :choice {:uuid (:uuid keep)}}])

      (= "select" (:prompt-type prompt))
      (answer-select state prompt rng)

      (sequential? choices)
      (answer-buttons prompt rng)

      (:card-title choices)
      ["choice" {:eid (:eid prompt) :choice "Hedge Fund"}]

      (:number choices)
      ["choice" {:eid (:eid prompt) :choice (.nextInt rng (inc (:number choices)))}]

      (or (int? choices) (= "credit" choices) (= "trace" (:prompt-type prompt)) (:base choices))
      ["choice" {:eid (:eid prompt) :choice 0}]

      :else nil)))

;; runs

(defn- run-server-key [state]
  (some-> (get-in state [:run :server]) first keyword))

(defn- approached-ice [state]
  (let [pos (get-in state [:run :position])
        ices (get-in state [:corp :servers (run-server-key state) :ices])]
    (when (and pos (seq ices) (<= 1 pos (count ices)))
      (nth ices (dec pos)))))

(defn- icebreakers [state]
  (->> (get-in state [:runner :rig :program])
       (filter (fn [c] (some #(str/includes? % "breaker") (map str/lower-case (:subtypes c [])))))))

(def ^:private continue-phases
  #{"initiation" "approach-ice" "encounter-ice" "movement"})

(defn- continue* [state side]
  (when (and (continue-phases (get-in state [:run :phase]))
             (not= (name side) (get-in state [:run :no-action])))
    ["continue" nil]))

(defn- corp-run-action [state rng]
  (let [ice (approached-ice state)
        phase (get-in state [:run :phase])]
    (cond
      (and ice (= "approach-ice" phase) (not (:rezzed ice)) (chance? rng 0.4))
      ["rez" {:card (card-ref ice)}]

      (and ice (= "encounter-ice" phase) (:rezzed ice) (chance? rng 0.5))
      ["unbroken-subroutines" {:card (card-ref ice)}]

      :else (continue* state :corp))))

(defn- runner-run-action [state rng]
  (let [ice (approached-ice state)]
    (cond
      (and ice (= "encounter-ice" (get-in state [:run :phase])) (:rezzed ice) (chance? rng 0.5))
      (if-let [breaker (rand-nth* rng (icebreakers state))]
        ["dynamic-ability" {:dynamic "auto-pump-and-break" :card (card-ref breaker)}]
        (continue* state :runner))

      (chance? rng 0.05) ["jack-out" nil]
      :else (continue* state :runner))))

;; turns

(defn- playable-from-hand [me]
  (filter :playable (:hand me)))

(defn- corp-install-server [card rng]
  (case (:type card)
    ("ICE" "Upgrade") (rand-nth* rng ["HQ" "R&D" "Archives" "New remote"])
    "New remote"))

(defn- play-card [side me rng]
  (when-let [card (rand-nth* rng (playable-from-hand me))]
    (if (and (= :corp side) (not= "Operation" (:type card)))
      ["play" {:card (card-ref card) :server (corp-install-server card rng)}]
      ["play" {:card (card-ref card)}])))

(defn- installed-cards [state side]
  (if (= :corp side)
    (mapcat (fn [[_ server]] (concat (:content server) (:ices server)))
            (get-in state [:corp :servers]))
    (mapcat #(get-in state [:runner :rig %]) [:program :hardware :resource])))

(defn- use-ability [state side rng]
  (let [cards (for [card (installed-cards state side)
                    [idx ab] (map-indexed vector (:abilities card))
                    :when (:playable ab)]
                [card idx])]
    (when-let [[card idx] (rand-nth* rng cards)]
      ["ability" {:card (card-ref card) :ability idx}])))

(defn- advanceable [state]
  (->> (installed-cards state :corp)
       (remove #(= "ICE" (:type %)))
       (filter #(or (:advanceable %) (= "Agenda" (:type %))))))

(defn- scoreable [state]
  (->> (installed-cards state :corp)
       (filter #(and (= "Agenda" (:type %))
                     (>= (:advance-counter % 0) (:advancementcost % 99))))))

(defn- corp-turn-action [state me rng]
  (or (when-let [agenda (first (scoreable state))]
        ["score" {:card (card-ref agenda)}])
      (let [roll (.nextDouble rng)]
        (cond
          (< roll 0.35) (or (play-card :corp me rng) ["credit" nil])
          (< roll 0.50) (or (when-let [card (rand-nth* rng (advanceable state))]
                              (when (pos? (:credit me 0))
                                ["advance" {:card (card-ref card)}]))
                            ["credit" nil])
          (< roll 0.60) (or (use-ability state :corp rng) ["credit" nil])
          (< roll 0.75) ["draw" nil]
          :else ["credit" nil]))))

(defn- runner-turn-action [state me rng]
  (let [roll (.nextDouble rng)]
    (cond
      (< roll 0.35) (or (play-card :runner me rng) ["credit" nil])
      (< roll 0.55) (if-let [server (rand-nth* rng (:runnable-list me))]
                      ["run" {:server server}]
                      ["credit" nil])
      (< roll 0.65) (or (use-ability state :runner rng) ["credit" nil])
      (< roll 0.80) ["draw" nil]
      :else ["credit" nil])))

(defn- no-prompt-action [state side me rng]
  (let [my-turn? (= (name side) (:active-player state))]
    (cond
      (:run state)
      (if (= :corp side) (corp-run-action state rng) (runner-run-action state rng))

      (get state (if (= :corp side) :corp-phase-12 :runner-phase-12))
      (when my-turn? ["end-phase-12" nil])

      (and (:end-turn state) (not my-turn?))
      ["start-turn" nil]

      (and my-turn? (not (:end-turn state)))
      (if (pos? (:click me 0))
        (if (= :corp side)
          (corp-turn-action state me rng)
          (runner-turn-action state me rng))
        ["end-turn" nil])

      :else nil)))

(defn decide
  "Returns [command args] for `side` given its visible state, or nil when
  there is nothing sensible to do (e.g. waiting on the opponent)."
  [state side ^java.util.Random rng]
  (when (and state (not (:winner state)))
    (let [me (get state side)
          prompt (:prompt-state me)]
      (cond
        (nil? prompt) (no-prompt-action state side me rng)
        (= "waiting" (:prompt-type prompt)) nil
        (= "run" (:prompt-type prompt)) (no-prompt-action state side me rng)
        :else (answer-prompt state side prompt rng)))))
