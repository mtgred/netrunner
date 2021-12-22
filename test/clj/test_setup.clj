(ns test-setup
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.board :as board]
   [game.core.card :refer [active? get-card get-counters get-title installed?
                           rezzed?]]
   [game.core.eid :as eid]
   [game.core.ice :refer [active-ice?]]
   [game.core.process-actions :as pa]
   [game.macros :refer [wait-for]]
   [game.utils :as utils :refer [same-card? server-card side-str]]
   [jinteki.cards :refer [all-cards]]
   [jinteki.utils :as jutils]))

(defmacro error-wrapper [form]
  `(try ~form
        (catch clojure.lang.ExceptionInfo ex#
          (let [msg# (.getMessage ^Throwable ex#)
                form# (:form (ex-data ex#))
                pred# (:pred (ex-data ex#))
                values# (:values (ex-data ex#))
                result# (:result (ex-data ex#))]
            (~do-report {:type :fail
                         :message msg#
                         :expected form#
                         :actual (list '~'not (cons pred# values#))})
            result#))))

(defn is'-predicate
  [msg form]
  (let [pred (first form)
        args (rest form)]
    `(let [values# (list ~@args)
           result# (apply ~pred values#)]
       (if result#
         (~do-report {:type :pass, :message ~msg,
                      :expected '~form, :actual (cons ~pred values#)})
         (throw (ex-info ~msg {:form '~form
                               :pred '~pred
                               :values values#
                               :result result#}))))))

(defn is'-any
  [msg form]
  `(let [value# ~form]
     (if value#
       (~do-report {:type :pass, :message ~msg,
                    :expected '~form, :actual value#})
       (~do-report {:type :fail, :message ~msg,
                    :expected '~form, :actual value#}))
     value#))

(defmacro is'
  ([form] `(is' ~form nil))
  ([form msg]
   (if (and (sequential? form)
            (~function? (first form)))
     (is'-predicate msg form)
     (is'-any msg form))))

;;; helper functions for prompt interaction
(defn get-prompt
  [state side]
  (-> @state side :prompt seq first))

(defn prompt-is-type?
  [state side prompt-type]
  (let [prompt (get-prompt state side)]
    (= prompt-type (:prompt-type prompt))))

(defn prompt-is-card?
  [state side card]
  (let [prompt (get-prompt state side)]
    (and (:cid card)
         (get-in prompt [:card :cid])
         (= (:cid card) (get-in prompt [:card :cid])))))

(defn no-prompt?
  [state side]
  (let [prompt (get-prompt state side)]
    (or (empty? prompt)
        (= :run (:prompt-type prompt)))))

(defn expect-type
  [type-name choice]
  (str "Expected a " type-name ", received [ " choice
       " ] of type " (type choice) "."))

(defn click-card-impl
  [state side card]
  (let [prompt (get-prompt state side)]
    (cond
      ;; Card and prompt types are correct
      (and (prompt-is-type? state side :select)
           (or (map? card)
               (string? card)))
      (if (map? card)
        (pa/process-action "select" state side {:card card})
        (let [all-cards (board/get-all-cards state)
              matching-cards (filter #(= card (get-title %)) all-cards)]
          (if (= (count matching-cards) 1)
            (pa/process-action "select" state side {:card (first matching-cards)})
            (is' (= 1 (count matching-cards))
                 (str "Expected to click card [ " card
                      " ] but found " (count matching-cards)
                      " matching cards. Current prompt is: " prompt)))))
      ;; Prompt isn't a select so click-card shouldn't be used
      (not (prompt-is-type? state side :select))
      (is' (true? (prompt-is-type? state side :select))
           (str "click-card should only be used with prompts "
                "requiring the user to click on cards on table"))
      ;; Prompt is a select, but card isn't correct type
      (not (or (map? card)
               (string? card)))
      (is' (true? (or (map? card) (string? card))) (expect-type "card string or map" card)))))

(defmacro click-card
  "Resolves a 'select prompt' by clicking a card. Takes a card map or a card name."
  [state side card]
  `(error-wrapper (click-card-impl ~state ~side ~card)))

(defn click-prompt-impl
  [state side choice & args]
  (let [prompt (get-prompt state side)
        choices (:choices prompt)]
    (cond
      ;; Integer prompts
      (or (= choices :credit)
          (:counter choices)
          (:number choices))
      (try
        (let [parsed-number (Integer/parseInt choice)]
          (when-not (pa/process-action "choice" state side {:choice parsed-number})
            (is' (not true) (str "Parsed number " parsed-number " is incorrect somehow"))))
        (catch Exception _
          (is' (number? (Integer/parseInt choice)) (expect-type "number string" choice))))

      (= :trace (:prompt-type prompt))
      (try
        (let [int-choice (Integer/parseInt choice)
              under (<= int-choice (:choices prompt))]
          (when-not (and under
                         (pa/process-action "choice" state side {:choice int-choice}))
            (is' (<= int-choice (:choices prompt))
                 (str (side-str side) " expected to pay [ "
                      int-choice " ] to trace but couldn't afford it."))))
        (catch Exception _
          (is' (number? (Integer/parseInt choice))
               (expect-type "number string" choice))))

      ;; List of card titles for auto-completion
      (:card-title choices)
      (when-not (pa/process-action "choice" state side {:choice choice})
        (is' (true? (or (map? choice) (string? choice))) (expect-type "card string or map" choice)))

      ;; Default text prompt
      :else
      (let [choice-fn #(or (= choice (:value %))
                           (= choice (get-in % [:value :title]))
                           (same-card? choice (:value %)))
            idx (or (:idx (first args)) 0)
            chosen (nth (filter choice-fn choices) idx nil)]
        (when-not (and chosen (pa/process-action "choice" state side {:choice {:uuid (:uuid chosen)}}))
          (is' (= choice (mapv :value choices))
               (str (side-str side) " expected to click [ "
                    (if (string? choice) choice (:title choice ""))
                    " ] but couldn't find it. Current prompt is: " prompt)))))))

(defmacro click-prompt
  "Clicks a button in a prompt. {choice} is a string or map only, no numbers."
  [state side choice & args]
  `(error-wrapper (click-prompt-impl ~state ~side ~choice ~@args)))

(defn last-log-contains?
  [state content]
  (some? (re-find (re-pattern content)
                  (-> @state :log last :text))))

(defmethod assert-expr 'last-log-contains?
  [msg form]
  `(let [state# ~(nth form 1)
         content# ~(nth form 2)
         log# (-> @state# :log last :text)
         found# ~form]
     (~do-report
       {:type (if found# :pass :fail)
        :actual log#
        :expected content#
        :message ~msg})
     found#))

(defn second-last-log-contains?
  [state content]
  (some? (re-find (re-pattern content)
                  (-> @state :log butlast last :text))))

(defmethod assert-expr 'second-last-log-contains?
  [msg form]
  `(let [state# ~(nth form 1)
         content# ~(nth form 2)
         log# (-> @state# :log butlast last :text)
         found# ~form]
     (~do-report
       {:type (if found# :pass :fail)
        :actual log#
        :expected content#
        :message ~msg})
     found#))

(defn last-n-log-contains?
  [state n content]
  (some? (re-find (re-pattern content)
                  (:text (nth (-> @state :log reverse) n)))))

(defmethod assert-expr 'last-n-log-contains?
  [msg form]
  `(let [state# ~(nth form 1)
         n# ~(nth form 2)
         content# ~(nth form 3)
         log# (:text (nth (-> @state# :log reverse) n#))
         found# ~form]
     (~do-report
       {:type (if found# :pass :fail)
        :actual log#
        :expected content#
        :message ~msg})
     found#))

(defmethod assert-expr 'prompt-is-type?
  [msg form]
  `(let [state# ~(nth form 1)
         side# ~(nth form 2)
         given-type# ~(nth form 3)
         prompt-type# (-> @state# side# :prompt :prompt-type)
         found# ~form]
     (~do-report
       {:type (if found# :pass :fail)
        :actual prompt-type#
        :expected given-type#
        :message ~msg})
     found#))

(defmethod assert-expr 'prompt-is-card?
  [msg form]
  `(let [state# ~(nth form 1)
         side# ~(nth form 2)
         card# ~(nth form 3)
         prompt-card# (-> @state# side# :prompt :card)
         found# ~form]
     (~do-report
       {:type (if found# :pass :fail)
        :actual prompt-card#
        :expected card#
        :message ~msg})
     found#))

(defmethod assert-expr 'no-prompt?
  [msg form]
  `(let [state# ~(nth form 1)
         side# ~(nth form 2)
         prompt# (-> @state# side# :prompt)
         prompt-type# (-> @state# side# :prompt :prompt-type)
         found# ~form]
     (~do-report
       {:type (if found# :pass :fail)
        :actual (select-keys (first prompt#) [:msg :prompt-type])
        :expected "No prompt or :prompt-type of :run"
        :message ~msg})
     found#))

;; Card information and definitions
(defn load-cards []
  (->> (io/file "data/cards.edn")
       slurp
       edn/read-string
       merge))

(defn load-all-cards []
  (when (empty? @all-cards)
    (->> (load-cards)
         (map (juxt :title identity))
         (into {})
         (reset! all-cards))
    (require '[game.cards.agendas]
             '[game.cards.assets]
             '[game.cards.basic]
             '[game.cards.events]
             '[game.cards.hardware]
             '[game.cards.ice]
             '[game.cards.identities]
             '[game.cards.operations]
             '[game.cards.programs]
             '[game.cards.resources]
             '[game.cards.upgrades])))
(load-all-cards)

;; General utilities necessary for starting a new game
(defn find-card
  "Copied from core so we can check printed title too"
  [title from]
  (some #(when (= (get-title %) title) %) from))

(defn starting-hand
  "Moves all cards in the player's hand to their draw pile, then moves the specified card names
  back into the player's hand."
  [state side cards]
  (doseq [c (get-in @state [side :hand])]
    (core/move state side c :deck))
  (doseq [ctitle cards]
    (core/move state side (find-card ctitle (get-in @state [side :deck])) :hand)))

(defn take-credits
  "Take credits for n clicks, or if no n given, for all remaining clicks of a side.
  If all clicks are used up, end turn and start the opponent's turn."
  [state side & n]
  (let [other (if (= side :corp) :runner :corp)]
    (dotimes [_ (or (first n) (get-in @state [side :click]))]
      (core/process-action "credit" state side nil))
    (when (zero? (get-in @state [side :click]))
      (core/process-action "end-turn" state side nil)
      (core/process-action "start-turn" state other nil))))

(defmacro start-turn
  [state side]
  `(do (is (and (empty? (get-in @~state [:corp :prompt]))
                (empty? (get-in @~state [:runner :prompt]))) "Players have prompts open")
       (when (and (empty? (get-in @~state [:corp :prompt]))
                  (empty? (get-in @~state [:runner :prompt])))
         (core/process-action "start-turn" ~state ~side nil))))


;; Deck construction helpers
(defn qty [card amt]
  (when (pos? amt)
    (repeat amt card)))

(defn card-vec->card-map
  [side [card amt]]
  (let [loaded-card (if (string? card) (server-card card) card)]
    (when-not loaded-card
      (throw (Exception. (str card " not found in @all-cards"))))
    (when (not= side (:side loaded-card))
      (throw (Exception. (str (:title loaded-card) " is not a " side " card"))))
    {:card loaded-card
     :qty amt}))

(defn transform
  [side cards]
  (->> cards
       flatten
       (filter string?)
       frequencies
       (map #(card-vec->card-map side %))
       seq))

(defn make-decks
  [{:keys [corp runner options]}]
  {:corp {:deck (or (transform "Corp" (conj (:deck corp)
                                            (:hand corp)
                                            (:discard corp)))
                    (transform "Corp" (qty "Hedge Fund" 3)))
          :hand (when-let [hand (:hand corp)]
                  (flatten hand))
          :discard (when-let [discard (:discard corp)]
                     (flatten discard))
          :identity (when-let [id (or (:id corp) (:identity corp))]
                      (server-card id))
          :credits (:credits corp)
          :bad-pub (:bad-pub corp)}
   :runner {:deck (or (transform "Runner" (conj (:deck runner)
                                                (:hand runner)
                                                (:discard runner)))
                      (transform "Runner" (qty "Sure Gamble" 3)))
            :hand (when-let [hand (:hand runner)]
                    (flatten hand))
            :discard (when-let [discard (:discard runner)]
                       (flatten discard))
            :identity (when-let [id (or (:id runner) (:identity runner))]
                        (server-card id))
            :credits (:credits runner)
            :tags (:tags runner)}
   :mulligan (:mulligan options)
   :start-as (:start-as options)
   :dont-start-turn (:dont-start-turn options)
   :dont-start-game (:dont-start-game options)
   :format (or (:format options) :casual)})

(defn new-game
  "Init a new game using given corp and runner. Keep starting hands (no mulligan) and start Corp's turn."
  ([] (new-game nil))
  ([players]
   (let [{:keys [corp runner mulligan start-as dont-start-turn dont-start-game format]} (make-decks players)
         state (core/init-game
                 {:gameid 1
                  :format format
                  :players [{:side "Corp"
                             :user {:username "Corp"}
                             :deck {:identity (:identity corp)
                                    :cards (:deck corp)}}
                            {:side "Runner"
                             :user {:username "Runner"}
                             :deck {:identity (:identity runner)
                                    :cards (:deck runner)}}]})]
     (when-not dont-start-game
       (if (#{:both :corp} mulligan)
         (click-prompt state :corp "Mulligan")
         (click-prompt state :corp "Keep"))
       (if (#{:both :runner} mulligan)
         (click-prompt state :runner "Mulligan")
         (click-prompt state :runner "Keep"))
       (when-not dont-start-turn (core/start-turn state :corp nil)))
     ;; Gotta move cards where they need to go
     (doseq [side [:corp :runner]]
       (let [side-map (if (= :corp side) corp runner)]
         (when-let [hand (:hand side-map)]
           (starting-hand state side hand))
         (when (seq (:discard side-map))
           (doseq [ctitle (:discard side-map)]
             (core/move state side
                        (or (find-card ctitle (get-in @state [side :deck]))
                            ;; This is necessary as a :discard card will only end up in
                            ;; the hand when we're not already using (starting-hand)
                            (when (empty? (:hand side-map))
                              (find-card ctitle (get-in @state [side :hand]))))
                        :discard)))
         (when (:credits side-map)
           (swap! state assoc-in [side :credit] (:credits side-map))))
       (core/clear-win state side))
     ;; These are side independent so they happen ouside the loop
     (when-let [bad-pub (:bad-pub corp)]
       (swap! state assoc-in [:corp :bad-publicity :base] bad-pub))
     (when-let [tags (:tags runner)]
       (swap! state assoc-in [:runner :tag :base] tags))
     (when (= start-as :runner) (take-credits state :corp))
     (core/fake-checkpoint state)
     state)))

;;; Card related functions
(defn card-ability-impl
  [state side card ability & targets]
  (let [card (get-card state card)
        ability (cond
                  (number? ability) ability
                  (string? ability) (some #(when (= (:label (second %)) ability) (first %)) (map-indexed vector (:abilities card)))
                  :else -1)
        has-ability? (and (number? ability)
                          (nth (:abilities card) ability nil))
        playable? (or (active? card)
                      (:autoresolve (nth (:abilities card) ability nil)))]
    (is' has-ability? (str (:title card) " has ability #" ability))
    (is' playable? (str (:title card) " is active or ability #" ability " is an auto resolve toggle"))
    (when (and has-ability? playable?)
      (core/process-action "ability" state side {:card card
                                                 :ability ability
                                                 :targets (first targets)}))))

(defmacro card-ability
  "Trigger a card's ability with its 0-based index. Refreshes the card argument before
  triggering the ability."
  [state side card ability & targets]
  `(error-wrapper (card-ability-impl ~state ~side ~card ~ability ~@targets)))

(defn card-subroutine-impl
  [state _ card ability]
  (let [ice (get-card state card)]
    (is' (active-ice? state ice) (str (:title ice) " is active"))
    (is' (core/get-current-encounter state) "Subroutines can be resolved")
    (when (and (active-ice? state ice)
               (core/get-current-encounter state))
      (core/process-action "subroutine" state :corp {:card ice :subroutine ability})
      true)))

(defmacro card-subroutine
  "Trigger a piece of ice's subroutine with the 0-based index."
  [state _ card ability]
  `(error-wrapper (card-subroutine-impl ~state nil ~card ~ability)))

(defn card-side-ability
  [state side card ability & targets]
  (let [ab {:card (get-card state card)
            :ability ability
            :targets (first targets)}]
    (if (= :corp side)
      (core/process-action "corp-ability" state side ab)
      (core/process-action "runner-ability" state side ab))))

(defn change-impl
  [state side value-key delta]
  (let [target {:key value-key
                :delta delta}]
    (is' (and (keyword? value-key) (number? delta)) "Passed in value-key and delta" )
    (when (and (keyword? value-key) (number? delta))
      (core/process-action "change" state side target))))

(defmacro change
  [state side value-key delta]
  `(error-wrapper (change-impl ~state ~side ~value-key ~delta)))

(def count-tags jutils/count-tags)
(def count-real-tags jutils/count-real-tags)
(def is-tagged? jutils/is-tagged?)
(def count-bad-pub jutils/count-bad-pub)
(def get-link core/get-link)
(def get-strength core/get-strength)

(defn gain-tags
  [state side n]
  (wait-for (core/gain-tags state side n)
            (core/fake-checkpoint state)))

(defn remove-tag
  [state side]
  (core/process-action "remove-tag" state side nil))

(defn get-ice
  "Get installed ice protecting server by position. If no pos, get all ice on the server."
  ([state server]
   (get-in @state [:corp :servers server :ices]))
  ([state server pos]
   (get-in @state [:corp :servers server :ices pos])))

(defn get-content
  "Get card in a server by position. If no pos, get all cards in the server."
  ([state server]
   (get-in @state [:corp :servers server :content]))
  ([state server pos]
   (get-in @state [:corp :servers server :content pos])))

(defn get-program
  "Get non-hosted program by position. If no pos, get all installed programs."
  ([state] (get-in @state [:runner :rig :program]))
  ([state pos]
   (get-in @state [:runner :rig :program pos])))

(defn get-hardware
  "Get hardware by position. If no pos, get all installed hardware."
  ([state] (get-in @state [:runner :rig :hardware]))
  ([state pos]
   (get-in @state [:runner :rig :hardware pos])))

(defn get-resource
  "Get non-hosted resource by position. If no pos, get all installed resources."
  ([state] (get-in @state [:runner :rig :resource]))
  ([state pos]
   (get-in @state [:runner :rig :resource pos])))

(defn get-runner-facedown
  "Get non-hosted runner facedown by position. If no pos, get all runner facedown installed cards."
  ([state] (get-in @state [:runner :rig :facedown]))
  ([state pos]
   (get-in @state [:runner :rig :facedown pos])))

(defn get-discarded
  "Get discarded card by position. If no pos, selects most recently discarded card."
  ([state side] (get-discarded state side (-> @state side :discard count dec)))
  ([state side pos]
   (get-in @state [side :discard pos])))

(defn get-scored
  "Get a card from the score area. Can find by name or index.
  If no index or name provided, gets all scored cards."
  ([state side] (get-in @state [side :scored]))
  ([state side x]
   (if (number? x)
     ;; Find by index
     (get-in @state [side :scored x])
     ;; Find by name
     (when (string? x)
       (find-card x (get-in @state [side :scored]))))))

(defn get-rfg
  ([state side] (get-in @state [side :rfg]))
  ([state side pos]
   (get-in @state [side :rfg pos])))

(defn play-from-hand-impl
  [state side title & server]
  (let [card (find-card title (get-in @state [side :hand]))]
    (is' (some? card) (str title " is in the hand"))
    (when (some? card)
      (core/process-action "play" state side {:card card :server (first server)})
      true)))

(defmacro play-from-hand
  "Play a card from hand based on its title. If installing a Corp card, also indicate
  the server to install into with a string."
  [state side title & server]
  `(error-wrapper (play-from-hand-impl ~state ~side ~title ~@server)))

;;; Run functions
(defn run-on-impl
  [state server]
  (let [run (:run @state)]
    (is' (not run) "There is no existing run")
    (is' (pos? (get-in @state [:runner :click])) "Runner can make a run")
    (when (and (not run) (pos? (get-in @state [:runner :click])))
      (core/process-action "run" state :runner {:server server})
      true)))

(defmacro run-on
  "Start run on specified server."
  [state server]
  `(error-wrapper (run-on-impl ~state ~server)))

(defn run-next-phase-impl
  [state]
  (let [run (:run @state)]
    (is' (some? run) "There is a run happening")
    (is' (:next-phase run) "The next phase has been set")
    (when (and (some? run) (:next-phase run))
      (core/process-action "start-next-phase" state :runner nil)
      true)))

(defmacro run-next-phase
  [state]
  `(error-wrapper (run-next-phase-impl ~state)))

(defn encounter-continue-impl
  ([state] (encounter-continue-impl state :any))
  ([state phase]
   (let [encounter (core/get-current-encounter state)]
     (is' (some? encounter) "There is an encounter happening")
     (is' (no-prompt? state :runner) "No open prompts for the runner")
     (is' (no-prompt? state :corp) "No open prompts for the corp")
     (is' (not (:no-action encounter)) "No player has pressed continue yet")
     (when (and (some? encounter)
                (no-prompt? state :runner)
                (no-prompt? state :corp)
                (not (:no-action encounter)))
       (core/process-action "continue" state :corp nil)
       (core/process-action "continue" state :runner nil)
       (when-not (= :any phase)
         (is (= phase (:phase (:run @state))) "Run is in the correct phase"))))))

(defmacro encounter-continue
  "No action from corp and continue for runner to proceed in current encounter."
  ([state] `(error-wrapper (encounter-continue-impl ~state :any)))
  ([state phase] `(error-wrapper (encounter-continue-impl ~state ~phase))))

(defn run-continue-impl
  ([state] (run-continue-impl state :any))
  ([state phase]
   (if (core/get-current-encounter state)
     (encounter-continue-impl state)
     (let [run (:run @state)]
       (is' (some? run) "There is a run happening")
       (is' (no-prompt? state :runner) "No open prompts for the runner")
       (is' (no-prompt? state :corp) "No open prompts for the corp")
       (is' (not (:no-action run)) "No player has pressed continue yet")
       (is' (not= :success (:phase run))
            "The run has not reached the server yet")
       (when (and (some? run)
                  (no-prompt? state :runner)
                  (no-prompt? state :corp)
                  (not (:no-action run))
                  (not= :success (:phase run)))
         (core/process-action "continue" state :corp nil)
         (core/process-action "continue" state :runner nil)
         (when-not (= :any phase)
           (is (= phase (:phase (:run @state))) "Run is in the correct phase")))))))

(defmacro run-continue
  "No action from corp and continue for runner to proceed in current run."
  ([state] `(error-wrapper (run-continue-impl ~state :any)))
  ([state phase] `(error-wrapper (run-continue-impl ~state ~phase))))

(defn run-jack-out-impl
  [state]
  (let [run (:run @state)]
    (is' (some? run) "There is a run happening")
    (is' (= :movement (:phase run)) "Runner is allowed to jack out")
    (when (and (some? run) (= :movement (:phase run)))
      (core/process-action "jack-out" state :runner nil)
      true)))

(defmacro run-jack-out
  "Jacks out in run."
  [state]
  `(error-wrapper (run-jack-out-impl ~state)))

(defmacro run-empty-server-impl
  [state server]
  `(when (run-on ~state ~server)
     (run-continue ~state)))

(defmacro run-empty-server
  "Make a successful run on specified server, assumes no ice in place."
  [state server]
  `(error-wrapper (run-empty-server-impl ~state ~server)))

(defn run-continue-until-impl
  [state phase ice]
  (is' (some? (:run @state)) "There is a run happening")
  (is' (some #(= phase %) [:approach-ice :encounter-ice :movement :success]) "Valid phase")
  (run-continue-impl state)
  (while (and (:run @state)
              (or (not= phase (:phase (:run @state)))
                  (and ice
                       (not (utils/same-card? ice (core/get-current-ice state)))))
              (not (and (= :movement (:phase (:run @state)))
                        (zero? (:position (:run @state)))))
              (no-prompt? state :runner)
              (no-prompt? state :corp)
              (not (:no-action (:run @state)))
              (not= :success (:phase (:run @state))))
    (run-continue-impl state))
  (when (and (= :success phase)
             (and (= :movement (:phase (:run @state)))
                  (zero? (:position (:run @state)))))
    (run-continue-impl state))
  (when ice
    (is' (utils/same-card? ice (core/get-current-ice state))) "Current ice reached"))

(defmacro run-continue-until
  ([state phase] `(error-wrapper (run-continue-until-impl ~state ~phase nil)))
  ([state phase ice]
   `(error-wrapper (run-continue-until-impl ~state ~phase ~ice))))

(defn fire-subs-impl
  [state card]
  (let [ice (get-card state card)]
    (is' (active-ice? state ice) (str (:title ice) " is active"))
    (is' (core/get-current-encounter state) "Subroutines can be resolved")
    (when (and (active-ice? state ice)
               (core/get-current-encounter state))
      (core/process-action "unbroken-subroutines" state :corp {:card ice}))))

(defmacro fire-subs
  [state card]
  `(error-wrapper (fire-subs-impl ~state ~card)))

(defn play-run-event-impl
  [state card server]
  (when (play-from-hand state :runner card)
    (is' (:run @state) "There is a run happening")
    (is' (= [server] (get-in @state [:run :server])) "Correct server is run")
    (when (and (:run @state)
               (= [server] (get-in @state [:run :server]))
               (run-continue state))
      (is' (seq (get-in @state [:runner :prompt])) "A prompt is shown")
      (is' (true? (get-in @state [:run :successful])) "Run is marked successful"))))

(defmacro play-run-event
  "Play a run event with a replace-access effect on an unprotected server.
  Advances the run timings to the point where replace-access occurs."
  [state card server]
  `(error-wrapper (play-run-event-impl ~state ~card ~server)))

(defn get-run-event
  ([state] (get-in @state [:runner :play-area]))
  ([state pos]
   (get-in @state [:runner :play-area pos])))

(defn rez-impl
  [state card & args]
  (let [card (get-card state card)]
    (is' (installed? card) (str (:title card) " is installed"))
    (is' (not (rezzed? card)) (str (:title card) " is unrezzed"))
    (when (and (installed? card)
               (not (rezzed? card)))
      (core/process-action "rez" state :corp (merge {:card card} (first args))))))

(defmacro rez
  [state _ card & args]
  `(error-wrapper (rez-impl ~state ~card ~@args)))

(defn derez-impl
  [state side card]
  (let [card (get-card state card)]
    (is' (installed? card) (str (:title card) " is installed"))
    (is' (rezzed? card) (str (:title card) " is rezzed"))
    (when (and (installed? card)
               (rezzed? card))
      (core/process-action "derez" state side {:card card}))))

(defmacro derez
  [state side card]
  `(error-wrapper (derez-impl ~state ~side ~card)))

(defn end-phase-12-impl
  [state side]
  (let [phase (keyword (str (name side) "-phase-12"))]
    (is' (phase @state) (str (jutils/capitalize (name side)) " in Step 1.2"))
    (when (phase @state)
      (core/process-action "end-phase-12" state side nil))))

(defmacro end-phase-12
  [state side]
  `(error-wrapper (end-phase-12-impl ~state ~side)))

(defn click-draw
  [state side]
  (core/process-action "draw" state side nil))

(defn click-credit
  [state side]
  (core/process-action "credit" state side nil))

(defn click-advance-impl
  [state side card]
  (let [card (get-card state card)]
    (is' (some? card) (str (:title card) " exists"))
    (is' (installed? card) (str (:title card) " is installed"))
    (when (and (some? card) (installed? card))
      (core/process-action "advance" state side {:card card}))))

(defmacro click-advance
  [state side card]
  `(error-wrapper (click-advance-impl ~state ~side ~card)))

(defn trash-impl
  [state side card]
  (let [card (get-card state card)]
    (is' (some? card) (str (:title card) " exists"))
    (when (some? card)
      (core/process-action "trash" state side {:card card}))))

(defmacro trash
  [state side card]
  `(error-wrapper (trash-impl ~state ~side ~card)))

(defn score-agenda-impl
  [state card]
  (let [card (get-card state card)
        advancementcost (:advancementcost card)]
    (is' (some? card) (str (:title card) " exists"))
    (when (some? card)
      (core/gain state :corp :click advancementcost :credit advancementcost)
      (core/fake-checkpoint state)
      (dotimes [_ advancementcost]
        (core/process-action "advance" state :corp {:card card}))
      (is' (= advancementcost (get-counters (get-card state card) :advancement)))
      (when (= advancementcost (get-counters (get-card state card) :advancement))
        (core/process-action "score" state :corp {:card card})
        (is (find-card (:title card) (get-scored state :corp)))))))

(defmacro score-agenda
  "Take clicks and credits needed to advance and score the given agenda."
  [state _ card]
  `(error-wrapper (score-agenda-impl ~state ~card)))

(defn score
  "Needed for calling the internal function directly"
  ([state _ card] (core/process-action "score" state :corp {:card card}))
  ([state _ card args] (core/process-action "score" state :corp (merge args {:card card}))))

(defn advance
  "Advance the given card."
  ([state card] (advance state card 1))
  ([state card n]
   (dotimes [_ n]
     (click-advance state :corp card))))

(defn trash-from-hand-impl
  [state side title]
  (let [card (find-card title (get-in @state [side :hand]))]
    (is' (some? card) (str (:title card) " exists"))
    (when (some? card)
      (trash state side card))))

(defmacro trash-from-hand
  "Trash specified card from hand of specified side"
  [state side title]
  `(error-wrapper (trash-from-hand-impl ~state ~side ~title)))

(defn trash-card-impl
  [state side card]
  (let [card (get-card state card)]
    (is' (some? card) (str (:title card) " exists"))
    (when (some? card)
      (trash state side card))))

(defmacro trash-card
  [state side card]
  `(error-wrapper (trash-card-impl ~state ~side ~card)))

(defn trash-resource
  "Click-trash a resource as the corp"
  [state]
  (core/process-action "trash-resource" state :corp nil))

(defn accessing
  "Checks to see if the runner has a prompt accessing the given card title"
  [state title]
  (= title (-> @state :runner :prompt first :card :title)))

(defn play-and-score
  "Play an agenda from the hand into a new server and score it. Unlike score-agenda, spends a click."
  [state title]
  (when (play-from-hand state :corp title "New remote")
    (score-agenda state :corp (get-content state (keyword (str "remote" (dec (:rid @state)))) 0))))

(defn damage
  [state side dmg-type qty]
  (core/damage state side (core/make-eid state) dmg-type qty nil))

(defn move
  [state side card location]
  (core/move state side card location)
  (core/fake-checkpoint state))

(defn draw
  ([state side] (draw state side 1 nil))
  ([state side n] (draw state side n nil))
  ([state side n args]
   (core/draw state side (core/make-eid state) n args)))

(defmacro do-game [s & body]
  `(let [~'state ~s
         ~'get-corp (fn [] (:corp @~'state))
         ~'get-runner (fn [] (:runner @~'state))
         ~'get-run (fn [] (:run @~'state))
         ~'hand-size (fn [side#] (game.core.hand-size/hand-size ~'state side#))
         ~'refresh (fn [card#] (game.core.card/get-card ~'state card#))
         ~'prompt-map (fn [side#] (-> @~'state side# :prompt first))
         ~'prompt-type (fn [side#] (:prompt-type (~'prompt-map side#)))
         ~'prompt-buttons (fn [side#] (->> (~'prompt-map side#) :choices (map :value)))
         ~'prompt-titles (fn [side#] (map :title (~'prompt-buttons side#)))
         ~'prompt-fmt (fn [side#]
                        (let [prompt# (~'prompt-map side#)
                              choices# (:choices prompt#)
                              choices# (cond
                                         (nil? choices#) nil
                                         (sequential? choices#) choices#
                                         :else [choices#])
                              card# (:card prompt#)
                              prompt-type# (:prompt-type prompt#)]
                          (str (game.utils/side-str side#) ": " (:msg prompt# "") "\n"
                               (when prompt-type# (str "Type: " prompt-type# "\n"))
                               (when card# (str "Card: " (:title card#) "\n"))
                               (clojure.string/join
                                 "\n" (map #(str "[ " (or (get-in % [:value :title])
                                                          (:value %)
                                                          %
                                                          "nil") " ]") choices#)) "\n")))]
     ~@body))

(defmacro deftest-pending [name & _body]
  (let [message (str "\n" name " is pending")]
    `(clojure.test/deftest- ~name (println ~message))))

(defmacro changes-val-macro [change-amt val-form msg & body-form]
  `(let [start-val# ~val-form]
     (do ~@body-form)
     (let [end-val# ~val-form
           actual-change# (- end-val# start-val#)]
       (clojure.test/do-report
         {:type (if (= actual-change# ~change-amt) :pass :fail)
          :actual actual-change#
          :expected ~change-amt
          :message (str "Changed from " start-val# " to " end-val# ", Expected end result of " (+ start-val# ~change-amt) " " ~msg " " (cons 'do '~body-form))}))))

(defmacro changes-val [change-amt val-form & body-form]
  `(changes-val-macro ~change-amt ~val-form "" ~@body-form))

(defmethod clojure.test/assert-expr 'changes-val [msg form]
  (let [change-amt (nth form 1)
        val-form (nth form 2)
        body-form (drop 3 form)]
    `(changes-val-macro ~change-amt ~val-form ~msg ~@body-form)))

(defmacro changes-credits [side change-amt & body-form]
  `(changes-val-macro ~change-amt (:credit ~side) "" ~@body-form))

;; Enables you to do this:
;; (is (changes-credits (get-runner) -5
;;   (play-from-hand state :runner "Magnum Opus")))
(defmethod clojure.test/assert-expr 'changes-credits [msg form]
  (let [side (nth form 1)
        change-amt (nth form 2)
        body-form (drop 3 form)]
    `(changes-val-macro ~change-amt (:credit ~side) ~msg ~@body-form)))

(defmacro before-each
  [let-bindings & testing-blocks]
  (assert (every? #(= 'testing (first %)) testing-blocks))
  (let [bundles (for [block testing-blocks] `(let [~@let-bindings] ~block))]
    `(do ~@bundles)))
