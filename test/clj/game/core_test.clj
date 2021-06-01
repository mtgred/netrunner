(ns game.core-test
  (:require [clojure.string :as string]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer :all]
            [hawk.core :as hawk]
            [game.core :as core]
            [game.core.card :refer [get-card installed? rezzed? active? get-counters]]
            [game.utils :as utils :refer [server-card]]
            [game.core.eid :as eid]
            [game.utils-test :refer [click-prompt error-wrapper is']]
            [game.macros :refer [wait-for]]
            [jinteki.cards :refer [all-cards]]
            [jinteki.utils :as jutils]))

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
(def find-card core/find-card)

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
          :identity (when-let [id (:id corp)]
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
            :identity (when-let [id (:id runner)]
                        (server-card id))
            :credits (:credits runner)
            :tags (:tags runner)}
   :mulligan (:mulligan options)
   :start-as (:start-as options)
   :dont-start-turn (:dont-start-turn options)
   :dont-start-game (:dont-start-game options)})

(defn new-game
  "Init a new game using given corp and runner. Keep starting hands (no mulligan) and start Corp's turn."
  ([] (new-game nil))
  ([players]
   (let [{:keys [corp runner mulligan start-as dont-start-turn dont-start-game]} (make-decks players)
         state (core/init-game
                 {:gameid 1
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
                  :else -1)]
    (is' (active? card) (str (:title card) " is active"))
    (is' (and (number? ability)
              (nth (:abilities card) ability nil))
         (str (:title card) " has ability #" ability))
    (when (and (active? card)
               (number? ability)
               (nth (:abilities card) ability nil))
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
  (let [ice (get-card state card)
        run (:run @state)]
    (is' (rezzed? ice) (str (:title ice) " is active"))
    (is' (some? run) "There is a run happening")
    (is' (= :encounter-ice (:phase run)) "Subroutines can be resolved")
    (when (and (rezzed? ice) (some? run) (= :encounter-ice (:phase run)))
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

(defn run-continue-impl
  ([state] (run-continue-impl state :any))
  ([state phase]
   (let [run (:run @state)]
     (is' (some? run) "There is a run happening")
     (is' (empty? (get-in @state [:runner :prompt])) "No open prompts for the runner")
     (is' (empty? (get-in @state [:corp :prompt])) "No open prompts for the corp")
     (is' (not (:no-action run)) "No player has pressed continue yet")
     (is' (not= :access-server (:phase run))
          "The run has not reached the server yet")
     (when (and (some? run)
                (empty? (get-in @state [:runner :prompt]))
                (empty? (get-in @state [:corp :prompt]))
                (not (:no-action run))
                (not= :access-server (:phase run)))
       (core/process-action "continue" state :corp nil)
       (core/process-action "continue" state :runner nil)
       (when-not (= :any phase)
         (is (= phase (get-in @state [:run :phase])) "Run is in the correct phase"))))))

(defmacro run-continue
  "No action from corp and continue for runner to proceed in current run."
  ([state] `(error-wrapper (run-continue-impl ~state :any)))
  ([state phase] `(error-wrapper (run-continue-impl ~state ~phase))))

(defn run-phase-43-impl
  [state]
  (let [run (:run @state)]
    (is' (some? run) "There is a run happening")
    (is' (zero? (:position run)) "Runner has passed all ice")
    (is' (not (:no-action run)) "No player has pressed continue yet")
    (is' (= :approach-server (:phase run)) "Runner is approaching the server")
    (when (and (some? run)
               (zero? (:position run))
               (not (:no-action run))
               (= :approach-server (:phase run)))
      (core/process-action "corp-phase-43" state :corp nil)
      (core/process-action "continue" state :runner nil)
      true)))

(defmacro run-phase-43
  "Ask for triggered abilities phase 4.3"
  [state]
  `(error-wrapper (run-phase-43-impl ~state)))

(defn run-jack-out-impl
  [state]
  (let [run (:run @state)]
    (is' (some? run) "There is a run happening")
    (is' (:jack-out run) "Runner is allowed to jack out")
    (when (and (some? run) (:jack-out run))
      (core/process-action "jack-out" state :runner nil)
      true)))

(defmacro run-jack-out
  "Jacks out in run."
  [state]
  `(error-wrapper (run-jack-out-impl ~state)))

(defn run-empty-server-impl
  [state server]
  (when (run-on state server)
    (run-continue state)))

(defmacro run-empty-server
  "Make a successful run on specified server, assumes no ice in place."
  [state server]
  `(error-wrapper (run-empty-server-impl ~state ~server)))

(defn fire-subs-impl
  [state card]
  (let [ice (get-card state card)
        run (:run @state)]
    (is' (rezzed? ice) (str (:title ice) " is active"))
    (is' (some? run) "There is a run happening")
    (is' (= :encounter-ice (:phase run)) "Subroutines can be resolved")
    (when (and (rezzed? ice) (some? run)) (= :encounter-ice (:phase run))
      (core/process-action "unbroken-subroutines" state :corp {:card ice}))))

(defmacro fire-subs
  [state card]
  `(error-wrapper (fire-subs-impl ~state ~card)))

(defn play-run-event-impl
  [state card server & show-prompt]
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
  [state card server & show-prompt]
  `(error-wrapper (play-run-event-impl ~state ~card ~server ~@show-prompt)))

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
  (do (core/move state side card location)
      (core/fake-checkpoint state)))
