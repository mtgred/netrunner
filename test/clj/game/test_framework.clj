(ns game.test-framework
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.board :refer [server-list]]
   [game.core.card :refer [active? get-card get-counters get-title installed?
                           rezzed?]]
   [game.core.eid :as eid]
   [game.core.events :refer [turn-events]]
   [game.core.ice :refer [active-ice?]]
   [game.core.initializing :refer [make-card]]
   [game.core.threat :refer [threat-level]]
   [game.test-framework.asserts]
   [game.utils :as utils]
   [game.utils-test :refer [error-wrapper is']]
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

(defn is-zone-impl
  "Is the hand exactly equal to a given set of cards?"
  [state side zone expected]
  (let [expected (seq (sort (flatten expected)))
        contents (seq (sort (map :title (get-in @state [side zone]))))]
    (is' (= expected contents) (str (name zone) " is not " expected))))

(defmacro is-hand?
  "Is the hand exactly equal to a given set of cards?"
  [state side expected-hand]
  `(error-wrapper (is-zone-impl ~state ~side :hand ~expected-hand)))

(defmacro is-deck?
  "Is the hand exactly equal to a given set of cards?"
  [state side expected-deck]
  `(error-wrapper (is-zone-impl ~state ~side :deck ~expected-deck)))

(defmacro is-discard?
  "Is the hand exactly equal to a given set of cards?"
  [state side expected-discard]
  `(error-wrapper (is-zone-impl ~state ~side :discard ~expected-discard)))

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

(defn waiting?
  "Is there a waiting-prompt for the given side?"
  [state side]
  (let [prompt (get-prompt state side)]
    (= :waiting (:prompt-type prompt))))

(defn anybody-waiting?
  "Either side is waiting"
  [state]
  (or (waiting? state :runner) (waiting? state :corp)))

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
        (core/process-action "select" state side {:card card :eid (:eid (get-prompt state side))})
        (let [all-cards (core/get-all-cards state)
              matching-cards (filter #(= card (core/get-title %)) all-cards)]
          (if (= (count matching-cards) 1)
            (core/process-action "select" state side {:card (first matching-cards) :eid (:eid (get-prompt state side))})
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
          (when-not (core/process-action "choice" state side {:choice parsed-number :eid (:eid (get-prompt state side))})
            (is' (not true) (str "Parsed number " parsed-number " is incorrect somehow"))))
        (catch Exception _
          (is' (number? (Integer/parseInt choice)) (expect-type "number string" choice))))

      (= :trace (:prompt-type prompt))
      (try
        (let [int-choice (Integer/parseInt choice)
              under (<= int-choice (:choices prompt))]
          (when-not (and under
                         (core/process-action "choice" state side {:choice int-choice :eid (:eid (get-prompt state side))}))
            (is' (<= int-choice (:choices prompt))
                 (str (utils/side-str side) " expected to pay [ "
                      int-choice " ] to trace but couldn't afford it."))))
        (catch Exception _
          (is' (number? (Integer/parseInt choice))
               (expect-type "number string" choice))))

      ;; List of card titles for auto-completion
      (:card-title choices)
      (when-not (core/process-action "choice" state side {:choice choice :eid (:eid (get-prompt state side))})
        (is' (true? (or (map? choice) (string? choice))) (expect-type "card string or map" choice)))

      ;; Default text prompt
      :else
      (let [choice-fn #(or (= choice (:value %))
                           (= choice (get-in % [:value :title]))
                           (utils/same-card? choice (:value %)))
            idx (or (:idx (first args)) 0)
            chosen (nth (filter choice-fn choices) idx nil)]
        (when-not (and chosen (core/process-action "choice" state side {:choice {:uuid (:uuid chosen)} :eid (:eid (get-prompt state side))}))
          (is' (= choice (mapv :value choices))
               (str (utils/side-str side) " expected to click [ "
                    (pr-str (if (string? choice) choice (:title choice "")))
                    " ] but couldn't find it. Current prompt is: " (pr-str prompt))))))))
(defmacro click-card
  "Resolves a 'select prompt' by clicking a card. Takes a card map or a card name."
  [state side card]
  `(error-wrapper (click-card-impl ~state ~side ~card)))

(defmacro click-prompt
  "Clicks a button in a prompt. {choice} is a string or map only, no numbers."
  [state side choice & args]
  `(error-wrapper (click-prompt-impl ~state ~side ~choice ~@args)))

(defn click-prompts-impl
  [state side prompts]
  (loop [[prompt & prompts] prompts]
    (let [prompt
          ;; if an 1-fn is passed in, scry it's output based on the game state
          (cond
            (fn? prompt) (prompt state)
            (fn? (:choice prompt)) (merge prompt {:choice ((:choice prompt) state)})
            :else prompt)]
      (cond
        (and (not prompt) (not (seq prompts))) true
        (not prompt) (is` nil "attempt to resolve nil prompt option")
        ;; it's a select prompt - we want to click on a card
        (prompt-is-type? state side :select)
        (do (if (or (:cid prompt) (string? prompt))
              ;; we can still clock done on a select prompt though
              (if (= prompt "Done")
                (click-prompt state side prompt)
                (click-card state side prompt))
              (click-card state (or (:side prompt) side) (:choice prompt)))
            (recur prompts))
        :else
        (do (if (or (:cid prompt) (string? prompt))
              (click-prompt state side prompt)
              (click-prompt state (or (:side prompt) side) (:choice prompt)))
            (recur prompts))))))

(defmacro click-prompts
  "click an arbitrary number of prompts, one after the other.
  You can tag a prompt like {:side :runner :choice ...}, feed in raw cards,
  or feed in cards like {:choice ...}"
  ([state side] true)
  ([state side & prompts]
   `(error-wrapper (click-prompts-impl ~state ~side ~(vec prompts)))))

(defn do-trash-prompt
  [state cost]
  (click-prompt state :runner (str "Pay " cost " [Credits] to trash")))

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

(defn starting-score-areas
  "Moves all cards that should be in the score areas zones into their respective score areas"
  [state score-area-corp score-area-runner]
  (when (or (seq score-area-corp) (seq score-area-runner))
    (let [hand-size (count (get-in @state [:corp :hand]))]
      (doseq [c (get-in @state [:corp :hand])]
        (core/move state :corp c :deck))
      (doseq [ctitle score-area-corp]
        (let [c (core/move state :corp (find-card ctitle (get-in @state [:corp :deck])) :scored)]
          (core/card-init state :corp c {:resolve-effect false :init-data true})))
      (doseq [ctitle score-area-runner]
        (core/move state :runner (find-card ctitle (get-in @state [:corp :deck])) :scored {:force true}))
      (doseq [c (take hand-size (get-in @state [:corp :deck]))]
        (core/move state :corp c :hand)))))

(defn ensure-no-prompts [state]
  (is' (no-prompt? state :corp) "Corp has prompts open")
  (is' (no-prompt? state :runner) "Runner has prompts open"))

(defn start-turn
  [state side]
  (core/process-action "start-turn" state side nil))

(defn end-turn [state side]
  (core/process-action "end-turn" state side nil))

(defmacro take-credits
  "Take credits for n clicks, or if no n given, for all remaining clicks of a side.
  If all clicks are used up, end turn and start the opponent's turn."
  ([state side] `(take-credits ~state ~side nil))
  ([state side n]
   `(let [other# (if (= ~side :corp) :runner :corp)]
      (error-wrapper (ensure-no-prompts ~state))
      (dotimes [_# (or ~n (get-in @~state [~side :click]))]
        (core/process-action "credit" ~state ~side nil))
      (when (zero? (get-in @~state [~side :click]))
        (end-turn ~state ~side)
        (start-turn ~state other#)))))

;; Deck construction helpers
(defn qty [card amt]
  (when (pos? amt)
    (repeat amt card)))

(defn card-vec->card-map
  [side [card amt]]
  (let [loaded-card (if (string? card) (utils/server-card card) card)]
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
                                            (:score-area runner)
                                            (:score-area corp)
                                            (:discard corp)))
                    (:deck corp)
                    (transform "Corp" (qty "Hedge Fund" 3)))
          :hand (when-let [hand (:hand corp)]
                  (flatten hand))
          :score-area (when-let [scored (:score-area corp)]
                    (flatten scored))
          :discard (when-let [discard (:discard corp)]
                     (flatten discard))
          :identity (when-let [id (or (:id corp) (:identity corp))]
                      (utils/server-card id))
          :credits (:credits corp)
          :bad-pub (:bad-pub corp)}
   :runner {:deck (or (transform "Runner" (conj (:deck runner)
                                                (:hand runner)
                                                (:discard runner)))
                      (:deck runner)
                      (transform "Runner" (qty "Sure Gamble" 3)))
            :hand (when-let [hand (:hand runner)]
                    (flatten hand))
            :score-area (when-let [scored (:score-area runner)]
                    (flatten scored))
            :discard (when-let [discard (:discard runner)]
                       (flatten discard))
            :identity (when-let [id (or (:id runner) (:identity runner))]
                        (utils/server-card id))
            :credits (:credits runner)
            :tags (:tags runner)}
   :mulligan (:mulligan options)
   :start-as (:start-as options)
   :dont-start-turn (:dont-start-turn options)
   :dont-start-game (:dont-start-game options)
   :format (or (:format options) :casual)})

(defn stack-deck
  "Stacks the top of the deck with the named cards in order, if possible"
  [state side ordered-names]
  (let [ordered-names (flatten ordered-names)
        deck-frequencies (frequencies (map :title (get-in @state [side :deck])))]
    (doseq [ctitle ordered-names]
      (let [c (find-card ctitle (get-in @state [side :deck]))]
        (is c (str "Unable to find card " ctitle " in deck while stacking deck (were any cards in the hand by mistake?)"))
        (when c (core/move state side c :set-aside))))
    (doseq [ctitle (reverse ordered-names)]
      (when-let [c (find-card ctitle (get-in @state [side :set-aside]))]
        (core/move state side c :deck {:front true})))
    (is (= deck-frequencies (frequencies (map :title (get-in @state [side :deck]))))
        "Deck is still composed of the same set of cards after being stacked")
    (let [top-n-titles (map :title (take (count ordered-names) (get-in @state [side :deck])))]
      (is (= ordered-names top-n-titles)
          (str "Deck is (from top to bottom): " (str/join ", " ordered-names))))))

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
     (starting-score-areas state (:score-area corp) (:score-area runner))
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

(defn maybe-card
  "Query a card by name if possible, for the use of card abilities"
  [state card]
  (if (map? card)
    (get-card state card)
    (let [all-cards (core/get-all-cards state)
          matching-cards (filter #(= card (core/get-title %)) all-cards)]
      (if (= (count matching-cards) 1)
        (first matching-cards)
        (is' (= 1 (count matching-cards))
             (str "Expected to use ability for card [ " card
                  " ] but found " (count matching-cards)
                  " matching cards."))))))

;;; Card related functions
(defn card-ability-impl
  [state side card ability & targets]
  (let [card (maybe-card state card)
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
                                                 :targets (first targets)})
      true)))

(defmacro card-ability
  "Trigger a card's ability with its 0-based index. Refreshes the card argument before
  triggering the ability."
  [state side card ability & targets]
  `(error-wrapper (card-ability-impl ~state ~side ~card ~ability ~@targets)))

(defn expend-impl
  [state side card]
  (let [card (get-card state card)]
    (is' (some? card) (str (:title card) " exists"))
    (when (some? card)
      (is' (core/process-action "expend" state side {:card card}))
      true)))

(defmacro expend
  "Trigger an Expendable card's ability."
  [state side card]
  `(error-wrapper (expend-impl ~state ~side ~card)))

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
  (core/gain-tags state side (core/make-eid state) n)
  (core/fake-checkpoint state))

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
  [state side title server]
  (let [card (find-card title (get-in @state [side :hand]))]
    (ensure-no-prompts state)
    (is' (some? card) (str title " is in the hand"))
    (when-not (some? card)
      (let [other-side (if (= side :runner) :corp :runner)]
        (when (some? (find-card title (get-in @state [other-side :hand])))
          (println title " was instead found in the opposing hand - was the wrong side used?"))))
    (when server
      (is' (some #{server} (concat (server-list state) ["New remote"]))
           (str server " is not a valid server.")))
    (when (some? card)
      (is' (core/process-action "play" state side {:card card :server server}))
      true)))

(defn flashback-impl
  [state side title]
  (let [card (find-card title (get-in @state [side :discard]))]
    (ensure-no-prompts state)
    (is' (some? card) (str title " is in discard"))
    (is' (core/process-action "flashback" state side {:card card :server nil}))))

(defmacro flashback
  "Play a card as a flashback based on its title"
  ([state side title]
   `(error-wrapper (flashback-impl ~state ~side ~title))))

(defmacro play-from-hand
  "Play a card from hand based on its title. If installing a Corp card, also indicate
  the server to install into with a string."
  ([state side title] `(play-from-hand ~state ~side ~title nil))
  ([state side title server]
   `(error-wrapper (play-from-hand-impl ~state ~side ~title ~server))))

(defn- split-on-keywords [coll]
  (reduce (fn [acc x]
            (if (keyword? x)
              (conj acc x)
              (let [last-el (peek acc)]
                (if (vector? last-el)
                  (conj (pop acc) (conj last-el x))
                  (conj acc [x])))))
          []
          coll))

(defn play-from-hand-with-prompts-impl
  [state side title choices]
  (let [card (find-card title (get-in @state [side :hand]))]
    (ensure-no-prompts state)
    (is' (some? card) (str title "is in hand"))
    (if-not (some? card)
      (do (let [other-side (if (= side :runner) :corp :runner)]
            (when (some? (find-card title (get-in @state [other-side :hand])))
              (println title " was instead found in the opposing hand - was the wrong side used?")))
          true)
      (when-let [played (core/process-action "play" state side {:card card})]
        (let [choice-sets (split-on-keywords choices)]
          (doseq [cs choice-sets]
            (cond
              (vector? cs) (click-prompts-impl state side cs)
              (#{:rez :rezzed} cs)
              (if-let [tgt (->> (turn-events state side :corp-install)
                                (apply concat)
                                (map :card)
                                (filter #(= (:title %) title))
                                (filter (complement rezzed?))
                                last)]
                (core/process-action "rez" state :corp {:card tgt})
                (throw (Exception. (str title " not found in an unrezzed state to be rezzed")))))))))))


(defmacro play-from-hand-with-prompts
  "Play a card from hand based on it's title, and then click any number of prompts
   accepts for prompt: a string, a fn, a card object"
  ([state side title] `(play-from-hand ~state ~side ~title nil))
  ([state side title & prompts]
   `(error-wrapper (play-from-hand-with-prompts-impl ~state ~side ~title ~(vec prompts)))))

(defn play-cards-impl
  ([state side plays]
   (doseq [play plays]
     (if (string? play)
       (play-from-hand state side play)
       (play-from-hand-with-prompts-impl state side (first play) (rest play))))))

(defmacro play-cards
  ([state side] `nil)
  ([state side & plays]
   `(error-wrapper (play-cards-impl ~state ~side ~(vec plays)))))

;;; Run functions
(defn run-on-impl
  [state server {:keys [wait-at-initiation]}]
  (let [run (:run @state)]
    (ensure-no-prompts state)
    (is' (not run) "There is no existing run")
    (is' (pos? (get-in @state [:runner :click])) "Runner can make a run")
    (when (and (not run) (pos? (get-in @state [:runner :click])))
      (core/process-action "run" state :runner {:server server})
      (when-not wait-at-initiation
        (core/process-action "continue" state :corp nil)
        (when-not (:no-action run)
          (core/process-action "continue" state :runner nil)))
      true)))

(defmacro run-on
  "Start run on specified server."
  ([state server] `(run-on ~state ~server {}))
  ([state server args]
   `(error-wrapper (run-on-impl ~state ~server ~args))))

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
     (ensure-no-prompts state)
     (when (and (some? encounter)
                (no-prompt? state :runner)
                (no-prompt? state :corp))
       (core/process-action "continue" state :corp nil)
       (when-not (:no-action encounter)
         (core/process-action "continue" state :runner nil))
       (when-not (= :any phase)
         (is' (= phase (:phase (:run @state))) "Run is in the correct phase"))))))

(defmacro encounter-continue
  "No action from corp and continue for runner to proceed in current encounter."
  ([state] `(error-wrapper (encounter-continue-impl ~state :any)))
  ([state phase] `(error-wrapper (encounter-continue-impl ~state ~phase))))

(defn run-continue-impl
  ([state] (run-continue-impl state :any))
  ([state phase]
   (if (core/get-current-encounter state)
     (encounter-continue-impl state phase)
     (let [run (:run @state)]
       (is' (some? run) "There is a run happening")
       (ensure-no-prompts state)
       (is' (not= :success (:phase run))
            "The run has not reached the server yet")
       (when (and (some? run)
                  (no-prompt? state :runner)
                  (no-prompt? state :corp)
                  (not= :success (:phase run)))
         (core/process-action "continue" state :corp nil)
         (when-not (:no-action run)
           (core/process-action "continue" state :runner nil))
         (when-not (= :any phase)
           (is' (= phase (:phase (:run @state))) "Run is in the correct phase")))))))

(defmacro run-continue
  "No action from corp and continue for runner to proceed in current run."
  ([state] `(run-continue ~state :any))
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
  (is' (#{:approach-ice :encounter-ice :movement :success} phase) "Valid phase")
  (run-continue-impl state)
  (while (and (:run @state)
              (or (not= phase (:phase (:run @state)))
                  (and ice
                       (not (utils/same-card? ice (core/get-current-ice state)))))
              (not (and (= :movement (:phase (:run @state)))
                        (zero? (:position (:run @state)))))
              (no-prompt? state :runner)
              (no-prompt? state :corp)
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

(defmacro auto-pump
  [state card]
  `(core/process-action "dynamic-ability" ~state :runner {:dynamic "auto-pump"
                                                          :card (get-card ~state ~card)}))

(defn auto-pump-and-break-impl
  [state card]
  (let [breaker (get-card state card)]
    (is' (active-ice? state) "Ice is active")
    (is' (core/get-current-encounter state) "Subroutines can be resolved")
    (when (and (active-ice? state)
               (core/get-current-encounter state))
      (core/process-action "dynamic-ability" state :runner {:dynamic "auto-pump-and-break"
                                                            :card breaker}))))

(defmacro auto-pump-and-break
  [state card]
  `(error-wrapper (auto-pump-and-break-impl ~state ~card)))

(defn play-run-event-impl
  [state card server]
  (ensure-no-prompts state)
  (when (play-from-hand state :runner card)
    (is' (:run @state) "There is a run happening")
    (is' (= [server] (get-in @state [:run :server])) "Correct server is run")
    (when (and (:run @state)
               (= [server] (get-in @state [:run :server]))
               (run-continue-until state :success))
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
  ([state side card] (rez-impl state side card nil))
  ([state _side card {:keys [expect-rez] :or {expect-rez true}}]
   (let [card (maybe-card state card)]
     (is' (installed? card) (str (:title card) " is installed"))
     (is' (not (rezzed? card)) (str (:title card) " is unrezzed"))
     (when (and (installed? card)
                (not (rezzed? card)))
       (core/process-action "rez" state :corp {:card card})
       (if expect-rez
         (is' (rezzed? (get-card state card)) (str (:title card) " is rezzed"))
         (is' (not (rezzed? (get-card state card))) (str (:title card) " is still unrezzed")))))))

(defmacro rez
  [state side card & opts]
  `(error-wrapper (rez-impl ~state ~side ~card ~@opts)))

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

(defn click-draw-impl
  [state side]
  (ensure-no-prompts state)
  (core/process-action "draw" state side nil))

(defmacro click-draw
  [state side]
  `(error-wrapper (click-draw-impl ~state ~side)))

(defn click-credit-impl
  [state side]
  (ensure-no-prompts state)
  (core/process-action "credit" state side nil))

(defmacro click-credit
  [state side]
  `(error-wrapper (click-credit-impl ~state ~side)))

(defn click-advance-impl
  [state side card]
  (let [card (get-card state card)]
    (ensure-no-prompts state)
    (is' (some? card) (str (:title card) " exists"))
    (is' (installed? card) (str (:title card) " is installed"))
    (when (and (some? card) (installed? card))
      (is' (core/process-action "advance" state side {:card card}))
      true)))

(defmacro click-advance
  [state side card]
  `(error-wrapper (click-advance-impl ~state ~side ~card)))

(defn trash-impl
  [state side card]
  (let [card (get-card state card)]
    (is' (some? card) (str (:title card) " exists"))
    (when (some? card)
      (is' (core/process-action "trash" state side {:card card}))
      true)))

(defmacro trash
  [state side card]
  `(error-wrapper (trash-impl ~state ~side ~card)))

(defn score-agenda-impl
  [state card]
  (let [card (get-card state card)
        advancementcost (:current-advancement-requirement card)]
    (ensure-no-prompts state)
    (is' (some? card) (str (:title card) " exists"))
    (is' (number? advancementcost) (str (:title card) " has an advancement cost"))
    (when (some? card)
      (core/gain state :corp :click advancementcost :credit advancementcost)
      (core/fake-checkpoint state)
      (dotimes [_ advancementcost]
        (core/process-action "advance" state :corp {:card card}))
      (is' (= advancementcost (get-counters (get-card state card) :advancement)))
      (when (= advancementcost (get-counters (get-card state card) :advancement))
        (core/process-action "score" state :corp {:card card})
        (is' (find-card (:title card) (get-scored state :corp)))
        true))))

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

(defn trash-resource-impl
  "Click-trash a resource as the corp"
  [state]
  (ensure-no-prompts state)
  (let [card (get-card state (get-in @state [:corp :basic-action-card]))
        abilities (:abilities card)
        ab (nth abilities 5)
        cost (core/card-ability-cost state :corp ab card nil)]
    (is' (core/can-pay? state :corp nil cost)))
  (is' (core/process-action "trash-resource" state :corp nil))
  true)

(defmacro trash-resource
  [state]
  `(error-wrapper (trash-resource-impl ~state)))

(defn accessing
  "Checks to see if the runner has a prompt accessing the given card title"
  [state title]
  (= title (-> @state :runner :prompt first :card :title)))

(defn play-and-score-impl
  "Play an agenda from the hand into a new server and score it. Unlike score-agenda, spends a click."
  [state title]
  (ensure-no-prompts state)
  (when (play-from-hand state :corp title "New remote")
    (score-agenda state :corp (get-content state (keyword (str "remote" (dec (:rid @state)))) 0))))

(defmacro play-and-score
  [state title]
  `(error-wrapper (play-and-score-impl ~state ~title)))

(defn damage
  [state side dmg-type qty]
  (core/damage state side (core/make-eid state) dmg-type qty nil)
  (core/fake-checkpoint state))

(defn move
  ([state side card location] (move state side card location nil))
  ([state side card location args]
   (core/move state side card location args)
   (core/fake-checkpoint state)))

(defn draw
  ([state side] (draw state side 1 nil))
  ([state side n] (draw state side n nil))
  ([state side n args]
   (core/draw state side (core/make-eid state) n args)
   (core/fake-checkpoint state)))

(defn purge
  [state side]
  (core/purge state side (core/make-eid state))
  (core/fake-checkpoint state))

(defn trace
  [state base]
  (core/init-trace state :corp
                   (make-card {:title "/trace command" :side "Corp"})
                   {:base base}))

(defn log-str [state]
  (->> (:log @state)
       (map :text)
       (str/join " ")))

(defn print-log [state]
  (prn (log-str state)))

(defmacro do-game [s & body]
  `(let [~'state ~s
         ~'get-corp (fn [] (:corp @~'state))
         ~'get-runner (fn [] (:runner @~'state))
         ~'get-run (fn [] (:run @~'state))
         ~'hand-size (fn [side#] (core/hand-size ~'state side#))
         ~'refresh (fn [card#]
                     ;; ;; uncommenting the below two assertions causes a looot of tests to fail
                     ;; (is ~'card "card passed to refresh should not be nil")
                     (let [~'ret (get-card ~'state card#)]
                       ;; (is ~'ret "(refresh card) is nil - if this is intended, use (core/get-card state card)")
                       ~'ret))
         ~'prompt-map (fn [side#] (-> @~'state side# :prompt first))
         ~'prompt-type (fn [side#] (:prompt-type (~'prompt-map side#)))
         ~'prompt-buttons (fn [side#] (->> (~'prompt-map side#) :choices (map :value)))
         ~'prompt-titles (fn [side#] (map #(or (:title %) %) (~'prompt-buttons side#)))
         ~'prompt-fmt (fn [side#]
                        (let [prompt# (~'prompt-map side#)
                              choices# (:choices prompt#)
                              choices# (cond
                                         (nil? choices#) nil
                                         (sequential? choices#) choices#
                                         :else [choices#])
                              card# (:card prompt#)
                              prompt-type# (:prompt-type prompt#)]
                          (str (utils/side-str side#) ": " (:msg prompt# "") "\n"
                               (when prompt-type# (str "Type: " prompt-type# "\n"))
                               (when card# (str "Card: " (:title card#) "\n"))
                               (str/join "\n" (map #(str "[ " (or (get-in % [:value :title])
                                                                  (:value %)
                                                                  %
                                                                  "nil") " ]") choices#))
                               "\n")))
         ~'print-prompts (fn []
                           (print (~'prompt-fmt :corp))
                           (println (~'prompt-fmt :runner)))]
     ~@body))

(defmacro before-each
  [let-bindings & testing-blocks]
  (assert (every? #(= 'testing (first %)) testing-blocks))
  (let [bundles (for [block testing-blocks] `(let [~@let-bindings] ~block))]
    `(do ~@bundles)))

(defn escape-log-string [s]
  (str/escape s {\[ "\\[" \] "\\]"}))

(defn last-log-contains?
  [state content]
  (->> (-> @state :log last :text)
       (re-find (re-pattern (escape-log-string content)))
       some?))

(defn second-last-log-contains?
  [state content]
  (->> (-> @state :log butlast last :text)
       (re-find (re-pattern (escape-log-string content)))
       some?))

(defn last-n-log-contains?
  [state n content]
  (->> (-> @state :log reverse (nth n) :text)
       (re-find (re-pattern (escape-log-string content)))
       some?))

(defn- make-zone
  [zone replacement]
  (if (and zone (int? zone))
    ;; note: (qty 0 X) evaluates to nil, so we should guard against that
    (let [wrapped [(qty replacement zone)]]
      (if (= wrapped [nil]) [] wrapped))
    zone))

(defn- update-zones
  [players updates]
  (if-not (seq updates)
    players
    (let [zone-replacement (first updates)
          remainder (rest updates)
          target-zone (first zone-replacement)
          replacement (second zone-replacement)
          updated-zone (make-zone (get-in players target-zone) replacement)
          updated-players (assoc-in players target-zone updated-zone)]
      (update-zones updated-players remainder))))

(defn run-and-encounter-ice-test
  ([card] (run-and-encounter-ice-test card nil))
  ([card players] (run-and-encounter-ice-test card players nil))
  ([card players {:keys [counters disable rig server threat] :as args}]
   (let [;; sometimes the number of cards are the only important things - this lets us do :hand X
         ;; or :deck X on either side (so we can reduce noise when reading tests)
         players (update-zones players [[[:corp :hand] "IPO"]
                                        [[:corp :deck] "IPO"]
                                        [[:runner :hand] "Inti"]
                                        [[:runner :deck] "Inti"]])
         players (update-in players [:corp :hand] conj card)
         players (update-in players [:runner :hand] concat rig)
         state (new-game players)
         server (or server "HQ")
         server-key (cond
                      (= server "New remote") :remote1
                      (= server "HQ") :hq
                      (= server "R&D") :rd
                      (= server "Archives") :archives)]
     ;; adjust the threat level for threat: ... subs
     (when threat
       (game.core.change-vals/change
         ;; theoretically, either side is fine!
         state (first (shuffle [:corp :runner])) {:key :agenda-point :delta threat})
       (is (threat-level threat state) "Threat set")
       (is (not (threat-level (inc threat) state)) "Threat is accurate"))
     ;; place the card in the target server
     (play-from-hand state :corp card server)
     (let [ice (get-ice state server-key 0)]
       ;; rez the card in a disabled state to avoid addl costs, abilities, etc
       (when disable (core/disable-card state :corp (get-ice state server-key 0)))
       ;; refund the cost - our default credits should be five
       (core/gain state :corp :credit (:cost ice))
       (rez state :corp (get-ice state server-key 0))
       (when disable (core/enable-card state :corp (get-ice state server-key 0)))
       ;; adjust counters when needed
       (when counters
         ;; counters of the form :counter {:power x :credit x}
         (doseq [[c-type c-count] counters]
           (core/add-counter state :corp (core/make-eid state) (get-ice state server-key 0) c-type c-count)))
       ;; ensure we start with the specified credit count (default 5)
       ;; by not actually clicking for creds
       (core/lose state :corp :click 2)
       (take-credits state :corp)
       ;; install every listed card in the runner rig - should end with default creds and clicks
       (doseq [r rig]
         (let [target-card (first (filter #(= (:title %) r) (:hand (:runner @state))))
               cost (:cost target-card)]
           (core/gain state :runner :credit cost)
           (core/gain state :runner :click 1)
           (play-from-hand state :runner r)))
       ;; runner should have the default click/credit count (- 1 click for the run)
       (run-on state server-key)
       (run-continue-until state :encounter-ice)
       state))))

(defn subroutine-test
  "Create a game, install an ice, encounter it, then fire a subroutine.
    Optionally specify: player map, target server, any number or type of counters,
    if the card should be disabled while rezzed, tags, threat level,
    cards installed in the runner rig"
  ([card sub] (subroutine-test card sub nil))
  ([card sub players] (subroutine-test card sub players nil))
  ([card sub players {:keys [server] :as args}]
   (let [state (run-and-encounter-ice-test card players args)
         server (or server "HQ")
         server-key (cond
                      (= server "New remote") :remote1
                      (= server "HQ") :hq
                      (= server "R&D") :rd
                      (= server "Archives") :archives)
         ice (get-ice state server-key 0)
         ;; :first and :last are useful for reasoning about variable-sub ice
         sub (cond
                (int? sub) sub
                (= :first sub) 0
                (= :last sub) (dec (count (:subroutines (get-ice state server-key 0))))
                :else sub)]
     (card-subroutine state :corp ice sub)
     state)))

(defn fire-all-subs-test
  "Create a game, install an ice, encounter it, then fire all subroutines.
    Optionally specify: player map, target server, any number or type of counters,
    if the card should be disabled while rezzed, tags, threat level,
    cards installed in the runner rig"
  ([card] (fire-all-subs-test card nil))
  ([card players] (fire-all-subs-test card players nil))
  ([card players {:keys [server] :as args}]
   (let [state (run-and-encounter-ice-test card players args)
         server (or server "HQ")
         server-key (cond
                      (= server "New remote") :remote1
                      (= server "HQ") :hq
                      (= server "R&D") :rd
                      (= server "Archives") :archives)
         ice (get-ice state server-key 0)]
     (fire-subs state ice)
     state)))


(defn is-deck-stacked-impl
  [state side expected-deck]
  (let [expected-deck (seq (flatten expected-deck))
        deck (seq (map :title (take (count expected-deck) (get-in @state [side :deck]))))]
    (is' (= expected-deck deck) (str "deck is not " expected-deck))))

(defmacro is-deck-stacked?
  [state side expected-deck]
  `(error-wrapper (is-deck-stacked-impl ~state ~side ~expected-deck)))

(defn provides-mu-impl
  [cname x]
  (let [state (new-game {:runner {:hand [cname] :credits 15}})] ;; todo - install-free
    (take-credits state :corp)
    (play-from-hand state :runner cname)
    (is' (= (+ 4 x) (core/available-mu state)) (str cname " provided " x " memory"))
    state))

(defmacro provides-mu
  [cname x]
  `(error-wrapper (provides-mu-impl ~cname ~x)))

(defn bad-usage [n]
  `(throw (new IllegalArgumentException (str ~n " should only be used inside 'is'"))))

#_{:clj-kondo/ignore [:unused-binding]}
(defmacro changed?
  "bindings & body
  Each binding pair must be an expression and a number.
  The expression will be evaluated before the body and then after, and the two results
  will be compared. If the difference is equal to the binding's number, then the test is
  a pass. Otherwise, it will be a failure. Each binding pair generates a new assertion."
  {:style/indent [1 [[:defn]] :form]}
  [bindings & body]
  (bad-usage "changed?"))
