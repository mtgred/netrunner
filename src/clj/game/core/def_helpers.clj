(ns game.core.def-helpers
  (:require
    [clojure.string :as str]
    [game.core.access :refer [access-bonus]]
    [game.core.card :refer [active? corp? faceup? get-card get-counters has-subtype? in-discard?]]
    [game.core.card-defs :as card-defs]
    [game.core.damage :refer [damage]]
    [game.core.eid :refer [effect-completed]]
    [game.core.engine :refer [resolve-ability trigger-event-sync]]
    [game.core.effects :refer [is-disabled-reg?]]
    [game.core.gaining :refer [gain-credits]]
    [game.core.moving :refer [move trash]]
    [game.core.play-instants :refer [async-rfg]]
    [game.core.prompts :refer [clear-wait-prompt]]
    [game.core.props :refer [add-counter]]
    [game.core.runs :refer [jack-out]]
    [game.core.say :refer [system-msg system-say]]
    [game.core.to-string :refer [card-str]]
    [game.core.toasts :refer [toast]]
    [game.macros :refer [continue-ability effect msg req wait-for]]
    [game.utils :refer [enumerate-str remove-once same-card? server-card to-keyword]]
    [jinteki.utils :refer [other-side]]))

(defn combine-abilities
  "Combines two or more abilities to a single one. Labels are joined together with a period between parts."
  ([ab-x ab-y]
   {:label (str (:label ab-x) ". " (:label ab-y))
    :async true
    :effect (req (wait-for (resolve-ability state side ab-x card nil)
                           (continue-ability state side ab-y card nil)))})
  ([ab-x ab-y & ab-more]
   (reduce combine-abilities (combine-abilities ab-x ab-y) ab-more)))

(def corp-rez-toast
  "Effect to be placed with `:runner-turn-ends` to remind players of 'when turn begins'
  triggers"
  {:event :runner-turn-ends
   :effect (req (toast state :corp "Reminder: You have unrezzed cards with \"when turn begins\" abilities." "info"))})

(declare reorder-final) ; forward reference since reorder-choice and reorder-final are mutually recursive

(defn reorder-choice
  "Generates a recursive prompt structure for cards that do reordering (Indexing, Making an Entrance, etc.)

  reorder-side is the side to be reordered, i.e. :corp for Indexing and Precognition.
  wait-side is the side that has a wait prompt while ordering is in progress, i.e. :corp for Indexing and Spy Camera.

  This is part 1 - the player keeps choosing cards until there are no more available choices. A wait prompt should
  exist before calling this function. See Indexing and Making an Entrance for examples on how to call this function."

  ([reorder-side cards] (reorder-choice reorder-side (other-side reorder-side) cards `() (count cards) cards nil))
  ([reorder-side wait-side remaining chosen n original] (reorder-choice reorder-side wait-side remaining chosen n original nil))
  ([reorder-side wait-side remaining chosen n original dest]
   (when (not-empty remaining)
     {:prompt (str "Choose a card to move next "
                   (if (= dest "bottom") "under " "onto ")
                   (if (= reorder-side :corp) "R&D" "the stack"))
      :choices remaining
      :async true
      :effect (req (let [chosen (cons target chosen)]
                     (if (< (count chosen) n)
                       (continue-ability
                         state side
                         (reorder-choice reorder-side wait-side (remove-once #(= target %) remaining) chosen n original dest)
                         card nil)
                       (continue-ability
                         state side
                         (reorder-final reorder-side wait-side chosen original dest)
                         card nil))))})))

(defn- reorder-final
  "Generates a recursive prompt structure for cards that do reordering (Indexing, Making an Entrance, etc.)
  This is part 2 - the player is asked for final confirmation of the reorder and is provided an opportunity to start over."

  ([reorder-side wait-side chosen original] (reorder-final reorder-side wait-side chosen original nil))
  ([reorder-side wait-side chosen original dest]
   {:prompt (if (= dest "bottom")
              (str "The bottom cards of " (if (= reorder-side :corp) "R&D" "the stack")
                   " will be " (enumerate-str (map :title (reverse chosen))) ".")
              (str "The top cards of " (if (= reorder-side :corp) "R&D" "the stack")
                   " will be " (enumerate-str (map :title chosen)) "."))
   :choices ["Done" "Start over"]
   :async true
   :effect (req
             (cond
               (and (= dest "bottom") (= target "Done"))
               (do (swap! state update-in [reorder-side :deck]
                          #(vec (concat (drop (count chosen) %) (reverse chosen))))
                   (when (and (= :corp reorder-side)
                              (:run @state)
                              (:access @state))
                     (swap! state assoc-in [:run :shuffled-during-access :rd] true))
                   (clear-wait-prompt state wait-side)
                   (effect-completed state side eid))

               (= target "Done")
               (do (swap! state update-in [reorder-side :deck]
                          #(vec (concat chosen (drop (count chosen) %))))
                   (when (and (= :corp reorder-side)
                              (:run @state)
                              (:access @state))
                     (swap! state assoc-in [:run :shuffled-during-access :rd] true))
                   (clear-wait-prompt state wait-side)
                   (effect-completed state side eid))

               :else
               (continue-ability state side (reorder-choice reorder-side wait-side original '() (count original) original dest) card nil)))}))

(defn breach-access-bonus
  "Access additional cards when breaching a server"
  ([server bonus] (breach-access-bonus server bonus nil))
  ([server bonus {:keys [duration msg] :as args}]
   {:event :breach-server
    :duration duration
    :req (if (:req args)
           (:req args)
           (req (= server target)))
    :msg msg
    :effect (effect (access-bonus :runner server bonus))}))

(defn do-net-damage
  "Do specified amount of net-damage."
  [dmg]
  {:label (str "Do " dmg " net damage")
   :async true
   :msg (str "do " dmg " net damage")
   :effect (effect (damage eid :net dmg {:card card}))})

(defn do-meat-damage
  "Do specified amount of meat damage."
  [dmg]
  {:label (str "Do " dmg " meat damage")
   :async true
   :msg (str "do " dmg " meat damage")
   :effect (effect (damage eid :meat dmg {:card card}))})

(defn do-brain-damage
  "Do specified amount of core damage."
  [dmg]
  {:label (str "Do " dmg " core damage")
   :async true
   :msg (str "do " dmg " core damage")
   :effect (effect (damage eid :brain dmg {:card card}))})

(defn rfg-on-empty
  "Used in :event maps for effects like Malandragem"
  [counter-type]
  {:event :counter-added
   :req (req (and (same-card? card target)
                  (not (pos? (get-counters card counter-type)))))
   :effect (effect (system-msg (str "removes " (:title card) " from the game"))
                   (move card :rfg))})

(defn trash-on-empty
  "Used in :event maps for effects like Daily Casts"
  [counter-type]
  {:event :counter-added
   :req (req (and (same-card? card target)
                  (not (pos? (get-counters card counter-type)))))
   :async true
   :effect (effect (system-msg (str "trashes " (:title card)))
                   (trash eid card {:unpreventable true :source-card card}))})

(defn make-recurring-ability
  [ability]
  (if (:recurring ability)
    (let [recurring-ability
          {:msg "take 1 [Recurring Credits]"
           :req (req (pos? (get-counters card :recurring)))
           :async true
           :effect (req (add-counter state side card :recurring -1)
                        (wait-for (gain-credits state side 1)
                                  (trigger-event-sync state side eid :spent-credits-from-card (get-card state card))))}]
      (update ability :abilities #(conj (into [] %) recurring-ability)))
    ability))

(defn trash-or-rfg
  [state _ eid card]
  (let [side (to-keyword (:side card))
        title (:title card)]
    (if (:rfg-instead-of-trashing card)
      (do (system-say state side (str title " is removed from the game."))
          (async-rfg state side eid card))
      (do (system-say state side (str title " is trashed."))
          (trash state side eid card {:unpreventable true :game-trash true})))))

(defn offer-jack-out
  "Returns an ability that prompts the Runner to jack out"
  ([] (offer-jack-out nil))
  ([{jack-out-req :req
     once :once}]
   {:optional
    {:player :runner
     :req (req (if jack-out-req
                 (jack-out-req state side eid card targets)
                 true))
     :once once
     :prompt "Jack out?"
     :waiting-prompt true
     :yes-ability {:async true
                   :effect (effect (system-msg :runner (str "uses " (:title card) " to jack out"))
                                   (jack-out eid))}
     :no-ability {:effect (effect (system-msg :runner (str "uses " (:title card) " to continue the run")))}}}))

(defn get-x-fn []
  (fn get-x-fn-inner
    [state side eid card targets]
    (if-let [x-fn (and (not (is-disabled-reg? state card)) (:x-fn card))]
      (x-fn state side eid card targets)
      0)))

(defn make-current-event-handler
  [title ability]
  (let [card (server-card title)]
    (if (has-subtype? card "Current")
      (let [event-keyword (if (corp? card) :agenda-stolen :agenda-scored)
            static-ab {:type :trash-when-expired
                       :req (req (some #(let [event (:event %)
                                              context-card (:card %)]
                                          (or (= event event-keyword)
                                              (and (#{:play-event :play-operation} event)
                                                   (and (not (same-card? card context-card))
                                                        (has-subtype? context-card "Current")
                                                        true))))
                                       targets))
                       :value trash-or-rfg}]
        (update ability :static-abilities #(conj (into [] %) static-ab)))
      ability)))

(defn add-default-abilities
  [title ability]
  (->> ability
       (make-current-event-handler title)
       (make-recurring-ability)))

(defn corp-recur
  ([] (corp-recur (constantly true)))
  ([pred]
   {:label "add card from Archives to HQ"
    :prompt "Choose a card to add to HQ"
    :waiting-prompt true
    :show-discard true
    :choices {:card #(and (corp? %)
                       (in-discard? %)
                       (pred %))}
    :msg (msg "add " (card-str state target {:visible (faceup? target)}) " to HQ")
    :effect (effect (move :corp target :hand))}))

(def card-defs-cache (atom {}))

(defmacro defcard
  [title ability]
  `(do (swap! card-defs-cache dissoc ~title)
       (defmethod card-defs/defcard-impl ~title [~'_]
         (or (get @card-defs-cache ~title)
             (let [ability# (add-default-abilities ~title ~ability)]
               (swap! card-defs-cache assoc ~title ability#)
               ability#)))))
