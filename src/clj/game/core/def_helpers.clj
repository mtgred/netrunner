(ns game.core.def-helpers
  (:require
    [clojure.string :as str]
    [game.core.access :refer [access-bonus]]
    [game.core.board :refer [all-installed get-all-cards]]
    [game.core.card :refer [active? can-be-advanced? corp? faceup? get-card get-counters has-subtype? in-discard? in-hand? operation? runner? ]]
    [game.core.card-defs :as card-defs]
    [game.core.damage :refer [damage]]
    [game.core.drawing :refer [draw]]
    [game.core.eid :refer [effect-completed make-eid]]
    [game.core.engine :refer [queue-event register-events resolve-ability trigger-event trigger-event-sync unregister-event-by-uuid]]
    [game.core.effects :refer [any-effects is-disabled-reg?]]
    [game.core.gaining :refer [gain-credits lose-credits]]
    [game.core.installing :refer [corp-install]]
    [game.core.moving :refer [move trash]]
    [game.core.payment :refer [build-cost-string can-pay?]]
    [game.core.play-instants :refer [async-rfg]]
    [game.core.prompts :refer [cancellable clear-wait-prompt]]
    [game.core.props :refer [add-counter]]
    [game.core.revealing :refer [conceal-hand reveal reveal-hand reveal-loud]]
    [game.core.runs :refer [can-run-server? make-run jack-out]]
    [game.core.say :refer [play-sfx system-msg system-say]]
    [game.core.servers :refer [zone->name]]
    [game.core.shuffling :refer [shuffle!]]
    [game.core.to-string :refer [card-str]]
    [game.core.toasts :refer [toast]]
    [game.core.tags :refer [gain-tags]]
    [game.macros :refer [continue-ability effect msg req wait-for]]
    [game.utils :refer [enumerate-cards remove-once same-card? server-card to-keyword quantify]]
    [jinteki.utils :refer [faction-label other-side]]))

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
                   " will be " (enumerate-cards (reverse chosen)) ".")
              (str "The top cards of " (if (= reorder-side :corp) "R&D" "the stack")
                   " will be " (enumerate-cards chosen) "."))
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
   :req (req (and (same-card? card (:card context))
                  (not (get-in card [:special :skipped-loading]))
                  (not (pos? (get-counters card counter-type)))))
   :effect (effect (system-msg (str "removes " (:title card) " from the game"))
                   (move card :rfg))})

(defn trash-on-empty
  "Used in :event maps for effects like Daily Casts"
  [counter-type]
  {:event :counter-added
   :req (req (and (same-card? card (:card context))
                  (not (get-in card [:special :skipped-loading]))
                  (not (pos? (get-counters card counter-type)))))
   :async true
   :effect (effect (system-msg (str "trashes " (:title card)))
                   (trash eid card {:unpreventable true :source-card card}))})

(defn pick-tiered-sfx
  [base upper-limit n]
  (cond
    (not (pos? n)) nil
    (= n 1) base
    (< n upper-limit) (str base "-" n)
    :else (str base "-" upper-limit)))

(defn play-tiered-sfx
  [state side base upper-limit n]
  (when-let [sfx (pick-tiered-sfx base upper-limit n)]
    (play-sfx state side sfx)))

(defn draw-abi
  "shorthand ability to draw x cards (apply args to the draw fn)"
  ([x] (draw-abi x nil))
  ([x draw-args] (draw-abi x draw-args nil))
  ([x draw-args ab-base]
   (merge {:msg (msg "draw " (quantify x "card"))
           :label (str "Draw " (quantify x "card"))
           :async true
           :effect (req (when (:action ab-base) (play-tiered-sfx state side "click-card" 3 x))
                        (draw state side eid x draw-args))}
          ab-base)))

(defn draw-loud
  "Draw n cards, using the given card as the source, and logging that cards were drawn"
  ([state side eid card n] (draw-loud state side eid card n nil))
  ([state side eid card n args]
   (resolve-ability state side eid (draw-abi n args) card nil)))

(defn give-tags
  "Basic give runner n tags ability."
  [n]
  {:label (str "Give the Runner " (quantify n "tag"))
   :msg (str "give the Runner " (quantify n "tag"))
   :interactive (req true)
   :async true
   :effect (effect (gain-tags :corp eid n))})

(defn run-server-ability
  "Runs a target server, if possible. "
  ([server] (run-server-ability server nil))
  ([server {:keys [events] :as ab-base}]
   (merge {:async true
           :change-in-game-state {:req (req (can-run-server? state server))}
           :label (str "run " (zone->name server))
           :msg (str "make a run on " (zone->name server))
           :makes-run true
           :effect (req (when (seq events)
                          (register-events state side card events))
                        (when (:action ab-base)
                          (play-sfx state side "click-run"))
                        (make-run state side eid server card))}
          (dissoc ab-base :events))))

(defn run-any-server-ability
  ([] (run-any-server-ability nil))
  ([{:keys [events] :as ab-base}]
   (merge {:async true
           :prompt "Choose a server"
           :choices (req runnable-servers)
           :label "Run a server"
           :makes-run true
           :msg (msg "make a run on " target)
           :effect (req (when (seq events)
                          (register-events state side card events))
                        (when (:action ab-base)
                          (play-sfx state side "click-run"))
                        (make-run state side eid target card))}
          (dissoc ab-base :events))))

(def run-remote-server-ability
  {:async true
   :prompt "Choose a remote server"
   :change-in-game-state {:req (req (seq (filter #(can-run-server? state %) remotes)))}
   :choices (req (filter #(can-run-server? state %) remotes))
   :label "Run a remote server"
   :msg (msg "make a run on " target)
   :effect (effect (make-run eid target card))})

(def run-central-server-ability
  {:prompt "Choose a central server"
   :choices (req (filter #{"HQ" "R&D" "Archives"} runnable-servers))
   :change-in-game-state {:req (req (seq (filter #{"HQ" "R&D" "Archives"} runnable-servers)))}
   :async true
   :label "Run a central server"
   :msg (msg "make a run on " target)
   :effect (effect (make-run eid target card))})

(defn run-server-from-choices-ability
  [choices]
  {:prompt "Choose a server"
   :choices (req (filter #(can-run-server? state %) choices))
   :change-in-game-state {:req (req (seq (filter (set choices) runnable-servers)))}
   :async true
   :msg (msg "make a run on " target)
   :effect (effect (make-run eid target card))})

(defn take-credits
  "Take n counters from a card and place them in your credit pool as if they were credits (if possible)"
  ([state side eid card type n] (take-credits state side eid card type n nil))
  ([state side eid card type n args]
   (if-let [card (get-card state card)]
     (let [n (if (= :all n) (get-counters card type) n)
           n (min n (get-counters card type))]
       (if (pos? n)
         (wait-for (add-counter state side card type (- n) {:placed true :suppress-checkpoint true})
                   ;; (queue-event state side :spent-credits-from-card card)
                   (gain-credits state side eid n args))
         (effect-completed state side eid)))
     (effect-completed state side eid))))

(defn take-n-credits-ability
  ([n] (take-n-credits-ability n "card" nil))
  ([n t] (take-n-credits-ability n t nil))
  ([n t ab-base]
   (merge
     {:label (str "Take " n " [Credits] from this " t)
      :change-in-game-state {:req (req (pos? (get-counters card :credit))) :silent (req (not (:action ab-base)))}
      :msg (msg "gain " (min n (get-counters card :credit)) " [Credits]")
      :async true
      :effect (req (when (:action ab-base) (play-tiered-sfx state side "click-credit" 3 n))
                   (take-credits state side eid card :credit n))}
     ab-base)))

(defn take-all-credits-ability
  [ab-base]
  (merge
    {:label "Take all hosted credits"
     :change-in-game-state {:req (req (pos? (get-counters card :credit)))}
     :async true
     :msg (msg "gain " (get-counters card :credit) " [Credits]")
     :effect (req (when (:action ab-base) (play-tiered-sfx state side "click-credit" 3 (get-counters card :credit)))
                  (take-credits state side eid card :credit :all))}
    ab-base))

(defn in-hand*?
  "Card is 'in-hand' for the purposes of being installed only"
  [state card]
  (or (in-hand? card)
      (any-effects state (:side card) :can-play-as-if-in-hand true? card)))

(defn all-cards-in-hand*
  "All cards that are in-hand for the purposes of being installed only"
  [state side]
  (filter #(and (if (= side :runner)
                  (runner? %)
                  (corp? %))
                (in-hand*? state %))
          (get-all-cards state)))

;; NOTE - update this as the above (take credits) is updated
(defn spend-credits
  "Take n counters from a card and place them in your credit pool (if possible) - and trigger an event as if the credits were spent"
  ([state side eid card type n] (spend-credits state side eid card type n nil))
  ([state side eid card type n args]
   (if-let [card (get-card state card)]
     (let [n (if (= :all n) (get-counters card type) n)
           n (min n (get-counters card type))]
       (if (pos? n)
         (wait-for (add-counter state side card type (- n) {:placed true :suppress-checkpoint true})
                   (queue-event state :spent-credits-from-card {:card card})
                   (gain-credits state side eid n args))
         (effect-completed state side eid)))
     (effect-completed state side eid))))

(defn make-recurring-ability
  [ability]
  (if (:recurring ability)
    (let [recurring-ability
          {:msg "take 1 [Recurring Credits]"
           :req (req (pos? (get-counters card :recurring)))
           :async true
           :effect (req (spend-credits state side eid card :recurring 1))}]
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
  (let [card (server-card title false)]
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

(defn something-can-be-advanced?
  "There's either a card on the field that can be advanced, or a card that has the potential to be an advancable card (hidden info)"
  [state]
  (some #(or (not (faceup? %)) (can-be-advanced? state %)) (all-installed state :corp)))

(defn corp-install-up-to-n-cards
  "Ability to install up to n corp cards"
  ([n] (corp-install-up-to-n-cards n nil))
  ([n args]
   {:prompt (str "install a card from HQ" (when (> n 1) (str " (" n " remaining)")))
    :choices {:card (every-pred corp? in-hand? (complement operation?))}
    :async true
    :effect (req (wait-for
                   (corp-install state side target nil (merge {:msg-keys {:install-source card}} args))
                   (if (> n 1)
                     (continue-ability state side (corp-install-up-to-n-cards (dec n)) card nil)
                     (effect-completed state side eid))))}))

(defn gain-credits-ability [x]
  {:msg (str "gain " x " [Credits]")
   :label (str "gain " x " [Credits]")
   :async true
   :effect (req (gain-credits state side eid x))})

(defn drain-credits
  ([draining-side victim-side qty] (drain-credits draining-side victim-side qty 1))
  ([draining-side victim-side qty multiplier] (drain-credits draining-side victim-side qty multiplier 0))
  ([draining-side victim-side qty multiplier tags-to-gain]
   (letfn [(to-drain [state] (let [qty (if (number? qty)
                                         qty
                                         (qty state draining-side (make-eid state) nil nil))]
                               (min (get-in @state [victim-side :credit] 0) qty)))
           (to-gain [state] (* (to-drain state) multiplier))]
     {:msg (msg "force the " (str/capitalize (name victim-side)) " to lose "
                (to-drain state) " [Credits], " (when (zero? tags-to-gain) " and ")
                "gain " (to-gain state) " [Credits]"
                (when (pos? tags-to-gain)
                  (str (if (= :corp draining-side)
                         ", and give Runner "
                         ", and take ")
                       (quantify tags-to-gain "tag"))))
      :async true
      :effect (req (let [c-drain (to-drain state)
                         c-gain (to-gain state)]
                     (if (zero? tags-to-gain)
                       (wait-for (lose-credits state victim-side c-drain {:suppress-checkpoint true})
                                 (gain-credits state draining-side eid c-gain))
                       (wait-for (gain-tags state :draining-side tags-to-gain)
                                 (wait-for (lose-credits state victim-side c-drain {:suppress-checkpoint true})
                                           (gain-credits state draining-side eid c-gain))))))})))

(defn corp-recur
  ([] (corp-recur (constantly true)))
  ([pred]
   {:label "add card from Archives to HQ"
    :prompt "Choose a card to add to HQ"
    :does-something (req (seq (:discard corp)))
    :waiting-prompt true
    :show-discard true
    :choices {:card #(and (corp? %)
                       (in-discard? %)
                       (pred %))}
    :msg (msg "add " (card-str state target {:visible (faceup? target)}) " to HQ")
    :effect (effect (move :corp target :hand))}))

(defn tutor-abi
  "Tutor a card. Optionally, pass a restriction, which is a 1-fn the cards must pass"
  ([reveal?] (tutor-abi reveal nil))
  ([reveal? restriction]
   {:change-in-game-state {:req (req (seq (get-in @state [side :deck])))}
    :prompt "Choose a card"
    :label (req (if (= side :corp)
                  "Search R&D and add 1 card to HQ"
                  "Search the Stack and add 1 card to the Grip"))
    :choices (req (cancellable
                    (filter #(or (not restriction) (restriction %))
                            (get-in @state [side :deck]))
                    :sorted))
    :msg (msg "search "
              (if (= side :corp) "R&D" "[their] Stack")
              " for "
              (if reveal? (:title target) "a card")
              " and add it to "
              (if (= side :corp) "HQ" "[their] Grip"))
    :cancel-effect (req (when (= side :runner)
                          (trigger-event state side :searched-stack))
                        (system-msg state side "shuffles their deck!")
                        (shuffle! state side :deck)
                        (effect-completed state side eid))
    :async true
    :effect (req (when (= side :runner)
                   (trigger-event state side :searched-stack))
                 (if reveal?
                   (wait-for (reveal state side target)
                             (move state side target :hand)
                             (shuffle! state side :deck)
                             (effect-completed state side eid))
                   (do (move state side target :hand)
                       (shuffle! state side :deck)
                       (effect-completed state side eid))))}))

(def card-defs-cache (atom {}))

(def trash-on-purge
  {:event :purge
   :async true
   :msg "trash itself"
   :effect (req (trash state :runner eid card {:cause :purge
                                               :cause-card card}))})

(defn scry
  "Looks at the top QUANT cards of target-side's deck. Completes an eid."
  [state side eid card target-side quant]
  (let [target-cards (take quant (get-in @state [target-side :deck]))
        zone-name (if (= :corp target-side) "R&D" "the stack")]
    (resolve-ability
      state side eid
      {:player side
       :waiting-prompt true
       :req (req (seq target-cards))
       :choices ["OK"]
       :prompt (if (= 1 (count target-cards))
                 (msg "the top card of " zone-name " is " (:title (first target-cards)))
                 (msg "the top " (quantify quant "card") " of " zone-name " are (top->bottom): " (enumerate-cards target-cards)))}
      card nil)))

(defn with-revealed-hand
  "Resolves an ability while a player has their hand revealed (so you can click cards in their hand)
  You can set the side that triggers the reveal (event-side) and if it displays as a forced reveal
  (forced) via the args"
  ([target-side abi] (with-revealed-hand target-side nil abi))
  ([target-side {:keys [event-side forced skip-reveal] :as args} abi]
   ;; note - if the target draws (ie with steelskin), then the hand should be unrevealed again
   ;; this only matters if a card like buffer drive or aniccam is in play that causes a prompt
   ;; and the server sends the paused state back with the new cards faceup
   (letfn [(maybe-register-ev
             [state side card was-open?]
             (if-not was-open?
               (let [uuid (:uuid (first (register-events state side card
                                                         [{:event :card-moved
                                                           :req (req (let [sidefn (if (= :corp target-side) corp? runner?)]
                                                                       (and (sidefn (:moved-card context))
                                                                            (in-hand? (:moved-card context)))))
                                                           :silent (req true)
                                                           :effect (req (conceal-hand state target-side))}])))]
                 (fn [] (unregister-event-by-uuid state side uuid)))
               (fn [] nil)))
           (maybe-reveal
             [state side eid card target-side {:keys [event-side forced skip-reveal] :as args}]
             (if skip-reveal
               (effect-completed state side eid)
               (reveal-loud state (or event-side side) eid card args (get-in @state [target-side :hand]))))]
     {:async true
      :effect (req (wait-for
                     (maybe-reveal state side card target-side args)
                     (let [was-open? (get-in @state [target-side :openhand])
                           unregister-ev-callback (maybe-register-ev state side card was-open?)]
                       (when-not was-open? (reveal-hand state target-side))
                       (wait-for (resolve-ability state side abi card targets)
                                 (when-not was-open? (conceal-hand state target-side))
                                 (unregister-ev-callback)
                                 (effect-completed state side eid)))))})))

(defn make-icon
  [text card]
  [text (:cid card) (faction-label card)])

(defmacro defcard
  "Define a card to be returned from card-def. The definition consists of the
  title (a string), 0 or more transformers and a body (an expression, usually
  a map). Each tranformer should be a symbol or a list, symbols are wrapped
  in lists and then the last transformer has the body inserted at the end,
  then the second last has the result inserted as its last item and so on."
  {:arglists '([title ability] [title transformers* ability])}
  [title body & more]
  `(do (swap! card-defs-cache dissoc ~title)
       (defmethod card-defs/defcard-impl ~title [~'_]
         (or (get @card-defs-cache ~title)
             (let [ability# (->> ~@(reverse (cons body more))
                                 (add-default-abilities ~title))]
               (swap! card-defs-cache assoc ~title ability#)
               ability#)))))
