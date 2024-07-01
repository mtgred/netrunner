(ns game.cards.ice
  (:require
   [clojure.string :as str]
   [cond-plus.core :refer [cond+]]
   [game.core.access :refer [access-bonus access-card breach-server max-access]]
   [game.core.bad-publicity :refer [gain-bad-publicity]]
   [game.core.board :refer [all-active-installed all-installed all-installed-runner 
                            all-installed-runner-type card->server
                            get-all-cards get-all-installed server->zone]]
   [game.core.card :refer [active? agenda? asset? card-index can-be-advanced?
                           corp? corp-installable-type? faceup?
                           get-card get-counters get-zone
                           hardware? has-subtype? ice? in-discard? in-hand? installed? is-type? operation?
                           program? protecting-a-central? protecting-archives? protecting-hq? protecting-rd?
                           resource? rezzed? runner?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.costs :refer [total-available-credits]]
   [game.core.damage :refer [damage]]
   [game.core.def-helpers :refer [combine-abilities corp-recur defcard
                                  do-brain-damage do-net-damage offer-jack-out
                                  reorder-choice get-x-fn]]
   [game.core.drawing :refer [draw maybe-draw draw-up-to]]
   [game.core.effects :refer [get-effects is-disabled? is-disabled-reg? register-lingering-effect unregister-effects-for-card unregister-static-abilities update-disabled-cards]]
   [game.core.eid :refer [complete-with-result effect-completed make-eid]]
   [game.core.engine :refer [gather-events pay register-default-events register-events
                             resolve-ability trigger-event trigger-event-simult unregister-events
                             ]]
   [game.core.events :refer [first-event? run-events]]
   [game.core.finding :refer [find-cid]]
   [game.core.flags :refer [can-rez? card-flag? prevent-draw prevent-jack-out
                            register-run-flag! register-turn-flag! run-flag? zone-locked?]]
   [game.core.gaining :refer [gain-credits lose-clicks lose-credits]]
   [game.core.hand-size :refer [hand-size]]
   [game.core.hosting :refer [host]]
   [game.core.ice :refer [add-sub add-sub! any-subs-broken? break-sub get-current-ice ice-strength-bonus
                          remove-sub! remove-subs! resolve-subroutine
                          set-current-ice unbroken-subroutines-choice update-all-ice update-all-icebreakers
                          update-ice-strength]]
   [game.core.identities :refer [disable-card enable-card]]
   [game.core.initializing :refer [card-init]]
   [game.core.installing :refer [corp-install corp-install-list
                                 corp-install-msg]]
   [game.core.memory :refer [available-mu init-mu-cost]]
   [game.core.moving :refer [as-agenda mill move swap-cards swap-cards-async
                             swap-ice swap-installed trash
                             trash-cards]]
   [game.core.optional :refer [get-autoresolve set-autoresolve]]
   [game.core.payment :refer [can-pay? cost->string build-cost-label ->c]]
   [game.core.prompts :refer [cancellable clear-wait-prompt]]
   [game.core.props :refer [add-counter add-icon add-prop remove-icon]]
   [game.core.purging :refer [purge]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [derez get-rez-cost rez]]
   [game.core.runs :refer [bypass-ice encounter-ends end-run
                           force-ice-encounter get-current-encounter prevent-access
                           redirect-run set-next-phase]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [central->name protecting-same-server?
                              target-server zone->name]]
   [game.core.shuffling :refer [shuffle!]]
   [game.core.subtypes :refer [update-all-subtypes]]
   [game.core.tags :refer [gain-tags lose-tags sum-tag-effects]]
   [game.core.threat :refer [threat threat-level]]
   [game.core.to-string :refer [card-str]]
   [game.core.toasts :refer [toast]]
   [game.core.update :refer [update!]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]))

;;;; Helper functions specific for ice
(defn reset-variable-subs
  ([state side card total sub] (reset-variable-subs state side card total sub nil))
  ([state side card total sub args]
   (let [args (merge {:variable true} args)
         old-subs (remove #(and (= (:cid card) (:from-cid %))
                                (:variable %))
                          (:subroutines card))
         new-card (assoc card :subroutines old-subs)
         new-subs (if (pos? total)
                    (->> (range total)
                         (reduce (fn [ice _] (add-sub ice sub (:cid ice) args)) new-card)
                         :subroutines
                         (into []))
                    (into [] (map-indexed (fn [idx sub] (assoc sub :index idx)) (:subroutines new-card))))
         new-card (assoc new-card :subroutines new-subs)]
     (update! state :corp new-card)
     (trigger-event state side :subroutines-changed (get-card state new-card)))))

(defn gain-variable-subs
  ([state side card total sub] (gain-variable-subs state side card total sub nil))
  ([state side card total sub args]
   (let [args (merge {:variable true} args)
         new-subs (->> (range total)
                       (reduce (fn [ice _] (add-sub ice sub (:cid ice) args)) card)
                       :subroutines
                       (into []))
         new-card (assoc card :subroutines new-subs)]
     (update! state :corp new-card)
     (trigger-event state side :subroutines-changed (get-card state new-card)))))

(defn reset-printed-subs
  ([state side card total sub] (reset-printed-subs state side card total sub {:printed true}))
  ([state side card total sub args]
   (let [old-subs (remove #(and (= (:cid card) (:from-cid %))
                                (:printed %))
                          (:subroutines card))
         new-card (assoc card :subroutines old-subs)
         new-subs (->> (range total)
                       (reduce (fn [ice _] (add-sub ice sub (:cid ice) args)) new-card)
                       :subroutines)
         new-card (assoc new-card :subroutines new-subs)]
     (update! state :corp new-card)
     (trigger-event state side :subroutines-changed (get-card state new-card)))))

(defn all-subroutines-not-broken-by
  ([state side card type context]
   (let [ice (:ice context)
         subs (:subroutines ice)]
     (and (= ice card)
          ;; printed and (not broken or not containing the subtype)
          (some #(and (:printed %)
                    (or (not (:broken %))
                        (not (has-subtype? (find-cid (:breaker %) (all-installed state :runner))type))))
                subs)))))

;;; Checks if the runner has active events that would force them to avoid/prevent a tag
(defn forced-to-avoid-tags?
  [state side]
  (let [cards (map :card (gather-events state side :pre-tag nil))]
    (pos? (count (filter #(card-flag? % :forced-to-avoid-tag true) cards)))))

;;; Break abilities on ice should only occur when encountering that ice
(defn currently-encountering-card
  [card state]
  (same-card? (:ice (get-current-encounter state)) card))

;;; Runner abilites for breaking subs
(defn bioroid-break
  ([cost qty] (bioroid-break cost qty nil))
  ([cost qty args]
   (break-sub [(->c :lose-click cost)] qty nil
              (assoc args :req (req (and (not (is-disabled-reg? state card))
                                         (currently-encountering-card card state)))))))

;;; General subroutines
(def end-the-run
  "Basic ETR subroutine"
  {:label "End the run"
   :msg "end the run"
   :async true
   :effect (effect (end-run :corp eid card))})

(def end-the-run-if-tagged
  "ETR subroutine if tagged"
  {:label "End the run if the Runner is tagged"
   :req (req tagged)
   :msg "end the run"
   :async true
   :effect (effect (end-run :corp eid card))})

;; helper for the faceup-archives-count cards
(defn- faceup-archives-types [corp]
  (count (distinct (map :type (filter faceup? (:discard corp))))))

(defn maybe-draw-sub
  "Ability to (maybe) draw a set number of cards"
  [qty]
  {:async true
   :label (str "You may draw " (quantify qty "card"))
   :effect (effect (maybe-draw eid card qty))})

(defn draw-up-to-sub
  "Ability to draw between 0 and n cards"
  ([qty] (draw-up-to-sub qty nil))
  ([qty args]
   {:async true
    :label (str "Draw up to " (quantify qty "card"))
    :effect (effect (draw-up-to eid card qty args))}))

(defn runner-pays
  "Ability to pay to avoid a subroutine by paying a resource"
  [cost]
  {:async true
   :effect (req (wait-for (pay state :runner (make-eid state eid) card cost)
                          (when-let [payment-str (:msg async-result)]
                            (system-msg state :runner
                                        (str payment-str
                                             " due to " (:title card)
                                             " subroutine")))
                          (effect-completed state side eid)))})

(defn end-the-run-unless-runner-pays
  ([cost] (end-the-run-unless-runner-pays cost "subroutine"))
  ([cost reason]
   {:player :runner
    :async true
    :label (str "End the run unless the Runner pays " (build-cost-label cost))
    :prompt "Choose one"
    :waiting-prompt true
    :choices (req ["End the run"
                   (when (can-pay? state :runner eid card nil cost)
                     (capitalize (cost->string cost)))])
    :msg (msg (if (= "End the run" target)
                (decapitalize target)
                (str "force the runner to " (decapitalize target))))
    :effect (req (if (= "End the run" target)
                   (end-run state :corp eid card)
                   (wait-for (pay state :runner (make-eid state eid) card cost)
                             (when-let [payment-str (:msg async-result)]
                               (system-msg state :runner
                                           (str payment-str
                                                " due to " (:title card)
                                                " " reason)))
                             (effect-completed state side eid))))}))

(defn end-the-run-unless-corp-pays
  [cost]
  {:async true
   :label (str "End the run unless the Corp pays " (build-cost-label cost))
   :prompt "Choose one"
   :waiting-prompt true
   :choices (req ["End the run"
                  (when (can-pay? state :corp eid card nil cost)
                    (capitalize (cost->string cost)))])
   :msg (msg (decapitalize target))
   :effect (req (if (= "End the run" target)
                  (end-run state :corp eid card)
                  (wait-for (pay state :corp (make-eid state eid) card cost)
                            (when-let [payment-str (:msg async-result)]
                              (system-msg state :corp payment-str))
                            (effect-completed state side eid))))})

(defn end-the-run-unless-runner
  [label prompt ability]
  {:player :runner
   :async true
   :label (str "End the run unless the Runner " label)
   :prompt "Choose one"
   :waiting-prompt true
   :choices ["End the run"
             (capitalize prompt)]
   :effect (req (if (= "End the run" target)
                  (do (system-msg state :corp
                                  (str "uses " (:title card) " to end the run"))
                      (end-run state :corp eid card))
                  (continue-ability state side ability card nil)))})

(defn give-tags
  "Basic give runner n tags subroutine."
  [n]
  {:label (str "Give the Runner " (quantify n "tag"))
   :msg (str "give the Runner " (quantify n "tag"))
   :async true
   :effect (effect (gain-tags :corp eid n))})

(def add-power-counter
  "Places 1 power counter on a card."
  {:label "Place 1 power counter"
   :msg "place 1 power counter on itself"
   :effect (effect (add-counter card :power 1))})

(defn trace-ability
  "Run a trace with specified base strength.
  If successful trigger specified ability"
  ([base {:keys [label] :as ability}]
   {:label (str "Trace " base " - " label)
    :trace {:base base
            :label label
            :successful ability}})
  ([base ability un-ability]
   (let [label (str (:label ability) " / " (:label un-ability))]
     {:label (str "Trace " base " - " label)
      :trace {:base base
              :label label
              :successful ability
              :unsuccessful un-ability}})))

(defn tag-trace
  "Trace ability for giving a tag, at specified base strength"
  ([base] (tag-trace base 1))
  ([base n] (trace-ability base (give-tags n))))

(defn gain-credits-sub
  "Gain specified amount of credits"
  [credits]
  {:label (str "Gain " credits " [Credits]")
   :msg (str "gain " credits " [Credits]")
   :async true
   :effect (effect (gain-credits eid credits))})

(defn power-counter-ability
  "Does specified ability using a power counter."
  [{:keys [label message] :as ability}]
  (assoc ability
         :label label
         :msg message
         :cost [(->c :power 1)]))

(defn do-psi
  "Start a psi game, if not equal do ability"
  ([{:keys [label] :as ability}]
   {:label (str "Psi Game - " label)
    :msg (str "start a psi game (" label ")")
    :psi {:not-equal ability}})
  ([{:keys [label-neq] :as neq-ability} {:keys [label-eq] :as eq-ability}]
   {:label (str "Psi Game - " label-neq " / " label-eq)
    :msg (str "start a psi game (" label-neq " / " label-eq ")")
    :psi {:not-equal neq-ability
          :equal eq-ability}}))

(def runner-loses-click
  ; Runner loses a click effect
  {:label "Force the Runner to lose [Click]"
   :msg "force the Runner to lose [Click], if able"
   :effect (effect (lose-clicks :runner 1))})

(defn runner-loses-credits
  "Runner loses credits effect"
  [credits]
  {:label (str "Runner loses " credits " [Credits]")
   :msg (str "force the Runner to lose " credits " [Credits]")
   :async true
   :effect (effect (lose-credits :runner eid credits))})

(def add-runner-card-to-grip
  "Add 1 installed Runner card to the grip"
  {:label "Add an installed Runner card to the grip"
   :req (req (not-empty (all-installed state :runner)))
   :waiting-prompt true
   :prompt "Choose a card"
   :choices {:card #(and (installed? %)
                         (runner? %))}
   :msg "add 1 installed card to the grip"
   :effect (effect (move :runner target :hand true)
                   (system-msg (str "adds " (:title target)
                                    " to the grip")))})

(def trash-program-sub
  {:prompt "Choose a program to trash"
   :label "Trash a program"
   :msg (msg "trash " (:title target))
   :choices {:card #(and (installed? %)
                         (program? %))}
   :async true
   :effect (effect (trash eid target {:cause :subroutine}))})

(def trash-hardware-sub
  {:prompt "Choose a piece of hardware to trash"
   :label "Trash a piece of hardware"
   :msg (msg "trash " (:title target))
   :choices {:card #(and (installed? %)
                         (hardware? %))}
   :async true
   :effect (effect (trash eid target {:cause :subroutine}))})

(def trash-resource-sub
  {:prompt "Choose a resource to trash"
   :label "Trash a resource"
   :msg (msg "trash " (:title target))
   :choices {:card #(and (installed? %)
                         (resource? %))}
   :async true
   :effect (effect (trash eid target {:cause :subroutine}))})

(def trash-installed-sub
  {:async true
   :prompt "Choose an installed card to trash"
   :label "Trash an installed Runner card"
   :msg (msg "trash " (:title target))
   :choices {:card #(and (installed? %)
                         (runner? %))}
   :effect (effect (trash eid target {:cause :subroutine}))})

(def runner-trash-installed-sub
  (assoc trash-installed-sub
         :player :runner
         :label "Force the Runner to trash an installed card"
         :msg (msg "force the Runner to trash " (:title target))))

(defn install-from-hq-or-archives-sub
  ([] (install-from-hq-or-archives-sub nil))
  ([args]
   {:label "Install a card from HQ or Archives"
    :prompt "Choose a card to install from Archives or HQ"
    :show-discard true
    :choices {:card #(and (corp-installable-type? %)
                          (or (in-hand? %)
                              (in-discard? %)))}
    :msg (msg (corp-install-msg target))
    :async true
    :effect (effect (corp-install eid target nil args))}))

(def cannot-steal-or-trash-sub
  {:label "The Runner cannot steal or trash Corp cards for the remainder of this run"
   :msg "prevent the Runner from stealing or trashing Corp cards for the remainder of the run"
   :effect (effect (register-run-flag!
                     card :can-steal
                     (fn [state _side _card]
                       ((constantly false)
                        (toast state :runner "Cannot steal due to subroutine." "warning"))))
                   (register-run-flag!
                     card :can-trash
                     (fn [state _side card]
                       ((constantly (not (corp? card)))
                        (toast state :runner "Cannot trash due to subroutine." "warning")))))})

;;; For Advanceable ice
(defn wall-ice
  [subroutines]
  {:advanceable :always
   :subroutines subroutines
   :static-abilities [(ice-strength-bonus (req (get-counters card :advancement)))]})

(defn space-ice
  "Creates data for Space ice with specified abilities."
  [& abilities]
  {:advanceable :always
   :subroutines (vec abilities)
   :rez-cost-bonus (req (* -3 (get-counters card :advancement)))})

;;; For Zed 1.0 and 2.0
(defn spent-click-to-break-sub
  "Checks if the runner has spent(lost) a click to break a subroutine this run"
  [state run]
  (let [all-cards (get-all-cards state)
        events (:events run)
        breaks (filter #(= :subroutines-broken (first %)) events)
        cards (vec (map #(first (second %)) breaks))
        subs (map #(:subroutines %) cards)
        broken-subs (map #(filter :broken %) subs)
        breakers (mapcat #(map :breaker %) broken-subs)
        ;; this is the list of every breaker the runner used to
        ;; break a subroutine this run. If we check that it has a
        ;; 'lose-click' break ability, we should be safe most of the
        ;; time. This can only be: adjusted matrix, ABR, and corp cards
        ;; If Adjusted Matrix ever gets correctly implemented, there will be
        ;; a minor edge case here if the runner uses a non-click break ab on a
        ;; card hosting adjusted matrix -nbkelly, 2022
        actual-breakers (map #(find-cid % all-cards) breakers)
        abs (mapcat #(if (= (:side %) "Runner") (:abilities %) (:runner-abilities %)) actual-breakers)
        costs (map :break-cost abs)
        clicks (filter #(= :lose-click (:cost/type %)) (flatten costs))]
    (not-empty clicks)))

;;; For Grail ice
(defn grail-in-hand
  "Req that specified card is a Grail card in the Corp's hand."
  [card]
  (and (corp? card)
       (in-hand? card)
       (has-subtype? card "Grail")))

(def reveal-grail
  "Ability for revealing Grail ice from HQ."
  {:label "Reveal up to 2 pieces of Grail ice from HQ"
   :choices {:max 2
             :card grail-in-hand}
   :async true
   :effect (effect (reveal eid targets))
   :msg (let [sub-label #(:label (first (:subroutines (card-def %))))]
          (msg "reveal " (enumerate-str (map #(str (:title %) " (" (sub-label %) ")") targets))))})

(def resolve-grail
  "Ability for resolving a subroutine on a Grail ice in HQ."
  {:label "Resolve a Grail ice subroutine from HQ"
   :choices {:card grail-in-hand}
   :effect (req (doseq [ice targets]
                  (let [subroutine (first (:subroutines (card-def ice)))]
                    (resolve-ability state side subroutine card nil))))})

(defn grail-ice
  "Creates data for grail ice"
  [ability]
  {:abilities [reveal-grail]
   :subroutines [ability resolve-grail]})

;;; For NEXT ice and Tour Guide
(defn variable-subs-ice
  [pred subs-count sub]
  (let [ef (effect (reset-variable-subs card (subs-count state) sub))
        rez-ability {:silent (req true)
                     :req (req (pred (:card context)))
                     :effect ef}
        trash-ability {:silent (req true)
                       :req (req (and (installed? (:card context))
                                      (rezzed? (:card context))
                                      (pred (:card context))))
                       :effect ef}]
    {:on-rez {:effect ef}
     :events [(assoc rez-ability :event :rez)
              (assoc rez-ability :event :derez)
              (assoc trash-ability :event :subroutines-should-update :req nil)
              (assoc trash-ability :event :game-trash)
              (assoc trash-ability :event :corp-trash)
              (assoc trash-ability :event :runner-trash)]}))

(defn subtype-ice-count
  "Counts number of rezzed pieces of ice with the given subtype"
  [corp subtype]
  (->> (:servers corp)
       (vals)
       (mapcat :ices)
       (filter #(and (rezzed? %) (has-subtype? % subtype)))
       (count)))

(defn next-ice-count
  "Counts number of rezzed pieces of NEXT ice - for use with NEXT Bronze and NEXT Gold"
  [corp]
  (subtype-ice-count corp "NEXT"))

(defn next-ice-variable-subs [sub]
  (variable-subs-ice
    (fn [card] (and (ice? card) (has-subtype? card "NEXT")))
    (fn [state] (next-ice-count (:corp @state)))
    sub))

;;; For Harmonic ice
(defn harmonic-ice-count
  "Counts the number of rezzed pieces of Harmonic ice - for use with Wave and others"
  [corp]
  (subtype-ice-count corp "Harmonic"))

;;; For Morph ice
(defn morph-ice
  "Creates the data for morph ice with specified types and ability."
  [base other ability]
  {:advanceable :always
   :static-abilities [{:type :lose-subtype
                       :req (req (and (same-card? card target)
                                      (odd? (get-counters (get-card state card) :advancement))))
                       :value base}
                      {:type :gain-subtype
                       :req (req (and (same-card? card target)
                                      (odd? (get-counters (get-card state card) :advancement))))
                       :value other}]
   :subroutines [ability]})

;;; For Constellation ice
(defn constellation-ice
  "Generates map for Constellation ice with specified effect."
  [ability]
  {:subroutines [(-> (trace-ability 2 ability)
                     (assoc-in [:trace :kicker] ability)
                     (assoc-in [:trace :kicker-min] 5))]})

;; For advance-only-while-rezzed, sub-growing ice
(defn zero-to-hero
  "Salvage, Tyrant, Woodcutter"
  [sub]
  (let [ability {:req (req (same-card? card (:card context)))
                 :effect (effect (reset-variable-subs card (get-counters card :advancement) sub))}]
    {:advanceable :while-rezzed
     :events [(assoc ability :event :advance)
              (assoc ability :event :subroutines-should-update :req nil)
              (assoc ability :event :advancement-placed)
              (assoc ability :event :rez)]}))

;; For 7 Wonders ice
(defn wonder-sub
  "Checks total number of advancement counters on a piece of ice against number"
  [card number]
  (<= number (get-counters card :advancement)))

(defn resolve-another-subroutine
  "For cards like Orion or Upayoga."
  ([] (resolve-another-subroutine (constantly true) "Resolve a subroutine on another ice"))
  ([pred] (resolve-another-subroutine pred "Resolve a subroutine on another ice"))
  ([pred label]
   (let [pred #(and (ice? %)
                    (rezzed? %)
                    (pred %))]
     {:async true
      :label label
      :effect
      (effect
        (continue-ability
          (when (< 1 (count (filter pred (all-active-installed state :corp))))
            {:async true
             :prompt "Choose the ice"
             :choices {:card pred
                       :all true}
             :effect (effect
                       (continue-ability
                         (let [ice target]
                           {:async true
                            :prompt "Choose the subroutine"
                            :choices (req (unbroken-subroutines-choice ice))
                            :msg (msg "resolve the subroutine (\"[subroutine] "
                                                                              target "\") from " (:title ice))
                            :effect (req (let [sub (first (filter #(= target (make-label (:sub-effect %))) (:subroutines ice)))]
                                           (continue-ability state side (:sub-effect sub) ice nil)))})
                         card nil))})
          card nil))})))

;;; Helper function for adding implementation notes to ice defined with functions
(defn- implementation-note
  "Adds an implementation note to the ice-definition"
  [note ice-def]
  (assoc ice-def :implementation note))

(def take-bad-pub
  ; Bad pub on rez effect
  {:async true
   :effect (effect (system-msg (str "takes 1 bad publicity from " (:title card)))
                   (gain-bad-publicity :corp eid 1))})

;; Card definitions

(defcard "Ablative Barrier"
  {:subroutines [end-the-run]
   :on-rez {:req (req (and (threat-level 3 state)
                           run
                           this-server))
            :prompt "Choose a card to install from Archives or HQ in another server"
            :waiting-prompt true
            :show-discard true
            :choices {:card #(and (corp? %)
                                  (corp-installable-type? %)
                                  (not (agenda? %))
                                  (or (in-hand? %)
                                      (in-discard? %)))}
            :async true
            :effect (req (let [this (zone->name (second (get-zone card)))
                               nice target]
                           (continue-ability state side
                                             {:prompt "Choose a server"
                                              :waiting-prompt true
                                              :choices (req (remove #(= this %) (corp-install-list state nice)))
                                              :async true
                                              :msg (msg (corp-install-msg nice))
                                              :effect (effect (corp-install eid nice target nil))}
                                             card nil)))}})

(defcard "Anemone"
  {:on-rez {:optional {:prompt "Trash a card from HQ to do 2 net damage?"
                       :req (req (and (< 0 (count (:hand corp)))
                                      run
                                      this-server))
                       :waiting-prompt true
                       :yes-ability {:msg "do 2 net damage"
                                     :cost [(->c :trash-from-hand 1)]
                                     :async true
                                     :effect (effect (damage eid :net 2 {:card card}))}
                       :no-ability {:effect (effect (system-msg :corp (str "declines to use " (:title card))))}}}
   :subroutines [(do-net-damage 1)]})

(defcard "Ansel 1.0"
  {:subroutines [trash-installed-sub
                 (install-from-hq-or-archives-sub)
                 cannot-steal-or-trash-sub]
   :runner-abilities [(bioroid-break 1 1)]})

(defcard "Anvil"
  (letfn [(encounter-ab []
            {:optional {:prompt "Trash another card?"
                        :waiting-prompt true
                        :req (req (can-pay? state side (assoc eid :source card :source-type :ability)
                                            card nil
                                            [(->c :trash-other-installed 1)]))
                        :yes-ability {:prompt "Select another installed card to trash"
                                      :cost [(->c :trash-other-installed 1)]
                                      :msg "prevent its printed subroutines being broken this encounter"
                                      :effect (effect (register-lingering-effect
                                                        card {:type :cannot-break-subs-on-ice
                                                              :req (req (same-card? card (:ice context)))
                                                              :value true
                                                              :duration :end-of-encounter}))}}})]
    {:on-encounter (encounter-ab)
     :subroutines[{:label "Gain 1 [Credits], Runner loses 1 [Credits]"
                   :msg "gain 1 [Credits] and force the Runner to lose 1 [Credits]"
                   :async true
                   :effect (req (wait-for (gain-credits state :corp 1)
                                          (lose-credits state :runner eid 1)))}
                  runner-trash-installed-sub]}))

(defcard "Afshar"
  (let [breakable-fn (req (if (and (= :hq (second (get-zone card)))
                                   (= (:title card) "Afshar") ;;loki protection
                                   (not (is-disabled-reg? state card)))
                            (empty? (filter #(and (:broken %) (:printed %)) (:subroutines card)))
                            :unrestricted))]
    {:subroutines [{:msg "make the Runner lose 2 [Credits]"
                    :breakable breakable-fn
                    :async true
                    :effect (effect (lose-credits :runner eid 2))}
                   (assoc end-the-run :breakable breakable-fn)]}))

(defcard "Aiki"
  {:subroutines [(do-psi {:label "Runner draws 2 cards"
                          :msg "make the Runner draw 2 cards"
                          :async true
                          :effect (effect (draw :runner eid 2))})
                 (do-net-damage 1)
                 (do-net-damage 1)]})

(defcard "Aimor"
  {:subroutines [{:async true
                  :label "Trash the top 3 cards of the stack"
                  :effect (req (system-msg
                                 state :corp
                                 (str "uses " (:title card) " to trash "
                                      (enumerate-str (map :title (take 3 (:deck runner))))
                                      " from the top of the stack and trash itself"))
                               (wait-for
                                 (mill state :corp (make-eid state eid) :runner 3)
                                 (wait-for (trash state :corp (make-eid state eid) card {:cause :subroutine})
                                           (encounter-ends state side eid))))}]})

(defcard "Akhet"
  (let [breakable-fn (req (if (and (<= 3 (get-counters card :advancement))
                                   (= (:title card) "Akhet") ;; loki protection
                                   (not (is-disabled-reg? state card)))
                            (empty? (filter #(and (:broken %) (:printed %)) (:subroutines card)))
                            :unrestricted))]
    {:advanceable :always
     :subroutines [{:label "Gain 1 [Credit]. Place 1 advancement token"
                    :breakable breakable-fn
                    :msg (msg "gain 1 [Credit] and place 1 advancement token on " (card-str state target))
                    :prompt "Choose an installed card"
                    :choices {:card installed?}
                    :async true
                    :effect (effect (add-prop target :advance-counter 1 {:placed true})
                                    (gain-credits eid 1))}
                   (assoc end-the-run :breakable breakable-fn)]
     :static-abilities [(ice-strength-bonus
                          (req (<= 3 (get-counters card :advancement)))
                          3)]}))

(defcard "Anansi"
  (let [corp-draw {:optional
                   {:waiting-prompt true
                    :prompt "Draw 1 card?"
                    :yes-ability {:async true
                                  :msg "draw 1 card"
                                  :effect (effect (draw eid 1))}}}
        runner-draw {:player :runner
                     :optional
                     {:waiting-prompt true
                      :prompt "Pay 2 [Credits] to draw 1 card?"
                      :yes-ability
                      {:async true
                       :effect (req (wait-for (pay state :runner (make-eid state eid) card [(->c :credit 2)])
                                              (if (:cost-paid async-result)
                                                (do (system-msg state :runner "pays 2 [Credits] to draw 1 card")
                                                    (draw state :runner eid 1))
                                                (do (system-msg state :runner "does not draw 1 card")
                                                    (effect-completed state side eid)))))}
                      :no-ability {:effect (effect (system-msg :runner "does not draw 1 card"))}}}]
    {:subroutines [{:msg "rearrange the top 5 cards of R&D"
                    :async true
                    :waiting-prompt true
                    :effect (req (let [from (take 5 (:deck corp))]
                                   (continue-ability
                                     state side
                                     (when (pos? (count from))
                                       (reorder-choice :corp :runner from '() (count from) from))
                                     card nil)))}
                   {:label "Draw 1 card, runner draws 1 card"
                    :async true
                    :effect (req (wait-for (resolve-ability state side corp-draw card nil)
                                           (continue-ability state :runner runner-draw card nil)))}
                   (do-net-damage 1)]
     :events [(assoc (do-net-damage 3)
                     :event :end-of-encounter
                     :req (req (and (= (:ice context) card)
                                    (seq (remove :broken (:subroutines (:ice context)))))))]}))

(defcard "Archangel"
  {:flags {:rd-reveal (req true)}
   :on-access
   {:optional
    {:req (req (not (in-discard? card)))
     :waiting-prompt true
     :prompt (msg "Pay 3 [Credits] to force Runner to encounter " (:title card) "?")
     :player :corp
     :yes-ability
     {:cost [(->c :credit 3)]
      :async true
      :msg "force the Runner to encounter it"
      :effect (req (force-ice-encounter state side eid card))}
     :no-ability {:effect (effect (system-msg :corp (str "declines to use " (:title card))))}}}
   :subroutines [(trace-ability 6 add-runner-card-to-grip)]})

(defcard "Archer"
  {:additional-cost [(->c :forfeit)]
   :subroutines [(gain-credits-sub 2)
                 trash-program-sub
                 trash-program-sub
                 end-the-run]})

(defcard "Architect"
  {:flags {:untrashable-while-rezzed true}
   :subroutines [{:async true
                  :label "Look at the top 5 cards of R&D"
                  :msg "look at the top 5 cards of R&D"
                  :prompt (msg "The top cards of R&D are (top->bottom) " (enumerate-str (map :title (take 5 (:deck corp)))))
                  :choices ["OK"]
                  :req (req (not-empty (:deck corp)))
                  :effect
                  (effect (continue-ability
                            {:prompt "Choose a card to install"
                             :choices (cancellable (filter corp-installable-type? (take 5 (:deck corp))))
                             :async true
                             :effect (effect (corp-install eid target nil {:ignore-all-cost true}))
                             :cancel-effect (effect (system-msg "does not install any of the top 5 cards")
                                                    (effect-completed eid))}
                            card nil))}
                 (install-from-hq-or-archives-sub)]})

(defcard "Ashigaru"
  {:on-rez {:effect (effect (reset-variable-subs card (count (:hand corp)) end-the-run))}
   :events [{:event :card-moved
             :req (req (and (corp? (:moved-card context))
                            (or (in-hand? (:moved-card context))
                                (= :hand (first (:previous-zone (:moved-card context)))))))
             :effect (effect (reset-variable-subs card (count (:hand corp)) end-the-run))}]})

(defcard "Assassin"
  {:subroutines [(trace-ability 5 (do-net-damage 3))
                 (trace-ability 4 trash-program-sub)]})

(defcard "Asteroid Belt"
  (space-ice end-the-run))

(defcard "Attini"
  ;; TODO - this should provide an aura that prevents the runner from paying credits
  ;; for paid abilities during subroutine resolution
  ;; we can figure out how to do that type of thing some time in the future
  (let [sub {:label "Do 1 net damage unless the Runner pays 2 [Credits]"
             :async true
             :effect (req (if (and (threat-level 3 state)
                                   (not (is-disabled-reg? state card)))
                            (damage state side eid :net 1 {:card card})
                            (continue-ability
                              state side
                              {:prompt "Choose one"
                               :waiting-prompt true
                               :player :runner
                               :async true
                               :choices (req ["Take 1 net damage"
                                              (when (can-pay? state :runner (assoc eid :source card :source-type :ability) card nil [(->c :credit 2)])
                                                "Pay 2 [Credits]")])
                               :effect
                               (req (if (= "Take 1 net damage" target)
                                      (damage state side eid :net 1 {:card card})
                                      (pay state :runner eid card (->c :credit 2))))
                               :msg (msg (if (= "Take 1 net damage" target)
                                           "do 1 net damage"
                                           (str "force the runner to " (decapitalize target))))}
                              card nil)))}]
    {:implementation "preventing paid abilities with credit costs not implemented (yet)"
     :subroutines [sub
                   sub
                   sub]}))

(defcard "Authenticator"
  {:on-encounter {:optional
                  {:req (req (and (not (:bypass run))
                                  (not (forced-to-avoid-tags? state side))))
                   :player :runner
                   :prompt "Take 1 tag to bypass Authenticator?"
                   :yes-ability
                   {:async true
                    :effect (req (system-msg state :runner "takes 1 tag on encountering Authenticator to bypass it")
                                 (bypass-ice state)
                                 (gain-tags state :runner eid 1 {:unpreventable true}))}}}
   :subroutines [(gain-credits-sub 2)
                 end-the-run]})

(defcard "Bailiff"
  (letfn [(bailiff-gain-credits [state side eid n]
            (if (pos? n)
              (wait-for (gain-credits state :corp (make-eid state eid) 1)
                        (bailiff-gain-credits state side eid (dec n)))
              (effect-completed state side eid)))]
    {:on-break-subs {:msg (msg (let [n-subs (count (second targets))]
                                 (str "gain " n-subs " [Credits] from the runner breaking subs")))
                     :async true
                     :effect (effect (bailiff-gain-credits eid (count (second targets))))}
     :subroutines [end-the-run]}))

(defcard "Ballista"
  {:subroutines [{:label "Trash 1 program or end the run"
                  :prompt "Choose one"
                  :waiting-prompt true
                  :choices (req [(when (not-empty (filter program? (all-active-installed state :runner)))
                                   "Trash a program")
                                 "End the run"])
                  :async true
                  :effect (effect
                            (continue-ability
                              (if (= target "Trash a program")
                                trash-program-sub
                                end-the-run)
                              card nil))}]})

(defcard "Bandwidth"
  {:subroutines [{:msg "give the Runner 1 tag"
                  :async true
                  :effect (req (wait-for (gain-tags state :corp 1)
                                         (register-events
                                           state side card
                                           [{:event :successful-run
                                             :duration :end-of-run
                                             :unregister-once-resolved true
                                             :async true
                                             :msg "make the Runner lose 1 tag"
                                             :effect (effect (lose-tags :corp eid 1))}])
                                         (effect-completed state side eid)))}]})

(defcard "Bastion"
  {:subroutines [end-the-run]})

(defcard "Bathynomus"
  {:subroutines [(do-net-damage 3)]
   :static-abilities [(ice-strength-bonus (req (protecting-archives? card)) 3)]})

(defcard "Battlement"
  {:subroutines [end-the-run
                 end-the-run]})

(defcard "Blockchain"
  (let [sub-count (fn [corp]
                    (quot (count (filter #(and (operation? %)
                                               (has-subtype? % "Transaction")
                                               (faceup? %))
                                         (:discard corp)))
                          2))
        sub {:label "Gain 1 [Credits], Runner loses 1 [Credits]"
             :msg "gain 1 [Credits] and force the Runner to lose 1 [Credits]"
             :async true
             :effect (req (wait-for (gain-credits state :corp 1)
                                    (lose-credits state :runner eid 1)))}
        reset-subs-abi {:effect (effect (reset-variable-subs card (sub-count corp)
                                                             sub
                                                             {:variable true :front true}))}]
    {:on-rez reset-subs-abi
     :events [(merge {:event :card-moved
                      :req (req (and
                                  (operation? (:moved-card context))
                                  (has-subtype? (:moved-card context) "Transaction")
                                  (or (in-discard? (:moved-card context))
                                      (= :discard (first (:previous-zone (:moved-card context)))))))}
                     reset-subs-abi)
              (merge {:event :subroutines-should-update
                      :silent (req true)}
                     reset-subs-abi)]
     :subroutines [sub
                   end-the-run]}))

(defcard "Bloodletter"
  {:subroutines [{:async true
                  :label "Runner trashes 1 program or top 2 cards of the stack"
                  :effect (req (if (empty? (filter program? (all-active-installed state :runner)))
                                 (do (system-msg state :runner "trashes the top 2 cards of the stack")
                                     (mill state :runner eid :runner 2))
                                 (continue-ability
                                   state :runner
                                   {:waiting-prompt true
                                    :prompt "Choose one"
                                    :choices (req [(when (seq (filter program? (all-active-installed state :runner)))
                                                     "Trash 1 program")
                                                   (when (<= 1 (count (:deck runner)))
                                                     "Trash the top 2 cards of the stack")])
                                    :async true
                                    :effect (req (if (= target "Trash 1 program")
                                                   (continue-ability state :runner trash-program-sub card nil)
                                                   (do (system-msg state :runner "trashes the top 2 cards of the stack")
                                                       (mill state :runner eid :runner 2))))}
                                   card nil)))}]})

(defcard "Bloom"
  {:subroutines
   [{:label "Install a piece of ice from HQ protecting another server, ignoring all costs"
     :prompt "Choose a piece of ice to install from HQ in another server"
     :async true
     :choices {:card #(and (ice? %)
                           (in-hand? %))}
     :effect (req (let [this (zone->name (second (get-zone card)))
                        nice target]
                    (continue-ability state side
                                      {:prompt (str "Choose a location to install " (:title target))
                                       :choices (req (remove #(= this %) (corp-install-list state nice)))
                                       :async true
                                       :effect (effect (corp-install eid nice target {:ignore-all-cost true}))}
                                      card nil)))}
    {:label "Install a piece of ice from HQ in the next innermost position, protecting this server, ignoring all costs"
     :prompt "Choose a piece of ice to install from HQ in this server"
     :async true
     :choices {:card #(and (ice? %)
                           (in-hand? %))}
     :effect (req (corp-install state side eid
                                target (zone->name (target-server run))
                                {:ignore-all-cost true
                                 :index (max (dec run-position) 0)}))}]})

(defcard "Bloop"
  {:additional-cost [(->c :derez-other-harmonic)]
   :subroutines [(do-brain-damage 1)
                 trash-program-sub
                 trash-program-sub]})

(defcard "Border Control"
  {:abilities [{:label "End the run"
                :msg "end the run"
                :async true
                :cost [(->c :trash-can)]
                :effect (effect (end-run eid card))}]
   :subroutines [{:label "Gain 1 [Credits] for each ice protecting this server"
                  :msg (msg "gain "
                            (count (:ices (card->server state card)))
                            " [Credits]")
                  :async true
                  :effect (req (let [num-ice (count (:ices (card->server state card)))]
                                 (gain-credits state :corp eid num-ice)))}
                 end-the-run]})

(defcard "Boto"
  (let [discard-card-to-end-the-run-sub
        {:label "Trash 1 card from HQ to end the run"
         :optional {:prompt "Trash 1 card from HQ to end the run?"
                    :yes-ability {:cost [(->c :trash-from-hand 1)]
                                  :msg "end the run"
                                  :async true
                                  :effect (effect (end-run eid card))}}}]
    {:static-abilities [(ice-strength-bonus (req (if (threat-level 4 state) 2 0)))]
    :subroutines [(do-net-damage 2)
                  discard-card-to-end-the-run-sub
                  discard-card-to-end-the-run-sub]}))

(defcard "Brainstorm"
  {:on-encounter {:effect (effect (gain-variable-subs card (count (:hand runner)) (do-brain-damage 1)))}
   :events [{:event :run-ends
             :effect (effect (reset-variable-subs card 0 nil))}]})

(defcard "Builder"
  (let [sub {:label "Place 1 advancement token on a piece of ice that can be advanced protecting this server"
             :msg (msg "place 1 advancement token on " (card-str state target))
             :choices {:req (req (and (ice? target)
                                      (can-be-advanced? state target)))}
             :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]
    {:abilities [{:label "Move this ice to the outermost position of any server"
                  :cost [(->c :click 1)]
                  :prompt "Choose a server"
                  :choices (req servers)
                  :msg (msg "move itself to the outermost position of " target)
                  :effect (effect (move card (conj (server->zone state target) :ices)))}]
     :subroutines [sub
                   sub]}))

(defcard "Brân 1.0"
  {:subroutines [{:async true
                  :label "Install an ice from HQ or Archives"
                  :prompt "Choose an ice to install from Archives or HQ"
                  :show-discard true
                  :choices {:card #(and (ice? %)
                                        (or (in-hand? %)
                                            (in-discard? %)))}
                  :msg (msg (corp-install-msg target))
                  :effect (req (wait-for (corp-install state :corp target
                                                       (zone->name (second (get-zone card)))
                                                       {:ignore-install-cost true
                                                        :index (:index card)})
                                         (effect-completed state side eid)))
                  :cancel-effect (effect (system-msg :corp (str "declines to use " (:title card) " to install a card"))
                                         (effect-completed eid))}
                 end-the-run
                 end-the-run]
   :runner-abilities [(bioroid-break 1 1)]})

(defcard "Bullfrog"
  {:subroutines [(do-psi {:label "Move this ice to another server"
                          :player :corp
                          :prompt "Choose a server"
                          :choices (req servers)
                          :msg (msg "move itself to the outermost position of " target)
                          :effect (effect (move card (conj (server->zone state target) :ices))
                                          (redirect-run target)
                                          (effect-completed eid))})]})

(defcard "Bulwark"
  (let [sub {:msg "gain 2 [Credits] and end the run"
             :async true
             :effect (req (wait-for (gain-credits state side 2)
                                    (end-run state side eid card)))}]
    {:on-rez take-bad-pub
     :on-encounter {:req (req (some #(has-subtype? % "AI") (all-active-installed state :runner)))
                    :msg "gain 2 [Credits] if there is an installed AI"
                    :async true
                    :effect (effect (gain-credits eid 2))}
     :subroutines [(assoc trash-program-sub
                          :player :runner
                          :msg "force the Runner to trash 1 program"
                          :label "The Runner trashes 1 program")
                   sub
                   sub]}))

(defcard "Burke Bugs"
  {:subroutines [(trace-ability 0 (assoc trash-program-sub
                                         :not-distinct true
                                         :player :runner
                                         :msg "force the Runner to trash a program"
                                         :label "Force the Runner to trash a program"))]})

(defcard "Caduceus"
  {:subroutines [(trace-ability 3 (gain-credits-sub 3))
                 (trace-ability 2 end-the-run)]})

(defcard "Capacitor"
  {:static-abilities [(ice-strength-bonus (req (if (is-tagged? state) 2 0)))]
   :subroutines [{:label "Gain 1 [Credits] for each tag the Runner has"
                  :async true
                  :msg (msg "gain " (count-tags state) " [Credits]")
                  :req (req (pos? (count-tags state)))
                  :effect (effect (gain-credits :corp eid (count-tags state)))}
                 end-the-run]})

(defcard "Cell Portal"
  {:subroutines [{:async true
                  :msg "make the Runner approach the outermost piece of ice"
                  :effect (req (let [server (zone->name (target-server run))]
                                 (redirect-run state side server :approach-ice)
                                 (wait-for (resolve-ability state :runner
                                                            (make-eid state eid)
                                                            (offer-jack-out) card nil)
                                           (derez state side card)
                                           (encounter-ends state side eid))))}]})

(defcard "Changeling"
  (morph-ice "Barrier" "Sentry" end-the-run))

(defcard "Checkpoint"
  {:on-rez take-bad-pub
   :subroutines [(trace-ability 5 {:label "Do 3 meat damage when this run is successful"
                                   :msg "do 3 meat damage when this run is successful"
                                   :effect (effect (register-events
                                                     card
                                                     [{:event :successful-run
                                                       :duration :end-of-run
                                                       :async true
                                                       :msg "do 3 meat damage"
                                                       :effect (effect (damage eid :meat 3 {:card card}))}]))})]})

(defcard "Chetana"
  {:subroutines [{:msg "make each player gain 2 [Credits]"
                  :async true
                  :effect (req (wait-for (gain-credits state :runner 2)
                                         (gain-credits state :corp eid 2)))}
                 (do-psi {:label "Do 1 net damage for each card in the grip"
                          :msg (msg "do " (count (get-in @state [:runner :hand])) " net damage")
                          :effect (effect (damage eid :net (count (get-in @state [:runner :hand])) {:card card}))})]})

(defcard "Chimera"
  {:on-rez {:prompt "Choose one subtype"
            :choices ["Barrier" "Code Gate" "Sentry"]
            :msg (msg "make itself gain " target)
            :effect (effect (update! (assoc card :subtype-target target)))}
   :static-abilities [{:type :gain-subtype
                       :req (req (and (same-card? card target) (:subtype-target card)))
                       :value (req (:subtype-target card))}]
   :events [{:event :runner-turn-ends
             :req (req (rezzed? card))
             :effect (effect (derez :corp card))}
            {:event :corp-turn-ends
             :req (req (rezzed? card))
             :effect (effect (derez :corp card))}]
   :subroutines [end-the-run]})

(defcard "Chiyashi"
  (letfn [(chiyashi-auto-trash [state side eid n]
            (if (pos? n)
              (wait-for (mill state :corp :runner 2)
                        (system-msg state side "uses Chiyashi to trash the top 2 cards of the Stack")
                        (chiyashi-auto-trash state side eid (dec n)))
              (effect-completed state side eid)))]
    {:events [{:event :subroutines-broken
               :req (req (and (same-card? card (first targets))
                              (seq (filter #(has-subtype? % "AI") (all-active-installed state :runner)))))
               :async true
               :effect (effect (chiyashi-auto-trash :corp eid (count (second targets))))}]
     :subroutines [(do-net-damage 2)
                   (do-net-damage 2)
                   end-the-run]}))

(defcard "Chrysalis"
  {:flags {:rd-reveal (req true)}
   :subroutines [(do-net-damage 2)]
   :on-access {:async true
               :req (req (not (in-discard? card)))
               :msg "force the Runner to encounter Chrysalis"
               :effect (req (force-ice-encounter state side eid card))}})

(defcard "Chum"
  {:subroutines
   [{:label "Give +2 strength to next piece of ice Runner encounters"
     :req (req this-server)
     :msg (msg "give +2 strength to the next piece of ice the Runner encounters")
     :effect
     (effect (register-events
               card
               [{:event :encounter-ice
                 :duration :end-of-run
                 :unregister-once-resolved true
                 :effect
                 (req (let [target-ice (:ice context)]
                        (register-lingering-effect
                          state side card
                          {:type :ice-strength
                           :duration :end-of-encounter
                           :value 2
                           :req (req (same-card? target target-ice))})
                        (register-events
                          state side card
                          [(assoc (do-net-damage 3)
                                  :event :end-of-encounter
                                  :duration :end-of-run
                                  :unregister-once-resolved true
                                  :req (req (and (same-card? (:ice context) target-ice)
                                                 (seq (remove :broken (:subroutines (:ice context)))))))])))}]))}]})

(defcard "Clairvoyant Monitor"
  {:subroutines [(do-psi {:label "Place 1 advancement token and end the run"
                          :player :corp
                          :prompt "Choose an installed card to place 1 advancement token on"
                          :msg (msg "place 1 advancement token on "
                                    (card-str state target) " and end the run")
                          :choices {:card installed?}
                          :effect (effect (add-prop target :advance-counter 1 {:placed true})
                                          (end-run eid card))})]})

(defcard "Cloud Eater"
  {:subroutines [trash-installed-sub
                 (give-tags 2)
                 (do-net-damage 3)]
   :events [{:event :end-of-encounter
             :req (req (and (= :this-turn (:rezzed card))
                            (same-card? (:ice context) card)))
             :effect (effect (continue-ability
                               {:prompt "Choose one"
                                :player :runner
                                :choices (req ["Corp trashes 1 Runner card"
                                               (when-not (forced-to-avoid-tags? state side) "Take 2 tags")
                                               (when (can-pay? state :runner eid card nil (->c :net 3))
                                                 "Suffer 3 net damage")])
                                :async true
                                :effect (req
                                          (continue-ability
                                            state (if (= target "Corp trashes 1 Runner card") :corp :runner)
                                            (cond
                                              (= target "Corp trashes 1 Runner card")
                                              trash-installed-sub
                                              (= target "Take 2 tags")
                                              {:msg (msg "force the Runner to " (decapitalize target))
                                               :effect (effect (gain-tags :runner eid 2 {:unpreventable true}))}
                                              (= target "Suffer 3 net damage")
                                              {:msg (msg "force the Runner to " (decapitalize target))
                                               :effect (req (pay state :runner eid card [(->c :net 3)]))})
                                            card nil))}
                               card nil))}]})

(defcard "Cobra"
  {:subroutines [trash-program-sub (do-net-damage 2)]})

(defcard "Colossus"
  (wall-ice [{:label "Give the Runner 1 tag (Give the Runner 2 tags)"
              :async true
              :msg (msg "give the Runner " (if (wonder-sub card 3) "2 tags" "1 tag"))
              :effect (effect (gain-tags :corp eid (if (wonder-sub card 3) 2 1)))}
             {:label "Trash 1 program (Trash 1 program and 1 resource)"
              :async true
              :msg (msg "trash 1 program" (when (wonder-sub card 3) " and 1 resource"))
              :effect (req (wait-for (resolve-ability state side trash-program-sub card nil)
                                     (if (wonder-sub card 3)
                                       (continue-ability
                                         state side
                                         {:prompt "Choose a resource to trash"
                                          :msg (msg "trash " (:title target))
                                          :choices {:card #(and (installed? %)
                                                                (resource? %))}
                                          :cancel-effect (req (effect-completed state side eid))
                                          :async true
                                          :effect (effect (trash eid target {:cause :subroutine}))}
                                         card nil)
                                       (effect-completed state side eid))))}]))

(defcard "Congratulations!"
  {:events [{:event :pass-ice
             :req (req (same-card? (:ice context) card))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits :corp eid 1))}]
   :subroutines [{:label "Gain 2 [Credits]. The Runner gains 1 [Credits]"
                  :msg "gain 2 [Credits]. The Runner gains 1 [Credits]"
                  :async true
                  :effect (req (wait-for (gain-credits state :corp 2)
                                         (gain-credits state :runner eid 1)))}]})

(defcard "Conundrum"
  {:subroutines [(assoc trash-program-sub
                        :player :runner
                        :msg "force the Runner to trash 1 program"
                        :label "The Runner trashes 1 program")
                 runner-loses-click
                 end-the-run]
   :static-abilities [(ice-strength-bonus
                        (req (some #(has-subtype? % "AI") (all-active-installed state :runner)))
                        3)]})

(defcard "Cortex Lock"
  {:subroutines [{:label "Do 1 net damage for each unused memory unit the Runner has"
                  :msg (msg "do " (available-mu state) " net damage")
                  :effect (effect (damage eid :net (available-mu state) {:card card}))}]})

(defcard "Crick"
  {:subroutines [{:label "install a card from Archives"
                  :prompt "Choose a card to install from Archives"
                  :show-discard true
                  :async true
                  :choices {:card #(and (corp-installable-type? %)
                                        (in-discard? %))}
                  :msg (msg (corp-install-msg target))
                  :effect (effect (corp-install eid target nil nil))}]
   :static-abilities [(ice-strength-bonus (req (protecting-archives? card)) 3)]})

(defcard "Curtain Wall"
  {:subroutines [end-the-run
                 end-the-run
                 end-the-run]
   :static-abilities [(ice-strength-bonus
                        (req (let [ices (:ices (card->server state card))]
                               (same-card? card (last ices))))
                        4)]
   :events [{:event :trash
             :req (req (and (not (same-card? card target))
                            (= (card->server state card) (card->server state target))))
             :effect (effect (update-ice-strength card))}
            {:event :corp-install
             :req (req (and (not (same-card? card (:card context)))
                            (= (card->server state card) (card->server state (:card context)))))
             :effect (effect (update-ice-strength card))}]})

(defcard "Data Hound"
  (letfn [(dh-trash [cards]
            {:prompt "Choose a card to trash"
             :choices cards
             :async true
             :msg (msg "trash " (:title target))
             :effect (req (wait-for (trash state side target {:unpreventable true
                                                              :cause :subroutine})
                                    (continue-ability
                                      state side
                                      (reorder-choice
                                        :runner :runner (remove-once #(= % target) cards)
                                        '() (count (remove-once #(= % target) cards))
                                        (remove-once #(= % target) cards))
                                      card nil)))})]
    {:subroutines [(trace-ability
                     2
                     {:async true
                      :label "Look at the top cards of the stack"
                      :msg "look at top cards of the stack"
                      :waiting-prompt true
                      :effect (req (let [c (- target (second targets))
                                         from (take c (:deck runner))]
                                     (system-msg state :corp
                                                 (str "looks at the top " (quantify c "card") " of the stack"))
                                     (if (< 1 c)
                                       (continue-ability state side (dh-trash from) card nil)
                                       (wait-for (trash state side (first from) {:unpreventable true
                                                                                 :cause :subroutine})
                                                 (system-msg state :corp (str "trashes " (:title (first from))))
                                                 (effect-completed state side eid)))))})]}))

(defcard "Data Loop"
  {:on-encounter {:req (req (pos? (count (:hand runner))))
                  :async true
                  :effect (effect
                            (continue-ability
                              :runner
                              (let [n (min 2 (count (:hand runner)))]
                                {:prompt (str "Choose " (quantify n "card") " in the grip to add to the top of the stack (second card targeted will be topmost)")
                                 :choices {:max n
                                           :all true
                                           :card #(and (in-hand? %)
                                                       (runner? %))}
                                 :msg (msg "add " (quantify n "card") " from the grip to the top of the stack")
                                 :effect (req (doseq [c targets]
                                                (move state :runner c :deck {:front true})))})
                              card nil))}
   :subroutines [end-the-run-if-tagged
                 end-the-run]})

(defcard "Data Mine"
  {:subroutines [{:msg "do 1 net damage and trash itself"
                  :async true
                  :effect (req (wait-for (damage state :runner :net 1 {:card card})
                                         (trash state :corp (make-eid state eid) card {:cause :subroutine})
                                         (encounter-ends state side eid)))}]})

(defcard "Data Raven"
  {:abilities [(power-counter-ability (give-tags 1))]
   :on-encounter {:msg (msg (if (= target "End the run")
                              (decapitalize target)
                              (str "force the runner to " (decapitalize target) " on encountering it")))
                  :player :runner
                  :prompt "Choose one"
                  :waiting-prompt true
                  :choices ["Take 1 tag" "End the run"]
                  :async true
                  :effect (req (if (= target "Take 1 tag")
                                 (gain-tags state :runner eid 1)
                                 (end-run state :runner eid card)))}
   :subroutines [(trace-ability 3 add-power-counter)]})

(defcard "Data Ward"
  {:on-encounter {:player :runner
                  :msg (msg "force the runner to " (decapitalize target) " on encountering it")
                  :prompt "Choose one"
                  :waiting-prompt true
                  :choices (req [(when (can-pay? state :runner eid card nil (->c :credit 3))  
                                   "Pay 3 [Credits]")
                                 "Take 1 tag"])
                  :async true
                  :effect (req (if (= target "Pay 3 [Credits]")
                                 (wait-for (pay state :runner (make-eid state eid) card (->c :credit 3))
                                           (system-msg state :runner (:msg async-result))
                                           (effect-completed state side eid))
                                 (gain-tags state :runner eid 1)))}
   :subroutines [end-the-run-if-tagged
                 end-the-run-if-tagged
                 end-the-run-if-tagged
                 end-the-run-if-tagged]})

(defcard "Datapike"
  {:subroutines [{:async true
                  :effect (req (wait-for (pay state :runner (make-eid state eid) card (->c :credit 2))
                                         (if (:cost-paid async-result)
                                           (do (system-msg state :runner (:msg async-result))
                                               (effect-completed state side eid))
                                           (end-run state :corp eid card))))}
                 end-the-run]})

(defcard "Diviner"
  {:subroutines
   [{:label "Do 1 net damage"
     :async true
     :msg "do 1 net damage"
     :effect (req (wait-for
                    (damage state :corp (make-eid state eid) :net 1 {:card card})
                    (let [[trashed-card] async-result]
                      (cond
                        (nil? trashed-card) (effect-completed state side eid)
                        (odd? (:cost trashed-card))
                        (do (system-msg state :corp (str "uses " (:title card) " to end the run"))
                            (end-run state :corp eid card))
                        :else (effect-completed state side eid)))))}]})

(defcard "DNA Tracker"
  (let [sub {:msg "do 1 net damage and make the Runner lose 2 [Credits]"
             :async true
             :effect (req (wait-for (damage state side :net 1 {:card card})
                                    (lose-credits state :runner eid 2)))}]
    {:subroutines [sub
                   sub
                   sub]}))

(defcard "Dracō"
  {:on-rez {:prompt "How many power counters do you want to place?"
            :choices :credit
            :msg (msg "place " (quantify target "power counter"))
            :effect (effect (add-counter card :power target)
                            (update-ice-strength card))}
   :static-abilities [(ice-strength-bonus (req (get-counters card :power)))]
   :subroutines [(trace-ability 2 {:label "Give the Runner 1 tag and end the run"
                                   :msg "give the Runner 1 tag and end the run"
                                   :async true
                                   :effect (req (wait-for (gain-tags state :corp 1)
                                                          (end-run state :corp eid card)))})]})

(defcard "Drafter"
  {:subroutines [(corp-recur)
                 (install-from-hq-or-archives-sub {:ignore-all-cost true})]})

(defcard "Echo"
  {:events [{:event :rez
             :req (req (and (has-subtype? (:card context) "Harmonic")
                            (ice? (:card context))))
             :effect (effect (add-counter card :power 1))}
            {:event :subroutines-should-update
             :silent (req true)
             :effect (effect (reset-variable-subs card (get-counters card :power) end-the-run {:variable true :front true}))}
            {:event :counter-added
             :req (req (same-card? card target))
             :effect (effect (reset-variable-subs card (get-counters card :power) end-the-run {:variable true :front true}))}]})

(defcard "Eli 1.0"
  {:subroutines [end-the-run
                 end-the-run]
   :runner-abilities [(bioroid-break 1 1)]})

(defcard "Eli 2.0"
  {:subroutines [{:label "Draw 1 card"
                  :optional
                  {:prompt "Draw 1 card?"
                   :autoresolve (get-autoresolve :auto-fire)
                   :yes-ability {:async true
                                 :msg "draw 1 card"
                                 :effect (effect (draw eid 1))}}}
                 end-the-run
                 end-the-run]
   :abilities [(set-autoresolve :auto-fire "Eli 2.0 drawing cards")]
   :runner-abilities [(bioroid-break 2 2)]})

(defcard "Endless EULA"
  (let [sub (end-the-run-unless-runner-pays (->c :credit 1))]
    (letfn [(break-fn [unbroken-subs total]
              {:async true
               :effect
               (req (if (seq unbroken-subs)
                      (wait-for (pay state :runner (make-eid state (assoc eid :source-type :subroutine)) card [(->c :credit 1)])
                                (system-msg state :runner (:msg async-result))
                                (continue-ability
                                  state side
                                  (break-fn (rest unbroken-subs) (inc total))
                                  card nil))
                      (let [msgs (when (pos? total)
                                   (str "resolves " (quantify total "unbroken subroutine")
                                        " on Endless EULA"
                                        " (\"[subroutine] "
                                                          (:label sub) "\")"))]
                        (when (pos? total)
                          (system-msg state side msgs))
                        (effect-completed state side eid))))})]
      {:subroutines [sub sub
                     sub sub
                     sub sub]
       :runner-abilities [{:req (req (<= (count (remove #(or (:broken %) (= false (:resolve %))) (:subroutines card)))
                                         (total-available-credits state :runner eid card)))
                           :async true
                           :label "Pay for all unbroken subs"
                           :effect (req (let [unbroken-subs (remove #(or (:broken %) (= false (:resolve %))) (:subroutines card))
                                              eid (assoc eid :source-type :subroutine)]
                                          (->> unbroken-subs
                                               (reduce resolve-subroutine card)
                                               (update! state side))
                                          (continue-ability
                                            state side
                                            (break-fn unbroken-subs 0)
                                            card nil)))}]})))

(defcard "Enforcer 1.0"
  {:additional-cost [(->c :forfeit)]
   :subroutines [trash-program-sub
                 (do-brain-damage 1)
                 {:label "Trash a console"
                  :prompt "Choose a console to trash"
                  :choices {:card #(has-subtype? % "Console")}
                  :msg (msg "trash " (:title target))
                  :async true
                  :effect (effect (trash eid target {:cause :subroutine}))}
                 {:msg "trash all virtual resources"
                  :async true
                  :effect (req (let [cards (filter #(has-subtype? % "Virtual") (all-active-installed state :runner))]
                                 (trash-cards state side eid cards {:cause :subroutine})))}]
   :runner-abilities [(bioroid-break 1 1)]})

(defcard "Engram Flush"
  (let [sub {:async true
             :label "Reveal the grip"
             :msg (msg "reveal " (enumerate-str (map :title (:hand runner))) " from the grip")
             :effect (effect (reveal eid (:hand runner)))}]
    {:on-encounter {:prompt "Choose a card type"
                    :choices ["Event" "Hardware" "Program" "Resource"]
                    :msg (msg "name " target)
                    :effect (req (let [cardtype target]
                                   (register-events
                                     state side card
                                     [{:event :corp-reveal
                                       :duration :end-of-encounter
                                       :async true
                                       :req (req (and
                                                   ; all revealed cards are in grip
                                                   (every? in-hand? targets)
                                                   ; entire grip was revealed
                                                   (= (count targets) (count (:hand runner)))
                                                   ; there are cards with the named card type
                                                   (some #(is-type? % cardtype) targets)))
                                       :prompt "Choose revealed card to trash"
                                       :choices (req (concat (filter #(is-type? % cardtype) targets) ["Done"]))
                                       :not-distinct true
                                       :msg (msg "trash " (:title target) " from the grip")
                                       :effect (req (if (= "Done" target)
                                                      (effect-completed state side eid)
                                                      (trash state side eid target {:cause :subroutine})))}])))}
     :subroutines [sub
                   sub]}))

(defcard "Enigma"
  {:subroutines [runner-loses-click
                 end-the-run]})

(defcard "Envelopment"
  (let [subs-effect (effect (reset-variable-subs card (get-counters card :power) end-the-run {:variable true :front true}))]
    {:on-rez {:effect (effect (add-counter card :power 4))}
     :events [{:event :corp-turn-begins
               :req (req (pos? (get-counters card :power)))
               :effect (effect (add-counter card :power -1))}
              {:event :subroutines-should-update
               :silent (req true)
               :effect subs-effect}
              {:event :counter-added
               :req (req (same-card? card target))
               :effect subs-effect}]
     :subroutines [{:label "Trash this ice"
                    :async true
                    :msg (msg "trash " (:title card))
                    :effect (effect (trash eid card {:cause :subroutine}))}]}))

(defcard "Envelope"
  {:subroutines [(do-net-damage 1)
                 end-the-run]})

(defcard "Errand Boy"
  (let [sub {:async true
             :label "Draw a card or gain 1 [Credits]"
             :prompt "Choose one"
             :waiting-prompt true
             :choices ["Gain 1 [Credits]" "Draw 1 card"]
             :msg (msg (decapitalize target))
             :effect (req (if (= target "Gain 1 [Credits]")
                            (gain-credits state :corp eid 1)
                            (draw state :corp eid 1)))}]
    {:subroutines [sub
                   sub
                   sub]}))

(defcard "Excalibur"
  {:subroutines [{:label "The Runner cannot make another run this turn"
                  :msg "prevent the Runner from making another run"
                  :effect (effect (register-turn-flag! card :can-run nil))}]})

(defcard "Executive Functioning"
  {:subroutines [(trace-ability 4 (do-brain-damage 1))]})

(defcard "F2P"
  {:subroutines [add-runner-card-to-grip
                 (give-tags 1)]
   :runner-abilities [(break-sub [(->c :credit 2)] 1 nil
                                 {:req (req (and (not tagged)
                                                 (currently-encountering-card card state)))})]})

(defcard "Fairchild"
  {:subroutines [(end-the-run-unless-runner-pays (->c :credit 4))
                 (end-the-run-unless-runner-pays (->c :credit 4))
                 (end-the-run-unless-runner-pays (->c :trash-installed 1))
                 (end-the-run-unless-runner-pays (->c :brain 1))]})

(defcard "Fairchild 1.0"
  (let [sub {:label "Force the Runner to pay 1 [Credits] or trash an installed card"
             :msg (msg "force the Runner to " (decapitalize target))
             :player :runner
             :prompt "Choose one"
             :waiting-prompt true
             :choices (req [(when (can-pay? state :runner eid card nil [(->c :credit 1)])
                              "Pay 1 [Credits]")
                            (when (or (not (can-pay? state :runner eid card nil [(->c :credit 1)]))
                                      (can-pay? state :runner eid card nil [(->c :trash-installed 1)]))
                              "Trash an installed card")])
             :async true
             :effect (req (if (= target "Pay 1 [Credits]")
                            (wait-for (pay state side (make-eid state eid) card (->c :credit 1))
                                      (system-msg state side (:msg async-result))
                                      (effect-completed state side eid))
                            (continue-ability state :runner runner-trash-installed-sub card nil)))}]
    {:subroutines [sub
                   sub]
     :runner-abilities [(bioroid-break 1 1)]}))

(defcard "Fairchild 2.0"
  (let [sub {:label "Force the Runner to pay 2 [Credits] or trash an installed card"
             :msg (msg "force the Runner to " (decapitalize target))
             :player :runner
             :prompt "Choose one"
             :waiting-prompt true
             :choices (req [(when (can-pay? state :runner eid card nil [(->c :credit 2)])
                              "Pay 2 [Credits]")
                            (when (or (not (can-pay? state :runner eid card nil [(->c :credit 2)]))
                                      (can-pay? state :runner eid card nil [(->c :trash-installed 1)]))
                              "Trash an installed card")])
             :async true
             :effect (req (if (= target "Pay 2 [Credits]")
                            (wait-for (pay state side (make-eid state eid) card (->c :credit 2))
                                      (system-msg state side (:msg async-result))
                                      (effect-completed state side eid))
                            (continue-ability state :runner runner-trash-installed-sub card nil)))}]
    {:subroutines [sub
                   sub
                   (do-brain-damage 1)]
     :runner-abilities [(bioroid-break 2 2)]}))

(defcard "Fairchild 3.0"
  (let [sub {:label "Force the Runner to pay 3 [Credits] or trash an installed card"
             :msg (msg "force the Runner to " (decapitalize target))
             :player :runner
             :prompt "Choose one"
             :waiting-prompt true
             :choices (req [(when (can-pay? state :runner eid card nil [(->c :credit 3)])
                              "Pay 3 [Credits]")
                            (when (or (not (can-pay? state :runner eid card nil [(->c :credit 3)]))
                                      (can-pay? state :runner eid card nil [(->c :trash-installed 1)]))
                              "Trash an installed card")])
             :async true
             :effect (req (if (= target "Pay 3 [Credits]")
                            (wait-for (pay state side (make-eid state eid) card (->c :credit 3))
                                      (system-msg state side (:msg async-result))
                                      (effect-completed state side eid))
                            (continue-ability state :runner runner-trash-installed-sub card nil)))}]
    {:subroutines [sub
                   sub
                   {:label "Do 1 core damage or end the run"
                    :prompt "Choose one"
                    :waiting-prompt true
                    :choices ["Do 1 core damage" "End the run"]
                    :msg (msg (decapitalize target))
                    :async true
                    :effect (req (if (= target "Do 1 core damage")
                                   (damage state side eid :brain 1 {:card card})
                                   (end-run state side eid card)))}]
     :runner-abilities [(bioroid-break 3 3)]}))

(defcard "Fenris"
  {:on-rez take-bad-pub
   :subroutines [(do-brain-damage 1)
                 end-the-run]})

(defcard "Fire Wall"
  (wall-ice [end-the-run]))

(defcard "Flare"
  {:subroutines [(trace-ability
                   6
                   {:label "Trash 1 piece of hardware, do 2 meat damage, and end the run"
                    :async true
                    :effect
                    (effect
                      (continue-ability
                        {:prompt "Choose a piece of hardware to trash"
                         :label "Trash a piece of hardware"
                         :choices {:card hardware?}
                         :msg (msg "trash " (:title target))
                         :async true
                         :effect (req (wait-for
                                        (trash state side target {:cause :subroutine})
                                        (system-msg state :corp
                                                    (str "uses " (:title card) " to trash " (:title target)))
                                        (wait-for (damage state side :meat 2 {:unpreventable true
                                                                              :card card})
                                        (system-msg state :corp
                                                    (str "uses " (:title card) " to deal 2 meat damage"))
                                        (system-msg state :corp
                                                    (str "uses " (:title card) " to end the run"))
                                        (end-run state side eid card))))
                         :cancel-effect (req (wait-for (damage state side :meat 2 {:unpreventable true
                                                                                   :card card})
                                                       (system-msg state :corp
                                                                   (str "uses " (:title card) " to deal 2 meat damage"))
                                                       (system-msg state :corp
                                                                   (str "uses " (:title card) " to end the run"))
                                                       (end-run state side eid card)))}
                        card nil))})]})

(defcard "Formicary"
  {:derezzed-events
   [{:event :approach-server
     :interactive (req (not= ((get-autoresolve :auto-fire) state side eid card nil) "No"))
     :silent (req (= ((get-autoresolve :auto-fire) state side eid card nil) "No"))
     :optional
     {:prompt (msg "Rez and move " (card-str state card {:visible true}) " to protect the approched server?")
      :autoresolve (get-autoresolve :auto-fire)
      :req (req (and (can-rez? state side card)
                     (can-pay? state side eid card nil (get-rez-cost state side card nil))))
      :yes-ability
      {:msg (msg "rez and move " (:title card) ". The Runner is now encountering it")
       :async true
       :effect (req (wait-for (rez state side card)
                              (when (rezzed? (:card async-result))
                                (move state side (get-card state card)
                                      [:servers (target-server run) :ices]
                                      {:front true})
                                (swap! state assoc-in [:run :position] 1)
                                (set-next-phase state :encounter-ice)
                                (set-current-ice state)
                                (update-all-ice state side)
                                (update-all-icebreakers state side))
                              (effect-completed state side eid)))}}}]
   :subroutines [(end-the-run-unless-runner-pays (->c :net 2))]
   :abilities [(set-autoresolve :auto-fire "Formicary rezzing and moving itself on approach")]})

(defcard "Free Lunch"
  {:abilities [{:cost [(->c :power 1)]
                :label "Runner loses 1 [Credits]"
                :msg "make the Runner lose 1 [Credits]"
                :async true
                :effect (effect (lose-credits :runner eid 1))}]
   :subroutines [add-power-counter
                 add-power-counter]})

(defcard "Funhouse"
  {:on-encounter {:msg (msg (if (= target "Take 1 tag")
                              (str "force the runner to " (decapitalize target) " on encountering it")
                              (decapitalize target)))
                  :player :runner
                  :prompt "Choose one"
                  :choices (req [(when-not (forced-to-avoid-tags? state side)
                                   "Take 1 tag")
                                 "End the run"])
                  :waiting-prompt true
                  :async true
                  :effect (req (if (= target "Take 1 tag")
                                 (gain-tags state :runner eid 1 {:unpreventable true})
                                 (end-run state :runner eid card)))}
   :subroutines [{:player :runner
                  :async true
                  :label "Give the Runner 1 tag unless they pay 4 [Credits]"
                  :prompt "Choose one"
                  :waiting-prompt true
                  :choices (req ["Take 1 tag"
                                 (when (can-pay? state :runner eid card nil [(->c :credit 4)])
                                   "Pay 4 [Credits]")])
                  :msg (msg (if (= target "Pay 4 [Credits]")
                              (str "force the runner to " (decapitalize target))
                              "give the runner 1 tag"))
                  :effect (req (if (= "Take 1 tag" target)
                                 (gain-tags state :corp eid 1)
                                 (wait-for (pay state side (make-eid state eid) card (->c :credit 4))
                                           (system-msg state side (:msg async-result))
                                           (effect-completed state side eid))))}]})

(defcard "Galahad"
  (grail-ice end-the-run))

(defcard "Gatekeeper"
  (let [reveal-and-shuffle {:prompt "Reveal and shuffle up to 3 agendas into R&D"
                            :show-discard true
                            :choices {:card #(and (corp? %)
                                                  (or (in-hand? %)
                                                      (in-discard? %))
                                                  (agenda? %))
                                      :max (req 3)}
                            :async true
                            :effect (req (wait-for
                                           (reveal state side targets)
                                           (doseq [c targets]
                                             (move state :corp c :deck))
                                           (shuffle! state :corp :deck)
                                           (effect-completed state :corp eid)))
                            :cancel-effect (effect (shuffle! :deck)
                                                   (effect-completed eid))
                            :msg (msg
                                   "reveal "
                                   (enumerate-str
                                     (filter identity
                                             [(when-let [h (->> targets
                                                                (filter in-hand?)
                                                                (map :title)
                                                                not-empty)]
                                                (str (enumerate-str h)
                                                     " from HQ"))
                                              (when-let [d (->> targets
                                                                (filter in-discard?)
                                                                (map :title)
                                                                not-empty)]
                                                (str (enumerate-str d)
                                                     " from Archives"))]))
                                   " and shuffle them into R&D")}
        draw-reveal-shuffle {:async true
                             :label "Draw cards, reveal and shuffle agendas"
                             :effect (req (wait-for (draw-up-to state side card 3)
                                                    (continue-ability state side reveal-and-shuffle card nil)))}]
    {:static-abilities [(ice-strength-bonus (req (= :this-turn (:rezzed card))) 6)]
     :subroutines [draw-reveal-shuffle
                   end-the-run]}))

(defcard "Gemini"
  (constellation-ice (do-net-damage 1)))

(defcard "Gold Farmer"
  (letfn [(gf-lose-credits [state side eid n]
            (if (pos? n)
              (wait-for (lose-credits state :runner (make-eid state eid) 1)
                        (gf-lose-credits state side eid (dec n)))
              (effect-completed state side eid)))]
    {:implementation "Auto breaking will break even with too few credits"
     :on-break-subs {:req (req (some :printed (second targets)))
                     :msg (msg (let [n-subs (count (filter :printed (second targets)))]
                                 (str "force the runner to lose "
                                      n-subs
                                      " [Credits] for breaking printed subs")))
                     :async true
                     :effect (effect (gf-lose-credits eid (count (filter :printed (second targets)))))}
     :subroutines [(end-the-run-unless-runner-pays (->c :credit 3))
                   (end-the-run-unless-runner-pays (->c :credit 3))]}))

(defcard "Grim"
  {:on-rez take-bad-pub
   :subroutines [trash-program-sub]})

(defcard "Guard"
  {:static-abilities [{:type :bypass-ice
                       :req (req (same-card? card target))
                       :value false}]
   :subroutines [end-the-run]})

(defcard "Gutenberg"
  {:subroutines [(tag-trace 7)]
   :static-abilities [(ice-strength-bonus (req (protecting-rd? card)) 3)]})

(defcard "Gyri Labyrinth"
  {:subroutines [{:req (req run)
                  :label "Reduce Runner's hand size by 2"
                  :msg "reduce the Runner's maximum hand size by 2 until the start of the next Corp turn"
                  :effect (effect (register-lingering-effect
                                    card
                                    {:type :hand-size
                                     :duration :until-corp-turn-begins
                                     :req (req (= :runner side))
                                     :value -2}))}]})

(defcard "Hadrian's Wall"
  (wall-ice [end-the-run end-the-run]))

(defcard "Hafrún"
  (letfn [(prevent-sub-break-by [t]
            {:type :prevent-paid-ability
             :duration :end-of-run
             :value true
             :req (req (let [[break-card break-ability] targets]
                         (and (same-card? break-card t)
                              (or (contains? break-ability :break)
                                  (contains? break-ability :breaks)
                                  (contains? break-ability :heap-breaker-break)
                                  (contains? break-ability :break-cost)))))})]
    {:subroutines [end-the-run]
     :on-rez {:optional
              {:prompt "Trash a card from HQ to prevent subroutines from being broken by a Runner card abilities for the remainder of the run?"
               :req (req (and run this-server
                              (seq (:hand corp))))
               :waiting-prompt true
               :yes-ability
               {:cost [(->c :trash-from-hand 1)]
                :async true
                :effect
                (effect (continue-ability
                          {:waiting-prompt true
                           :prompt "Choose an installed Runner card"
                           :async true
                           :choices {:card #(and (installed? %)
                                                 (runner? %))}
                           :msg (msg "trash 1 card from HQ to prevent subroutines from being broken by "
                                     (:title target)
                                     " abilities for the remainder of the run")
                           :effect (req (let [t target]
                                          (add-icon state side card target "H" (faction-label card))
                                          (register-events state side card
                                            [{:event :run-ends
                                              :duration :end-of-run
                                              :effect (effect (remove-icon card t))}])
                                          (register-lingering-effect state side card (prevent-sub-break-by t))
                                          (effect-completed state side eid)))}
                          card nil))}
               :no-ability {:effect (effect (system-msg :corp (str "declines to use " (:title card))))}}}}))

(defcard "Hákarl 1.0"
  {:runner-abilities [(bioroid-break 1 1)]
   :subroutines [(do-brain-damage 1)
                 end-the-run]
   :on-rez {:req (req (and run this-server
                           (->> (get-all-installed state) (remove #(same-card? card %)) (filter rezzed?) (count) (pos?))))
            :prompt "Derez another card to prevent the runner from using printed abilities on bioroid ice this turn?"
            :choices {:not-self true
                      :req (req (and (installed? target)
                                     (rezzed? target)))}
            :waiting-prompt true
            :cancel-effect (effect (system-msg :corp (str "declines to use " (:title card)))
                                   (effect-completed eid))
            :effect (effect (derez target)
                            (system-msg (str "prevents the runner from using printed abilities on bioroid ice for the rest of the turn"))
                            (register-lingering-effect
                             card
                             {:type :prevent-paid-ability
                              :duration :end-of-turn
                              :req (req (and (ice? target)
                                             (has-subtype? target "Bioroid")))
                              :value true}))}})

(defcard "Hagen"
  {:subroutines [{:label "Trash 1 program"
                  :prompt "Choose a program that is not a decoder, fracter or killer"
                  :msg (msg "trash " (:title target))
                  :choices {:card #(and (installed? %)
                                        (program? %)
                                        (not (has-subtype? % "Decoder"))
                                        (not (has-subtype? % "Fracter"))
                                        (not (has-subtype? % "Killer")))}
                  :async true
                  :effect (effect (clear-wait-prompt :runner)
                                  (trash eid target {:cause :subroutine}))}
                 end-the-run]
   :static-abilities [(ice-strength-bonus (req (- (count (filter #(has-subtype? % "Icebreaker")
                                                                 (all-active-installed state :runner))))))]})

(defcard "Hailstorm"
  {:subroutines [{:label  "Remove a card in the Heap from the game"
                  :req     (req (not (zone-locked? state :runner :discard)))
                  :prompt  "Choose a card in the Heap"
                  :choices (req (cancellable (:discard runner) :sorted))
                  :msg     (msg "remove " (:title target) " from the game")
                  :effect  (effect (move :runner target :rfg))}
                 end-the-run]})

(defcard "Hammer"
  {:implementation "Breaking restriction not implemented"
   :subroutines [(give-tags 1)
                 {:label "Trash 1 resource or piece of hardware"
                  :msg (msg "trash " (:title target))
                  :prompt "Choose a resource of piece of hardware"
                  :choices {:req (req (and (installed? target)
                                           (or (hardware? target)
                                               (resource? target))))}
                  :async true
                  :effect (effect (trash eid target {:cause :subroutine}))}
                 {:label "Trash 1 program that is not a decoder, fracter or killer"
                  :prompt "Choose a program that is not a decoder, fracter or killer"
                  :msg (msg "trash " (:title target))
                  :choices {:card #(and (installed? %)
                                        (program? %)
                                        (not (has-subtype? % "Decoder"))
                                        (not (has-subtype? % "Fracter"))
                                        (not (has-subtype? % "Killer")))}
                  :async true
                  :effect (effect (trash eid target {:cause :subroutine}))}]})

(defcard "Descent"
  (let [shuffle-ab
        {:label "Draw 1 card and shuffle up to 2 agendas in HQ and/or Archives into R&D"
         :msg "draw 1 card"
         :async true
         :cost [(->c :credit 1)]
         :effect
         (req (wait-for
                (draw state side 1)
                (continue-ability
                  state side
                  {:prompt "Choose up to 2 agendas in HQ and/or Archives"
                   :choices {:max 2
                   :card #(and (agenda? %)
                               (or (in-hand? %)
                                   (in-discard? %)))}
                   :async true
                   :show-discard true
                   :effect
                   (req (wait-for
                          (reveal state side targets)
                          (doseq [c targets]
                            (move state :corp c :deck))
                          (shuffle! state :corp :deck)
                          (let [from-hq (map :title (filter in-hand? targets))
                                from-archives (map :title (filter in-discard? targets))]
                            (system-msg
                              state side
                              (str "uses " (:title card) " to reveal "
                                   (enumerate-str
                                     (filter identity
                                             [(when (not-empty from-hq)
                                                (str (enumerate-str from-hq)
                                                     " from HQ"))
                                              (when (not-empty from-archives)
                                                (str (enumerate-str from-archives)
                                                     " from Archives"))]))
                                   ", shuffle them into R&D")))
                          (effect-completed state side eid)))}
                card nil)))}]
    {:events [{:event :corp-turn-begins
               :interactive (req true)
               :req (req (rezzed? card))
               :optional {:prompt (msg "Add " (card-str state card) " to HQ?")
                          :yes-ability {:effect (req (move state side card :hand))
                                        :msg (msg "add " (card-str state card) " to HQ")}}}]
     :expend shuffle-ab
     :subroutines [end-the-run]}))

(defcard "Harvester"
  (let [sub {:label "Runner draws 3 cards and discards down to maximum hand size"
             :msg "make the Runner draw 3 cards and discard down to [runner-pronoun] maximum hand size"
             :async true
             :effect (req (wait-for (draw state :runner 3)
                                    (continue-ability
                                      state :runner
                                      (let [delta (- (count (get-in @state [:runner :hand])) (hand-size state :runner))]
                                        (when (pos? delta)
                                          {:prompt (msg "Choose " (quantify delta "card") " to discard")
                                           :player :runner
                                           :choices {:max delta
                                                     :card #(in-hand? %)}
                                           :async true
                                           :effect (req (wait-for (trash-cards state :runner targets)
                                                                  (system-msg
                                                                    state :runner
                                                                    (str "trashes " (enumerate-str (map :title targets))))
                                                                  (effect-completed state side eid)))}))
                                      card nil)))}]
    {:subroutines [sub
                   sub]}))

(defcard "Heimdall 1.0"
  {:subroutines [(do-brain-damage 1)
                 end-the-run end-the-run]
   :runner-abilities [(bioroid-break 1 1)]})

(defcard "Heimdall 2.0"
  {:subroutines [(do-brain-damage 1)
                 {:msg "do 1 core damage and end the run"
                  :effect (req (wait-for (damage state side :brain 1 {:card card})
                                         (end-run state side eid card)))}
                 end-the-run]
   :runner-abilities [(bioroid-break 2 2)]})

(defcard "Herald"
  {:flags {:rd-reveal (req true)}
   :subroutines [(gain-credits-sub 2)
                 {:async true
                  :label "Pay up to 2 [Credits] to place up to 2 advancement tokens"
                  :prompt "How many advancement tokens do you want to place?"
                  :choices (req (map str (range (inc (min 2 (:credit corp))))))
                  :effect (req (let [c (str->int target)]
                                 (if (can-pay? state side (assoc eid :source card :source-type :subroutine) card (:title card) (->c :credit c))
                                   (let [new-eid (make-eid state {:source card :source-type :subroutine})]
                                     (wait-for (pay state :corp new-eid card (->c :credit c))
                                               (system-msg state :corp (:msg async-result))
                                               (continue-ability
                                                 state side
                                                 {:msg (msg "pay " c " [Credits] and place " (quantify c "advancement token")
                                                            " on " (card-str state target))
                                                  :choices {:req (req (can-be-advanced? state target))}
                                                  :effect (effect (add-prop target :advance-counter c {:placed true}))}
                                                 card nil)))
                                   (effect-completed state side eid))))}]
   :on-access {:async true
               :req (req (not (in-discard? card)))
               :msg "force the Runner to encounter Herald"
               :effect (req (force-ice-encounter state side eid card))}})

(defcard "Himitsu-Bako"
  {:abilities [{:msg "add itself to HQ"
                :cost [(->c :credit 1)]
                :effect (effect (move card :hand))}]
   :subroutines [end-the-run]})

(defcard "Hive"
  (let [corp-points (fn [corp] (min 5 (max 0 (- 5 (:agenda-point corp 0)))))
        ability {:silent (req true)
                 :effect (effect (reset-printed-subs card (corp-points corp) end-the-run))}
        reset-to-5 {:silent (req true)
                    :effect (effect (reset-printed-subs card 5 end-the-run))}]
    {:events [(assoc ability
                     :event :rez
                     :req (req (same-card? card (:card context))))
              (assoc ability :event :agenda-scored)
              (assoc ability :event :subroutines-should-update)
              (assoc ability
                     :event :card-moved
                     :req (req (= :corp (:scored-side (:card context)))))]
     :abilities [{:label "Lose subroutines"
                  :msg (msg "lose " (- 5 (corp-points corp)) " subroutines")
                  :effect (effect (reset-printed-subs card (corp-points corp) end-the-run))}]
     :subroutines [end-the-run
                   end-the-run
                   end-the-run
                   end-the-run
                   end-the-run]}))

(defcard "Holmegaard"
  {:subroutines [(trace-ability 4 {:label "Runner cannot access any cards this run"
                                   :msg "stop the Runner from accessing any cards this run"
                                   :effect (effect (prevent-access))})
                 {:label "Trash an icebreaker"
                  :prompt "Choose an icebreaker to trash"
                  :msg (msg "trash " (:title target))
                  :choices {:card #(and (installed? %)
                                        (has-subtype? % "Icebreaker"))}
                  :async true
                  :effect (effect (clear-wait-prompt :runner)
                                  (trash eid target {:cause :subroutine}))}]})

(defcard "Hortum"
  (letfn [(hort [n] {:prompt "Choose a card to add to HQ"
                     :async true
                     :choices (req (cancellable (:deck corp) :sorted))
                     :msg "add 1 card to HQ from R&D"
                     :cancel-effect (req (shuffle! state side :deck)
                                         (system-msg state side (str "shuffles R&D"))
                                         (effect-completed state side eid))
                     :effect (req (move state side target :hand)
                                  (if (< n 2)
                                    (continue-ability state side (hort (inc n)) card nil)
                                    (do (shuffle! state side :deck)
                                        (system-msg state side (str "shuffles R&D"))
                                        (effect-completed state side eid))))})]
    (let [breakable-fn (req (when (or (> 3 (get-counters card :advancement))
                                      (not (has-subtype? target "AI"))
                                      (not= (:title card) "Hortum") ;; loki protection
                                      (is-disabled-reg? state card))
                              :unrestricted))]
      {:advanceable :always
       :subroutines [{:label "Gain 1 [Credits] (Gain 4 [Credits])"
                      :breakable breakable-fn
                      :msg (msg "gain " (if (wonder-sub card 3) "4" "1") " [Credits]")
                      :async true
                      :effect (effect (gain-credits :corp eid (if (wonder-sub card 3) 4 1)))}
                     {:label "End the run (Search R&D for up to 2 cards and add them to HQ, shuffle R&D, end the run)"
                      :async true
                      :breakable breakable-fn
                      :effect (req (if (wonder-sub card 3)
                                     (wait-for
                                       (resolve-ability state side (hort 1) card nil)
                                       (do (system-msg state side
                                                       (str "uses " (:title card) " to add 2 cards to HQ from R&D, "
                                                            "shuffle R&D, and end the run"))
                                           (end-run state side eid card)))
                                     (do (system-msg state side (str "uses " (:title card) " to end the run"))
                                         (end-run state side eid card))))}]})))

(defcard "Hourglass"
  {:subroutines [runner-loses-click
                 runner-loses-click
                 runner-loses-click]})

(defcard "Howler"
  {:subroutines
   [{:label "Install a piece of Bioroid ice from HQ or Archives"
     :req (req (some #(and (corp? %)
                           (or (in-hand? %)
                               (in-discard? %))
                           (has-subtype? % "Bioroid"))
                     (concat (:hand corp) (:discard corp))))
     :async true
     :prompt "Choose a piece of Bioroid ice in HQ or Archives to install"
     :show-discard true
     :choices {:card #(and (corp? %)
                           (or (in-hand? %)
                               (in-discard? %))
                           (has-subtype? % "Bioroid"))}
     :effect (req (wait-for (corp-install state side (make-eid state eid)
                                          target (zone->name (target-server run))
                                          {:ignore-all-cost true
                                           :index (card-index state card)})
                            (let [new-ice async-result]
                              (register-events
                                state side card
                                [{:event :run-ends
                                  :duration :end-of-run
                                  :async true
                                  :msg (req (msg "derez " (:title new-ice) " and trash itself"))
                                  :effect (effect (derez new-ice)
                                                  (trash eid card {:cause :subroutine}))}]))))}]})

(defcard "Hudson 1.0"
  (let [sub {:msg "prevent the Runner from accessing more than 1 card during this run"
             :effect (req (max-access state 1))}]
    {:subroutines [sub
                   sub]
     :runner-abilities [(bioroid-break 1 1)]}))

(defcard "Hunter"
  {:subroutines [(tag-trace 3)]})

(defcard "Hydra"
  (letfn [(otherwise-tag [message ability]
            {:msg (msg (if tagged message "give the Runner 1 tag"))
             :label (str (capitalize message) " if the Runner is tagged; otherwise, give the Runner 1 tag")
             :async true
             :effect (req (if tagged
                            (ability state :runner eid card nil)
                            (gain-tags state :runner eid 1)))})]
    {:subroutines [(otherwise-tag
                     "do 3 net damage"
                     (effect (damage :runner eid :net 3 {:card card})))
                   (otherwise-tag
                     "gain 5 [Credits]"
                     (effect (gain-credits :corp eid 5)))
                   (otherwise-tag
                     "end the run"
                     (effect (end-run eid card)))]}))

(defcard "Ice Wall"
  (wall-ice [end-the-run]))

(defcard "Ichi 1.0"
  {:subroutines [trash-program-sub trash-program-sub
                 (trace-ability 1 {:label "Give the Runner 1 tag and do 1 core damage"
                                   :msg "give the Runner 1 tag and do 1 core damage"
                                   :async true
                                   :effect (req (wait-for (damage state :runner :brain 1 {:card card})
                                                          (gain-tags state :corp eid 1)))})]
   :runner-abilities [(bioroid-break 1 1)]})

(defcard "Ichi 2.0"
  {:subroutines [trash-program-sub trash-program-sub
                 (trace-ability 3 {:label "Give the Runner 1 tag and do 1 core damage"
                                   :msg "give the Runner 1 tag and do 1 core damage"
                                   :async true
                                   :effect (req (wait-for (damage state :runner :brain 1 {:card card})
                                                          (gain-tags state :corp eid 1)))})]
   :runner-abilities [(bioroid-break 2 2)]})

(defcard "Inazuma"
  {:subroutines
   [{:msg "prevent the Runner from breaking subroutines on the next piece of ice they encounter this run"
     :effect
     (effect (register-events
              card
              [{:event :encounter-ice
                :duration :end-of-run
                :unregister-once-resolved true
                :msg (msg "prevent the runner from breaking subroutines on " (:title (:ice context)))
                :effect (effect (register-lingering-effect
                                 card
                                 (let [encountered-ice (:ice context)]
                                   {:type :cannot-break-subs-on-ice
                                    :duration :end-of-encounter
                                    :req (req (same-card? encountered-ice (:ice context)))
                                    :value true})))}]))}
    {:msg "prevent the Runner from jacking out until after the next piece of ice"
     :effect
     (req (prevent-jack-out state side)
          (register-events
           state side card
           [{:event :encounter-ice
             :duration :end-of-run
             :unregister-once-resolved true
             :effect
             (req (let [encountered-ice (:ice context)]
                    (register-events
                     state side card
                     [{:event :end-of-encounter
                       :duration :end-of-encounter
                       :unregister-once-resolved true
                       :msg (msg "can jack out again after encountering " (:title encountered-ice))
                       :effect (req (swap! state update :run dissoc :cannot-jack-out))
                       :req (req (same-card? encountered-ice (:ice context)))}])))}]))}]})

(defcard "Information Overload"
  {:on-encounter (tag-trace 1)
   :on-rez {:effect (effect (reset-variable-subs card (count-tags state) runner-trash-installed-sub))}
   :events [{:event :subroutines-should-update
             :effect (effect (reset-variable-subs card (count-tags state) runner-trash-installed-sub))}
            {:event :tags-changed
             :effect (effect (reset-variable-subs card (count-tags state) runner-trash-installed-sub))}]})

(defcard "Interrupt 0"
  (let [sub {:label "Make the Runner pay 1 [Credits] to use icebreaker"
             :msg "make the Runner pay 1 [Credits] to use icebreakers to break subroutines during this run"
             :effect (effect (register-lingering-effect
                               card
                               {:type :break-sub-additional-cost
                                :duration :end-of-run
                                :req (req (and ; The card is an icebreaker
                                               (has-subtype? (:card context) "Icebreaker")
                                               ; and is using a break ability
                                               (contains? (:ability context) :break)
                                               (pos? (:break (:ability context) 0))))
                                :value (->c :credit 1)}))}]
    {:subroutines [sub
                   sub]}))

(defcard "IP Block"
  {:on-encounter (assoc (give-tags 1)
                        :req (req (seq (filter #(has-subtype? % "AI") (all-active-installed state :runner))))
                        :msg "give the runner 1 tag because there is an installed AI")
   :subroutines [(tag-trace 3)
                 end-the-run-if-tagged]})

(defcard "IQ"
  {:subroutines [end-the-run]
   :static-abilities [(ice-strength-bonus (req (count (:hand corp))))]
   :rez-cost-bonus (req (count (:hand corp)))
   :leave-play (req (remove-watch state (keyword (str "iq" (:cid card)))))})

(defcard "Ireress"
  (let [sub {:msg "make the Runner lose 1 [Credits]"
             :async true
             :effect (effect (lose-credits :runner eid 1))}
        ability {:effect (effect (reset-variable-subs card (count-bad-pub state) sub))}]
    {:events [(assoc ability
                     :event :rez
                     :req (req (same-card? card (:card context))))
              (assoc ability :event :corp-gain-bad-publicity)
              (assoc ability :event :corp-lose-bad-publicity)]}))

(defcard "It's a Trap!"
  {:on-expose {:msg "do 2 net damage"
               :async true
               :effect (effect (damage eid :net 2 {:card card}))}
   :subroutines [(assoc runner-trash-installed-sub
                        :effect (req (wait-for (trash state side target {:cause :subroutine})
                                               (system-msg state :corp (str "uses " (:title card) " to trash itself"))
                                               (trash state :corp (make-eid state eid) card {:cause :subroutine})
                                               (encounter-ends state side eid))))]})

(defcard "Ivik"
  {:subroutines [(do-net-damage 2)
                 end-the-run]
   :rez-cost-bonus (req (- (subtype-ice-count corp "Code Gate")))})

(defcard "Jaguarundi"
  {:on-encounter {:req (req (threat-level 4 state))
                  :async true
                  :effect (req
                            (continue-ability
                              state side
                              {:player :runner
                               :prompt "Choose one"
                               :choices (req ["Take 1 tag"
                                              (when (can-pay? state :runner nil card nil [(->c :click 1 {:allowed-during-run true})])
                                                "Spend [Click]")])
                               :waiting-prompt true
                               :async true
                               :msg (msg (if (= target "Take 1 tag")
                                           "give the Runner 1 tag"
                                           (str "force the runner to " (decapitalize target) " on encountering it")))
                               :effect (req (if (= target "Take 1 tag")
                                              (gain-tags state :runner eid 1)
                                              (wait-for (pay state :runner (make-eid state eid) card (->c :click 1 {:allowed-during-run true}))
                                                        (system-msg state side (:msg async-result))
                                                        (effect-completed state :runner eid))))}
                              card nil))}
   :subroutines [(give-tags 1)
                 {:label "Do 1 core damage if the Runner is tagged"
                  :req (req tagged)
                  :msg "do 1 core damage"
                  :async true
                  :effect (req (damage state side eid :brain 1 {:card card}))}]})

(defcard "Janus 1.0"
  {:subroutines [(do-brain-damage 1)
                 (do-brain-damage 1)
                 (do-brain-damage 1)
                 (do-brain-damage 1)]
   :runner-abilities [(bioroid-break 1 1)]})

(defcard "Jua"
  {:on-encounter {:msg "prevent the Runner from installing cards for the rest of the turn"
                  :effect (effect (register-turn-flag! card :runner-lock-install (constantly true)))}
   :subroutines [{:label "Choose 2 installed Runner cards, if able. The Runner must add 1 of those to the top of the Stack"
                  :req (req (>= (count (all-installed state :runner)) 2))
                  :async true
                  :prompt "Choose 2 installed Runner cards"
                  :choices {:card #(and (runner? %)
                                        (installed? %))
                            :max 2
                            :all true}
                  :msg (msg "add either " (card-str state (first targets))
                            " or " (card-str state (second targets))
                            " to the top of the Stack")
                  :effect (effect (continue-ability
                                    (when (= 2 (count targets))
                                      {:player :runner
                                       :waiting-prompt true
                                       :prompt "Choose a card to move to the top of the Stack"
                                       :choices {:card #(some (partial same-card? %) targets)}
                                       :effect (req (move state :runner target :deck {:front true})
                                                    (system-msg state :runner (str "selected " (card-str state target) " to move to the top of the Stack")))})
                                    card nil))}]})

(defcard "Kakugo"
  {:events [{:event :pass-ice
             :async true
             :req (req (same-card? (:ice context) card))
             :msg "do 1 net damage"
             :effect (effect (damage eid :net 1 {:card card}))}]
   :subroutines [end-the-run]})

(defcard "Kamali 1.0"
  (letfn [(sub-map [cost]
            {:player :runner
             :async true
             :waiting-prompt true
             :prompt "Choose one"
             :choices (req ["Take 1 core damage"
                            (when (can-pay? state :runner eid card nil cost)
                              (capitalize (build-cost-label cost)))])
             :msg (msg (if (= target "Take 1 core damage")
                         "do 1 core damage"
                         (str "force the runner to " (decapitalize target))))
             :effect (req (if (= target "Take 1 core damage")
                            (damage state :runner eid :brain 1 {:card card})
                            (wait-for (pay state :runner (make-eid state eid) card cost)
                                      (system-msg state :runner (:msg async-result))
                                      (effect-completed state side eid))))})
          (brain-damage-unless-runner-pays [cost]
            {:label (str "Force the Runner to take 1 core damage or " (build-cost-label cost))
             :async true
             :effect (req (wait-for (resolve-ability state side (sub-map cost) card nil)
                                    (clear-wait-prompt state :corp)))})]
    {:subroutines [(brain-damage-unless-runner-pays [(->c :resource 1)])
                   (brain-damage-unless-runner-pays [(->c :hardware 1)])
                   (brain-damage-unless-runner-pays [(->c :program 1)])]
     :runner-abilities [(bioroid-break 1 1)]}))

(defcard "Karunā"
  {:subroutines [{:label "Do 2 net damage. The Runner may jack out"
                  :async true
                  :effect (req (wait-for (resolve-ability state side
                                                          (do-net-damage 2)
                                                          card nil)
                                         (continue-ability state side (offer-jack-out) card nil)))}
                 (do-net-damage 2)]})

(defcard "Kitsune"
  {:subroutines [{:label "Force the Runner to access a card in HQ"
                  :optional
                  {:req (req (pos? (count (:hand corp))))
                   :prompt "Force the Runner to access a card in HQ?"
                   :yes-ability
                   {:async true
                    :prompt "Choose a card in HQ"
                    :choices {:card (every-pred in-hand? corp?)
                              :all true}
                    :label "Force the Runner to breach HQ and access a card"
                    :msg (msg "force the Runner to breach HQ and access " (:title target))
                    :effect (req (wait-for (breach-server state :runner [:hq] {:no-root true
                                                                               :access-first target})
                                           (system-msg state :corp (str "uses " (:title card) " to trash itself"))
                                           (trash state :corp (make-eid state eid) card {:cause :subroutine})
                                           (encounter-ends state side eid)))}}}]})

(defcard "Klevetnik"
  ;; TODO - make this use a floating effect to disable cards
  (let [re-enable-target
        (fn [t] {:event :corp-turn-ends
                 :unregister-once-resolved true
                 :msg (msg "unblank " (:title t))
                 :effect
                 (req (when (:disabled (get-card state t))
                        (enable-card state :runner (get-card state t))
                        (when-let [reactivate-effect (:reactivate (card-def t))]
                          (resolve-ability state :runner reactivate-effect (get-card state t) nil))))})
        register-corp-next-turn-end
        (fn [t] {:event :corp-turn-ends ;; delayed registration to make it wait the Corp next turn end
                 :unregister-once-resolved true
                 :effect (effect (register-events card [(re-enable-target t)]))})
        on-rez-ability {:prompt "Choose an installed resource"
                        :waiting-prompt true
                        :choices {:card #(and (installed? %)
                                              (resource? %))}
                        :async true
                        :msg (msg "let the Runner gain 2 [Credits] to"
                                  " blank the text box of " (:title target)
                                  " until the Corp next turn ends")
                        :effect
                        (req (let [t target]
                               (wait-for (gain-credits state :runner 2)
                                         (disable-card state :runner t)
                                         (register-events
                                           state side card
                                           [(if (= (:active-player @state) :runner)
                                              (re-enable-target t)
                                              (register-corp-next-turn-end t))])
                                         (effect-completed state side eid))))}]
    {:subroutines [end-the-run]
     :on-rez {:optional
              {:prompt "Let the Runner gain 2 [Credits]?"
               :waiting-prompt true
               :req (req (and run this-server
                              (seq (all-installed-runner-type state :resource))))
               :yes-ability {:async true
                             :effect (effect (continue-ability on-rez-ability card nil))}
               :no-ability {:effect (effect (system-msg :corp (str "declines to use " (:title card))))}}}}))

(defcard "Komainu"
  {:on-encounter {:effect (effect (gain-variable-subs card (count (:hand runner)) (do-net-damage 1)))}
   :events [{:event :run-ends
             :effect (effect (reset-variable-subs card 0 nil))}]})

(defcard "Konjin"
  {:on-encounter (do-psi {:async true
                          :label "Force the runner to encounter another ice"
                          :prompt "Choose a piece of ice"
                          :choices {:card #(and (ice? %)
                                                (rezzed? %))
                                    :not-self true}
                          :msg (msg "force the Runner to encounter " (card-str state target))
                          :effect (req (force-ice-encounter state side eid target))})})

(defcard "Lab Dog"
  {:subroutines [{:label "Force the Runner to trash an installed piece of hardware"
                  :player :runner
                  :async true
                  :msg (msg "force the Runner to trash " (:title target) " and trash itself")
                  :prompt "Choose a piece of hardware to trash"
                  :choices {:card #(and (installed? %)
                                        (hardware? %))}
                  :effect (req (wait-for (trash state side target {:cause :subroutine})
                                         (trash state :corp (make-eid state eid) card {:cause :subroutine})
                                         (encounter-ends state side eid)))}]})

(defcard "Lancelot"
  (grail-ice trash-program-sub))

(defcard "Little Engine"
  {:subroutines [end-the-run
                 end-the-run
                 {:msg "make the Runner gain 5 [Credits]"
                  :async true
                  :effect (effect (gain-credits :runner eid 5))}]})

(defcard "Lockdown"
  {:subroutines [{:label "The Runner cannot draw cards for the remainder of this turn"
                  :msg "prevent the Runner from drawing cards"
                  :effect (effect (prevent-draw))}]})

(defcard "Logjam"
  {:advanceable :always
   :static-abilities [(ice-strength-bonus (req (get-counters card :advancement)))]
   :on-rez {:msg (msg "place " (quantify (inc (faceup-archives-types corp)) "advancement counter") " on itself")
            :effect (effect (add-prop card
                                      :advance-counter
                                      (inc (faceup-archives-types corp))
                                      {:placed true}))}
   :subroutines [(combine-abilities (gain-credits-sub 2) end-the-run)
                 end-the-run
                 end-the-run]})

(defcard "Loki"
  {:on-encounter
   {:req (req (<= 2 (count (filter ice? (all-active-installed state :corp)))))
    :choices {:card #(and (ice? %)
                          (active? %))
              :not-self true}
    :effect (req (let [target-subtypes (:subtype target)]
                   (register-lingering-effect
                     state :corp card
                     {:type :gain-subtype
                      :duration :end-of-run
                      :req (req (same-card? card target))
                      :value (:subtypes target)})
                   (system-msg state :corp (str "uses " (:title card) " to choose " (card-str state target)))
                   (doseq [sub (:subroutines target)]
                     (add-sub! state side (get-card state card) sub (:cid target) {:front true}))
                   (register-events
                     state side card
                     (let [cid (:cid target)]
                       [{:event :run-ends
                         :unregister-once-resolved true
                         :req (req (get-card state card))
                         :effect (effect (remove-subs! (get-card state card) #(= cid (:from-cid %))))}]))))}
   :subroutines [{:label "End the run unless the Runner shuffles the grip into the stack"
                  :player :runner
                  :async true
                  :prompt "Choose one"
                  :waiting-prompt true
                  :choices (req [(when-not (and (zero? (count (:hand runner)))
                                                (< (count (:deck runner)) 2)) ; UFAQ 24
                                   "Shuffle the grip into the stack")
                                 "End the run"])
                  :msg (msg (if (= target "End the run")
                                   (decapitalize target)
                                   (str "force the Runner to " (decapitalize target))))
                  :effect (req (if (= target "End the run")
                                 (end-run state :corp eid card)
                                 (do (doseq [c (:hand runner)]
                                        (move state :runner c :deck))
                                      (shuffle! state :runner :deck)
                                      (effect-completed state side eid))))}]})

(defcard "Loot Box"
  (letfn [(top-3 [state] (take 3 (get-in @state [:runner :deck])))
          (top-3-names [state] (map :title (top-3 state)))]
    {:subroutines [(end-the-run-unless-runner-pays (->c :credit 2))
                   {:label "Reveal the top 3 cards of the Stack"
                    :async true
                    :effect (req (system-msg state side (str "uses " (:title card) " to reveal "
                                                             (enumerate-str (top-3-names state))
                                                             " from the top of the stack"))
                              (wait-for
                                (reveal state side (top-3 state))
                                (continue-ability
                                  state side
                                  {:waiting-prompt true
                                   :prompt "Choose a card to add to the Grip"
                                   :choices (req (top-3 state))
                                   :msg (msg "add " (:title target) " to the Grip, gain " (:cost target)
                                             " [Credits], shuffle the Stack and trash itself")
                                   :async true
                                   :effect (req (move state :runner target :hand)
                                                (wait-for (gain-credits state :corp (:cost target))
                                                          (shuffle! state :runner :deck)
                                                          (trash state :corp (make-eid state eid) card {:cause :subroutine})
                                                          (encounter-ends state side eid)))}
                                  card nil)))}]}))

(defcard "Lotus Field"
  {:static-abilities [{:type :cannot-lower-strength
                       :req (req (same-card? card (:ice context)))
                       :value true}]
   :subroutines [end-the-run]})

(defcard "Lycan"
  (morph-ice "Sentry" "Code Gate" trash-program-sub))

(defcard "Lycian Multi-Munition"
  (letfn [(ice-subtype-choice [choices]
            {:prompt "Choose an ice subtype"
             :choices choices
             :async true
             :effect (req (if (= target "Done")
                            (effect-completed state side eid)
                            (let [remaining-choices (remove #{target} choices)
                                  new-choices (dedupe (conj remaining-choices "Done"))]
                              ;; note this is a lingering ability and persists so
                              ;; long as the card is rezzed
                              ;; if the card is hushed, it will not derez, so the subtypes will stay!
                              ;; - nbkelly, jan '24
                              (system-msg state side (str "uses " (:title card) " to make itself gain " target))
                              (register-lingering-effect
                                state side card
                                (let [ice card]
                                  {:type :gain-subtype
                                   :req (req (same-card? ice target))
                                   :value target}))
                              (continue-ability
                                state side
                                (ice-subtype-choice new-choices)
                                card nil))))})]
    {:on-rez {:async true
              :waiting-prompt true
              :effect (effect (continue-ability (ice-subtype-choice ["Barrier" "Code Gate" "Sentry"]) card nil))}
     :derez-effect {:effect (req (unregister-effects-for-card state side card #(= :gain-subtype (:type %))))}
     :static-abilities [{:type :gain-subtype
                         :req (req (and (same-card? card target) (:subtype-target card)))
                         :value (req (:subtype-target card))}]
     :events [{:event :runner-turn-ends
               :req (req (rezzed? card))
               :effect (effect (derez :corp card))}
              {:event :corp-turn-ends
               :req (req (rezzed? card))
               :effect (effect (derez :corp card))}]
     :subroutines [{:label "(Code Gate) Force the Runner to lose [Click] and 1 [Credit]"
                    :msg "force the Runner to lose [Click] and 1 [Credit]"
                    :req (req (has-subtype? card "Code Gate"))
                    :async true
                    :effect (req (wait-for
                                   (lose-credits state :runner 1)
                                   (lose-clicks state :runner 1)
                                   (effect-completed state side eid)))}
                   {:label "(Sentry) Trash a program"
                    :prompt "Choose a program to trash"
                    :req (req (has-subtype? card "Sentry"))
                    :msg (msg "trash " (:title target))
                    :choices {:card #(and (installed? %)
                                          (program? %))}
                    :async true
                    :effect (effect (trash eid target {:cause :subroutine}))}
                   {:label "(Barrier) Gain 1 [Credit] and end the run"
                    :msg "gain 1 [Credit] and end the run"
                    :req (req (has-subtype? card "Barrier"))
                    :async true
                    :effect (req (wait-for
                                   (gain-credits state :corp 1)
                                   (end-run state :corp eid card)))}]}))

(defcard "M.I.C."
  {:abilities [{:label "End the run unless the Runner spends [Click]"
                :msg "end the run unless the Runner spends [Click]"
                :req (req (and run this-server))
                :async true
                :cost [(->c :trash-can)]
                :effect (req (wait-for (resolve-ability
                                         state side
                                         (end-the-run-unless-runner-pays (->c :click 1 {:allowed-during-run true}) "ability")
                                         card nil)
                                       (when (and run (= (get-current-ice state) card))
                                         (encounter-ends state side eid))))}]
   :subroutines [runner-loses-click
                 runner-loses-click
                 end-the-run]})

(defcard "Machicolation A"
  {:subroutines [trash-program-sub
                 trash-program-sub
                 trash-hardware-sub
                 {:label "Runner loses 3 [Credits], if able. End the run"
                  :msg "make the Runner lose 3 [Credits] and end the run"
                  :async true
                  :effect (req (if (>= (:credit runner) 3)
                                 (wait-for (lose-credits state :runner (make-eid state eid) 3)
                                           (end-run state :corp eid card))
                                 (end-run state :corp eid card)))}]})

(defcard "Machicolation B"
  {:subroutines [trash-resource-sub
                 trash-resource-sub
                 (do-net-damage 1)
                 {:label "Runner loses [click], if able. End the run"
                  :msg "make the Runner lose [click] and end the run"
                  :async true
                  :effect (req (lose-clicks state :runner 1)
                               (end-run state :corp eid card))}]})

(defcard "Macrophage"
  {:subroutines [(trace-ability 4 {:label  "Purge virus counters"
                                   :msg    "purge virus counters"
                                   :async true
                                   :effect (effect (purge eid))})
                 (trace-ability 3 {:label   "Trash a virus"
                                   :prompt  "Choose a virus to trash"
                                   :choices {:card #(and (installed? %)
                                                         (has-subtype? % "Virus"))}
                                   :msg     (msg "trash " (:title target))
                                   :async   true
                                   :effect  (effect (clear-wait-prompt :runner)
                                                    (trash eid target {:cause :subroutine}))})
                 (trace-ability 2 {:label   "Remove a virus in the Heap from the game"
                                   :req     (req (not (zone-locked? state :runner :discard)))
                                   :prompt  "Choose a virus in the Heap to remove from the game"
                                   :choices (req (cancellable (filter #(has-subtype? % "Virus") (:discard runner)) :sorted))
                                   :msg     (msg "remove " (:title target) " from the game")
                                   :effect  (effect (move :runner target :rfg))})
                 (trace-ability 1 end-the-run)]})

(defcard "Magnet"
  {:on-rez {:req (req (some #(some program? (:hosted %))
                            (remove-once #(same-card? % card)
                                         (filter ice? (all-installed state corp)))))
            :prompt "Choose a Program to host"
            :msg (msg "host " (card-str state target))
            :choices {:req (req (and (program? target)
                                     (ice? (:host target))
                                     (not (same-card? (:host target) card))))}
            :effect (req (host state side card target)
                         (update-disabled-cards state)
                         (trigger-event
                           state :corp
                           :subroutines-should-update))}
   :static-abilities [{:type :disable-card
                       :req (req (and (same-card? (:host target) card)
                                      (not= (:title target) "Hush")
                                      (program? target)))
                       :value true}]
   :subroutines [end-the-run]})

(defcard "Mamba"
  {:abilities [(power-counter-ability (do-net-damage 1))]
   :subroutines [(do-net-damage 1)
                 (do-psi {:label "Place 1 power counter"
                          :msg "place 1 power counter"
                          :effect (effect (add-counter card :power 1)
                                          (effect-completed eid))})]})

(defcard "Marker"
  {:subroutines [{:label "Give next encountered ice \"End the run\""
                  :msg "give next encountered ice \"[Subroutine] End the run\" after all its other subroutines for the remainder of the run"
                  :effect (effect
                            (register-events
                              card
                              [{:event :encounter-ice
                                :duration :end-of-run
                                :unregister-once-resolved true
                                :req (req (rezzed? (:ice context)))
                                :msg (msg "give " (:title (:ice context)) "\"[Subroutine] End the run\" after all its other subroutines")
                                :effect (effect (add-sub! (:ice context) end-the-run (:cid card) {:back true}))}
                               {:event :end-of-encounter
                                :duration :end-of-run
                                :effect (effect (remove-sub! (:ice context) #(= (:cid card) (:from-cid %))))}]))}]})

(defcard "Markus 1.0"
  {:subroutines [runner-trash-installed-sub
                 end-the-run]
   :runner-abilities [(bioroid-break 1 1)]})

(defcard "Maskirovka"
  {:subroutines [(gain-credits-sub 2)
                 end-the-run]})

(defcard "Masvingo"
  (let [ability {:req (req (same-card? card (:card context)))
                 :effect (effect (reset-variable-subs card (get-counters card :advancement) end-the-run))}]
    {:advanceable :always
     :on-rez {:effect (effect (add-prop card :advance-counter 1 {:placed true}))}
     :events [(assoc ability :event :advance)
              (assoc ability :event :subroutines-should-update :req nil)
              (assoc ability :event :advancement-placed)
              (assoc ability :event :rez)]}))

(defcard "Matrix Analyzer"
  {:on-encounter {:cost [(->c :credit 1)]
                  :choices {:req (req (can-be-advanced? state target))}
                  :msg (msg "place 1 advancement token on " (card-str state target))
                  :effect (effect (add-prop target :advance-counter 1 {:placed true}))}
   :subroutines [(tag-trace 2)]})

(defcard "Mausolus"
  {:advanceable :always
   :subroutines [{:label "Gain 1 [Credits] (Gain 3 [Credits])"
                  :msg (msg "gain " (if (wonder-sub card 3) 3 1) " [Credits]")
                  :async true
                  :effect (effect (gain-credits eid (if (wonder-sub card 3) 3 1)))}
                 {:label "Do 1 net damage (Do 3 net damage)"
                  :async true
                  :msg (msg "do " (if (wonder-sub card 3) 3 1) " net damage")
                  :effect (effect (damage eid :net (if (wonder-sub card 3) 3 1) {:card card}))}
                 {:label "Give the Runner 1 tag (and end the run)"
                  :async true
                  :msg (msg "give the Runner 1 tag"
                            (when (wonder-sub card 3)
                              " and end the run"))
                  :effect (req (gain-tags state :corp eid 1)
                               (if (wonder-sub card 3)
                                 (end-run state side eid card)
                                 (effect-completed state side eid)))}]})

(defcard "Meridian"
  {:subroutines [{:label "Gain 4 [Credits] and end the run"
                  :waiting-prompt true
                  :prompt "Choose one"
                  :choices ["Corp gains 4 [Credits] and end the run" "Add Meridian to score area"]
                  :msg (msg (if (str/starts-with? target "Corp")
                              "gain 4 [Credits] and end the run"
                              (str "force the Runner to " (decapitalize target))))
                  :player :runner
                  :async true
                  :effect (req (if (str/starts-with? target "Corp")
                                 (wait-for (gain-credits state :corp 4)
                                           (end-run state :runner eid card))
                                 (wait-for (encounter-ends state side (make-eid state eid))
                                           (as-agenda state :runner card -1)
                                           (effect-completed state side eid))))}]})

(defcard "Merlin"
  (grail-ice (do-net-damage 2)))

(defcard "Meru Mati"
  {:subroutines [end-the-run]
   :static-abilities [(ice-strength-bonus (req (protecting-hq? card)) 3)]})

(defcard "Metamorph"
  {:subroutines [{:label "Swap 2 pieces of ice or swap 2 installed non-ice"
                  :msg "swap 2 pieces of ice or swap 2 installed non-ice"
                  :async true
                  :prompt "Choose one"
                  :waiting-prompt true
                  :req (req (or (<= 2 (count (filter ice? (all-installed state :corp))))
                                (<= 2 (count (remove ice? (all-installed state :corp))))))
                  :choices (req [(when (<= 2 (count (filter ice? (all-installed state :corp))))
                                   "Swap 2 pieces of ice")
                                 (when (<= 2 (count (remove ice? (all-installed state :corp))))
                                   "Swap 2 non-ice")])
                  :effect (effect
                            (continue-ability
                              (if (= target "Swap 2 pieces of ice")
                                {:prompt "Choose 2 pieces of ice to swap"
                                 :choices {:card #(and (installed? %)
                                                       (ice? %))
                                           :not-self true
                                           :max 2
                                           :all true}
                                 :msg (msg "swap the positions of " (card-str state (first targets))
                                           " and " (card-str state (second targets)))
                                 :effect (req (apply swap-ice state side targets))}
                                {:prompt "Choose 2 cards to swap"
                                 :choices {:card #(and (installed? %)
                                                       (not (ice? %)))
                                           :max 2
                                           :all true}
                                 :msg (msg "swap the positions of " (card-str state (first targets))
                                           " and " (card-str state (second targets)))
                                 :effect (req (apply swap-installed state side targets))})
                              card nil))}]})

(defcard "Mestnichestvo"
  {:advanceable :always
   :on-encounter
   {:optional {:prompt "Remove 1 hosted advancement counter to make the Runner lose 3 [Credits]?"
               :req (req (pos? (get-counters (get-card state card) :advancement)))
               :yes-ability {:async true
                             :msg (msg "spend 1 hosted advancement counter from " (:title card) " to force the Runner to lose 3 [Credits]")
                             :effect (effect (add-prop :corp card :advance-counter -1 {:placed true})
                                             (lose-credits :runner eid 3))}
               :no-ability {:effect (effect (system-msg :corp (str "declines to use " (:title card))))}}}
   :subroutines [{:label "The Runner loses 3 [Credits]"
                  :msg "force the Runner to lose 3 [Credits]"
                  :async true
                  :effect (effect (lose-credits :runner eid 3))}
                 end-the-run]})

(defcard "Mganga"
  {:subroutines [(do-psi {:async true
                          :label "Do 2 net damage"
                          :msg "do 2 net damage and trash itself"
                          :player :corp
                          :effect (req (wait-for (damage state :corp :net 2 {:card card})
                                                 (trash state :corp (make-eid state eid) card {:cause :subroutine})
                                                 (encounter-ends state side eid)))}
                         {:async true
                          :label "Do 1 net damage"
                          :msg "do 1 net damage and trash itself"
                          :player :corp
                          :effect (req (wait-for (damage state :corp :net 1 {:card card})
                                                 (trash state :corp (make-eid state eid) card {:cause :subroutine})
                                                 (encounter-ends state side eid)))})]})

(defcard "Mind Game"
  {:subroutines [(do-psi {:label "Redirect the run to another server"
                          :async true
                          :player :corp
                          :prompt "Choose a server"
                          :waiting-prompt true
                          :choices (req (remove #{(-> @state :run :server central->name)} servers))
                          :msg (msg "redirect the run to " target
                                    " and for the remainder of the run, the runner must add 1 installed card to the bottom of the stack as an additional cost to jack out")
                          :effect (req (let [can-redirect? (and (:run @state)
                                                                (= 1 (count (:encounters @state)))
                                                                (not= :success (:phase (:run @state))))]
                                         (when can-redirect?
                                           (redirect-run state side target :approach-ice))
                                         (register-lingering-effect
                                           state side card
                                           {:type :jack-out-additional-cost
                                            :duration :end-of-run
                                            :value [(->c :add-installed-to-bottom-of-deck 1)]})
                                         (wait-for (resolve-ability state side (offer-jack-out) card nil)
                                                   (if (and can-redirect?
                                                            (not (:ended (:end-run @state))))
                                                     (encounter-ends state side eid)
                                                     (effect-completed state side eid)))))})]})

(defcard "Minelayer"
  {:subroutines [{:msg "install a piece of ice from HQ"
                  :async true
                  :choices {:card #(and (ice? %)
                                        (in-hand? %))}
                  :prompt "Choose a piece of ice to install from HQ"
                  :effect (effect (corp-install eid target (zone->name (target-server run)) {:ignore-all-cost true}))}]})

(defcard "Mirāju"
  {:events [{:event :end-of-encounter
             :async true
             :req (req (and (same-card? card (:ice context))
                            (:broken (first (filter :printed (:subroutines (:ice context)))))))
             :msg "make the Runner continue the run on Archives. Mirāju is derezzed"
             :effect (req (when (and (:run @state)
                                     (= 1 (count (:encounters @state)))
                                     (not= :success (:phase (:run @state))))
                            (redirect-run state side "Archives" :approach-ice))
                          (wait-for (resolve-ability state :runner
                                                     (make-eid state eid)
                                                     (offer-jack-out)
                                                     card nil)
                                    (derez state side card)
                                    (effect-completed state side eid)))}]
   :subroutines [{:async true
                  :label "Draw 1 card, then shuffle 1 card from HQ into R&D"
                  :effect (req (wait-for (resolve-ability
                                           state side
                                           {:optional
                                            {:prompt "Draw 1 card?"
                                             :yes-ability {:async true
                                                           :msg "draw 1 card"
                                                           :effect (effect (draw eid 1))}}}
                                           card nil)
                                         (continue-ability
                                           state side
                                           {:prompt "Choose 1 card in HQ to shuffle into R&D"
                                            :choices {:card #(and (in-hand? %)
                                                                  (corp? %))}
                                            :msg "shuffle 1 card in HQ into R&D"
                                            :effect (effect (move target :deck)
                                                            (shuffle! :deck))}
                                           card nil)))}]})

(defcard "Mlinzi"
  (letfn [(net-or-mill [net-dmg mill-cnt]
            {:label (str "Do " net-dmg " net damage")
             :player :runner
             :waiting-prompt true
             :prompt "Choose one"
             :choices (req [(str "Take " net-dmg " net damage")
                            (when (can-pay? state :runner eid card nil [(->c :trash-from-deck mill-cnt)])
                              (capitalize (build-cost-label [(->c :trash-from-deck mill-cnt)])))])
             :async true
             :effect (req (if (= target (str "Take " net-dmg " net damage"))
                            (do (system-msg state :corp
                                            (str "uses " (:title card) " to do "
                                                 net-dmg " net damage"))
                                (damage state :runner eid :net net-dmg {:card card}))
                            (wait-for (pay state :runner (make-eid state eid) card [(->c :trash-from-deck mill-cnt)])
                                      (system-msg state :runner (:msg async-result))
                                      (effect-completed state side eid))))})]
    {:subroutines [(net-or-mill 1 2)
                   (net-or-mill 2 3)
                   (net-or-mill 3 4)]}))

(defcard "Mother Goddess"
  (let [context-mg {:req (req (ice? (:card context)))
                   :effect (effect (update-all-subtypes))}
        mg {:req (req (ice? target))
            :effect (effect (update-all-subtypes))}]
    {:static-abilities [{:type :gain-subtype
                         :req (req (same-card? card target))
                         :value (req (->> (vals (:servers corp))
                                          (mapcat :ices)
                                          (filter #(and (rezzed? %)
                                                        (not (same-card? card %))))
                                          (mapcat :subtypes)))}]
     :subroutines [end-the-run]
     :events [(assoc context-mg :event :rez)
              (assoc context-mg :event :derez)
              (assoc context-mg :event :card-moved)
              (assoc mg :event :ice-subtype-changed)]}))

(defcard "Muckraker"
  {:on-rez take-bad-pub
   :subroutines [(tag-trace 1)
                 (tag-trace 2)
                 (tag-trace 3)
                 end-the-run-if-tagged]})

(defcard "Najja 1.0"
  {:subroutines [end-the-run end-the-run]
   :runner-abilities [(bioroid-break 1 1)]})

(defcard "Nebula"
  (space-ice trash-program-sub))

(defcard "Negotiator"
  {:subroutines [(gain-credits-sub 2)
                 trash-program-sub]
   :runner-abilities [(break-sub [(->c :credit 2)] 1 "All" {:req (req (currently-encountering-card card state))})]})

(defcard "Nerine 2.0"
  (let [sub {:label "Do 1 core damage and Corp may draw 1 card"
             :async true
             :msg "do 1 core damage"
             :effect (req (wait-for (damage state :runner :brain 1 {:card card})
                                    (continue-ability
                                      state side
                                      {:optional
                                       {:prompt "Draw 1 card?"
                                        :autoresolve (get-autoresolve :auto-fire)
                                        :yes-ability {:async true
                                                      :msg "draw 1 card"
                                                      :effect (effect (draw eid 1))}}}
                                      card nil)))}]
    {:subroutines [sub
                   sub]
     :runner-abilities [(bioroid-break 2 2)]
     :abilities [(set-autoresolve :auto-fire "Nerine 2.0 drawing cards")]}))

(defcard "Neural Katana"
  {:subroutines [(do-net-damage 3)]})

(defcard "News Hound"
  (let [gain-sub {:req (req (and (= 1 (count (concat (:current corp) (:current runner))))
                                 (has-subtype? (:card context) "Current")))
                  :msg "make News Hound gain \"[subroutine] End the run\""
                  :effect (effect (reset-variable-subs card 1 end-the-run {:back true}))}
        lose-sub {:req (req (and (zero? (count (concat (:current corp) (:current runner))))
                                 (has-subtype? (:card context) "Current")
                                 (active? (:card context))))
                  :msg "make News Hound lose \"[subroutine] End the run\""
                  :effect (effect (reset-variable-subs card 0 nil))}]
    {:on-rez {:effect (req (when (pos? (count (concat (get-in @state [:corp :current])
                                                      (get-in @state [:runner :current]))))
                             (reset-variable-subs state side card 1 end-the-run {:back true})))}
     :events [(assoc gain-sub :event :play-event)
              (assoc gain-sub :event :play-operation)
              (assoc lose-sub :event :corp-trash)
              (assoc lose-sub :event :runner-trash)
              (assoc lose-sub :event :game-trash)]
     :subroutines [(tag-trace 3)]}))

(defcard "NEXT Bronze"
  {:subroutines [end-the-run]
   :static-abilities [(ice-strength-bonus (req (next-ice-count corp)))]})

(defcard "NEXT Diamond"
  {:rez-cost-bonus (req (- (next-ice-count corp)))
   :subroutines [(do-brain-damage 1)
                 (do-brain-damage 1)
                 {:prompt "Choose a card to trash"
                  :label "Trash 1 installed Runner card"
                  :msg (msg "trash " (:title target))
                  :choices {:card #(and (installed? %)
                                        (runner? %))}
                  :async true
                  :effect (effect (trash eid target {:cause :subroutine}))}]})

(defcard "NEXT Gold"
  (letfn [(trash-programs [cnt state side card eid]
            (if (pos? cnt)
              (wait-for (resolve-ability state side trash-program-sub card nil)
                        (trash-programs (dec cnt) state side card eid))
              (effect-completed state side eid)))]
    {:x-fn (req (next-ice-count corp))
     :subroutines [{:label "Do X net damage"
                    :msg (msg "do " ((get-x-fn) state side eid card targets) " net damage")
                    :effect (effect (damage eid :net ((get-x-fn) state side eid card targets) {:card card}))}
                   {:label "Trash X programs"
                    :async true
                    :effect (req (trash-programs (min (count (filter program? (all-active-installed state :runner)))
                                                      ((get-x-fn) state side eid card targets))
                                                 state side card eid))}]}))

(defcard "NEXT Opal"
  (let [sub {:label "Install a card from HQ, paying all costs"
             :prompt "Choose a card in HQ to install"
             :req (req (some #(corp-installable-type? %) (:hand corp)))
             :choices {:card #(and (corp-installable-type? %)
                                   (in-hand? %))}
             :async true
             :effect (effect (corp-install eid target nil nil))
             :msg (msg (corp-install-msg target))}]
    (next-ice-variable-subs sub)))

(defcard "NEXT Sapphire"
  {:x-fn (req (next-ice-count corp))
   :subroutines [{:label "Draw up to X cards"
                  :prompt "How many cards do you want to draw?"
                  :waiting-prompt true
                  :msg (msg "draw " (quantify target "card"))
                  :choices {:number (get-x-fn)
                            :default (req 1)}
                  :async true
                  :effect (effect (draw eid target))}
                 {:label "Add up to X cards from Archives to HQ"
                  :prompt "Choose cards to add to HQ"
                  :show-discard  true
                  :choices {:card #(and (corp? %)
                                        (in-discard? %))
                            :max (get-x-fn)}
                  :effect (req (doseq [c targets]
                                 (move state side c :hand)))
                  :msg (msg "add "
                            (let [seen (filter :seen targets)
                                  m (count (filter #(not (:seen %)) targets))]
                              (str (enumerate-str (map :title seen))
                                   (when (pos? m)
                                     (str (when-not (empty? seen) " and ")
                                          (quantify m "unseen card")))))
                            " to HQ")}
                 {:label "Shuffle up to X cards from HQ into R&D"
                  :prompt "Choose cards to shuffle into R&D"
                  :choices {:card #(and (corp? %)
                                        (in-hand? %))
                            :max (get-x-fn)}
                  :effect (req (doseq [c targets]
                                 (move state :corp c :deck))
                               (shuffle! state :corp :deck))
                  :cancel-effect (effect (shuffle! :corp :deck))
                  :msg (msg "shuffle " (quantify (count targets) "card") " from HQ into R&D")}]})

(defcard "NEXT Silver"
  (next-ice-variable-subs end-the-run))

(defcard "Nightdancer"
  (let [sub {:label (str "The Runner loses [Click], if able. "
                         "You have an additional [Click] to spend during your next turn")
             :msg (str "force the runner to lose a [Click], if able. "
                       "Corp gains an additional [Click] to spend during [their] next turn")
             :effect (req (lose-clicks state :runner 1)
                          (swap! state update-in [:corp :extra-click-temp] (fnil inc 0)))}]
    {:subroutines [sub
                   sub]}))

(defcard "Oduduwa"
  {:on-encounter
   {:msg "place 1 advancement counter on itself"
    :async true
    :effect (effect (add-prop card :advance-counter 1 {:placed true})
                    (continue-ability
                      (let [card (get-card state card)
                            counters ((get-x-fn) state side eid card targets)]
                        {:optional
                         {:prompt (str "Place " (quantify counters "advancement counter") " on another ice?")
                          :yes-ability
                          {:msg (msg "place " (quantify counters "advancement counter") " on " (card-str state target))
                           :choices {:card ice?
                                     :not-self true}
                           :effect (effect (add-prop target :advance-counter counters {:placed true}))}}})
                      (get-card state card) nil))}
   :x-fn (req (get-counters card :advancement))
   :subroutines [end-the-run
                 end-the-run]})

(defcard "Orion"
  (space-ice trash-program-sub
             (resolve-another-subroutine)
             end-the-run))

(defcard "Otoroshi"
  {:subroutines [{:async true
                  :label "Place 3 advancement tokens on an installed card"
                  :msg "place 3 advancement tokens on an installed card"
                  :prompt "Choose an installed card in the root of a remote server"
                  :req (req (some (complement ice?) (all-installed state :corp)))
                  :choices {:card #(and (corp? %)
                                        (installed? %)
                                        (not (ice? %)))}
                  :effect (req (let [c target
                                     title (card-str state target)]
                                 (add-counter state side c :advancement 3)
                                 (continue-ability
                                   state side
                                   {:player :runner
                                    :async true
                                    :waiting-prompt true
                                    :prompt "Choose one"
                                    :choices [(str "Access " title)
                                              (when (can-pay? state :runner eid card nil [(->c :credit 3)])
                                                "Pay 3 [Credits]")]
                                    :msg (msg "force the Runner to " (decapitalize target))
                                    :effect (req (if (= target "Pay 3 [Credits]")
                                                   (wait-for (pay state :runner (make-eid state eid) card (->c :credit 3))
                                                             (system-msg state :runner (:msg async-result))
                                                             (effect-completed state side eid))
                                                   (access-card state :runner eid c)))}
                                   card nil)))}]})

(defcard "Owl"
  {:subroutines [{:choices {:card #(and (installed? %)
                                        (program? %))}
                  :label "Add installed program to the top of the stack"
                  :msg "add 1 installed program to the top of the stack"
                  :effect (effect (move :runner target :deck {:front true})
                                  (system-msg (str "adds " (:title target) " to the top of the stack")))}]})

(defcard "Pachinko"
  {:subroutines [end-the-run-if-tagged
                 end-the-run-if-tagged]})

(defcard "Palisade"
  {:static-abilities [(ice-strength-bonus (req (not (protecting-a-central? card))) 2)]
   :subroutines [end-the-run]})

(defcard "Paper Wall"
  {:events [{:event :subroutines-broken
             :req (req (and (same-card? card target)
                            (empty? (remove :broken (:subroutines target)))))
             :async true
             :effect (effect (trash :corp eid card {:cause-card card :cause :effect}))}]
   :subroutines [end-the-run]})

(defcard "Peeping Tom"
  (let [sub (end-the-run-unless-runner
              "takes 1 tag"
              "take 1 tag"
              (give-tags 1))]
    {:on-encounter {:prompt "Choose a card type"
                    :choices ["Event" "Hardware" "Program" "Resource"]
                    :async true
                    :effect (req (let [n (count (filter #(is-type? % target) (:hand runner)))]
                                   (system-msg state side
                                               (str "uses " (:title card) " to name " target ", reveal "
                                                    (enumerate-str (map :title (:hand runner)))
                                                    " from the grip, and gain " (quantify n "subroutine")))
                                   (wait-for
                                     (reveal state side (:hand runner))
                                     (gain-variable-subs state side card n sub)
                                     (effect-completed state side eid))))}
     :events [{:event :run-ends
               :effect (effect (reset-variable-subs card 0 nil))}]}))

(defcard "Pharos"
  {:advanceable :always
   :subroutines [(give-tags 1)
                 end-the-run
                 end-the-run]
   :static-abilities [(ice-strength-bonus (req (wonder-sub card 3)) 5)]})

(defcard "Phoneutria"
  {:subroutines [(do-net-damage 1)
                 (do-net-damage 1)]
   :events [{:event :pass-ice
             :req (req (and (same-card? (:ice context) card)
                            (<= 4 (count (:hand runner)))))
             :msg "give the Runner 1 tag"
             :async true
             :effect (effect (gain-tags eid 1))}]})

(defcard "Ping"
  {:on-rez {:req (req (and run this-server))
            :msg "give the Runner 1 tag"
            :async true
            :effect (effect (gain-tags :corp eid 1))}
   :subroutines [end-the-run]})

(defcard "Piranhas"
  {:additional-cost [(->c :tag-or-bad-pub)]
   :subroutines [(maybe-draw-sub 1)
                 (do-net-damage 1)
                 (assoc end-the-run
                        :label "End the run if there are more cards in HQ than in the grip"
                        :req (req (> (count (:hand corp)) (count (:hand runner)))))]})

(defcard "Pop-up Window"
  {:on-encounter (gain-credits-sub 1)
   :subroutines [(end-the-run-unless-runner-pays (->c :credit 1))]})

(defcard "Pulse"
  {:on-rez {:req (req (and run this-server))
            :msg "force the runner to lose [Click]"
            :effect (effect (lose-clicks :runner 1))}
   :subroutines [{:label "Runner loses 1 [Credits] for each rezzed piece of Harmonic ice"
                  :msg (msg "make the runner lose " (harmonic-ice-count corp) " [Credits]")
                  :async true
                  :effect (req (lose-credits state :runner eid (harmonic-ice-count corp)))}
                 (end-the-run-unless-runner-pays (->c :click 1 {:allowed-during-run true}))]})

(defcard "Pup"
  (let [sub {:player :runner
             :async true
             :label "Do 1 net damage unless the Runner pays 1 [Credits]"
             :prompt "Choose one"
             :waiting-prompt true
             :choices (req ["Suffer 1 net damage"
                            (when (can-pay? state :runner eid card nil [(->c :credit 1)])
                              "Pay 1 [Credits]")])
             :effect (req (if (= "Suffer 1 net damage" target)
                            (continue-ability state :corp (do-net-damage 1) card nil)
                            (wait-for (pay state :runner (make-eid state eid) card [(->c :credit 1)])
                                      (system-msg state :runner (:msg async-result))
                                      (effect-completed state side eid))))}]
    {:subroutines [sub
                   sub]}))

(defcard "Quandary"
  {:subroutines [end-the-run]})

(defcard "Quicksand"
  {:on-encounter {:msg "place 1 power counter on itself"
                  :effect (effect (add-counter card :power 1)
                                  (update-all-ice))}
   :subroutines [end-the-run]
   :static-abilities [(ice-strength-bonus (req (get-counters card :power)))]})

(defcard "Rainbow"
  {:subroutines [end-the-run]})

(defcard "Ravana 1.0"
  (let [sub (resolve-another-subroutine
              #(has-subtype? % "Bioroid")
              "Resolve a subroutine on a rezzed bioroid ice")]
    {:subroutines [sub
                   sub]
     :runner-abilities [(bioroid-break 1 1)]}))

(defcard "Red Tape"
  {:subroutines [{:label "Give +3 strength to all ice for the remainder of the run"
                  :msg "give +3 strength to all ice for the remainder of the run"
                  :effect (effect (register-lingering-effect
                                  card
                                  {:type :ice-strength
                                   :duration :end-of-run
                                   :value 3})
                                  (update-all-ice))}]})

(defcard "Resistor"
  {:static-abilities [(ice-strength-bonus (req (count-tags state)))]
   :subroutines [(trace-ability 4 end-the-run)]})

(defcard "Rime"
  {:implementation "Can be rezzed anytime already"
   :on-rez {:effect (effect (update-all-ice))}
   :subroutines [{:label "Runner loses 1 [Credit]"
                  :msg "force the Runner to lose 1 [Credit]"
                  :async true
                  :effect (effect (lose-credits :runner eid 1))}]
   :static-abilities [{:type :ice-strength
                       :req (req (protecting-same-server? card target))
                       :value 1}]})

(defcard "Rototurret"
  {:subroutines [trash-program-sub
                 end-the-run]})

(defcard "Sadaka"
  (let [maybe-draw-effect
        {:optional
         {:player :corp
          :waiting-prompt true
          :prompt "Draw 1 card?"
          :yes-ability
          {:async true
           :effect (effect (draw eid 1))
           :msg "draw 1 card"}}}]
    {:subroutines [{:label "Look at the top 3 cards of R&D"
                    :req (req (not-empty (:deck corp)))
                    :async true
                    :effect
                    (effect (continue-ability
                              (let [top-cards (take 3 (:deck corp))
                                    top-names (map :title top-cards)]
                                {:waiting-prompt true
                                 :prompt (str "The top cards of R&D are (top->bottom): " (enumerate-str top-names))
                                 :choices ["Arrange cards" "Shuffle R&D"]
                                 :async true
                                 :effect
                                 (req (if (= target "Arrange cards")
                                        (wait-for
                                          (resolve-ability state side (reorder-choice :corp top-cards) card nil)
                                          (system-msg state :corp (str "rearranges the top "
                                                                       (quantify (count top-cards) "card")
                                                                       " of R&D"))
                                          (continue-ability state side maybe-draw-effect card nil))
                                        (do
                                          (shuffle! state :corp :deck)
                                          (system-msg state :corp (str "shuffles R&D"))
                                          (continue-ability state side maybe-draw-effect card nil))))})
                              card nil))}
                   {:label "Trash 1 card in HQ"
                    :async true
                    :effect
                    (req (wait-for
                           (resolve-ability
                             state side
                             {:waiting-prompt true
                              :prompt "Choose a card in HQ to trash"
                              :choices (req (cancellable (:hand corp) :sorted))
                              :async true
                              :cancel-effect (effect (system-msg :corp (str "declines to use " (:title card)))
                                                     (effect-completed eid))
                              :effect (req (wait-for
                                             (trash state :corp target {:cause :subroutine})
                                             (system-msg state :corp "trashes a card from HQ")
                                             (continue-ability state side trash-resource-sub card nil)))}
                             card nil)
                           (wait-for (trash state :corp (make-eid state eid) card {:cause-card card})
                                     (system-msg state :corp (str "uses " (:title card) " to trash itself"))
                                     (encounter-ends state side eid))))}]}))

(defcard "Sagittarius"
  (constellation-ice trash-program-sub))

(defcard "Saisentan"
  (let [sub {:label "Do 1 net damage"
             :async true
             :msg "do 1 net damage"
             :effect (req (damage state side eid :net 1 {:card card :cause :subroutine}))}]
    {:on-encounter {:waiting-prompt true
                    :prompt "Choose a card type"
                    :choices ["Event" "Hardware" "Program" "Resource"]
                    :msg (msg "choose the card type " target)
                    :effect (effect (update! (assoc card :card-target target)))}
     :events [{:event :damage
               :req (req (and (= (:damage-type context) :net)
                              (= (:cause context) :subroutine)
                              (same-card? (:card context) card)))
               :async true
               :effect (req (let [trashed-cards (:cards-trashed context)
                                  chosen-type (:card-target card)
                                  matching-type (filter #(= (:type %) chosen-type) trashed-cards)]
                              ;; theoretically, if this ice is made to deal 2 or more net damage from
                              ;; a sub (who knows what the future holds), we can be dealing out more
                              ;; than one additional damage...
                              (if-not (seq matching-type)
                                (effect-completed state side eid)
                                (letfn [(resolve-extra-damage [x]
                                          (system-msg state :corp (str "uses " (:title card) " to deal 1 additional net damage" (when (> x 1) (str " (" (dec x) " remaining)"))))
                                          (if-not (> x 1)
                                            (damage state side eid :net 1 {:card card})
                                            (wait-for (damage side :net 1 {:card card})
                                                      (resolve-extra-damage (dec x)))))]
                                  (resolve-extra-damage (count matching-type))))))}
              {:event :end-of-encounter
               :req (req (:card-target card))
               :effect (effect (update! (dissoc card :card-target)))}]
     :subroutines [sub
                   sub
                   sub]}))

(defcard "Salvage"
  (zero-to-hero (tag-trace 2)))

(defcard "Sand Storm"
  {:subroutines [{:async true
                  :label "Move this ice and the run to another server"
                  :prompt "Choose another server and redirect the run to its outermost position"
                  :choices (req (remove #{(zone->name (:server (:run @state)))} (cancellable servers)))
                  :msg (msg "move itself and the run on " target " and trash itself")
                  :effect (req (let [moved-ice (move state side card (conj (server->zone state target) :ices))]
                                 (redirect-run state side target)
                                 (wait-for (trash state side (make-eid state eid) moved-ice {:unpreventable true :cause :subroutine})
                                           (encounter-ends state side eid))))}]})

(defcard "Sandman"
  {:subroutines [add-runner-card-to-grip
                 add-runner-card-to-grip]})

(defcard "Sandstone"
  {:subroutines [end-the-run]
   :static-abilities [(ice-strength-bonus (req (- (get-counters card :virus))))]
   :on-encounter {:msg "place 1 virus counter on itself"
                  :effect (effect (add-counter card :virus 1)
                                  (update-ice-strength (get-card state card)))}})

(defcard "Sapper"
  {:flags {:rd-reveal (req true)}
   :subroutines [trash-program-sub]
   :on-access {:async true
               :req (req (not (in-discard? card)))
               :msg "force the Runner to encounter Sapper"
               :effect (req (force-ice-encounter state side eid card))}})

(defcard "Searchlight"
  (let [sub {:label "Trace X - Give the Runner 1 tag"
             :trace {:base (get-x-fn)
                     :label "Give the Runner 1 tag"
                     :successful (give-tags 1)}}]
    {:x-fn (req (get-counters card :advancement))
     :advanceable :always
     :subroutines [sub
                   sub]}))

(defcard "Seidr Adaptive Barrier"
  {:static-abilities [(ice-strength-bonus (req (count (:ices (card->server state card)))))]
   :subroutines [end-the-run]})

(defcard "Self-Adapting Code Wall"
  {:static-abilities [{:type :cannot-lower-strength
                       :req (req (same-card? card (:ice context)))
                       :value true}]
   :subroutines [end-the-run]})


(defcard "Sensei"
  {:subroutines [{:label "Give encountered ice \"End the run\""
                  :msg "give encountered ice \"[Subroutine] End the run\" after all its other subroutines for the remainder of the run"
                  :effect (effect
                            (register-events
                              card
                              [{:event :encounter-ice
                                :duration :end-of-run
                                :req (req (not (same-card? card (:ice context))))
                                :msg (msg "give " (:title (:ice context)) "\"[Subroutine] End the run\" after all its other subroutines")
                                :effect (effect (add-sub! (:ice context) end-the-run (:cid card) {:back true}))}
                               {:event :end-of-encounter
                                :duration :end-of-run
                                :effect (effect (remove-sub! (:ice context) #(= (:cid card) (:from-cid %))))}]))}]})

(defcard "Seraph"
  (let [encounter-ab
        {:prompt "Choose one"
         :player :runner
         :waiting-prompt true
         :choices (req ["Lose 3 [Credits]"
                        (when (>= (count (:hand runner)) 2)
                          "Suffer 2 net damage")
                        (when-not (forced-to-avoid-tags? state side)
                          "Take 1 tag")])
         :msg (msg "force the Runner to " (decapitalize target) " on encountering it")
         :async true
         :effect (req (cond
                        (= "Lose 3 [Credits]" target)
                          (lose-credits state :runner eid 3)
                        (= "Suffer 2 net damage" target)
                          (pay state :runner eid card [(->c :net 2)])
                        (= "Take 1 tag" target)
                          (gain-tags state :runner eid 1 {:unpreventable true})))}]
    {:on-encounter encounter-ab
     :subroutines [(runner-loses-credits 3)
                   (do-net-damage 2)
                   (give-tags 1)]}))

(defcard "Shadow"
  (wall-ice [(gain-credits-sub 2)
             (tag-trace 3)]))

(defcard "Sherlock 1.0"
  (let [sub (trace-ability 4 {:choices {:card #(and (installed? %)
                                                    (program? %))}
                              :label "Add 1 installed program to the top of the stack"
                              :msg (msg "add " (:title target) " to the top of the stack")
                              :effect (effect (move :runner target :deck {:front true}))})]
    {:subroutines [sub
                   sub]
     :runner-abilities [(bioroid-break 1 1)]}))

(defcard "Sherlock 2.0"
  (let [sub (trace-ability 4 {:choices {:card #(and (installed? %)
                                                    (program? %))}
                              :label "Add 1 installed program to the bottom of the stack"
                              :msg (msg "add " (:title target) " to the bottom of the stack")
                              :effect (effect (move :runner target :deck))})]
    {:subroutines [sub
                   sub
                   (give-tags 1)]
     :runner-abilities [(bioroid-break 2 2)]}))

(defcard "Shinobi"
  {:on-rez take-bad-pub
   :subroutines [(trace-ability 1 (do-net-damage 1))
                 (trace-ability 2 (do-net-damage 2))
                 (trace-ability 3 {:label "Do 3 net damage and end the run"
                                   :msg "do 3 net damage and end the run"
                                   :effect (req (wait-for (damage state side :net 3 {:card card})
                                                          (end-run state side eid card)))})]})

(defcard "Shiro"
  {:subroutines [{:label "Rearrange the top 3 cards of R&D"
                  :msg "rearrange the top 3 cards of R&D"
                  :async true
                  :waiting-prompt true
                  :effect (effect (continue-ability
                                    (let [from (take 3 (:deck corp))]
                                      (when (pos? (count from))
                                        (reorder-choice :corp :runner from '() (count from) from)))
                                    card nil))}
                 {:label "The runner breaches R&D unless the corp pays 1 [Credit]"
                  :optional
                  {:prompt "Pay 1 [Credits] to keep the Runner from breaching R&D?"
                   :yes-ability {:cost [(->c :credit 1)]
                                 :msg "keep the Runner from breaching R&D"}
                   :no-ability {:async true
                                :msg "make the Runner breach R&D"
                                :effect (effect (breach-server :runner eid [:rd] {:no-root true}))}}}]})

(defcard "Slot Machine"
  (letfn [(top-3 [state] (take 3 (get-in @state [:runner :deck])))
          (effect-type [card] (keyword (str "slot-machine-top-3-" (:cid card))))
          (name-builder [card] (str (:title card) " (" (:type card) ")"))
          (top-3-names [cards] (map name-builder cards))
          (top-3-types [state card et]
            (->> (get-effects state :corp et card)
                 first
                 (keep :type)
                 (into #{})
                 count))
          (ability []
            {:label "Encounter ability (manual)"
             :async true
             :effect (req (move state :runner (first (:deck runner)) :deck)
                          (let [t3 (top-3 state)
                                effect-type (effect-type card)]
                            (register-lingering-effect
                              state side card
                              {:type effect-type
                               :duration :end-of-encounter
                               :value t3})
                            (system-msg state side
                                        (str "uses " (:title card) " to put the top card of the stack to the bottom,"
                                             " then reveal "
                                             (enumerate-str (top-3-names t3))
                                             " from the top of the stack"))
                            (reveal state side eid t3)))})]
    {:on-encounter (ability)
     :abilities [(ability)]
     :subroutines [{:label "Runner loses 3 [Credits]"
                    :msg "force the Runner to lose 3 [Credits]"
                    :async true
                    :effect (effect (lose-credits :runner eid 3))}
                   {:label "Gain 3 [Credits]"
                    :async true
                    :effect (req (let [et (effect-type card)
                                       unique-types (top-3-types state card et)]
                                   ;; When there are 3 cards in the deck, sub needs 2 or fewer unique types
                                   ;; When there are 2 cards in the deck, sub needs 1 unique type
                                   (if (or (and (<= unique-types 2)
                                                  (= 3 (count (first (get-effects state :corp et card)))))
                                             (and (= unique-types 1)
                                                  (= 2 (count (first (get-effects state :corp et card))))))
                                     (do (system-msg state :corp (str "uses " (:title card) " to gain 3 [Credits]"))
                                         (gain-credits state :corp eid 3))
                                     (effect-completed state side eid))))}
                   {:label "Place 3 advancement tokens"
                    :async true
                    :effect (effect
                              (continue-ability
                                (let [et (effect-type card)
                                      unique-types (top-3-types state card et)]
                                  (when (and (= 3 (count (first (get-effects state :corp et card))))
                                             (= 1 unique-types))
                                    {:choices {:card installed?}
                                     :prompt "Choose an installed card"
                                     :msg (msg "place 3 advancement tokens on "
                                               (card-str state target))
                                     :effect (effect (add-prop target :advance-counter 3 {:placed true}))}))
                                card nil))}]}))

(defcard "Snoop"
  {:on-encounter {:msg (msg "reveal "
                            (enumerate-str (map :title (:hand runner)))
                            " from the grip")
                  :async true
                  :effect (effect (reveal eid (:hand runner)))}
   :abilities [{:async true
                :req (req (pos? (get-counters card :power)))
                :cost [(->c :power 1)]
                :label "Reveal all cards in the grip and trash 1 card"
                :msg (msg "reveal all cards in the grip and trash " (:title target))
                :choices (req (cancellable (:hand runner) :sorted))
                :prompt "Choose a card to trash"
                :effect (effect (reveal (:hand runner))
                                (trash eid target {:cause :subroutine}))}]
   :subroutines [(trace-ability 3 add-power-counter)]})

(defcard "Snowflake"
  {:subroutines [(do-psi end-the-run)]})

(defcard "Sorocaban Blade"
  {:events [{:event :corp-trash
             :silent (req true)
             :once-per-instance true
             :req (req (and
                         (get-current-encounter state)
                         (some #(and (runner? %) (installed? %)) (map :card targets))))
             :effect (req (update! state side (assoc-in card [:special :sorocaban-blade] true)))}
            {:event :end-of-encounter
             :silent (req true)
             :req (req true)
             :effect
             (req (update! state side (dissoc-in card [:special :sorocaban-blade])))}]
   :subroutines [trash-resource-sub
                 (assoc trash-hardware-sub
                        :req (req (not (get-in card [:special :sorocaban-blade]))))
                 (assoc trash-program-sub
                        :req (req (not (get-in card [:special :sorocaban-blade]))))]})

(defcard "Special Offer"
  {:subroutines [{:label "Gain 5 [Credits] and trash this ice"
                  :msg "gain 5 [Credits] and trash itself"
                  :async true
                  :effect (req (wait-for (gain-credits state :corp 5)
                                         (trash state :corp (make-eid state eid) card {:cause :subroutine})
                                         (encounter-ends state side eid)))}]})

(defcard "Spiderweb"
  {:subroutines [end-the-run
                 end-the-run
                 end-the-run]})

(defcard "Starlit Knight"
  (let [sub-count (fn [state]
                    (sum-tag-effects state))
        sub end-the-run]
    {:on-encounter {:req (req (threat-level 4 state))
                    :effect (effect (gain-variable-subs card (sub-count state) sub {:variable true :front false :end true}))}
     :events [{:event :run-ends
               :effect (effect (reset-variable-subs card 0 nil))}]
     :subroutines [(give-tags 1)
                   (give-tags 1)]}))

(defcard "Stavka"
  {:on-rez {:optional {:prompt "Trash another card to give Stavka +5 strength?"
                       :waiting-prompt true
                       :req (req (can-pay? state side (assoc eid :source card :source-type :ability)
                                           card nil
                                           [(->c :trash-other-installed 1)]))
                       :yes-ability {:prompt "Choose another installed card to trash"
                                     :cost [(->c :trash-other-installed 1)]
                                     :msg "give itself +5 strength for the remainder of the run"
                                     :effect (req (when (:run @state)
                                                       (register-lingering-effect
                                                         state side card
                                                         {:type :ice-strength
                                                          :duration :end-of-run
                                                          :req (req (same-card? target card))
                                                          :value 5})
                                                       (update-ice-strength state side card)))}}}
   :subroutines [trash-program-sub
                 trash-program-sub]})

(defcard "Surveyor"
  {:static-abilities [(ice-strength-bonus (get-x-fn))]
   :x-fn (req (* 2 (count (:ices (card->server state card)))))
   :subroutines [{:label "Trace X - Give the Runner 2 tags"
                  :trace {:base (get-x-fn)
                          :label "Give the Runner 2 tags"
                          :successful (give-tags 2)}}
                 {:label "Trace X - End the run"
                  :trace {:base (get-x-fn)
                          :label "End the run"
                          :successful end-the-run}}]})

(defcard "Susanoo-no-Mikoto"
  {:subroutines [{:async true
                  :req (req (not= (:server run) [:discard]))
                  :msg "make the Runner continue the run on Archives"
                  :effect (req (if run
                                 (do (prevent-jack-out state side)
                                     (register-events
                                      state side card
                                      [{:event :encounter-ice
                                        :duration :end-of-run
                                        :unregister-once-resolved true
                                        :effect (req (swap! state update :run dissoc :cannot-jack-out))}])
                                     (if (and (= 1 (count (:encounters @state)))
                                              (not= :success (:phase run)))
                                       (do (redirect-run state side "Archives" :approach-ice)
                                           (encounter-ends state side eid))
                                       (effect-completed state side eid)))
                                 (effect-completed state side eid)))}]})

(defcard "Swarm"
  (let [sub {:player :runner
             :async true
             :label "Trash a program"
             :prompt "Choose one"
             :waiting-prompt true
             :choices (req ["The Corp trashes a program"
                            (when (can-pay? state :runner eid card nil [(->c :credit 3)])
                              "Pay 3 [Credits]")])
             :effect (req (if (= "Pay 3 [Credits]" target)
                            (wait-for (pay state :runner (make-eid state eid) card [(->c :credit 3)])
                                      (system-msg state :runner (:msg async-result))
                                      (effect-completed state side eid))
                            (continue-ability state :corp trash-program-sub card nil)))}
        ability {:req (req (same-card? card (:card context)))
                 :effect (effect (reset-variable-subs card (get-counters card :advancement) sub))}]
    {:advanceable :always
     :on-rez take-bad-pub
     :events [(assoc ability :event :advance)
              (assoc ability :event :subroutines-should-update :req nil)
              (assoc ability :event :advancement-placed)
              (assoc ability :event :rez)]}))

(defcard "Swordsman"
  {:static-abilities [{:type :cannot-break-subs-on-ice
                       :req (req (and (same-card? card (:ice context))
                                      (has-subtype? (:icebreaker context) "AI")))
                       :value true}]
   :subroutines [{:async true
                  :prompt "Choose an AI program to trash"
                  :msg (msg "trash " (:title target))
                  :label "Trash an AI program"
                  :choices {:card #(and (installed? %)
                                        (program? %)
                                        (has-subtype? % "AI"))}
                  :effect (effect (trash eid target {:cause :subroutine}))}
                 (do-net-damage 1)]})

(defcard "SYNC BRE"
  {:subroutines [(tag-trace 4)
                 (trace-ability 2 {:label "Runner reduces cards accessed by 1 for this run"
                                   :async true
                                   :msg "reduce cards accessed for this run by 1"
                                   :effect (effect (access-bonus :total -1))})]})

(defcard "Tapestry"
  {:subroutines [runner-loses-click
                 {:label "Draw 1 card"
                  :optional
                  {:prompt "Draw 1 card?"
                   :autoresolve (get-autoresolve :auto-fire)
                   :yes-ability {:async true
                                 :msg "draw 1 card"
                                 :effect (effect (draw eid 1))}}}
                 {:req (req (pos? (count (:hand corp))))
                  :prompt "Choose a card in HQ to move to the top of R&D"
                  :choices {:card #(and (in-hand? %)
                                        (corp? %))}
                  :msg "add 1 card in HQ to the top of R&D"
                  :effect (effect (move target :deck {:front true}))}]
  :abilities [(set-autoresolve :auto-fire "Tapestry drawing cards")]})

(defcard "Tatu-Bola"
  {:events [{:event :pass-ice
             :interactive (req run)
             :req (req (same-card? (:ice context) card))
             :async true
             :effect (req (continue-ability
                            state side
                            (if (seq (filter ice? (:hand corp)))
                              {:optional
                               {:prompt (msg "Gain 4 [Credits] and swap " (card-str state card) " with a piece of ice in HQ?")
                                :waiting-prompt true
                                :no-ability {:effect (effect (system-msg :corp (str "declines to use " (:title card))))}
                                :yes-ability {:prompt "Choose a piece of ice"
                                              :waiting-prompt true
                                              :choices (req (filter ice? (:hand corp)))
                                              :async true
                                              :effect (req (wait-for (swap-cards-async state side (make-eid state eid) target current-ice)
                                                                     (gain-credits state :corp eid 4)))
                                              :msg (msg "swap " (card-str state card)
                                                        " with a piece of ice from HQ and gain 4 [Credits]")}}}
                              {:prompt "You have no ice"
                               :choices ["OK"]
                               :waiting-prompt true
                               :effect (effect (system-msg :runner (str "declines to use " (:title card) " to install a card")))})
                            card nil))}]
   :subroutines [end-the-run]})

(defcard "Taurus"
  (constellation-ice trash-hardware-sub))

(defcard "Thimblerig"
  (let [ability {:interactive (req run)
                 :optional
                 {:req (req (and (<= 2 (count (filter ice? (all-installed state :corp))))
                                 (if run (same-card? (:ice context) card) true)))
                  :prompt (msg "Swap " (card-str state card) " with another ice?")
                  :yes-ability {:prompt "Choose a piece of ice to swap Thimblerig with"
                                :choices {:card ice?
                                          :not-self true}
                                :effect (effect (swap-ice card target))
                                :msg (msg "swap " (card-str state card)
                                          " with " (card-str state target))}}}]
    {:events [(assoc ability :event :pass-ice)
              (assoc ability :event :corp-turn-begins)]
     :subroutines [end-the-run]}))

(defcard "Thoth"
  {:on-encounter (give-tags 1)
   :subroutines [(trace-ability 4 {:label "Do 1 net damage for each Runner tag"
                                   :async true
                                   :msg (msg "do " (count-tags state) " net damage")
                                   :effect (effect (damage eid :net (count-tags state) {:card card}))})
                 (trace-ability 4 {:label "Runner loses 1 [Credits] for each tag"
                                   :async true
                                   :msg (msg "force the Runner to lose " (count-tags state) " [Credits]")
                                   :effect (effect (lose-credits :runner eid (count-tags state)))})]})

(defcard "Tithe"
  {:subroutines [(do-net-damage 1)
                 (gain-credits-sub 1)]})

(defcard "Tithonium"
  {:alternative-cost [(->c :forfeit)]
   :cannot-host true
   :subroutines [trash-program-sub
                 trash-program-sub
                 {:label "Trash a resource and end the run"
                  :async true
                  :effect (req (wait-for
                                 (resolve-ability
                                   state side
                                   {:req (req (pos? (count (filter resource? (all-installed state :runner)))))
                                    :async true
                                    :choices {:all true
                                              :card #(and (installed? %)
                                                          (resource? %))}
                                    :effect (req (wait-for (trash state side target {:cause :subroutine})
                                                           (complete-with-result state side eid target)))}
                                   card nil)
                                 (system-msg state side
                                             (str "uses " (:title card) " to "
                                                  (if async-result
                                                    (str "trash " (:title async-result)
                                                         " and ends the run")
                                                    "end the run")))
                                 (end-run state side eid card)))}]})

(defcard "TL;DR"
  {:subroutines
   [{:label "Duplicate each subroutine on a piece of ice"
     :effect (effect
               (register-events
                 card
                 [{:event :encounter-ice
                   :duration :end-of-run
                   :unregister-once-resolved true
                   :msg (msg "duplicate each subroutine on " (:title (:ice context)))
                   :effect
                   (req (let [curr-subs (map #(assoc % :from-cid (:cid (:ice context))) (:subroutines (:ice context)))
                              tldr-subs (map #(assoc % :from-cid (:cid card)) curr-subs)
                              new-subs (->> (interleave curr-subs tldr-subs)
                                            (reduce
                                              (fn [ice sub] (add-sub ice sub (:from-cid sub) nil))
                                              (assoc (:ice context) :subroutines []))
                                            :subroutines
                                            (into []))
                              new-card (assoc (:ice context) :subroutines new-subs)]
                          (update! state :corp new-card)
                          (register-events
                            state side card
                            (let [cid (:cid card)]
                              [{:event :end-of-encounter
                                :duration :end-of-encounter
                                :unregister-once-resolved true
                                :req (req (get-card state new-card))
                                :effect (effect (remove-subs! (get-card state new-card) #(= cid (:from-cid %))))}]))))}]))}]})

(defcard "TMI"
  {:on-rez {:trace {:base 2
                    :msg "keep TMI rezzed"
                    :label "Keep TMI rezzed"
                    :unsuccessful {:effect (effect (derez card))}}}
   :subroutines [end-the-run]})

(defcard "Tollbooth"
  {:on-encounter {:async true
                  :effect (req (wait-for (pay state :runner (make-eid state eid) card [(->c :credit 3)])
                                         (if (:cost-paid async-result)
                                           (do (system-msg state :runner (str (:msg async-result) " on encountering " (:title card)))
                                               (effect-completed state side eid))
                                           (do (system-msg state :corp (str "uses " (:title card) " to end the run"))
                                               (end-run state :corp eid card)))))}
   :subroutines [end-the-run]})

(defcard "Tour Guide"
  (variable-subs-ice
    (fn [card] (asset? card))
    (fn [state] (count (filter asset? (all-active-installed state :corp))))
    end-the-run))

(defcard "Trebuchet"
  {:on-rez take-bad-pub
   :subroutines [{:prompt "Choose a card to trash"
                  :label "Trash 1 installed Runner card"
                  :msg (msg "trash " (:title target))
                  :choices {:card #(and (installed? %)
                                        (runner? %))}
                  :async true
                  :effect (req (trash state side eid target {:cause :subroutine}))}
                 (trace-ability 6 cannot-steal-or-trash-sub)]})

(defcard "Tree Line"
  {:static-abilities [(ice-strength-bonus (req (get-counters card :advancement)))]
   :subroutines [{:msg "gain 1 [Credits] and end the run"
                  :async true
                  :effect (req (wait-for (gain-credits state side 1)
                                         (end-run state side eid card)))}]
   :advanceable :always
   :expend {:req (req (some ice? (all-installed state :corp)))
            :cost [(->c :credit 1)]
            :choices {:req (req (and (ice? target)
                                     (installed? target)))}
            :msg (msg "place 3 advancement counters on " (card-str state target))
            :effect (effect (add-counter target :advancement 3 {:placed true}))}})

(defcard "Tribunal"
  {:subroutines [runner-trash-installed-sub
                 runner-trash-installed-sub
                 runner-trash-installed-sub]})

(defcard "Tributary"
  {:subroutines [{:label "Draw 1 card and install a piece of ice from HQ protecting another server"
                  :async true
                  :effect (req (wait-for
                                 (maybe-draw state side card 1)
                                 (continue-ability
                                   state side
                                   {:choices {:card #(and (ice? %)
                                                          (in-hand? %))}
                                    :prompt "Choose a piece of ice to install"
                                    :async true
                                    :effect (req (let [this (zone->name (second (get-zone card)))
                                                       nice target]
                                                   (continue-ability state side
                                                                     {:prompt (str "Choose a location to install " (:title target))
                                                                      :choices (req (remove #(= this %) (corp-install-list state nice)))
                                                                      :async true
                                                                      :effect (effect (corp-install eid nice target {:ignore-install-cost true}))}
                                                                     card nil)))}
                                   card nil)))}
                 {:label "Give +2 strength to each piece of ice for the remainder of the run"
                  :msg "give +2 strength to each piece of ice for the remainder of the run"
                  :effect (effect (register-lingering-effect
                                    card
                                    {:type :ice-strength
                                     :duration :end-of-run
                                     :value 2})
                                  (update-all-ice))}]
   :events [{:event :run
             :req (req (and (first-event? state side :run)))
             :effect (req (let [target-server (:server run)]
                            (continue-ability
                              state side
                              {:optional
                               {:prompt (msg "Move " (:title card) " to the outermost position of " (zone->name target-server) "?")
                                :yes-ability {:once :per-turn
                                              :msg (msg "move itself to the outermost position of " (zone->name target-server))
                                              :effect (req (let [moved (move state side
                                                                             (get-card state card)
                                                                             (conj [:servers (first target-server)] :ices))]
                                                                 (redirect-run state side target-server)
                                                                 ;;ugly hack - TODO: figure out why the event gets disabled after the card moves!
                                                                 ;;- maybe this should be inserted into move? -nbkelly, Jan '24
                                                                 (unregister-events state side moved)
                                                                 (register-default-events state side moved)
                                                                 (effect-completed state side eid)))}}}
                              card nil)))}]})

(defcard "Troll"
  {:on-encounter
   (trace-ability 2 {:msg (msg (if (= target "Spend [Click]")
                                 (str "force the runner to " (decapitalize target))
                                 (decapitalize target)))
                     :player :runner
                     :prompt "Choose one"
                     :waiting-prompt true
                     :choices (req [(when (can-pay? state :runner eid card nil [(->c :click 1 {:allowed-during-run true})])
                                      "Spend [Click]")
                                    "End the run"])
                     :async true
                     :effect (req (if (and (= target "Spend [Click]")
                                           (can-pay? state :runner eid card nil [(->c :click 1 {:allowed-during-run true})]))
                                    (wait-for (pay state side (make-eid state eid) card (->c :click 1 {:allowed-during-run true}))
                                              (system-msg state side (:msg async-result))
                                              (effect-completed state :runner eid))
                                    (end-run state :corp eid card)))})})

(defcard "Tsurugi"
  {:subroutines [(end-the-run-unless-corp-pays [(->c :credit 1)])
                 (do-net-damage 1)
                 (do-net-damage 1)
                 (do-net-damage 1)]})

(defcard "Turing"
  {:static-abilities [{:type :cannot-break-subs-on-ice
                       :req (req (and (same-card? card (:ice context))
                                      (has-subtype? (:icebreaker context) "AI")))
                       :value true}
                      (ice-strength-bonus (req (not (protecting-a-central? card))) 3)]
   :subroutines [(end-the-run-unless-runner-pays (->c :click 3 {:allowed-during-run true}))]})

(defcard "Turnpike"
  {:on-encounter {:msg "force the Runner to lose 1 [Credits]"
                  :async true
                  :effect (effect (lose-credits :runner eid 1))}
   :subroutines [(tag-trace 5)]})

(defcard "Týr"
  {:subroutines [(do-brain-damage 2)
                 (combine-abilities trash-installed-sub (gain-credits-sub 3))
                 end-the-run]
   :runner-abilities [(bioroid-break 1 1 {:additional-ability {:effect (req (swap! state update-in [:corp :extra-click-temp] (fnil inc 0)))}})]})

(defcard "Tyrant"
  (zero-to-hero end-the-run))

(defcard "Universal Connectivity Fee"
  {:subroutines [{:label "Force the Runner to lose credits"
                  :msg (msg "force the Runner to lose " (if tagged "all credits and trash itself" "1 [Credits]"))
                  :async true
                  :effect (req (if tagged
                                 (wait-for (lose-credits state :runner (make-eid state eid) :all)
                                           (trash state :corp (make-eid state eid) card {:cause :subroutine})
                                           (encounter-ends state side eid))
                                 (lose-credits state :runner eid 1)))}]})

(defcard "Unsmiling Tsarevna"
  (let [on-rez-ability {:async true
                        :msg (msg "let the Runner gain 2 [Credits] to"
                                  " prevent them from breaking more than 1 subroutine"
                                  " on this ice per encounter for the remainder of this run")
                        :effect
                        (req (wait-for (gain-credits state :runner 2)
                                       (register-lingering-effect
                                         state side card
                                         {:type :cannot-break-subs-on-ice
                                          :duration :end-of-run
                                          :req (req (same-card? card (:ice context)))
                                          :value (req (any-subs-broken? (:ice context)))})
                                       (effect-completed state side eid)))}]
    {:subroutines [(give-tags 1)
                   (do-net-damage 2)
                   (maybe-draw-sub 2)]
     :on-rez {:optional
              {:prompt "Let the Runner gain 2 [Credits]?"
               :waiting-prompt true
               :req (req (and run this-server))
               :yes-ability {:async true
                             :effect (effect (continue-ability on-rez-ability card nil))}
               :no-ability
               {:effect (effect (system-msg :corp (str "declines to use " (:title card))))}}}}))

(defcard "Upayoga"
  {:subroutines [(do-psi {:label "Make the Runner lose 2 [Credits]"
                          :msg "make the Runner lose 2 [Credits]"
                          :async true
                          :effect (effect (lose-credits :runner eid 2))})
                 (resolve-another-subroutine
                   #(has-subtype? % "Psi")
                   "Resolve a subroutine on a rezzed psi ice")]})

(defcard "Uroboros"
  {:subroutines [(trace-ability 4 {:label "Prevent the Runner from making another run"
                                   :msg "prevent the Runner from making another run"
                                   :effect (effect (register-turn-flag! card :can-run nil))})
                 (trace-ability 4 end-the-run)]})

(defcard "Valentão"
  {:additional-cost [(->c :tag-or-bad-pub)]
   :subroutines [(gain-credits-sub 2)
                 (runner-loses-credits 2)
                 (assoc end-the-run
                        :label "End the run if you have more credits than the Runner"
                        :req (req (> (:credit corp) (:credit runner))))]})

(defcard "Vampyronassa"
  {:subroutines [(runner-loses-credits 2)
                 (gain-credits-sub 2)
                 (do-net-damage 2)
                 (draw-up-to-sub 2)]})

(defcard "Vanilla"
  {:subroutines [end-the-run]})

(defcard "Vasilisa"
  {:on-encounter
   {:optional {:prompt "Pay 1 [Credits] to place 1 advancement counter on a card that can be advanced?"
               :waiting-prompt true
               :req (req (and (can-pay? state side eid card nil [(->c :credit 1)])
                              (some #(or (not (rezzed? %))
                                         (can-be-advanced? state %))
                                    (all-installed state :corp))))
               :yes-ability {:cost [(->c :credit 1)]
                             :choices {:req (req (can-be-advanced? state target))}
                             :prompt "Choose a card that can be advanced"
                             :msg (msg "place 1 advancement counter on " (card-str state target))
                             :effect (effect (add-prop target :advance-counter 1 {:placed true}))
                             :cancel-effect (effect (system-msg :corp (str "declines to use " (:title card)))
                                                    (effect-completed eid))}
               :no-ability {:effect (effect (system-msg :corp (str "declines to use " (:title card))))}}}
   :subroutines [(give-tags 1)]})

(defcard "Veritas"
  {:subroutines [{:label "Corp gains 2 [Credits]"
                  :msg "gain 2 [Credits]"
                  :async true
                  :effect (effect (gain-credits :corp eid 2))}
                 {:label "Runner loses 2 [Credits]"
                  :msg "force the Runner to lose 2 [Credits]"
                  :async true
                  :effect (effect (lose-credits :runner eid 2))}
                 (trace-ability 2 (give-tags 1))]})

(defcard "Vikram 1.0"
  {:implementation "Program prevention is not implemented"
   :subroutines [{:msg "prevent the Runner from using programs for the remainder of this run"}
                 (trace-ability 4 (do-brain-damage 1))
                 (trace-ability 4 (do-brain-damage 1))]
   :runner-abilities [(bioroid-break 1 1)]})

(defcard "Viktor 1.0"
  {:subroutines [(do-brain-damage 1)
                 end-the-run]
   :runner-abilities [(bioroid-break 1 1)]})

(defcard "Viktor 2.0"
  {:abilities [(power-counter-ability (do-brain-damage 1))]
   :subroutines [(trace-ability 2 add-power-counter)
                 end-the-run]
   :runner-abilities [(bioroid-break 2 2)]})

(defcard "Viper"
  {:subroutines [(trace-ability 3 runner-loses-click)
                 (trace-ability 3 end-the-run)]})

(defcard "Virgo"
  (constellation-ice (give-tags 1)))

(defcard "Virtual Service Agent"
  {:subroutines [(runner-loses-credits 1)]
   :implementation "Might be incorrect if decoder is uninstalled"
   :events [{:event :end-of-encounter
             :req (req (all-subroutines-not-broken-by state side card "Decoder" context))
             :msg "give the Runner 1 tag"
             :async true
             :effect (effect (gain-tags eid 1))}]})

(defcard "Waiver"
  {:subroutines [(trace-ability
                   5 {:label "Reveal the grip and trash cards"
                      :msg (msg "reveal "
                                (enumerate-str (map :title (:hand runner)))
                                " from the grip")
                      :async true
                      :effect (req (wait-for
                                     (reveal state side (:hand runner))
                                     (let [delta (- target (second targets))
                                           cards (filter #(<= (:cost %) delta) (:hand runner))]
                                       (system-msg state side (str "uses " (:title card) " to trash "
                                                                   (enumerate-str (map :title cards))))
                                       (trash-cards state side eid cards {:cause :subroutine}))))})]})

(defcard "Wall of Static"
  {:subroutines [end-the-run]})

(defcard "Wall of Thorns"
  {:subroutines [(do-net-damage 2)
                 end-the-run]})

(defcard "Watchtower"
  {:subroutines [{:label "Search R&D and add 1 card to HQ"
                  :prompt "Choose a card to add to HQ"
                  :msg "add a card from R&D to HQ"
                  :choices (req (cancellable (:deck corp) :sorted))
                  :cancel-effect (effect (system-msg "cancels the effect of Watchtower"))
                  :effect (effect (shuffle! :deck)
                                  (move target :hand))}]})

(defcard "Wave"
  {:on-rez
   {:optional {:prompt "Search R&D for a piece of ice?"
               :req (req (and run this-server))
               :yes-ability {:prompt "Choose a piece of ice"
                             :async true
                             :msg (msg "reveal " (:title target) " from R&D and add it to HQ")
                             :choices (req (cancellable (filter #(ice? %) (:deck corp)) :sorted))
                             :cancel-effect (effect (system-msg (str "uses " (:title card) " to shuffle R&D"))
                                                    (shuffle! :deck))
                             :effect (req (wait-for (reveal state side target)
                                                    (shuffle! state side :deck)
                                                    (move state side target :hand)
                                                    (effect-completed state side eid)))}
               :no-ability {:effect (effect (system-msg :corp (str "declines to use " (:title card))))}}}
   :subroutines [{:label (str "Gain 1 [Credits] for each rezzed piece of Harmonic ice")
                  :msg (msg "gain " (harmonic-ice-count corp) " [Credits]")
                  :async true
                  :effect (req (gain-credits state :corp eid (harmonic-ice-count corp)))}]})

(defcard "Weir"
  {:subroutines [runner-loses-click
                 {:label "Runner trashes 1 card from the grip"
                  :req (req (pos? (count (:hand runner))))
                  :prompt "Choose a card to trash"
                  :player :runner
                  :choices (req (:hand runner))
                  :not-distinct true
                  :async true
                  :effect (effect (system-msg :runner (str "trashes " (:title target) " from the grip"))
                                  (trash :runner eid target {:cause :subroutine}))}]})

(defcard "Wendigo"
  (implementation-note
    "Program prevention is not implemented"
    (morph-ice "Code Gate" "Barrier"
               {:msg "prevent the Runner from using a chosen program for the remainder of this run"})))

(defcard "Whirlpool"
  {:subroutines [{:label "The Runner cannot jack out for the remainder of this run"
                  :msg "prevent the Runner from jacking out and trash itself"
                  :async true
                  :effect (req (prevent-jack-out state side)
                               (wait-for (trash state :corp (make-eid state eid) card {:cause :subroutine})
                                         (encounter-ends state side eid)))}]})

(defcard "Whitespace"
  {:subroutines [(runner-loses-credits 3)
                 {:label "End the run if the Runner has 6 [Credits] or less"
                  :req (req (< (:credit runner) 7))
                  :msg "end the run"
                  :async true
                  :effect (effect (end-run :corp eid card))}]})

(defcard "Winchester"
  (let [ab {:effect (req (reset-variable-subs state :corp card
                                              (if (protecting-hq? card) 1 0)
                                              (trace-ability 3 end-the-run) {:back true}))}]
    {:subroutines [(trace-ability 4 trash-program-sub)
                   (trace-ability 3 trash-hardware-sub)]
     :on-rez ab
     :events [(assoc ab :event :rez)
              (assoc ab :event :card-moved)
              (assoc ab :event :approach-ice)
              (assoc ab :event :swap
                     :req (req (and (= :installed (:swap-type context))
                                    (or (protecting-hq? (:card1 context))
                                        (protecting-hq? (:card2 context))))))]}))

(defcard "Woodcutter"
  (zero-to-hero (do-net-damage 1)))

(defcard "Wormhole"
  (space-ice (resolve-another-subroutine)))

(defcard "Wotan"
  {:subroutines [(end-the-run-unless-runner-pays (->c :click 2 {:allowed-during-run true}))
                 (end-the-run-unless-runner-pays (->c :credit 3))
                 (end-the-run-unless-runner-pays (->c :program 1))
                 (end-the-run-unless-runner-pays (->c :brain 1))]})

(defcard "Wraparound"
  {:subroutines [end-the-run]
   :static-abilities [(ice-strength-bonus
                        (req (not-any? #(has-subtype? % "Fracter") (all-active-installed state :runner)))
                        7)]})

(defcard "Yagura"
  {:subroutines [{:label "Look at the top card of R&D"
                  :optional {:prompt (msg "Move " (:title (first (:deck corp))) " to the bottom of R&D?")
                             :yes-ability {:msg "move the top card of R&D to the bottom"
                                           :effect (effect (move (first (:deck corp)) :deck))}
                             :no-ability {:effect (effect (system-msg :corp (str "declines to use " (:title card) " to move the top card of R&D to the bottom")))}}}
                 (do-net-damage 1)]})

(defcard "Zed 1.0"
  {:subroutines [{:label "Do 1 core damage"
                  :async true
                  :effect (req (if (spent-click-to-break-sub state run)
                                 (continue-ability state side (do-brain-damage 1) card nil)
                                 (do (system-msg state side "does not do core damage with Zed 1.0")
                                     (effect-completed state side eid))))}
                 {:label "Do 1 core damage"
                  :async true
                  :effect (req (if (spent-click-to-break-sub state run)
                                 (continue-ability state side (do-brain-damage 1) card nil)
                                 (do (system-msg state side "does not do core damage with Zed 1.0")
                                     (effect-completed state side eid))))}]
   :runner-abilities [(bioroid-break 1 1)]})

(defcard "Zed 2.0"
  {:subroutines [trash-hardware-sub
                 trash-hardware-sub
                 {:label "Do 1 core damage"
                  :async true
                  :effect (req (if (spent-click-to-break-sub state run)
                                 (continue-ability state side (do-brain-damage 2) card nil)
                                 (do (system-msg state side "does not do core damage with Zed 2.0")
                                     (effect-completed state side eid))))}]
   :runner-abilities [(bioroid-break 2 2)]})
