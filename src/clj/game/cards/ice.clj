(ns game.cards.ice
  (:require [game.core :refer :all]
            [game.core.card :refer :all]
            [game.core.eid :refer [make-eid effect-completed complete-with-result]]
            [game.core.card-defs :refer [card-def]]
            [game.core.prompts :refer [show-wait-prompt clear-wait-prompt]]
            [game.core.toasts :refer [toast]]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability when-let*]]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [clojure.stacktrace :refer [print-stack-trace]]
            [jinteki.utils :refer :all]))

;;;; Helper functions specific for ICE
(defn reset-variable-subs
  ([state side card total sub] (reset-variable-subs state side card total sub nil))
  ([state side card total sub args]
   (let [args (merge {:variable true} args)
         old-subs (remove #(and (= (:cid card) (:from-cid %))
                                (:variable %))
                          (:subroutines card))
         new-card (assoc card :subroutines old-subs)
         new-subs (->> (range total)
                       (reduce (fn [ice _] (add-sub ice sub (:cid ice) args)) new-card)
                       :subroutines
                       (into []))
         new-card (assoc new-card :subroutines new-subs)]
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

;;; Runner abilites for breaking subs
(defn runner-pay-or-break
  "Ability to break a subroutine by spending a resource (Bioroids, Negotiator, etc)"
  [cost qty label]
  (let [cost-str (build-cost-string cost cost->label)
        subs-str (quantify qty "subroutine")]
    {:cost cost
     :label (str label " " subs-str)
     :effect (req (system-msg state :runner (str "spends " cost-str " to " label " " subs-str " on " (:title card))))}))

(defn runner-pay
  "Ability to pay to avoid a subroutine by spending a resource (Popup Window, Turing, etc)"
  [cost qty]
  (runner-pay-or-break cost qty "pay for"))

(defn bioroid-break
  [cost qty]
  (break-sub [:click cost] qty))

;;; General subroutines
(def end-the-run
  "Basic ETR subroutine"
  {:label "End the run"
   :msg "end the run"
   :async true
   :effect (effect (end-run eid card))})

(def end-the-run-if-tagged
  "ETR subroutine if tagged"
  {:label "End the run if the Runner is tagged"
   :req (req tagged)
   :msg "end the run"
   :async true
   :effect (effect (end-run eid card))})

(defn end-the-run-unless-runner-pays
  [amount]
  {:player :runner
   :async true
   :label (str "End the run unless the Runner pays " amount " [Credits]")
   :prompt (str "End the run or pay " amount " [Credits]?")
   :choices ["End the run"
             (str "Pay " amount " [Credits]")]
   :effect (req (if (= "End the run" target)
                  (end-run state :corp eid)
                  (pay-sync state :runner eid [:credit amount])))})

(defn end-the-run-unless-runner
  [label prompt ability]
  {:player :runner
   :async true
   :label (str "End the run unless the Runner " label)
   :prompt (str "End the run or " prompt "?")
   :choices ["End the run"
             (capitalize prompt)]
   :effect (req (if (= "End the run" target)
                  (end-run state :corp eid)
                  (continue-ability state side ability card nil)))})

(defn give-tags
  "Basic give runner n tags subroutine."
  [n]
  {:label (str "Give the Runner " (quantify n "tag"))
   :msg (str "give the Runner " (quantify n "tag"))
   :async true
   :effect (effect (gain-tags :corp eid n))})

(def add-power-counter
  "Adds 1 power counter to the card."
  {:label "Add 1 power counter"
   :msg "add 1 power counter"
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
   :effect (effect (gain-credits credits))})

(defn power-counter-ability
  "Does specified ability using a power counter."
  [{:keys [label message] :as ability}]
  (assoc ability
         :label (str "Hosted power counter: " label)
         :msg (str message " using 1 power counter")
         :cost [:power 1]))

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

(def take-bad-pub
  "Bad pub on rez effect."
  (effect (gain-bad-publicity :corp 1)
          (system-msg (str "takes 1 bad publicity from " (:title card)))))

(def runner-loses-click
  "Runner loses a click effect"
  (effect (lose :runner :click 1)))

;;; For Advanceable ICE
(defn get-advance-counters
  [card]
  (+ (get-counters card :advancement) (:extra-advance-counter card 0)))

(def advance-counters
  "Number of advancement counters - for advanceable ICE."
  (req (get-advance-counters card)))

(def space-ice-rez-bonus
  "Amount of rez reduction for the Space ICE."
  (req (* -3 (get-advance-counters card))))

(defn space-ice
  "Creates data for Space ICE with specified abilities."
  [& abilities]
  {:advanceable :always
   :subroutines (vec abilities)
   :rez-cost-bonus space-ice-rez-bonus})

;;; For Grail ICE
(defn grail-in-hand
  "Req that specified card is a Grail card in the Corp's hand."
  [card]
  (and (corp? card)
       (in-hand? card)
       (has-subtype? card "Grail")))

(def reveal-grail
  "Ability for revealing Grail ICE from HQ."
  {:label "Reveal up to 2 Grail ICE from HQ"
   :choices {:max 2
             :req grail-in-hand}
   :effect (effect (reveal targets))
   :msg (let [sub-label #(:label (first (:subroutines (card-def %))))]
          (msg "reveal " (join ", " (map #(str (:title %) " (" (sub-label %) ")") targets))))})

(def resolve-grail
  "Ability for resolving a subroutine on a Grail ICE in HQ."
  {:label "Resolve a Grail ICE subroutine from HQ"
   :choices {:req grail-in-hand}
   :effect (req (doseq [ice targets]
                  (let [subroutine (first (:subroutines (card-def ice)))]
                    (resolve-ability state side subroutine card nil))))})

(defn grail-ice
  "Creates data for grail ICE"
  [ability]
  {:abilities [reveal-grail]
   :subroutines [ability resolve-grail]})

;;; For NEXT ICE
(defn next-ice-count
  "Counts number of rezzed NEXT ICE - for use with NEXT Bronze and NEXT Gold"
  [corp]
  (let [servers (flatten (seq (:servers corp)))
        rezzed-next? #(and (rezzed? %) (has-subtype? % "NEXT"))]
    (reduce (fn [c server] (+ c (count (filter rezzed-next? (:ices server))))) 0 servers)))

;;; For Morph ICE
(defn morph [state side card new old]
  (update! state side (assoc card
                             :subtype-target new
                             :subtype (combine-subtypes true
                                                        (remove-subtypes (:subtype card) old)
                                                        new)))
  (update-ice-strength state side card))

(defn morph-effect
  "Creates morph effect for ICE. Morphs from base type to other type"
  [base other]
  (req (if (odd? (get-counters (get-card state card) :advancement))
         (morph state side card other base)
         (morph state side card base other))))

(defn morph-ice
  "Creates the data for morph ICE with specified types and ability."
  [base other ability]
  (let [ab {:req (req (same-card? card target))
            :effect (morph-effect base other)}]
    {:advanceable :always
     :effect (morph-effect base other)
     :subroutines [ability]
     :events {:advance ab :advancement-placed ab}}))

;;; For Constellation ICE
(defn constellation-ice
  "Generates map for Constellation ICE with specified effect."
  [ability]
  {:subroutines [(assoc-in (trace-ability 2 ability) [:trace :kicker] (assoc ability :min 5))]})

;; For advance-only-while-rezzed, sub-growing ICE
(defn zero-to-hero
  "Salvage, Tyrant, Woodcutter"
  [sub]
  (let [ability {:req (req (same-card? card target))
                 :effect (effect (reset-variable-subs card (get-counters card :advancement) sub))}]
    {:advanceable :while-rezzed
     :events {:advance ability
              :advancement-placed ability
              :rez ability}}))

;; For 7 Wonders ICE
(defn wonder-sub
  "Checks total number of advancement counters on a piece of ice against number"
  [card number]
  (<= number (get-advance-counters card)))

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
            {:prompt "Select the ice"
             :choices {:req pred
                       :all true}
             :effect (effect
                       (continue-ability
                         (let [ice target]
                           {:prompt "Select the subroutine"
                            :choices (req (unbroken-subroutines-choice ice))
                            :msg (msg "resolve the subroutine (\"[subroutine] "
                                      target "\") from " (:title ice))
                            :effect (req (let [sub (first (filter #(= target (make-label (:sub-effect %))) (:subroutines ice)))]
                                           (continue-ability state side (:sub-effect sub) ice nil)))})
                         card nil))})
          card nil))})))

;;; Helper function for adding implementation notes to ICE defined with functions
(defn- implementation-note
  "Adds an implementation note to the ice-definition"
  [note ice-def]
  (assoc ice-def :implementation note))

;; Card definitions
(def card-definitions
  {"Afshar"
   {:implementation "Breaking both subs not restricted"
    :subroutines [{:msg "make the Runner lose 2 [Credits]"
                   :effect (effect (lose-credits :runner 2))}
                  end-the-run]}

   "Aiki"
   {:subroutines [(do-psi {:label "Runner draws 2 cards"
                           :msg "make the Runner draw 2 cards"
                           :async true
                           :effect (effect (draw :runner eid 2 nil))})
                  (do-net-damage 1)
                  (do-net-damage 1)]}

   "Aimor"
   {:subroutines [{:label "Trash the top 3 cards of the Stack. Trash Aimor."
                   :effect (req (when (not-empty (:deck runner))
                                  (system-msg state :corp
                                              (str "uses Aimor to trash "
                                                   (join ", " (map :title (take 3 (:deck runner))))
                                                   " from the Runner's Stack"))
                                  (mill state :corp :runner 3))
                                (when current-ice
                                  (no-action state :corp nil)
                                  (continue state :runner nil))
                                (trash state side card)
                                (system-msg state side (str "trashes Aimor")))}]}

   "Anansi"
   (let [corp-draw {:optional {:prompt "Draw 1 card?"
                               :yes-ability {:async true
                                             :msg "draw 1 card"
                                             :effect (effect (draw eid 1 nil))}}}
         runner-draw {:async true
                      :effect (req (show-wait-prompt state :corp "Runner to decide on card draw")
                                   (continue-ability state side
                                                     {:player :runner
                                                      :optional
                                                      {:prompt "Pay 2 [Credits] to draw 1 card?"
                                                       :no-ability {:effect (effect (system-msg :runner "does not draw 1 card")
                                                                                    (clear-wait-prompt :corp))}
                                                       :yes-ability {:async true
                                                                     :effect (effect
                                                                               (system-msg :runner "pays 2 [Credits] to draw 1 card")
                                                                               (lose-credits 2)
                                                                               (clear-wait-prompt :corp)
                                                                               (draw eid 1 nil))}}}
                                                     card nil))}]
     {:implementation "Encounter-ends effect is manually triggered."
      :subroutines [{:msg "rearrange the top 5 cards of R&D"
                     :async true
                     :effect (req (show-wait-prompt state :runner "Corp to rearrange the top cards of R&D")
                                  (let [from (take 5 (:deck corp))]
                                    (if (pos? (count from))
                                      (continue-ability state side (reorder-choice :corp :runner from '()
                                                                                   (count from) from)
                                                        card nil)
                                      (do (clear-wait-prompt state :runner)
                                          (effect-completed state side eid)))))}
                    {:label "Draw 1 card; allow runner to draw 1 card"
                     :async true
                     :effect (req (wait-for (resolve-ability state side corp-draw card nil)
                                            (continue-ability state :runner runner-draw card nil)))}
                    (do-net-damage 1)]
      :abilities [(do-net-damage 3)]})

   "Archangel"
   {:flags {:rd-reveal (req true)}
    :access
    {:async true
     :req (req (not= (first (:zone card)) :discard))
     :effect (effect (show-wait-prompt :runner "Corp to decide to trigger Archangel")
                     (continue-ability
                       {:optional
                        {:prompt "Pay 3 [Credits] to force Runner to encounter Archangel?"
                         :yes-ability
                         {:cost [:credit 3]
                          :async true
                          :effect (effect (system-msg :corp "pays 3 [Credits] to force the Runner to encounter Archangel")
                                          (clear-wait-prompt :runner)
                                          (continue-ability
                                            :runner
                                            {:optional
                                             {:player :runner
                                              :prompt "You are encountering Archangel. Allow its subroutine to fire?"
                                              :priority 1
                                              :yes-ability {:async true
                                                            :effect (effect (resolve-unbroken-subs! eid card))}
                                              :no-ability {:effect (effect (effect-completed eid))}}}
                                            card nil))}
                         :no-ability {:effect (effect (system-msg :corp "declines to force the Runner to encounter Archangel")
                                                      (clear-wait-prompt :runner))}}}
                       card nil))}
    :subroutines [(trace-ability
                    6
                    {:async true
                     :effect (effect (show-wait-prompt :runner "Corp to select Archangel target")
                                     (continue-ability
                                       {:choices {:req #(and (installed? %)
                                                             (runner? %))}
                                        :label "Add 1 installed card to the Runner's Grip"
                                        :msg "add 1 installed card to the Runner's Grip"
                                        :effect (effect (clear-wait-prompt :runner)
                                                        (move :runner target :hand true)
                                                        (system-msg (str "adds " (:title target)
                                                                         " to the Runner's Grip")))
                                        :cancel-effect (effect (clear-wait-prompt :runner)
                                                               (effect-completed eid))}
                                       card nil))})]}

   "Archer"
   {:additional-cost [:forfeit]
    :subroutines [(gain-credits-sub 2)
                  trash-program
                  trash-program
                  end-the-run]}

   "Architect"
   {:flags {:untrashable-while-rezzed true}
    :subroutines [{:label "Look at the top 5 cards of R&D"
                   :prompt "Choose a card to install"
                   :priority true
                   :activatemsg "uses Architect to look at the top 5 cards of R&D"
                   :req (req (and (not (string? target))
                                  (not (operation? target))))
                   :not-distinct true
                   :choices (req (conj (take 5 (:deck corp)) "No install"))
                   :effect (effect (system-msg (str "chooses the card in position "
                                                    (+ 1 (.indexOf (take 5 (:deck corp)) target))
                                                    " from R&D (top is 1)"))
                                   (corp-install (move state side target :play-area) nil {:ignore-all-cost true}))}
                  {:label "Install a card from HQ or Archives"
                   :prompt "Select a card to install from Archives or HQ"
                   :show-discard true
                   :priority true
                   :choices {:req #(and (not (operation? %))
                                        (#{[:hand] [:discard]} (:zone %))
                                        (corp? %))}
                   :effect (effect (corp-install target nil))
                   :msg (msg (corp-install-msg target))}]}

   "Ashigaru"
   {:events {:card-moved {:req (req (and (corp? target)
                                         (or (= :hand (first (:zone target)))
                                             (= :hand (first (:previous-zone target))))))
                          :effect (effect (reset-variable-subs card (count (:hand corp)) end-the-run))}
             :rez {:req (req (same-card? card target))
                   :effect (effect (reset-variable-subs card (count (:hand corp)) end-the-run))}}}

   "Assassin"
   {:subroutines [(trace-ability 5 (do-net-damage 3))
                  (trace-ability 4 trash-program)]}

   "Asteroid Belt"
   (space-ice end-the-run)

   "Authenticator"
   {:implementation "Encounter effect is manual"
    :abilities [(give-tags 1)]
    :runner-abilities [{:label "Take 1 tag"
                        :async true
                        :effect (req (system-msg state :runner "takes 1 tag on encountering Authenticator to Bypass it")
                                     (gain-tags state :runner eid 1 {:unpreventable true}))}]
    :subroutines [(gain-credits-sub 2)
                  end-the-run]}

   "Bailiff"
   {:implementation "Gain credit is manual"
    :abilities [(gain-credits-sub 1)]
    :subroutines [end-the-run]}

   "Bandwidth"
   {:subroutines [{:msg "give the Runner 1 tag"
                   :async true
                   :effect (effect (gain-tags :corp eid 1)
                                   (register-events
                                     {:successful-run {:effect (effect (lose-tags :corp 1))
                                                       :msg "make the Runner lose 1 tag"}
                                      :run-ends {:effect (effect (unregister-events card))}}
                                     card))}]
    :events {:successful-run nil :run-ends nil}}

   "Bastion"
   {:subroutines [end-the-run]}

   "Battlement"
   {:subroutines [end-the-run
                  end-the-run]}

   "Blockchain"
   (let [sub-count (fn [corp]
                     (quot (count (filter #(and (operation? %)
                                                (has-subtype? % "Transaction")
                                                (:seen %))
                                          (:discard corp)))
                           2))
         sub {:label "Gain 1 [Credits], Runner loses 1 [Credits]"
              :msg "gain 1 [Credits] and force the Runner to lose 1 [Credits]"
              :effect (effect (gain-credits 1)
                              (lose-credits :runner 1))}]
     {:implementation "Gaining/losing subs is manual"
      :abilities [{:label "Reset subroutines"
                   :effect (effect (reset-variable-subs card (sub-count corp) sub {:variable true :front true}))}]
      :subroutines [sub
                    end-the-run]})

   "Bloodletter"
   {:subroutines [{:label "Runner trashes 1 program or top 2 cards of their Stack"
                   :effect (req (if (empty? (filter program? (all-active-installed state :runner)))
                                  (do (mill state :runner 2)
                                      (system-msg state :runner (str "trashes the top 2 cards of their Stack")))
                                  (do (show-wait-prompt state :corp "Runner to choose an option for Bloodletter")
                                      (resolve-ability
                                        state :runner
                                        {:prompt "Trash 1 program or trash top 2 cards of the Stack?"
                                         :choices ["Trash 1 program" "Trash top 2 of Stack"]
                                         :effect (req (if (and (= target "Trash top 2 of Stack") (> (count (:deck runner)) 1))
                                                        (do (mill state :runner 2)
                                                            (system-msg state :runner (str "trashes the top 2 cards of their Stack")))
                                                        (resolve-ability state :runner trash-program card nil))
                                                      (clear-wait-prompt state :corp))}
                                        card nil))))}]}

   "Bloom"
   (let [ice-index (fn [state i] (first (keep-indexed #(when (same-card? %2 i) %1)
                                                      (get-in @state (cons :corp (:zone i))))))]
     {:subroutines
      [{:label "Install a piece of ice from HQ protecting another server, ignoring all costs"
        :prompt "Choose ICE to install from HQ in another server"
        :async true
        :choices {:req #(and (ice? %)
                             (in-hand? %))}
        :effect (req (let [this (zone->name (second (:zone card)))
                           nice target]
                       (continue-ability state side
                                         {:prompt (str "Choose a location to install " (:title target))
                                          :choices (req (remove #(= this %) (corp-install-list state nice)))
                                          :async true
                                          :effect (effect (corp-install nice target {:ignore-all-cost true}))}
                                         card nil)))}
       {:label "Install a piece of ice from HQ in the next innermost position, protecting this server, ignoring all costs"
        :prompt "Choose ICE to install from HQ in this server"
        :async true
        :choices {:req #(and (ice? %)
                             (in-hand? %))}
        :effect (req (let [newice (assoc target :zone (:zone card))
                           bndx (ice-index state card)
                           ices (get-in @state (cons :corp (:zone card)))
                           newices (apply conj (subvec ices 0 bndx) newice (subvec ices bndx))]
                       (swap! state assoc-in (cons :corp (:zone card)) newices)
                       (swap! state update-in (cons :corp (:zone target))
                              (fn [coll] (remove-once #(same-card? % target) coll)))
                       (card-init state side newice {:resolve-effect false
                                                     :init-data true})
                       (trigger-event state side :corp-install newice)))}]})

   "Border Control"
   {:abilities [{:label "End the run"
                 :msg (msg "end the run")
                 :async true
                 :cost [:trash]
                 :effect (effect (end-run eid card))}]
    :subroutines [{:label "Gain 1 [Credits] for each ice protecting this server"
                   :msg (msg "gain "
                             (count (:ices (card->server state card)))
                             " [Credits]")
                   :effect (req (let [num-ice (count (:ices (card->server state card)))]
                                  (gain-credits state :corp num-ice)))}
                  end-the-run]}

   "Brainstorm"
   {:implementation "Encounter effect is manual"
    :events {:run-ends {:effect (effect (reset-variable-subs card 0 nil))}}
    :abilities [{:label "Reset subroutines"
                 :effect (effect (reset-variable-subs card (count (:hand runner)) (do-brain-damage 1)))}]}

   "Builder"
   (let [sub {:label "Place 1 advancement token on an ICE that can be advanced protecting this server"
                   :msg (msg "place 1 advancement token on " (card-str state target))
                   :choices {:req #(and (ice? %)
                                        (can-be-advanced? %))}
                   :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]
     {:abilities [{:label "Move Builder to the outermost position of any server"
                   :cost [:click 1]
                   :prompt "Choose a server"
                   :choices (req servers)
                   :msg (msg "move it to the outermost position of " target)
                   :effect (effect (move card (conj (server->zone state target) :ices)))}]
      :subroutines [sub
                    sub]})

   "Bullfrog"
   {:subroutines [(do-psi {:label "Move Bullfrog to another server"
                           :player :corp
                           :prompt "Choose a server"
                           :choices (req servers)
                           :msg (msg "move it to the outermost position of " target)
                           :effect (req (let [dest (server->zone state target)]
                                          (swap! state update-in [:run]
                                                 #(assoc %
                                                         :position (count (get-in corp (conj dest :ices)))
                                                         :server (rest dest))))
                                        (move state side card
                                              (conj (server->zone state target) :ices))
                                        (effect-completed state side eid))})]}

   "Bulwark"
   (let [sub {:msg "gain 2 [Credits] and end the run"
              :effect (effect (gain-credits 2)
                              (end-run eid card))}]
     {:effect take-bad-pub
      :abilities [{:msg "gain 2 [Credits] if there is an installed AI"
                   :req (req (some #(has-subtype? % "AI") (all-active-installed state :runner)))
                   :effect (effect (gain-credits 2))}]
      :subroutines [(assoc trash-program
                           :player :runner
                           :msg "force the Runner to trash 1 program"
                           :label "The Runner trashes 1 program")
                    sub
                    sub]})

   "Burke Bugs"
   {:subroutines [(trace-ability 0 (assoc trash-program
                                          :not-distinct true
                                          :player :runner
                                          :msg "force the Runner to trash a program"
                                          :label "Force the Runner to trash a program"))]}

   "Caduceus"
   {:subroutines [(trace-ability 3 (gain-credits-sub 3))
                  (trace-ability 2 end-the-run)]}

   "Cell Portal"
   {:subroutines [{:msg "make the Runner approach the outermost ICE"
                   :effect (req (let [srv (first (:server run))
                                      n (count (get-in @state [:corp :servers srv :ices]))]
                                  (swap! state assoc-in [:run :position] n)
                                  (derez state side card)))}]}

   "Changeling"
   (morph-ice "Barrier" "Sentry" end-the-run)

   "Checkpoint"
   {:effect take-bad-pub
    :subroutines [(trace-ability 5 {:label "Do 3 meat damage when this run is successful"
                                    :msg "do 3 meat damage when this run is successful"
                                    :effect (effect (register-events
                                                      {:successful-run
                                                       {:async true
                                                        :msg "do 3 meat damage"
                                                        :effect (effect (damage eid :meat 3 {:card card}))}
                                                       :run-ends {:effect (effect (unregister-events card))}}
                                                      card))})]
    :events {:successful-run nil :run-ends nil}}

   "Chetana"
   {:subroutines [{:msg "make each player gain 2 [Credits]"
                   :effect (effect (gain-credits :runner 2)
                                   (gain-credits :corp 2))}
                  (do-psi {:label "Do 1 net damage for each card in the Runner's grip"
                           :msg (msg "do " (count (get-in @state [:runner :hand])) " net damage")
                           :effect (effect (damage eid :net (count (get-in @state [:runner :hand])) {:card card}))})]}

   "Chimera"
   (let [turn-end-ability {:effect (effect (derez :corp card)
                                           (update! (assoc (get-card state card) :subtype "Mythic")))}]
     {:prompt "Choose one subtype"
      :choices ["Barrier" "Code Gate" "Sentry"]
      :msg (msg "make it gain " target " until the end of the turn")
      :effect (effect (update! (assoc card
                                      :subtype-target target
                                      :subtype (combine-subtypes true (:subtype card) target)))
                      (update-ice-strength card))
      :events {:runner-turn-ends turn-end-ability
               :corp-turn-ends turn-end-ability}
      :subroutines [end-the-run]})

   "Chiyashi"
   {:implementation "Trash effect when using an AI to break is activated manually"
    :abilities [{:label "Trash the top 2 cards of the Runner's Stack"
                 :req (req (some #(has-subtype? % "AI") (all-active-installed state :runner)))
                 :msg (msg (str "trash " (join ", " (map :title (take 2 (:deck runner)))) " from the Runner's Stack"))
                 :effect (effect (mill :corp :runner 2))}]
    :subroutines [(do-net-damage 2)
                  (do-net-damage 2)
                  end-the-run]}

   "Chrysalis"
   {:flags {:rd-reveal (req true)}
    :subroutines [(do-net-damage 2)]
    :access {:async true
             :req (req (not= (first (:zone card)) :discard))
             :effect (effect (show-wait-prompt :corp "Runner to decide to break Chrysalis subroutine")
                             (continue-ability
                               :runner {:optional
                                        {:player :runner
                                         :prompt "You are encountering Chrysalis. Allow its subroutine to fire?"
                                         :priority 1
                                         :yes-ability {:effect (effect (clear-wait-prompt :corp)
                                                                       (resolve-unbroken-subs! :corp eid card))}
                                         :no-ability {:effect (effect (clear-wait-prompt :corp)
                                                                      (effect-completed eid))}}}
                               card nil))}}

   "Chum"
   {:subroutines [{:label "Give +2 strength to next ICE Runner encounters"
                   :req (req this-server)
                   :prompt "Select the ICE the Runner is encountering"
                   :choices {:req #(and (rezzed? %) (ice? %))}
                   :msg (msg "give " (:title target) " +2 strength")
                   :effect (req (let [ice (:cid target)]
                                  (register-events state side
                                                   {:pre-ice-strength {:req (req (= (:cid target) ice))
                                                                       :effect (effect (ice-strength-bonus 2 target))}
                                                    :run-ends {:effect (effect (unregister-events card))}}
                                                   card)
                                  (update-all-ice state side)))}
                  (do-net-damage 3)]
    :events {:pre-ice-strength nil :run-ends nil}}

   "Clairvoyant Monitor"
   {:subroutines [(do-psi {:label "Place 1 advancement token and end the run"
                           :player :corp
                           :prompt "Select a target for Clairvoyant Monitor"
                           :msg (msg "place 1 advancement token on "
                                     (card-str state target) " and end the run")
                           :choices {:req installed?}
                           :effect (effect (add-prop target :advance-counter 1 {:placed true})
                                           (end-run eid))})]}

   "Cobra"
   {:subroutines [trash-program (do-net-damage 2)]}

   "Colossus"
   {:advanceable :always
    :subroutines [{:label "Give the Runner 1 tag (Give the Runner 2 tags)"
                   :async true
                   :msg (msg "give the Runner " (if (wonder-sub card 3) "2 tags" "1 tag"))
                   :effect (effect (gain-tags :corp eid (if (wonder-sub card 3) 2 1)))}
                  {:label "Trash 1 program (Trash 1 program and 1 resource)"
                   :async true
                   :msg (msg "trash 1 program" (when (wonder-sub card 3) " and 1 resource"))
                   :effect (req (wait-for (resolve-ability state side trash-program card nil)
                                          (if (wonder-sub card 3)
                                            (continue-ability
                                              state side
                                              {:prompt "Choose a resource to trash"
                                               :msg (msg "trash " (:title target))
                                               :choices {:req #(and (installed? %)
                                                                    (resource? %))}
                                               :cancel-effect (req (effect-completed state side eid))
                                               :effect (effect (trash target {:cause :subroutine}))}
                                              card nil)
                                            (effect-completed state side eid))))}]
    :strength-bonus advance-counters}

   "Congratulations!"
   {:events {:pass-ice {:req (req (same-card? target card))
                        :msg "gain 1 [Credits]"
                        :effect (effect (gain-credits :corp 1))}}
    :subroutines [{:label "Gain 2 [Credits]. The Runner gains 1 [Credits]"
                   :msg "gain 2 [Credits]. The Runner gains 1 [Credits]"
                   :effect (effect (gain-credits :corp 2)
                                   (gain-credits :runner 1))}]}

   "Conundrum"
   {:subroutines [(assoc trash-program
                         :player :runner
                         :msg "force the Runner to trash 1 program"
                         :label "The Runner trashes 1 program")
                  {:msg "force the Runner to lose 1 [Click] if able"
                   :effect runner-loses-click}
                  end-the-run]
    :strength-bonus (req (if (some #(has-subtype? % "AI") (all-active-installed state :runner)) 3 0))}

   "Cortex Lock"
   {:subroutines [{:label "Do 1 net damage for each unused memory unit the Runner has"
                   :msg (msg "do " (available-mu state) " net damage")
                   :effect (effect (damage eid :net (available-mu state) {:card card}))}]}

   "Crick"
   {:subroutines [{:label "install a card from Archives"
                   :prompt "Select a card to install from Archives"
                   :show-discard true
                   :priority true
                   :choices {:req #(and (not (operation? %))
                                        (in-discard? %)
                                        (corp? %))}
                   :msg (msg (corp-install-msg target))
                   :effect (effect (corp-install target nil))}]
    :strength-bonus (req (if (= (second (:zone card)) :archives) 3 0))}

   "Curtain Wall"
   {:subroutines [end-the-run
                  end-the-run
                  end-the-run]
    :strength-bonus (req (let [ices (:ices (card->server state card))]
                           (if (same-card? card (last ices)) 4 0)))
    :events (let [cw {:req (req (and (not (same-card? card target))
                                     (= (card->server state card) (card->server state target))))
                      :effect (effect (update-ice-strength card))}]
              {:corp-install cw
               :trash cw
               :card-moved cw})}

   "Data Hound"
   (letfn [(dh-trash [cards]
             {:prompt "Choose a card to trash"
              :choices cards
              :async true
              :msg (msg "trash " (:title target))
              :effect (req (trash state side target {:unpreventable true})
                           (continue-ability
                             state side
                             (reorder-choice
                               :runner :runner (remove-once #(= % target) cards)
                               '() (count (remove-once #(= % target) cards))
                               (remove-once #(= % target) cards))
                             card nil))})]
     {:subroutines [(trace-ability
                      2
                      {:async true
                       :label "Look at the top of Stack"
                       :msg "look at top X cards of Stack"
                       :effect (req (show-wait-prompt state :runner "Corp to rearrange the top cards of the Runner's Stack")
                                    (let [c (- target (second targets))
                                          from (take c (:deck runner))]
                                      (system-msg state :corp
                                                  (str "looks at the top " c " cards of Stack"))
                                      (if (< 1 c)
                                        (continue-ability state side (dh-trash from) card nil)
                                        (do (system-msg state :corp (str "trashes " (:title (first from))))
                                            (trash state side (first from) {:unpreventable true})
                                            (clear-wait-prompt state :runner)
                                            (effect-completed state side eid)))))})]})

   "Data Loop"
   (let [ability {:label "Add 2 cards from your Grip to the top of the Stack"
                  :req (req (pos? (count (:hand runner))))
                  :effect (req (let [n (min 2 (count (:hand runner)))]
                                 (resolve-ability state side
                                                  {:prompt (msg "Choose " n " cards in your Grip to add to the top of the Stack (first card targeted will be topmost)")
                                                   :choices {:max n :all true
                                                             :req #(and (in-hand? %) (runner? %))}
                                                   :effect (req (doseq [c targets]
                                                                  (move state :runner c :deck {:front true}))
                                                                (system-msg state :runner (str "adds " n " cards from their Grip to the top of the Stack")))}
                                                  card nil)))}]
     {:implementation "Encounter effect is manual"
      :subroutines [end-the-run-if-tagged
                    end-the-run]
      :abilities [ability]
      :runner-abilities [ability]})

   "Data Mine"
   {:subroutines [{:msg "do 1 net damage"
                   :effect (req (damage state :runner eid :net 1 {:card card})
                                (when current-ice
                                  (no-action state side nil)
                                  (continue state side nil))
                                (trash state side card))}]}

   "Data Raven"
   {:implementation "Encounter effect is manual"
    :abilities [(give-tags 1)
                (power-counter-ability (give-tags 1))]
    :runner-abilities [{:label "End the run"
                        :effect (req (end-run state :runner)
                                     (system-msg state :runner "chooses to end the run on encountering Data Raven"))}
                       {:label "Take 1 tag"
                        :async true
                        :effect (req (system-msg state :runner "chooses to take 1 tag on encountering Data Raven")
                                     (gain-tags state :runner eid 1))}]
    :subroutines [(trace-ability 3 add-power-counter)]}

   "Data Ward"
   {:runner-abilities [{:label "Pay 3 [Credits]"
                        :effect (req (pay state :runner card :credit 3)
                                     (system-msg state :runner "chooses to pay 3 [Credits] on encountering Data Ward"))}
                       {:label "Take 1 tag"
                        :async true
                        :effect (req (system-msg state :runner "chooses to take 1 tag on encountering Data Ward")
                                     (gain-tags state :runner eid 1))}]
    :subroutines [end-the-run-if-tagged
                  end-the-run-if-tagged
                  end-the-run-if-tagged
                  end-the-run-if-tagged]}

   "Datapike"
   {:subroutines [{:msg "force the Runner to pay 2 [Credits] if able"
                   :effect (effect (pay :runner card :credit 2))}
                  end-the-run]}

   "DNA Tracker"
   (let [sub {:msg "do 1 net damage and make the Runner lose 2 [Credits]"
              :async true
              :effect (req (wait-for (damage state side :net 1 {:card card})
                                     (lose-credits state :runner 2)
                                     (effect-completed state side eid)))}]
     {:subroutines [sub
                    sub
                    sub]})

   "Dracō"
   {:prompt "How many power counters?"
    :choices :credit
    :msg (msg "add " target " power counters")
    :effect (effect (add-counter card :power target)
                    (update-ice-strength card))
    :strength-bonus (req (get-counters card :power))
    :subroutines [(trace-ability 2 {:label "Give the Runner 1 tag and end the run"
                                    :msg "give the Runner 1 tag and end the run"
                                    :async true
                                    :effect (effect (gain-tags :corp eid 1)
                                                    (end-run))})]}

   "Eli 1.0"
   {:subroutines [end-the-run
                  end-the-run]
    :runner-abilities [(bioroid-break 1 1)]}

   "Eli 2.0"
   {:subroutines [{:msg "draw 1 card"
                   :effect (effect (draw))}
                  end-the-run
                  end-the-run]
    :runner-abilities [(bioroid-break 2 2)]}

   "Endless EULA"
   (let [sub (end-the-run-unless-runner-pays 1)]
     (letfn [(break-fn [unbroken-subs total]
               {:effect
                (req (if (seq unbroken-subs)
                       (wait-for (pay-sync state :runner (make-eid state {:source-type :subroutine}) card [:credit 1])
                                 (system-msg state :runner async-result)
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
      :runner-abilities [{:req (req (<= (count (remove :broken (:subroutines card)))
                                        (total-available-credits state :runner eid card)))
                          :async true
                          :label "Pay for all unbroken subs"
                          :effect (req (let [unbroken-subs (remove :broken (:subroutines card))
                                             eid (assoc eid :source-type :subroutine)]
                                         (->> unbroken-subs
                                              (reduce resolve-subroutine card)
                                              (update! state side))
                                         (continue-ability
                                           state side
                                           (break-fn unbroken-subs 0)
                                           card nil)))}]}))

   "Enforcer 1.0"
   {:additional-cost [:forfeit]
    :subroutines [trash-program
                  (do-brain-damage 1)
                  {:label "Trash a console"
                   :prompt "Select a console to trash"
                   :choices {:req #(has-subtype? % "Console")}
                   :msg (msg "trash " (:title target))
                   :effect (effect (trash target))}
                  {:msg "trash all virtual resources"
                   :effect (req (doseq [c (filter #(has-subtype? % "Virtual") (all-active-installed state :runner))]
                                  (trash state side c)))}]
    :runner-abilities [(bioroid-break 1 1)]}

   "Enigma"
   {:subroutines [{:msg "force the Runner to lose 1 [Click] if able"
                   :effect runner-loses-click}
                  end-the-run]}

   "Envelope"
   {:subroutines [(do-net-damage 1)
                  end-the-run]}

   "Errand Boy"
   (let [sub {:label "Draw a card or gain 1 [Credits]"
              :prompt "Choose one:"
              :choices ["Gain 1 [Credits]" "Draw 1 card"]
              :msg (req (if (= target "Gain 1 [Credits]")
                          "gain 1 [Credits]"
                          "draw 1 card"))
              :effect (req (if (= target "Gain 1 [Credits]")
                                   (gain-credits state :corp 1)
                                   (draw state :corp eid 1 nil)))}]
     {:subroutines [sub sub sub]})

   "Excalibur"
   {:subroutines [{:label "The Runner cannot make another run this turn"
                   :msg "prevent the Runner from making another run"
                   :effect (effect (register-turn-flag! card :can-run nil))}]}

   "Executive Functioning"
   {:subroutines [(trace-ability 4 (do-brain-damage 1))]}

   "Fairchild"
   {:subroutines [(end-the-run-unless-runner-pays 4)
                  (end-the-run-unless-runner-pays 4)
                  (end-the-run-unless-runner
                    "trashes an installed card"
                    "trash an installed card"
                    trash-installed)
                  (end-the-run-unless-runner
                    "suffers 1 brain damage"
                    "suffer 1 brain damage"
                    (do-brain-damage 1))]}

   "Fairchild 1.0"
   (let [sub {:label "Force the Runner to pay 1 [Credits] or trash an installed card"
              :msg "force the Runner to pay 1 [Credits] or trash an installed card"
              :player :runner
              :prompt "Choose one"
              :choices ["Pay 1 [Credits]" "Trash an installed card"]
              :effect (req (if (= target "Pay 1 [Credits]")
                             (do (pay state side card :credit 1)
                                 (system-msg state side "pays 1 [Credits]"))
                             (continue-ability state :runner trash-installed card nil)))}]
     {:subroutines [sub sub]
      :runner-abilities [(bioroid-break 1 1)]})

   "Fairchild 2.0"
   (let [sub {:label "Force the Runner to pay 2 [Credits] or trash an installed card"
              :msg "force the Runner to pay 2 [Credits] or trash an installed card"
              :player :runner
              :prompt "Choose one"
              :choices ["Pay 2 [Credits]" "Trash an installed card"]
              :effect (req (if (= target "Pay 2 [Credits]")
                             (do (pay state side card :credit 2)
                                 (system-msg state side "pays 2 [Credits]"))
                             (continue-ability state :runner trash-installed card nil)))}]
     {:subroutines [sub sub
                    (do-brain-damage 1)]
      :runner-abilities [(bioroid-break 2 2)]})

   "Fairchild 3.0"
   (let [sub {:label "Force the Runner to pay 3 [Credits] or trash an installed card"
              :msg "force the Runner to pay 3 [Credits] or trash an installed card"
              :player :runner
              :prompt "Choose one"
              :choices ["Pay 3 [Credits]" "Trash an installed card"]
              :effect (req (if (= target "Pay 3 [Credits]")
                             (do (pay state side card :credit 3)
                                 (system-msg state side "pays 3 [Credits]"))
                             (continue-ability state :runner trash-installed card nil)))}]
     {:subroutines [sub sub
                    {:label "Do 1 brain damage or end the run"
                     :prompt "Choose one"
                     :choices ["Do 1 brain damage" "End the run"]
                     :msg (msg (lower-case target))
                     :async true
                     :effect (req (if (= target "Do 1 brain damage")
                                    (damage state side eid :brain 1 {:card card})
                                    (end-run state side eid)))}]
      :runner-abilities [(bioroid-break 3 3)]})

   "Fenris"
   {:effect take-bad-pub
    :subroutines [(do-brain-damage 1)
                  end-the-run]}

   "Fire Wall"
   {:advanceable :always
    :subroutines [end-the-run]
    :strength-bonus advance-counters}

   "Flare"
   {:subroutines [(trace-ability 6 {:label "Trash 1 hardware, do 2 meat damage, and end the run"
                                    :msg "trash 1 hardware, do 2 meat damage, and end the run"
                                    :async true
                                    :effect (effect (continue-ability
                                                      {:prompt "Select a piece of hardware to trash"
                                                       :label "Trash a piece of hardware"
                                                       :choices {:req hardware?}
                                                       :msg (msg "trash " (:title target))
                                                       :effect (req (wait-for
                                                                      (trash state side target {:cause :subroutine})
                                                                      (do (damage state side eid :meat 2 {:unpreventable true
                                                                                                          :card card})
                                                                          (end-run state side))))
                                                       :cancel-effect (effect (damage eid :meat 2 {:unpreventable true :card card})
                                                                              (end-run))}
                                                      card nil))})]}

   "Formicary"
   {:optional {:prompt "Move Formicary?"
               :req (req (and (:run @state)
                              (zero? (:position run))
                              (not (contains? run :corp-phase-43))
                              (not (contains? run :successful))))
               :yes-ability {:msg "rez and move Formicary. The Runner is now approaching Formicary."
                             :effect (req (move state side card
                                                [:servers (first (:server run)) :ices]
                                                {:front true})
                                          (swap! state assoc-in [:run :position] 1))}
               :no-ability {:msg "rez Formicary without moving it"}}
    :subroutines [{:label "End the run unless the Runner suffers 2 net damage"
                   :async true
                   :effect (req (wait-for (resolve-ability
                                            state :runner
                                            {:optional
                                             {:prompt "Suffer 2 net damage? (If not, end the run)"
                                              :yes-ability {:async true
                                                            :msg "let the Runner suffer 2 net damage"
                                                            :effect (effect (damage eid :net 2 {:card card :unpreventable true}))}
                                              :no-ability end-the-run}}
                                            card nil)))}]}

   "Free Lunch"
   {:abilities [(power-counter-ability {:label "Runner loses 1 [Credits]"
                                        :msg "make the Runner lose 1 [Credits]"
                                        :effect (effect (lose-credits :runner 1))})]
    :subroutines [add-power-counter
                  add-power-counter]}

   "Galahad"
   (grail-ice end-the-run)

   "Gatekeeper"
   (let [draw {:async true
               :prompt "Draw how many cards?"
               :choices {:number (req 3)
                         :max (req 3)
                         :default (req 1)}
               :msg (msg "draw " target "cards")
               :effect (effect (draw eid target nil))}
         reveal-and-shuffle {:prompt "Reveal and shuffle up to 3 agendas"
                             :show-discard true
                             :choices {:req #(and (corp? %)
                                                  (or (= [:discard] (:zone %))
                                                      (= [:hand] (:zone %)))
                                                  (agenda? %))
                                       :max (req 3)}
                             :effect (req (reveal state side targets)
                                          (doseq [c targets]
                                            (move state :corp c :deck))
                                          (shuffle! state :corp :deck))
                             :cancel-effect (effect (shuffle! :deck))
                             :msg (msg "add "
                                       (str (join ", " (map :title targets)))
                                       " to R&D")}
         draw-reveal-shuffle {:async true
                              :label "Draw cards, reveal and shuffle agendas"
                              :effect (req (wait-for (resolve-ability state side draw card nil)
                                                     (continue-ability state side reveal-and-shuffle card nil)))}]
     {:strength-bonus (req (if (= :this-turn (:rezzed card)) 6 0))
      :subroutines [draw-reveal-shuffle
                    end-the-run]})

   "Gemini"
   (constellation-ice (do-net-damage 1))

   "Grim"
   {:effect take-bad-pub
    :subroutines [trash-program]}

   "Guard"
   {:implementation "Prevent bypass is manual"
    :subroutines [end-the-run]}

   "Gutenberg"
   {:subroutines [(tag-trace 7)]
    :strength-bonus (req (if (= (second (:zone card)) :rd) 3 0))}

   "Gyri Labyrinth"
   {:implementation "Hand size is not restored if trashed or derezzed after firing"
    :subroutines [{:req (req (:run @state))
                   :label "Reduce Runner's maximum hand size by 2 until start of next Corp turn"
                   :msg "reduce the Runner's maximum hand size by 2 until the start of the next Corp turn"
                   :effect (effect (lose :runner :hand-size 2)
                                   (register-events {:corp-turn-begins
                                                     {:msg "increase the Runner's maximum hand size by 2"
                                                      :effect (effect (gain :runner :hand-size 2)
                                                                      (unregister-events card))}} card))}]
    :events {:corp-turn-begins nil}}

   "Hadrian's Wall"
   {:advanceable :always
    :subroutines [end-the-run end-the-run]
    :strength-bonus advance-counters}

   "Hagen"
   {:subroutines [{:label "Trash 1 program"
                   :prompt "Choose a program that is not a decoder, fracter or killer"
                   :msg (msg "trash " (:title target))
                   :choices {:req #(and (installed? %)
                                        (program? %)
                                        (not (has-subtype? % "Decoder"))
                                        (not (has-subtype? % "Fracter"))
                                        (not (has-subtype? % "Killer")))}
                   :effect (effect (trash target {:cause :subroutine})
                                   (clear-wait-prompt :runner))}
                  end-the-run]
    :strength-bonus (req (- (count (filter #(has-subtype? % "Icebreaker")
                                            (all-active-installed state :runner)))))}

   "Hailstorm"
   {:subroutines [{:label "Remove a card in the Heap from the game"
                   :prompt "Choose a card in the Runner's Heap"
                   :choices (req (:discard runner))
                   :msg (msg "remove " (:title target) " from the game")
                   :effect (effect (move :runner target :rfg))}
                  end-the-run]}

   "Harvester"
   (let [sub {:label "Runner draws 3 cards and discards down to maximum hand size"
              :msg "make the Runner draw 3 cards and discard down to their maximum hand size"
              :async true
              :effect (req (wait-for (draw state :runner 3 nil)
                                     (let [delta (- (count (get-in @state [:runner :hand])) (hand-size state :runner))]
                                       (if (pos? delta)
                                         (continue-ability
                                           state :runner
                                           {:prompt (msg "Select " delta " cards to discard")
                                            :player :runner
                                            :choices {:max delta
                                                      :req #(in-hand? %)}
                                            :effect (req (doseq [c targets]
                                                           (trash state :runner c))
                                                         (system-msg state :runner
                                                                     (str "trashes " (join ", " (map :title targets)))))}
                                           card nil)
                                         (effect-completed state side eid)))))}]
     {:subroutines [sub sub]})

   "Heimdall 1.0"
   {:subroutines [(do-brain-damage 1)
                  end-the-run end-the-run]
    :runner-abilities [(bioroid-break 1 1)]}

   "Heimdall 2.0"
   {:subroutines [(do-brain-damage 1)
                  {:msg "do 1 brain damage and end the run"
                   :effect (req (wait-for (damage state side :brain 1 {:card card})
                                          (end-run state side eid)))}
                  end-the-run]
    :runner-abilities [(bioroid-break 2 2)]}

   "Herald"
   {:flags {:rd-reveal (req true)}
    :subroutines [(gain-credits-sub 2)
                  {:async true
                   :label "Pay up to 2 [Credits] to place up to 2 advancement tokens"
                   :prompt "How many advancement tokens?"
                   :choices (req (map str (range (inc (min 2 (:credit corp))))))
                   :effect (req (let [c (str->int target)]
                                  (if (can-pay? state side eid card (:title card) :credit c)
                                    (do (pay state :corp card :credit c)
                                        (continue-ability
                                          state side
                                          {:msg (msg "pay " c "[Credits] and place " (quantify c " advancement token")
                                                     " on " (card-str state target))
                                           :choices {:req can-be-advanced?}
                                           :effect (effect (add-prop target :advance-counter c {:placed true}))}
                                          card nil))
                                    (effect-completed state side eid))))}]
    :access {:async true
             :req (req (not= (first (:zone card)) :discard))
             :effect (effect (show-wait-prompt :corp "Runner to decide to break Herald subroutines")
                             (continue-ability
                               :runner {:optional
                                        {:player :runner
                                         :prompt "You are encountering Herald. Allow its subroutines to fire?"
                                         :priority 1
                                         :yes-ability {:effect (effect (clear-wait-prompt :corp)
                                                                       (resolve-unbroken-subs! :corp eid card))}
                                         :no-ability {:effect (effect (clear-wait-prompt :corp)
                                                                      (effect-completed eid))}}}
                               card nil))}}

   "Himitsu-Bako"
   {:abilities [{:msg "add it to HQ"
                 :cost [:credit 1]
                 :effect (effect (move card :hand))}]
    :subroutines [end-the-run]}

   "Hive"
   (let [corp-points (fn [corp] (min 5 (max 0 (- 5 (:agenda-point corp 0)))))
         ability {:effect (effect (reset-printed-subs card (corp-points corp) end-the-run))}]
     {:events {:rez (assoc ability :req (req (same-card? card target)))
               :agenda-scored ability
               :as-agenda (assoc ability :req (req (= "Corp" (:as-agenda-side target))))}
      :abilities [{:label "Lose subroutines"
                   :msg (msg "lose " (- 5 (corp-points corp)) " subroutines")
                   :effect (effect (reset-printed-subs card (corp-points corp) end-the-run))}]
      :subroutines [end-the-run
                    end-the-run
                    end-the-run
                    end-the-run
                    end-the-run]})

   "Holmegaard"
   {:subroutines [(trace-ability 4 {:label "Runner cannot access any cards this run"
                                    :msg "stop the Runner from accessing any cards this run"
                                    :effect (effect (prevent-access))})
                  {:label "Trash an icebreaker"
                   :prompt "Choose an icebreaker to trash"
                   :msg (msg "trash " (:title target))
                   :choices {:req #(and (installed? %)
                                        (has-subtype? % "Icebreaker"))}
                   :effect (effect (trash target {:cause :subroutine})
                                   (clear-wait-prompt :runner))}]}

   "Hortum"
   (letfn [(hort [n] {:prompt "Choose a card to add to HQ with Hortum"
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
     {:advanceable :always
      :subroutines [{:label "Gain 1 [Credits] (Gain 4 [Credits])"
                     :msg (msg "gain " (if (wonder-sub card 3) "4" "1") " [Credits]")
                     :effect (effect (gain-credits :corp (if (wonder-sub card 3) 4 1)))}
                    {:label "End the run (Search R&D for up to 2 cards and add them to HQ, shuffle R&D, end the run)"
                     :async true
                     :effect (req (if (wonder-sub card 3)
                                    (wait-for
                                      (resolve-ability state side (hort 1) card nil)
                                      (do (end-run state side)
                                          (system-msg state side
                                                      (str "uses Hortum to add 2 cards to HQ from R&D, "
                                                           "shuffle R&D, and end the run"))))
                                    (do (end-run state side)
                                        (system-msg state side (str "uses Hortum to end the run"))
                                        (effect-completed state side eid))))}]})

   "Hourglass"
   (let [sub {:msg "force the Runner to lose 1 [Click] if able"
              :effect runner-loses-click}]
     {:subroutines [sub sub sub]})

   "Howler"
   (let [ice-index (fn [state i] (first (keep-indexed #(when (same-card? %2 i) %1)
                                                      (get-in @state (cons :corp (:zone i))))))]
     {:subroutines
      [{:label "Install a piece of Bioroid ICE from HQ or Archives"
        :prompt "Install ICE from HQ or Archives?"
        :choices ["HQ" "Archives"]
        :effect (req (let [fr target]
                       (resolve-ability state side
                                        {:prompt "Choose a Bioroid ICE to install"
                                         :choices (req (filter #(and (ice? %)
                                                                     (has-subtype? % "Bioroid"))
                                                               ((if (= fr "HQ") :hand :discard) corp)))
                                         :effect (req (let [newice (assoc target :zone (:zone card) :rezzed true)
                                                            hndx (ice-index state card)
                                                            ices (get-in @state (cons :corp (:zone card)))
                                                            newices (apply conj (subvec ices 0 hndx) newice (subvec ices hndx))]
                                                        (swap! state assoc-in (cons :corp (:zone card)) newices)
                                                        (swap! state update-in (cons :corp (:zone target))
                                                               (fn [coll] (remove-once #(same-card? % target) coll)))
                                                        (update! state side (assoc card :howler-target newice))
                                                        (card-init state side newice {:resolve-effect false
                                                                                      :init-data true})
                                                        (trigger-event state side :corp-install newice)))} card nil)))}]
      :events {:run-ends {:req (req (:howler-target card))
                          :effect (effect (trash card {:cause :self-trash})
                                          (derez (get-card state (:howler-target card))))}}})

   "Hudson 1.0"
   (let [sub {:msg "prevent the Runner from accessing more than 1 card during this run"
              :effect (effect (max-access 1))}]
     {:subroutines [sub sub]
      :runner-abilities [(bioroid-break 1 1)]})

   "Hunter"
   {:subroutines [(tag-trace 3)]}

   "Hydra"
   (letfn [(otherwise-tag [message ability]
             {:msg (msg (if tagged message "give the Runner 1 tag"))
              :label (str (capitalize message) " if the Runner is tagged; otherwise, give the Runner 1 tag")
              :async true
              :effect (req (if tagged
                             (ability state :runner eid card nil)
                             (gain-tags state :runner eid 1)))})]
     {:subroutines [(otherwise-tag
                      "do 3 net damage"
                      (req (damage state :runner :net 3 {:card card})))
                    (otherwise-tag
                      "gain 5 [Credits]"
                      (req (gain-credits state :corp 5)
                           (effect-completed state side eid)))
                    (otherwise-tag
                      "end the run"
                      (req (end-run state side eid)))]})

   "Ice Wall"
   {:advanceable :always
    :subroutines [end-the-run]
    :strength-bonus advance-counters}

   "Ichi 1.0"
   {:subroutines [trash-program trash-program
                  (trace-ability 1 {:label "Give the Runner 1 tag and do 1 brain damage"
                                    :msg "give the Runner 1 tag and do 1 brain damage"
                                    :async true
                                    :effect (req (wait-for (damage state :runner :brain 1 {:card card})
                                                           (gain-tags state :corp eid 1)))})]
    :runner-abilities [(bioroid-break 1 1)]}

   "Ichi 2.0"
   {:subroutines [trash-program trash-program
                  (trace-ability 3 {:label "Give the Runner 1 tag and do 1 brain damage"
                                    :msg "give the Runner 1 tag and do 1 brain damage"
                                    :async true
                                    :effect (req (wait-for (damage state :runner :brain 1 {:card card})
                                                           (gain-tags state :corp eid 1)))})]
    :runner-abilities [(bioroid-break 2 2)]}

   "Inazuma"
   {:abilities [{:msg "prevent the Runner from breaking subroutines on the next piece of ICE they encounter this run"}
                {:msg "prevent the Runner from jacking out until after the next piece of ICE"
                 :effect (effect (register-events
                                   {:pass-ice {:effect (req (swap! state update-in [:run] dissoc :prevent-jack-out)
                                                            (unregister-events state side card))}}
                                   card)
                                 (prevent-jack-out))}]}

   "Information Overload"
   (let [ability {:effect (effect (reset-variable-subs card (count-tags state) trash-installed))}]
     {:implementation "Encounter effect is manual"
      :events {:rez (assoc ability :req (req (same-card? card target)))
               :runner-gain-tag ability
               :runner-lose-tag ability}
      :abilities [(tag-trace 1)]})

   "Interrupt 0"
   (let [sub {:label "Make the Runner pay 1 [Credits] to use icebreaker"
              :msg "For the remainder of this run, the Runner must pay 1 [Credits] as an additional cost each time they use an icebreaker to break at least 1 subroutine."}]
     {:subroutines [sub sub]})

   "IP Block"
   {:abilities [(assoc (give-tags 1)
                       :req (req (seq (filter #(has-subtype? % "AI") (all-active-installed state :runner))))
                       :label "Give the Runner 1 tag if there is an installed AI")]
    :subroutines [(tag-trace 3)
                  end-the-run-if-tagged]}

   "IQ"
   {:effect (req (add-watch state (keyword (str "iq" (:cid card)))
                            (fn [k ref old new]
                              (let [handsize (count (get-in new [:corp :hand]))]
                                (when (not= (count (get-in old [:corp :hand])) handsize)
                                  (update! ref side (assoc (get-card ref card) :strength-bonus handsize))
                                  (update-ice-strength ref side (get-card ref card)))))))
    :subroutines [end-the-run]
    :strength-bonus (req (count (:hand corp)))
    :rez-cost-bonus (req (count (:hand corp)))
    :leave-play (req (remove-watch state (keyword (str "iq" (:cid card)))))}

   "Ireress"
   (let [sub {:msg "make the Runner lose 1 [Credits]"
              :effect (effect (lose-credits :runner 1))}
         ability {:effect (effect (reset-variable-subs card (count-bad-pub state) sub))}]
     {:events {:corp-gain-bad-publicity ability
               :corp-lose-bad-publicity ability}})

   "It's a Trap!"
   {:expose {:msg "do 2 net damage"
             :async true
             :effect (effect (damage eid :net 2 {:card card}))}
    :subroutines [(assoc trash-installed :effect (req (trash state side target {:cause :subroutine})
                                                      (when current-ice
                                                        (no-action state side nil)
                                                        (continue state side nil))
                                                      (trash state side card)))]}

   "Janus 1.0"
   {:subroutines [(do-brain-damage 1)
                  (do-brain-damage 1)
                  (do-brain-damage 1)
                  (do-brain-damage 1)]
    :runner-abilities [(bioroid-break 1 1)]}

   "Jua"
   {:implementation "Encounter effect is manual"
    :abilities [{:msg "prevent the Runner from installing cards for the rest of the turn"
                 :effect (effect (register-turn-flag! card :runner-lock-install (constantly true)))}]
    :subroutines [{:label "Choose 2 installed Runner cards, if able. The Runner must add 1 of those to the top of the Stack."
                   :req (req (>= (count (all-installed state :runner)) 2))
                   :async true
                   :prompt "Select 2 installed Runner cards"
                   :choices {:req #(and (runner? %)
                                        (installed? %))
                             :max 2
                             :all true}
                   :msg (msg "add either " (card-str state (first targets)) " or " (card-str state (second targets)) " to the Stack")
                   :effect (req (when (= (count targets) 2)
                                  (show-wait-prompt state :corp "Runner to decide which card to move")
                                  (continue-ability
                                    state :runner
                                    {:player :runner
                                     :priority 1
                                     :prompt "Select a card to move to the Stack"
                                     :choices targets ;{:req (fn [x] (some #(= % x) targets))} - Alternative version
                                     :effect (req (let [c target]
                                                    (clear-wait-prompt state :corp)
                                                    (move state :runner c :deck {:front true})
                                                    (system-msg state :runner (str "selected " (card-str state c) " to move to the Stack"))))}
                                    card nil)))}]}

   "Kakugo"
   {:events {:pass-ice {:async true
                        :req (req (same-card? target card))
                        :msg "do 1 net damage"
                        :effect (effect (damage eid :net 1 {:card card}))}}
    :subroutines [end-the-run]}

   "Kamali 1.0"
   (letfn [(better-name [kind] (if (= "hardware" kind) "piece of hardware" kind))
           (runner-trash [kind]
             {:prompt (str "Select an installed " (better-name kind) " to trash")
              :label (str "Trash an installed " (better-name kind))
              :msg (msg "trash " (:title target))
              :async true
              :choices {:req #(and (installed? %)
                                   (is-type? % (capitalize kind)))}
              :cancel-effect (effect (system-msg (str "fails to trash an installed " (better-name kind)))
                                     (effect-completed eid))
              :effect (effect (trash eid target {:cause :subroutine}))})
           (sub-map [kind]
             {:player :runner
              :async true
              :prompt "Choose one"
              :choices ["Take 1 brain damage" (str "Trash an installed " (better-name kind))]
              :effect (req (if (= target "Take 1 brain damage")
                             (do (system-msg state :corp "uses Kamali 1.0 to give the Runner 1 brain damage")
                                 (damage state :runner eid :brain 1 {:card card}))
                             (continue-ability state :runner (runner-trash kind) card nil)))})
           (brain-trash [kind]
             {:label (str "Force the Runner to take 1 brain damage or trash an installed " (better-name kind))
              :msg (str "force the Runner to take 1 brain damage or trash an installed " (better-name kind))
              :async true
              :effect (req (show-wait-prompt state :corp "Runner to decide on Kamali 1.0 action")
                           (wait-for (resolve-ability state side (sub-map kind) card nil)
                                     (clear-wait-prompt state :corp)))})]
     {:subroutines [(brain-trash "resource")
                    (brain-trash "hardware")
                    (brain-trash "program")]
      :runner-abilities [(bioroid-break 1 1)]})

   "Kitsune"
   {:subroutines [{:prompt "Select a card in HQ to force access"
                   :choices {:req in-hand?}
                   :label "Force the Runner to access a card in HQ"
                   :msg (msg "force the Runner to access " (:title target))
                   :effect (req (trash state side card)
                                (wait-for (trigger-event-sync state side :pre-access :hq)
                                          (wait-for (access-card state side target)
                                                    (let [from-hq (dec (access-count state side :hq-access))]
                                                      (continue-ability
                                                        state :runner
                                                        (access-helper-hq
                                                          state from-hq
                                                          ;; access-helper-hq uses a set to keep track of which cards have already
                                                          ;; been accessed. by adding HQ root's contents to this set, we make the runner
                                                          ;; unable to access those cards, as Kitsune intends.
                                                          (conj (set (get-in @state [:corp :servers :hq :content])) target))
                                                        card nil)))))}]}

   "Komainu"
   (let [sub (do-net-damage 1)]
     {:implementation "Encounter effect is manual"
      :events {:run-ends {:effect (effect (reset-variable-subs card 0 nil))}}
      :abilities [{:label "Reset subroutines"
                   :effect (effect (reset-variable-subs card (count (:hand runner)) sub))}]})

   "Lab Dog"
   {:subroutines [(assoc trash-hardware
                         :label "Force the Runner to trash an installed piece of hardware"
                         :player :runner
                         :msg (msg "force the Runner to trash " (:title target))
                         :effect (req (trash state side target)
                                      (when current-ice
                                        (no-action state side nil)
                                        (continue state side nil))
                                      (trash state side card)))]}

   "Lancelot"
   (grail-ice trash-program)

   "Little Engine"
   {:subroutines [end-the-run end-the-run
                  {:msg "make the Runner gain 5 [Credits]"
                   :effect (effect (gain-credits :runner 5))}]}

   "Lockdown"
   {:subroutines [{:label "The Runner cannot draw cards for the remainder of this turn"
                   :msg "prevent the Runner from drawing cards"
                   :effect (effect (prevent-draw))}]}

   "Loki"
   {:implementation "Encounter effects not implemented"
    :subroutines [{:label "End the run unless the Runner shuffles their Grip into the Stack"
                   :effect (req (if (zero? (count (:hand runner)))
                                  (do (end-run state side)
                                      (system-msg state :corp (str "uses Loki to end the run")))
                                  (do (show-wait-prompt state :corp "Runner to decide to shuffle their Grip into the Stack")
                                      (resolve-ability
                                        state :runner
                                        {:optional
                                         {:prompt "Reshuffle your Grip into the Stack?"
                                          :player :runner
                                          :yes-ability {:effect (req (doseq [c (:hand runner)]
                                                                       (move state :runner c :deck))
                                                                     (shuffle! state :runner :deck)
                                                                     (system-msg state :runner (str "shuffles their Grip into their Stack"))
                                                                     (clear-wait-prompt state :corp))}
                                          :no-ability {:effect (effect (end-run)
                                                                       (system-msg :runner (str "doesn't shuffle their Grip into their Stack. Loki ends the run"))
                                                                       (clear-wait-prompt :corp))}}}
                                        card nil))))}]}

   "Loot Box"
   (letfn [(top-3 [state] (take 3 (get-in @state [:runner :deck])))
           (top-3-names [state] (map :title (top-3 state)))]
     {:subroutines [{:label "End the run unless the Runner pays 2 [Credits]"
                     :msg "force the Runner to pay 2 [Credits] or end the run"
                     :player :runner
                     :prompt "Choose one"
                     :choices ["Pay 2 [Credits]" "End the run"]
                     :effect (req (if (= target "Pay 2 [Credits]")
                                    (do (pay state :runner card :credit 2)
                                        (system-msg state side "pays 2 [Credits]"))
                                    (end-run state side)))}
                    {:label "Reveal the top 3 cards of the Stack"
                     :effect (effect (system-msg (str "uses Loot Box to reveal the top 3 cards of the stack: "
                                                      (join ", " (top-3-names state))))
                               (reveal (top-3 state))
                               (show-wait-prompt :runner "Corp to choose a card to add to the Grip")
                               (continue-ability
                                 {:prompt "Choose a card to add to the Grip"
                                  :msg (msg "add " (:title target) " to the Grip, gain " (:cost target)
                                            " [Credits] and shuffle the Stack. Loot Box is trashed")
                                  :choices (req (top-3 state))
                                  :effect (effect (move :runner target :hand)
                                                  (gain-credits :corp (:cost target))
                                                  (shuffle! :runner :deck)
                                                  (trash card)
                                                  (clear-wait-prompt :runner))} card nil))}]})

   "Lotus Field"
   {:subroutines [end-the-run]
    :flags {:cannot-lower-strength true}}

   "Lycan"
   (morph-ice "Sentry" "Code Gate" trash-program)

   "Macrophage"
   {:subroutines [(trace-ability 4 {:label "Purge virus counters"
                                    :msg "purge virus counters"
                                    :effect (effect (purge))})
                  (trace-ability 3 {:label "Trash a virus"
                                    :prompt "Choose a virus to trash"
                                    :msg (msg "trash " (:title target))
                                    :choices {:req #(and (installed? %)
                                                         (has-subtype? % "Virus"))}
                                    :effect (effect (trash target {:cause :subroutine})
                                                    (clear-wait-prompt :runner))})
                  (trace-ability 2 {:label "Remove a virus in the Heap from the game"
                                    :prompt "Choose a virus in the Heap to remove from the game"
                                    :choices (req (cancellable (filter #(has-subtype? % "Virus") (:discard runner)) :sorted))
                                    :msg (msg "remove " (:title target) " from the game")
                                    :effect (effect (move :runner target :rfg))})
                  (trace-ability 1 end-the-run)]}

   "Magnet"
   (letfn [(disable-hosted [state side c]
             (doseq [hc (:hosted (get-card state c))]
               (unregister-events state side hc)
               (update! state side (dissoc hc :abilities))))]
     {:async true
      :effect (req (let [magnet card]
                     (wait-for (resolve-ability
                                 state side
                                 {:req (req (some #(some program? (:hosted %))
                                                  (remove-once #(same-card? % magnet)
                                                               (filter ice? (all-installed state corp)))))
                                  :prompt "Select a Program to host on Magnet"
                                  :choices {:req #(and (program? %)
                                                       (ice? (:host %))
                                                       (not (same-card? (:host %) magnet)))}
                                  :effect (effect (host card target))}
                                 card nil)
                               (disable-hosted state side card))))
      :derez-effect {:req (req (not-empty (:hosted card)))
                     :effect (req (doseq [c (get-in card [:hosted])]
                                    (card-init state side c {:resolve-effect false})))}
      :events {:runner-install {:req (req (same-card? card (:host target)))
                                :effect (req (disable-hosted state side card)
                                          (update-ice-strength state side card))}}
      :subroutines [end-the-run]})

   "Mamba"
   {:abilities [(power-counter-ability (do-net-damage 1))]
    :subroutines [(do-net-damage 1)
                  (do-psi {:label "Add 1 power counter"
                           :msg "add 1 power counter"
                           :effect (effect (add-counter card :power 1)
                                           (effect-completed eid))})]}

   "Marker"
   {:implementation "Subroutine-adding is manual"
    :events {:run-ends
             {:effect (req (let [cid (:cid card)
                                 ices (get-in card [:special :marker])]
                             (doseq [i ices]
                               (when-let [ice (get-card state i)]
                                 (remove-sub! state side ice #(= cid (:from-cid %))))))
                           (update! state side (dissoc-in card [:special :marker])))}}
    :abilities [{:label "Give an End the run subroutine"
                 :choices {:req #(and (ice? %)
                                      (installed? %)
                                      (rezzed? %))}
                 :effect (effect (add-sub! target end-the-run (:cid card) {:back true})
                                 (update! (update-in card [:special :marker] #(conj % target))))}]
    :subroutines [{:label "Give the next ICE encountered \"End the run\""
                   :msg (str "give the next ICE encountered \"[Subroutine] End the run\" "
                             "after all its other subroutines for the remainder of the run")}]}

   "Markus 1.0"
   {:subroutines [trash-installed end-the-run]
    :runner-abilities [(bioroid-break 1 1)]}

   "Masvingo"
   (let [ability {:req (req (same-card? card target))
                  :effect (effect (reset-variable-subs card (get-counters card :advancement) end-the-run))}]
     {:advanceable :always
      :effect (effect (add-prop card :advance-counter 1))
      :events {:advance ability
               :advancement-placed ability
               :rez ability}})

   "Matrix Analyzer"
   {:implementation "Encounter effect is manual"
    :abilities [{:label "Place 1 advancement token on a card that can be advanced"
                 :msg (msg "place 1 advancement token on " (card-str state target))
                 :choices {:req can-be-advanced?}
                 :cost [:credit 1] :effect (effect (add-prop target :advance-counter 1))}]
    :subroutines [(tag-trace 2)]}

   "Mausolus"
   {:advanceable :always
    :subroutines [{:label "Gain 1 [Credits] (Gain 3 [Credits])"
                   :msg (msg "gain " (if (wonder-sub card 3) 3 1) "[Credits]")
                   :effect (effect (gain-credits (if (wonder-sub card 3) 3 1)))}
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
                                (when (wonder-sub card 3)
                                  (end-run state side)))}]}

   "Meridian"
   {:subroutines [{:label "Gain 4 [Credits] and end the run, unless the runner adds Meridian to their score area as an agenda worth -1 agenda points"
                   :async true
                   :effect (req (show-wait-prompt state :corp "Runner to choose an option for Meridian")
                                (continue-ability
                                  state :runner
                                  {:prompt "Choose one"
                                   :choices ["End the run" "Add Meridian to score area"]
                                   :player :runner
                                   :async true
                                   :effect (req (if (= target "End the run")
                                                  (do (system-msg state :corp (str "uses Meridian to gain 4 [Credits] and end the run"))
                                                      (clear-wait-prompt state :corp)
                                                      (gain-credits state :corp 4)
                                                      (end-run state :runner eid))
                                                  (do (system-msg state :runner (str "adds Meridian to their score area as an agenda worth -1 agenda points"))
                                                      (clear-wait-prompt state :corp)
                                                      (wait-for (as-agenda state :runner card -1)
                                                                (when current-ice
                                                                  (no-action state side nil)
                                                                  (continue state side nil))
                                                                (effect-completed state side eid)))))}
                                  card nil))}]}

   "Merlin"
   (grail-ice (do-net-damage 2))

   "Meru Mati"
   {:subroutines [end-the-run]
    :strength-bonus (req (if (= (second (:zone card)) :hq) 3 0))}

   "Metamorph"
   {:subroutines [{:label "Swap two ICE or swap two installed non-ICE"
                   :msg "swap two ICE or swap two installed non-ICE"
                   :async true
                   :prompt "Choose one"
                   :choices ["Swap two ICE" "Swap two non-ICE"]
                   :effect (req (if (= target "Swap two ICE")
                                  (continue-ability
                                    state side
                                    {:prompt "Select the two ICE to swap"
                                     :async true
                                     :choices {:req #(and (installed? %) (ice? %)) :max 2 :all true}
                                     :msg (msg "swap the positions of " (card-str state (first targets)) " and " (card-str state (second targets)))
                                     :effect (req (when (= (count targets) 2)
                                                    (swap-ice state side (first targets) (second targets))
                                                    (effect-completed state side eid)))}
                                    card nil)
                                  (continue-ability
                                    state side
                                    {:prompt "Select the two cards to swap"
                                     :async true
                                     :choices {:req #(and (installed? %) (not (ice? %))) :max 2 :all true}
                                     :msg (msg "swap the positions of " (card-str state (first targets)) " and " (card-str state (second targets)))
                                     :effect (req (when (= (count targets) 2)
                                                    (swap-installed state side (first targets) (second targets))
                                                    (effect-completed state side eid)))}
                                    card nil)))}]}

   "Mganga"
   {:subroutines [(do-psi {:label "do 2 net damage"
                           :player :corp
                           :effect (req (wait-for (damage state :corp :net 2 {:card card})
                                                  (trash state :corp eid card nil)))}
                          {:label "do 1 net damage"
                           :player :corp
                           :effect (req (wait-for (damage state :corp :net 1 {:card card})
                                                  (trash state :corp eid card nil)))})]}

   "Mind Game"
   {:subroutines [(do-psi {:label "Redirect the run to another server"
                           :player :corp
                           :prompt "Choose a server"
                           :choices (req (remove #{(-> @state :run :server central->name)} servers))
                           :msg (msg "redirect the run to " target)
                           :effect (req (let [dest (server->zone state target)]
                                          (swap! state update-in [:run]
                                                 #(assoc % :position (count (get-in corp (conj dest :ices)))
                                                           :server (rest dest))))
                                        (effect-completed state side eid))})]
    :runner-abilities [{:label "Add an installed card to the bottom of your Stack"
                        :prompt "Choose one of your installed cards"
                        :choices {:req #(and (installed? %)
                                             (runner? %))}
                        :effect (effect (move target :deck)
                                        (system-msg :runner (str "adds " (:title target) " to the bottom of their Stack")))}]}

   "Minelayer"
   {:subroutines [{:msg "install an ICE from HQ"
                   :choices {:req #(and (ice? %)
                                        (in-hand? %))}
                   :prompt "Choose an ICE to install from HQ"
                   :effect (req (corp-install state side target (zone->name (first (:server run))) {:ignore-all-cost true}))}]}

   "Mirāju"
   {:abilities [{:label "Runner broke subroutine: Redirect run to Archives"
                 :msg "make the Runner continue the run on Archives. Mirāju is derezzed"
                 :effect (req (swap! state update-in [:run]
                                     #(assoc % :position (count (get-in corp [:servers :archives :ices]))
                                               :server [:archives]))
                              (derez state side card))}]
    :subroutines [{:label "Draw 1 card, then shuffle 1 card from HQ into R&D"
                   :effect (req (wait-for (resolve-ability
                                            state side
                                            {:optional
                                             {:prompt "Draw 1 card?"
                                              :yes-ability {:async true
                                                            :msg "draw 1 card"
                                                            :effect (effect (draw eid 1 nil))}}}
                                            card nil)
                                          (resolve-ability
                                            state side
                                            {:prompt "Choose 1 card in HQ to shuffle into R&D"
                                             :choices {:req #(and (in-hand? %) (corp? %))}
                                             :msg "shuffle 1 card in HQ into R&D"
                                             :effect (effect (move target :deck)
                                                             (shuffle! :deck))}
                                            card nil)))}]}

   "Mlinzi"
   (letfn [(net-or-trash [net-dmg mill-cnt]
             {:label (str "Do " net-dmg " net damage")
              :effect (req (show-wait-prompt state :corp "Runner to choose an option for Mlinzi")
                           (resolve-ability
                             state :runner
                             {:prompt "Take net damage or trash cards from the stack?"
                              :choices [(str "Take " net-dmg " net damage")
                                        (str "Trash the top " mill-cnt " cards of the stack")]
                              :effect (req (if (.startsWith target "Take")
                                             (do (system-msg state :corp
                                                             (str "uses Mlinzi to do "
                                                                  net-dmg " net damage"))
                                                 (clear-wait-prompt state :corp)
                                                 (damage state :runner eid :net net-dmg {:card card}))
                                             (do (system-msg state :corp
                                                             (str "uses Mlinzi to trash "
                                                                  (join ", " (map :title (take mill-cnt (:deck runner))))
                                                                  " from the runner's stack"))
                                                 (clear-wait-prompt state :corp)
                                                 (mill state :runner mill-cnt))))}
                             card nil))})]
     {:subroutines [(net-or-trash 1 2)
                    (net-or-trash 2 3)
                    (net-or-trash 3 4)]})

   "Mother Goddess"
   (let [ab (effect (update! (let [subtype (->> (mapcat :ices (flatten (seq (:servers corp))))
                                                (filter #(and (rezzed? %)
                                                              (not (same-card? card %))))
                                                (mapcat #(split (:subtype %) #" - "))
                                                (cons "Mythic")
                                                distinct
                                                (join " - "))]
                               (assoc card
                                      :subtype-target (remove-subtypes subtype "Mythic")
                                      :subtype subtype))))
         mg {:req (req (ice? target))
             :effect ab}]
     {:effect ab
      :subroutines [end-the-run]
      :events {:rez mg
               :card-moved mg
               :derez mg
               :ice-subtype-changed mg}})

   "Muckraker"
   {:effect take-bad-pub
    :subroutines [(tag-trace 1)
                  (tag-trace 2)
                  (tag-trace 3)
                  end-the-run-if-tagged]}

   "Najja 1.0"
   {:subroutines [end-the-run end-the-run]
    :runner-abilities [(bioroid-break 1 1)]}

   "Nebula"
   (space-ice trash-program)

   "Negotiator"
   {:subroutines [(gain-credits-sub 2)
                  trash-program]
    :runner-abilities [(bioroid-break 2 1)]}

   "Nerine 2.0"
   (let [sub {:label "Do 1 brain damage and Corp may draw 1 card"
              :async true
              :msg "do 1 brain damage"
              :effect (req (wait-for (damage state :runner :brain 1 {:card card})
                                     (resolve-ability
                                       state side
                                       {:optional
                                        {:prompt "Draw 1 card?"
                                         :yes-ability {:async true
                                                       :msg "draw 1 card"
                                                       :effect (effect (draw eid 1 nil))}}}
                                       card nil)))}]
     {:subroutines [sub sub]
      :runner-abilities [(bioroid-break 2 2)]})

   "Neural Katana"
   {:subroutines [(do-net-damage 3)]}

   "News Hound"
   (let [ab {:req (req (has-subtype? target "Current"))
             :msg "make News Hound gain \"[subroutine] End the run\""
             :effect (effect (continue-ability
                               (reset-variable-subs state side card 1 end-the-run {:back true})
                               card nil))}]
     {:effect (effect (continue-ability
                        (when (pos? (count (concat (get-in @state [:corp :current])
                                                   (get-in @state [:runner :current]))))
                          (reset-variable-subs state side card 1 end-the-run {:back true}))
                        card nil))
      :events {:play-event ab
               :play-operation ab
               :trash-current {:msg "make News Hound lose \"[subroutine] End the run\""
                               :effect (effect (continue-ability
                                                 (reset-variable-subs state side card 0 nil)
                                                 card nil))}}
      :subroutines [(tag-trace 3)]})

   "NEXT Bronze"
   {:subroutines [end-the-run]
    :strength-bonus (req (next-ice-count corp))
    :events (let [nb {:req (req (and (not (same-card? target card))
                                     (has-subtype? target "NEXT")))
                      :effect (effect (update-ice-strength card))}]
              {:rez nb
               :derez nb
               :trash nb
               :card-moved nb})}

   "NEXT Diamond"
   {:rez-cost-bonus (req (- (next-ice-count corp)))
    :subroutines [(do-brain-damage 1)
                  (do-brain-damage 1)
                  {:prompt "Select a card to trash"
                   :label "Trash 1 installed Runner card"
                   :msg (msg "trash " (:title target))
                   :choices {:req #(and (installed? %)
                                        (runner? %))}
                   :async true
                   :effect (req (trash state side eid target {:cause :subroutine}))}]}

   "NEXT Gold"
   {:subroutines [{:label "Do 1 net damage for each rezzed NEXT ice"
                   :msg (msg "do " (next-ice-count corp) " net damage")
                   :effect (effect (damage eid :net (next-ice-count corp) {:card card}))}
                  trash-program]}

   "NEXT Opal"
   (let [sub {:label "Install a card from HQ, paying all costs"
              :prompt "Choose a card in HQ to install"
              :req (req (some #(not (operation? %)) (:hand corp)))
              :choices {:req #(and (not (operation? %))
                                   (in-hand? %)
                                   (corp? %))}
              :effect (effect (corp-install eid target nil nil))
              :msg (msg (corp-install-msg target))}
         ability {:req (req (and (ice? target)
                                 (has-subtype? target "NEXT")))
                  :effect (effect (reset-variable-subs card (next-ice-count corp) sub))}]
     {:events {:rez ability
               :derez ability}})

   "NEXT Sapphire"
   {:subroutines [{:label "Draw up to X cards"
                   :prompt "Draw how many cards?"
                   :msg (msg "draw " target " cards")
                   :choices {:number (req (next-ice-count corp))
                             :default (req 1)}
                   :async true
                   :effect (effect (draw eid target nil))}
                  {:label "Add up to X cards from Archives to HQ"
                   :prompt "Select cards to add to HQ"
                   :show-discard  true
                   :choices {:req #(and (corp? %)
                                        (in-discard? %))
                             :max (req (next-ice-count corp))}
                   :effect (req (doseq [c targets]
                                  (move state side c :hand)))
                   :msg (msg "add "
                             (let [seen (filter :seen targets)
                                   m (count (filter #(not (:seen %)) targets))]
                               (str (join ", " (map :title seen))
                                    (when (pos? m)
                                      (str (when-not (empty? seen) " and ")
                                           (quantify m "unseen card")))))
                             " to HQ")}
                  {:label "Shuffle up to X cards from HQ into R&D"
                   :prompt "Select cards to shuffle into R&D"
                   :choices {:req #(and (corp? %)
                                        (in-hand? %))
                             :max (req (next-ice-count corp))}
                   :effect (req (doseq [c targets]
                                  (move state :corp c :deck))
                                (shuffle! state :corp :deck))
                   :cancel-effect (effect (shuffle! :corp :deck))
                   :msg (msg "shuffle " (count targets) " cards from HQ into R&D")}]}

   "NEXT Silver"
   (let [ability {:req (req (and (ice? target)
                                 (has-subtype? target "NEXT")))
                  :effect (effect (reset-variable-subs card (next-ice-count corp) end-the-run))}]
     {:events {:rez ability
               :derez ability}})

   "Nightdancer"
   (let [sub {:label (str "The Runner loses [Click], if able. "
                          "You have an additional [Click] to spend during your next turn.")
              :msg (str "force the runner to lose a [Click], if able. "
                        "Corp gains an additional [Click] to spend during their next turn")
              :effect (req (lose state :runner :click 1)
                           (swap! state update-in [:corp :extra-click-temp] (fnil inc 0)))}]
     {:subroutines [sub sub]})

   "Oduduwa"
   {:implementation "Encounter effect is manual"
    :abilities [{:label "Place 1 advancement counter on Oduduwa"
                 :msg (msg "place 1 advancement counter on Oduduwa")
                 :effect (req (add-prop state side card :advance-counter 1 {:placed true}))}
                {:label "Place X advancement token on another piece of ice"
                 :msg (msg "place " (get-counters card :advancement) " advancement token on " (card-str state target))
                 :choices {:req ice?
                           :not-self true}
                 :effect (req (add-prop state side target :advance-counter (get-counters card :advancement) {:placed true}))}]
    :subroutines [end-the-run end-the-run]}

   "Orion"
   (space-ice trash-program
              (resolve-another-subroutine)
              end-the-run)

   "Otoroshi"
   {:subroutines [{:async true
                   :label "Place 3 advancement tokens on installed card"
                   :msg "place 3 advancement tokens on installed card"
                   :prompt "Choose an installed Corp card"
                   :choices {:req #(and (corp? %)
                                        (installed? %))}
                   :effect (req (let [c target
                                      title (if (:rezzed c)
                                              (:title c)
                                              "selected unrezzed card")]
                                  (add-counter state side c :advancement 3)
                                  (show-wait-prompt state side "Runner to resolve Otoroshi")
                                  (continue-ability
                                    state side
                                    {:player :runner
                                     :async true
                                     :prompt (str "Access " title " or pay 3 [Credits]?")
                                     :choices (concat ["Access card"]
                                                      (when (>= (:credit runner) 3)
                                                        ["Pay 3 [Credits]"]))
                                     :msg (msg "force the Runner to "
                                               (if (= target "Access card")
                                                 (str "access " title)
                                                 "pay 3 [Credits]"))
                                     :effect (req (clear-wait-prompt state :corp)
                                                  (if (= target "Access card")
                                                    (access-card state :runner eid c)
                                                    (pay-sync state :runner eid card :credit 3)))}
                                    card nil)))}]}

   "Owl"
   {:subroutines [{:choices {:req #(and (installed? %)
                                        (program? %))}
                   :label "Add installed program to the top of the Runner's Stack"
                   :msg "add an installed program to the top of the Runner's Stack"
                   :effect (effect (move :runner target :deck {:front true})
                                   (system-msg (str "adds " (:title target) " to the top of the Runner's Stack")))}]}

   "Pachinko"
   {:subroutines [end-the-run-if-tagged
                  end-the-run-if-tagged]}

   "Paper Wall"
   {:implementation "Trash on break is manual"
    :subroutines [end-the-run]}

   "Peeping Tom"
   (let [sub {:label "End the run unless Runner takes 1 tag"
              :async true
              :effect (req (show-wait-prompt state :corp "Runner deciding about Peeping Tom")
                           (continue-ability
                             state :runner
                             {:prompt "Take 1 tag or end the run?"
                              :choices ["Take 1 tag" "End the run"]
                              :async true
                              :effect (req (clear-wait-prompt state :corp)
                                           (if (= target "Take 1 tag")
                                             (do (system-msg state :runner "chooses to take 1 tag from Peeping Tom")
                                                 (gain-tags state :runner eid 1))
                                             (do (system-msg state :runner "chooses to end the run")
                                                 (end-run state :runner eid card))))}
                             card nil))}]
     {:implementation "Encounter effect is manual"
      :events {:run-ends {:effect (effect (reset-variable-subs card 0 nil))}}
      :abilities [{:label "Name a card type and reveal all cards in the Runner's Grip"
                   :prompt "Choose a card type"
                   :choices ["Event" "Hardware" "Program" "Resource"]
                   :effect (req (let [n (count (filter #(is-type? % target) (:hand runner)))]
                                  (system-msg state side
                                              (str "uses Peeping Tom to name " target ", then reveals "
                                                   (join ", " (map :title (:hand runner)))
                                                   " in the Runner's Grip. Peeping Tom gains " n " subroutines"))
                                  (reveal state side (:hand runner))
                                  (reset-variable-subs state side card n sub)))}]})

   "Pop-up Window"
   {:implementation "Encounter effect is manual."
    :abilities [(gain-credits-sub 1)]
    :subroutines [(end-the-run-unless-runner-pays 1)]}

   "Pup"
   (let [sub {:player :runner
              :async true
              :label (str "Do 1 net damage unless the Runner pays 1 [Credits]")
              :prompt (str "Suffer 1 net damage or pay 1 [Credits]?")
              :choices ["Suffer 1 net damage"
                        "Pay 1 [Credits]"]
              :effect (req (if (= "Suffer 1 net damage" target)
                             (continue-ability state side (do-net-damage 1) card nil)
                             (do (pay state :runner :credit 1)
                                 (effect-completed state side eid))))}]
     {:subroutines [sub sub]})

   "Quandary"
   {:subroutines [end-the-run]}

   "Quicksand"
   {:implementation "Encounter effect is manual"
    :abilities [{:req (req (= current-ice card))
                 :label "Add 1 power counter"
                 :effect (effect (add-counter card :power 1)
                                 (update-all-ice))}]
    :subroutines [end-the-run]
    :strength-bonus (req (get-counters card :power))}

   "Rainbow"
   {:subroutines [end-the-run]}

   "Ravana 1.0"
   (let [sub (resolve-another-subroutine
               #(has-subtype? % "Bioroid")
               "Resolve a subroutine on a rezzed bioroid ice")]
     {:subroutines [sub sub]
      :runner-abilities [(bioroid-break 1 1)]})

   "Red Tape"
   {:subroutines [{:label "Give +3 strength to all ICE for the remainder of the run"
                   :msg "give +3 strength to all ICE for the remainder of the run"
                   :effect (effect (register-events
                                     {:pre-ice-strength {:effect (effect (ice-strength-bonus 3 target))}
                                      :run-ends {:effect (effect (unregister-events card))}}
                                     card)
                                   (update-all-ice))}]
    :events {:pre-ice-strength nil :run-ends nil}}

   "Resistor"
   (let [resistor-effect {:effect (effect (update! (assoc (get-card state card) :strength-bonus (count-tags state)))
                                          (update-ice-strength (get-card state card)))}]
     {:events {:runner-gain-tag resistor-effect
               :runner-lose-tag resistor-effect
               :runner-additional-tag-change resistor-effect}
      :strength-bonus (req (count-tags state))
      :subroutines [(trace-ability 4 end-the-run)]})

   "Rime"
   {:implementation "Can be rezzed anytime already"
    :effect (effect (update-all-ice))
    :subroutines [{:label "Runner loses 1 [Credit]"
                   :msg "force the Runner to lose 1 [Credit]"
                   :effect (effect (lose-credits :runner 1))}]
    :events {:corp-moved {:req (req (ice? target))
                          :effect (effect (update-ice-strength target))}
             :corp-install {:req (req (ice? target))
                            :effect (effect (update-ice-strength target))}
             :pre-ice-strength {:req (req (and (ice? target)
                                               (protecting-same-server? card target)))
                                :effect (effect (ice-strength-bonus 1 target))}}}

   "Rototurret"
   {:subroutines [trash-program
                  end-the-run]}

   "Sadaka"
   (let [maybe-draw-effect
         {:async true
          :effect (req (show-wait-prompt state :runner "Corp to decide on Sadaka card draw action")
                       (continue-ability
                         state side
                         {:optional
                          {:player :corp
                           :prompt "Draw 1 card?"
                           :yes-ability
                           {:async true
                            :effect (effect (clear-wait-prompt :runner)
                                            (draw eid 1 nil))
                            :msg "draw 1 card"}
                           :no-ability {:effect (effect (clear-wait-prompt :runner)
                                                        (effect-completed eid))}}}
                         card nil))}]
     {:subroutines [{:label "Look at the top 3 cards of R&D"
                     :req (req (not-empty (:deck corp)))
                     :async true
                     :effect (req (let [top-cards (take 3 (:deck corp))
                                        top-names (map :title top-cards)]
                                    (show-wait-prompt state :runner "Corp to decide on Sadaka R&D card actions")
                                    (continue-ability
                                      state side
                                      {:prompt (str "Top 3 cards of R&D: " (clojure.string/join ", " top-names))
                                       :choices ["Arrange cards" "Shuffle R&D"]
                                       :async true
                                       :effect
                                       (req (if (= target "Arrange cards")
                                              (wait-for
                                                (resolve-ability state side (reorder-choice :corp top-cards) card nil)
                                                (do
                                                  (system-msg state :corp (str "rearranges the top "
                                                                               (quantify (count top-cards) "card")
                                                                               " of R&D"))
                                                  (clear-wait-prompt state :runner)
                                                  (continue-ability state side maybe-draw-effect card nil)))
                                              (do
                                                (shuffle! state :corp :deck)
                                                (system-msg state :corp (str "shuffles R&D"))
                                                (clear-wait-prompt state :runner)
                                                (continue-ability state side maybe-draw-effect card nil))))}
                                      card nil)))}
                    {:label "Trash 1 card in HQ"
                     :async true
                     :effect
                     (req (show-wait-prompt state :runner "Corp to select cards to trash with Sadaka")
                          (wait-for
                            (resolve-ability
                              state side
                              {:prompt "Choose a card in HQ to trash"
                               :choices (req (cancellable (:hand corp) :sorted))
                               :async true
                               :cancel-effect (effect (system-msg "chooses not to trash a card from HQ")
                                                      (effect-completed eid))
                               :effect (req (wait-for
                                              (trash state :corp (make-eid state) target nil)
                                              (do
                                                (system-msg state :corp "trashes a card from HQ")
                                                (wait-for
                                                  (resolve-ability state side trash-resource-sub card nil)
                                                  (effect-completed state side eid)))))}
                              card nil)
                            (do
                              (system-msg state :corp "trashes Sadaka")
                              (clear-wait-prompt state :runner)
                              (when current-ice
                                (no-action state side nil)
                                (continue state side nil))
                              (trash state :corp eid card nil))))}]})

   "Sagittarius"
   (constellation-ice trash-program)

   "Saisentan"
   (let [sub {:label "Do 1 net damage"
              :async true
              :msg "do 1 net damage"
              :effect (req (wait-for (damage state side :net 1 {:card card})
                                     (when-let* [choice (get-in card [:special :saisentan])
                                                 cards (some #(when (same-card? (second %) card) (last %))
                                                             (turn-events state :corp :damage))
                                                 dmg (some #(when (= (:type %) choice) %) cards)]
                                                (system-msg state :corp "uses Saisentan to deal a second net damage")
                                                (damage state side eid :net 1 {:card card}))))}]
     {:implementation "Encounter effect is manual"
      :subroutines [sub sub sub]
      :abilities [{:label "Choose card type"
                   :req (req (and (same-card? current-ice card)
                                  (rezzed? card)))
                   :effect (effect (show-wait-prompt :runner "Corp to choose Saisentan card type")
                                   (continue-ability
                                     {:prompt "Choose a card type"
                                      :choices ["Event" "Hardware" "Program" "Resource"]
                                      :msg (msg "choose the card type " target)
                                      :effect (effect (update! (assoc-in card [:special :saisentan] target))
                                                      (clear-wait-prompt :runner))}
                                     card nil))}]})

   "Salvage"
   (zero-to-hero (tag-trace 2))

   "Sand Storm"
   {:subroutines [{:req (req (:run @state))
                   :label "Move Sand Storm and the run to another server"
                   :prompt "Choose another server and redirect the run to its outermost position"
                   :choices (req (cancellable servers))
                   :msg (msg "move Sand Storm and the run.  The Runner is now running on " target ". Sand Storm is trashed")
                   :effect (req (let [dest (server->zone state target)]
                                  (swap! state update-in [:run]
                                         #(assoc % :position (count (get-in corp (conj dest :ices)))
                                                 :server (rest dest)))
                                  (trash state side card {:unpreventable true})))}]}

   "Sandstone"
   {:implementation "Encounter effect is manual"
    :subroutines [end-the-run]
    :strength-bonus (req (- (get-counters card :virus)))
    :abilities [{:label "Place one virus counter"
                 :req (req (same-card? current-ice card))
                 :msg "place 1 virus counter on Sandstone"
                 :effect (effect (add-counter card :virus 1)
                                 (update-ice-strength (get-card state card)))}]}

   "Sandman"
   (let [sub {:label "Add an installed Runner card to the grip"
              :req (req (not-empty (all-installed state :runner)))
              :effect (effect (show-wait-prompt :runner "Corp to select Sandman target")
                              (resolve-ability {:choices {:req #(and (installed? %)
                                                                     (runner? %))}
                                                :msg (msg "to add " (:title target) " to the grip")
                                                :effect (effect (clear-wait-prompt :runner)
                                                                (move :runner target :hand true))
                                                :cancel-effect (effect (clear-wait-prompt :runner))}
                                               card nil))}]
     {:subroutines [sub sub]})

   "Sapper"
   {:flags {:rd-reveal (req true)}
    :subroutines [trash-program]
    :access {:async true
             :req (req (and (not= (first (:zone card)) :discard)
                            (some program? (all-active-installed state :runner))))
             :effect (effect (show-wait-prompt :corp "Runner to decide to break Sapper subroutine")
                             (continue-ability
                               :runner
                               {:optional
                                {:player :runner
                                 :prompt "Allow Sapper subroutine to fire?"
                                 :priority 1
                                 :yes-ability
                                 {:effect (effect (clear-wait-prompt :corp)
                                                  (show-wait-prompt :runner "Corp to trash a program with Sapper")
                                                  (resolve-unbroken-subs! :corp eid card))}
                                 :no-ability
                                 {:effect (effect (clear-wait-prompt :corp)
                                                  (effect-completed eid))}}}
                               card nil))}}

   "Searchlight"
   (let [sub {:label "Trace X - Give the Runner 1 tag"
              :trace {:base advance-counters
                      :label "Give the Runner 1 tag"
                      :successful (give-tags 1)}}]
     {:advanceable :always
      :subroutines [sub sub]})

   "Seidr Adaptive Barrier"
   (let [recalculate-strength (req (update-ice-strength state side (get-card state card)))
         recalc-event {:req (req (= (:zone target) (:zone card)))
                       :effect recalculate-strength}]
     {:effect recalculate-strength
      :strength-bonus (req (count (:ices (card->server state card))))
      :subroutines [end-the-run]
      :events {:card-moved recalc-event
               :corp-install recalc-event}})

   "Self-Adapting Code Wall"
   {:subroutines [end-the-run]
    :flags {:cannot-lower-strength true}}

   "Sensei"
   {:implementation "Subroutine-adding is manual"
    :events {:run-ends
             {:effect (req (let [cid (:cid card)
                                 ices (get-in card [:special :sensei])]
                             (doseq [i ices]
                               (when-let [ice (get-card state i)]
                                 (remove-sub! state side ice #(= cid (:from-cid %))))))
                           (update! state side (dissoc-in card [:special :sensei])))}}
    :abilities [{:label "Give an End the run subroutine"
                 :choices {:req #(and (ice? %)
                                      (installed? %)
                                      (rezzed? %))}
                 :effect (effect (add-sub! target end-the-run (:cid card) {:back true})
                                 (update! (update-in card [:special :sensei] conj target)))}]
    :subroutines [{:label "Give each other ICE encountered \"End the run\""
                   :msg (msg "give each other ICE encountered \"[Subroutine] End the run\" after all its other subroutines for the remainder of the run")}]}

   "Shadow"
   {:advanceable :always
    :subroutines [(gain-credits-sub 2)
                  (tag-trace 3)]
    :strength-bonus advance-counters}

   "Sherlock 1.0"
   {:subroutines [(trace-ability 4 {:choices {:req #(and (installed? %)
                                                         (program? %))}
                                    :label "Add an installed program to the top of the Runner's Stack"
                                    :msg (msg "add " (:title target) " to the top of the Runner's Stack")
                                    :effect (effect (move :runner target :deck {:front true}))})]
    :runner-abilities [(bioroid-break 1 1)]}

   "Sherlock 2.0"
   (let [sub (trace-ability 4 {:choices {:req #(and (installed? %)
                                                    (program? %))}
                               :label "Add an installed program to the bottom of the Runner's Stack"
                               :msg (msg "add " (:title target) " to the bottom of the Runner's Stack")
                               :effect (effect (move :runner target :deck))})]
     {:subroutines [sub sub
                    (give-tags 1)]
      :runner-abilities [(bioroid-break 2 2)]})

   "Shinobi"
   {:effect take-bad-pub
    :subroutines [(trace-ability 1 (do-net-damage 1))
                  (trace-ability 2 (do-net-damage 2))
                  (trace-ability 3 {:label "Do 3 net damage and end the run"
                                    :msg "do 3 net damage and end the run"
                                    :effect (effect (damage eid :net 3 {:card card})
                                                    (end-run))})]}

   "Shiro"
   {:subroutines [{:label "Rearrange the top 3 cards of R&D"
                   :msg "rearrange the top 3 cards of R&D"
                   :async true
                   :effect (req (show-wait-prompt state :runner "Corp to rearrange the top cards of R&D")
                                (let [from (take 3 (:deck corp))]
                                  (if (pos? (count from))
                                    (continue-ability state side (reorder-choice :corp :runner from '()
                                                                                 (count from) from) card nil)
                                    (do (clear-wait-prompt state :runner)
                                        (effect-completed state side eid)))))}
                  {:label "Force the Runner to access the top card of R&D"
                   :async true
                   :effect (req (do-access state :runner eid [:rd] {:no-root true}))}]}

   "Slot Machine"
   (letfn [(name-builder [card] (str (:title card) " (" (:type card) ")"))
           (top-3 [state] (take 3 (get-in @state [:runner :deck])))
           (top-3-names [state] (map name-builder (top-3 state)))
           (top-3-types [state] (->> (top-3 state) (map :type) (into #{}) count))]
     {:implementation "Encounter effect is manual"
      :abilities [{:label "Roll them bones"
                   :req (req (same-card? current-ice card))
                   :effect (effect (move :runner (first (:deck runner)) :deck)
                                   (reveal (take 3 (:deck runner)))
                                   (system-msg (str "uses Slot Machine to put the top card of the stack to the bottom,"
                                                    " then reveal the top 3 cards in the stack: "
                                                    (join ", " (top-3-names state)))))}]
      :subroutines [{:label "Runner loses 3 [Credits]"
                     :msg "force the Runner to lose 3 [Credits]"
                     :effect (effect (lose-credits :runner 3))}
                    {:label "Gain 3 [Credits]"
                     :effect (req (let [unique-types (top-3-types state)]
                                    (when (>= 2 unique-types)
                                      (system-msg state :corp (str "uses Slot Machine to gain 3 [Credits]"))
                                      (gain-credits state :corp 3))))}
                    {:label "Place 3 advancement tokens"
                     :effect (req (let [unique-types (top-3-types state)]
                                    (when (= 1 unique-types)
                                      (continue-ability
                                        state side
                                        {:choices {:req installed?}
                                         :prompt "Choose an installed card"
                                         :msg (msg "place 3 advancement tokens on "
                                                   (card-str state target))
                                         :effect (effect (add-prop target :advance-counter 3 {:placed true}))}
                                        card nil))))}]})

   "Snoop"
   {:implementation "Encounter effect is manual"
    :abilities [{:req (req (= current-ice card))
                 :label "Reveal all cards in the Runner's Grip"
                 :msg (msg "reveal the Runner's Grip ( " (join ", " (map :title (:hand runner))) " )")}
                {:req (req (pos? (get-counters card :power)))
                 :cost [:power 1]
                 :label "Hosted power counter: Reveal all cards in Grip and trash 1 card"
                 :msg (msg "look at all cards in Grip and trash " (:title target)
                           " using 1 power counter")
                 :choices (req (cancellable (:hand runner) :sorted))
                 :prompt "Choose a card to trash"
                 :effect (effect (reveal (:hand runner))
                                 (trash target))}]
    :subroutines [(trace-ability 3 add-power-counter)]}

   "Snowflake"
   {:subroutines [(do-psi {:label "End the run"
                           :msg "end the run"
                           :effect (effect (end-run eid))})]}

   "Special Offer"
   {:subroutines [{:label "Gain 5 [Credits] and trash Special Offer"
                   :effect (req (gain-credits state :corp 5)
                                (when current-ice
                                  (no-action state side nil)
                                  (continue state side nil))
                                (trash state side card)
                                (system-msg state side (str "gains 5 [Credits] and trashes Special Offer")))}]}

   "Spiderweb"
   {:subroutines [end-the-run
                  end-the-run
                  end-the-run]}

   "Surveyor"
   (let [x (req (* 2 (count (:ices (card->server state card)))))
         recalculate-strength (req (update-ice-strength state side (get-card state card)))
         recalc-event {:req (req (= (:zone target) (:zone card)))
                       :effect recalculate-strength}]
     {:effect recalculate-strength
      :strength-bonus x
      :subroutines [{:label "Trace X - Give the Runner 2 tags"
                     :trace {:base x
                             :label "Give the Runner 2 tags"
                             :successful (give-tags 2)}}
                    {:label "Trace X - End the run"
                     :trace {:base x
                             :label "End the run"
                             :successful end-the-run}}]
      :events {:card-moved recalc-event
               :corp-install recalc-event}})

   "Susanoo-no-Mikoto"
   {:subroutines [{:req (req (not= (:server run) [:discard]))
                   :msg "make the Runner continue the run on Archives"
                   :effect (req (swap! state update-in [:run]
                                       #(assoc %
                                               :position (count (get-in corp [:servers :archives :ices]))
                                               :server [:archives])))}]}

   "Swarm"
   (let [sub {:label "End the run unless Runner pays 3 [Credits]"
              :effect (req (show-wait-prompt state :corp "Runner deciding about Swarm")
                           (continue-ability
                             state :runner
                             {:prompt "Pay 3 [Credits] or end the run?"
                              :choices ["Pay 3 [Credits]" "End the run"]
                              :async true
                              :effect (req (clear-wait-prompt state :corp)
                                           (if (= target "Pay 3 [Credits]")
                                             (do (system-msg state :runner "chooses to pay 3 [Credits]")
                                                 (pay-sync :runner eid card :credit 3))
                                             (do (system-msg state :runner "chooses to end the run")
                                                 (end-run state :runner eid card))))}
                             card nil))}
         ability {:req (req (same-card? card target))
                  :effect (effect (reset-variable-subs card (get-counters card :advancement) sub))}]
     {:advanceable :always
      :effect take-bad-pub
      :events {:advance ability
               :advancement-placed ability
               :rez ability}})

   "Swordsman"
   {:implementation "AI restriction not implemented"
    :subroutines [(do-net-damage 1)
                  {:prompt "Select an AI program to trash"
                   :msg (msg "trash " (:title target))
                   :label "Trash an AI program"
                   :effect (effect (trash target))
                   :choices {:req #(and (installed? %)
                                        (program? %)
                                        (has-subtype? % "AI"))}}]}

   "SYNC BRE"
   {:subroutines [(tag-trace 4)
                  (trace-ability 2 {:label "Runner reduces cards accessed by 1 for this run"
                                    :async true
                                    :msg "reduce cards accessed for this run by 1"
                                    :effect (effect (access-bonus (-> card :zone second) -1))})]}

   "Tapestry"
   {:subroutines [{:label "force the Runner to lose 1 [Click], if able"
                   :msg "force the Runner to lose 1 [Click]"
                   :effect runner-loses-click}
                  {:msg "draw 1 card"
                   :effect (effect (draw))}
                  {:req (req (pos? (count (:hand corp))))
                   :prompt "Choose a card in HQ to move to the top of R&D"
                   :choices {:req #(and (in-hand? %) (corp? %))}
                   :msg "add 1 card in HQ to the top of R&D"
                   :effect (effect (move target :deck {:front true}))}]}

   "Taurus"
   (constellation-ice trash-hardware)

   "Thimblerig"
   {:flags {:corp-phase-12 (req (>= (count (filter ice? (all-installed state :corp))) 2))}
    :implementation "Does not restrict usage of swap ability to start of turn or after pass"
    :abilities [{:label "Swap Thimblerig with a piece of ice"
                 :prompt "Choose a piece of ice to swap Thimblerig with"
                 :choices {:req ice?
                           :not-self true}
                 :effect (effect (swap-ice card target))
                 :msg (msg "swap " (card-str state card) " with " (card-str state target))}]
    :subroutines [end-the-run]}

   "Thoth"
   {:implementation "Encounter effect is manual"
    :abilities [(give-tags 1)]
    :runner-abilities [{:label "Take 1 tag"
                        :async true
                        :effect (req (system-msg state :runner "takes 1 tag on encountering Thoth")
                                     (gain-tags state :corp eid 1))}]
    :subroutines [(trace-ability 4 {:label "Do 1 net damage for each Runner tag"
                                    :async true
                                    :msg (msg "do " (count-tags state) " net damage")
                                    :effect (effect (damage eid :net (count-tags state) {:card card}))})
                  (trace-ability 4 {:label "Runner loses 1 [Credits] for each tag"
                                    :async true
                                    :msg (msg "force the Runner to lose " (count-tags state) " [Credits]")
                                    :effect (effect (lose-credits :runner (count-tags state)))})]}

   "Tithonium"
   {:alternative-cost [:forfeit]
    :implementation "Does not handle UFAQ for Pawn or Blackguard interaction"
    :cannot-host true
    :subroutines [trash-program
                  trash-program
                  {:label "Trash a resource and end the run"
                   :async true
                   :effect (req (wait-for
                                  (resolve-ability
                                    state side
                                    {:req (req (pos? (count (filter resource? (all-installed state :runner)))))
                                     :async true
                                     :choices {:all true
                                               :req #(and (installed? %)
                                                          (resource? %))}
                                     :effect (req (wait-for (trash state side target {:cause :subroutine})
                                                            (complete-with-result state side eid target)))}
                                    card nil)
                                  (system-msg state side
                                              (str "uses Tithonium to "
                                                   (if async-result
                                                     (str "trash " (:title async-result)
                                                          " and ends the run")
                                                     "end the run")))
                                  (end-run state side eid card)))}]}

   "TL;DR"
   {:implementation "Subroutine-adding is manual"
    :events {:pass-ice
             {:req (req (some #(same-card? % target) (get-in card [:special :tldr])))
              :effect (req (let [curr-subs (:subroutines target)
                                 new-subs (into [] (take-nth 2 curr-subs))
                                 new-card (assoc target :subroutines new-subs)
                                 card-list (remove-once #(same-card? % target) (get-in card [:special :tldr]))]
                             (update! state :corp new-card)
                             (update! state :corp (assoc-in card [:special :tldr] card-list))))}}
    :abilities [{:label "Double subroutines on an ICE"
                 :choices {:req #(and (ice? %)
                                      (installed? %)
                                      (rezzed? %))}
                 :effect (req (let [curr-subs (:subroutines target)
                                    new-subs (into [] (interleave curr-subs curr-subs))
                                    new-card (assoc target :subroutines new-subs)]
                                (update! state :corp new-card)
                                (update! state :corp (update-in card [:special :tldr] conj target))))}]
    :subroutines [{:label "Give the next ICE encountered \"End the run\""
                   :msg (msg "give the next ICE encountered \"[Subroutine] End the run\" after all its other subroutines for the remainder of the run")}]}

   "TMI"
   {:trace {:base 2
            :msg "keep TMI rezzed"
            :label "Keep TMI rezzed"
            :unsuccessful {:effect (effect (derez card))}}
    :subroutines [end-the-run]}

   "Tollbooth"
   {:implementation "Encounter effect is manual"
    :abilities [{:msg "make the Runner pay 3 [Credits], if able"
                 :effect (effect (pay :runner card :credit 3))}]
    :subroutines [end-the-run]}

   "Tour Guide"
   (let [ef (effect (reset-variable-subs card (count (filter asset? (all-active-installed state :corp))) end-the-run))
         ability {:label "Reset number of subs"
                  :req (req (asset? target))
                  :effect ef}]
     {:effect ef
      :abilities [(dissoc ability :req)]
      :events {:rez ability
               :derez ability}})

   "Trebuchet"
   {:effect take-bad-pub
    :subroutines [{:prompt "Select a card to trash"
                   :label "Trash 1 installed Runner card"
                   :msg (msg "trash " (:title target))
                   :choices {:req #(and (installed? %)
                                        (runner? %))}
                   :async true
                   :effect (req (trash state side eid target {:cause :subroutine}))}
                  (trace-ability 6 {:label "The Runner cannot steal or trash Corp cards for the remainder of this run"
                                     :msg "prevent the Runner from stealing or trashing Corp cards for the remainder of the run"
                                     :effect (req (register-run-flag! state side card :can-steal
                                                                      (fn [state side card]
                                                                        ((constantly false)
                                                                         (toast state :runner "Cannot steal due to Trebuchet." "warning"))))
                                                  (register-run-flag! state side card :can-trash
                                                                      (fn [state side card]
                                                                        ((constantly (not= (:side card) "Corp"))
                                                                         (toast state :runner "Cannot trash due to Trebuchet." "warning")))))})]}

   "Tribunal"
   (let [sub {:msg "force the Runner to trash 1 installed card"
              :effect (effect (continue-ability :runner trash-installed card nil))}]
     {:subroutines [sub sub sub]})

   "Troll"
   {:implementation "Encounter effect is manual"
    :abilities [(trace-ability 2 {:label "Force the Runner to lose [Click] or end the run"
                                  :msg "force the Runner to lose [Click] or end the run"
                                  :player :runner
                                  :prompt "Choose one"
                                  :choices ["Lose [Click]" "End the run"]
                                  :effect (req (if-not (and (= target "Lose [Click]")
                                                            (can-pay? state :runner eid card nil [:click 1]))
                                                 (do (end-run state side)
                                                     (system-msg state side "ends the run"))
                                                 (do (lose state side :click 1)
                                                     (system-msg state side "loses [Click]"))))})]}

   "Tsurugi"
   {:subroutines [(end-the-run-unless-runner-pays 1)
                  (do-net-damage 1)
                  (do-net-damage 1)
                  (do-net-damage 1)]}

   "Turing"
   {:implementation "AI restriction not implemented"
    :subroutines [(end-the-run-unless-runner
                    "spends [Click][Click][Click]"
                    "spend [Click][Click][Click]"
                    (runner-pay [:click 3] 1))]
    :strength-bonus (req (if (is-remote? (second (:zone card))) 3 0))}

   "Turnpike"
   {:implementation "Encounter effect is manual"
    :abilities [{:msg "force the Runner to lose 1 [Credits]"
                 :effect (effect (lose-credits :runner 1))}]
    :subroutines [(tag-trace 5)]}

   "Tyrant"
   (zero-to-hero end-the-run)

   "Universal Connectivity Fee"
   {:subroutines [{:label "Force the Runner to lose credits"
                   :msg (msg "force the Runner to lose " (if tagged "all credits" "1 [Credits]"))
                   :effect (req (if tagged
                                  (do (lose-credits state :runner :all)
                                      (lose state :runner :run-credit :all)
                                      (when current-ice
                                        (no-action state side nil)
                                        (continue state side nil))
                                      (trash state side card))
                                  (lose-credits state :runner 1)))}]}

   "Upayoga"
   {:subroutines [(do-psi {:label "Make the Runner lose 2 [Credits]"
                           :msg "make the Runner lose 2 [Credits]"
                           :effect (effect (lose-credits :runner 2)
                                           (effect-completed eid))})
                  (resolve-another-subroutine
                    #(has-subtype? % "Psi")
                    "Resolve a subroutine on a rezzed psi ice")]}

   "Uroboros"
   {:subroutines [(trace-ability 4 {:label "Prevent the Runner from making another run"
                                    :msg "prevent the Runner from making another run"
                                    :effect (effect (register-turn-flag! card :can-run nil))})
                  (trace-ability 4 end-the-run)]}

   "Vanilla"
   {:subroutines [end-the-run]}

   "Veritas"
   {:subroutines [{:label "Corp gains 2 [Credits]"
                   :msg "gain 2 [Credits]"
                   :effect (effect (gain-credits :corp 2))}
                  {:label "Runner loses 2 [Credits]"
                   :msg "force the Runner to lose 2 [Credits]"
                   :effect (effect (lose-credits :runner 2))}
                  (trace-ability 2 (give-tags 1))]}

   "Vikram 1.0"
   {:implementation "Program prevention is not implemented"
    :subroutines [{:msg "prevent the Runner from using programs for the remainder of this run"}
                  (trace-ability 4 (do-brain-damage 1))
                  (trace-ability 4 (do-brain-damage 1))]
    :runner-abilities [(bioroid-break 1 1)]}

   "Viktor 1.0"
   {:subroutines [(do-brain-damage 1)
                  end-the-run]
    :runner-abilities [(bioroid-break 1 1)]}

   "Viktor 2.0"
   {:abilities [(power-counter-ability (do-brain-damage 1))]
    :subroutines [(trace-ability 2 add-power-counter)
                  end-the-run]
    :runner-abilities [(bioroid-break 2 2)]}

   "Viper"
   {:subroutines [(trace-ability 3 {:label "The Runner loses 1 [Click] if able"
                                    :msg "force the Runner to lose 1 [Click] if able"
                                    :effect runner-loses-click})
                  (trace-ability 3 end-the-run)]}

   "Virgo"
   (constellation-ice (give-tags 1))

   "Waiver"
   {:subroutines [(trace-ability 5 {:label "Reveal the Runner's Grip and trash cards"
                                    :msg (msg "reveal all cards in the Runner's Grip: " (join ", " (map :title (:hand runner)))
                                              ". Cards with a play/install cost less than or equal to " (- target (second targets))
                                              " will be trashed")
                                    :effect (req (reveal state side (:hand runner))
                                                 (let [delta (- target (second targets))]
                                                   (doseq [c (:hand runner)]
                                                     (when (<= (:cost c) delta)
                                                       (resolve-ability
                                                         state side
                                                         {:msg (msg "trash " (:title c))
                                                          :effect (effect (trash c))}
                                                         card nil)))))})]}

   "Wall of Static"
   {:subroutines [end-the-run]}

   "Wall of Thorns"
   {:subroutines [(do-net-damage 2)
                  end-the-run]}

   "Watchtower"
   {:subroutines [{:label "Search R&D and add 1 card to HQ"
                   :prompt "Choose a card to add to HQ"
                   :msg "add a card from R&D to HQ"
                   :choices (req (cancellable (:deck corp) :sorted))
                   :cancel-effect (effect (system-msg "cancels the effect of Watchtower"))
                   :effect (effect (shuffle! :deck)
                                   (move target :hand))}]}

   "Weir"
   {:subroutines [{:label "force the Runner to lose 1 [Click], if able"
                   :msg "force the Runner to lose 1 [Click]"
                   :effect runner-loses-click}
                  {:label "Runner trashes 1 card from their Grip"
                   :req (req (pos? (count (:hand runner))))
                   :prompt "Choose a card to trash from your Grip"
                   :player :runner
                   :choices (req (:hand runner))
                   :not-distinct true
                   :effect (effect (trash :runner target)
                                   (system-msg :runner (str "trashes " (:title target) " from their Grip")))}]}

   "Wendigo"
   (implementation-note
     "Program prevention is not implemented"
     (morph-ice "Code Gate" "Barrier"
                {:msg "prevent the Runner from using a chosen program for the remainder of this run"}))

   "Whirlpool"
   {:subroutines [{:msg "prevent the Runner from jacking out"
                   :effect (req (when (and (is-remote? (second (:zone card)))
                                           (> (count (concat (:ices (card->server state card))
                                                             (:content (card->server state card)))) 1))
                                  (prevent-jack-out state side))
                                (when current-ice
                                  (no-action state side nil)
                                  (continue state side nil))
                                (trash state side card))}]}

   "Woodcutter"
   (zero-to-hero (do-net-damage 1))

   "Wormhole"
   (space-ice (resolve-another-subroutine))

   "Wotan"
   {:subroutines [(end-the-run-unless-runner
                    "spends [Click][Click]"
                    "spend [Click][Click]"
                    (runner-pay [:click 2] 1))
                  (end-the-run-unless-runner-pays 3)
                  (end-the-run-unless-runner
                    "trashes an installed program"
                    "trash an installed program"
                    trash-program)
                  (end-the-run-unless-runner
                    "takes 1 brain damage"
                    "take 1 brain damage"
                    (do-brain-damage 1))]}

   "Wraparound"
   {:subroutines [end-the-run]
    :strength-bonus (req (if (some #(has-subtype? % "Fracter") (all-active-installed state :runner))
                           0 7))
    :events (let [wr {:silent (req true)
                      :req (req (and (not (same-card? target card))
                                     (has-subtype? target "Fracter")))
                      :effect (effect (update-ice-strength card))}]
              {:runner-install wr :trash wr :card-moved wr})}

   "Yagura"
   {:subroutines [(do-net-damage 1)
                  {:msg "look at the top card of R&D"
                   :optional {:prompt (msg "Move " (:title (first (:deck corp))) " to the bottom of R&D?")
                              :yes-ability {:msg "move the top card of R&D to the bottom"
                                            :effect (effect (move (first (:deck corp)) :deck))}
                              :no-ability {:effect (effect (system-msg :corp (str "does not use Yagura to move the top card of R&D to the bottom")))}}}]}

   "Zed 1.0"
   {:implementation "Restriction on having spent [click] is not implemented"
    :subroutines [(do-brain-damage 1)
                  (do-brain-damage 1)]
    :runner-abilities [(bioroid-break 1 1)]}

   "Zed 2.0"
   {:implementation "Restriction on having spent [click] is not implemented"
    :subroutines [trash-hardware
                  trash-hardware
                  (do-brain-damage 2)]
    :runner-abilities [(bioroid-break 2 2)]}})
