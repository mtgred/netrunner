(ns game.cards.ice
  (:require [game.core :refer :all]
            [game.core.card :refer :all]
            [game.core.card-defs :refer [define-card]]
            [game.core.eid :refer [make-eid effect-completed complete-with-result]]
            [game.core.card-defs :refer [card-def]]
            [game.core.prompts :refer [show-wait-prompt clear-wait-prompt]]
            [game.core.toasts :refer [toast]]
            [game.core.effects :refer :all]
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
  ([cost qty] (bioroid-break cost qty nil))
  ([cost qty args]
   (break-sub [:click cost {:action :bioroid-cost}] qty nil args)))

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

(defn end-the-run-unless-runner-pays
  [amount]
  {:player :runner
   :async true
   :label (str "End the run unless the Runner pays " amount " [Credits]")
   :prompt (str "End the run or pay " amount " [Credits]?")
   :choices ["End the run"
             (str "Pay " amount " [Credits]")]
   :effect (req (if (= "End the run" target)
                  (do (system-msg state :corp
                                  (str "uses " (:title card) " to end the run"))
                      (end-run state :corp eid card))
                  (wait-for (pay-sync state :runner card [:credit amount])
                            (when async-result
                              (let [cost-str (str async-result
                                                  " due to " (:title card)
                                                  " subroutine")]
                                (system-msg state :runner cost-str)))
                            (effect-completed state side eid))))})

(defn end-the-run-unless-corp-pays
  [amount]
  {:async true
   :label (str "End the run unless the Corp pays " amount " [Credits]")
   :prompt (str "End the run or pay " amount " [Credits]?")
   :choices ["End the run"
             (str "Pay " amount " [Credits]")]
   :effect (req (if (= "End the run" target)
                  (end-run state :corp eid card)
                  (pay-sync state :corp eid card [:credit amount])))})

(defn end-the-run-unless-runner
  [label prompt ability]
  {:player :runner
   :async true
   :label (str "End the run unless the Runner " label)
   :prompt (str "End the run or " prompt "?")
   :choices ["End the run"
             (capitalize prompt)]
   :effect (req (if (= "End the run" target)
                  (end-run state :corp eid card)
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
         :label label
         :msg message
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
  ; Bad pub on rez effect
  (effect (gain-bad-publicity :corp 1)
          (system-msg (str "takes 1 bad publicity from " (:title card)))))

(def runner-loses-click
  ; Runner loses a click effect
  {:label "Force the Runner to lose 1 [Click]"
   :msg "force the Runner to lose 1 [Click] if able"
   :effect (effect (lose :runner :click 1))})

(def add-runner-card-to-grip
  "Add 1 installed Runner card to the grip"
  {:async true
   :label "Add an installed Runner card to the grip"
   :req (req (not-empty (all-installed state :runner)))
   :effect (effect (show-wait-prompt :runner (str "Corp to select " (:title card) " target"))
                   (continue-ability
                     {:choices {:card #(and (installed? %)
                                            (runner? %))}
                      :msg "add 1 installed card to the Runner's Grip"
                      :effect (effect (clear-wait-prompt :runner)
                                      (move :runner target :hand true)
                                      (system-msg (str "adds " (:title target)
                                                       " to the Runner's Grip")))
                      :cancel-effect (effect (clear-wait-prompt :runner)
                                             (effect-completed eid))}
                     card nil))})

;;; For Advanceable ICE
(def advance-counters
  "Number of advancement counters - for advanceable ICE."
  (req (get-counters card :advancement)))

(defn space-ice
  "Creates data for Space ICE with specified abilities."
  [& abilities]
  {:advanceable :always
   :subroutines (vec abilities)
   :rez-cost-bonus (req (* -3 (get-counters card :advancement)))})

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
             :card grail-in-hand}
   :effect (effect (reveal targets))
   :msg (let [sub-label #(:label (first (:subroutines (card-def %))))]
          (msg "reveal " (join ", " (map #(str (:title %) " (" (sub-label %) ")") targets))))})

(def resolve-grail
  "Ability for resolving a subroutine on a Grail ICE in HQ."
  {:label "Resolve a Grail ICE subroutine from HQ"
   :choices {:card grail-in-hand}
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
     :events [(assoc ab :event :advance)
              (assoc ab :event :advancement-placed)]}))

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
     :events [(assoc ability :event :advance)
              (assoc ability :event :advancement-placed)
              (assoc ability :event :rez)]}))

;; For 7 Wonders ICE
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
             :prompt "Select the ice"
             :choices {:card pred
                       :all true}
             :effect (effect
                       (continue-ability
                         (let [ice target]
                           {:async true
                            :prompt "Select the subroutine"
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

(define-card "Afshar"
  (let [breakable-fn (req (if (= :hq (second (:zone card)))
                            (empty? (filter #(and (:broken %) (:printed %)) (:subroutines card)))
                            :unrestricted))]
    {:subroutines [{:msg "make the Runner lose 2 [Credits]"
                    :breakable breakable-fn
                    :effect (effect (lose-credits :runner 2))}
                   (assoc end-the-run :breakable breakable-fn)]}))

(define-card "Aiki"
  {:subroutines [(do-psi {:label "Runner draws 2 cards"
                          :msg "make the Runner draw 2 cards"
                          :async true
                          :effect (effect (draw :runner eid 2 nil))})
                 (do-net-damage 1)
                 (do-net-damage 1)]})

(define-card "Aimor"
  {:subroutines [{:async true
                  :label "Trash the top 3 cards of the Stack. Trash Aimor."
                  :effect (req (system-msg state :corp
                                           (str "uses Aimor to trash "
                                                (join ", " (map :title (take 3 (:deck runner))))
                                                " from the Runner's Stack"))
                               (wait-for (mill state :corp :runner 3)
                                         (system-msg state side (str "trashes Aimor"))
                                         (trash state side eid card {:cause :subroutine})))}]})

(define-card "Akhet"
  (let [breakable-fn (req (if (<= 3 (get-counters card :advancement))
                            (empty? (filter #(and (:broken %) (:printed %)) (:subroutines card)))
                            :unrestricted))]
    {:advanceable :always
     :subroutines [{:label "Gain 1[Credit]. Place 1 advancement token."
                    :breakable breakable-fn
                    :msg (msg "gain 1 [Credit] and place 1 advancement token on " (card-str state target))
                    :prompt "Choose an installed card"
                    :choices {:card installed?}
                    :effect (effect (gain-credits 1)
                                    (add-prop target :advance-counter 1 {:placed true}))}
                   (assoc end-the-run :breakable breakable-fn)]
     :strength-bonus (req (if (<= 3 (get-counters card :advancement)) 3 0))}))

(define-card "Anansi"
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
    {:subroutines [{:msg "rearrange the top 5 cards of R&D"
                    :async true
                    :effect (req (show-wait-prompt state :runner "Corp to rearrange the top cards of R&D")
                                 (let [from (take 5 (:deck corp))]
                                   (if (pos? (count from))
                                     (continue-ability state side (reorder-choice :corp :runner from '()
                                                                                  (count from) from)
                                                       card nil)
                                     (do (clear-wait-prompt state :runner)
                                         (effect-completed state side eid)))))}
                   {:label "Draw 1 card, runner draws 1 card"
                    :async true
                    :effect (req (wait-for (resolve-ability state side corp-draw card nil)
                                           (continue-ability state :runner runner-draw card nil)))}
                   (do-net-damage 1)]
     :events [(assoc (do-net-damage 3)
                     :event :encounter-ice-ends
                     :req (req (and (= target card)
                                    (seq (remove :broken (:subroutines target))))))]}))

(define-card "Archangel"
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
                                             :yes-ability {:async true
                                                           :effect (effect (resolve-unbroken-subs! eid card))}
                                             :no-ability {:effect (effect (effect-completed eid))}}}
                                           card nil))}
                        :no-ability {:effect (effect (system-msg :corp "declines to force the Runner to encounter Archangel")
                                                     (clear-wait-prompt :runner))}}}
                      card nil))}
   :subroutines [(trace-ability 6 add-runner-card-to-grip)]})

(define-card "Archer"
  {:additional-cost [:forfeit]
   :subroutines [(gain-credits-sub 2)
                 trash-program
                 trash-program
                 end-the-run]})

(define-card "Architect"
  {:flags {:untrashable-while-rezzed true}
   :subroutines [{:label "Look at the top 5 cards of R&D"
                  :prompt "Choose a card to install"
                  :async true
                  :activatemsg "uses Architect to look at the top 5 cards of R&D"
                  :req (req (and (not (string? target))
                                 (not (operation? target))))
                  :not-distinct true
                  :choices (req (conj (take 5 (:deck corp)) "No install"))
                  :effect (effect (system-msg (str "chooses the card in position "
                                                   (+ 1 (.indexOf (take 5 (:deck corp)) target))
                                                   " from R&D (top is 1)"))
                                  (corp-install eid target nil {:ignore-all-cost true}))}
                 {:label "Install a card from HQ or Archives"
                  :prompt "Select a card to install from Archives or HQ"
                  :show-discard true
                  :choices {:card #(and (corp? %)
                                        (not (operation? %))
                                        (or (in-hand? %)
                                            (in-discard? %)))}
                  :async true
                  :msg (msg (corp-install-msg target))
                  :effect (effect (corp-install eid target nil nil))}]})

(define-card "Ashigaru"
  {:effect (effect (reset-variable-subs card (count (:hand corp)) end-the-run))
   :events [{:event :card-moved
             :req (req (let [target (nth targets 1)]
                         (and (corp? target)
                              (or (= :hand (first (:zone target)))
                                  (= :hand (first (:previous-zone target)))))))
             :effect (effect (reset-variable-subs card (count (:hand corp)) end-the-run))}]})

(define-card "Assassin"
  {:subroutines [(trace-ability 5 (do-net-damage 3))
                 (trace-ability 4 trash-program)]})

(define-card "Asteroid Belt"
  (space-ice end-the-run))

(define-card "Authenticator"
  {:on-encounter {:optional
                  {:req (req (and (not (:bypass run))
                                  (same-card? card target)))
                   :player :runner
                   :prompt "Take 1 tag to bypass?"
                   :yes-ability
                   {:async true
                    :effect (req (system-msg state :runner "takes 1 tag on encountering Authenticator to bypass it")
                                 (bypass-ice state)
                                 (gain-tags state :runner eid 1 {:unpreventable true}))}}}
   :subroutines [(gain-credits-sub 2)
                 end-the-run]})

(define-card "Bailiff"
  {:implementation "Gain credit is manual"
   :abilities [(gain-credits-sub 1)]
   :subroutines [end-the-run]})

(define-card "Bandwidth"
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

(define-card "Bastion"
  {:subroutines [end-the-run]})

(define-card "Battlement"
  {:subroutines [end-the-run
                 end-the-run]})

(define-card "Blockchain"
  (let [sub-count (fn [corp]
                    (quot (count (filter #(and (operation? %)
                                               (has-subtype? % "Transaction")
                                               (faceup? %))
                                         (:discard corp)))
                          2))
        sub {:label "Gain 1 [Credits], Runner loses 1 [Credits]"
             :msg "gain 1 [Credits] and force the Runner to lose 1 [Credits]"
             :effect (effect (gain-credits 1)
                             (lose-credits :runner 1))}]
    {:effect (effect (reset-variable-subs card (sub-count corp) sub {:variable true :front true}))
     :events [{:event :card-moved
               :req (req (let [target (nth targets 1)]
                           (and (operation? target)
                                (has-subtype? target "Transaction")
                                (faceup? target)
                                (or (= :discard (first (:zone target)))
                                    (= :discard (first (:previous-zone target)))))))
               :effect (effect (reset-variable-subs card (sub-count corp) sub {:variable true :front true}))}]
     :subroutines [sub
                   end-the-run]}))

(define-card "Bloodletter"
  {:subroutines [{:async true
                  :label "Runner trashes 1 program or top 2 cards of their Stack"
                  :effect (req (if (empty? (filter program? (all-active-installed state :runner)))
                                 (do (system-msg state :runner (str "trashes the top 2 cards of their Stack"))
                                     (mill state :runner eid :runner 2))
                                 (do (show-wait-prompt state :corp "Runner to choose an option for Bloodletter")
                                     (continue-ability
                                       state :runner
                                       {:async true
                                        :prompt "Trash 1 program or trash top 2 cards of the Stack?"
                                        :choices ["Trash 1 program" "Trash top 2 of Stack"]
                                        :effect (req (clear-wait-prompt state :corp)
                                                     (if (and (= target "Trash top 2 of Stack") (> (count (:deck runner)) 1))
                                                       (do (system-msg state :runner (str "trashes the top 2 cards of their Stack"))
                                                           (mill state :runner eid :runner 2))
                                                       (continue-ability state :runner trash-program card nil)))}
                                       card nil))))}]})

(define-card "Bloom"
  {:subroutines
   [{:label "Install a piece of ice from HQ protecting another server, ignoring all costs"
     :prompt "Choose ICE to install from HQ in another server"
     :async true
     :choices {:card #(and (ice? %)
                           (in-hand? %))}
     :effect (req (let [this (zone->name (second (:zone card)))
                        nice target]
                    (continue-ability state side
                                      {:prompt (str "Choose a location to install " (:title target))
                                       :choices (req (remove #(= this %) (corp-install-list state nice)))
                                       :async true
                                       :effect (effect (corp-install eid nice target {:ignore-all-cost true}))}
                                      card nil)))}
    {:label "Install a piece of ice from HQ in the next innermost position, protecting this server, ignoring all costs"
     :prompt "Choose ICE to install from HQ in this server"
     :async true
     :choices {:card #(and (ice? %)
                           (in-hand? %))}
     :effect (req (corp-install state side eid
                                target (zone->name (first (:server run)))
                                {:ignore-all-cost true
                                 :index (max (dec run-position) 0)})
                  (swap! state update-in [:run :position] inc))}]})

(define-card "Border Control"
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
                 end-the-run]})

(define-card "Brainstorm"
  {:on-encounter {:effect (effect (gain-variable-subs card (count (:hand runner)) (do-brain-damage 1)))}
   :events [{:event :run-ends
             :effect (effect (reset-variable-subs card 0 nil))}]})

(define-card "Builder"
  (let [sub {:label "Place 1 advancement token on an ICE that can be advanced protecting this server"
             :msg (msg "place 1 advancement token on " (card-str state target))
             :choices {:card #(and (ice? %)
                                   (can-be-advanced? %))}
             :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]
    {:abilities [{:label "Move Builder to the outermost position of any server"
                  :cost [:click 1]
                  :prompt "Choose a server"
                  :choices (req servers)
                  :msg (msg "move it to the outermost position of " target)
                  :effect (effect (move card (conj (server->zone state target) :ices)))}]
     :subroutines [sub
                   sub]}))

(define-card "Bullfrog"
  {:subroutines [(do-psi {:label "Move Bullfrog to another server"
                          :player :corp
                          :prompt "Choose a server"
                          :choices (req servers)
                          :msg (msg "move it to the outermost position of " target)
                          :effect (effect (move card (conj (server->zone state target) :ices))
                                          (redirect-run target)
                                          (effect-completed eid))})]})

(define-card "Bulwark"
  (let [sub {:msg "gain 2 [Credits] and end the run"
             :effect (effect (gain-credits 2)
                             (end-run eid card))}]
    {:effect take-bad-pub
     :on-encounter {:req (req (some #(has-subtype? % "AI") (all-active-installed state :runner)))
                    :msg "gain 2 [Credits] if there is an installed AI"
                    :effect (effect (gain-credits 2))}
     :subroutines [(assoc trash-program
                          :player :runner
                          :msg "force the Runner to trash 1 program"
                          :label "The Runner trashes 1 program")
                   sub
                   sub]}))

(define-card "Burke Bugs"
  {:subroutines [(trace-ability 0 (assoc trash-program
                                         :not-distinct true
                                         :player :runner
                                         :msg "force the Runner to trash a program"
                                         :label "Force the Runner to trash a program"))]})

(define-card "Caduceus"
  {:subroutines [(trace-ability 3 (gain-credits-sub 3))
                 (trace-ability 2 end-the-run)]})

(define-card "Cell Portal"
  {:subroutines [{:msg "make the Runner approach the outermost ICE"
                  :effect (req (let [server (central->name (first (:server run)))]
                                 (redirect-run state side server :approach-ice)
                                 (derez state side card)))}]})

(define-card "Changeling"
  (morph-ice "Barrier" "Sentry" end-the-run))

(define-card "Checkpoint"
  {:effect take-bad-pub
   :subroutines [(trace-ability 5 {:label "Do 3 meat damage when this run is successful"
                                   :msg "do 3 meat damage when this run is successful"
                                   :effect (effect (register-events
                                                     card
                                                     [{:event :run-ends
                                                       :duration :end-of-run
                                                       :async true
                                                       :req (req (:successful target))
                                                       :msg "do 3 meat damage"
                                                       :effect (effect (damage eid :meat 3 {:card card}))}]))})]})

(define-card "Chetana"
  {:subroutines [{:msg "make each player gain 2 [Credits]"
                  :effect (effect (gain-credits :runner 2)
                                  (gain-credits :corp 2))}
                 (do-psi {:label "Do 1 net damage for each card in the Runner's grip"
                          :msg (msg "do " (count (get-in @state [:runner :hand])) " net damage")
                          :effect (effect (damage eid :net (count (get-in @state [:runner :hand])) {:card card}))})]})

(define-card "Chimera"
  (let [turn-end-ability {:effect (effect (derez :corp card)
                                          (update! (assoc (get-card state card) :subtype "Mythic")))}]
    {:prompt "Choose one subtype"
     :choices ["Barrier" "Code Gate" "Sentry"]
     :msg (msg "make it gain " target " until the end of the turn")
     :effect (effect (update! (assoc card
                                     :subtype-target target
                                     :subtype (combine-subtypes true (:subtype card) target)))
                     (update-ice-strength card))
     :events [(assoc turn-end-ability :event :runner-turn-ends)
              (assoc turn-end-ability :event :corp-turn-ends)]
     :subroutines [end-the-run]}))

(define-card "Chiyashi"
  {:implementation "Trash effect when using an AI to break is activated manually"
   :abilities [{:async true
                :label "Trash the top 2 cards of the Runner's Stack"
                :req (req (some #(has-subtype? % "AI") (all-active-installed state :runner)))
                :msg (msg (str "trash " (join ", " (map :title (take 2 (:deck runner)))) " from the Runner's Stack"))
                :effect (effect (mill :corp eid :runner 2))}]
   :subroutines [(do-net-damage 2)
                 (do-net-damage 2)
                 end-the-run]})

(define-card "Chrysalis"
  {:flags {:rd-reveal (req true)}
   :subroutines [(do-net-damage 2)]
   :access {:async true
            :req (req (not= (first (:zone card)) :discard))
            :effect (effect (show-wait-prompt :corp "Runner to decide to break Chrysalis subroutine")
                            (continue-ability
                              :runner {:optional
                                       {:player :runner
                                        :prompt "You are encountering Chrysalis. Allow its subroutine to fire?"
                                        :yes-ability {:effect (effect (clear-wait-prompt :corp)
                                                                      (resolve-unbroken-subs! :corp eid card))}
                                        :no-ability {:effect (effect (clear-wait-prompt :corp)
                                                                     (effect-completed eid))}}}
                              card nil))}})

(define-card "Chum"
  {:subroutines
   [{:label "Give +2 strength to next ICE Runner encounters"
     :req (req this-server)
     :msg (msg "give +2 strength to the next ICE the Runner encounters")
     :effect
     (effect (register-events
               card
               [{:event :encounter-ice
                 :duration :end-of-run
                 :unregister-once-resolved true
                 :req (req (and (rezzed? target)
                                (not (same-card? target card))))
                 :effect
                 (req (let [target-ice target]
                        (register-floating-effect
                          state side card
                          {:type :ice-strength
                           :duration :end-of-encounter
                           :value 2
                           :req (req (same-card? target target-ice))})
                        (register-events
                          state side card
                          [(assoc (do-net-damage 3)
                                  :event :encounter-ice-ends
                                  :duration :end-of-run
                                  :unregister-once-resolved true
                                  :req (req (and (same-card? target target-ice)
                                                 (seq (remove :broken (:subroutines (get-card state target-ice)))))))])))}]))}]})

(define-card "Clairvoyant Monitor"
  {:subroutines [(do-psi {:label "Place 1 advancement token and end the run"
                          :player :corp
                          :prompt "Select a target for Clairvoyant Monitor"
                          :msg (msg "place 1 advancement token on "
                                    (card-str state target) " and end the run")
                          :choices {:card installed?}
                          :effect (effect (add-prop target :advance-counter 1 {:placed true})
                                          (end-run eid card))})]})

(define-card "Cobra"
  {:subroutines [trash-program (do-net-damage 2)]})

(define-card "Colossus"
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
                                              :choices {:card #(and (installed? %)
                                                                    (resource? %))}
                                              :cancel-effect (req (effect-completed state side eid))
                                              :async true
                                              :effect (effect (trash eid target {:cause :subroutine}))}
                                             card nil)
                                           (effect-completed state side eid))))}]
   :strength-bonus advance-counters})

(define-card "Congratulations!"
  {:events [{:event :pass-ice
             :req (req (same-card? target card))
             :msg "gain 1 [Credits]"
             :effect (effect (gain-credits :corp 1))}]
   :subroutines [{:label "Gain 2 [Credits]. The Runner gains 1 [Credits]"
                  :msg "gain 2 [Credits]. The Runner gains 1 [Credits]"
                  :effect (effect (gain-credits :corp 2)
                                  (gain-credits :runner 1))}]})

(define-card "Conundrum"
  {:subroutines [(assoc trash-program
                        :player :runner
                        :msg "force the Runner to trash 1 program"
                        :label "The Runner trashes 1 program")
                 runner-loses-click
                 end-the-run]
   :strength-bonus (req (if (some #(has-subtype? % "AI") (all-active-installed state :runner)) 3 0))})

(define-card "Cortex Lock"
  {:subroutines [{:label "Do 1 net damage for each unused memory unit the Runner has"
                  :msg (msg "do " (available-mu state) " net damage")
                  :effect (effect (damage eid :net (available-mu state) {:card card}))}]})

(define-card "Crick"
  {:subroutines [{:label "install a card from Archives"
                  :prompt "Select a card to install from Archives"
                  :show-discard true
                  :async true
                  :choices {:card #(and (not (operation? %))
                                        (in-discard? %)
                                        (corp? %))}
                  :msg (msg (corp-install-msg target))
                  :effect (effect (corp-install eid target nil nil))}]
   :strength-bonus (req (if (= (second (:zone card)) :archives) 3 0))})

(define-card "Curtain Wall"
  {:subroutines [end-the-run
                 end-the-run
                 end-the-run]
   :strength-bonus (req (let [ices (:ices (card->server state card))]
                          (if (same-card? card (last ices)) 4 0)))
   :events (let [cw {:req (req (and (not (same-card? card target))
                                    (= (card->server state card) (card->server state target))))
                     :effect (effect (update-ice-strength card))}]
             [(assoc cw :event :corp-install)
              (assoc cw :event :trash)
              {:req (req (let [target (nth targets 1)]
                           (and (not (same-card? card target))
                                (= (card->server state card) (card->server state target)))))
               :effect (effect (update-ice-strength card))}])})

(define-card "Data Hound"
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
                      :label "Look at the top of the stack"
                      :msg "look at top X cards of the stack"
                      :effect (req (show-wait-prompt state :runner "Corp to rearrange the top cards of the stack")
                                   (let [c (- target (second targets))
                                         from (take c (:deck runner))]
                                     (system-msg state :corp
                                                 (str "looks at the top " (quantify c "card") " of the stack"))
                                     (if (< 1 c)
                                       (continue-ability state side (dh-trash from) card nil)
                                       (wait-for (trash state side (first from) {:unpreventable true
                                                                                 :cause :subroutine})
                                                 (system-msg state :corp (str "trashes " (:title (first from))))
                                                 (clear-wait-prompt state :runner)
                                                 (effect-completed state side eid)))))})]}))

(define-card "Data Loop"
  {:on-encounter {:req (req (pos? (count (:hand runner))))
                  :async true
                  :effect (effect
                            (continue-ability
                              :runner
                              (let [n (min 2 (count (:hand runner)))]
                                {:prompt (str "Choose " (quantify n "card") " in your Grip to add to the top of the Stack (first card targeted will be topmost)")
                                 :choices {:max n
                                           :all true
                                           :card #(and (in-hand? %)
                                                       (runner? %))}
                                 :msg (msg "add " n " cards from their Grip to the top of the Stack")
                                 :effect (req (doseq [c targets]
                                                (move state :runner c :deck {:front true})))})
                              card nil))}
   :subroutines [end-the-run-if-tagged
                 end-the-run]})

(define-card "Data Mine"
  {:subroutines [{:msg "do 1 net damage"
                  :async true
                  :effect (req (wait-for (damage state :runner :net 1 {:card card})
                                         (trash state :corp eid card {:cause :subroutine})))}]})

(define-card "Data Raven"
  {:abilities [(power-counter-ability (give-tags 1))]
   :on-encounter {:msg "force the Runner to take 1 tag or end the run"
                  :player :runner
                  :prompt "Choose one"
                  :choices ["Take 1 tag" "End the run"]
                  :async true
                  :effect (req (if (= target "Take 1 tag")
                                 (do (system-msg state :runner "chooses to take 1 tag")
                                     (gain-tags state :runner eid 1))
                                 (do (system-msg state :runner "ends the run")
                                     (end-run state :runner eid card))))}
   :subroutines [(trace-ability 3 add-power-counter)]})

(define-card "Data Ward"
  {:on-encounter {:player :runner
                  :prompt "Choose one"
                  :choices ["Pay 3 [Credits]" "Take 1 tag"]
                  :async true
                  :effect (req (if (= target "Pay 3 [Credits]")
                                 (do (system-msg state :runner "pays 3 [Credits]")
                                     (pay-sync state :runner eid card :credit 3))
                                 (do (system-msg state :runner "takes 1 tag on encountering Data Ward")
                                     (gain-tags state :runner eid 1))))}
   :subroutines [end-the-run-if-tagged
                 end-the-run-if-tagged
                 end-the-run-if-tagged
                 end-the-run-if-tagged]})

(define-card "Datapike"
  {:subroutines [{:msg "force the Runner to pay 2 [Credits] if able"
                  :effect (effect (pay :runner card :credit 2))}
                 end-the-run]})

(define-card "DNA Tracker"
  (let [sub {:msg "do 1 net damage and make the Runner lose 2 [Credits]"
             :async true
             :effect (req (wait-for (damage state side :net 1 {:card card})
                                    (lose-credits state :runner 2)
                                    (effect-completed state side eid)))}]
    {:subroutines [sub
                   sub
                   sub]}))

(define-card "Dracō"
  {:prompt "How many power counters?"
   :choices :credit
   :msg (msg "add " target " power counters")
   :effect (effect (add-counter card :power target)
                   (update-ice-strength card))
   :strength-bonus (req (get-counters card :power))
   :subroutines [(trace-ability 2 {:label "Give the Runner 1 tag and end the run"
                                   :msg "give the Runner 1 tag and end the run"
                                   :async true
                                   :effect (req (wait-for (gain-tags state :corp 1)
                                                          (end-run state :corp eid card)))})]})

(define-card "Drafter"
  {:subroutines [{:label "Add 1 card from Archives to HQ"
                  :prompt "Select a card from Archives to add to HQ"
                  :show-discard true
                  :choices {:card #(and (corp? %)
                                        (in-discard? %))}
                  :msg (msg "add " (if (faceup? target) (:title target) "an unseen card") " to HQ")
                  :effect (effect (move target :hand))}
                 {:async true
                  :label "Install a card from HQ or Archives"
                  :prompt "Select a card to install from Archives or HQ"
                  :show-discard true
                  :choices {:card #(and (corp? %)
                                        (not (operation? %))
                                        (or (in-hand? %)
                                            (in-discard? %)))}
                  :msg (msg (corp-install-msg target))
                  :effect (effect (corp-install eid target nil {:ignore-all-cost true}))}]})

(define-card "Eli 1.0"
  {:subroutines [end-the-run
                 end-the-run]
   :runner-abilities [(bioroid-break 1 1)]})

(define-card "Eli 2.0"
  {:subroutines [{:async true
                  :msg "draw 1 card"
                  :effect (effect (draw eid 1 nil))}
                 end-the-run
                 end-the-run]
   :runner-abilities [(bioroid-break 2 2)]})

(define-card "Endless EULA"
  (let [sub (end-the-run-unless-runner-pays 1)]
    (letfn [(break-fn [unbroken-subs total]
              {:asycn true
               :effect
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

(define-card "Enforcer 1.0"
  {:additional-cost [:forfeit]
   :subroutines [trash-program
                 (do-brain-damage 1)
                 {:label "Trash a console"
                  :prompt "Select a console to trash"
                  :choices {:card #(has-subtype? % "Console")}
                  :msg (msg "trash " (:title target))
                  :async true
                  :effect (effect (trash eid target {:cause :subroutine}))}
                 {:msg "trash all virtual resources"
                  :async true
                  :effect (req (let [cards (filter #(has-subtype? % "Virtual") (all-active-installed state :runner))]
                                 (trash-cards state side eid cards {:cause :subroutine})))}]
   :runner-abilities [(bioroid-break 1 1)]})

(define-card "Engram Flush"
  (let [sub {:async true
             :label "Reveal the grip"
             :msg (msg "reveal " (quantify (count (:hand runner)) "card")
                       " from grip: " (join ", " (map :title (:hand runner))))
             ;; This has to be manual instead of calling `reveal` because `reveal` isn't
             ;; async and I don't feel like trying to make it async just for this interaction.
             ;; TODO: Make `reveal` async
             :effect (req (apply trigger-event-sync state side eid :corp-reveal (:hand runner)))}]
    {:on-encounter {:prompt "Choose a card type"
                    :choices ["Event" "Hardware" "Program" "Resource"]
                    :effect (req (let [cardtype target]
                                   (system-msg state side
                                               (str "uses " (:title card) " to name " target))
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
                                       :prompt "Select revealed card to trash"
                                       :choices (req (concat (filter #(is-type? % cardtype) targets) ["None"]))
                                       :msg (msg "trash " (:title target) " from grip")
                                       :effect (req (if (= "None" target)
                                                      (effect-completed state side eid)
                                                      (trash state side eid target {:cause :subroutine})))}])))}
     :subroutines [sub
                   sub]}))

(define-card "Enigma"
  {:subroutines [runner-loses-click
                 end-the-run]})

(define-card "Envelope"
  {:subroutines [(do-net-damage 1)
                 end-the-run]})

(define-card "Errand Boy"
  (let [sub {:async true
             :label "Draw a card or gain 1 [Credits]"
             :prompt "Choose one:"
             :choices ["Gain 1 [Credits]" "Draw 1 card"]
             :msg (req (if (= target "Gain 1 [Credits]")
                         "gain 1 [Credits]"
                         "draw 1 card"))
             :effect (req (if (= target "Gain 1 [Credits]")
                            (do (gain-credits state :corp 1)
                                (effect-completed state side eid))
                            (draw state :corp eid 1 nil)))}]
    {:subroutines [sub
                   sub
                   sub]}))

(define-card "Excalibur"
  {:subroutines [{:label "The Runner cannot make another run this turn"
                  :msg "prevent the Runner from making another run"
                  :effect (effect (register-turn-flag! card :can-run nil))}]})

(define-card "Executive Functioning"
  {:subroutines [(trace-ability 4 (do-brain-damage 1))]})

(define-card "F2P"
  {:subroutines [add-runner-card-to-grip
                 (give-tags 1)]
   :runner-abilities [(break-sub [:credit 2] 1 nil {:req (req (not tagged))})]})

(define-card "Fairchild"
  {:subroutines [(end-the-run-unless-runner-pays 4)
                 (end-the-run-unless-runner-pays 4)
                 (end-the-run-unless-runner
                   "trashes an installed card"
                   "trash an installed card"
                   runner-trash-installed-sub)
                 (end-the-run-unless-runner
                   "suffers 1 brain damage"
                   "suffer 1 brain damage"
                   (do-brain-damage 1))]})

(define-card "Fairchild 1.0"
  (let [sub {:label "Force the Runner to pay 1 [Credits] or trash an installed card"
             :msg "force the Runner to pay 1 [Credits] or trash an installed card"
             :player :runner
             :prompt "Choose one"
             :choices ["Pay 1 [Credits]" "Trash an installed card"]
             :effect (req (if (= target "Pay 1 [Credits]")
                            (do (pay state side card :credit 1)
                                (system-msg state side "pays 1 [Credits]"))
                            (continue-ability state :runner runner-trash-installed-sub card nil)))}]
    {:subroutines [sub
                   sub]
     :runner-abilities [(bioroid-break 1 1)]}))

(define-card "Fairchild 2.0"
  (let [sub {:label "Force the Runner to pay 2 [Credits] or trash an installed card"
             :msg "force the Runner to pay 2 [Credits] or trash an installed card"
             :player :runner
             :prompt "Choose one"
             :choices ["Pay 2 [Credits]" "Trash an installed card"]
             :effect (req (if (= target "Pay 2 [Credits]")
                            (do (pay state side card :credit 2)
                                (system-msg state side "pays 2 [Credits]"))
                            (continue-ability state :runner runner-trash-installed-sub card nil)))}]
    {:subroutines [sub
                   sub
                   (do-brain-damage 1)]
     :runner-abilities [(bioroid-break 2 2)]}))

(define-card "Fairchild 3.0"
  (let [sub {:label "Force the Runner to pay 3 [Credits] or trash an installed card"
             :msg "force the Runner to pay 3 [Credits] or trash an installed card"
             :player :runner
             :prompt "Choose one"
             :choices ["Pay 3 [Credits]" "Trash an installed card"]
             :effect (req (if (= target "Pay 3 [Credits]")
                            (do (pay state side card :credit 3)
                                (system-msg state side "pays 3 [Credits]"))
                            (continue-ability state :runner runner-trash-installed-sub card nil)))}]
    {:subroutines [sub
                   sub
                   {:label "Do 1 brain damage or end the run"
                    :prompt "Choose one"
                    :choices ["Do 1 brain damage" "End the run"]
                    :msg (msg (lower-case target))
                    :async true
                    :effect (req (if (= target "Do 1 brain damage")
                                   (damage state side eid :brain 1 {:card card})
                                   (end-run state side eid card)))}]
     :runner-abilities [(bioroid-break 3 3)]}))

(define-card "Fenris"
  {:effect take-bad-pub
   :subroutines [(do-brain-damage 1)
                 end-the-run]})

(define-card "Fire Wall"
  {:advanceable :always
   :subroutines [end-the-run]
   :strength-bonus advance-counters})

(define-card "Flare"
  {:subroutines [(trace-ability
                   6
                   {:label "Trash 1 hardware, do 2 meat damage, and end the run"
                    :async true
                    :effect
                    (effect
                      (continue-ability
                        {:prompt "Select a piece of hardware to trash"
                         :label "Trash a piece of hardware"
                         :choices {:card hardware?}
                         :msg (msg "trash " (:title target))
                         :async true
                         :effect (req (wait-for
                                        (trash state side target {:cause :subroutine})
                                        (system-msg state :corp
                                                    (str "uses Flare to trash " (:title target)))
                                        (wait-for (damage state side :meat 2 {:unpreventable true
                                                                              :card card})
                                        (system-msg state :corp
                                                    (str "uses Flare to deal 2 meat damage"))
                                        (system-msg state :corp
                                                    (str "uses Flare to end the run"))
                                        (end-run state side eid card))))
                         :cancel-effect (req (wait-for (damage state side :meat 2 {:unpreventable true
                                                                                   :card card})
                                                       (system-msg state :corp
                                                                   (str "uses Flare to deal 2 meat damage"))
                                                       (system-msg state :corp
                                                                   (str "uses Flare to end the run"))
                                                       (end-run state side eid card)))}
                        card nil))})]})

(define-card "Formicary"
  {:derezzed-events
   [{:event :approach-server
     :interactive (req true)
     :optional
     {:prompt "Rez Formicary?"
      :yes-ability
      {:msg "rez and move Formicary. The Runner is now approaching Formicary."
       :async true
       :effect (req (wait-for (rez state side card nil)
                              (move state side (get-card state card)
                                    [:servers (first (:server run)) :ices]
                                    {:front true})
                              (swap! state assoc-in [:run :position] 1)
                              (set-next-phase state :encounter-ice)
                              (set-current-ice state)
                              (update-all-ice state side)
                              (update-all-icebreakers state side)
                              (effect-completed state side eid)
                              (start-next-phase state side nil)))}}}]
   :subroutines [{:label "End the run unless the Runner suffers 2 net damage"
                  :player :runner
                  :async true
                  :prompt "Suffer 2 net damage or end the run?"
                  :choices ["2 net damage" "End the run"]
                  :effect (req (if (= target "End the run")
                                 (end-run state :corp eid card)
                                 (damage state :runner eid :net 2 {:card card :unpreventable true})))}]})

(define-card "Free Lunch"
  {:abilities [{:cost [:power 1]
                :label "Runner loses 1 [Credits]"
                :msg "make the Runner lose 1 [Credits]"
                :effect (effect (lose-credits :runner 1))}]
   :subroutines [add-power-counter
                 add-power-counter]})

(define-card "Galahad"
  (grail-ice end-the-run))

(define-card "Gatekeeper"
  (let [draw-ab {:async true
                 :prompt "Draw how many cards?"
                 :choices {:number (req 3)
                           :max (req 3)
                           :default (req 1)}
                 :msg (msg "draw " target "cards")
                 :effect (effect (draw eid target nil))}
        reveal-and-shuffle {:prompt "Reveal and shuffle up to 3 agendas"
                            :show-discard true
                            :choices {:card #(and (corp? %)
                                                  (or (= [:discard] (:zone %))
                                                      (= [:hand] (:zone %)))
                                                  (agenda? %))
                                      :max (req 3)}
                            :effect (req (reveal state side targets)
                                         (doseq [c targets]
                                           (move state :corp c :deck))
                                         (shuffle! state :corp :deck)
                                         (effect-completed state :corp eid))
                            :cancel-effect (effect (shuffle! :deck)
                                                   (effect-completed eid))
                            :msg (msg "add "
                                      (str (join ", " (map :title targets)))
                                      " to R&D")}
        draw-reveal-shuffle {:async true
                             :label "Draw cards, reveal and shuffle agendas"
                             :effect (req (wait-for (resolve-ability state side draw-ab card nil)
                                                    (continue-ability state side reveal-and-shuffle card nil)))}]
    {:strength-bonus (req (if (= :this-turn (:rezzed card)) 6 0))
     :subroutines [draw-reveal-shuffle
                   end-the-run]}))

(define-card "Gemini"
  (constellation-ice (do-net-damage 1)))

(define-card "Gold Farmer"
  {:on-break-subs {:req (req (some :printed (second targets)))
                   :msg (msg (let [n-subs (count (filter :printed (second targets)))]
                               (str "force the runner to lose "
                                    n-subs
                                    " [Credits] for breaking printed subs")))
                   :effect (req (dotimes [_ (count (filter :printed (second targets)))]
                                  (lose-credits state :runner 1)))}
   :subroutines [(end-the-run-unless-runner-pays 3)
                 (end-the-run-unless-runner-pays 3)]})

(define-card "Grim"
  {:effect take-bad-pub
   :subroutines [trash-program]})

(define-card "Guard"
  {:constant-effects [{:type :bypass-ice
                       :req (req (same-card? card target))
                       :value false}]
   :subroutines [end-the-run]})

(define-card "Gutenberg"
  {:subroutines [(tag-trace 7)]
   :strength-bonus (req (if (= (second (:zone card)) :rd) 3 0))})

(define-card "Gyri Labyrinth"
  {:implementation "Hand size is not restored if trashed or derezzed after firing"
   :subroutines [{:req (req (:run @state))
                  :label "Reduce Runner's maximum hand size by 2 until start of next Corp turn"
                  :msg "reduce the Runner's maximum hand size by 2 until the start of the next Corp turn"
                  :effect (effect (lose :runner :hand-size 2)
                                  (register-events
                                    card
                                    [{:event :corp-turn-begins
                                      :msg "increase the Runner's maximum hand size by 2"
                                      :effect (effect (gain :runner :hand-size 2)
                                                      (unregister-events card))}]))}]
   :events [{:event :corp-turn-begins}]})

(define-card "Hadrian's Wall"
  {:advanceable :always
   :subroutines [end-the-run end-the-run]
   :strength-bonus advance-counters})

(define-card "Hagen"
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
   :strength-bonus (req (- (count (filter #(has-subtype? % "Icebreaker")
                                          (all-active-installed state :runner)))))})

(define-card "Hailstorm"
  {:subroutines [{:label "Remove a card in the Heap from the game"
                  :prompt "Choose a card in the Runner's Heap"
                  :choices (req (:discard runner))
                  :msg (msg "remove " (:title target) " from the game")
                  :effect (effect (move :runner target :rfg))}
                 end-the-run]})

(define-card "Harvester"
  (let [sub {:label "Runner draws 3 cards and discards down to maximum hand size"
             :msg "make the Runner draw 3 cards and discard down to their maximum hand size"
             :async true
             :effect (req (wait-for (draw state :runner 3 nil)
                                    (continue-ability
                                      state :runner
                                      (let [delta (- (count (get-in @state [:runner :hand])) (hand-size state :runner))]
                                        (when (pos? delta)
                                          {:prompt (msg "Select " delta " cards to discard")
                                           :player :runner
                                           :choices {:max delta
                                                     :card #(in-hand? %)}
                                           :async true
                                           :effect (req (wait-for (trash-cards state :runner targets)
                                                                  (system-msg
                                                                    state :runner
                                                                    (str "trashes " (join ", " (map :title targets))))
                                                                  (effect-completed state side eid)))}))
                                      card nil)))}]
    {:subroutines [sub
                   sub]}))

(define-card "Heimdall 1.0"
  {:subroutines [(do-brain-damage 1)
                 end-the-run end-the-run]
   :runner-abilities [(bioroid-break 1 1)]})

(define-card "Heimdall 2.0"
  {:subroutines [(do-brain-damage 1)
                 {:msg "do 1 brain damage and end the run"
                  :effect (req (wait-for (damage state side :brain 1 {:card card})
                                         (end-run state side eid card)))}
                 end-the-run]
   :runner-abilities [(bioroid-break 2 2)]})

(define-card "Herald"
  {:flags {:rd-reveal (req true)}
   :subroutines [(gain-credits-sub 2)
                 {:async true
                  :label "Pay up to 2 [Credits] to place up to 2 advancement tokens"
                  :prompt "How many advancement tokens?"
                  :choices (req (map str (range (inc (min 2 (:credit corp))))))
                  :effect (req (let [c (str->int target)]
                                 (if (can-pay? state side (assoc eid :source card :source-type :subroutine) card (:title card) :credit c)
                                   (do (pay state :corp card :credit c)
                                       (continue-ability
                                         state side
                                         {:msg (msg "pay " c "[Credits] and place " (quantify c " advancement token")
                                                    " on " (card-str state target))
                                          :choices {:card can-be-advanced?}
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
                                        :yes-ability {:effect (effect (clear-wait-prompt :corp)
                                                                      (resolve-unbroken-subs! :corp eid card))}
                                        :no-ability {:effect (effect (clear-wait-prompt :corp)
                                                                     (effect-completed eid))}}}
                              card nil))}})

(define-card "Himitsu-Bako"
  {:abilities [{:msg "add it to HQ"
                :cost [:credit 1]
                :effect (effect (move card :hand))}]
   :subroutines [end-the-run]})

(define-card "Hive"
  (let [corp-points (fn [corp] (min 5 (max 0 (- 5 (:agenda-point corp 0)))))
        ability {:effect (effect (reset-printed-subs card (corp-points corp) end-the-run))}]
    {:events [(assoc ability
                     :event :rez
                     :req (req (same-card? card target)))
              (assoc ability :event :agenda-scored)
              (assoc ability
                     :event :as-agenda
                     :req (req (= "Corp" (:as-agenda-side target))))]
     :abilities [{:label "Lose subroutines"
                  :msg (msg "lose " (- 5 (corp-points corp)) " subroutines")
                  :effect (effect (reset-printed-subs card (corp-points corp) end-the-run))}]
     :subroutines [end-the-run
                   end-the-run
                   end-the-run
                   end-the-run
                   end-the-run]}))

(define-card "Holmegaard"
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

(define-card "Hortum"
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
    (let [breakable-fn (req (if (<= 3 (get-counters card :advancement))
                              (not (has-subtype? target "AI"))
                              :unrestricted))]
      {:advanceable :always
       :subroutines [{:label "Gain 1 [Credits] (Gain 4 [Credits])"
                      :breakable breakable-fn
                      :msg (msg "gain " (if (wonder-sub card 3) "4" "1") " [Credits]")
                      :effect (effect (gain-credits :corp (if (wonder-sub card 3) 4 1)))}
                     {:label "End the run (Search R&D for up to 2 cards and add them to HQ, shuffle R&D, end the run)"
                      :async true
                      :breakable breakable-fn
                      :effect (req (if (wonder-sub card 3)
                                     (wait-for
                                       (resolve-ability state side (hort 1) card nil)
                                       (do (system-msg state side
                                                       (str "uses Hortum to add 2 cards to HQ from R&D, "
                                                            "shuffle R&D, and end the run"))
                                           (end-run state side eid card)))
                                     (do (system-msg state side (str "uses Hortum to end the run"))
                                         (end-run state side eid card))))}]})))

(define-card "Hourglass"
  {:subroutines [runner-loses-click
                 runner-loses-click
                 runner-loses-click]})

(define-card "Howler"
  (let [ice-index (fn [state i] (first (keep-indexed #(when (same-card? %2 i) %1)
                                                     (get-in @state (cons :corp (:zone i))))))]
    {:subroutines
     [{:label "Install a piece of Bioroid ICE from HQ or Archives"
       :async true
       :prompt "Install ICE from HQ or Archives?"
       :choices ["HQ" "Archives"]
       :effect (effect
                 (continue-ability
                   (let [fr target]
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
                                     (trigger-event state side :corp-install newice)))})
                   card nil))}]
     :events [{:event :run-ends
               :req (req (:howler-target card))
               :async true
               :effect (effect (derez (get-card state (:howler-target card)))
                               (trash eid card {:cause :subroutine}))}]}))

(define-card "Hudson 1.0"
  (let [sub {:msg "prevent the Runner from accessing more than 1 card during this run"
             :effect (effect (max-access 1))}]
    {:subroutines [sub
                   sub]
     :runner-abilities [(bioroid-break 1 1)]}))

(define-card "Hunter"
  {:subroutines [(tag-trace 3)]})

(define-card "Hydra"
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
                     (req (end-run state side eid card)))]}))

(define-card "Ice Wall"
  {:advanceable :always
   :subroutines [end-the-run]
   :strength-bonus advance-counters})

(define-card "Ichi 1.0"
  {:subroutines [trash-program trash-program
                 (trace-ability 1 {:label "Give the Runner 1 tag and do 1 brain damage"
                                   :msg "give the Runner 1 tag and do 1 brain damage"
                                   :async true
                                   :effect (req (wait-for (damage state :runner :brain 1 {:card card})
                                                          (gain-tags state :corp eid 1)))})]
   :runner-abilities [(bioroid-break 1 1)]})

(define-card "Ichi 2.0"
  {:subroutines [trash-program trash-program
                 (trace-ability 3 {:label "Give the Runner 1 tag and do 1 brain damage"
                                   :msg "give the Runner 1 tag and do 1 brain damage"
                                   :async true
                                   :effect (req (wait-for (damage state :runner :brain 1 {:card card})
                                                          (gain-tags state :corp eid 1)))})]
   :runner-abilities [(bioroid-break 2 2)]})

(define-card "Inazuma"
  {:subroutines [{:msg "prevent the Runner from breaking subroutines on the next piece of ICE they encounter this run"
                  :effect (effect
                            (register-events
                              card
                              [{:event :encounter-ice
                                :duration :end-of-run
                                :unregister-once-resolved true
                                :msg (msg "prevent the runner from breaking subroutines on " (:title target))}]))}
                 {:msg "prevent the Runner from jacking out until after the next piece of ICE"
                  :effect (effect
                            (register-events
                              card
                              [{:event :encounter-ice
                                :duration :end-of-run
                                :unregister-once-resolved true
                                :msg "prevent the runner from jacking out"
                                :effect (req (prevent-jack-out state side)
                                             (register-events
                                               state side card
                                               [{:event :encounter-ice-ends
                                                 :duration :end-of-encounter
                                                 :unregister-once-resolved true
                                                 :effect (req (swap! state update :run dissoc :cannot-jack-out))}]))}]))}]})

(define-card "Information Overload"
  (let [ef (effect (reset-variable-subs card (count-tags state) runner-trash-installed-sub))
        ability {:effect ef}]
    {:on-encounter (tag-trace 1)
     :effect ef
     :events [(assoc ability :event :runner-gain-tag)
              (assoc ability :event :runner-lose-tag)]}))

(define-card "Interrupt 0"
  (let [sub {:label "Make the Runner pay 1 [Credits] to use icebreaker"
             :msg "make the Runner pay 1 [Credits] to use icebreakers to break subroutines during this run"
             :effect (effect (register-floating-effect
                               card
                               {:type :break-sub-additional-cost
                                :duration :end-of-run
                                :req (req (and ; The card is an icebreaker
                                               (has-subtype? target "Icebreaker")
                                               ; and is using a break ability
                                               (contains? (second targets) :break)
                                               (pos? (:break (second targets) 0))))
                                :value [:credit 1]}))}]
    {:subroutines [sub
                   sub]}))

(define-card "IP Block"
  {:on-encounter (assoc (give-tags 1)
                        :req (req (seq (filter #(has-subtype? % "AI") (all-active-installed state :runner))))
                        :msg "give the runner 1 tag because there is an installed AI")
   :subroutines [(tag-trace 3)
                 end-the-run-if-tagged]})

(define-card "IQ"
  {:effect (req (add-watch state (keyword (str "iq" (:cid card)))
                           (fn [k ref old new]
                             (let [handsize (count (get-in new [:corp :hand]))]
                               (when (not= (count (get-in old [:corp :hand])) handsize)
                                 (update! ref side (assoc (get-card ref card) :strength-bonus handsize))
                                 (update-ice-strength ref side (get-card ref card)))))))
   :subroutines [end-the-run]
   :strength-bonus (req (count (:hand corp)))
   :rez-cost-bonus (req (count (:hand corp)))
   :leave-play (req (remove-watch state (keyword (str "iq" (:cid card)))))})

(define-card "Ireress"
  (let [sub {:msg "make the Runner lose 1 [Credits]"
             :effect (effect (lose-credits :runner 1))}
        ability {:effect (effect (reset-variable-subs card (count-bad-pub state) sub))}]
    {:events [(assoc ability
                     :event :rez
                     :req (req (same-card? card target)))
              (assoc ability :event :corp-gain-bad-publicity)
              (assoc ability :event :corp-lose-bad-publicity)]}))

(define-card "It's a Trap!"
  {:expose {:msg "do 2 net damage"
            :async true
            :effect (effect (damage eid :net 2 {:card card}))}
   :subroutines [(assoc runner-trash-installed-sub
                        :effect (req (wait-for (trash state side target {:cause :subroutine})
                                               (trash state side eid card {:cause :subroutine}))))]})

(define-card "Janus 1.0"
  {:subroutines [(do-brain-damage 1)
                 (do-brain-damage 1)
                 (do-brain-damage 1)
                 (do-brain-damage 1)]
   :runner-abilities [(bioroid-break 1 1)]})

(define-card "Jua"
  {:on-encounter {:msg "prevent the Runner from installing cards for the rest of the turn"
                  :effect (effect (register-turn-flag! card :runner-lock-install (constantly true)))}
   :subroutines [{:label "Choose 2 installed Runner cards, if able. The Runner must add 1 of those to the top of the Stack."
                  :req (req (>= (count (all-installed state :runner)) 2))
                  :async true
                  :prompt "Select 2 installed Runner cards"
                  :choices {:card #(and (runner? %)
                                        (installed? %))
                            :max 2
                            :all true}
                  :msg (msg "add either " (card-str state (first targets))
                            " or " (card-str state (second targets))
                            " to the Stack")
                  :effect (req (if (= 2 (count targets))
                                 (do (show-wait-prompt state :corp "Runner to decide which card to move")
                                     (continue-ability
                                       state :runner
                                       {:player :runner
                                        :prompt "Select a card to move to the Stack"
                                        :choices {:card #(some (partial same-card? %) targets)}
                                        :effect (req (clear-wait-prompt state :corp)
                                                     (move state :runner target :deck {:front true})
                                                     (system-msg state :runner (str "selected " (card-str state target) " to move to the Stack")))}
                                       card nil))
                                 (effect-completed state side eid)))}]})

(define-card "Kakugo"
  {:events [{:event :pass-ice
             :async true
             :req (req (same-card? target card))
             :msg "do 1 net damage"
             :effect (effect (damage eid :net 1 {:card card}))}]
   :subroutines [end-the-run]})

(define-card "Kamali 1.0"
  (letfn [(better-name [kind] (if (= "hardware" kind) "piece of hardware" kind))
          (runner-trash [kind]
            {:prompt (str "Select an installed " (better-name kind) " to trash")
             :label (str "Trash an installed " (better-name kind))
             :msg (msg "trash " (:title target))
             :async true
             :choices {:card #(and (installed? %)
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
     :runner-abilities [(bioroid-break 1 1)]}))

(define-card "Kitsune"
  {:subroutines [{:optional
                  {:req (req (pos? (count (:hand corp))))
                   :prompt "Force the Runner to access a card in HQ?"
                   :yes-ability
                   {:async true
                    :prompt "Select a card in HQ to force access"
                    :choices {:card (every-pred in-hand? corp?)
                              :all true}
                    :label "Force the Runner to access a card in HQ"
                    :msg (msg "force the Runner to access " (:title target))
                    :effect (req (wait-for (do-access state :runner [:hq] {:no-root true
                                                                        :access-first target})
                                           (trash state side eid card {:cause :subroutine})))}}}]})

(define-card "Komainu"
  {:on-encounter {:effect (effect (gain-variable-subs card (count (:hand runner)) (do-net-damage 1)))}
   :events [{:event :run-ends
             :effect (effect (reset-variable-subs card 0 nil))}]})

(define-card "Konjin"
  {:implementation "Encounter effect is manual"
   :on-encounter (do-psi {:label "Force the runner to encounter another ice"
                          :prompt "Choose a piece of ice"
                          :choices {:card ice?
                                    :not-self true}
                          :msg (msg "force the Runner to encounter " (card-str state target))})})

(define-card "Lab Dog"
  {:subroutines [{:label "Force the Runner to trash an installed piece of hardware"
                  :player :runner
                  :async true
                  :msg (msg "force the Runner to trash " (:title target))
                  :prompt "Select a piece of hardware to trash"
                  :choices {:card #(and (installed? %)
                                        (hardware? %))}
                  :effect (req (wait-for (trash state side target {:cause :subroutine})
                                         (when current-ice
                                           (continue state :corp nil)
                                           (continue state :runner nil))
                                         (trash state side eid card {:cause :subroutine})))}]})

(define-card "Lancelot"
  (grail-ice trash-program))

(define-card "Little Engine"
  {:subroutines [end-the-run end-the-run
                 {:msg "make the Runner gain 5 [Credits]"
                  :effect (effect (gain-credits :runner 5))}]})

(define-card "Lockdown"
  {:subroutines [{:label "The Runner cannot draw cards for the remainder of this turn"
                  :msg "prevent the Runner from drawing cards"
                  :effect (effect (prevent-draw))}]})

(define-card "Loki"
  {:implementation "Encounter effects not implemented"
   :subroutines [{:label "End the run unless the Runner shuffles their Grip into the Stack"
                  :async true
                  :effect (req (if (zero? (count (:hand runner)))
                                 (do (system-msg state :corp (str "uses Loki to end the run"))
                                     (end-run state side eid card))
                                 (do (show-wait-prompt state :corp "Runner to decide to shuffle their Grip into the Stack")
                                     (continue-ability
                                       state :runner
                                       {:optional
                                        {:prompt "Reshuffle your Grip into the Stack?"
                                         :player :runner
                                         :yes-ability {:effect (req (doseq [c (:hand runner)]
                                                                      (move state :runner c :deck))
                                                                    (shuffle! state :runner :deck)
                                                                    (system-msg state :runner (str "shuffles their Grip into their Stack"))
                                                                    (clear-wait-prompt state :corp))}
                                         :no-ability {:async true
                                                      :effect (effect (clear-wait-prompt :corp)
                                                                      (system-msg :runner (str "doesn't shuffle their Grip into their Stack. Loki ends the run"))
                                                                      (end-run eid card))}}}
                                       card nil))))}]})

(define-card "Loot Box"
  (letfn [(top-3 [state] (take 3 (get-in @state [:runner :deck])))
          (top-3-names [state] (map :title (top-3 state)))]
    {:subroutines [(end-the-run-unless-runner-pays 2)
                   {:label "Reveal the top 3 cards of the Stack"
                    :async true
                    :effect (effect (system-msg (str "uses Loot Box to reveal the top 3 cards of the stack: "
                                                     (join ", " (top-3-names state))))
                              (reveal (top-3 state))
                              (show-wait-prompt :runner "Corp to choose a card to add to the Grip")
                              (continue-ability
                                {:prompt "Choose a card to add to the Grip"
                                 :choices (req (top-3 state))
                                 :msg (msg "add " (:title target) " to the Grip, gain " (:cost target)
                                           " [Credits] and shuffle the Stack. Loot Box is trashed")
                                 :async true
                                 :effect (effect (move :runner target :hand)
                                                 (gain-credits :corp (:cost target))
                                                 (shuffle! :runner :deck)
                                                 (clear-wait-prompt :runner)
                                                 (trash eid card {:cause :subroutine}))}
                                card nil))}]}))

(define-card "Lotus Field"
  {:subroutines [end-the-run]
   :flags {:cannot-lower-strength true}})

(define-card "Lycan"
  (morph-ice "Sentry" "Code Gate" trash-program))

(define-card "Macrophage"
  {:subroutines [(trace-ability 4 {:label "Purge virus counters"
                                   :msg "purge virus counters"
                                   :effect (effect (purge))})
                 (trace-ability 3 {:label "Trash a virus"
                                   :prompt "Choose a virus to trash"
                                   :choices {:card #(and (installed? %)
                                                         (has-subtype? % "Virus"))}
                                   :msg (msg "trash " (:title target))
                                   :async true
                                   :effect (effect (clear-wait-prompt :runner)
                                                   (trash eid target {:cause :subroutine}))})
                 (trace-ability 2 {:label "Remove a virus in the Heap from the game"
                                   :prompt "Choose a virus in the Heap to remove from the game"
                                   :choices (req (cancellable (filter #(has-subtype? % "Virus") (:discard runner)) :sorted))
                                   :msg (msg "remove " (:title target) " from the game")
                                   :effect (effect (move :runner target :rfg))})
                 (trace-ability 1 end-the-run)]})

(define-card "Magnet"
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
                                 :choices {:card #(and (program? %)
                                                       (ice? (:host %))
                                                       (not (same-card? (:host %) magnet)))}
                                 :effect (effect (host card target))}
                                card nil)
                              (disable-hosted state side card))))
     :derez-effect {:req (req (not-empty (:hosted card)))
                    :effect (req (doseq [c (get-in card [:hosted])]
                                   (card-init state side c {:resolve-effect false})))}
     :events [{:event :runner-install
               :req (req (same-card? card (:host target)))
               :effect (req (disable-hosted state side card)
                         (update-ice-strength state side card))}]
     :subroutines [end-the-run]}))

(define-card "Mamba"
  {:abilities [(power-counter-ability (do-net-damage 1))]
   :subroutines [(do-net-damage 1)
                 (do-psi {:label "Add 1 power counter"
                          :msg "add 1 power counter"
                          :effect (effect (add-counter card :power 1)
                                          (effect-completed eid))})]})

(define-card "Marker"
  {:subroutines [{:label "Give next encountered ice \"End the run\""
                  :msg (msg "give next encountered ice \"[Subroutine] End the run\" after all its other subroutines for the remainder of the run")
                  :effect (effect
                            (register-events
                              card
                              [{:event :encounter-ice
                                :once :per-turn
                                :duration :end-of-run
                                :unregister-once-resolved true
                                :req (req (rezzed? target))
                                :msg (msg "give " (:title target) "\"[Subroutine] End the run\" after all its other subroutines")
                                :effect (effect (add-sub! target end-the-run (:cid card) {:back true}))}
                               {:event :encounter-ice-ends
                                :duration :end-of-run
                                :effect (effect (remove-sub! target #(= (:cid card) (:from-cid %))))}]))}]})

(define-card "Markus 1.0"
  {:subroutines [runner-trash-installed-sub
                 end-the-run]
   :runner-abilities [(bioroid-break 1 1)]})

(define-card "Masvingo"
  (let [ability {:req (req (same-card? card target))
                 :effect (effect (reset-variable-subs card (get-counters card :advancement) end-the-run))}]
    {:advanceable :always
     :effect (effect (add-prop card :advance-counter 1))
     :events [(assoc ability :event :advance)
              (assoc ability :event :advancement-placed)
              (assoc ability :event :rez)]}))

(define-card "Matrix Analyzer"
  {:on-encounter {:cost [:credit 1]
                  :choices {:card can-be-advanced?}
                  :msg (msg "place 1 advancement token on " (card-str state target))
                  :effect (effect (add-prop target :advance-counter 1))}
   :subroutines [(tag-trace 2)]})

(define-card "Mausolus"
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
                               (if (wonder-sub card 3)
                                 (end-run state side eid card)
                                 (effect-completed state side eid)))}]})

(define-card "Meridian"
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
                                                     (end-run state :runner eid card))
                                                 (do (system-msg state :runner (str "adds Meridian to their score area as an agenda worth -1 agenda points"))
                                                     (clear-wait-prompt state :corp)
                                                     (wait-for (as-agenda state :runner card -1)
                                                               (when current-ice
                                                                 (continue state :corp nil)
                                                                 (continue state :runner nil))
                                                               (effect-completed state side eid)))))}
                                 card nil))}]})

(define-card "Merlin"
  (grail-ice (do-net-damage 2)))

(define-card "Meru Mati"
  {:subroutines [end-the-run]
   :strength-bonus (req (if (= (second (:zone card)) :hq) 3 0))})

(define-card "Metamorph"
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
                                    :choices {:card #(and (installed? %)
                                                          (ice? %))
                                              :max 2
                                              :all true}
                                    :msg (msg "swap the positions of " (card-str state (first targets))
                                              " and " (card-str state (second targets)))
                                    :effect (req (when (= (count targets) 2)
                                                   (swap-ice state side (first targets) (second targets))
                                                   (effect-completed state side eid)))}
                                   card nil)
                                 (continue-ability
                                   state side
                                   {:prompt "Select the two cards to swap"
                                    :async true
                                    :choices {:card #(and (installed? %)
                                                          (not (ice? %)))
                                              :max 2
                                              :all true}
                                    :msg (msg "swap the positions of " (card-str state (first targets)) " and " (card-str state (second targets)))
                                    :effect (req (when (= (count targets) 2)
                                                   (swap-installed state side (first targets) (second targets))
                                                   (effect-completed state side eid)))}
                                   card nil)))}]})

(define-card "Mganga"
  {:subroutines [(do-psi {:async true
                          :label "do 2 net damage"
                          :player :corp
                          :effect (req (wait-for (damage state :corp :net 2 {:card card})
                                                 (trash state :corp eid card {:cause :subroutine})))}
                         {:async true
                          :label "do 1 net damage"
                          :player :corp
                          :effect (req (wait-for (damage state :corp :net 1 {:card card})
                                                 (trash state :corp eid card {:cause :subroutine})))})]})

(define-card "Mind Game"
  {:subroutines [(do-psi {:label "Redirect the run to another server"
                          :player :corp
                          :prompt "Choose a server"
                          :choices (req (remove #{(-> @state :run :server central->name)} servers))
                          :msg (msg "redirect the run to " target
                                    " and for the remainder of the run, the runner must add 1 installed card to the bottom of their stack as an additional cost to jack out")
                          :effect (effect (redirect-run target :approach-ice)
                                          (register-floating-effect
                                            card
                                            {:type :jack-out-additional-cost
                                             :duration :end-of-run
                                             :value [:add-installed-to-bottom-of-deck 1]})
                                          (effect-completed eid)
                                          (start-next-phase nil))})]})

(define-card "Minelayer"
  {:subroutines [{:msg "install an ICE from HQ"
                  :async true
                  :choices {:card #(and (ice? %)
                                        (in-hand? %))}
                  :prompt "Choose an ICE to install from HQ"
                  :effect (effect (corp-install eid target (zone->name (first (:server run))) {:ignore-all-cost true}))}]})

(define-card "Mirāju"
  {:events [{:event :encounter-ice-ends
             :req (req (and (same-card? card target)
                            (:broken (first (filter :printed (:subroutines target))))))
             :msg "make the Runner continue the run on Archives. Mirāju is derezzed"
             :effect (req (redirect-run state side "Archives" :approach-ice)
                          (derez state side card))}]
   :subroutines [{:async true
                  :label "Draw 1 card, then shuffle 1 card from HQ into R&D"
                  :effect (req (wait-for (resolve-ability
                                           state side
                                           {:optional
                                            {:prompt "Draw 1 card?"
                                             :yes-ability {:async true
                                                           :msg "draw 1 card"
                                                           :effect (effect (draw eid 1 nil))}}}
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

(define-card "Mlinzi"
  (letfn [(net-or-trash [net-dmg mill-cnt]
            {:label (str "Do " net-dmg " net damage")
             :async true
             :effect (req (show-wait-prompt state :corp "Runner to choose an option for Mlinzi")
                          (continue-ability
                            state :runner
                            {:prompt "Take net damage or trash cards from the stack?"
                             :choices (concat [(str "Take " net-dmg " net damage")]
                                              (when (<= mill-cnt (count (:deck runner)))
                                                [(str "Trash the top " mill-cnt " cards of the stack")]))
                             :async true
                             :effect (req (clear-wait-prompt state :corp)
                                          (if (= target (str "Take " net-dmg " net damage"))
                                            (do (system-msg state :corp
                                                            (str "uses Mlinzi to do "
                                                                 net-dmg " net damage"))
                                                (clear-wait-prompt state :corp)
                                                (damage state :runner eid :net net-dmg {:card card}))
                                            (do (system-msg state :corp
                                                            (str "uses Mlinzi to trash "
                                                                 (join ", " (map :title (take mill-cnt (:deck runner))))
                                                                 " from the runner's stack"))
                                                (mill state :runner eid :runner mill-cnt))))}
                            card nil))})]
    {:subroutines [(net-or-trash 1 2)
                   (net-or-trash 2 3)
                   (net-or-trash 3 4)]}))

(define-card "Mother Goddess"
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
     :events [(assoc mg :event :rez)
              (assoc mg :event :card-moved)
              (assoc mg :event :derez)
              (assoc mg :event :ice-subtype-changed)]}))

(define-card "Muckraker"
  {:effect take-bad-pub
   :subroutines [(tag-trace 1)
                 (tag-trace 2)
                 (tag-trace 3)
                 end-the-run-if-tagged]})

(define-card "Najja 1.0"
  {:subroutines [end-the-run end-the-run]
   :runner-abilities [(bioroid-break 1 1)]})

(define-card "Nebula"
  (space-ice trash-program))

(define-card "Negotiator"
  {:subroutines [(gain-credits-sub 2)
                 trash-program]
   :runner-abilities [(bioroid-break 2 1)]})

(define-card "Nerine 2.0"
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
    {:subroutines [sub
                   sub]
     :runner-abilities [(bioroid-break 2 2)]}))

(define-card "Neural Katana"
  {:subroutines [(do-net-damage 3)]})

(define-card "News Hound"
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
     :events [(assoc ab :event :play-event)
              (assoc ab :event :play-operation)
              {:event :trash-current
               :msg "make News Hound lose \"[subroutine] End the run\""
               :effect (effect (continue-ability
                                 (reset-variable-subs state side card 0 nil)
                                 card nil))}]
     :subroutines [(tag-trace 3)]}))

(define-card "NEXT Bronze"
  {:subroutines [end-the-run]
   :strength-bonus (req (next-ice-count corp))
   :events (let [nb {:req (req (and (not (same-card? target card))
                                    (has-subtype? target "NEXT")))
                     :effect (effect (update-ice-strength card))}]
             [(assoc nb :event :rez)
              (assoc nb :event :derez)
              (assoc nb :event :trash)
              (assoc nb :event :card-moved)])})

(define-card "NEXT Diamond"
  {:rez-cost-bonus (req (- (next-ice-count corp)))
   :subroutines [(do-brain-damage 1)
                 (do-brain-damage 1)
                 {:prompt "Select a card to trash"
                  :label "Trash 1 installed Runner card"
                  :msg (msg "trash " (:title target))
                  :choices {:card #(and (installed? %)
                                        (runner? %))}
                  :async true
                  :effect (effect (trash eid target {:cause :subroutine}))}]})

(define-card "NEXT Gold"
  {:subroutines [{:label "Do 1 net damage for each rezzed NEXT ice"
                  :msg (msg "do " (next-ice-count corp) " net damage")
                  :effect (effect (damage eid :net (next-ice-count corp) {:card card}))}
                 trash-program]})

(define-card "NEXT Opal"
  (let [sub {:label "Install a card from HQ, paying all costs"
             :prompt "Choose a card in HQ to install"
             :req (req (some #(not (operation? %)) (:hand corp)))
             :choices {:card #(and (not (operation? %))
                                   (in-hand? %)
                                   (corp? %))}
             :async true
             :effect (effect (corp-install eid target nil nil))
             :msg (msg (corp-install-msg target))}
        ability {:req (req (and (ice? target)
                                (has-subtype? target "NEXT")))
                 :effect (effect (reset-variable-subs card (next-ice-count corp) sub))}]
    {:events [(assoc ability :event :rez)
              (assoc ability :event :derez)]}))

(define-card "NEXT Sapphire"
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
                  :choices {:card #(and (corp? %)
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
                  :choices {:card #(and (corp? %)
                                        (in-hand? %))
                            :max (req (next-ice-count corp))}
                  :effect (req (doseq [c targets]
                                 (move state :corp c :deck))
                               (shuffle! state :corp :deck))
                  :cancel-effect (effect (shuffle! :corp :deck))
                  :msg (msg "shuffle " (count targets) " cards from HQ into R&D")}]})

(define-card "NEXT Silver"
  (let [ability {:req (req (and (ice? target)
                                (has-subtype? target "NEXT")))
                 :effect (effect (reset-variable-subs card (next-ice-count corp) end-the-run))}]
    {:events [(assoc ability :event :rez)
              (assoc ability :event :derez)]}))

(define-card "Nightdancer"
  (let [sub {:label (str "The Runner loses [Click], if able. "
                         "You have an additional [Click] to spend during your next turn.")
             :msg (str "force the runner to lose a [Click], if able. "
                       "Corp gains an additional [Click] to spend during their next turn")
             :effect (req (lose state :runner :click 1)
                          (swap! state update-in [:corp :extra-click-temp] (fnil inc 0)))}]
    {:subroutines [sub
                   sub]}))

(define-card "Oduduwa"
  {:on-encounter
   {:msg "place 1 advancement counter on Oduduwa"
    :async true
    :effect (effect (add-prop card :advance-counter 1 {:placed true})
                    (continue-ability
                      (let [card (get-card state card)
                            counters (get-counters card :advancement)]
                        {:optional
                         {:prompt (str "Place " (quantify counters "advancement counter") " on another ice?")
                          :yes-ability
                          {:msg (msg "place " (quantify counters "advancement counter") " on " (card-str state target))
                           :choices {:card ice?
                                     :not-self true}
                           :effect (effect (add-prop target :advance-counter counters {:placed true}))}}})
                      (get-card state card) nil))}
   :subroutines [end-the-run
                 end-the-run]})

(define-card "Orion"
  (space-ice trash-program
             (resolve-another-subroutine)
             end-the-run))

(define-card "Otoroshi"
  {:subroutines [{:async true
                  :label "Place 3 advancement tokens on installed card"
                  :msg "place 3 advancement tokens on installed card"
                  :prompt "Choose an installed Corp card"
                  :req (req (some (complement ice?) (all-installed state :corp)))
                  :choices {:card #(and (corp? %)
                                        (installed? %)
                                        (not (ice? %)))}
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
                                   card nil)))}]})

(define-card "Owl"
  {:subroutines [{:choices {:card #(and (installed? %)
                                        (program? %))}
                  :label "Add installed program to the top of the Runner's Stack"
                  :msg "add an installed program to the top of the Runner's Stack"
                  :effect (effect (move :runner target :deck {:front true})
                                  (system-msg (str "adds " (:title target) " to the top of the Runner's Stack")))}]})

(define-card "Pachinko"
  {:subroutines [end-the-run-if-tagged
                 end-the-run-if-tagged]})

(define-card "Paper Wall"
  {:events [{:event :subroutines-broken
             :req (req (and (same-card? card target)
                            (empty? (remove :broken (:subroutines target)))))
             :async true
             :effect (effect (trash :corp eid card {:cause :effect}))}]
   :subroutines [end-the-run]})

(define-card "Peeping Tom"
  (let [sub (end-the-run-unless-runner
              "takes 1 tag"
              "take 1 tag"
              (give-tags 1))]
    {:on-encounter {:prompt "Choose a card type"
                    :choices ["Event" "Hardware" "Program" "Resource"]
                    :effect (req (let [n (count (filter #(is-type? % target) (:hand runner)))]
                                   (system-msg state side
                                               (str "uses Peeping Tom to name " target ", then reveals "
                                                    (join ", " (map :title (:hand runner)))
                                                    " in the Runner's Grip. Peeping Tom gains " n " subroutines"))
                                   (reveal state side (:hand runner))
                                   (gain-variable-subs state side card n sub)))}
     :events [{:event :run-ends
               :effect (effect (reset-variable-subs card 0 nil))}]}))

(define-card "Pop-up Window"
  {:on-encounter (gain-credits-sub 1)
   :subroutines [(end-the-run-unless-runner-pays 1)]})

(define-card "Pup"
  (let [sub {:player :runner
             :async true
             :label (str "Do 1 net damage unless the Runner pays 1 [Credits]")
             :prompt (str "Suffer 1 net damage or pay 1 [Credits]?")
             :choices ["Suffer 1 net damage"
                       "Pay 1 [Credits]"]
             :effect (req (if (= "Suffer 1 net damage" target)
                            (continue-ability state side (do-net-damage 1) card nil)
                            (pay-sync state :runner eid card [:credit 1])))}]
    {:subroutines [sub
                   sub]}))

(define-card "Quandary"
  {:subroutines [end-the-run]})

(define-card "Quicksand"
  {:on-encounter {:msg "add 1 power counter to Quicksand"
                  :effect (effect (add-counter card :power 1)
                                  (update-all-ice))}
   :subroutines [end-the-run]
   :strength-bonus (req (get-counters card :power))})

(define-card "Rainbow"
  {:subroutines [end-the-run]})

(define-card "Ravana 1.0"
  (let [sub (resolve-another-subroutine
              #(has-subtype? % "Bioroid")
              "Resolve a subroutine on a rezzed bioroid ice")]
    {:subroutines [sub
                   sub]
     :runner-abilities [(bioroid-break 1 1)]}))

(define-card "Red Tape"
  {:subroutines [{:label "Give +3 strength to all ICE for the remainder of the run"
                  :msg "give +3 strength to all ICE for the remainder of the run"
                  :effect (effect (pump-ice target 3 :end-of-run))}]})

(define-card "Resistor"
  (let [resistor-effect {:effect (effect (update! (assoc (get-card state card) :strength-bonus (count-tags state)))
                                         (update-ice-strength (get-card state card)))}]
    {:events [(assoc resistor-effect :event :runner-gain-tag)
              (assoc resistor-effect :event :runner-lose-tag)
              (assoc resistor-effect :event :runner-additional-tag-change)]
     :strength-bonus (req (count-tags state))
     :subroutines [(trace-ability 4 end-the-run)]}))

(define-card "Rime"
  {:implementation "Can be rezzed anytime already"
   :effect (effect (update-all-ice))
   :subroutines [{:label "Runner loses 1 [Credit]"
                  :msg "force the Runner to lose 1 [Credit]"
                  :effect (effect (lose-credits :runner 1))}]
   :constant-effects [{:type :ice-strength
                       :req (req (protecting-same-server? card target))
                       :value 1}]
   :events [{:event :corp-moved
             :req (req (ice? target))
             :effect (effect (update-ice-strength target))}
            {:event :corp-install
             :req (req (ice? target))
             :effect (effect (update-ice-strength target))}]})

(define-card "Rototurret"
  {:subroutines [trash-program
                 end-the-run]})

(define-card "Sadaka"
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
                                             (trash state :corp target {:cause :subroutine})
                                             (system-msg state :corp "trashes a card from HQ")
                                             (continue-ability state side trash-resource-sub card nil)))}
                             card nil)
                           (system-msg state :corp "trashes Sadaka")
                           (clear-wait-prompt state :runner)
                           (when current-ice
                             (continue state :corp nil)
                             (continue state :runner nil))
                           (trash state :corp eid card nil)))}]}))

(define-card "Sagittarius"
  (constellation-ice trash-program))

(define-card "Saisentan"
  (let [sub {:label "Do 1 net damage"
             :async true
             :msg "do 1 net damage"
             :effect (req (wait-for (damage state side :net 1 {:card card})
                                    (when-let* [choice (get-in card [:special :saisentan])
                                                cards (last async-result)
                                                dmg (some #(when (= (:type %) choice) %) cards)]
                                               (system-msg state :corp "uses Saisentan to deal a second net damage")
                                               (damage state side eid :net 1 {:card card}))))}]
    {:on-encounter
     {:effect (effect (show-wait-prompt :runner "Corp to choose Saisentan card type")
                      (continue-ability
                        {:prompt "Choose a card type"
                         :choices ["Event" "Hardware" "Program" "Resource"]
                         :msg (msg "choose the card type " target)
                         :effect (effect (update! (assoc-in card [:special :saisentan] target))
                                         (clear-wait-prompt :runner))}
                        card nil))}
     :events [{:event :encounter-ice-ends
               :req (req (get-in card [:special :saisentan]))
               :effect (effect (update! (dissoc-in card [:special :saisentan])))}]
     :subroutines [sub
                   sub
                   sub]}))

(define-card "Salvage"
  (zero-to-hero (tag-trace 2)))

(define-card "Sand Storm"
  {:subroutines [{:async true
                  :label "Move Sand Storm and the run to another server"
                  :prompt "Choose another server and redirect the run to its outermost position"
                  :choices (req (remove #{(zone->name (:server (:run @state)))} (cancellable servers)))
                  :msg (msg "move Sand Storm and the run. The Runner is now running on " target ". Sand Storm is trashed")
                  :effect (effect (redirect-run target :approach-ice)
                                  (trash eid card {:unpreventable true :cause :subroutine}))}]})

(define-card "Sandstone"
  {:subroutines [end-the-run]
   :strength-bonus (req (- (get-counters card :virus)))
   :on-encounter {:msg "place 1 virus counter on Sandstone"
                  :effect (effect (add-counter card :virus 1)
                                  (update-ice-strength (get-card state card)))}})

(define-card "Sandman"
  {:subroutines [add-runner-card-to-grip
                 add-runner-card-to-grip]})

(define-card "Sapper"
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
                                :yes-ability
                                {:effect (effect (clear-wait-prompt :corp)
                                                 (show-wait-prompt :runner "Corp to trash a program with Sapper")
                                                 (resolve-unbroken-subs! :corp eid card))}
                                :no-ability
                                {:effect (effect (clear-wait-prompt :corp)
                                                 (effect-completed eid))}}}
                              card nil))}})

(define-card "Searchlight"
  (let [sub {:label "Trace X - Give the Runner 1 tag"
             :trace {:base advance-counters
                     :label "Give the Runner 1 tag"
                     :successful (give-tags 1)}}]
    {:advanceable :always
     :subroutines [sub
                   sub]}))

(define-card "Seidr Adaptive Barrier"
  (let [recalculate-strength (req (update-ice-strength state side (get-card state card)))
        recalc-event {:req (req (= (:zone target) (:zone card)))
                      :effect recalculate-strength}]
    {:effect recalculate-strength
     :strength-bonus (req (count (:ices (card->server state card))))
     :subroutines [end-the-run]
     :events [(assoc recalc-event :event :card-moved)
              (assoc recalc-event :event :corp-install)]}))

(define-card "Self-Adapting Code Wall"
  {:subroutines [end-the-run]
   :flags {:cannot-lower-strength true}})

(define-card "Sensei"
  {:subroutines [{:label "Give encountered ice \"End the run\""
                  :msg (msg "give encountered ice \"[Subroutine] End the run\" after all its other subroutines for the remainder of the run")
                  :effect (effect
                            (register-events
                              card
                              [{:event :encounter-ice
                                :duration :end-of-run
                                :req (req (not (same-card? card target)))
                                :msg (msg "give " (:title target) "\"[Subroutine] End the run\" after all its other subroutines")
                                :effect (effect (add-sub! target end-the-run (:cid card) {:back true}))}
                               {:event :encounter-ice-ends
                                :duration :end-of-run
                                :effect (effect (remove-sub! target #(= (:cid card) (:from-cid %))))}]))}]})

(define-card "Shadow"
  {:advanceable :always
   :subroutines [(gain-credits-sub 2)
                 (tag-trace 3)]
   :strength-bonus advance-counters})

(define-card "Sherlock 1.0"
  (let [sub (trace-ability 4 {:choices {:card #(and (installed? %)
                                                    (program? %))}
                              :label "Add an installed program to the top of the Runner's Stack"
                              :msg (msg "add " (:title target) " to the top of the Runner's Stack")
                              :effect (effect (move :runner target :deck {:front true}))})]
    {:subroutines [sub
                   sub]
     :runner-abilities [(bioroid-break 1 1)]}))

(define-card "Sherlock 2.0"
  (let [sub (trace-ability 4 {:choices {:card #(and (installed? %)
                                                    (program? %))}
                              :label "Add an installed program to the bottom of the Runner's Stack"
                              :msg (msg "add " (:title target) " to the bottom of the Runner's Stack")
                              :effect (effect (move :runner target :deck))})]
    {:subroutines [sub
                   sub
                   (give-tags 1)]
     :runner-abilities [(bioroid-break 2 2)]}))

(define-card "Shinobi"
  {:effect take-bad-pub
   :subroutines [(trace-ability 1 (do-net-damage 1))
                 (trace-ability 2 (do-net-damage 2))
                 (trace-ability 3 {:label "Do 3 net damage and end the run"
                                   :msg "do 3 net damage and end the run"
                                   :effect (req (wait-for (damage state side :net 3 {:card card})
                                                          (end-run state side eid card)))})]})

(define-card "Shiro"
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
                 {:optional
                  {:prompt "Pay 1 [Credits] to keep the Runner from accessing the top card of R&D?"
                   :yes-ability {:cost [:credit 1]
                                 :msg "keep the Runner from accessing the top card of R&D"}
                   :no-ability {:async true
                                :msg "make the Runner access the top card of R&D"
                                :effect (effect (do-access :runner eid [:rd] {:no-root true}))}}}]})

(define-card "Slot Machine"
  (letfn [(top-3 [state] (take 3 (get-in @state [:runner :deck])))
          (effect-type [card] (keyword (str "slot-machine-top-3-" (:cid card))))
          (name-builder [card] (str (:title card) " (" (:type card) ")"))
          (top-3-names [cards] (map name-builder cards))
          (top-3-types [state card et]
            (->> (get-effects state :corp card et)
                 first
                 (keep :type)
                 (into #{})
                 count))]
    {:on-encounter {:effect (req (move state :runner (first (:deck runner)) :deck)
                              (let [t3 (top-3 state)
                                    effect-type (effect-type card)]
                                (register-floating-effect
                                  state side card
                                  {:type effect-type
                                   :duration :end-of-encounter
                                   :value t3})
                                (reveal state side t3)
                                (system-msg state side
                                            (str "uses Slot Machine to put the top card of the stack to the bottom,"
                                                 " then reveal the top 3 cards in the stack: "
                                                 (join ", " (top-3-names t3))))))}
     :subroutines [{:label "Runner loses 3 [Credits]"
                    :msg "force the Runner to lose 3 [Credits]"
                    :effect (effect (lose-credits :runner 3))}
                   {:label "Gain 3 [Credits]"
                    :effect (req (let [et (effect-type card)
                                       unique-types (top-3-types state card et)]
                                   ;; When there are 3 cards in the deck, sub needs 2 or fewer unique types
                                   ;; When there are 2 cards in the deck, sub needs 1 unique type
                                   (when (or (and (<= unique-types 2)
                                                  (= 3 (count (first (get-effects state :corp card et)))))
                                             (and (= unique-types 1)
                                                  (= 2 (count (first (get-effects state :corp card et))))))
                                     (system-msg state :corp (str "uses Slot Machine to gain 3 [Credits]"))
                                     (gain-credits state :corp 3))))}
                   {:label "Place 3 advancement tokens"
                    :async true
                    :effect (effect
                              (continue-ability
                                (let [et (effect-type card)
                                      unique-types (top-3-types state card et)]
                                  (when (and (= 3 (count (first (get-effects state :corp card et))))
                                             (= 1 unique-types))
                                    {:choices {:card installed?}
                                     :prompt "Choose an installed card"
                                     :msg (msg "place 3 advancement tokens on "
                                               (card-str state target))
                                     :effect (effect (add-prop target :advance-counter 3 {:placed true}))}))
                                card nil))}]}))

(define-card "Snoop"
  {:on-encounter {:msg (msg "reveal the Runner's Grip (" (join ", " (map :title (:hand runner))) ")")
                  :effect (effect (reveal (:hand runner)))}
   :abilities [{:async true
                :req (req (pos? (get-counters card :power)))
                :cost [:power 1]
                :label "Reveal all cards in Grip and trash 1 card"
                :msg (msg "look at all cards in Grip and trash " (:title target)
                          " using 1 power counter")
                :choices (req (cancellable (:hand runner) :sorted))
                :prompt "Choose a card to trash"
                :effect (effect (reveal (:hand runner))
                                (trash eid target {:cause :subroutine}))}]
   :subroutines [(trace-ability 3 add-power-counter)]})

(define-card "Snowflake"
  {:subroutines [(do-psi end-the-run)]})

(define-card "Special Offer"
  {:subroutines [{:label "Gain 5 [Credits] and trash Special Offer"
                  :msg "gains 5 [Credits] and trashes Special Offer"
                  :async true
                  :effect (req (gain-credits state :corp 5)
                               (when current-ice
                                 (continue state :corp nil)
                                 (continue state :runner nil))
                               (trash state side eid card {:cause :subroutine}))}]})

(define-card "Spiderweb"
  {:subroutines [end-the-run
                 end-the-run
                 end-the-run]})

(define-card "Surveyor"
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
     :events [(assoc recalc-event :event :card-moved)
              (assoc recalc-event :event :corp-install)]}))

(define-card "Susanoo-no-Mikoto"
  {:subroutines [{:req (req (not= (:server run) [:discard]))
                  :msg "make the Runner continue the run on Archives"
                  :effect (req (redirect-run state side "Archives" :approach-ice)
                               (register-events
                                 state side card
                                 [{:event :approach-ice
                                   :duration :end-of-run
                                   :unregister-once-resolved true
                                   :msg "prevent the runner from jacking out"
                                   :effect (req (prevent-jack-out state side)
                                                (register-events
                                                  state side card
                                                  [{:event :encounter-ice-ends
                                                    :duration :end-of-encounter
                                                    :unregister-once-resolved true
                                                    :effect (req (swap! state update :run dissoc :cannot-jack-out))}]))}]))}]})

(define-card "Swarm"
  (let [sub (end-the-run-unless-runner-pays 3)
        ability {:req (req (same-card? card target))
                 :effect (effect (reset-variable-subs card (get-counters card :advancement) sub))}]
    {:advanceable :always
     :effect take-bad-pub
     :events [(assoc ability :event :advance)
              (assoc ability :event :advancement-placed)
              (assoc ability :event :rez)]}))

(define-card "Swordsman"
  {:implementation "AI restriction not implemented"
   :subroutines [(do-net-damage 1)
                 {:async true
                  :prompt "Select an AI program to trash"
                  :msg (msg "trash " (:title target))
                  :label "Trash an AI program"
                  :choices {:card #(and (installed? %)
                                        (program? %)
                                        (has-subtype? % "AI"))}
                  :effect (effect (trash eid target {:cause :subroutine}))}]})

(define-card "SYNC BRE"
  {:subroutines [(tag-trace 4)
                 (trace-ability 2 {:label "Runner reduces cards accessed by 1 for this run"
                                   :async true
                                   :msg "reduce cards accessed for this run by 1"
                                   :effect (effect (access-bonus :total -1))})]})

(define-card "Tapestry"
  {:subroutines [runner-loses-click
                 {:async true
                  :msg "draw 1 card"
                  :effect (effect (draw eid 1 nil))}
                 {:req (req (pos? (count (:hand corp))))
                  :prompt "Choose a card in HQ to move to the top of R&D"
                  :choices {:card #(and (in-hand? %)
                                        (corp? %))}
                  :msg "add 1 card in HQ to the top of R&D"
                  :effect (effect (move target :deck {:front true}))}]})

(define-card "Taurus"
  (constellation-ice trash-hardware))

(define-card "Thimblerig"
  (let [ability {:optional
                 {:req (req (and (<= 2 (count (filter ice? (all-installed state :corp))))
                                 (if run (same-card? target card) true)))
                  :prompt "Swap Thimblerig with another ice?"
                  :yes-ability {:prompt "Choose a piece of ice to swap Thimblerig with"
                                :choices {:card ice?
                                          :not-self true}
                                :effect (effect (swap-ice card target))
                                :msg (msg "swap " (card-str state card) " with " (card-str state target))}}}]
    {:events [(assoc ability :event :pass-ice)
              (assoc ability :event :corp-turn-begins)]
     :subroutines [end-the-run]}))

(define-card "Thoth"
  {:on-encounter (give-tags 1)
   :subroutines [(trace-ability 4 {:label "Do 1 net damage for each Runner tag"
                                   :async true
                                   :msg (msg "do " (count-tags state) " net damage")
                                   :effect (effect (damage eid :net (count-tags state) {:card card}))})
                 (trace-ability 4 {:label "Runner loses 1 [Credits] for each tag"
                                   :async true
                                   :msg (msg "force the Runner to lose " (count-tags state) " [Credits]")
                                   :effect (effect (lose-credits :runner (count-tags state)))})]})

(define-card "Tithonium"
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
                                              :card #(and (installed? %)
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
                                 (end-run state side eid card)))}]})

(define-card "TL;DR"
  {:subroutines
   [{:label "Double subroutines on an ICE"
     :effect (effect
               (register-events
                 card
                 [{:event :encounter-ice
                   :duration :end-of-run
                   :unregister-once-resolved true
                   :msg (msg "duplicate each subroutine on " (:title target))
                   :effect
                   (req (let [curr-subs (map #(assoc % :from-cid (:cid target)) (:subroutines target))
                              tldr-subs (map #(assoc % :from-cid (:cid card)) curr-subs)
                              new-subs (->> (interleave curr-subs tldr-subs)
                                            (reduce
                                              (fn [ice sub] (add-sub ice sub (:from-cid sub) nil))
                                              (assoc target :subroutines []))
                                            :subroutines
                                            (into []))
                              new-card (assoc target :subroutines new-subs)]
                          (update! state :corp new-card)
                          (register-events
                            state side card
                            (let [cid (:cid card)]
                              [{:event :encounter-ice-ends
                                :duration :end-of-encounter
                                :unregister-once-resolved true
                                :req (req (get-card state new-card))
                                :effect (effect (remove-subs! (get-card state new-card) #(= cid (:from-cid %))))}]))))}]))}]})

(define-card "TMI"
  {:trace {:base 2
           :msg "keep TMI rezzed"
           :label "Keep TMI rezzed"
           :unsuccessful {:effect (effect (derez card))}}
   :subroutines [end-the-run]})

(define-card "Tollbooth"
  {:on-encounter {:async true
                  :effect (req (wait-for (pay-sync state :runner card [:credit 3])
                                         (if-let [cost-str async-result]
                                           (do (system-msg state :corp "uses Tollbooth to force the Runner to pay 3 [Credits]")
                                               (effect-completed state side eid))
                                           (do (system-msg state :corp "uses Tollbooth to end the run as the Runner can't pay 3 [Credits]")
                                               (end-run state :corp eid card)))))}
   :subroutines [end-the-run]})

(define-card "Tour Guide"
  (let [ef (effect (reset-variable-subs card (count (filter asset? (all-active-installed state :corp))) end-the-run))
        ability {:label "Reset number of subs"
                 :req (req (asset? target))
                 :effect ef}
        trash-req (req (some #(and (asset? %)
                                   (installed? %)
                                   (rezzed? %))
                             targets))]
    {:effect ef
     :events [(assoc ability :event :rez)
              (assoc ability :event :derez)
              (assoc ability :event :game-trash :req trash-req)
              (assoc ability :event :corp-trash :req trash-req)
              (assoc ability :event :runner-trash :req trash-req)]}))

(define-card "Trebuchet"
  {:effect take-bad-pub
   :subroutines [{:prompt "Select a card to trash"
                  :label "Trash 1 installed Runner card"
                  :msg (msg "trash " (:title target))
                  :choices {:card #(and (installed? %)
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
                                                                       (toast state :runner "Cannot trash due to Trebuchet." "warning")))))})]})

(define-card "Tribunal"
  {:subroutines [runner-trash-installed-sub
                 runner-trash-installed-sub
                 runner-trash-installed-sub]})

(define-card "Troll"
  {:on-encounter
   (trace-ability 2 {:msg "force the Runner to lose [Click] or end the run"
                     :player :runner
                     :prompt "Choose one"
                     :choices ["Lose [Click]" "End the run"]
                     :async true
                     :effect (req (if (and (= target "Lose [Click]")
                                           (can-pay? state :runner (assoc eid :source card :source-type :subroutine) card nil [:click 1]))
                                    (do (system-msg state :runner "loses [Click]")
                                        (lose state :runner :click 1)
                                        (effect-completed state :runner eid))
                                    (do (system-msg state :corp "ends the run")
                                        (end-run state :corp eid card))))})})

(define-card "Tsurugi"
  {:subroutines [(end-the-run-unless-corp-pays 1)
                 (do-net-damage 1)
                 (do-net-damage 1)
                 (do-net-damage 1)]})

(define-card "Turing"
  {:implementation "AI restriction not implemented"
   :subroutines [(end-the-run-unless-runner
                   "spends [Click][Click][Click]"
                   "spend [Click][Click][Click]"
                   (runner-pay [:click 3] 1))]
   :strength-bonus (req (if (is-remote? (second (:zone card))) 3 0))})

(define-card "Turnpike"
  {:on-encounter {:msg "force the Runner to lose 1 [Credits]"
                  :effect (effect (lose-credits :runner 1))}
   :subroutines [(tag-trace 5)]})

(define-card "Tyrant"
  (zero-to-hero end-the-run))

(define-card "Týr"
  {:subroutines [(do-brain-damage 2)
                 (combine-abilities trash-installed-sub (gain-credits-sub 3))
                 end-the-run]
   :runner-abilities [(bioroid-break 1 1 {:additional-ability {:effect (req (swap! state update-in [:corp :extra-click-temp] (fnil inc 0)))}})]})

(define-card "Universal Connectivity Fee"
  {:subroutines [{:label "Force the Runner to lose credits"
                  :msg (msg "force the Runner to lose " (if tagged "all credits" "1 [Credits]"))
                  :async true
                  :effect (req (if tagged
                                 (do (lose-credits state :runner :all)
                                     (lose state :runner :run-credit :all)
                                     (when current-ice
                                       (continue state :corp nil)
                                       (continue state :runner nil))
                                     (trash state side eid card {:cause :subroutine}))
                                 (do (lose-credits state :runner 1)
                                     (effect-completed state side eid))))}]})

(define-card "Upayoga"
  {:subroutines [(do-psi {:label "Make the Runner lose 2 [Credits]"
                          :msg "make the Runner lose 2 [Credits]"
                          :effect (effect (lose-credits :runner 2)
                                          (effect-completed eid))})
                 (resolve-another-subroutine
                   #(has-subtype? % "Psi")
                   "Resolve a subroutine on a rezzed psi ice")]})

(define-card "Uroboros"
  {:subroutines [(trace-ability 4 {:label "Prevent the Runner from making another run"
                                   :msg "prevent the Runner from making another run"
                                   :effect (effect (register-turn-flag! card :can-run nil))})
                 (trace-ability 4 end-the-run)]})

(define-card "Vanilla"
  {:subroutines [end-the-run]})

(define-card "Veritas"
  {:subroutines [{:label "Corp gains 2 [Credits]"
                  :msg "gain 2 [Credits]"
                  :effect (effect (gain-credits :corp 2))}
                 {:label "Runner loses 2 [Credits]"
                  :msg "force the Runner to lose 2 [Credits]"
                  :effect (effect (lose-credits :runner 2))}
                 (trace-ability 2 (give-tags 1))]})

(define-card "Vikram 1.0"
  {:implementation "Program prevention is not implemented"
   :subroutines [{:msg "prevent the Runner from using programs for the remainder of this run"}
                 (trace-ability 4 (do-brain-damage 1))
                 (trace-ability 4 (do-brain-damage 1))]
   :runner-abilities [(bioroid-break 1 1)]})

(define-card "Viktor 1.0"
  {:subroutines [(do-brain-damage 1)
                 end-the-run]
   :runner-abilities [(bioroid-break 1 1)]})

(define-card "Viktor 2.0"
  {:abilities [(power-counter-ability (do-brain-damage 1))]
   :subroutines [(trace-ability 2 add-power-counter)
                 end-the-run]
   :runner-abilities [(bioroid-break 2 2)]})

(define-card "Viper"
  {:subroutines [(trace-ability 3 runner-loses-click)
                 (trace-ability 3 end-the-run)]})

(define-card "Virgo"
  (constellation-ice (give-tags 1)))

(define-card "Waiver"
  {:subroutines [(trace-ability
                   5 {:label "Reveal the grip and trash cards"
                      :msg (msg "reveal all cards in the grip: " (join ", " (map :title (:hand runner))))
                      :async true
                      :effect (req (reveal state side (:hand runner))
                                   (let [delta (- target (second targets))
                                         cards (filter #(<= (:cost %) delta) (:hand runner))]
                                     (system-msg state side (str "uses Waiver to trash "
                                                                 (join ", " (map :title cards))))
                                     (trash-cards state side eid cards {:cause :subroutine})))})]})

(define-card "Wall of Static"
  {:subroutines [end-the-run]})

(define-card "Wall of Thorns"
  {:subroutines [(do-net-damage 2)
                 end-the-run]})

(define-card "Watchtower"
  {:subroutines [{:label "Search R&D and add 1 card to HQ"
                  :prompt "Choose a card to add to HQ"
                  :msg "add a card from R&D to HQ"
                  :choices (req (cancellable (:deck corp) :sorted))
                  :cancel-effect (effect (system-msg "cancels the effect of Watchtower"))
                  :effect (effect (shuffle! :deck)
                                  (move target :hand))}]})

(define-card "Weir"
  {:subroutines [runner-loses-click
                 {:label "Runner trashes 1 card from their Grip"
                  :req (req (pos? (count (:hand runner))))
                  :prompt "Choose a card to trash from your Grip"
                  :player :runner
                  :choices (req (:hand runner))
                  :not-distinct true
                  :async true
                  :effect (effect (system-msg :runner (str "trashes " (:title target) " from their Grip"))
                                  (trash :runner eid target {:cause :subroutine}))}]})

(define-card "Wendigo"
  (implementation-note
    "Program prevention is not implemented"
    (morph-ice "Code Gate" "Barrier"
               {:msg "prevent the Runner from using a chosen program for the remainder of this run"})))

(define-card "Whirlpool"
  {:subroutines [{:label "The Runner cannot jack out for the remainder of this run"
                  :msg "prevent the Runner from jacking out"
                  :async true
                  :effect (req (prevent-jack-out state side)
                               (when current-ice
                                 (continue state :corp nil)
                                 (continue state :runner nil))
                               (trash state side eid card {:cause :subroutine}))}]})

(define-card "Winchester"
  (let [protecting-hq-req (req (= (second (:zone card)) :hq))
        ab {:req protecting-hq-req
            :effect (effect (continue-ability
                              (reset-variable-subs state side card 1 (trace-ability 3 end-the-run) {:back true})
                              card nil))}]
    {:subroutines [(trace-ability 4 trash-program)
                   (trace-ability 3 trash-hardware)]
     :effect (effect (continue-ability ab card nil))
     :events [(assoc ab :event :rez)
              (assoc ab :event :card-moved)
              (assoc ab :event :approach-ice)]}))

(define-card "Woodcutter"
  (zero-to-hero (do-net-damage 1)))

(define-card "Wormhole"
  (space-ice (resolve-another-subroutine)))

(define-card "Wotan"
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
                   (do-brain-damage 1))]})

(define-card "Wraparound"
  {:subroutines [end-the-run]
   :strength-bonus (req (if (some #(has-subtype? % "Fracter") (all-active-installed state :runner))
                          0 7))
   :events (let [wr {:silent (req true)
                     :req (req (and (not (same-card? target card))
                                    (has-subtype? target "Fracter")))
                     :effect (effect (update-ice-strength card))}]
             [(assoc wr :event :runner-install)
              (assoc wr :event :trash)
              (assoc wr :event :card-moved)])})

(define-card "Yagura"
  {:subroutines [{:msg "look at the top card of R&D"
                  :optional {:prompt (msg "Move " (:title (first (:deck corp))) " to the bottom of R&D?")
                             :yes-ability {:msg "move the top card of R&D to the bottom"
                                           :effect (effect (move (first (:deck corp)) :deck))}
                             :no-ability {:effect (effect (system-msg :corp (str "does not use Yagura to move the top card of R&D to the bottom")))}}}
                 (do-net-damage 1)]})

(define-card "Zed 1.0"
  {:implementation "Restriction on having spent [click] is not implemented"
   :subroutines [(do-brain-damage 1)
                 (do-brain-damage 1)]
   :runner-abilities [(bioroid-break 1 1)]})

(define-card "Zed 2.0"
  {:implementation "Restriction on having spent [click] is not implemented"
   :subroutines [trash-hardware
                 trash-hardware
                 (do-brain-damage 2)]
   :runner-abilities [(bioroid-break 2 2)]})
