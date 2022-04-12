(ns game.cards.ice
  (:require [game.core :refer :all]
            [game.utils :refer :all]
            [jinteki.utils :refer :all]
            [clojure.string :as string]
            [cond-plus.core :refer [cond+]]))

;;;; Helper functions specific for ice
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

;;; Checks if the runner has active events that would force them to avoid/prevent a tag
(defn forced-to-avoid-tags?
  [state side]
  (let [cards (map :card (gather-events state side :pre-tag nil))]
    (pos? (count (filter #(card-flag? % :forced-to-avoid-tag true) cards)))))

;;; Runner abilites for breaking subs
(defn bioroid-break
  ([cost qty] (bioroid-break cost qty nil))
  ([cost qty args]
   (break-sub [:lose-click cost] qty nil args)))

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
                  (continue-ability state side (runner-pays [:credit amount]) card nil)))})

(defn end-the-run-unless-corp-pays
  [amount]
  {:async true
   :label (str "End the run unless the Corp pays " amount " [Credits]")
   :prompt (str "End the run or pay " amount " [Credits]?")
   :choices ["End the run"
             (str "Pay " amount " [Credits]")]
   :effect (req (if (= "End the run" target)
                  (end-run state :corp eid card)
                  (wait-for (pay state :corp (make-eid state eid) card [:credit amount])
                            (when-let [payment-str (:msg async-result)]
                              (system-msg state :corp payment-str))
                            (effect-completed state side eid))))})

(defn end-the-run-unless-runner
  [label prompt ability]
  {:player :runner
   :async true
   :label (str "End the run unless the Runner " label)
   :prompt (str "End the run or " prompt "?")
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
  "Adds 1 power counter to the card."
  {:label "Place 1 power counter"
   :msg "place 1 power counter"
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

(def runner-loses-click
  ; Runner loses a click effect
  {:label "Force the Runner to lose 1 [Click]"
   :msg "force the Runner to lose 1 [Click] if able"
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
   :waiting-prompt "Corp to choose a target"
   :prompt "Choose a card"
   :choices {:card #(and (installed? %)
                         (runner? %))}
   :msg "add 1 installed card to the Runner's Grip"
   :effect (effect (move :runner target :hand true)
                   (system-msg (str "adds " (:title target)
                                    " to the Runner's Grip")))})

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
    :choices {:card #(and (corp? %)
                          (not (operation? %))
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
                     (fn [state side card]
                       ((constantly false)
                        (toast state :runner "Cannot steal due to subroutine." "warning"))))
                   (register-run-flag!
                     card :can-trash
                     (fn [state side card]
                       ((constantly (not (corp? card)))
                        (toast state :runner "Cannot trash due to subroutine." "warning")))))})

;;; For Advanceable ice
(defn wall-ice
  [subroutines]
  {:advanceable :always
   :subroutines subroutines
   :strength-bonus (req (get-counters card :advancement))})

(defn space-ice
  "Creates data for Space ice with specified abilities."
  [& abilities]
  {:advanceable :always
   :subroutines (vec abilities)
   :rez-cost-bonus (req (* -3 (get-counters card :advancement)))})

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
          (msg "reveal " (string/join ", " (map #(str (:title %) " (" (sub-label %) ")") targets))))})

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

;;; For NEXT ice
(defn next-ice-count
  "Counts number of rezzed pieces of NEXT ice - for use with NEXT Bronze and NEXT Gold"
  [corp]
  (let [servers (flatten (seq (:servers corp)))
        rezzed-next? #(and (rezzed? %) (has-subtype? % "NEXT"))]
    (reduce (fn [c server] (+ c (count (filter rezzed-next? (:ices server))))) 0 servers)))

;;; For Morph ice
(defn morph-ice
  "Creates the data for morph ice with specified types and ability."
  [base other ability]
  {:advanceable :always
   :constant-effects [{:type :lose-subtype
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
  (let [ability {:req (req (same-card? card target))
                 :effect (effect (reset-variable-subs card (get-counters card :advancement) sub))}]
    {:advanceable :while-rezzed
     :events [(assoc ability :event :advance)
              (assoc ability :event :advancement-placed)
              {:event :rez
               :req (req (same-card? card (:card context)))
               :effect (effect (reset-variable-subs card (get-counters card :advancement) sub))}]}))

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

(defcard "Anemone"
  {:on-rez {:optional {:prompt "trash a card from HQ to do 2 net damage?"
                       :req (req (and (< 0 (count (:hand corp)))
                                      run
                                      this-server))
                       :waiting-prompt "Corp to resolve Anemone"
                       :yes-ability {:msg "do 2 net damage"
                                     :cost [:trash-from-hand 1]
                                     :async true
                                     :effect (effect (damage eid :net 2 {:card card}))}
                       :no-ability {:msg "decline to deal 2 net damage"}}}
   :subroutines [(do-net-damage 1)]})

(defcard "Ansel 1.0"
  {:subroutines [trash-installed-sub
                 (install-from-hq-or-archives-sub)
                 cannot-steal-or-trash-sub]
   :runner-abilities [(bioroid-break 1 1)]})

(defcard "Afshar"
  (let [breakable-fn (req (if (= :hq (second (get-zone card)))
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
                  :label "Trash the top 3 cards of the Stack. Trash Aimor."
                  :effect (req (system-msg state :corp
                                           (str "uses Aimor to trash "
                                                (string/join ", " (map :title (take 3 (:deck runner))))
                                                " from the Runner's Stack"))
                               (wait-for (mill state :corp :runner 3)
                                         (system-msg state side (str "trashes Aimor"))
                                         (trash state side eid card {:cause :subroutine})))}]})

(defcard "Akhet"
  (let [breakable-fn (req (if (<= 3 (get-counters card :advancement))
                            (empty? (filter #(and (:broken %) (:printed %)) (:subroutines card)))
                            :unrestricted))]
    {:advanceable :always
     :subroutines [{:label "Gain 1 [Credit]. Place 1 advancement token."
                    :breakable breakable-fn
                    :msg (msg "gain 1 [Credit] and place 1 advancement token on " (card-str state target))
                    :prompt "Choose an installed card"
                    :choices {:card installed?}
                    :async true
                    :effect (effect (add-prop target :advance-counter 1 {:placed true})
                                    (gain-credits eid 1))}
                   (assoc end-the-run :breakable breakable-fn)]
     :strength-bonus (req (if (<= 3 (get-counters card :advancement)) 3 0))}))

(defcard "Anansi"
  (let [corp-draw {:optional
                   {:waiting-prompt "Corp to choose an option"
                    :prompt "Draw 1 card?"
                    :yes-ability {:async true
                                  :msg "draw 1 card"
                                  :effect (effect (draw eid 1))}}}
        runner-draw {:player :runner
                     :optional
                     {:waiting-prompt "Runner to choose an option"
                      :prompt "Pay 2 [Credits] to draw 1 card?"
                      :yes-ability
                      {:async true
                       :effect (req (wait-for (pay state :runner (make-eid state eid) card [:credit 2])
                                              (if (:cost-paid async-result)
                                                (do (system-msg state :runner "pays 2 [Credits] to draw 1 card")
                                                    (draw state :runner eid 1))
                                                (do (system-msg state :runner "does not draw 1 card")
                                                    (effect-completed state side eid)))))}
                      :no-ability {:effect (effect (system-msg :runner "does not draw 1 card"))}}}]
    {:subroutines [{:msg "rearrange the top 5 cards of R&D"
                    :async true
                    :waiting-prompt "Corp to make a decision"
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
   :access
   {:optional
    {:req (req (not (in-discard? card)))
     :waiting-prompt "Corp to choose an option"
     :prompt "Pay 3 [Credits] to force Runner to encounter Archangel?"
     :player :corp
     :yes-ability
     {:cost [:credit 3]
      :async true
      :msg "force the Runner to encounter Archangel"
      :effect (req (force-ice-encounter state side eid card))}
     :no-ability {:effect (effect (system-msg :corp "declines to use Archangel to force the Runner to encounter it"))}}}
   :subroutines [(trace-ability 6 add-runner-card-to-grip)]})

(defcard "Archer"
  {:additional-cost [:forfeit]
   :subroutines [(gain-credits-sub 2)
                 trash-program-sub
                 trash-program-sub
                 end-the-run]})

(defcard "Architect"
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
                  :prompt "Choose a card to install from Archives or HQ"
                  :show-discard true
                  :choices {:card #(and (corp? %)
                                        (not (operation? %))
                                        (or (in-hand? %)
                                            (in-discard? %)))}
                  :async true
                  :msg (msg (corp-install-msg target))
                  :effect (effect (corp-install eid target nil nil))}]})

(defcard "Ashigaru"
  {:on-rez {:effect (effect (reset-variable-subs card (count (:hand corp)) end-the-run))}
   :events [{:event :card-moved
             :req (req (let [target (nth targets 1)]
                         (and (corp? target)
                              (or (in-hand? target)
                                  (= :hand (first (:previous-zone target)))))))
             :effect (effect (reset-variable-subs card (count (:hand corp)) end-the-run))}]})

(defcard "Assassin"
  {:subroutines [(trace-ability 5 (do-net-damage 3))
                 (trace-ability 4 trash-program-sub)]})

(defcard "Asteroid Belt"
  (space-ice end-the-run))

(defcard "Authenticator"
  {:on-encounter {:optional
                  {:req (req (and (not (:bypass run))
                                  (not (forced-to-avoid-tags? state side))))
                   :player :runner
                   :prompt "Take 1 tag to bypass?"
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
                                    (lose-credits state :runner eid 1)))}]
    {:on-rez {:effect (effect (reset-variable-subs card (sub-count corp) sub {:variable true :front true}))}
     :events [{:event :card-moved
               :req (req (let [target (nth targets 1)]
                           (and (operation? target)
                                (has-subtype? target "Transaction")
                                (or (in-discard? target)
                                    (= :discard (first (:previous-zone target)))))))
               :effect (effect (reset-variable-subs card (sub-count corp) sub {:variable true :front true}))}]
     :subroutines [sub
                   end-the-run]}))

(defcard "Bloodletter"
  {:subroutines [{:async true
                  :label "Runner trashes 1 program or top 2 cards of their Stack"
                  :effect (req (if (empty? (filter program? (all-active-installed state :runner)))
                                 (do (system-msg state :runner "trashes the top 2 cards of their Stack")
                                     (mill state :runner eid :runner 2))
                                 (continue-ability
                                   state :runner
                                   {:waiting-prompt "Runner to choose an option"
                                    :prompt "Trash 1 program or trash top 2 cards of the Stack?"
                                    :choices (req [(when (seq (filter program? (all-active-installed state :runner)))
                                                     "Trash 1 program")
                                                   (when (<= 1 (count (:deck runner)))
                                                     "Trash top 2 of Stack")])
                                    :async true
                                    :effect (req (if (= target "Trash top 2 of Stack")
                                                   (do (system-msg state :runner "trashes the top 2 cards of their Stack")
                                                       (mill state :runner eid :runner 2))
                                                   (continue-ability state :runner trash-program-sub card nil)))}
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

(defcard "Border Control"
  {:abilities [{:label "End the run"
                :msg "end the run"
                :async true
                :cost [:trash-can]
                :effect (effect (end-run eid card))}]
   :subroutines [{:label "Gain 1 [Credits] for each ice protecting this server"
                  :msg (msg "gain "
                            (count (:ices (card->server state card)))
                            " [Credits]")
                  :async true
                  :effect (req (let [num-ice (count (:ices (card->server state card)))]
                                 (gain-credits state :corp eid num-ice)))}
                 end-the-run]})

(defcard "Brainstorm"
  {:on-encounter {:effect (effect (gain-variable-subs card (count (:hand runner)) (do-brain-damage 1)))}
   :events [{:event :run-ends
             :effect (effect (reset-variable-subs card 0 nil))}]})

(defcard "Builder"
  (let [sub {:label "Place 1 advancement token on a piece of ice that can be advanced protecting this server"
             :msg (msg "place 1 advancement token on " (card-str state target))
             :choices {:card #(and (ice? %)
                                   (can-be-advanced? %))}
             :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]
    {:abilities [{:label "Move Builder to the outermost position of any server"
                  :cost [:click 1]
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
                  :cancel-effect (req (system-msg state :corp "declines to use Brân 1.0 to install a card")
                                      (effect-completed state side eid))}
                 end-the-run
                 end-the-run]
   :runner-abilities [(bioroid-break 1 1)]})

(defcard "Bullfrog"
  {:subroutines [(do-psi {:label "Move Bullfrog to another server"
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
                 (do-psi {:label "Do 1 net damage for each card in the Runner's grip"
                          :msg (msg "do " (count (get-in @state [:runner :hand])) " net damage")
                          :effect (effect (damage eid :net (count (get-in @state [:runner :hand])) {:card card}))})]})

(defcard "Chimera"
  {:on-rez {:prompt "Choose one subtype"
            :choices ["Barrier" "Code Gate" "Sentry"]
            :msg (msg "make itself gain " target)
            :effect (effect (update! (assoc card :subtype-target target)))}
   :constant-effects [{:type :gain-subtype
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
  {:implementation "Trash effect when using an AI to break is activated manually"
   :abilities [{:async true
                :label "Trash the top 2 cards of the Runner's Stack"
                :req (req (some #(has-subtype? % "AI") (all-active-installed state :runner)))
                :msg (msg (str "trash " (string/join ", " (map :title (take 2 (:deck runner)))) " from the Runner's Stack"))
                :effect (effect (mill :corp eid :runner 2))}]
   :subroutines [(do-net-damage 2)
                 (do-net-damage 2)
                 end-the-run]})

(defcard "Chrysalis"
  {:flags {:rd-reveal (req true)}
   :subroutines [(do-net-damage 2)]
   :access {:async true
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
                        (register-floating-effect
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
   :strength-bonus (req (if (some #(has-subtype? % "AI") (all-active-installed state :runner)) 3 0))})

(defcard "Cortex Lock"
  {:subroutines [{:label "Do 1 net damage for each unused memory unit the Runner has"
                  :msg (msg "do " (available-mu state) " net damage")
                  :effect (effect (damage eid :net (available-mu state) {:card card}))}]})

(defcard "Crick"
  {:subroutines [{:label "install a card from Archives"
                  :prompt "Choose a card to install from Archives"
                  :show-discard true
                  :async true
                  :choices {:card #(and (not (operation? %))
                                        (in-discard? %)
                                        (corp? %))}
                  :msg (msg (corp-install-msg target))
                  :effect (effect (corp-install eid target nil nil))}]
   :strength-bonus (req (if (= (second (get-zone card)) :archives) 3 0))})

(defcard "Curtain Wall"
  {:subroutines [end-the-run
                 end-the-run
                 end-the-run]
   :strength-bonus (req (let [ices (:ices (card->server state card))]
                          (if (same-card? card (last ices)) 4 0)))
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
                      :label "Look at the top of the stack"
                      :msg "look at top X cards of the stack"
                      :waiting-prompt "Corp to make a decision"
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
                                {:prompt (str "Choose " (quantify n "card") " in your Grip to add to the top of the Stack (second card targeted will be topmost)")
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

(defcard "Data Mine"
  {:subroutines [{:msg "do 1 net damage"
                  :async true
                  :effect (req (wait-for (damage state :runner :net 1 {:card card})
                                         (trash state :corp eid card {:cause :subroutine})))}]})

(defcard "Data Raven"
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

(defcard "Data Ward"
  {:on-encounter {:player :runner
                  :prompt "Choose one"
                  :choices ["Pay 3 [Credits]" "Take 1 tag"]
                  :async true
                  :effect (req (if (= target "Pay 3 [Credits]")
                                 (wait-for (pay state :runner (make-eid state eid) card :credit 3)
                                           (when-let [payment-str (:msg async-result)]
                                             (system-msg state :runner payment-str))
                                           (effect-completed state side eid))
                                 (do (system-msg state :runner "takes 1 tag on encountering Data Ward")
                                     (gain-tags state :runner eid 1))))}
   :subroutines [end-the-run-if-tagged
                 end-the-run-if-tagged
                 end-the-run-if-tagged
                 end-the-run-if-tagged]})

(defcard "Datapike"
  {:subroutines [{:msg "force the Runner to pay 2 [Credits] if able"
                  :async true
                  :effect (req (wait-for (pay state :runner (make-eid state eid) card :credit 2)
                                         (when-let [payment-str (:msg async-result)]
                                           (system-msg state :runner payment-str))
                                         (effect-completed state side eid)))}
                 end-the-run]})

(defcard "Diviner"
  {:subroutines
   [{:label "Do 1 net damage"
     :async true
     :msg "do 1 net damage"
     :effect (req (wait-for
                    (damage state :corp (make-eid state eid) :net 1 {:card card})
                    (let [[trashed-card] async-result]
                      (cond+
                        [(nil? trashed-card)
                         (effect-completed state side eid)]
                        [(odd? (:cost trashed-card))
                         (system-say
                           state :corp
                           (str (:title trashed-card)
                                " has an odd cost so Corp uses Diviner to end the run."))
                         (end-run state :corp eid card)]
                        [:else
                         (system-say
                           state :corp
                           (str (:title trashed-card)
                                " has an even cost so Corp does not use Diviner to end the run."))
                         (effect-completed state side eid)]))))}]})

(defcard "DNA Tracker"
  (let [sub {:msg "do 1 net damage and make the Runner lose 2 [Credits]"
             :async true
             :effect (req (wait-for (damage state side :net 1 {:card card})
                                    (lose-credits state :runner eid 2)))}]
    {:subroutines [sub
                   sub
                   sub]}))

(defcard "Dracō"
  {:on-rez {:prompt "How many power counters?"
            :choices :credit
            :msg (msg "place " (quantify target "power counter"))
            :effect (effect (add-counter card :power target)
                            (update-ice-strength card))}
   :strength-bonus (req (get-counters card :power))
   :subroutines [(trace-ability 2 {:label "Give the Runner 1 tag and end the run"
                                   :msg "give the Runner 1 tag and end the run"
                                   :async true
                                   :effect (req (wait-for (gain-tags state :corp 1)
                                                          (end-run state :corp eid card)))})]})

(defcard "Drafter"
  {:subroutines [(corp-recur)
                 (install-from-hq-or-archives-sub {:ignore-all-cost true})]})

(defcard "Eli 1.0"
  {:subroutines [end-the-run
                 end-the-run]
   :runner-abilities [(bioroid-break 1 1)]})

(defcard "Eli 2.0"
  {:subroutines [{:async true
                  :msg "draw 1 card"
                  :effect (effect (draw eid 1))}
                 end-the-run
                 end-the-run]
   :runner-abilities [(bioroid-break 2 2)]})

(defcard "Endless EULA"
  (let [sub (end-the-run-unless-runner-pays 1)]
    (letfn [(break-fn [unbroken-subs total]
              {:async true
               :effect
               (req (if (seq unbroken-subs)
                      (wait-for (pay state :runner (make-eid state (assoc eid :source-type :subroutine)) card [:credit 1])
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
  {:additional-cost [:forfeit]
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
             :msg (msg "reveal " (quantify (count (:hand runner)) "card")
                       " from grip: " (string/join ", " (map :title (:hand runner))))
             :effect (effect (reveal eid (:hand runner)))}]
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
                                       :prompt "Choose revealed card to trash"
                                       :choices (req (concat (filter #(is-type? % cardtype) targets) ["None"]))
                                       :msg (msg "trash " (:title target) " from grip")
                                       :effect (req (if (= "None" target)
                                                      (effect-completed state side eid)
                                                      (trash state side eid target {:cause :subroutine})))}])))}
     :subroutines [sub
                   sub]}))

(defcard "Enigma"
  {:subroutines [runner-loses-click
                 end-the-run]})

(defcard "Envelope"
  {:subroutines [(do-net-damage 1)
                 end-the-run]})

(defcard "Errand Boy"
  (let [sub {:async true
             :label "Draw a card or gain 1 [Credits]"
             :prompt "Choose one:"
             :choices ["Gain 1 [Credits]" "Draw 1 card"]
             :msg (req (if (= target "Gain 1 [Credits]")
                         "gain 1 [Credits]"
                         "draw 1 card"))
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
   :runner-abilities [(break-sub [:credit 2] 1 nil {:req (req (not tagged))})]})

(defcard "Fairchild"
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

(defcard "Fairchild 1.0"
  (let [sub {:label "Force the Runner to pay 1 [Credits] or trash an installed card"
             :msg "force the Runner to pay 1 [Credits] or trash an installed card"
             :player :runner
             :prompt "Choose one"
             :choices ["Pay 1 [Credits]" "Trash an installed card"]
             :async true
             :effect (req (if (= target "Pay 1 [Credits]")
                            (wait-for (pay state side (make-eid state eid) card :credit 1)
                                      (system-msg state side (:msg async-result))
                                      (effect-completed state side eid))
                            (continue-ability state :runner runner-trash-installed-sub card nil)))}]
    {:subroutines [sub
                   sub]
     :runner-abilities [(bioroid-break 1 1)]}))

(defcard "Fairchild 2.0"
  (let [sub {:label "Force the Runner to pay 2 [Credits] or trash an installed card"
             :msg "force the Runner to pay 2 [Credits] or trash an installed card"
             :player :runner
             :prompt "Choose one"
             :choices ["Pay 2 [Credits]" "Trash an installed card"]
             :async true
             :effect (req (if (= target "Pay 2 [Credits]")
                            (wait-for (pay state side (make-eid state eid) card :credit 2)
                                      (system-msg state side (:msg async-result))
                                      (effect-completed state side eid))
                            (continue-ability state :runner runner-trash-installed-sub card nil)))}]
    {:subroutines [sub
                   sub
                   (do-brain-damage 1)]
     :runner-abilities [(bioroid-break 2 2)]}))

(defcard "Fairchild 3.0"
  (let [sub {:label "Force the Runner to pay 3 [Credits] or trash an installed card"
             :msg "force the Runner to pay 3 [Credits] or trash an installed card"
             :player :runner
             :prompt "Choose one"
             :choices ["Pay 3 [Credits]" "Trash an installed card"]
             :async true
             :effect (req (if (= target "Pay 3 [Credits]")
                            (wait-for (pay state side (make-eid state eid) card :credit 3)
                                      (system-msg state side (:msg async-result))
                                      (effect-completed state side eid))
                            (continue-ability state :runner runner-trash-installed-sub card nil)))}]
    {:subroutines [sub
                   sub
                   {:label "Do 1 brain damage or end the run"
                    :prompt "Choose one"
                    :choices ["Do 1 brain damage" "End the run"]
                    :msg (msg (string/lower-case target))
                    :async true
                    :effect (req (if (= target "Do 1 brain damage")
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

(defcard "Formicary"
  {:derezzed-events
   [{:event :approach-server
     :interactive (req true)
     :optional
     {:prompt "Rez Formicary?"
      :autoresolve (get-autoresolve :auto-formicary)
      :req (req (and (can-rez? state side card)
                     (can-pay? state side eid card nil (get-rez-cost state side card nil))))
      :yes-ability
      {:msg "rez and move Formicary. The Runner is now encountering Formicary"
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
   :subroutines [{:label "End the run unless the Runner suffers 2 net damage"
                  :player :runner
                  :async true
                  :prompt "Suffer 2 net damage or end the run?"
                  :choices ["2 net damage" "End the run"]
                  :effect (req (if (= target "End the run")
                                 (do (system-msg state :runner "chooses to end the run")
                                     (end-run state :corp eid card))
                                 (damage state :runner eid :net 2 {:card card :unpreventable true})))}]
   :abilities [(set-autoresolve :auto-formicary "Formicary")]})

(defcard "Free Lunch"
  {:abilities [{:cost [:power 1]
                :label "Runner loses 1 [Credits]"
                :msg "make the Runner lose 1 [Credits]"
                :async true
                :effect (effect (lose-credits :runner eid 1))}]
   :subroutines [add-power-counter
                 add-power-counter]})

(defcard "Funhouse"
  {:on-encounter {:msg "force the Runner to take 1 tag or end the run"
                  :player :runner
                  :prompt "Choose one"
                  :choices (req [(when-not (forced-to-avoid-tags? state side)
                                   "Take 1 tag")
                                 "End the run"])
                  :async true
                  :effect (req (if (= target "Take 1 tag")
                                 (do (system-msg state :runner "chooses to take 1 tag")
                                     (gain-tags state :runner eid 1 {:unpreventable true}))
                                 (do (system-msg state :runner "ends the run")
                                     (end-run state :runner eid card))))}
   :subroutines [{:player :runner
                  :async true
                  :label "Give the Runner 1 tag unless they pay 4 [Credits]"
                  :prompt "Take 1 tag or pay 4 [Credits]"
                  :choices (req ["Take 1 tag"
                                 (when (can-pay? state :runner eid card "Funhouse" :credit 4)
                                   "Pay 4 [Credits]")])
                  :effect (effect (continue-ability
                                    (if (= "Take 1 tag" target)
                                      (give-tags 1)
                                      (runner-pays [:credit 4]))
                                    card nil))}]})

(defcard "Galahad"
  (grail-ice end-the-run))

(defcard "Gatekeeper"
  (let [draw-ab {:async true
                 :prompt "Draw how many cards?"
                 :choices {:number (req 3)
                           :max (req 3)
                           :default (req 1)}
                 :msg (msg "draw " target " cards")
                 :effect (effect (draw eid target))}
        reveal-and-shuffle {:prompt "Reveal and shuffle up to 3 agendas"
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
                                   "shuffle "
                                   (string/join
                                     " and "
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
                                   " into R&D")}
        draw-reveal-shuffle {:async true
                             :label "Draw cards, reveal and shuffle agendas"
                             :effect (req (wait-for (resolve-ability state side draw-ab card nil)
                                                    (continue-ability state side reveal-and-shuffle card nil)))}]
    {:strength-bonus (req (if (= :this-turn (:rezzed card)) 6 0))
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
     :subroutines [(end-the-run-unless-runner-pays 3)
                   (end-the-run-unless-runner-pays 3)]}))

(defcard "Grim"
  {:on-rez take-bad-pub
   :subroutines [trash-program-sub]})

(defcard "Guard"
  {:constant-effects [{:type :bypass-ice
                       :req (req (same-card? card target))
                       :value false}]
   :subroutines [end-the-run]})

(defcard "Gutenberg"
  {:subroutines [(tag-trace 7)]
   :strength-bonus (req (if (= (second (get-zone card)) :rd) 3 0))})

(defcard "Gyri Labyrinth"
  {:subroutines [{:req (req run)
                  :label "Reduce Runner's hand size by 2"
                  :msg "reduce the Runner's maximum hand size by 2 until the start of the next Corp turn"
                  :effect (effect (register-floating-effect
                                    card
                                    {:type :hand-size
                                     :duration :until-corp-turn-begins
                                     :req (req (= :runner side))
                                     :value -2}))}]})

(defcard "Hadrian's Wall"
  (wall-ice [end-the-run end-the-run]))


(defcard "Hákarl 1.0"
  {:runner-abilities [(bioroid-break 1 1)]
   :subroutines [(do-brain-damage 1)
                 end-the-run]   
   :on-rez {:req (req (and run this-server
                           (->> (get-all-installed state) (remove #(same-card? card %)) (filter rezzed?) (count) (pos?))))
            :prompt "Derez another card to prevent the runner using printed abilities on bioroid ice this turn?"
            :choices {:req (req (and (installed? target)
                                     (rezzed? target)
                                     (not (same-card? card target))))}
            :waiting-prompt "Corp to resolve Hákarl 1.0"
            :effect (effect (derez target)
                            (system-msg (str "prevents the runner from using printed abilities on bioroid ice for the rest of the turn"))
                            (register-floating-effect
                             card
                             {:type :prevent-paid-ability
                              :duration :end-of-turn
                              :req (req (let [target-card (first targets)
                                              ability (second targets)]
                                          (and (ice? target-card)
                                               (has-subtype? target-card "Bioroid"))))
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
   :strength-bonus (req (- (count (filter #(has-subtype? % "Icebreaker")
                                          (all-active-installed state :runner)))))})

(defcard "Hailstorm"
  {:subroutines [{:label  "Remove a card in the Heap from the game"
                  :req     (req (not (zone-locked? state :runner :discard)))
                  :prompt  "Choose a card in the Runner's Heap"
                  :choices (req (cancellable (:discard runner) :sorted))
                  :msg     (msg "remove " (:title target) " from the game")
                  :effect  (effect (move :runner target :rfg))}
                 end-the-run]})

(defcard "Harvester"
  (let [sub {:label "Runner draws 3 cards and discards down to maximum hand size"
             :msg "make the Runner draw 3 cards and discard down to their maximum hand size"
             :async true
             :effect (req (wait-for (draw state :runner 3)
                                    (continue-ability
                                      state :runner
                                      (let [delta (- (count (get-in @state [:runner :hand])) (hand-size state :runner))]
                                        (when (pos? delta)
                                          {:prompt (msg "Choose " delta " cards to discard")
                                           :player :runner
                                           :choices {:max delta
                                                     :card #(in-hand? %)}
                                           :async true
                                           :effect (req (wait-for (trash-cards state :runner targets)
                                                                  (system-msg
                                                                    state :runner
                                                                    (str "trashes " (string/join ", " (map :title targets))))
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
                 {:msg "do 1 brain damage and end the run"
                  :effect (req (wait-for (damage state side :brain 1 {:card card})
                                         (end-run state side eid card)))}
                 end-the-run]
   :runner-abilities [(bioroid-break 2 2)]})

(defcard "Herald"
  {:flags {:rd-reveal (req true)}
   :subroutines [(gain-credits-sub 2)
                 {:async true
                  :label "Pay up to 2 [Credits] to place up to 2 advancement tokens"
                  :prompt "How many advancement tokens?"
                  :choices (req (map str (range (inc (min 2 (:credit corp))))))
                  :effect (req (let [c (str->int target)]
                                 (if (can-pay? state side (assoc eid :source card :source-type :subroutine) card (:title card) :credit c)
                                   (let [new-eid (make-eid state {:source card :source-type :subroutine})]
                                     (wait-for (pay state :corp new-eid card :credit c)
                                               (system-msg state :corp (:msg async-result))
                                               (continue-ability
                                                 state side
                                                 {:msg (msg "pay " c " [Credits] and place " (quantify c "advancement token")
                                                            " on " (card-str state target))
                                                  :choices {:card can-be-advanced?}
                                                  :effect (effect (add-prop target :advance-counter c {:placed true}))}
                                                 card nil)))
                                   (effect-completed state side eid))))}]
   :access {:async true
            :req (req (not (in-discard? card)))
            :msg "force the Runner to encounter Herald"
            :effect (req (force-ice-encounter state side eid card))}})

(defcard "Himitsu-Bako"
  {:abilities [{:msg "add itself to HQ"
                :cost [:credit 1]
                :effect (effect (move card :hand))}]
   :subroutines [end-the-run]})

(defcard "Hive"
  (let [corp-points (fn [corp] (min 5 (max 0 (- 5 (:agenda-point corp 0)))))
        ability {:silent (req true)
                 :effect (effect (reset-printed-subs card (corp-points corp) end-the-run))}]
    {:events [(assoc ability
                     :event :rez
                     :req (req (same-card? card (:card context))))
              (assoc ability :event :agenda-scored)
              (assoc ability
                     :event :card-moved
                     :req (req (= :corp (:scored-side target))))]
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
    (let [breakable-fn (req (when (or (> 3 (get-counters card :advancement))
                                      (not (has-subtype? target "AI")))
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
                                                       (str "uses Hortum to add 2 cards to HQ from R&D, "
                                                            "shuffle R&D, and end the run"))
                                           (end-run state side eid card)))
                                     (do (system-msg state side (str "uses Hortum to end the run"))
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
     :prompt "Install a piece of Bioroid ice from HQ or Archives?"
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
                 (trace-ability 1 {:label "Give the Runner 1 tag and do 1 brain damage"
                                   :msg "give the Runner 1 tag and do 1 brain damage"
                                   :async true
                                   :effect (req (wait-for (damage state :runner :brain 1 {:card card})
                                                          (gain-tags state :corp eid 1)))})]
   :runner-abilities [(bioroid-break 1 1)]})

(defcard "Ichi 2.0"
  {:subroutines [trash-program-sub trash-program-sub
                 (trace-ability 3 {:label "Give the Runner 1 tag and do 1 brain damage"
                                   :msg "give the Runner 1 tag and do 1 brain damage"
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
                :effect (effect (register-floating-effect
                                 card
                                 (let [encountered-ice (:ice context)]
                                   {:type :cannot-break-subs-on-ice
                                    :duration :end-of-encounter
                                    :req (req (same-card? encountered-ice target))
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
   :events [{:event :tags-changed
             :effect (effect (reset-variable-subs card (count-tags state) runner-trash-installed-sub))}]})

(defcard "Interrupt 0"
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

(defcard "IP Block"
  {:on-encounter (assoc (give-tags 1)
                        :req (req (seq (filter #(has-subtype? % "AI") (all-active-installed state :runner))))
                        :msg "give the runner 1 tag because there is an installed AI")
   :subroutines [(tag-trace 3)
                 end-the-run-if-tagged]})

(defcard "IQ"
  {:subroutines [end-the-run]
   :strength-bonus (req (count (:hand corp)))
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
  {:expose {:msg "do 2 net damage"
            :async true
            :effect (effect (damage eid :net 2 {:card card}))}
   :subroutines [(assoc runner-trash-installed-sub
                        :effect (req (wait-for (trash state side target {:cause :subroutine})
                                               (trash state side eid card {:cause :subroutine}))))]})

(defcard "Janus 1.0"
  {:subroutines [(do-brain-damage 1)
                 (do-brain-damage 1)
                 (do-brain-damage 1)
                 (do-brain-damage 1)]
   :runner-abilities [(bioroid-break 1 1)]})

(defcard "Jua"
  {:on-encounter {:msg "prevent the Runner from installing cards for the rest of the turn"
                  :effect (effect (register-turn-flag! card :runner-lock-install (constantly true)))}
   :subroutines [{:label "Choose 2 installed Runner cards, if able. The Runner must add 1 of those to the top of the Stack."
                  :req (req (>= (count (all-installed state :runner)) 2))
                  :async true
                  :prompt "Choose 2 installed Runner cards"
                  :choices {:card #(and (runner? %)
                                        (installed? %))
                            :max 2
                            :all true}
                  :msg (msg "add either " (card-str state (first targets))
                            " or " (card-str state (second targets))
                            " to the Stack")
                  :effect (effect (continue-ability
                                    (when (= 2 (count targets))
                                      {:player :runner
                                       :waiting-prompt "Runner to make a decision"
                                       :prompt "Choose a card to move to the Stack"
                                       :choices {:card #(some (partial same-card? %) targets)}
                                       :effect (req (move state :runner target :deck {:front true})
                                                    (system-msg state :runner (str "selected " (card-str state target) " to move to the Stack")))})
                                    card nil))}]})

(defcard "Kakugo"
  {:events [{:event :pass-ice
             :async true
             :req (req (same-card? (:ice context) card))
             :msg "do 1 net damage"
             :effect (effect (damage eid :net 1 {:card card}))}]
   :subroutines [end-the-run]})

(defcard "Kamali 1.0"
  (letfn [(better-name [kind] (if (= "hardware" kind) "piece of hardware" kind))
          (runner-trash [kind]
            {:prompt (str "Choose an installed " (better-name kind) " to trash")
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
             :waiting-prompt "Runner to choose an option"
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
             :effect (req (wait-for (resolve-ability state side (sub-map kind) card nil)
                                    (clear-wait-prompt state :corp)))})]
    {:subroutines [(brain-trash "resource")
                   (brain-trash "hardware")
                   (brain-trash "program")]
     :runner-abilities [(bioroid-break 1 1)]}))

(defcard "Karunā"
  {:subroutines [{:label "Do 2 net damage. The Runner may jack out."
                  :async true
                  :effect (req (wait-for (resolve-ability state side
                                                          (do-net-damage 2)
                                                          card nil)
                                         (continue-ability state side (offer-jack-out) card nil)))}
                 (do-net-damage 2)]})

(defcard "Kitsune"
  {:subroutines [{:optional
                  {:req (req (pos? (count (:hand corp))))
                   :prompt "Force the Runner to access a card in HQ?"
                   :yes-ability
                   {:async true
                    :prompt "Choose a card in HQ to force access"
                    :choices {:card (every-pred in-hand? corp?)
                              :all true}
                    :label "Force the Runner to breach HQ and access a card"
                    :msg (msg "force the Runner to breach HQ and access " (:title target))
                    :effect (req (wait-for (breach-server state :runner [:hq] {:no-root true
                                                                               :access-first target})
                                           (trash state side eid card {:cause :subroutine})))}}}]})

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
                  :msg (msg "force the Runner to trash " (:title target))
                  :prompt "Choose a piece of hardware to trash"
                  :choices {:card #(and (installed? %)
                                        (hardware? %))}
                  :effect (req (wait-for (trash state side target {:cause :subroutine})
                                         (when current-ice
                                           (continue state :corp nil)
                                           (continue state :runner nil))
                                         (trash state side eid card {:cause :subroutine})))}]})

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

(defcard "Loki"
  {:on-encounter
   {:req (req (<= 2 (count (filter ice? (all-active-installed state :corp)))))
    :choices {:card #(and (ice? %)
                          (active? %))
              :not-self true}
    :effect (req (let [target-subtypes (:subtype target)]
                   (register-floating-effect
                     state :corp card
                     {:type :gain-subtype
                      :duration :end-of-run
                      :req (req (same-card? card target))
                      :value (:subtypes target)})
                   (system-msg state :corp (str "chooses " (card-str state target) " for Loki's ability"))
                   (doseq [sub (:subroutines target)]
                     (add-sub! state side (get-card state card) sub (:cid target) {:front true}))
                   (register-events
                     state side card
                     (let [cid (:cid target)]
                       [{:event :run-ends
                         :unregister-once-resolved true
                         :req (req (get-card state card))
                         :effect (effect (remove-subs! (get-card state card) #(= cid (:from-cid %))))}]))))}
   :subroutines [{:label "End the run unless the Runner shuffles their Grip into the Stack"
                  :async true
                  :effect (req (if (and (zero? (count (:hand runner)))
                                        (< (count (:deck runner)) 2)) ; UFAQ 24
                                 (do (system-msg state :corp (str "uses Loki to end the run"))
                                     (end-run state side eid card))
                                 (continue-ability
                                   state :runner
                                   {:optional
                                    {:waiting-prompt "Runner to choose an option"
                                     :prompt "Reshuffle your Grip into the Stack?"
                                     :player :runner
                                     :yes-ability
                                     {:effect (req (doseq [c (:hand runner)]
                                                     (move state :runner c :deck))
                                                   (shuffle! state :runner :deck)
                                                   (system-msg state :runner "shuffles their Grip into their Stack"))}
                                     :no-ability
                                     {:async true
                                      :effect (effect (system-msg :runner "declines to shuffle their Grip into their Stack. Loki ends the run")
                                                      (end-run eid card))}}}
                                   card nil)))}]})

(defcard "Loot Box"
  (letfn [(top-3 [state] (take 3 (get-in @state [:runner :deck])))
          (top-3-names [state] (map :title (top-3 state)))]
    {:subroutines [(end-the-run-unless-runner-pays 2)
                   {:label "Reveal the top 3 cards of the Stack"
                    :async true
                    :effect (req (system-msg state side (str "uses Loot Box to reveal the top 3 cards of the stack: "
                                                             (string/join ", " (top-3-names state))))
                              (wait-for
                                (reveal state side (top-3 state))
                                (continue-ability
                                  state side
                                  {:waiting-prompt "Corp to make a decision"
                                   :prompt "Choose a card to add to the Grip"
                                   :choices (req (top-3 state))
                                   :msg (msg "add " (:title target) " to the Grip, gain " (:cost target)
                                             " [Credits] and shuffle the Stack. Loot Box is trashed")
                                   :async true
                                   :effect (req (move state :runner target :hand)
                                                (wait-for (gain-credits state :corp (:cost target))
                                                          (shuffle! state :runner :deck)
                                                          (trash state side eid card {:cause :subroutine})))}
                                  card nil)))}]}))

(defcard "Lotus Field"
  {:subroutines [end-the-run]
   :flags {:cannot-lower-strength true}})

(defcard "Lycan"
  (morph-ice "Sentry" "Code Gate" trash-program-sub))

(defcard "Machicolation A"
  {:subroutines [trash-program-sub
                 trash-program-sub
                 trash-hardware-sub
                 {:label "Runner loses 3 [credit], if able. End the run."
                  :msg "make the Runner lose 3 [credit] and end the run"
                  :async true
                  :effect (req (if (>= (:credit runner) 3)
                                 (wait-for (lose-credits state (make-eid state eid) :runner 3)
                                           (end-run state :corp eid card))
                                 (end-run state :corp eid card)))}]})

(defcard "Machicolation B"
  {:subroutines [trash-resource-sub
                 trash-resource-sub
                 (do-net-damage 1)
                 {:label "Runner loses 1[click], if able. End the run."
                  :msg "make the Runner lose 1[click] and end the run"
                  :async true
                  :effect (req (lose-clicks state :runner 1)
                               (end-run state :corp eid card))}]})

(defcard "Macrophage"
  {:subroutines [(trace-ability 4 {:label  "Purge virus counters"
                                   :msg    "purge virus counters"
                                   :effect (effect (purge))})
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
  (letfn [(disable-hosted [state side c]
            (doseq [hc (:hosted (get-card state c))]
              (unregister-events state side hc)
              (update! state side (dissoc hc :abilities))))]
    {:on-rez {:async true
              :effect (req (let [magnet card]
                             (wait-for (resolve-ability
                                         state side
                                         {:req (req (some #(some program? (:hosted %))
                                                          (remove-once #(same-card? % magnet)
                                                                       (filter ice? (all-installed state corp)))))
                                          :prompt "Choose a Program to host on Magnet"
                                          :msg (msg "host " (card-str state target))
                                          :choices {:card #(and (program? %)
                                                                (ice? (:host %))
                                                                (not (same-card? (:host %) magnet)))}
                                          :effect (effect (host card target))}
                                         card nil)
                                       (disable-hosted state side card)
                                       (effect-completed state side eid))))}
     :derez-effect {:req (req (not-empty (:hosted card)))
                    :effect (req (doseq [c (get-in card [:hosted])]
                                   (card-init state side c {:resolve-effect false})))}
     :events [{:event :runner-install
               :req (req (same-card? card (:host (:card context))))
               :effect (req (disable-hosted state side card)
                         (update-ice-strength state side card))}]
     :subroutines [end-the-run]}))

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

(defcard "Masvingo"
  (let [subs-effect (effect (reset-variable-subs card (get-counters card :advancement) end-the-run))
        ability {:req (req (same-card? card target))
                 :effect subs-effect}]
    {:advanceable :always
     :on-rez {:effect (effect (add-prop card :advance-counter 1 {:placed true}))}
     :events [(assoc ability :event :advance)
              (assoc ability :event :advancement-placed)
              {:event :rez
               :req (req (same-card? card (:card context)))
               :effect subs-effect}]}))

(defcard "Matrix Analyzer"
  {:on-encounter {:cost [:credit 1]
                  :choices {:card can-be-advanced?}
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
                  :waiting-prompt "Runner to choose an option"
                  :prompt "Choose one"
                  :choices ["End the run" "Add Meridian to score area"]
                  :player :runner
                  :async true
                  :effect (req (if (= target "End the run")
                                 (do (system-msg state :corp "uses Meridian to gain 4 [Credits] and end the run")
                                     (wait-for (gain-credits state :corp 4)
                                               (end-run state :runner eid card)))
                                 (do (system-msg state :runner "adds Meridian to their score area as an agenda worth -1 agenda points")
                                     (as-agenda state :runner card -1)
                                     (when current-ice
                                       (continue state :corp nil)
                                       (continue state :runner nil))
                                     (effect-completed state side eid))))}]})

(defcard "Merlin"
  (grail-ice (do-net-damage 2)))

(defcard "Meru Mati"
  {:subroutines [end-the-run]
   :strength-bonus (req (if (= (second (get-zone card)) :hq) 3 0))})

(defcard "Metamorph"
  {:subroutines [{:label "Swap two pieces of ice or swap two installed non-ice"
                  :msg "swap two pieces of ice or swap two installed non-ice"
                  :async true
                  :prompt "Choose one"
                  :req (req (or (<= 2 (count (filter ice? (all-installed state :corp))))
                                (<= 2 (count (remove ice? (all-installed state :corp))))))
                  :choices (req [(when (<= 2 (count (filter ice? (all-installed state :corp))))
                                   "Swap two pieces of ice")
                                 (when (<= 2 (count (remove ice? (all-installed state :corp))))
                                   "Swap two non-ice")])
                  :effect (effect
                            (continue-ability
                              (if (= target "Swap two pieces of ice")
                                {:prompt "Choose the two pieces of ice to swap"
                                 :choices {:card #(and (installed? %)
                                                       (ice? %))
                                           :not-self true
                                           :max 2
                                           :all true}
                                 :msg (msg "swap the positions of " (card-str state (first targets))
                                           " and " (card-str state (second targets)))
                                 :effect (req (apply swap-ice state side targets))}
                                {:prompt "Choose the two cards to swap"
                                 :choices {:card #(and (installed? %)
                                                       (not (ice? %)))
                                           :max 2
                                           :all true}
                                 :msg (msg "swap the positions of " (card-str state (first targets))
                                           " and " (card-str state (second targets)))
                                 :effect (req (apply swap-installed state side targets))})
                              card nil))}]})

(defcard "Mganga"
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

(defcard "Mind Game"
  {:subroutines [(do-psi {:label "Redirect the run to another server"
                          :async true
                          :player :corp
                          :prompt "Choose a server"
                          :waiting-prompt "Corp to choose a server"
                          :choices (req (remove #{(-> @state :run :server central->name)} servers))
                          :msg (msg "redirect the run to " target
                                    " and for the remainder of the run, the runner must add 1 installed card to the bottom of their stack as an additional cost to jack out")
                          :effect (req (let [can-redirect? (and (:run @state)
                                                                (= 1 (count (:encounters @state)))
                                                                (not= :success (:phase (:run @state))))]
                                         (when can-redirect?
                                           (redirect-run state side target :approach-ice))
                                         (register-floating-effect
                                           state side card
                                           {:type :jack-out-additional-cost
                                            :duration :end-of-run
                                            :value [:add-installed-to-bottom-of-deck 1]})
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
  (letfn [(net-or-trash [net-dmg mill-cnt]
            {:label (str "Do " net-dmg " net damage")
             :player :runner
             :waiting-prompt "Runner to choose an option"
             :prompt "Take net damage or trash cards from the stack?"
             :choices (req [(str "Take " net-dmg " net damage")
                            (when (<= mill-cnt (count (:deck runner)))
                              (str "Trash the top " mill-cnt " cards of the stack"))])
             :async true
             :effect (req (if (= target (str "Take " net-dmg " net damage"))
                            (do (system-msg state :corp
                                            (str "uses Mlinzi to do "
                                                 net-dmg " net damage"))
                                (damage state :runner eid :net net-dmg {:card card}))
                            (do (system-msg state :corp
                                            (str "uses Mlinzi to trash "
                                                 (string/join ", " (map :title (take mill-cnt (:deck runner))))
                                                 " from the runner's stack"))
                                (mill state :runner eid :runner mill-cnt))))})]
    {:subroutines [(net-or-trash 1 2)
                   (net-or-trash 2 3)
                   (net-or-trash 3 4)]}))

(defcard "Mother Goddess"
  (let [mg {:req (req (ice? target))
            :effect (effect (update-all-subtypes))}]
    {:constant-effects [{:type :gain-subtype
                         :req (req (same-card? card target))
                         :value (req (->> (vals (:servers corp))
                                          (mapcat :ices)
                                          (filter #(and (rezzed? %)
                                                        (not (same-card? card %))))
                                          (mapcat :subtypes)))}]
     :subroutines [end-the-run]
     :events [{:event :rez
               :req (req (ice? (:card context)))
               :effect (effect (update-all-subtypes))}
              (assoc mg :event :card-moved)
              (assoc mg :event :derez)
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
   :runner-abilities [(break-sub [:credit 2] 1)]})

(defcard "Nerine 2.0"
  (let [sub {:label "Do 1 brain damage and Corp may draw 1 card"
             :async true
             :msg "do 1 brain damage"
             :effect (req (wait-for (damage state :runner :brain 1 {:card card})
                                    (continue-ability
                                      state side
                                      {:optional
                                       {:prompt "Draw 1 card?"
                                        :yes-ability {:async true
                                                      :msg "draw 1 card"
                                                      :effect (effect (draw eid 1))}}}
                                      card nil)))}]
    {:subroutines [sub
                   sub]
     :runner-abilities [(bioroid-break 2 2)]}))

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
   :strength-bonus (req (next-ice-count corp))})

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
            (if (> cnt 0)
              (wait-for (resolve-ability state side trash-program-sub card nil)
                        (trash-programs (dec cnt) state side card eid))
              (effect-completed state side eid)))]
    {:subroutines [{:label "Do 1 net damage for each rezzed NEXT ice"
                    :msg (msg "do " (next-ice-count corp) " net damage")
                    :effect (effect (damage eid :net (next-ice-count corp) {:card card}))}
                   {:label "Trash 1 program for each rezzed NEXT ice"
                    :async true
                    :effect (req (trash-programs (min (count (filter program? (all-active-installed state :runner)))
                                                      (next-ice-count corp))
                                                 state side card eid))}]}))

(defcard "NEXT Opal"
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
    {:events [{:event :rez
               :req (req (and (ice? (:card context))
                              (has-subtype? (:card context) "NEXT")))
               :effect (effect (reset-variable-subs card (next-ice-count corp) sub))}
              {:event :derez
               :req (req (and (ice? target)
                              (has-subtype? target "NEXT")))
               :effect (effect (reset-variable-subs card (next-ice-count corp) sub))}]}))

(defcard "NEXT Sapphire"
  {:subroutines [{:label "Draw up to X cards"
                  :prompt "Draw how many cards?"
                  :msg (msg "draw " target " cards")
                  :choices {:number (req (next-ice-count corp))
                            :default (req 1)}
                  :async true
                  :effect (effect (draw eid target))}
                 {:label "Add up to X cards from Archives to HQ"
                  :prompt "Choose cards to add to HQ"
                  :show-discard  true
                  :choices {:card #(and (corp? %)
                                        (in-discard? %))
                            :max (req (next-ice-count corp))}
                  :effect (req (doseq [c targets]
                                 (move state side c :hand)))
                  :msg (msg "add "
                            (let [seen (filter :seen targets)
                                  m (count (filter #(not (:seen %)) targets))]
                              (str (string/join ", " (map :title seen))
                                   (when (pos? m)
                                     (str (when-not (empty? seen) " and ")
                                          (quantify m "unseen card")))))
                            " to HQ")}
                 {:label "Shuffle up to X cards from HQ into R&D"
                  :prompt "Choose cards to shuffle into R&D"
                  :choices {:card #(and (corp? %)
                                        (in-hand? %))
                            :max (req (next-ice-count corp))}
                  :effect (req (doseq [c targets]
                                 (move state :corp c :deck))
                               (shuffle! state :corp :deck))
                  :cancel-effect (effect (shuffle! :corp :deck))
                  :msg (msg "shuffle " (count targets) " cards from HQ into R&D")}]})

(defcard "NEXT Silver"
  {:events [{:event :rez
             :req (req (and (ice? (:card context))
                            (has-subtype? (:card context) "NEXT")))
             :effect (effect (reset-variable-subs card (next-ice-count corp) end-the-run))}
            {:event :derez
             :req (req (and (ice? target)
                            (has-subtype? target "NEXT")))
             :effect (effect (reset-variable-subs card (next-ice-count corp) end-the-run))}]})

(defcard "Nightdancer"
  (let [sub {:label (str "The Runner loses [Click], if able. "
                         "You have an additional [Click] to spend during your next turn.")
             :msg (str "force the runner to lose a [Click], if able. "
                       "Corp gains an additional [Click] to spend during their next turn")
             :effect (req (lose-clicks state :runner 1)
                          (swap! state update-in [:corp :extra-click-temp] (fnil inc 0)))}]
    {:subroutines [sub
                   sub]}))

(defcard "Oduduwa"
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

(defcard "Orion"
  (space-ice trash-program-sub
             (resolve-another-subroutine)
             end-the-run))

(defcard "Otoroshi"
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
                                 (continue-ability
                                   state side
                                   {:player :runner
                                    :async true
                                    :waiting-prompt "Runner to choose an option"
                                    :prompt (str "Access " title " or pay 3 [Credits]?")
                                    :choices ["Access card"
                                              (when (>= (:credit runner) 3)
                                                "Pay 3 [Credits]")]
                                    :msg (msg "force the Runner to "
                                              (if (= target "Access card")
                                                (str "access " title)
                                                "pay 3 [Credits]"))
                                    :effect (req (if (= target "Access card")
                                                   (access-card state :runner eid c)
                                                   (wait-for (pay state :runner (make-eid state eid) card :credit 3)
                                                             (system-msg state :runner (:msg async-result))
                                                             (effect-completed state side eid))))}
                                   card nil)))}]})

(defcard "Owl"
  {:subroutines [{:choices {:card #(and (installed? %)
                                        (program? %))}
                  :label "Add installed program to the top of the Runner's Stack"
                  :msg "add an installed program to the top of the Runner's Stack"
                  :effect (effect (move :runner target :deck {:front true})
                                  (system-msg (str "adds " (:title target) " to the top of the Runner's Stack")))}]})

(defcard "Pachinko"
  {:subroutines [end-the-run-if-tagged
                 end-the-run-if-tagged]})

(defcard "Palisade"
  {:strength-bonus (req (if (protecting-a-central? card) 0 2))
   :subroutines [end-the-run]})

(defcard "Paper Wall"
  {:events [{:event :subroutines-broken
             :req (req (and (same-card? card target)
                            (empty? (remove :broken (:subroutines target)))))
             :async true
             :effect (effect (trash :corp eid card {:cause :effect}))}]
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
                                               (str "uses Peeping Tom to name " target ", then reveals "
                                                    (string/join ", " (map :title (:hand runner)))
                                                    " in the Runner's Grip. Peeping Tom gains " n " subroutines"))
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
   :strength-bonus (req (if (wonder-sub card 3) 5 0))})

(defcard "Ping"
  {:on-rez {:req (req (and run this-server))
            :msg "give the Runner 1 tag"
            :async true
            :effect (effect (gain-tags :corp eid 1))}
   :subroutines [end-the-run]})

(defcard "Pop-up Window"
  {:on-encounter (gain-credits-sub 1)
   :subroutines [(end-the-run-unless-runner-pays 1)]})

(defcard "Pup"
  (let [sub {:player :runner
             :async true
             :label (str "Do 1 net damage unless the Runner pays 1 [Credits]")
             :prompt (str "Suffer 1 net damage or pay 1 [Credits]?")
             :choices ["Suffer 1 net damage"
                       "Pay 1 [Credits]"]
             :effect (req (if (= "Suffer 1 net damage" target)
                            (continue-ability state :corp (do-net-damage 1) card nil)
                            (wait-for (pay state :runner (make-eid state eid) card [:credit 1])
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
   :strength-bonus (req (get-counters card :power))})

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
                  :effect (effect (register-floating-effect
                                  card
                                  {:type :ice-strength
                                   :duration :end-of-run
                                   :value 3})
                                  (update-all-ice))}]})

(defcard "Resistor"
  {:strength-bonus (req (count-tags state))
   :subroutines [(trace-ability 4 end-the-run)]})

(defcard "Rime"
  {:implementation "Can be rezzed anytime already"
   :on-rez {:effect (effect (update-all-ice))}
   :subroutines [{:label "Runner loses 1 [Credit]"
                  :msg "force the Runner to lose 1 [Credit]"
                  :async true
                  :effect (effect (lose-credits :runner eid 1))}]
   :constant-effects [{:type :ice-strength
                       :req (req (protecting-same-server? card target))
                       :value 1}]})

(defcard "Rototurret"
  {:subroutines [trash-program-sub
                 end-the-run]})

(defcard "Sadaka"
  (let [maybe-draw-effect
        {:optional
         {:player :corp
          :waiting-prompt "Corp to choose an option"
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
                                {:waiting-prompt "Corp to make a decision"
                                 :prompt (str "Top 3 cards of R&D: " (string/join ", " top-names))
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
                             {:waiting-prompt "Corp to make a decision"
                              :prompt "Choose a card in HQ to trash"
                              :choices (req (cancellable (:hand corp) :sorted))
                              :async true
                              :cancel-effect (effect (system-msg "declines to use Sadaka to trash a card from HQ")
                                                     (effect-completed eid))
                              :effect (req (wait-for
                                             (trash state :corp target {:cause :subroutine})
                                             (system-msg state :corp "trashes a card from HQ")
                                             (continue-ability state side trash-resource-sub card nil)))}
                             card nil)
                           (wait-for (trash state :corp (make-eid state eid) card nil)
                                     (encounter-ends state side eid))))}]}))

(defcard "Sagittarius"
  (constellation-ice trash-program-sub))

(defcard "Saisentan"
  (let [sub {:label "Do 1 net damage"
             :async true
             :msg "do 1 net damage"
             :effect (req (wait-for (damage state side :net 1 {:card card})
                                    (let [choice (get-in card [:special :saisentan])
                                          cards async-result
                                          dmg (some #(when (= (:type %) choice) %) cards)]
                                      (if dmg
                                        (do (system-msg state :corp "uses Saisentan to deal a second net damage")
                                            (damage state side eid :net 1 {:card card}))
                                        (effect-completed state side eid)))))}]
    {:on-encounter {:waiting-prompt "Corp to choose an option"
                    :prompt "Choose a card type"
                    :choices ["Event" "Hardware" "Program" "Resource"]
                    :msg (msg "choose the card type " target)
                    :effect (effect (update! (assoc-in card [:special :saisentan] target)))}
     :events [{:event :end-of-encounter
               :req (req (get-in card [:special :saisentan]))
               :effect (effect (update! (dissoc-in card [:special :saisentan])))}]
     :subroutines [sub
                   sub
                   sub]}))

(defcard "Salvage"
  (zero-to-hero (tag-trace 2)))

(defcard "Sand Storm"
  {:subroutines [{:async true
                  :label "Move Sand Storm and the run to another server"
                  :prompt "Choose another server and redirect the run to its outermost position"
                  :choices (req (remove #{(zone->name (:server (:run @state)))} (cancellable servers)))
                  :msg (msg "move Sand Storm and the run. The Runner is now running on " target ". Sand Storm is trashed")
                  :effect (req (let [moved-ice (move state side card (conj (server->zone state target) :ices))]
                                 (redirect-run state side target)
                                 (wait-for (trash state side (make-eid state eid) moved-ice {:unpreventable true :cause :subroutine})
                                           (encounter-ends state side eid))))}]})

(defcard "Sandman"
  {:subroutines [add-runner-card-to-grip
                 add-runner-card-to-grip]})

(defcard "Sandstone"
  {:subroutines [end-the-run]
   :strength-bonus (req (- (get-counters card :virus)))
   :on-encounter {:msg "place 1 virus counter on Sandstone"
                  :effect (effect (add-counter card :virus 1)
                                  (update-ice-strength (get-card state card)))}})

(defcard "Sapper"
  {:flags {:rd-reveal (req true)}
   :subroutines [trash-program-sub]
   :access {:async true
            :req (req (not (in-discard? card)))
            :msg "force the Runner to encounter Sapper"
            :effect (req (force-ice-encounter state side eid card))}})

(defcard "Searchlight"
  (let [sub {:label "Trace X - Give the Runner 1 tag"
             :trace {:base (req (get-counters card :advancement))
                     :label "Give the Runner 1 tag"
                     :successful (give-tags 1)}}]
    {:advanceable :always
     :subroutines [sub
                   sub]}))

(defcard "Seidr Adaptive Barrier"
  {:strength-bonus (req (count (:ices (card->server state card))))
   :subroutines [end-the-run]})

(defcard "Self-Adapting Code Wall"
  {:subroutines [end-the-run]
   :flags {:cannot-lower-strength true}})

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

(defcard "Shadow"
  (wall-ice [(gain-credits-sub 2)
             (tag-trace 3)]))

(defcard "Sherlock 1.0"
  (let [sub (trace-ability 4 {:choices {:card #(and (installed? %)
                                                    (program? %))}
                              :label "Add an installed program to the top of the Runner's Stack"
                              :msg (msg "add " (:title target) " to the top of the Runner's Stack")
                              :effect (effect (move :runner target :deck {:front true}))})]
    {:subroutines [sub
                   sub]
     :runner-abilities [(bioroid-break 1 1)]}))

(defcard "Sherlock 2.0"
  (let [sub (trace-ability 4 {:choices {:card #(and (installed? %)
                                                    (program? %))}
                              :label "Add an installed program to the bottom of the Runner's Stack"
                              :msg (msg "add " (:title target) " to the bottom of the Runner's Stack")
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
                  :waiting-prompt "Corp to make a decision"
                  :effect (effect (continue-ability
                                    (let [from (take 3 (:deck corp))]
                                      (when (pos? (count from))
                                        (reorder-choice :corp :runner from '() (count from) from)))
                                    card nil))}
                 {:optional
                  {:prompt "Pay 1 [Credits] to keep the Runner from breaching R&D?"
                   :yes-ability {:cost [:credit 1]
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
            (->> (get-effects state :corp card et)
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
                            (register-floating-effect
                              state side card
                              {:type effect-type
                               :duration :end-of-encounter
                               :value t3})
                            (system-msg state side
                                        (str "uses Slot Machine to put the top card of the stack to the bottom,"
                                             " then reveal the top 3 cards in the stack: "
                                             (string/join ", " (top-3-names t3))))
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
                                                  (= 3 (count (first (get-effects state :corp card et)))))
                                             (and (= unique-types 1)
                                                  (= 2 (count (first (get-effects state :corp card et))))))
                                     (do (system-msg state :corp (str "uses Slot Machine to gain 3 [Credits]"))
                                         (gain-credits state :corp eid 3))
                                     (effect-completed state side eid))))}
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

(defcard "Snoop"
  {:on-encounter {:msg (msg "reveal the Runner's Grip ("
                            (string/join ", " (map :title (:hand runner)))
                            ")")
                  :async true
                  :effect (effect (reveal eid (:hand runner)))}
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

(defcard "Snowflake"
  {:subroutines [(do-psi end-the-run)]})

(defcard "Special Offer"
  {:subroutines [{:label "Gain 5 [Credits] and trash Special Offer"
                  :msg "gains 5 [Credits] and trashes Special Offer"
                  :async true
                  :effect (req (wait-for (gain-credits state :corp 5)
                                         (when current-ice
                                           (continue state :corp nil)
                                           (continue state :runner nil))
                                         (trash state side eid card {:cause :subroutine})))}]})

(defcard "Spiderweb"
  {:subroutines [end-the-run
                 end-the-run
                 end-the-run]})

(defcard "Surveyor"
  (let [x (req (* 2 (count (:ices (card->server state card)))))]
    {:strength-bonus x
     :subroutines [{:label "Trace X - Give the Runner 2 tags"
                    :trace {:base x
                            :label "Give the Runner 2 tags"
                            :successful (give-tags 2)}}
                   {:label "Trace X - End the run"
                    :trace {:base x
                            :label "End the run"
                            :successful end-the-run}}]}))

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
             :prompt "Let Corp trash 1 program or pay 3 [Credits]?"
             :choices ["Corp trash"
                       "Pay 3 [Credits]"]
             :effect (req (if (= "Corp trash" target)
                            (continue-ability state :corp trash-program-sub card nil)
                            (wait-for (pay state :runner (make-eid state eid) card [:credit 3])
                                      (system-msg state :runner (:msg async-result))
                                      (effect-completed state side eid))))}
        ability {:req (req (same-card? card target))
                 :effect (effect (reset-variable-subs card (get-counters card :advancement) sub))}]
    {:advanceable :always
     :on-rez take-bad-pub
     :events [(assoc ability :event :advance)
              (assoc ability :event :advancement-placed)
              {:event :rez
               :req (req (same-card? card (:card context)))
               :effect (effect (reset-variable-subs card (get-counters card :advancement) sub))}]}))

(defcard "Swordsman"
  (let [breakable-fn (req (when-not (has-subtype? target "AI") :unrestricted))]
    {:subroutines [{:async true
                    :breakable breakable-fn
                    :prompt "Choose an AI program to trash"
                    :msg (msg "trash " (:title target))
                    :label "Trash an AI program"
                    :choices {:card #(and (installed? %)
                                          (program? %)
                                          (has-subtype? % "AI"))}
                    :effect (effect (trash eid target {:cause :subroutine}))}
                   (assoc (do-net-damage 1) :breakable breakable-fn)]}))

(defcard "SYNC BRE"
  {:subroutines [(tag-trace 4)
                 (trace-ability 2 {:label "Runner reduces cards accessed by 1 for this run"
                                   :async true
                                   :msg "reduce cards accessed for this run by 1"
                                   :effect (effect (access-bonus :total -1))})]})

(defcard "Tapestry"
  {:subroutines [runner-loses-click
                 {:async true
                  :msg "draw 1 card"
                  :effect (effect (draw eid 1))}
                 {:req (req (pos? (count (:hand corp))))
                  :prompt "Choose a card in HQ to move to the top of R&D"
                  :choices {:card #(and (in-hand? %)
                                        (corp? %))}
                  :msg "add 1 card in HQ to the top of R&D"
                  :effect (effect (move target :deck {:front true}))}]})

(defcard "Taurus"
  (constellation-ice trash-hardware-sub))

(defcard "Thimblerig"
  (let [ability {:optional
                 {:req (req (and (<= 2 (count (filter ice? (all-installed state :corp))))
                                 (if run (same-card? (:ice context) card) true)))
                  :prompt "Swap Thimblerig with another ice?"
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
  {:alternative-cost [:forfeit]
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
                                             (str "uses Tithonium to "
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
                  :effect (req (wait-for (pay state :runner (make-eid state eid) card [:credit 3])
                                         (if (:cost-paid async-result)
                                           (do (system-msg state :runner (str (:msg async-result) " when encountering Tollbooth"))
                                               (effect-completed state side eid))
                                           (do (system-msg state :runner "ends the run, as the Runner can't pay 3 [Credits] when encountering Tollbooth")
                                               (end-run state :corp eid card)))))}
   :subroutines [end-the-run]})

(defcard "Tour Guide"
  (let [ef (effect (reset-variable-subs card (count (filter asset? (all-active-installed state :corp))) end-the-run))
        ability {:label "Reset number of subs"
                 :silent (req true)
                 :req (req (asset? target))
                 :effect ef}
        trash-req (req (and (asset? (:card target))
                            (installed? (:card target))
                            (rezzed? (:card target))))]
    {:on-rez {:effect ef}
     :events [{:event :rez
               :label "Reset number of subs"
               :silent (req true)
               :req (req (asset? (:card context)))
               :effect ef}
              (assoc ability :event :derez)
              (assoc ability :event :game-trash :req trash-req)
              (assoc ability :event :corp-trash :req trash-req)
              (assoc ability :event :runner-trash :req trash-req)]}))

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

(defcard "Tribunal"
  {:subroutines [runner-trash-installed-sub
                 runner-trash-installed-sub
                 runner-trash-installed-sub]})

(defcard "Troll"
  {:on-encounter
   (trace-ability 2 {:msg "force the Runner to lose [Click] or end the run"
                     :player :runner
                     :prompt "Choose one"
                     :choices ["Lose [Click]" "End the run"]
                     :async true
                     :effect (req (if (and (= target "Lose [Click]")
                                           (can-pay? state :runner eid card nil [:click 1]))
                                    (do (system-msg state :runner "loses [Click]")
                                        (lose-clicks state :runner 1)
                                        (effect-completed state :runner eid))
                                    (do (system-msg state :corp "ends the run")
                                        (end-run state :corp eid card))))})})

(defcard "Tsurugi"
  {:subroutines [(end-the-run-unless-corp-pays 1)
                 (do-net-damage 1)
                 (do-net-damage 1)
                 (do-net-damage 1)]})

(defcard "Turing"
  (let [breakable-fn (req (when-not (has-subtype? target "AI") :unrestricted))]
    {:subroutines [(assoc (end-the-run-unless-runner
                            "spends [Click][Click][Click]"
                            "spend [Click][Click][Click]"
                            (runner-pays [:click 3]))
                          :breakable breakable-fn)]
     :strength-bonus (req (if (is-remote? (second (get-zone card))) 3 0))}))

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
                  :msg (msg "force the Runner to lose " (if tagged "all credits" "1 [Credits]"))
                  :async true
                  :effect (req (if tagged
                                 (wait-for (lose-credits state :runner (make-eid state eid) :all)
                                           (when current-ice
                                             (continue state :corp nil)
                                             (continue state :runner nil))
                                           (trash state side eid card {:cause :subroutine}))
                                 (lose-credits state :runner eid 1)))}]})

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

(defcard "Vanilla"
  {:subroutines [end-the-run]})

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

(defcard "Waiver"
  {:subroutines [(trace-ability
                   5 {:label "Reveal the grip and trash cards"
                      :msg (msg "reveal all cards in the grip: " (string/join ", " (map :title (:hand runner))))
                      :async true
                      :effect (req (wait-for
                                     (reveal state side (:hand runner))
                                     (let [delta (- target (second targets))
                                           cards (filter #(<= (:cost %) delta) (:hand runner))]
                                       (system-msg state side (str "uses Waiver to trash "
                                                                   (string/join ", " (map :title cards))))
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

(defcard "Weir"
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

(defcard "Wendigo"
  (implementation-note
    "Program prevention is not implemented"
    (morph-ice "Code Gate" "Barrier"
               {:msg "prevent the Runner from using a chosen program for the remainder of this run"})))

(defcard "Whirlpool"
  {:subroutines [{:label "The Runner cannot jack out for the remainder of this run"
                  :msg "prevent the Runner from jacking out"
                  :async true
                  :effect (req (prevent-jack-out state side)
                               (when current-ice
                                 (continue state :corp nil)
                                 (continue state :runner nil))
                               (trash state side eid card {:cause :subroutine}))}]})

(defcard "Whitespace"
  {:subroutines [(runner-loses-credits 3)
                 {:label "End the run if the Runner has 6 [Credits] or less"
                  :req (req (< (:credit runner) 7))
                  :msg "end the run"
                  :async true
                  :effect (effect (end-run :corp eid card))}]})

(defcard "Winchester"
  (let [ab {:req (req (protecting-hq? card))
            :effect (req (reset-variable-subs state :corp card 1 (trace-ability 3 end-the-run) {:back true}))}]
    {:subroutines [(trace-ability 4 trash-program-sub)
                   (trace-ability 3 trash-hardware-sub)]
     :on-rez {:effect (effect (continue-ability ab card nil))}
     :events [(assoc ab :event :rez)
              (assoc ab :event :card-moved)
              (assoc ab :event :approach-ice)
              (assoc ab :event :swap
                     :req (req (or (protecting-hq? target)
                                   (protecting-hq? (second targets)))))]}))

(defcard "Woodcutter"
  (zero-to-hero (do-net-damage 1)))

(defcard "Wormhole"
  (space-ice (resolve-another-subroutine)))

(defcard "Wotan"
  {:subroutines [(end-the-run-unless-runner
                   "spends [Click][Click]"
                   "spend [Click][Click]"
                   (runner-pays [:click 2]))
                 (end-the-run-unless-runner-pays 3)
                 (end-the-run-unless-runner
                   "trashes an installed program"
                   "trash an installed program"
                   trash-program-sub)
                 (end-the-run-unless-runner
                   "takes 1 brain damage"
                   "take 1 brain damage"
                   (do-brain-damage 1))]})

(defcard "Wraparound"
  {:subroutines [end-the-run]
   :strength-bonus (req (if (some #(has-subtype? % "Fracter") (all-active-installed state :runner))
                          0 7))})

(defcard "Yagura"
  {:subroutines [{:msg "look at the top card of R&D"
                  :optional {:prompt (msg "Move " (:title (first (:deck corp))) " to the bottom of R&D?")
                             :yes-ability {:msg "move the top card of R&D to the bottom"
                                           :effect (effect (move (first (:deck corp)) :deck))}
                             :no-ability {:effect (effect (system-msg :corp (str "does not use Yagura to move the top card of R&D to the bottom")))}}}
                 (do-net-damage 1)]})

(defcard "Zed 1.0"
  {:implementation "Restriction on having spent [click] is not implemented"
   :subroutines [(do-brain-damage 1)
                 (do-brain-damage 1)]
   :runner-abilities [(bioroid-break 1 1)]})

(defcard "Zed 2.0"
  {:implementation "Restriction on having spent [click] is not implemented"
   :subroutines [trash-hardware-sub
                 trash-hardware-sub
                 (do-brain-damage 2)]
   :runner-abilities [(bioroid-break 2 2)]})
