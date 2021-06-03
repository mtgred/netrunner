(ns game.cards.hardware
  (:require [game.core :refer :all]
            [game.utils :refer :all]
            [game.core.cost-fns :refer [all-stealth min-stealth]]
            [jinteki.utils :refer :all]
            [clojure.string :as string]
            [clojure.set :as clj-set]))

;; Card definitions

(defcard "Acacia"
  {:events [{:event :pre-purge
             :effect (req (let [counters (number-of-virus-counters state)]
                            (update! state side (assoc-in (get-card state card) [:special :numpurged] counters))))}
            {:event :purge
             :optional
             {:player :runner
              :waiting-prompt "Runner to choose an option"
              :prompt "Use Acacia?"
              :yes-ability
              {:async true
               :effect (req (let [counters (- (get-in (get-card state card) [:special :numpurged])
                                              (number-of-virus-counters state))]
                              (wait-for (trash state side card nil)
                                        (system-msg state side (str "trashes Acacia and gains " counters " [Credit]"))
                                        (gain-credits state side eid counters))))}}}]})

(defcard "Adjusted Matrix"
  {:implementation "Click Adjusted Matrix to use ability."
   :on-install {:req (req (not-empty (filter #(has-subtype? % "Icebreaker") (all-active-installed state :runner))))
                :prompt "Choose Icebreaker on which to install Adjusted Matrix"
                :choices {:card #(and (runner? %)
                                      (has-subtype? % "Icebreaker")
                                      (installed? %))}
                :msg (msg "host it on " (card-str state target))
                :effect (effect (host (get-card state target) (get-card state card)))}
   :constant-effects [{:type :gain-subtype
                       :req (req (same-card? target (:host card)))
                       :value "AI"}]
   :abilities [(break-sub [:click 1] 1 "All" {:req (req true)})]})

(defcard "Akamatsu Mem Chip"
  {:constant-effects [(mu+ 1)]})

(defcard "Aniccam"
  (let [ability {:async true
                 :once-per-instance true
                 :req (req (and (some #(event? (:card %)) targets)
                                (letfn [(event-targets? [targets]
                                          (some #(event? (:card %)) targets))]
                                  (first-trash? state event-targets?))))
                 :msg "draw 1 card"
                 :effect (effect (draw :runner eid 1 nil))}]
    {:constant-effects [(mu+ 1)]
     :events [(assoc ability :event :corp-trash)
              (assoc ability :event :runner-trash)
              (assoc ability :event :game-trash)]}))

(defcard "Archives Interface"
  {:events
   [{:event :pre-access
     :async true
     :interactive (req true)
     :req (req (and (= target :archives)
                    (not= (:max-access run) 0)
                    (not-empty (:discard corp))))
     :effect (req (swap! state update-in [:corp :discard] #(map (fn [c] (assoc c :seen true)) %))
                  (continue-ability
                    state side
                    {:optional
                     {:prompt "Use Archives Interface to remove a card from the game instead of accessing it?"
                      :yes-ability {:prompt "Choose a card in Archives to remove from the game instead of accessing"
                                    :choices (req (:discard corp))
                                    :msg (msg "remove " (:title target) " from the game")
                                    :effect (effect (move :corp target :rfg))}}} card nil))}]})

(defcard "Astrolabe"
  {:constant-effects [(mu+ 1)]
   :events [{:event :server-created
             :msg "draw 1 card"
             :async true
             :effect (effect (draw :runner eid 1 nil))}]})

(defcard "Autoscripter"
  {:events [{:event :runner-install
             :silent (req true)
             :req (req (and (program? (:card context))
                            ;; only trigger on Runner's turn
                            (= (:active-player @state) :runner)
                            ;; only trigger when playing a Program from grip
                            (some #{:hand} (:previous-zone (:card context)))
                            ;; check that we haven't played a Program from the grip this turn
                            ;; which translates to just one case of playing a Program in turn-events
                            (first-event? state :runner :runner-install
                                          (fn [[context]]
                                            (and (some #{:hand} (:previous-zone (:card context)))
                                                 (program? (:card context)))))))
             :msg "gain [Click]"
             :effect (effect (gain :click 1))}
            {:event :unsuccessful-run
             :async true
             :effect (effect (system-msg "trashes Autoscripter")
                             (trash eid card))}]})

(defcard "Blackguard"
  {:constant-effects [(mu+ 2)]
   :events [{:event :expose
             :msg (msg "attempt to force the rez of " (:title target))
             :async true
             :effect (req (let [c target
                                cdef (card-def c)
                                cname (:title c)
                                cost (rez-cost state side target)
                                additional-costs (rez-additional-cost-bonus state side target)]
                            (if (seq additional-costs)
                              (continue-ability
                                state side
                                {:optional
                                 {:waiting-prompt (str "Corp to decide if they will rez " cname)
                                  :prompt (msg (build-cost-string [:credit cost])
                                               ", plus " (string/lower-case (build-cost-string additional-costs))
                                               " as an additional cost to rez " cname "?")
                                  :player :corp
                                  :yes-ability {:async true
                                                :effect (effect (rez :corp eid c))}
                                  :no-ability {:msg (msg "declines to pay additional costs"
                                                         " and is not forced to rez " cname)}}}
                                card nil)
                              (rez state :corp eid target))))}]})

(defcard "Bookmark"
  {:abilities [{:label "Host up to 3 cards from your Grip facedown"
                :cost [:click 1]
                :keep-open :while-clicks-left
                :msg "host up to 3 cards from their Grip facedown"
                :choices {:max 3
                          :card #(and (runner? %)
                                      (in-hand? %))}
                :effect (req (doseq [c targets]
                               (host state side (get-card state card) c {:facedown true})))}
               {:label "Add all hosted cards to Grip"
                :cost [:click 1]
                :msg "add all hosted cards to their Grip"
                :effect (req (doseq [c (:hosted card)]
                               (move state side c :hand)))}
               {:label "Add all hosted cards to Grip"
                :trash-icon true
                :effect (req (doseq [c (:hosted card)]
                               (move state side c :hand))
                             (continue-ability
                               state side
                               {:cost [:trash]
                                :msg "add all hosted cards to their Grip"}
                               (get-card state card) nil))}]})

(defcard "Boomerang"
  {:on-install {:prompt "Choose an installed piece of ice"
                :msg (msg "target " (card-str state target))
                :choices {:card #(and (installed? %)
                                      (ice? %))}
                :effect (effect (add-icon card target "B" "blue")
                                (update! (assoc-in (get-card state card) [:special :boomerang-target] target)))}
   :leave-play (effect (remove-icon card))
   :abilities [(assoc
                 (break-sub
                   [:trash] 2 "All"
                   {:req (req (if-let [boomerang-target (get-in card [:special :boomerang-target])]
                                (same-card? current-ice boomerang-target)
                                true))}) ; When eg. flipped by Assimilator
                 :effect
                 (req (wait-for
                        (trash state side (make-eid state eid) card {:cause :ability-cost
                                                                     :unpreventable true})
                        (continue-ability
                          state :runner
                          (when-let [[boomerang] async-result]
                            (break-sub
                              nil 2 "All"
                              {:additional-ability
                               {:effect
                                (effect
                                  (register-events
                                    boomerang
                                    [{:event :run-ends
                                      :duration :end-of-run
                                      :optional
                                      {:req (req (and (:successful target)
                                                      (not (zone-locked? state :runner :discard))))
                                       :once :per-run
                                       :prompt (msg "Shuffle a copy of " (:title card) " back into the Stack?")
                                       :yes-ability
                                       {:msg (msg "shuffle a copy of " (:title card) " back into the Stack")
                                        :effect (effect (move card :deck)
                                                        (shuffle! :deck))}}}]))}}))
                          card nil))))]})

(defcard "Box-E"
  {:constant-effects [(mu+ 2)
                      (runner-hand-size+ 2)]})

(defcard "Brain Cage"
  {:constant-effects [(runner-hand-size+ 3)]
   :on-install {:async true
                :effect (effect (damage eid :brain 1 {:card card}))}})

(defcard "Brain Chip"
  (let [runner-points (fn [s] (max (get-in @s [:runner :agenda-point] 0) 0))]
    {:constant-effects [(mu+
                          (req (pos? (runner-points state)))
                          ;; [:regular N] is needed to make the mu system work
                          (req [:regular (runner-points state)]))
                        (runner-hand-size+ (req (runner-points state)))]}))

(defcard "Buffer Drive"
  (let [grip-or-stack-trash?
        (fn [targets]
          (some #(and (runner? (:card %))
                      (or (in-hand? (:card %))
                          (in-deck? (:card %))))
                targets))
        triggered-ability
        {:once-per-instance true
         :req (req (and (grip-or-stack-trash? targets)
                        (first-trash? state grip-or-stack-trash?)))
         :prompt "Add a trashed card to the bottom of the stack"
         :choices (req (conj (sort (map :title (map :card targets))) "No action"))
         :async true
         :effect (req (if (= "No action" target)
                        (effect-completed state side eid)
                        (do (system-msg state side
                                        (str "uses Buffer Drive to add " target
                                             " to the bottom of the stack"))
                            (move state side (find-card target (:discard (:runner @state))) :deck)
                            (effect-completed state side eid))))}]
    {:events [(assoc triggered-ability :event :runner-trash)
              (assoc triggered-ability :event :corp-trash)]
     :abilities [{:req (req (not (zone-locked? state :runner :discard)))
                  :label "Add a card from the heap to the top of the stack"
                  :cost [:remove-from-game]
                  :show-discard true
                  :choices {:card #(and (runner? %)
                                        (in-discard? %))}
                  :msg (msg "add " (:title target) " to the top of the stack")
                  :effect (effect (move target :deck {:front true}))}]}))

(defcard "Capstone"
  {:abilities [{:req (req (pos? (count (:hand runner))))
                :label "trash and install cards"
                :cost [:click 1]
                :async true
                :prompt "Select any number of cards to trash from your grip"
                :choices {:max (req (count (:hand runner)))
                          :card #(and (runner? %)
                                      (in-hand? %))}
                :effect (req (let [trashed-card-names (keep :title targets)
                                   installed-card-names (keep :title (all-active-installed state :runner))
                                   overlap (clj-set/intersection (set trashed-card-names)
                                                                 (set installed-card-names))]
                               (wait-for (trash-cards state side targets {:unpreventable true})
                                         (let [trashed-cards async-result]
                                           (wait-for (draw state side (count (filter overlap trashed-card-names)) nil)
                                                     (system-msg state side
                                                                 (str "spends [Click] to use Capstone to trash "
                                                                      (string/join ", " (map :title trashed-cards))
                                                                      " from the grip and draw "
                                                                      (quantify (count async-result) "card")))
                                                     (effect-completed state side eid))))))}]})

(defcard "Carnivore"
  {:constant-effects [(mu+ 1)]
   :interactions
   {:access-ability
    {:label "Trash card"
     :req (req (and (not (get-in @state [:per-turn (:cid card)]))
                    (<= 2 (count (:hand runner)))))
     :cost [:trash-from-hand 2]
     :msg (msg "trash " (:title target) " at no cost")
     :once :per-turn
     :async true
     :effect (effect (trash eid (assoc target :seen true) {:accessed true}))}}})

(defcard "Chop Bot 3000"
  {:flags {:runner-phase-12 (req (>= (count (all-installed state :runner)) 2))}
   :abilities [{:req (req (:runner-phase-12 @state))
                :label "trash and draw or remove tag"
                :choices {:card #(and (runner? %)
                                      (installed? %))
                          :not-self true}
                :async true
                :effect (req (wait-for (trash state :runner target nil)
                                       (continue-ability
                                         state side
                                         (let [trash-str (str "uses Chop Bot 3000 to trash " (:title target))
                                               deck (pos? (count (:deck runner)))
                                               tags (pos? (count-real-tags state))]
                                           {:req (req (or deck tags))
                                            :prompt "Draw 1 card or remove 1 tag"
                                            :choices [(when deck "Draw 1 card")
                                                      (when tags "Remove 1 tag")]
                                            :async true
                                            :effect (req (if (= target "Draw 1 card")
                                                           (do (system-msg state :runner (str trash-str " and draw 1 card"))
                                                               (draw state :runner eid 1 nil))
                                                           (do (system-msg state :runner (str trash-str " and remove 1 tag"))
                                                               (lose-tags state :runner eid 1))))})
                                         card nil)))}]})

(defcard "Clone Chip"
  {:abilities [{:prompt "Select a program to install from your Heap"
                :label "install card from heap"
                :show-discard true
                :req (req (and (not (zone-locked? state :runner :discard))
                               (not (install-locked? state side))
                               (some #(and (program? %)
                                           (can-pay? state side (assoc eid :source card :source-type :runner-install) % nil
                                                     [:credit (install-cost state side %)]))
                                     (:discard runner))))
                :choices {:req (req (and (program? target)
                                         (in-discard? target)
                                         (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                                   [:credit (install-cost state side target)])))}
                :cost [:trash]
                :msg (msg "install " (:title target))
                :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target nil))}]})

(defcard "Comet"
  {:constant-effects [(mu+ 1)]
   :events [{:event :play-event
             :req (req (first-event? state side :play-event))
             :effect (req (system-msg state :runner
                                      (str "can play another event without spending a [Click] by clicking on Comet"))
                          (update! state side (assoc card :comet-event true)))}]
   :abilities [{:async true
                :label "play an event in hand twice"
                :req (req (:comet-event card))
                :prompt "Select an Event in your Grip to play"
                :choices {:card #(and (event? %)
                                      (in-hand? %))}
                :msg (msg "play " (:title target))
                :effect (effect (update! (dissoc (get-card state card) :comet-event))
                                (play-instant eid target nil))}]})

(defcard "Cortez Chip"
  {:abilities [{:prompt "Select a piece of ice"
                :label "increase rez cost of ice"
                :choices {:card #(and (ice? %)
                                      (not (rezzed? %)))}
                :msg (msg "increase the rez cost of " (card-str state target)
                          " by 2 [Credits] until the end of the turn")
                :cost [:trash]
                :effect (effect (register-floating-effect
                                  card
                                  (let [ice target]
                                    {:type :rez-additional-cost
                                     :duration :end-of-turn
                                     :req (req (same-card? target ice))
                                     :value [:credit 2]})))}]})

(defcard "Cyberdelia"
  {:constant-effects [(mu+ 1)]
   :events [{:event :subroutines-broken
             :req (req (and (every? :broken (:subroutines target))
                            (first-event? state side :subroutines-broken #(every? :broken (:subroutines (first %))))))
             :msg "gain 1 [Credits] for breaking all subroutines on a piece of ice"
             :async true
             :effect (effect (gain-credits eid 1))}]})

(defcard "Cyberfeeder"
  {:recurring 1
   :interactions {:pay-credits {:req (req (or (and (= :runner-install (:source-type eid))
                                                   (has-subtype? target "Virus")
                                                   (program? target))
                                              (and (= :ability (:source-type eid))
                                                   (has-subtype? target "Icebreaker"))))
                                :type :recurring}}})

(defcard "CyberSolutions Mem Chip"
  {:constant-effects [(mu+ 2)]})

(defcard "Cybsoft MacroDrive"
  {:recurring 1
   :interactions {:pay-credits {:req (req (and (= :runner-install (:source-type eid))
                                               (program? target)))
                                :type :recurring}}})

(defcard "Daredevil"
  {:constant-effects [(mu+ 2)]
   :events [{:event :run
             :once :per-turn
             :req (req (and (<= 2 (:position target))
                            (first-event? state side :run #(<= 2 (:position (first %))))))
             :msg "draw two cards"
             :async true
             :effect (effect (draw eid 2 nil))}]})

(defcard "Dedicated Processor"
  {:implementation "Click Dedicated Processor to use ability"
   :req (req (not-empty (filter #(has-subtype? % "Icebreaker") (all-active-installed state :runner))))
   :hosting {:card #(and (has-subtype? % "Icebreaker")
                         (not (has-subtype? % "AI"))
                         (installed? %))}
   :abilities [{:cost [:credit 2]
                :label "add 4 strength for the remainder of the run"
                :req (req run)
                :effect (effect (pump (get-card state (:host card)) 4))
                :msg (msg "pump the strength of " (get-in card [:host :title]) " by 4")}]})

(defcard "Deep Red"
  {:constant-effects [(caissa-mu+ 3)]
   :events [{:event :runner-install
             :optional
             {:req (req (has-subtype? (:card context) "Caïssa"))
              :prompt "Use Deep Red?"
              :yes-ability
              {:async true
               :effect (effect
                         (continue-ability
                           (let [cid (:cid (:card context))]
                             {:async true
                              :prompt "Choose the just-installed Caïssa to have Deep Red trigger its [Click] ability"
                              :choices {:card #(= cid (:cid %))}
                              :msg (msg "trigger the [Click] ability of " (:title target)
                                        " without spending [Click]")
                              :effect (req (gain state :runner :click 1)
                                           (play-ability state side {:card target :ability 0})
                                           (effect-completed state side eid))})
                           card nil))}}}]})

(defcard "Demolisher"
  {:constant-effects [(mu+ 1)
                      {:type :trash-cost
                       :value -1}]
   :events [{:event :runner-trash
             :req (req (and (corp? (:card target))
                            (first-event? state side :runner-trash
                                          (fn [targets]
                                            (some #(corp? (:card %)) targets)))))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits :runner eid 1))}]})

(defcard "Desperado"
  {:constant-effects [(mu+ 1)]
   :events [{:event :successful-run
             :silent (req true)
             :async true
             :msg "gain 1 [Credits]"
             :effect (effect (gain-credits eid 1))}]})

(defcard "Devil Charm"
  {:events [{:event :encounter-ice
             :interactive (req true)
             :optional
             {:prompt "Remove this card from the game: give encountered ice -6 strength?"
              :yes-ability
              {:msg (msg "give -6 strength to " (card-str state (:ice context)) " for the remainder of the run")
               :cost [:remove-from-game]
               :effect (effect (register-floating-effect
                                 card
                                 (let [ice (:ice context)]
                                   {:type :ice-strength
                                    :duration :end-of-run
                                    :req (req (same-card? target ice))
                                    :value -6}))
                               (update-all-ice))}}}]})

(defcard "Dinosaurus"
  {:abilities [{:label "Install a non-AI icebreaker on Dinosaurus"
                :req (req (empty? (:hosted card)))
                :cost [:click 1]
                :prompt "Select a non-AI icebreaker in your Grip to install on Dinosaurus"
                :choices {:card #(and (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "AI"))
                                      (in-hand? %))}
                :async true
                :effect (effect (runner-install eid target {:host-card card :no-mu true}))}
               {:label "Host an installed non-AI icebreaker on Dinosaurus"
                :req (req (empty? (:hosted card)))
                :prompt "Select an installed non-AI icebreaker to host on Dinosaurus"
                :choices {:card #(and (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "AI"))
                                      (installed? %))}
                :msg (msg "host " (:title target))
                :effect (effect (host card target)
                                (unregister-effects-for-card target #(= :used-mu (:type %)))
                                (update-mu))}]
   :constant-effects [{:type :breaker-strength
                       :req (req (same-card? target (first (:hosted card))))
                       :value 2}]})

(defcard "Docklands Pass"
  {:events [{:event :pre-access
             :req (req (and (= :hq target)
                            (first-event? state side :pre-access #(= :hq (first %)))))
             :silent (req true)
             :msg "access 1 additional cards from HQ"
             :effect (effect (access-bonus :runner :hq 1))}]})

(defcard "Doppelgänger"
  {:constant-effects [(mu+ 1)]
   :events [{:event :runner-install
             :req (req (same-card? card (:card context)))
             :silent (req true)
             :effect (effect (update! (assoc card :dopp-active true)))}
            {:event :runner-turn-begins
             :effect (effect (update! (assoc card :dopp-active true)))}
            {:event :run-ends
             :interactive (req true)
             :optional
             {:req (req (and (:successful target)
                             (:dopp-active (get-card state card))))
              :player :runner
              :prompt "Use Doppelgänger to run again?"
              :yes-ability {:prompt "Choose a server"
                            :async true
                            :choices (req runnable-servers)
                            :msg (msg "make a run on " target)
                            :makes-run true
                            :effect (effect (update! (dissoc card :dopp-active))
                                            (unregister-floating-effects :end-of-run)
                                            (unregister-floating-events :end-of-run)
                                            (update-all-icebreakers)
                                            (update-all-ice)
                                            (reset-all-ice)
                                            (clear-wait-prompt :corp)
                                            (make-run eid target (get-card state card)))}}}]})

(defcard "Dorm Computer"
  {:data {:counter {:power 4}}
   :abilities [{:cost [:click 1 :power 1]
                :req (req (not run))
                :prompt "Choose a server"
                :choices (req runnable-servers)
                :msg "make a run and avoid all tags for the remainder of the run"
                :makes-run true
                :async true
                :effect (effect (register-events
                                  card
                                  [{:event :pre-tag
                                    :duration :end-of-run
                                    :async true
                                    :msg "avoid all tags during the run"
                                    :effect (effect (tag-prevent :runner eid Integer/MAX_VALUE))}])
                                (make-run eid target card))}]})

(defcard "Dyson Fractal Generator"
  {:recurring 1
   :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                               (has-subtype? target "Fracter")
                                               (has-subtype? target "Icebreaker")))
                                :type :recurring}}})

(defcard "Dyson Mem Chip"
  {:constant-effects [(mu+ 1)
                      (link+ 1)]})

(defcard "DZMZ Optimizer"
  {:constant-effects [(mu+ 1)
                      {:type :install-cost
                       :req (req (and (program? target)
                                      (no-event? state :runner :runner-install #(program? (:card (first %))))))
                       :value -1}]
   :events [{:event :runner-install
             :req (req (and (program? target)
                            (first-event? state :runner :runner-install #(program? (first %)))))
             :silent (req true)
             :msg (msg "reduce the install cost of " (:title target) " by 1 [Credits]")}]})

(defcard "e3 Feedback Implants"
  {:abilities [(break-sub 1 1 "All" {:req (req true)})]})

(defcard "Ekomind"
  (let [update-base-mu (fn [state n] (swap! state assoc-in [:runner :memory :base] n))]
    {:effect (req (update-base-mu state (count (get-in @state [:runner :hand])))
                  (add-watch state :ekomind (fn [k ref old new]
                                              (let [hand-size (count (get-in new [:runner :hand]))]
                                                (when (not= (count (get-in old [:runner :hand])) hand-size)
                                                  (update-base-mu ref hand-size))))))
     :leave-play (req (remove-watch state :ekomind))}))

(defcard "EMP Device"
  {:abilities [{:req (req run)
                :msg "prevent the Corp from rezzing more than 1 piece of ice for the remainder of the run"
                :cost [:trash]
                :effect (effect
                          (register-events
                            card
                            [{:event :rez
                              :duration :end-of-run
                              :unregister-once-resolved true
                              :req (req (ice? (:card context)))
                              :effect (effect (register-run-flag!
                                                card :can-rez
                                                (fn [state side card]
                                                  (if (ice? card)
                                                    ((constantly false)
                                                     (toast state :corp "Cannot rez ice the rest of this run due to EMP Device"))
                                                    true))))}]))}]})

(defcard "Feedback Filter"
  {:interactions {:prevent [{:type #{:net :brain}
                             :req (req true)}]}
   :abilities [{:cost [:credit 3]
                :msg "prevent 1 net damage"
                :effect (effect (damage-prevent :net 1))}
               {:label "Prevent up to 2 brain damage"
                :msg "prevent up to 2 brain damage"
                :cost [:trash]
                :effect (effect (damage-prevent :brain 2))}]})

(defcard "Flame-out"
  (let [turn-end {:async true
                  :effect (req (unregister-events state :runner card)
                               (if-let [hosted (first (:hosted card))]
                                 (do (system-msg state :runner (str "trashes " (:title hosted) " from Flame-out"))
                                     (trash state side eid hosted nil))
                                 (effect-completed state side eid)))}]
    {:implementation "Credit usage restriction not enforced"
     :data {:counter {:credit 9}}
     :abilities [{:label "Take 1 [Credits] from Flame-out"
                  :req (req (and (not-empty (:hosted card))
                                 (pos? (get-counters card :credit))))
                  :async true
                  :effect (req (add-counter state side card :credit -1)
                               (system-msg state :runner "takes 1 [Credits] from Flame-out")
                               (register-events
                                 state :runner (get-card state card)
                                 [(assoc turn-end :event :runner-turn-ends)
                                  (assoc turn-end :event :corp-turn-ends)])
                               (gain-credits state :runner eid 1))}
                 {:label "Take all [Credits] from Flame-out"
                  :req (req (and (not-empty (:hosted card))
                                 (pos? (get-counters card :credit))))
                  :async true
                  :effect (req (let [credits (get-counters card :credit)]
                                 (update! state :runner (dissoc-in card [:counter :credit]))
                                 (system-msg state :runner (str "takes " credits " [Credits] from Flame-out"))
                                 (register-events
                                   state :runner (get-card state card)
                                   [(assoc turn-end :event :runner-turn-ends)
                                    (assoc turn-end :event :corp-turn-ends)])
                                 (gain-credits state :runner eid credits)))}
                 {:label "Install a program on Flame-out"
                  :req (req (empty? (:hosted card)))
                  :cost [:click 1]
                  :prompt "Select a program in your Grip to install on Flame-out"
                  :choices {:card #(and (program? %)
                                        (in-hand? %))}
                  :async true
                  :effect (effect (update! (assoc-in (get-card state card) [:special :flame-out] (:cid target)))
                                  (runner-install eid target {:host-card card}))}
                 {:label "Host an installed program on Flame-out"
                  :req (req (empty? (:hosted card)))
                  :prompt "Select an installed program to host on Flame-out"
                  :choices {:card #(and (program? %)
                                        (installed? %))}
                  :msg (msg "host " (:title target))
                  :effect (req (->> target
                                    (get-card state)
                                    (host state side card))
                               (update! state side (assoc-in (get-card state card) [:special :flame-out] (:cid target))))}]
     :events [{:event :card-moved
               :req (req (= (:cid target) (get-in (get-card state card) [:special :flame-out])))
               :effect (effect (update! (dissoc-in card [:special :flame-out])))}
              {:event :runner-turn-ends}
              {:event :corp-turn-ends}]
     :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                                 (same-card? card (:host target))
                                                 (pos? (get-counters card :credit))))
                                  :custom-amount 1
                                  :custom (req (add-counter state side card :credit -1)
                                               (register-events
                                                 state side (get-card state card)
                                                 [(assoc turn-end :event :runner-turn-ends)
                                                  (assoc turn-end :event :corp-turn-ends)])
                                               (effect-completed state side (make-result eid 1)))
                                  :type :custom}}}))

(defcard "Flip Switch"
  {:events [{:event :pre-init-trace
             :optional
             {:req (req (= :runner (:active-player @state)))
              :waiting-prompt "Runner to choose an option"
              :prompt "Use Flip Switch to reduce base trace strength to 0?"
              :player :runner
              :yes-ability {:msg "reduce the base trace strength to 0"
                            :cost [:trash]
                            :effect (req (swap! state assoc-in [:trace :force-base] 0))}}}]
   :abilities [{:label "Jack out"
                :req (req (and run
                               (= :runner (:active-player @state))))
                :msg "jack out"
                :cost [:trash]
                :async true
                :effect (effect (jack-out eid))}
               {:label "Remove 1 tag"
                :req (req (and (pos? (count-real-tags state))
                               (= :runner (:active-player @state))))
                :msg "remove 1 tag"
                :cost [:trash]
                :async true
                :effect (effect (lose-tags eid 1))}]})

(defcard "Forger"
  {:interactions {:prevent [{:type #{:tag}
                             :req (req true)}]}
   :constant-effects [(link+ 1)]
   :abilities [{:msg "avoid 1 tag"
                :label "Avoid 1 tag"
                :async true
                :cost [:trash]
                :effect (effect (tag-prevent :runner eid 1))}
               {:msg "remove 1 tag"
                :label "Remove 1 tag"
                :cost [:trash]
                :async true
                :effect (effect (lose-tags eid 1))}]})

(defcard "Friday Chip"
  (let [ability {:msg (msg "move 1 virus counter to " (:title target))
                 :req (req (and (pos? (get-counters card :virus))
                                (pos? (count-virus-programs state))))
                 :choices {:card virus-program?}
                 :effect (req (add-counter state :runner card :virus -1)
                              (add-counter state :runner target :virus 1))}]
    {:abilities [(set-autoresolve :auto-accept "Friday Chip")]
     :on-install {:effect (effect (toast "Tip: You can toggle automatically adding virus counters by clicking Friday Chip."))}
     :events [(assoc ability :event :runner-turn-begins)
              {:event :runner-trash
               :once-per-instance true
               :async true
               :req (req (some #(corp? (:card %)) targets))
               :effect (req (let [amt-trashed (count (filter #(corp? (:card %)) targets))
                                  sing-ab {:optional
                                           {:prompt "Place a virus counter on Friday Chip?"
                                            :autoresolve (get-autoresolve :auto-accept)
                                            :yes-ability {:effect (effect (system-msg
                                                                            :runner
                                                                            "places 1 virus counter on Friday Chip")
                                                                          (add-counter :runner card :virus 1))}}}
                                  mult-ab {:prompt "Place virus counters on Friday Chip?"
                                           :choices {:number (req amt-trashed)
                                                     :default (req amt-trashed)}
                                           :effect (effect (system-msg :runner
                                                                       (str "places "
                                                                            (quantify target "virus counter")
                                                                            " on Friday Chip"))
                                                           (add-counter :runner card :virus target))}
                                  ab (if (> amt-trashed 1) mult-ab sing-ab)]
                              (continue-ability state side ab card targets)))}]}))

(defcard "Gachapon"
  (letfn [(shuffle-end [remove-from-game shuffle-back]
            {:msg (msg "shuffle " (string/join ", " (map :title shuffle-back)) " into the stack"
                       " and remove " (string/join ", " (map :title remove-from-game)) " from the game")
             :effect (req
                       (doseq [c remove-from-game]
                         (move state side c :rfg))
                       (doseq [c shuffle-back]
                         (move state side c :deck))
                       (shuffle! state side :deck))})
          (shuffle-next [set-aside target to-shuffle]
            (let [set-aside (remove-once #(= % target) set-aside)
                  to-shuffle (if target
                               (concat to-shuffle [target])
                               [])
                  finished? (or (= 3 (count to-shuffle))
                                (empty? set-aside))]
              {:prompt (msg (if finished?
                              (str "Removing: " (if (not-empty set-aside)
                                                  (string/join ", " (map :title set-aside))
                                                  "nothing")
                                   "[br]Shuffling: " (if (not-empty to-shuffle)
                                                       (string/join ", " (map :title to-shuffle))
                                                       "nothing"))
                              (str "Choose " (- 3 (count to-shuffle)) " more cards to shuffle back."
                                   (when (not-empty to-shuffle)
                                     (str "[br]Currently shuffling back: " (string/join ", " (map :title to-shuffle)))))))
               :async true
               :not-distinct true ; show cards separately
               :choices (req (if finished?
                               ["Done" "Start over"]
                               (seq set-aside)))
               :effect (req (if finished?
                              (if (= "Done" target)
                                (continue-ability state side
                                                  (shuffle-end set-aside to-shuffle)
                                                  card nil)
                                (continue-ability state side
                                                  (shuffle-next (sort-by :title (concat set-aside to-shuffle)) nil nil)
                                                  card nil))
                              (continue-ability state side
                                                (shuffle-next set-aside target to-shuffle)
                                                card nil)))}))]
    {:abilities [{:label "Install a card from the top of the stack"
                  :cost [:trash]
                  :msg "install a card from the top of the stack"
                  :async true
                  :waiting-prompt "Runner to make a decision"
                  :effect (req (let [set-aside (sort-by :title (take 6 (:deck runner)))]
                                 (wait-for
                                   (resolve-ability state side
                                                    {:prompt (msg "The set aside cards are: " (string/join ", " (map :title set-aside)))
                                                     :choices ["OK"]}
                                                    card nil)
                                   (continue-ability
                                     state side
                                     {:prompt "Choose a card to install"
                                      :async true
                                      :choices (req (concat
                                                      (filter #(and (or (program? %)
                                                                        (and (resource? %)
                                                                             (has-subtype? % "Virtual")))
                                                                    (can-pay? state side
                                                                              (assoc eid :source card :source-type :runner-install)
                                                                              % nil [:credit (install-cost state side % {:cost-bonus -2})]))
                                                              set-aside)
                                                      ["No action"]))
                                      :cancel-effect (effect (continue-ability (shuffle-next set-aside nil nil) card nil))
                                      :effect (req (if (= "No action" target)
                                                     (continue-ability state side (shuffle-next set-aside nil nil) card nil)
                                                     (let [to-install target
                                                           set-aside (remove-once #(= % target) set-aside)
                                                           new-eid (assoc eid :source card :source-type :runner-install)]
                                                       (wait-for (runner-install state side new-eid target {:cost-bonus -2})
                                                                 (continue-ability state side (shuffle-next set-aside nil nil) card nil)))))}
                                     card nil))))}]}))

(defcard "Gebrselassie"
  {:abilities [{:msg "host it on an installed non-AI icebreaker"
                :cost [:click 1]
                :choices {:card #(and (installed? %)
                                      (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "AI")))}
                :effect (req (when-let [host (get-card state (:host card))]
                               (swap! state assoc :effects
                                      (reduce
                                        (fn [effects e]
                                          (conj effects
                                                (if (and (same-card? host (:card e))
                                                         (= :breaker-strength (:type e))
                                                         (:original-duration e))
                                                  (-> e
                                                      (assoc :duration (:original-duration e))
                                                      (dissoc :original-duration))
                                                  e)))
                                        []
                                        (:effects @state)))
                               (update-breaker-strength state side host))
                             (host state side target card))}]
   :events [{:event :pump-breaker
             :req (req (same-card? target (:host card)))
             :effect (req (let [last-pump (assoc (second targets)
                                                 :duration :end-of-turn
                                                 :original-duration (:duration (last (:effects @state))))]
                            (swap! state assoc :effects
                                   (->> (:effects @state)
                                        (remove #(= (:uuid last-pump) (:uuid %)))
                                        (#(conj % last-pump))
                                        (into []))))
                          (update-breaker-strength state side target))}]
   :leave-play (req (when-let [host (get-card state (:host card))]
                      (swap! state assoc :effects
                             (reduce
                               (fn [effects e]
                                 (conj effects
                                       (if (and (same-card? host (:card e))
                                                (= :breaker-strength (:type e))
                                                (:original-duration e))
                                         (-> e
                                             (assoc :duration (:original-duration e))
                                             (dissoc :original-duration))
                                         e)))
                               []
                               (:effects @state)))
                      (update-breaker-strength state side host)))})

(defcard "GPI Net Tap"
  {:implementation "Trash and jack out effect is manual"
   :abilities [{:req (req (and (ice? current-ice) (not (rezzed? current-ice))))
                :label "expose approached ice"
                :async true
                :effect (effect (expose eid current-ice))}]})

(defcard "Grimoire"
  {:constant-effects [(mu+ 2)]
   :events [{:event :runner-install
             :silent (req true)
             :req (req (has-subtype? (:card context) "Virus"))
             :effect (effect (add-counter (:card context) :virus 1))}]})

(defcard "Heartbeat"
  {:constant-effects [(mu+ 1)]
   :interactions {:prevent [{:type #{:net :brain :meat}
                             :req (req true)}]}
   :abilities [{:label "Prevent 1 damage"
                :msg "prevent 1 damage"
                :cost [:installed 1]
                :effect (effect (damage-prevent :brain 1)
                                (damage-prevent :meat 1)
                                (damage-prevent :net 1))}]})

(defcard "Hijacked Router"
  {:events [{:event :server-created
             :msg "force the Corp to lose 1 [Credits]"
             :async true
             :effect (effect (lose-credits :corp eid 1))}
            {:event :successful-run
             :optional
             {:req (req (= :archives (target-server context)))
              :prompt "Trash Hijacked Router to force the Corp to lose 3 [Credits]?"
              :yes-ability
              {:async true
               :effect (req (system-msg state :runner "trashes Hijacked Router to force the Corp to lose 3 [Credits]")
                            (wait-for (trash state :runner card {:unpreventable true})
                                      (lose-credits state :corp eid 3)))}}}]})

(defcard "Hippo"
  (letfn [(build-hippo-pred [outermost-ices]
            (fn [events]
              (not (empty? (filter #(true? %) (map #(and (same-card? % (first events))
                                                         (every? :broken (:subroutines (first events)))) outermost-ices))))))]
    {:events [{:event :subroutines-broken
               :optional
               {:req (req (let [servers (->> (:corp @state) :servers seq flatten)
                                outermost-ices (filter #(some? %) (map #(last (:ices %)) servers))
                                pred (build-hippo-pred outermost-ices)]
                            (and (same-card? (last run-ices) target)
                                 (every? :broken (:subroutines target))
                                 (first-event? state side :subroutines-broken pred))))
                :prompt (msg "Remove Hippo from the game to trash " (:title target) "?")
                :yes-ability
                {:async true
                 :effect (effect (system-msg (str "removes Hippo from the game to trash " (card-str state target)))
                                 (move card :rfg)
                                 (trash eid target nil))}}}]}))

(defcard "HQ Interface"
  {:in-play [:hq-access 1]})

(defcard "Keiko"
  {:constant-effects [(mu+ 2)]
   :events [{:event :spent-credits-from-card
             :req (req (and (not (facedown? target))
                            (has-subtype? target "Companion")
                            (= 1 (+ (event-count state :runner :spent-credits-from-card
                                                 #(and (not (facedown? (first %)))
                                                       (has-subtype? (first %) "Companion")))
                                    (event-count state :runner :runner-install
                                                 #(and (not (facedown? (:card (first %))))
                                                       (has-subtype? (:card (first %)) "Companion")))))))
             :msg "gain 1 [Credit]"
             :async true
             :effect (effect (gain-credits :runner eid 1))}
            {:event :runner-install
             :req (req (and (not (:facedown context))
                            (has-subtype? (:card context) "Companion")
                            (= 1 (+ (event-count state :runner :spent-credits-from-card
                                                 #(and (not (facedown? (first %)))
                                                       (has-subtype? (first %) "Companion")))
                                    (event-count state :runner :runner-install
                                                 #(and (not (:facedown (first %)))
                                                       (has-subtype? (:card (first %)) "Companion")))))))
             :msg "gain 1 [Credit]"
             :async true
             :effect (effect (gain-credits :runner eid 1))}]})

(defcard "Knobkierie"
  {:constant-effects [(virus-mu+ 3)]
   :events [{:event :successful-run
             :interactive (req true)
             :optional {:req (req (and (first-event? state :runner :successful-run)
                                       (pos? (count-virus-programs state))))
                        :prompt "Place a virus counter?"
                        :autoresolve (get-autoresolve :auto-add)
                        :yes-ability {:prompt "Select an installed virus program for Knobkierie to add a virus counter to"
                                      :choices {:card #(and (installed? %)
                                                            (has-subtype? % "Virus")
                                                            (program? %))}
                                      :msg (msg "place 1 virus counter on " (:title target))
                                      :effect (effect (add-counter target :virus 1))}}}]
   :abilities [(set-autoresolve :auto-add "Knobkierie")]})

(defcard "Lemuria Codecracker"
  {:abilities [{:cost [:click 1 :credit 1]
                :req (req (some #{:hq} (:successful-run runner-reg)))
                :choices {:card installed?}
                :effect (effect (expose eid target))
                :msg "expose 1 card"}]})

(defcard "LLDS Memory Diamond"
  {:constant-effects [(link+ 1)
                      (runner-hand-size+ 1)
                      (mu+ 1)]})

(defcard "LLDS Processor"
  {:events [{:event :runner-install
             :silent (req true)
             :req (req (has-subtype? (:card context) "Icebreaker"))
             :effect (effect (pump (:card context) 1 :end-of-turn))}]})

(defcard "Lockpick"
  {:recurring 1
   :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                               (has-subtype? target "Decoder")
                                               (has-subtype? target "Icebreaker")))
                                :type :recurring}}})

(defcard "Logos"
  {:constant-effects [(mu+ 1)
                      (runner-hand-size+ 1)]
   :events [{:event :agenda-scored
             :player :runner
             :prompt "Choose a card"
             :msg "add 1 card to their Grip from their Stack"
             :choices (req (cancellable (:deck runner)))
             :effect (effect (trigger-event :searched-stack nil)
                             (shuffle! :deck)
                             (move target :hand))}]})

(defcard "Lucky Charm"
  {:interactions {:prevent [{:type #{:end-run}
                             :req (req (and (some #{:hq} (:successful-run runner-reg))
                                            (corp? (:card-cause target))))}]}
   :abilities [{:msg "prevent the run from ending"
                :req (req (some #{:hq} (:successful-run runner-reg)))
                :cost [:remove-from-game]
                :effect (effect (end-run-prevent))}]})

(defcard "Mâché"
  (letfn [(pred [{:keys [card accessed]}]
            (and accessed (corp? card)))]
    {:abilities [{:label "Draw 1 card"
                  :msg "draw 1 card"
                  :cost [:power 3]
                  :keep-open :while-3-power-tokens-left
                  :async true
                  :effect (effect (draw :runner eid 1 nil))}]
     :events [{:event :runner-trash
               :once-per-instance true
               :req (req (and (some pred targets)
                              (first-event? state side :runner-trash (fn [targets] (some pred targets)))))
               :effect (req (let [target (some #(when (pred %) (:card %)) targets)
                                  cost (trash-cost state side target)]
                              (when cost
                                (system-msg state side (str "places " cost
                                                            " power counters on Mâché"))
                                (add-counter state side card :power cost))))}]}))

(defcard "Masterwork (v37)"
  {:constant-effects [(mu+ 1)]
   :events [{:event :run
             :interactive (req true)
             :optional
             {:req (req (some #(and (hardware? %)
                                    (can-pay? state side (assoc eid :source card :source-type :runner-install) card %
                                              [:credit (install-cost state side % {:cost-bonus 1})]))
                              (:hand runner)))
              :prompt "Pay 1 [Credit] to install a hardware?"
              :yes-ability {:async true
                            :prompt "Select a piece of hardware"
                            :choices
                            {:req (req (and (in-hand? target)
                                            (hardware? target)
                                            (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                                      [:credit (install-cost state side target {:cost-bonus 1})])))}
                            :msg (msg "install " (:title target) " from the grip, paying 1 [Credit] more")
                            :effect (effect (runner-install eid target {:cost-bonus 1}))}}}
            {:event :runner-install
             :async true
             :interactive (req true)
             :req (req (and (hardware? (:card context))
                            (first-event? state side :runner-install #(hardware? (:card (first %))))))
             :effect (effect (draw eid 1 nil))}]})

(defcard "Māui"
  {:constant-effects [(mu+ 2)]
   :recurring (req (count (get-in corp [:servers :hq :ices])))
   :interactions {:pay-credits {:req (req (= [:hq] (get-in @state [:run :server])))
                                :type :recurring}}})

(defcard "Maw"
  {:constant-effects [(mu+ 2)]
   :events [{:event :post-access-card
             :label "Trash a card from HQ"
             :async true
             :req (req (and (= 1 (get-in @state [:runner :register :no-trash-or-steal]))
                            (pos? (count (:hand corp)))
                            (not (in-discard? target))))
             :once :per-turn
             :msg "force the Corp to trash a random card from HQ"
             :effect (req (let [card-to-trash (first (shuffle (:hand corp)))
                                card-seen? (same-card? target card-to-trash)
                                card-to-trash (if card-seen? (assoc card-to-trash :seen true)
                                                card-to-trash)]
                            (trash state :corp eid card-to-trash nil)))}]})

(defcard "Maya"
  {:constant-effects [(mu+ 2)]
   :events [{:event :post-access-card
             :optional
             {:req (req (in-deck? (second targets)))
              :once :per-turn
              :prompt (msg "Move " (:title target) " to the bottom of R&D?")
              :yes-ability
              {:msg "move the card just accessed to the bottom of R&D"
               :async true
               :effect (req (move state :corp target :deck)
                            (gain-tags state :runner eid 1))}}}]})

(defcard "MemStrips"
  {:constant-effects [(virus-mu+ 3)]})

(defcard "Mind's Eye"
  {:implementation "Power counters added automatically"
   :constant-effects [(mu+ 1)]
   :events [{:event :successful-run
             :silent (req true)
             :req (req (= :rd (target-server context)))
             :effect (effect (add-counter card :power 1))}]
   :abilities [{:async true
                :cost [:click 1 :power 3]
                :msg "access the top card of R&D"
                :effect (req (do-access state side eid [:rd] {:no-root true}))}]})

(defcard "Mirror"
  {:constant-effects [(mu+ 2)]
   :events [{:event :successful-run
             :async true
             :req (req (= :rd (target-server context)))
             :effect (effect (continue-ability
                               {:prompt "Select a card and replace 1 spent [Recurring Credits] on it"
                                :choices {:card #(< (get-counters % :recurring) (:recurring (card-def %) 0))}
                                :msg (msg "replace 1 spent [Recurring Credits] on " (:title target))
                                :effect (effect (add-counter target :recurring 1))}
                               card nil))}]})

(defcard "Monolith"
  (let [mhelper
        (fn mh [n]
          {:prompt "Select a program to install"
           :choices {:req (req (and (program? target)
                                    (in-hand? target)
                                    (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                              [:credit (install-cost state side target {:cost-bonus -4})])))}
           :async true
           :effect (req (wait-for (runner-install state side target {:cost-bonus -4})
                                  (continue-ability state side (when (< n 3) (mh (inc n))) card nil)))})]
    {:interactions {:prevent [{:type #{:net :brain}
                               :req (req true)}]}
     :constant-effects [(mu+ 3)]
     :on-install {:async true
                  :effect (effect (continue-ability (mhelper 1) card nil))}
     :abilities [{:msg "prevent 1 brain or net damage"
                  :cost [:trash-program-from-hand 1]
                  :effect (effect (damage-prevent :brain 1)
                                  (damage-prevent :net 1))}]}))

(defcard "Mu Safecracker"
  {:implementation "Stealth credit restriction not enforced"
   :events [{:event :successful-run
             :optional
             {:req (req (and (= :hq (target-server context))
                             (some #(has-subtype? % "Stealth")
                                   (all-active state :runner))))
              :prompt "Pay 1 [Credits] to access 1 additional card?"
              :yes-ability
              {:async true
               :effect
               (effect
                 (continue-ability
                   {:eid (assoc eid :source-type :ability)
                    :async true
                    :cost [:credit 1]
                    :cost-req all-stealth
                    :msg "access 1 additional card from HQ"
                    :effect (effect (access-bonus :hq 1)
                                    (effect-completed eid))}
                   card nil))}}}
            {:event :successful-run
             :optional
             {:req (req (and (= :rd (target-server context))
                             (some #(has-subtype? % "Stealth")
                                   (all-active state :runner))))
              :prompt "Pay 2 [Credits] to access 1 additional card?"
              :yes-ability
              {:async true
               :effect
               (effect
                 (continue-ability
                   {:eid (assoc eid :source-type :ability)
                    :async true
                    :cost [:credit 2]
                    :cost-req all-stealth
                    :msg "access 1 additional card from R&D"
                    :effect (effect (access-bonus :rd 1)
                                    (effect-completed eid))}
                   card nil))}}}]})

(defcard "Muresh Bodysuit"
  {:events [{:event :pre-damage
             :once :per-turn
             :once-key :muresh-bodysuit
             :req (req (= target :meat))
             :msg "prevent the first meat damage this turn"
             :effect (effect (damage-prevent :meat 1))}]})

(defcard "Net-Ready Eyes"
  {:on-install {:async true
                :msg "suffer 2 meat damage"
                :effect (effect (damage eid :meat 2 {:unboostable true :card card}))}
   :events [{:event :run
             :req (req (some #(and (program? %)
                                   (has-subtype? % "Icebreaker"))
                             (all-active-installed state :runner)))
             :choices {:card #(and (installed? %)
                                   (has-subtype? % "Icebreaker"))}
             :msg (msg "give " (:title target) " +1 strength")
             :effect (effect (pump target 1 :end-of-run))}]})

(defcard "NetChip"
  {:abilities [{:async true
                :label "Install a program on NetChip"
                :req (req (empty? (:hosted card)))
                :effect (effect
                          (continue-ability
                            (let [n (count (filter #(= (:title %) (:title card)) (all-active-installed state :runner)))]
                              {:async true
                               :cost [:click 1]
                               :prompt "Select a program in your Grip to install on NetChip"
                               :choices {:card #(and (program? %)
                                                     (runner-can-install? state side % false)
                                                     (<= (:memoryunits %) n)
                                                     (in-hand? %))}
                               :msg (msg "host " (:title target))
                               :effect (effect (runner-install eid target {:host-card card :no-mu true}))})
                            card nil))}
               {:async true
                :label "Host an installed program on NetChip"
                :req (req (empty? (:hosted card)))
                :effect (effect
                          (continue-ability
                            (let [n (count (filter #(= (:title %) (:title card)) (all-active-installed state :runner)))]
                              {:prompt "Select an installed program to host on NetChip"
                               :choices {:card #(and (program? %)
                                                     (<= (:memoryunits %) n)
                                                     (installed? %))}
                               :msg (msg "host " (:title target))
                               :effect (effect (host card target)
                                               (unregister-effects-for-card target #(= :used-mu (:type %)))
                                               (update-mu))})
                            card nil))}]})

(defcard "Obelus"
  {:constant-effects [(mu+ 1)
                      (runner-hand-size+ (req (count-tags state)))]
   :events [{:event :run-ends
             :once :per-turn
             :req (req (and (:successful target)
                            (#{:rd :hq} (target-server target))
                            (first-event? state side :run-ends
                                          #(and (:successful (first %))
                                                (#{:rd :hq} (target-server (first %)))))))
             :msg (msg "draw " (total-cards-accessed target) " cards")
             :async true
             :effect (effect (draw eid (total-cards-accessed target) nil))}]})

(defcard "Omni-drive"
  {:recurring 1
   :abilities [{:async true
                :label "Install and host a program of 1[mu] or less on Omni-drive"
                :req (req (empty? (:hosted card)))
                :cost [:click 1]
                :prompt "Select a program of 1[mu] or less to install on Omni-drive from your grip"
                :choices {:card #(and (program? %)
                                      (<= (:memoryunits %) 1)
                                      (in-hand? %))}
                :msg (msg "host " (:title target))
                :effect (effect (runner-install eid target {:host-card card :no-mu true}))}
               {:label "Host an installed program of 1[mu] or less on Omni-drive"
                :prompt "Select an installed program of 1[mu] or less to host on Omni-drive"
                :choices {:card #(and (program? %)
                                      (<= (:memoryunits %) 1)
                                      (installed? %))}
                :msg (msg "host " (:title target))
                :effect (effect (host card target)
                                (unregister-effects-for-card target #(= :used-mu (:type %)))
                                (update-mu))}]
   :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                               (program? target)
                                               (same-card? card (:host target))))
                                :type :recurring}}})

(defcard "Pantograph"
  (let [install-ability
        {:optional
         {:waiting-prompt "Runner to make a decision"
          :prompt "Install card with Pantograph ability?"
          :player :runner
          :yes-ability
          {:async true
           :prompt "Select a card to install with Pantograph"
           :choices
           {:req (req (and (runner? target)
                           (in-hand? target)
                           (not (event? target))
                           (can-pay? state side (assoc eid :source card :source-type :runner-install)
                                     target nil
                                     [:credit (install-cost state side target nil)])))}
           :msg (msg "install " (:title target))
           :effect (effect (runner-install
                             (assoc eid :source card :source-type :runner-install)
                             target nil))
           :cancel-effect (effect (effect-completed eid))}}}
        gain-credit-ability
        {:interactive (req true)
         :async true
         :effect (req (wait-for (resolve-ability
                                  state side
                                  {:optional
                                   {:prompt "Gain 1 [Credits] with Pantograph ability?"
                                    :yes-ability
                                    {:async true
                                     :msg "gain 1 [Credits]"
                                     :effect (req (gain-credits state :runner eid 1))}}}
                                  card nil)
                                (continue-ability state side install-ability card nil)))}]
    {:constant-effects [(mu+ 1)]
     :events [(assoc gain-credit-ability :event :agenda-scored)
              (assoc gain-credit-ability :event :agenda-stolen)]}))

(defcard "Paragon"
  {:constant-effects [(mu+ 1)]
   :events [{:event :successful-run
             :interactive (get-autoresolve :auto-fire (complement never?))
             :silent (get-autoresolve :auto-fire never?)
             :optional
             {:req (req (first-event? state side :successful-run))
              :player :runner
              :autoresolve (get-autoresolve :auto-fire)
              :waiting-prompt "Runner to make a decision"
              :prompt "Use Paragon?"
              :yes-ability
              {:msg "gain 1 [Credit] and look at the top card of Stack"
               :async true
               :effect
               (req
                 (wait-for (gain-credits state :runner 1)
                           (continue-ability
                             state :runner
                             {:player :runner
                              :optional
                              {:prompt (msg "Add " (:title (first (:deck runner))) " to bottom of Stack?")
                               :yes-ability
                               {:msg "add the top card of Stack to the bottom"
                                :effect (effect (move :runner (first (:deck runner)) :deck))}
                               :no-ability
                               {:effect (effect (system-msg "does not add the top card of the Stack to the bottom"))}}}
                             card nil)))}
              :no-ability {:effect (effect (system-msg "does not gain 1 [Credit] and look at the top card of the Stack"))}}}]
   :abilities [(set-autoresolve :auto-fire "Paragon")]})

(defcard "Patchwork"
  (let [patchwork-ability {:once :per-turn
                           :effect (effect (update! (assoc-in card [:special :patchwork] true)))}]
    {:constant-effects [(mu+ 1)]
     :interactions
     {:pay-credits
      {:req (req (and (or (= :play (:source-type eid))
                          (= :runner-install (:source-type eid)))
                      ;; We need at least one card (that is not the card played) in hand
                      (not-empty (remove (partial same-card? target) (:hand runner)))
                      ;; Patchwork wasn't used in the traditional way
                      (not (get-in card [:special :patchwork]))
                      ;; Check if Patchwork can trigger
                      (can-trigger? state side eid patchwork-ability card targets)))
       :custom-amount 2
       :custom (req (let [cost-type (str (when (= :play (:source-type eid)) "play")
                                         (when (= :runner-install (:source-type eid)) "install"))
                          patchwork card
                          targetcard target]
                      (continue-ability
                        state side
                        {:prompt (str "Trash a card to lower the " cost-type
                                      " cost of " (:title targetcard) " by 2 [Credits].")
                         :async true
                         :choices {:card #(and (in-hand? %)
                                               (runner? %)
                                               (not (same-card? % targetcard)))}
                         :msg (msg "trash " (:title target) " to lower the " cost-type " cost of "
                                   (:title targetcard) " by 2 [Credits]")
                         ; provide 2 credits
                         :effect (req (wait-for (trash state side target {:unpreventable true})
                                                (register-once state side patchwork-ability patchwork)
                                                (effect-completed state side (make-result eid 2))))
                         ; provide 0 credits
                         :cancel-effect (effect (effect-completed (make-result eid 0)))}
                        card nil)))
       :type :custom}}}))

(defcard "Pennyshaver"
  {:constant-effects [(mu+ 1)]
   :events [{:event :successful-run
             :silent (req true)
             :async true
             :msg "place 1 [Credits]"
             :effect (req (add-counter state :runner eid card :credit 1 nil))}]
   :abilities [{:cost [:click 1]
                :label "Gain 1 [Credits]. Take all hosted credits"
                :async true
                :msg (msg "gain " (inc (get-counters card :credit)) " [Credits]")
                :effect (req (let [credits (inc (get-counters card :credit))]
                               (add-counter state side card :credit (-(dec credits)))
                               (gain-credits state :runner eid credits)))}]})

(defcard "Plascrete Carapace"
  {:data {:counter {:power 4}}
   :interactions {:prevent [{:type #{:meat}
                             :req (req true)}]}
   :events [(trash-on-empty :power)]
   :abilities [{:cost [:power 1]
                :msg "prevent 1 meat damage"
                :effect (req (damage-prevent state side :meat 1))}]})

(defcard "Polyhistor"
  (let [abi {:optional
             {:prompt "Draw 1 card to force the Corp to draw 1 card?"
              :yes-ability {:msg "draw 1 card and force the Corp to draw 1 card"
                            :async true
                            :effect (req (wait-for (draw state :runner 1 nil)
                                                   (draw state :corp eid 1 nil)))}
              :no-ability {:effect (req (system-msg state side (str "does not use Polyhistor"))
                                        (effect-completed state side eid))}}}]
    {:constant-effects [(mu+ 1)
                        (link+ 1)]
     :events [{:event :pass-ice
               :req (req (and (= (:server run) [:hq])
                              (= (:position run) 1) ; trigger when last piece of ice is passed
                              (pos? (count (:deck runner)))))
               :async true
               :once :per-turn
               :effect (req (continue-ability state :runner abi card nil))}
              {:event :run
               :req (req (and (= (:server target) [:hq])
                              (zero? (:position target)) ; trigger on unprotected HQ
                              (pos? (count (:deck runner)))))
               :async true
               :once :per-turn
               :effect (req (continue-ability state :runner abi card nil))}]}))

(defcard "Prepaid VoicePAD"
  {:recurring 1
   :interactions {:pay-credits {:req (req (= :play (:source-type eid)))
                                :type :recurring}}})

(defcard "Prognostic Q-Loop"
  {:events [{:event :run
             :interactive (get-autoresolve :auto-fire (complement never?))
             :silent (get-autoresolve :auto-fire never?)
             :optional {:req (req (and (first-event? state side :run)
                                       (pos? (count (:deck runner)))))
                        :autoresolve (get-autoresolve :auto-fire)
                        :player :runner
                        :prompt "Look at top 2 cards of the stack?"
                        :yes-ability
                        {:msg "look at the top 2 cards of the stack"
                         :choices ["OK"]
                         :prompt (msg "The top two cards of your Stack are "
                                      (string/join ", " (map :title (take 2 (:deck runner))))
                                      ".")}}}]
   :abilities [(set-autoresolve :auto-fire "Prognostic Q-Loop")
               {:label "Reveal and install top card of stack"
                :once :per-turn
                :cost [:credit 1]
                :req (req (pos? (count (:deck runner))))
                :msg (msg "reveal the top card of the stack: " (:title (first (:deck runner))))
                :async true
                :effect
                (req
                  (wait-for
                    (reveal state side (first (:deck runner)))
                    (continue-ability
                      state side
                      (let [top-card (first (:deck runner))]
                        {:optional
                         {:req (req (or (program? top-card)
                                        (hardware? top-card)))
                          :prompt (msg "Install " (:title top-card) "?")
                          :yes-ability
                          {:async true
                           :effect (effect (runner-install (assoc eid :source-type :runner-install) top-card nil))}}})
                      card nil)))}]})

(defcard "Public Terminal"
  {:recurring 1
   :interactions {:pay-credits {:req (req (and (= :play (:source-type eid))
                                               (has-subtype? target "Run")))
                                :type :recurring}}})

(defcard "Q-Coherence Chip"
  {:constant-effects [(mu+ 1)]
   :events (let [e {:async true
                    :interactive (req true)
                    :req (req (and (installed? (:card target))
                                   (program? (:card target))))
                    :effect (effect (system-msg "trashes Q-Coherence Chip")
                                    (trash eid card nil))}]
             [(assoc e :event :runner-trash)
              (assoc e :event :corp-trash)])})

(defcard "Qianju PT"
  {:flags {:runner-phase-12 (req true)}
   :abilities [{:label "Lose [Click], avoid 1 tag (start of turn)"
                :once :per-turn
                :req (req (:runner-phase-12 @state))
                :effect (effect (update! (assoc card :qianju-active true)))
                :msg "lose [Click] and avoid the first tag received until their next turn"}]
   :events [{:event :corp-turn-ends
             :effect (effect (update! (dissoc card :qianju-active)))}
            {:event :runner-turn-begins
             :req (req (:qianju-active card))
             :effect (effect (lose :click 1))}
            {:event :pre-tag
             :async true
             :req (req (:qianju-active card))
             :msg "avoid the first tag received"
             :effect (effect (update! (dissoc card :qianju-active))
                             (tag-prevent :runner eid 1))}]})

(defcard "R&D Interface"
  {:in-play [:rd-access 1]})

(defcard "Rabbit Hole"
  {:constant-effects [(link+ 1)]
   :on-install
   {:optional
    {:req (req (some #(when (= (:title %) "Rabbit Hole") %) (:deck runner)))
     :prompt "Install another Rabbit Hole?"
     :msg "install another Rabbit Hole"
     :yes-ability {:async true
                   :effect (req (trigger-event state side :searched-stack nil)
                                (shuffle! state :runner :deck)
                                (when-let [c (some #(when (= (:title %) "Rabbit Hole") %)
                                                   (:deck runner))]
                                  (runner-install state side eid c nil)))}}}})

(defcard "Ramujan-reliant 550 BMI"
  {:interactions {:prevent [{:type #{:net :brain}
                             :req (req true)}]}
   :abilities [{:async true
                :label "prevent net or brain damage"
                :trash-icon true
                :req (req (not-empty (:deck runner)))
                :effect (req (let [n (count (filter #(= (:title %) (:title card)) (all-active-installed state :runner)))]
                               (continue-ability
                                 state side
                                 {:async true
                                  :prompt "Choose how much damage to prevent"
                                  :choices {:number (req (min n (count (:deck runner))))}
                                  :msg (msg "trash " (string/join ", " (map :title (take target (:deck runner))))
                                            " from their Stack and prevent " target " damage")
                                  :cost [:trash]
                                  :effect (effect (damage-prevent :net target)
                                                  (damage-prevent :brain target)
                                                  (mill :runner eid :runner target))}
                                 card nil)))}]})

(defcard "Recon Drone"
  ; eventmap uses reverse so we get the most recent event of each kind into map
  (letfn [(eventmap [s]
            (into {} (reverse (get s :turn-events))))]
    {:interactions {:prevent [{:type #{:net :brain :meat}
                               :req (req (:access @state))}]}
     :abilities [{:cost [:x-credits :trash]
                  :label "prevent damage"
                  :req (req (and (:access @state)
                                 (= (:cid (second (:pre-damage (eventmap @state))))
                                    (:cid (first (:pre-access-card (eventmap @state)))))))
                  :msg (msg "prevent " (cost-value eid :x-credits) " damage")
                  :effect (effect (damage-prevent (first (:pre-damage (eventmap @state))) (cost-value eid :x-credits)))}]}))

(defcard "Record Reconstructor"
  {:events [(successful-run-replace-access
              {:target-server :archives
               :ability
               {:prompt "Choose one faceup card to add to the top of R&D"
                :req (req (seq (filter :seen (:discard corp))))
                :choices (req (filter :seen (:discard corp)))
                :msg (msg "add " (:title target) " to the top of R&D")
                :effect (effect (move :corp target :deck {:front true}))}})]})

(defcard "Reflection"
  {:constant-effects [(mu+ 1)
                      (link+ 1)]
   :events [{:event :jack-out
             :async true
             :effect (req (let [card (first (shuffle (:hand corp)))]
                            (system-msg state :runner (str  "force the Corp to reveal " (:title card) " from HQ"))
                            (reveal state :corp eid card)))}]})

(defcard "Replicator"
  (letfn [(hardware-and-in-deck? [target runner]
            (and (hardware? target)
                 (some #(= (:title %) (:title target)) (:deck runner))))]
    {:events [{:event :runner-install
               :interactive (req (hardware-and-in-deck? (:card context) runner))
               :silent (req (not (hardware-and-in-deck? (:card context) runner)))
               :optional
               {:prompt "Use Replicator to add a copy?"
                :req (req (hardware-and-in-deck? (:card context) runner))
                :yes-ability
                {:msg (msg "add a copy of " (:title (:card context)) " to their Grip")
                 :effect (effect (trigger-event :searched-stack nil)
                           (shuffle! :deck)
                           (move (some #(when (= (:title %) (:title (:card context))) %) (:deck runner)) :hand))}}}]}))

(defcard "Respirocytes"
  (let [ability {:once :per-turn
                 :msg "draw 1 card and add a power counter"
                 :async true
                 :effect (req (wait-for (draw state :runner 1 nil)
                                        (add-counter state side (get-card state card) :power 1)
                                        (if (= 3 (get-counters (get-card state card) :power))
                                          (do (system-msg state :runner "trashes Respirocytes as it reached 3 power counters")
                                              (trash state side eid card {:unpreventable true}))
                                          (effect-completed state side eid))))}
        event {:req (req (zero? (count (:hand runner))))
               :async true
               :effect (effect (continue-ability ability card targets))}]
    {:implementation "Only watches trashes, playing events, and installing"
     :on-install {:async true
                  :msg "suffer 1 meat damage"
                  :effect (effect (damage eid :meat 1 {:unboostable true
                                                       :card card}))}
     :events [(assoc event :event :play-event)
              (assoc event
                     :event :runner-trash
                     :once-per-instance true
                     :req (req (and (some #(and (runner? (:card %))
                                                (in-hand? (:card %)))
                                          targets)
                                    (zero? (count (:hand runner))))))
              (assoc event
                     :event :corp-trash
                     :once-per-instance true
                     :req (req (and (some #(and (runner? (:card %))
                                                (in-hand? (:card %)))
                                          targets)
                                    (zero? (count (:hand runner))))))
              (assoc event
                     :event :runner-install
                     :req (req (and (some #{:hand} (:previous-zone (:card context)))
                                    (zero? (count (:hand runner))))))
              {:event :runner-turn-begins
               :req (req (empty? (:hand runner)))
               :async true
               :effect (effect (continue-ability ability card nil))}
              {:event :corp-turn-begins
               :req (req (empty? (:hand runner)))
               :async true
               :effect (effect (continue-ability ability card nil))}]
     :abilities [ability]}))

(defcard "Rubicon Switch"
  {:abilities [{:cost [:click 1]
                :label "derez ice"
                :once :per-turn
                :async true
                :prompt "How many [Credits]?"
                :choices :credit
                :effect (effect (system-msg (str "spends a [Click] and " target " [Credit] on Rubicon Switch"))
                                (continue-ability
                                  {:choices {:card #(and (ice? %)
                                                         (= :this-turn (:rezzed %))
                                                         (<= (:cost %) target))}
                                   :effect (effect (derez target))
                                   :msg (msg "derez " (:title target))}
                                  card nil))}]})

(defcard "Security Chip"
  {:abilities [{:label "Add [Link] strength to a non-Cloud icebreaker until the end of the run"
                :msg (msg "add " (get-link state)
                          " strength to " (:title target)
                          " until the end of the run")
                :req (req (:run @state))
                :prompt "Select one non-Cloud icebreaker"
                :choices {:card #(and (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "Cloud"))
                                      (installed? %))}
                :cost [:trash]
                :effect (effect (pump target (get-link state) :end-of-run))}
               {:label "Add [Link] strength to any Cloud icebreakers until the end of the run"
                :msg (msg "add " (get-link state)
                          " strength to " (count targets)
                          " Cloud icebreakers until the end of the run")
                :req (req (:run @state))
                :prompt "Select any number of Cloud icebreakers"
                :choices {:max 50
                          :card #(and (has-subtype? % "Icebreaker")
                                      (has-subtype? % "Cloud")
                                      (installed? %))}
                :cost [:trash]
                :effect (req (doseq [t targets]
                               (pump state side t (get-link state) :end-of-run)
                               (update-breaker-strength state side t)))}]})

(defcard "Security Nexus"
  {:constant-effects [(mu+ 1)
                      (link+ 1)]
   :events [{:event :encounter-ice
             :interactive (req true)
             :optional
             {:prompt "Trace 5 to bypass current ice?"
              :once :per-turn
              :yes-ability
              {:msg "force the Corp to initiate a trace"
               :trace {:base 5
                       :successful {:msg "give the Runner 1 tag and end the run"
                                    :async true
                                    :effect (req (wait-for (gain-tags state :runner 1)
                                                           (end-run state side eid card)))}
                       :unsuccessful {:msg (msg "bypass " (card-str state current-ice))
                                      :effect (req (swap! state assoc-in [:run :bypass] true))}}}}}]})

(defcard "Severnius Stim Implant"
  (letfn [(implant-fn [srv kw]
            {:prompt "Choose at least 2 cards in your Grip to trash with Severnius Stim Implant"
             :cost [:click 1]
             :choices {:max (req (count (:hand runner)))
                       :card #(and (runner? %)
                                   (in-hand? %))}
             :msg (msg "trash " (quantify (count targets) "card")
                       " and access " (quantify (quot (count targets) 2) "additional card"))
             :async true
             :effect (req (let [bonus (quot (count targets) 2)]
                            (wait-for (trash-cards state side targets {:unpreventable true})
                                      (register-events
                                        state side card
                                        [{:event :pre-access
                                          :duration :end-of-run
                                          :silent (req true)
                                          :effect (effect (access-bonus kw bonus))}])
                                      (make-run state side eid srv card))))})]
    {:abilities [{:req (req (<= 2 (count (:hand runner))))
                  :label "run a server"
                  :prompt "Choose a server to run with Severnius Stim Implant"
                  :choices ["HQ" "R&D"]
                  :async true
                  :effect (effect (continue-ability (implant-fn target (if (= target "HQ") :hq :rd)) card nil))}]}))

(defcard "Şifr"
  (letfn [(index-of [pred coll]
            (some (fn [[idx item]] (if (pred item) idx))
                  (map-indexed vector coll)))
          (gather-pre-sifr-effects [sifr state side eid target targets]
            ;; This is needed because of the stupid ass rulings about how Sifr modifies
            ;; ice strength: Sifr only lowers the ice to 0 at the point it's activated,
            ;; and then other abilities (Sandburg, etc) can raise it back after, which
            ;; is DUMB, so that means we have to on the fly calculate what the strength
            ;; of the ice is at the moment Sifr would affect it.
            (if (card-flag? target :cannot-lower-strength true)
              0
              (->> (:effects @state)
                   (filter #(= :ice-strength (:type %)))
                   (filter #(if-not (:req %)
                              true
                              ((:req %) state side eid (get-card state (:card %)) (cons target targets))))
                   (#(split-at (index-of (fn [item] (same-card? sifr (:card item))) %) %))
                   (first)
                   (mapv #(if-not (fn? (:value %))
                            (:value %)
                            ((:value %) state side eid (get-card state (:card %)) (cons target targets))))
                   (reduce +)
                   (abs))))]
    {:constant-effects [(mu+ 2)]
     :events [{:event :encounter-ice
               :optional
               {:prompt "Use Şifr?"
                :once :per-turn
                :yes-ability
                {:msg (msg "lower their maximum hand size by 1 and lower the strength of " (:title current-ice) " to 0")
                 :effect (effect
                           (register-floating-effect
                             card
                             {:type :hand-size
                              :duration :until-runner-turn-begins
                              :req (req (= :runner side))
                              :value -1})
                           (register-floating-effect
                             :runner card
                             (let [ice current-ice]
                               {:type :ice-strength
                                :duration :end-of-encounter
                                :req (req (same-card? target ice))
                                :value (req (- (+ (:strength target)
                                                  (gather-pre-sifr-effects card state side eid target (rest targets)))))})))}}}]}))

(defcard "Silencer"
  {:recurring 1
   :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                               (has-subtype? target "Killer")
                                               (has-subtype? target "Icebreaker")))
                                :type :recurring}}})

(defcard "Simulchip"
  {:constant-effects [{:type :card-ability-additional-cost
                       :req (req (and (same-card? card target)
                                      (let [pred (fn [targets]
                                                   (some #(and (runner? (:card %))
                                                               (installed? (:card %))
                                                               (program? (:card %)))
                                                         targets))]
                                        (zero? (+ (event-count state nil :runner-trash pred)
                                                  (event-count state nil :corp-trash pred)
                                                  (event-count state nil :game-trash pred))))))
                       :value [:program 1]}]
   :abilities [{:async true
                :label "Install a program from the heap"
                :req (req (and (not (install-locked? state side))
                               (not (zone-locked? state :runner :discard))
                               (some #(and (program? %)
                                           (can-pay? state side (assoc eid :source card :source-type :runner-install) % nil
                                                     [:credit (install-cost state side % {:cost-bonus -3})]))
                                     (:discard runner))))
                :cost [:trash]
                :msg "install a program"
                :effect
                (effect
                  (continue-ability
                    {:show-discard true
                     :choices {:req (req (and (in-discard? target)
                                              (program? target)
                                              (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                                        [:credit (install-cost state side target {:cost-bonus -3})])))}
                     :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target {:cost-bonus -3}))}
                    card nil))}]})

(defcard "Skulljack"
  {:on-install {:async true
                :effect (effect (damage eid :brain 1 {:card card}))}
   :constant-effects [{:type :trash-cost
                       :value -1}]})

(defcard "Spinal Modem"
  {:constant-effects [(mu+ 1)]
   :recurring 2
   :events [{:event :successful-trace
             :req (req run)
             :effect (effect (system-msg (str "suffers 1 brain damage from Spinal Modem"))
                             (damage eid :brain 1 {:card card}))}]
   :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                               (has-subtype? target "Icebreaker")))
                                :type :recurring}}})

(defcard "Sports Hopper"
  {:constant-effects [(link+ 1)]
   :abilities [{:label "Draw 3 cards"
                :msg "draw 3 cards"
                :async true
                :cost [:trash]
                :effect (effect (draw :runner eid 3 nil))}]})

(defcard "Spy Camera"
  {:abilities [{:cost [:click 1]
                :async true
                :label "Look at the top X cards of your Stack"
                :msg "look at the top X cards of their Stack and rearrange them"
                :waiting-prompt "Runner to make a decision"
                :effect (req (let [n (count (filter #(= (:title %) (:title card))
                                                    (all-active-installed state :runner)))
                                   from (take n (:deck runner))]
                               (continue-ability
                                 state side
                                 (when (pos? (count from))
                                   (reorder-choice :runner :corp from '() (count from) from))
                                 card nil)))}
               {:label "Look at the top card of R&D"
                :msg "look at the top card of R&D"
                :cost [:trash]
                :effect (effect (prompt! card (str "The top card of R&D is " (:title (first (:deck corp)))) ["OK"] {}))}]})

(defcard "Supercorridor"
  {:constant-effects [(mu+ 2)
                      (runner-hand-size+ 1)]
   :events [{:event :runner-turn-ends
             :interactive (get-autoresolve :auto-fire (complement never?))
             :silent (get-autoresolve :auto-fire never?)
             :optional
             {:req (req (= (:credit runner) (:credit corp)))
              :waiting-prompt "Runner to choose an option"
              :prompt "Gain credits from Supercorridor?"
              :player :runner
              :autoresolve (get-autoresolve :auto-fire)
              :yes-ability {:msg "gain 2 [Credits]"
                            :async true
                            :effect (effect (gain-credits eid 2))}
              :no-ability {:effect (effect (system-msg "chooses not to gain 2 [Credits] from Supercorridor"))}}}]
   :abilities [(set-autoresolve :auto-fire "Supercorridor")]})

(defcard "Swift"
  {:constant-effects [(mu+ 1)]
   :events [{:event :play-event
             :req (req (and (has-subtype? (:card context) "Run")
                            (first-event? state side :play-event #(has-subtype? (:card (first %)) "Run"))))
             :msg "gain a [click]"
             :effect (effect (gain :click 1))}]})

(defcard "T400 Memory Diamond"
  {:constant-effects [(mu+ 1)
                      {:type :hand-size
                       :req (req (= :runner side))
                       :value 1}]})

(defcard "The Gauntlet"
  {:constant-effects [(mu+ 2)]
   :events [{:event :pre-access
             :req (req (= :hq target))
             :effect (req (let [broken-ice
                                (->> (run-events state side :subroutines-broken)
                                     (filter (fn [[ice broken-subs]]
                                               (and (= :hq (second (get-zone ice)))
                                                    (all-subs-broken? ice)
                                                    (get-card state ice))))
                                     (keep #(:cid (first %)))
                                     (into #{}))
                                hq-ice
                                (->> (get-in @state (concat [:corp :servers :hq :ices]))
                                     (keep :cid)
                                     (filter broken-ice))]
                            (access-bonus state :runner :hq (count hq-ice))))}]})

(defcard "The Personal Touch"
  {:hosting {:card #(and (has-subtype? % "Icebreaker")
                         (installed? %))}
   :on-install {:effect (effect (update-breaker-strength (:host card)))}
   :constant-effects [{:type :breaker-strength
                       :req (req (same-card? target (:host card)))
                       :value 1}]})

(defcard "The Toolbox"
  {:constant-effects [(mu+ 2)
                      (link+ 2)]
   :recurring 2
   :interactions {:pay-credits {:req (req (and (= :ability (:source-type eid))
                                               (has-subtype? target "Icebreaker")))
                                :type :recurring}}})

(defcard "Titanium Ribs"
  {:on-install {:async true
                :effect (effect (enable-runner-damage-choice)
                                (system-msg (str "suffers 2 meat damage from installing Titanium Ribs"))
                                (damage eid :meat 2 {:unboostable true :card card}))}
   :leave-play (req (swap! state update :damage dissoc :damage-choose-runner))
   :events [{:event :pre-resolve-damage
             :async true
             :req (req (and (pos? (last targets))
                            (runner-can-choose-damage? state)
                            (not (get-in @state [:damage :damage-replace]))))
             :effect (req (let [dtype target
                                src (second targets)
                                dmg (last targets)
                                hand (:hand runner)]
                            (continue-ability
                              state :runner
                              (if (< (count hand) dmg)
                                {:effect (effect (chosen-damage :runner hand))}
                                {:waiting-prompt "Runner to make a decision"
                                 :prompt (msg "Select " dmg " cards to trash for the " (name dtype) " damage")
                                 :choices {:max dmg
                                           :all true
                                           :card #(and (in-hand? %)
                                                       (runner? %))}
                                 :msg (msg "trash " (string/join ", " (map :title targets)))
                                 :effect (effect (chosen-damage :runner targets))})
                              card nil)))}]})

(defcard "Top Hat"
  {:events [(successful-run-replace-access
              {:target-server :rd
               :ability {:req (req (and (not= (:max-access run) 0)
                                        (pos? (count (:deck corp)))))
                         :prompt "Which card from the top of R&D would you like to access? (Card 1 is on top.)"
                         :choices (req (map str (take (count (:deck corp)) (range 1 6))))
                         :msg (msg "only access the card at position " target " of R&D")
                         :async true
                         :effect (req (if (get-only-card-to-access state)
                                        (effect-completed state nil eid)
                                        (access-card state side eid (nth (:deck corp) (dec (str->int target))) "an unseen card")))}})]})

(defcard "Turntable"
  {:constant-effects [(mu+ 1)]
   :events [{:event :agenda-stolen
             :interactive (req true)
             :req (req (not (empty? (:scored corp))))
             :async true
             :effect (effect
                       (continue-ability
                         (let [stolen (:card context)]
                           {:optional
                            {:prompt (msg "Swap " (:title stolen) " for an agenda in the Corp's score area?")
                             :yes-ability
                             {:prompt (str "Select a scored Corp agenda to swap with " (:title stolen))
                              :choices {:card #(in-corp-scored? state side %)}
                              :msg (msg "swap " (:title stolen) " for " (:title target))
                              :effect (effect (swap-agendas target stolen))}}})
                         card targets))}]})

(defcard "Ubax"
  (let [ability {:req (req (:runner-phase-12 @state))
                 :msg "draw 1 card"
                 :label "Draw 1 card (start of turn)"
                 :once :per-turn
                 :async true
                 :effect (effect (draw eid 1 nil))}]
    {:constant-effects [(mu+ 1)]
     :flags {:runner-turn-draw true
             :runner-phase-12 (req (< 1 (count (filter #(card-flag? % :runner-turn-draw true)
                                                       (cons (get-in @state [:runner :identity])
                                                             (all-active-installed state :runner))))))}
     :events [(assoc ability :event :runner-turn-begins)]
     :abilities [ability]}))

(defcard "Unregistered S&W '35"
  {:abilities
   [{:cost [:click 2]
     :req (req (and (some #{:hq} (:successful-run runner-reg))
                    (seq (filter
                           #(and (rezzed? %)
                                 (installed? %)
                                 (or (has-subtype? % "Bioroid")
                                     (has-subtype? % "Clone")
                                     (has-subtype? % "Executive")
                                     (has-subtype? % "Sysop")))
                           (all-active-installed state :corp)))))
     :label "trash a Bioroid, Clone, Executive or Sysop"
     :prompt "Select a Bioroid, Clone, Executive, or Sysop to trash"
     :choices {:card #(and (rezzed? %)
                           (installed? %)
                           (or (has-subtype? % "Bioroid")
                               (has-subtype? % "Clone")
                               (has-subtype? % "Executive")
                               (has-subtype? % "Sysop")))}
     :async true
     :msg (msg "trash " (:title target))
     :effect (effect (trash eid target nil))}]})

(defcard "Vigil"
  (let [ability {:req (req (and (:runner-phase-12 @state)
                                (= (count (:hand corp)) (hand-size state :corp))))
                 :msg "draw 1 card"
                 :label "Draw 1 card (start of turn)"
                 :once :per-turn
                 :async true
                 :effect (effect (draw eid 1 nil))}]
    {:constant-effects [(mu+ 1)]
     :events [(assoc ability :event :runner-turn-begins)]
     :abilities [ability]}))

(defcard "Window"
  {:abilities [{:cost [:click 1]
                :keep-open :while-clicks-left
                :msg "draw 1 card from the bottom of their Stack"
                :effect (effect (move (last (:deck runner)) :hand))}]})

(defcard "Zamba"
  {:implementation "Credit gain is automatic"
   :constant-effects [(mu+ 2)]
   :events [{:event :expose
             :async true
             :effect (effect (gain-credits :runner eid 1))
             :msg "gain 1 [Credits]"}]})

(defcard "Zer0"
  {:abilities [{:cost [:click 1 :net 1]
                :once :per-turn
                :msg "gain 1 [Credits] and draw 2 cards"
                :async true
                :effect (req (wait-for (gain-credits state side 1)
                                       (draw state side eid 2 nil)))}]})
