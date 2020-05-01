(ns game.cards.assets
  (:require [game.core :refer :all]
            [game.core.card :refer :all]
            [game.core.card-defs :refer [define-card]]
            [game.core.effects :refer [register-floating-effect]]
            [game.core.eid :refer [effect-completed]]
            [game.core.card-defs :refer [card-def]]
            [game.core.prompts :refer [show-wait-prompt clear-wait-prompt]]
            [game.core.toasts :refer [toast]]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability]]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [clojure.stacktrace :refer [print-stack-trace]]
            [jinteki.utils :refer :all]))

;;; Asset-specific helpers
(defn installed-access-trigger
  "Effect for triggering ambush on access.
  Ability is what happends upon access. If cost is specified Corp needs to pay that to trigger."
  ([cost ability]
   (let [ab (if (pos? cost) (assoc ability :cost [:credit cost]) ability)
         prompt (if (pos? cost)
                  (req (str "Pay " cost " [Credits] to use " (:title card) " ability?"))
                  (req (str "Use " (:title card) " ability?")))]
     (installed-access-trigger cost ab prompt)))
  ([cost ability prompt]
   {:access {:req (req (and installed (>= (:credit corp) cost)))
             :async true
             :effect (effect (show-wait-prompt :runner (str "Corp to use " (:title card)))
                             (continue-ability
                               {:optional
                                {:prompt prompt
                                 :yes-ability ability
                                 :end-effect (effect (clear-wait-prompt :runner))}}
                               card nil))}}))

(defn advance-ambush
  "Creates advanceable ambush structure with specified ability for specified cost"
  ([cost ability] (assoc (installed-access-trigger cost ability) :advanceable :always))
  ([cost ability prompt] (assoc (installed-access-trigger cost ability prompt) :advanceable :always)))

(defn campaign
  "Creates a Campaign with X counters draining Y per-turn.
  Trashes itself when out of counters"
  ([counters per-turn] (campaign counters per-turn :credit))
  ([counters per-turn counter-type]
   (let [num-counters (fn [card] (min per-turn (get-counters card counter-type)))
         ability {:msg (msg "gain " (num-counters card) " [Credits]")
                  :once :per-turn
                  :req (req (:corp-phase-12 @state))
                  :label (str "Gain " per-turn " [Credits] (start of turn)")
                  :async true
                  :effect (effect (gain-credits (num-counters card))
                                  (add-counter eid card counter-type (- (num-counters card)) nil))}]
     {:effect (req (add-counter state side card counter-type counters))
      :derezzed-events [corp-rez-toast]
      :events [(trash-on-empty counter-type)
               (assoc ability :event :corp-turn-begins)]
      :abilities [ability]})))

(def executive-trash-effect
  {:when-inactive true
   :req (req (and (= side :runner)
                  (same-card? target (:access @state))))
   :msg "add it to the Runner's score area as an agenda worth 2 agenda points"
   :async true
   :effect (req (as-agenda state :runner eid card 2))})

;; Card definitions

(define-card "Adonis Campaign"
  (campaign 12 3))

(define-card "Advanced Assembly Lines"
  {:effect (effect (gain-credits 3))
   :msg (msg "gain 3 [Credits]")
   :abilities [{:label "Install a non-agenda card from HQ"
                :async true
                :prompt "Select a non-agenda card to install from HQ"
                :req (req (not (:run @state)))
                :choices {:card #(and (not (operation? %))
                                      (not (agenda? %))
                                      (in-hand? %)
                                      (corp? %))}
                :msg (msg (corp-install-msg target))
                :cost [:trash]
                :effect (effect (corp-install eid target nil nil))}]})

(define-card "Aggressive Secretary"
  (advance-ambush 2 {:req (req (pos? (get-counters (get-card state card) :advancement)))
                     :prompt (msg "Choose " (quantify (get-counters (get-card state card) :advancement) "program") " to trash")
                     :cost [:credit 2]
                     :choices {:max (req (get-counters (get-card state card) :advancement))
                               :card #(and (installed? %)
                                           (program? %))}
                     :msg (msg "trash " (join ", " (map :title targets)))
                     :async true
                     :effect (effect (trash-cards eid targets))}))

(define-card "Alexa Belsky"
  {:abilities [{:label "Shuffle all cards in HQ into R&D"
                :cost [:trash]
                :effect (effect (show-wait-prompt :corp "Runner to decide whether or not to prevent Alexa Belsky")
                                (resolve-ability
                                  {:prompt "Prevent Alexa Belsky from shuffling back in 1 card for every 2 [Credits] spent. How many credits?"
                                   :choices :credit
                                   :player :runner
                                   :msg (msg "shuffle "
                                             (quantify (- (count (:hand corp)) (quot target 2)) "card")
                                             " in HQ into R&D")
                                   :effect (req (if (pos? (quot target 2))
                                                  (let [prevented (quot target 2)
                                                        unprevented (- (count (:hand corp)) prevented)]
                                                    (doseq [c (take unprevented (shuffle (:hand corp)))]
                                                      (move state :corp c :deck))
                                                    (when (pos? unprevented)
                                                      (shuffle! state :corp :deck))
                                                    (system-msg state :runner
                                                                (str "pays " target " [Credits] to prevent "
                                                                     (quantify prevented "random card")
                                                                     " in HQ from being shuffled into R&D")))
                                                  (shuffle-into-deck state :corp :hand)))
                                   :end-effect (effect (clear-wait-prompt :corp))}
                                  card nil))}]})

(define-card "Alix T4LB07"
  {:events [{:event :corp-install
             :effect (effect (add-counter card :power 1))}]
   :abilities [{:label "Gain 2 [Credits] for each counter on Alix T4LB07"
                :cost [:click 1 :trash]
                :msg (msg "gain " (* 2 (get-counters card :power)) " [Credits]")
                :effect (effect (gain-credits (* 2 (get-counters card :power))))}]})

(define-card "Allele Repression"
  {:implementation "Card swapping is manual"
   :advanceable :always
   :abilities [{:label "Swap 1 card in HQ and Archives for each advancement token"
                :cost [:trash]
                :msg (msg "swap " (get-counters card :advancement) " cards in HQ and Archives")}]})

(define-card "Amani Senai"
  (letfn [(senai-ability [agenda]
            {:interactive (req true)
             :optional {:prompt "Trace with Amani Senai?"
                        :player :corp
                        :autoresolve (get-autoresolve :auto-fire)
                        :yes-ability {:trace {:base (effect (advancement-cost agenda))
                                              :successful
                                              {:choices {:card #(and (installed? %)
                                                                     (runner? %))}
                                               :label "add an installed card to the Grip"
                                               :msg (msg "add " (:title target) " to the Runner's Grip")
                                               :effect (effect (move :runner target :hand))}}}}})]
    {:events [{:event :agenda-scored
               :interactive (req true)
               :async true
               :effect (effect (continue-ability (senai-ability target) card nil))}
              {:event :agenda-stolen
               :interactive (req true)
               :async true
               :effect (effect (continue-ability (senai-ability target) card nil))}]
     :abilities [(set-autoresolve :auto-fire "whether to fire Amani Senai")]}))

(define-card "Anson Rose"
  (let [ability {:label "Place 1 advancement token on Anson Rose (start of turn)"
                 :once :per-turn
                 :effect (effect (system-msg (str "places 1 advancement counter on Anson Rose"))
                                 (add-prop card :advance-counter 1 {:placed true}))}]
    {:derezzed-events [corp-rez-toast]
     :flags {:corp-phase-12 (req true)}
     :events [(assoc ability :event :corp-turn-begins)
              {:event :rez
               :req (req (and (ice? target)
                              (pos? (get-counters card :advancement))))
               :async true
               :effect (req (let [ice (get-card state target)
                                  icename (:title ice)]
                              (show-wait-prompt state :runner "Corp to use Anson Rose")
                              (continue-ability
                                state side
                                {:optional
                                 {:prompt (msg "Move advancement tokens from Anson Rose to " icename "?")
                                  :yes-ability
                                  {:prompt "Choose how many advancement tokens to remove from Anson Rose"
                                   :choices {:number (req (get-counters card :advancement))}
                                   :effect (effect (add-prop :corp ice :advance-counter target {:placed true})
                                                   (add-prop :corp card :advance-counter (- target) {:placed true})
                                                   (system-msg (str "uses Anson Rose to move " target
                                                                    " advancement tokens to " (card-str state ice))))}
                                  :end-effect (effect (clear-wait-prompt :runner))}}
                                card nil)))}]
     :abilities [ability]}))

(define-card "API-S Keeper Isobel"
  (letfn [(counters-available? [state] (some #(pos? (get-counters % :advancement)) (all-installed state :corp)))]
    {:flags {:corp-phase-12 (req (counters-available? state))}
     :abilities [{:req (req (and (:corp-phase-12 @state)
                                 (counters-available? state)))
                  :once :per-turn
                  :label "Remove an advancement token (start of turn)"
                  :prompt "Select a card to remove an advancement token from"
                  :choices {:card #(and (pos? (get-counters % :advancement))
                                     (installed? %))}
                  :effect (req (let [cnt (get-counters target :advancement)]
                                 (set-prop state side target :advance-counter (dec cnt))
                                 (gain-credits state :corp 3)
                                 (system-msg state :corp (str "uses API-S Keeper Isobel to remove an advancement token from "
                                                              (card-str state target) " and gains 3 [Credits]"))))}]}))

(define-card "Aryabhata Tech"
  {:events [{:event :successful-trace
             :msg "gain 1 [Credit] and force the Runner to lose 1 [Credit]"
             :effect (effect (gain-credits 1)
                             (lose-credits :runner 1))}]})

(define-card "Bass CH1R180G4"
  {:abilities [{:cost [:click 1 :trash]
                :msg "gain [Click][Click]"
                :effect (effect (gain :click 2))}]})

(define-card "Bio-Ethics Association"
  (let [ability {:req (req unprotected)
                 :async true
                 :label "Do 1 net damage (start of turn)"
                 :once :per-turn
                 :msg "do 1 net damage"
                 :effect (effect (damage eid :net 1 {:card card}))}]
    {:derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(define-card "Bioroid Work Crew"
  {:implementation "Timing restriction of ability use not enforced"
   :abilities [{:label "Install 1 card, paying all costs"
                :req (req (= (:active-player @state) :corp))
                :prompt "Select a card in HQ to install"
                :choices {:card #(and (not (operation? %))
                                      (in-hand? %)
                                      (corp? %))}
                :cost [:trash]
                :async true
                :effect (effect (corp-install eid target nil nil))
                :msg (msg (corp-install-msg target))}]})

(define-card "Blacklist"
  {:effect (effect (lock-zone (:cid card) :runner :discard))
   :leave-play (effect (release-zone (:cid card) :runner :discard))})

(define-card "Brain-Taping Warehouse"
  {:constant-effects [{:type :rez-cost
                       :req (req (and (ice? target)
                                      (has-subtype? target "Bioroid")))
                       :value (req (- (:click runner)))}]})

(define-card "Breached Dome"
  {:flags {:rd-reveal (req true)}
   :access {:async true
            :effect (req (let [c (first (get-in @state [:runner :deck]))]
                           (system-msg state :corp (str "uses Breached Dome to do one meat damage and to trash " (:title c)
                                                        " from the top of the Runner's Stack"))
                           (wait-for (mill state :corp :runner 1)
                                     (damage state side eid :meat 1 {:card card}))))}})

(define-card "Broadcast Square"
  {:events [{:event :pre-bad-publicity
             :async true
             :trace {:base 3
                     :successful {:msg "prevents all bad publicity"
                                  :effect (effect (bad-publicity-prevent Integer/MAX_VALUE))}}}]})

(define-card "Calvin B4L3Y"
  {:abilities [{:cost [:click 1]
                :msg "draw 2 cards"
                :once :per-turn
                :async true
                :effect (effect (draw eid 2 nil))}]
   :trash-effect {:async true
                  :interactive (req true)
                  :req (req (= :servers (first (:previous-zone card))))
                  :effect (effect (show-wait-prompt :runner "Corp to use Calvin B4L3Y")
                                  (continue-ability :corp
                                                    {:optional
                                                     {:prompt "Draw 2 cards?"
                                                      :player :corp
                                                      :yes-ability {:msg "draw 2 cards"
                                                                    :effect (effect (draw eid 2 nil))}
                                                      :end-effect (effect (clear-wait-prompt :runner))}}
                                                    card nil))}})

(define-card "C.I. Fund"
  {:derezzed-events [corp-rez-toast]
   :flags {:corp-phase-12 (req (pos? (:credit corp)))}
   :abilities [{:label "Move up to 3 [Credit] from credit pool to C.I. Fund"
                :prompt "Choose how many [Credit] to move"
                :once :per-turn
                :choices {:number (req (min (:credit corp) 3))}
                :effect (effect (lose-credits target)
                                (add-counter card :credit target))
                :msg (msg "move " target " [Credit] to C.I. Fund")}
               {:label "Take all credits from C.I. Fund"
                :cost [:credit 2 :trash]
                :msg (msg "trash it and gain " (get-counters card :credit) " [Credits]")
                :effect (effect (gain-credits (get-counters card :credit)))}]
   :events [{:event :corp-turn-begins
             :req (req (>= (get-counters card :credit) 6))
             :effect (effect (add-counter card :credit 2)
                             (system-msg (str "adds 2 [Credits] to C.I. Fund")))}]})

(define-card "Capital Investors"
  {:abilities [{:cost [:click 1]
                :msg "gain 2 [Credits]"
                :effect (effect (gain-credits 2))}]})

(define-card "Cerebral Overwriter"
  (advance-ambush 3 {:async true
                     :req (req (pos? (get-counters (get-card state card) :advancement)))
                     :msg (msg "do " (get-counters (get-card state card) :advancement) " brain damage")
                     :effect (effect (damage eid :brain (get-counters (get-card state card) :advancement) {:card card}))}))

(define-card "Chairman Hiro"
  {:effect (effect (lose :runner :hand-size 2))
   :leave-play (effect (gain :runner :hand-size 2))
   :trash-effect executive-trash-effect})

(define-card "Chief Slee"
  {:events [{:event :encounter-ice-ends
             :msg "add 1 power counter to Chief Slee"
             :effect (effect (add-counter :corp card :power (count (remove :broken (:subroutines target)))))}]
   :abilities [{:cost [:click 1 :power 5]
                :async true
                :msg "do 5 meat damage"
                :effect (effect (damage eid :meat 5 {:card card}))}]})

(define-card "City Surveillance"
  {:derezzed-events [corp-rez-toast]
   :flags {:runner-phase-12 (req (pos? (:credit runner)))}
   :events [{:event :runner-turn-begins
             :player :runner
             :prompt "Pay 1 [Credits] or take 1 tag"
             :choices (req (concat (when (pos? (:credit runner))
                                     ["Pay 1 [Credits]"])
                                   ["Take 1 tag"]))
             :msg "make the Runner pay 1 [Credits] or take 1 tag"
             :async true
             :effect (req (case target
                            "Pay 1 [Credits]"
                            (do (system-msg state :runner "pays 1 [Credits]")
                                (pay state :runner card :credit 1)
                                (effect-completed state side eid))
                            (do (system-msg state :runner "takes 1 tag")
                                (gain-tags state :corp eid 1))))}]})

(define-card "Clone Suffrage Movement"
  {:derezzed-events [corp-rez-toast]
   :flags {:corp-phase-12 (req (and (some operation? (:discard corp))
                                    unprotected))}
   :abilities [{:label "Add 1 operation from Archives to HQ"
                :effect (effect (show-wait-prompt :runner "Corp to use Clone Suffrage Movement")
                                (continue-ability
                                  {:prompt "Select an operation in Archives to add to HQ"
                                   :once :per-turn
                                   :show-discard true
                                   :choices {:card #(and (operation? %)
                                                         (in-discard? %))}
                                   :msg (msg "add "
                                             (if (:seen target)
                                               (:title target)
                                               "a facedown card")
                                             " to HQ")
                                   :effect (effect (move target :hand))
                                   :end-effect (effect (clear-wait-prompt :runner))}
                                  card nil))}]})

(define-card "Clyde Van Rite"
  (let [ability {:async true
                 :req (req (or (pos? (:credit runner))
                               (pos? (count (:deck runner)))))
                 :player :runner
                 :once :per-turn
                 :prompt "Pay 1 [Credits] or trash the top card of the Stack"
                 :choices (req (concat (when (pos? (:credit runner))
                                         ["Pay 1 [Credits]"])
                                       (when (pos? (count (:deck runner)))
                                         ["Trash top card"])))
                 :msg "make the Runner pay 1 [Credits] or trash the top card of the Stack"
                 :effect (req (case target
                                "Pay 1 [Credits]"
                                (do (system-msg state side "pays 1 [Credits]")
                                    (pay-sync state side eid card :credit 1))
                                "Trash top card"
                                (do (system-msg state side "trashes the top card of the Stack")
                                    (mill state :runner eid :runner 1))))}]
    {:derezzed-events [corp-rez-toast]
     :flags {:corp-phase-12 (req true)}
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(define-card "Commercial Bankers Group"
  (let [ability {:req (req unprotected)
                 :label "Gain 3 [Credits] (start of turn)"
                 :once :per-turn
                 :msg "gain 3 [Credits]"
                 :effect (effect (gain-credits 3))}]
    {:derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(define-card "Constellation Protocol"
  {:derezzed-events [corp-rez-toast]
   :flags {:corp-phase-12
           (req (let [a-token (->> (all-installed state :corp)
                                   (filter ice?)
                                   (filter #(pos? (get-counters % :advancement)))
                                   (remove empty?)
                                   first
                                   :title)]
                  (as-> (all-installed state :corp) it
                    (filter ice? it)
                    (filter can-be-advanced? it)
                    (remove empty? it)
                    (map :title it)
                    (split-with (partial not= a-token) it)
                    (concat (first it) (-> it rest first rest))
                    (count it)
                    (pos? it))))}
   :abilities [{:label "Move an advancement counter between ICE"
                :once :per-turn
                :effect (req (show-wait-prompt state :runner "Corp to use Constellation Protocol")
                             (continue-ability
                               state side
                               {:choices {:card #(and (ice? %)
                                                      (get-counters % :advancement))}
                                :effect (req (let [from-ice target]
                                               (continue-ability
                                                 state side
                                                 {:prompt "Move to where?"
                                                  :choices {:card #(and (ice? %)
                                                                        (not (same-card? from-ice %))
                                                                        (can-be-advanced? %))}
                                                  :msg (msg "move an advancement token from "
                                                            (card-str state from-ice)
                                                            " to "
                                                            (card-str state target))
                                                  :effect (effect (add-prop :corp target :advance-counter 1)
                                                                  (add-prop :corp from-ice :advance-counter -1)
                                                                  (clear-wait-prompt :runner))}
                                                 card nil)))}
                               card nil))}]})

(define-card "Contract Killer"
  {:advanceable :always
   :abilities [{:label "Trash a connection"
                :async true
                :cost [:click 1 :trash]
                :req (req (>= (get-counters card :advancement) 2))
                :choices {:card #(has-subtype? % "Connection")}
                :msg (msg "trash " (:title target))
                :effect (effect (trash eid target nil))}
               {:label "Do 2 meat damage"
                :async true
                :cost [:click 1 :trash]
                :req (req (>= (get-counters card :advancement) 2))
                :msg "do 2 meat damage"
                :effect (effect (damage eid :meat 2 {:card card}))}]})

(define-card "Corporate Town"
  {:derezzed-events [corp-rez-toast]
   :additional-cost [:forfeit]
   :flags {:corp-phase-12 (req (and (rezzed? card)
                                    (->> (all-active-installed state :runner)
                                         (filter resource?)
                                         count
                                         pos?)))}
   :abilities [{:label "Trash a resource"
                :once :per-turn
                :async true
                :prompt "Select a resource to trash with Corporate Town"
                :choices {:card resource?}
                :msg (msg "trash " (:title target))
                :effect (effect (trash eid target {:unpreventable true}))}]})

(define-card "CPC Generator"
  {:events [{:event :runner-click-credit
             :req (req (first-event? state side :runner-click-credit))
             :msg "gain 1 [Credits]"
             :effect (effect (gain-credits :corp 1))}]})

(define-card "CSR Campaign"
  (let [ability {:once :per-turn
                 :async true
                 :label "Draw 1 card (start of turn)"
                 :interactive (req true)
                 :effect (effect (continue-ability
                                   {:optional
                                    {:prompt "Use CSR Campaign to draw 1 card?"
                                     :yes-ability {:async true
                                                   :msg "draw 1 card"
                                                   :effect (effect (draw eid 1 nil))}}}
                                   card nil))}]
    {:derezzed-events [corp-rez-toast]
     :flags {:corp-phase-12 (req true)}
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(define-card "Cybernetics Court"
  {:in-play [:hand-size 4]})

(define-card "Daily Business Show"
  {:events [{:event :pre-corp-draw
             :msg "draw additional cards"
             ;; The req catches draw events that happened before DBS was rezzed.
             :req (req (first-event? state :corp :pre-corp-draw))
             ;; The once and once-key force a single DBS to act on behalf of all rezzed DBS's.
             :once :per-turn
             :once-key :daily-business-show-draw-bonus
             :effect (req (let [dbs (count (filter #(and (= "Daily Business Show" (:title %))
                                                         (rezzed? %))
                                                   (all-installed state :corp)))]
                            (draw-bonus state side dbs)))}
            {:event :post-corp-draw
             :req (req (first-event? state :corp :post-corp-draw))
             :once :per-turn
             :once-key :daily-business-show-put-bottom
             :async true
             :effect (req (let [dbs (count (filter #(and (= "Daily Business Show" (:title %))
                                                         (rezzed? %))
                                                   (all-installed state :corp)))
                                drawn (get-in @state [:corp :register :most-recent-drawn])]
                            (show-wait-prompt state :runner "Corp to use Daily Business Show")
                            (wait-for (resolve-ability
                                        state side
                                        {:prompt (str "Select " (quantify dbs "card") " to add to the bottom of R&D")
                                         :msg (msg "add " (quantify dbs "card") " to the bottom of R&D")
                                         :choices {:max dbs
                                                   :card #(some (fn [c] (same-card? c %)) drawn)
                                                   :all true}
                                         :effect (req (doseq [c targets]
                                                        (move state side c :deck)))}
                                        card targets)
                                      (clear-wait-prompt state :runner)
                                      (effect-completed state side eid))))}]})

(define-card "Daily Quest"
  (let [ability {:req (req (let [servers (get-in @state [:runner :register-last-turn :successful-run])]
                             (not (some (into #{}
                                              (list (second (:zone card))
                                                    (second (:zone (:host card)))))
                                        servers))))
                 :msg "gain 3 [Credits]"
                 :effect (effect (gain-credits :corp 3))}]
    {:rez-req (req (= (:active-player @state) :corp))
     :events [{:event :successful-run
               :req (req this-server)
               :effect (effect (gain-credits :runner 2)
                               (system-msg :runner (str "gains 2 [Credits] for a successful run "
                                                        "on the Daily Quest server")))}
              (assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(define-card "Dedicated Response Team"
  {:events [{:event :run-ends
             :req (req (and tagged (:successful target)))
             :msg "do 2 meat damage"
             :async true
             :effect (effect (damage eid :meat 2 {:card card}))}]})

(define-card "Dedicated Server"
  {:recurring 2
   :interactions {:pay-credits {:req (req (and (= :rez (:source-type eid))
                                               (ice? target)))
                                :type :recurring}}})

(define-card "Director Haas"
  {:in-play [:click-per-turn 1]
   :trash-effect executive-trash-effect})

(define-card "Docklands Crackdown"
  {:abilities [{:cost [:click 2]
                :msg "add 1 power counter"
                :effect (effect (add-counter card :power 1))}]
   :constant-effects [{:type :install-cost
                       :req (req (and (pos? (get-counters card :power))
                                      (not (get-in @state [:per-turn (:cid card)]))))
                       :value (req (get-counters card :power))}]
   :events [{:event :runner-install
             :silent (req true)
             :req (req (and (pos? (get-counters card :power))
                            (not (get-in @state [:per-turn (:cid card)]))))
             :msg (msg "increase the install cost of " (:title target) " by " (get-counters card :power) " [Credits]")
             :effect (req (swap! state assoc-in [:per-turn (:cid card)] true))}]})

(define-card "Drudge Work"
  {:effect (effect (add-counter card :power 3))
   :events [(trash-on-empty :power)]
   :abilities [{:cost [:click 1 :power 1]
                :choices {:card #(and (agenda? %)
                                      (or (in-hand? %)
                                          (in-discard? %)))}
                :label "Reveal an agenda from HQ or Archives"
                :msg (msg "reveal " (:title target) " from " (zone->name (:zone target))
                          (let [target-agenda-points (get-agenda-points state :corp target)]
                            (str ", gain " target-agenda-points " [Credits], "))
                          " and shuffle it into R&D")
                :effect (req (reveal state side target)
                             (gain-credits state :corp (get-agenda-points state :corp target))
                             (move state :corp target :deck)
                             (shuffle! state :corp :deck))}]})

(define-card "Early Premiere"
  {:derezzed-events [corp-rez-toast]
   :flags {:corp-phase-12 (req (some #(and (can-be-advanced? %)
                                           (in-server? %))
                                     (all-installed state :corp)))}
   :abilities [{:cost [:credit 1]
                :label "Place 1 advancement token on a card that can be advanced in a server"
                :choices {:card #(and (can-be-advanced? %)
                                      (installed? %)
                                      (in-server? %))} ; should be *in* a server
                :once :per-turn
                :msg (msg "place 1 advancement token on " (card-str state target))
                :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]})

(define-card "Echo Chamber"
  {:abilities [{:label "Add Echo Chamber to your score area as an agenda worth 1 agenda point"
                :cost [:click 3]
                :msg "add it to their score area as an agenda worth 1 agenda point"
                :async true
                :effect (req (as-agenda state :corp eid card 1))}]})

(define-card "Edge of World"
  (letfn [(ice-count [state]
            (count (get-in (:corp @state) [:servers (last (:server (:run @state))) :ices])))]
    (installed-access-trigger 3 {:msg (msg "do " (ice-count state) " brain damage")
                                 :async true
                                 :effect (effect (damage eid :brain (ice-count state)
                                                         {:card card}))})))

(define-card "Eliza's Toybox"
  {:abilities [{:cost [:click 3]
                :choices {:card #(not (:rezzed %))}
                :label "Rez a card at no cost" :msg (msg "rez " (:title target) " at no cost")
                :effect (effect (rez target {:ignore-cost :all-costs}))}]})

(define-card "Elizabeth Mills"
  {:effect (effect (lose-bad-publicity 1))
   :msg "remove 1 bad publicity"
   :abilities [{:cost [:click 1 :trash]
                :label "Trash a location"
                :msg (msg "trash " (:title target) " and take 1 bad publicity")
                :choices {:card #(has-subtype? % "Location")}
                :async true
                :effect (req (wait-for (trash state side target nil)
                                       (gain-bad-publicity state :corp eid 1)))}]})

(define-card "Encryption Protocol"
  {:constant-effects [{:type :trash-cost
                       :req (req (installed? target))
                       :value 1}]})

(define-card "Estelle Moon"
  {:events [{:event :corp-install
             :req (req (and (or (asset? target) (agenda? target) (upgrade? target))
                            (is-remote? (second (:zone target)))))
             :effect (effect (add-counter card :power 1)
                             (system-msg (str "places 1 power counter on Estelle Moon")))}]
   :abilities [{:label "Draw 1 card and gain 2 [Credits] for each power counter"
                :cost [:trash]
                :effect (req (let [counters (get-counters card :power)
                                   credits (* 2 counters)]
                               (draw state side counters)
                               (gain-credits state side credits)
                               (system-msg state side (str "uses Estelle Moon to draw " counters
                                                           " cards and gain " credits " [Credits]"))))}]})

(define-card "Eve Campaign"
  (campaign 16 2))

(define-card "Executive Boot Camp"
  {:derezzed-events [corp-rez-toast]
   :flags {:corp-phase-12 (req (some #(not (rezzed? %)) (all-installed state :corp)))}
   ; A card rezzed by Executive Bootcamp is ineligible to receive the turn-begins event for this turn.
   :suppress [{:event :corp-turn-begins
               :req (req (= (:cid target) (:ebc-rezzed (get-card state card))))}]
   :events [{:event :corp-turn-ends
             :req (req (:ebc-rezzed card))
             :effect (effect (update! (dissoc card :ebc-rezzed)))}]
   :abilities [{:async true
                :once :per-turn
                :choices {:card (complement rezzed?)}
                :label "Rez a card, lowering the cost by 1 [Credits]"
                :msg (msg "rez " (:title target))
                :effect (req (wait-for (rez state side target {:no-warning true :cost-bonus -1})
                                       (update! state side (assoc card :ebc-rezzed (:cid target)))))}
               {:prompt "Choose an asset to reveal and add to HQ"
                :msg (msg "reveal " (:title target) " and add it to HQ")
                :activatemsg "searches R&D for an asset"
                :choices (req (cancellable (filter asset?
                                                   (:deck corp))
                                           :sorted))
                :cost [:credit 1 :trash]
                :label "Search R&D for an asset"
                :effect (effect (reveal target)
                                (shuffle! :deck)
                                (move target :hand))}]})

(define-card "Executive Search Firm"
  {:abilities [{:prompt "Choose an Executive, Sysop, or Character to add to HQ"
                :msg (msg "add " (:title target) " to HQ and shuffle R&D")
                :activatemsg "searches R&D for an Executive, Sysop, or Character"
                :choices (req (cancellable (filter #(or (has-subtype? % "Executive")
                                                        (has-subtype? % "Sysop")
                                                        (has-subtype? % "Character"))
                                                   (:deck corp))
                                           :sorted))
                :cost [:click 1]
                :label "Search R&D for an Executive, Sysop, or Character"
                :effect (effect (move target :hand)
                                (shuffle! :deck))}]})

(define-card "Exposé"
  {:advanceable :always
   :abilities [{:label "Remove 1 bad publicity for each advancement token on Exposé"
                :msg (msg "remove " (get-counters card :advancement) " bad publicity")
                :cost [:trash]
                :effect (effect (lose-bad-publicity (get-counters card :advancement)))}]})

(define-card "False Flag"
  (letfn [(tag-count [false-flag]
            (int (/ (get-counters false-flag :advancement) 2)))]
    {:advanceable :always
     :access {:req (req (pos? (get-counters (get-card state card) :advancement)))
              :msg (msg "give the runner " (quantify (tag-count (get-card state card)) "tag"))
              :async true
              :effect (effect (gain-tags :corp eid (tag-count (get-card state card))))}
     :abilities [{:cost [:click 1 :advancement 7]
                  :label "Add False Flag to your score area as an agenda worth 3 agenda points"
                  :msg "add it to their score area as an agenda worth 3 agenda points"
                  :async true
                  :effect (req (as-agenda state :corp eid card 3))}]}))

(define-card "Franchise City"
  {:events [{:event :access
             :req (req (agenda? target))
             :msg "add it to their score area as an agenda worth 1 agenda point"
             :async true
             :effect (req (as-agenda state :corp eid card 1))}]})

(define-card "Full Immersion RecStudio"
  {:can-host (req (and (or (asset? target) (agenda? target))
                       (> 2 (count (:hosted card)))))
   :trash-cost-bonus (req (* 3 (count (:hosted card))))
   :abilities [{:label "Install an asset or agenda on Full Immersion RecStudio"
                :req (req (< (count (:hosted card)) 2))
                :cost [:click 1]
                :prompt "Select an asset or agenda to install"
                :choices {:card #(and (or (asset? %)
                                          (agenda? %))
                                      (in-hand? %)
                                      (corp? %))}
                :msg "install and host an asset or agenda"
                :async true
                :effect (effect (corp-install eid target card nil))}
               {:label "Install a previously-installed asset or agenda on Full Immersion RecStudio (fixes only)"
                :req (req (< (count (:hosted card)) 2))
                :prompt "Select an installed asset or agenda to host on Full Immersion RecStudio"
                :choices {:card #(and (or (asset? %) (agenda? %))
                                      (installed? %)
                                      (corp? %))}
                :msg "install and host an asset or agenda"
                :effect (req (host state side card target))}]})

(define-card "Fumiko Yamamori"
  {:events [{:event :reveal-spent-credits
             :async true
             :req (req (and (some? (first targets))
                            (some? (second targets))
                            (not= (first targets) (second targets))))
             :msg "do 1 meat damage"
             :effect (effect (damage eid :meat 1 {:card card}))}]})

(define-card "Gene Splicer"
  {:advanceable :always
   :access {:req (req (pos? (get-counters (get-card state card) :advancement)))
            :msg (msg "do " (get-counters (get-card state card) :advancement) " net damage")
            :async true
            :effect (effect (damage eid :net (get-counters (get-card state card) :advancement)
                                    {:card card}))}
   :abilities [{:cost [:click 1 :advancement 3]
                :label "Add Gene Splicing to your score area as an agenda worth 1 agenda point"
                :msg "add it to their score area as an agenda worth 1 agenda point"
                :async true
                :effect (req (as-agenda state :corp eid card 1))}]})

(define-card "Genetics Pavilion"
  {:msg "prevent the Runner from drawing more than 2 cards during their turn"
   :effect (req (max-draw state :runner 2)
                (when (zero? (remaining-draws state :runner))
                  (prevent-draw state :runner)))
   :events [{:event :runner-turn-begins
             :effect (effect (max-draw :runner 2))}]
   :leave-play (req (swap! state update-in [:runner :register] dissoc :max-draw :cannot-draw))})

(define-card "Ghost Branch"
  (advance-ambush 0 {:async true
                     :req (req (pos? (get-counters (get-card state card) :advancement)))
                     :msg (msg "give the Runner " (quantify (get-counters (get-card state card) :advancement) "tag"))
                     :effect (effect (gain-tags :corp eid (get-counters (get-card state card) :advancement)))}))

(define-card "GRNDL Refinery"
  {:advanceable :always
   :abilities [{:label "Gain 4 [Credits] for each advancement token on GRNDL Refinery"
                :cost [:click 1 :trash]
                :msg (msg "gain " (* 4 (get-counters card :advancement)) " [Credits]")
                :effect (effect (gain-credits (* 4 (get-counters card :advancement))))}]})

(define-card "Haas Arcology AI"
  {:advanceable :while-unrezzed
   :abilities [{:label "Gain [Click][Click]"
                :once :per-turn
                :msg "gain [Click][Click]"
                :cost [:click 1 :advancement 1]
                :effect (effect (gain :click 2))}]})

(define-card "Honeyfarm"
  {:flags {:rd-reveal (req true)}
   :access {:msg "force the Runner to lose 1 [Credits]"
            :effect (effect (lose-credits :runner 1))}})

(define-card "Hostile Infrastructure"
  (let [ability
        {:async true
         :req (req (and (= side :runner)
                        (some corp? targets)))
         :msg (msg (str "do " (count (filter corp? targets))
                        " net damage"))
         :effect (req (letfn [(do-damage [t]
                                (if (seq t)
                                  (wait-for (damage state :corp :net 1 {:card card})
                                            (do-damage (rest t)))
                                  (effect-completed state side eid)))]
                        (do-damage (filter corp? targets))))}]
    {:trash-effect ability
     :events [(assoc ability :event :runner-trash)]
     :abilities [{:msg "do 1 net damage"
                  :async true
                  :effect (effect (damage eid :net 1 {:card card}))}]}))

(define-card "Hyoubu Research Facility"
  {:events [{:event :reveal-spent-credits
             :req (req (some? (first targets)))
             :once :per-turn
             :msg (msg "gain " target " [Credits]")
             :effect (effect (gain-credits :corp target))}]})

(define-card "Ibrahim Salem"
  (let [trash-ability (fn [card-type]
                        {:req (req (seq (filter #(is-type? % card-type) (:hand runner))))
                         :prompt (str "Choose a " card-type " to trash")
                         :choices (req (filter #(is-type? % card-type) (:hand runner)))
                         :async true
                         :effect (effect (trash eid target nil))
                         :msg (msg "trash " (:title target) " from the grip")})
        choose-ability {:label "Trash 1 card in the grip of a named type"
                        :once :per-turn
                        :req (req (seq (:hand runner)))
                        :prompt "Choose a card type"
                        :choices ["Event" "Hardware" "Program" "Resource"]
                        :msg (msg "reveal " (join ", " (map :title (:hand runner))))
                        :async true
                        :effect (effect (continue-ability (trash-ability target) card nil))}]
    {:additional-cost [:forfeit]
     :flags {:corp-phase-12 (constantly true)}
     :derezzed-events [corp-rez-toast]
     :abilities [choose-ability]}))

(define-card "Illegal Arms Factory"
  (let [ability {:msg "gain 1 [Credits] and draw 1 card"
                 :label "Gain 1 [Credits] and draw 1 card (start of turn)"
                 :once :per-turn
                 :async true
                 :req (req (:corp-phase-12 @state))
                 :effect (effect (gain-credits 1)
                                 (draw eid 1 nil))}]
    {:derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]
     :trash-effect {:req (req (and (= :servers (first (:previous-zone card)))
                                   (= side :runner)))
                    :msg "take 1 bad publicity"
                    :effect (effect (gain-bad-publicity :corp 1))}}))

(define-card "Indian Union Stock Exchange"
  (let [iuse {:req (req (not= (:faction target) (:faction (:identity corp))))
              :msg "gain 1 [Credits]"
              :effect (effect (gain-credits 1))}]
    {:events [(assoc iuse :event :play-operation)
              (assoc iuse :event :rez)]}))

(define-card "Isabel McGuire"
  {:abilities [{:label "Add an installed card to HQ"
                :cost [:click 1]
                :choices {:card installed?}
                :msg (msg "move " (card-str state target) " to HQ")
                :effect (effect (move target :hand))}]})

(define-card "IT Department"
  {:abilities [{:cost [:click 1]
                :msg "add 1 counter"
                :effect (effect (add-counter card :power 1))}
               {:cost [:power 1]
                :label "Add strength to a rezzed ICE"
                :choices {:card #(and (ice? %)
                                      (rezzed? %))}
                :req (req (pos? (get-counters card :power)))
                :msg (msg "add strength to a rezzed ICE")
                :effect (effect (register-floating-effect
                                  card
                                  (let [it-target target]
                                    {:type :ice-strength
                                     :duration :end-of-turn
                                     :req (req (same-card? target it-target))
                                     :value (req (inc (get-counters card :power)))}))
                                (update-ice-strength target))}]})

(define-card "Jackson Howard"
  {:abilities [{:cost [:click 1]
                :msg "draw 2 cards"
                :effect (effect (draw 2))}
               {:label "Shuffle up to 3 cards from Archives into R&D"
                :cost [:remove-from-game]
                :effect (effect (shuffle-into-rd-effect card 3))}]})

(define-card "Jeeves Model Bioroids"
  (let [ability {:label "Gain [Click]"
                 :msg "gain [Click]"
                 :once :per-turn
                 :effect (effect (gain :click 1))}
        cleanup (effect (update! (dissoc card :seen-this-turn)))]
    {:abilities [ability]
     :leave-play cleanup
     :events [{:event :corp-spent-click
               :effect (req (let [target (or (first (filter number? target))
                                             (first target))]
                              (when-not target
                                (print-stack-trace (Exception. (str "WHY JEEVES WHY: " targets))))
                              (update! state side (update-in card [:seen-this-turn (or target :this-is-a-hack)]
                                                             (fnil + 0) (second targets)))
                              (when (>= (get-in (get-card state card) [:seen-this-turn (or target :this-is-a-hack)]) 3)
                                (resolve-ability state side ability card nil))))}
              {:event :corp-turn-ends
               :effect cleanup}]}))

(define-card "Kala Ghoda Real TV"
  {:derezzed-events [corp-rez-toast]
   :flags {:corp-phase-12 (req true)}
   :abilities [{:msg "look at the top card of the Runner's Stack"
                :effect (effect (prompt! card (str "The top card of the Runner's Stack is "
                                                   (:title (first (:deck runner)))) ["OK"] {}))}
               {:async true
                :label "Trash the top card of the Runner's Stack"
                :msg (msg "trash " (:title (first (:deck runner))) " from the Runner's Stack")
                :cost [:trash]
                :effect (effect (mill :corp eid :runner 1))}]})

(define-card "Kuwinda K4H1U3"
  {:derezzed-events [corp-rez-toast]
   :flags {:corp-phase-12 (req true)}
   :abilities [{:label "Trace X - do 1 brain damage (start of turn)"
                :trace {:base (req (get-counters card :power))
                        :successful
                        {:async true
                         :msg "do 1 brain damage"
                         :effect (req (wait-for (damage state :runner :brain 1 {:card card})
                                                (trash state side eid card nil)))}
                        :unsuccessful
                        {:effect (effect (add-counter card :power 1)
                                         (system-msg "adds 1 power counter to Kuwinda K4H1U3"))}}}]})

(define-card "Lady Liberty"
  {:abilities [{:cost [:click 3]
                :label "Add agenda from HQ to score area"
                :req (req (let [counters (get-counters (get-card state card) :power)]
                            (some #(and (agenda? %)
                                        (= counters (:agendapoints %)))
                                  (:hand corp))))
                :async true
                :effect (req (show-wait-prompt state :runner "Corp to select an agenda for Lady Liberty")
                             (continue-ability
                               state side
                               {:prompt "Select an Agenda in HQ to move to score area"
                                :choices {:card #(and (agenda? %)
                                                      (= (:agendapoints %) (get-counters (get-card state card) :power))
                                                      (in-hand? %))}
                                :msg (msg "add " (:title target) " to score area")
                                :async true
                                :effect (req (clear-wait-prompt state :runner)
                                             (as-agenda state :corp eid target (:agendapoints target) {:register-events true}))}
                               card nil))}]
   :events [{:event :corp-turn-begins
             :effect (effect (add-counter card :power 1))}]})

(define-card "Lakshmi Smartfabrics"
  {:events [{:event :rez
             :effect (effect (add-counter card :power 1))}]
   :abilities [{:req (req (seq (filter #(and (agenda? %)
                                             (>= (get-counters card :power)
                                                 (:agendapoints %)))
                                       (:hand corp))))
                :label "X power counters: Reveal an agenda worth X points from HQ"
                :effect (req (let [c (get-counters card :power)]
                               (resolve-ability
                                 state side
                                 {:prompt "Select an agenda in HQ to reveal"
                                  :choices {:card #(and (agenda? %)
                                                        (>= c (:agendapoints %)))}
                                  :msg (msg "reveal " (:title target) " from HQ")
                                  :effect (req (reveal state side target)
                                               (let [title (:title target)
                                                     pts (:agendapoints target)]
                                                 (register-turn-flag!
                                                   state side
                                                   card :can-steal
                                                   (fn [state side card]
                                                     (if (= (:title card) title)
                                                       ((constantly false)
                                                        (toast state :runner "Cannot steal due to Lakshmi Smartfabrics." "warning"))
                                                       true)))
                                                 (add-counter state side card :power (- pts))))}
                                 card nil)))}]})

(define-card "Launch Campaign"
  (campaign 6 2))

(define-card "Levy University"
  {:abilities [{:prompt "Choose an ICE"
                :msg (msg "adds " (:title target) " to HQ")
                :choices (req (cancellable (filter ice? (:deck corp)) :sorted))
                :label "Search R&D for a piece of ICE"
                :cost [:click 1 :credit 1]
                :effect (effect (move target :hand)
                                (shuffle! :deck))}]})

(define-card "Lily Lockwell"
  {:async true
   :effect (effect (draw eid 3 nil))
   :msg (msg "draw 3 cards")
   :abilities [{:label "Search R&D for an operation"
                :prompt "Choose an operation to put on top of R&D"
                :cost [:click 1 :tag 1]
                :msg (msg (if (= target "No action")
                            "search R&D, but does not find an operation"
                            (str "put " (:title target) " on top of R&D")))
                :choices (req (conj (vec (sort-by :title (filter operation? (:deck corp)))) "No action"))
                :effect (req (if (= target "No action")
                               (shuffle! state :corp :deck)
                               (let [c (move state :corp target :play-area)]
                                 (reveal state side c)
                                 (shuffle! state :corp :deck)
                                 (move state :corp c :deck {:front true}))))}]})

(define-card "Long-Term Investment"
  {:derezzed-events [corp-rez-toast]
   :abilities [{:label "Move any number of [Credits] to your credit pool"
                :req (req (>= (get-counters card :credit) 8))
                :cost [:click 1]
                :prompt "How many [Credits]?"
                :choices {:counter :credit}
                :msg (msg "gain " target " [Credits]")
                :effect (effect (gain-credits target))}]
   :events [{:event :corp-turn-begins
             :effect (effect (add-counter card :credit 2)
                             (system-msg (str "adds 2 [Credit] to Long-Term Investment")))}]})

(define-card "Malia Z0L0K4"
  (let [re-enable-target (req (when-let [malia-target (:malia-target card)]
                                (when (:disabled (get-card state malia-target))
                                  (system-msg state side (str "uses "  (:title card) " to unblank "
                                                              (card-str state malia-target)))
                                  (enable-card state :runner (get-card state malia-target))
                                  (when-let [reactivate-effect (:reactivate (card-def malia-target))]
                                    (resolve-ability state :runner reactivate-effect (get-card state malia-target) nil)))))]
    {:effect (effect (update! (assoc card :malia-target target))
                     (disable-card :runner target))
     :msg (msg (str "blank the text box of " (card-str state target)))
     :choices {:card #(and (runner? %)
                           (installed? %)
                           (resource? %)
                           (not (has-subtype? % "Virtual")))}
     :leave-play re-enable-target
     :move-zone re-enable-target}))

(define-card "Marilyn Campaign"
  (let [ability {:once :per-turn
                 :interactive (req true)
                 :req (req (:corp-phase-12 @state))
                 :label (str "Gain 2 [Credits] (start of turn)")
                 :msg (msg "gain " (min 2 (get-counters card :credit)) " [Credits]")
                 :async true
                 :effect (req (let [credits (min 2 (get-counters card :credit))]
                                (add-counter state side card :credit (- credits))
                                (gain-credits state :corp credits))
                              (if (not (pos? (get-counters (get-card state card) :credit)))
                                (trash state :corp eid card {:unpreventable true})
                                (effect-completed state :corp eid)))}]
    {:data {:counter {:credit 8}}
     :derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [(set-autoresolve :auto-reshuffle "Marilyn reshuffle")]
     :trash-effect {:req (req (= :servers (first (:previous-zone card))))
                    :async true
                    :interactive (req true)
                    :effect (effect (show-wait-prompt :runner "Corp to use Marilyn Campaign")
                                    (continue-ability
                                      :corp
                                      {:optional
                                       {:prompt "Shuffle Marilyn Campaign into R&D?"
                                        :autoresolve (get-autoresolve :auto-reshuffle)
                                        :player :corp
                                        :yes-ability {:msg "shuffle it back into R&D"
                                                      :effect (effect (move :corp card :deck)
                                                                      (shuffle! :corp :deck))}
                                        :end-effect (effect (clear-wait-prompt :runner))}}
                                      card nil))}}))

(define-card "Mark Yale"
  {:events [{:event :agenda-counter-spent
             :msg "gain 1 [Credits]"
             :effect (effect (gain-credits 1))}]
   :abilities [{:label "Gain 2 [Credits]"
                :msg "gain 2 [Credits]"
                :cost [:trash]
                :effect (effect (gain-credits 2))}
               {:label "Gain 2 [Credits]"
                :msg "gain 2 [Credits]"
                :cost [:any-agenda-counter]
                :effect (effect (gain-credits 2)
                                (update-all-agenda-points))}]})

(define-card "Marked Accounts"
  (let [ability {:msg "take 1 [Credits]"
                 :label "Take 1 [Credits] (start of turn)"
                 :once :per-turn
                 :req (req (pos? (get-counters card :credit)))
                 :effect (effect (add-counter card :credit -1)
                                 (gain-credits 1))}]
    {:abilities [ability
                 {:cost [:click 1]
                  :msg "store 3 [Credits]"
                  :effect (effect (add-counter card :credit 3))}]
     :events [(assoc ability :event :corp-turn-begins)]}))

(define-card "MCA Austerity Policy"
  {:abilities [{:cost [:click 1]
                :once :per-turn
                :msg "to force the Runner to lose a [Click] next turn and place a power counter on itself"
                :effect (req (swap! state update-in [:runner :extra-click-temp] (fnil dec 0))
                             (add-counter state side card :power 1))}
               {:cost [:click 1 :power 3 :trash]
                :msg "gain 4 [Click] and trash itself"
                :effect (effect (gain :click 4))}]})

(define-card "Melange Mining Corp."
  {:abilities [{:cost [:click 3]
                :effect (effect (gain-credits 7))
                :msg "gain 7 [Credits]"}]})

(define-card "Mental Health Clinic"
  (let [ability {:msg "gain 1 [Credits]"
                 :label "Gain 1 [Credits] (start of turn)"
                 :once :per-turn
                 :effect (effect (gain-credits 1))}]
    {:effect (effect (gain :runner :hand-size 1))
     :leave-play (effect (lose :runner :hand-size 1))
     :derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(define-card "Mr. Stone"
  {:events [{:event :runner-gain-tag
             :async true
             :msg "do 1 meat damage"
             :effect (effect (damage :corp eid :meat 1 {:card card}))}]})

(define-card "Mumba Temple"
  {:recurring 2
   :interactions {:pay-credits {:req (req (= :rez (:source-type eid)))
                                :type :recurring}}})

(define-card "Mumbad City Hall"
  {:abilities [{:label "Search R&D for an Alliance card"
                :cost [:click 1]
                :prompt "Choose an Alliance card to play or install"
                :choices (req (cancellable (filter #(and (has-subtype? % "Alliance")
                                                         (if (operation? %)
                                                           (<= (:cost %) (:credit corp))
                                                           true))
                                                   (:deck corp))
                                           :sorted))
                :msg (msg "reveal " (:title target)
                          " from R&D and "
                          (if (= (:type target) "Operation") "play" "install")
                          " it")
                :async true
                :effect (req (reveal state side target)
                             (shuffle! state side :deck)
                             (if (operation? target)
                               (play-instant state side eid target nil)
                               (corp-install state side eid target nil nil)))}]})

(define-card "Mumbad Construction Co."
  {:derezzed-events [corp-rez-toast]
   :events [{:event :corp-turn-begins
             :effect (effect (add-prop card :advance-counter 1 {:placed true}))}]
   :abilities [{:cost [:credit 2]
                :req (req (and (pos? (get-counters card :advancement))
                               (not-empty (all-active-installed state :corp))))
                :label "Move an advancement token to a faceup card"
                :prompt "Select a faceup card"
                :choices {:card rezzed?}
                :msg (msg "move an advancement token to " (card-str state target))
                :effect (effect (add-prop card :advance-counter -1 {:placed true})
                                (add-prop target :advance-counter 1 {:placed true}))}]})

(define-card "Museum of History"
  {:derezzed-events [corp-rez-toast]
   :flags {:corp-phase-12 (req (pos? (count (get-in @state [:corp :discard]))))}
   :abilities [{:label "Shuffle cards in Archives into R&D"
                :prompt (msg (let [mus (count (filter #(and (= "10019" (:code %))
                                                            (rezzed? %))
                                                      (all-installed state :corp)))]
                               (str "Select "
                                    (if (> mus 1) "a card " (str mus " cards "))
                                    "in Archives to shuffle into R&D")))
                :choices {:card #(and (corp? %)
                                      (in-discard? %))
                          :max (req (count (filter #(and (= "10019" (:code %))
                                                         (rezzed? %))
                                                   (all-installed state :corp))))}
                :show-discard true
                :once :per-turn
                :once-key :museum-of-history
                :msg (msg "shuffle "
                          (let [seen (filter :seen targets)
                                n (count (filter #(not (:seen %)) targets))]
                            (str (join ", " (map :title seen))
                                 (when (pos? n)
                                   (str (when-not (empty? seen) " and ")
                                        (quantify n "card")))))
                          " into R&D")
                :effect (req (doseq [c targets]
                               (move state side c :deck))
                             (shuffle! state side :deck))}]
   :implementation "Errata from FAQ 3.1: should be unique"})

(define-card "Nanoetching Matrix"
  {:abilities [{:cost [:click 1]
                :once :per-turn
                :msg "gain 2 [Credits]"
                :effect (effect (gain-credits 2))}]
   :trash-effect {:req (req (= :runner side))
                  :effect (effect (show-wait-prompt :runner "Corp to use Nanoetching Matrix")
                                  (continue-ability
                                    :corp
                                    {:optional
                                     {:prompt "Gain 2 [credits]?"
                                      :yes-ability {:msg (msg "gain 2 [Credits]")
                                                    :effect (effect (gain-credits :corp 2))}
                                      :end-effect (effect (clear-wait-prompt :runner))}}
                                    card nil))}})

(define-card "NASX"
  (let [ability {:msg "gain 1 [Credits]"
                 :label "Gain 1 [Credits] (start of turn)"
                 :once :per-turn
                 :effect (effect (gain-credits 1))}]
    {:implementation "Manual - click NASX to add power counters"
     :derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability
                 {:label "Place 1 power counter"
                  :cost [:credit 1]
                  :effect (effect (add-counter card :power 1)
                                  (system-msg (str "places 1 power counter on NASX")))}
                 {:label "Place 2 power counters"
                  :cost [:credit 2]
                  :effect (effect (add-counter card :power 2)
                                  (system-msg (str "places 2 power counters on NASX")))}
                 {:label "[Trash] and gain 2 [Credits] for each power counter"
                  :cost [:click 1 :trash]
                  :msg (msg "gain " (* 2 (get-counters card :power)) " [Credits]")
                  :effect (effect (gain-credits (* 2 (get-counters card :power))))}]}))

(define-card "Net Analytics"
  (let [ability {:req (req (seq (filter #(some #{:tag} %) targets)))
                 :effect (effect (show-wait-prompt :runner "Corp to use Net Analytics")
                                 (continue-ability
                                   :corp
                                   {:optional
                                    {:prompt "Draw from Net Analytics?"
                                     :yes-ability {:msg (msg "draw a card")
                                                   :effect (effect (draw :corp eid 1 nil))}
                                     :end-effect (effect (clear-wait-prompt :runner))}}
                                   card nil))}]
    {:events [(assoc ability
                     :event :runner-lose-tag
                     :req (req (= side :runner)))
              (assoc ability
                     :event :runner-prevent
                     :req (req (seq (filter #(some #{:tag} %) targets))))]}))

(define-card "Net Police"
  {:recurring (effect (set-prop card :rec-counter (:link runner)))
   :effect (effect (set-prop card :rec-counter (:link runner)))
   :interactions {:pay-credits {:req (req (= :trace (:source-type eid)))
                                :type :recurring}}})

(define-card "Neurostasis"
  (advance-ambush 3 {:req (req (pos? (get-counters (get-card state card) :advancement)))
                     :async true
                     :effect (req (let [cnt (get-counters (get-card state card) :advancement)]
                                    (continue-ability
                                      state side
                                      {:prompt (msg "Choose " (quantify cnt "installed card") " to shuffle into the stack")
                                       :player :corp
                                       :cost [:credit 3]
                                       :choices {:card #(and (installed? %)
                                                             (runner? %))
                                                 :max cnt}
                                       :msg (msg "shuffle " (join ", " (map :title targets)) " into the stack")
                                       :effect (req (doseq [c targets]
                                                      (move state :runner c :deck))
                                                    (shuffle! state :runner :deck))}
                                      card nil)))}
                  "Pay 3 [Credits] to use Neurostasis ability?"))

(define-card "News Team"
  {:flags {:rd-reveal (req true)}
   :access {:async true
            :msg "force the Runner take 2 tags or add it to their score area as an agenda worth -1 agenda point"
            :effect (effect (continue-ability
                              {:player :runner
                               :async true
                               :prompt "Take 2 tags or add News Team to your score area as an agenda worth -1 agenda point?"
                               :choices ["Take 2 tags" "Add News Team to score area"]
                               :effect (req (if (= target "Add News Team to score area")
                                              (do (system-msg state :runner (str "adds " (:title card)
                                                                                 " to their score area as an agenda worth "
                                                                                 (quantify -1 "agenda point")))
                                                  (as-agenda state :runner eid card -1 {:force true}))
                                              (do (system-msg state :runner (str "takes 2 tags from News Team"))
                                                  (gain-tags state :runner eid 2))))}
                              card targets))}})

(define-card "NGO Front"
  (letfn [(builder [cost cred]
            {:cost [:advancement cost :trash]
             :effect (effect (gain-credits cred))
             :label (str "Gain " cred " [Credits]")
             :msg (str "gain " cred " [Credits]")})]
    {:advanceable :always
     :abilities [(builder 1 5)
                 (builder 2 8)]}))

(define-card "Open Forum"
  {:events [{:event :corp-mandatory-draw
             :interactive (req true)
             :msg (msg (if (-> corp :deck count pos?)
                         (str "reveal and draw " (-> corp :deck first :title) " from R&D")
                         "reveal and draw from R&D but it is empty"))
             :async true
             :effect (effect (reveal (-> corp :deck first))
                             (draw 1)
                             (continue-ability
                               {:prompt "Choose a card in HQ to put on top of R&D"
                                :async true
                                :choices {:card #(and (in-hand? %)
                                                      (corp? %))}
                                :msg "add 1 card from HQ to the top of R&D"
                                :effect (effect (move target :deck {:front true})
                                                (effect-completed eid))}
                               card nil))}]})

(define-card "PAD Campaign"
  (let [ability {:msg "gain 1 [Credits]"
                 :label "Gain 1 [Credits] (start of turn)"
                 :once :per-turn
                 :effect (effect (gain-credits 1))}]
    {:derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(define-card "PAD Factory"
  {:abilities [{:cost [:click 1]
                :label "Place 1 advancement token on a card"
                :choices {:card installed?}
                :msg (msg "place 1 advancement token on " (card-str state target))
                :effect (req (add-prop state :corp target :advance-counter 1 {:placed true})
                             (let [tgtcid (:cid target)]
                               (register-turn-flag!
                                 state side
                                 target :can-score
                                 (fn [state side card]
                                   (if (and (= tgtcid
                                               (:cid card))
                                            (>= (get-counters card :advancement)
                                                (or (:current-cost card)
                                                    (:advancementcost card))))
                                     ((constantly false) (toast state :corp "Cannot score due to PAD Factory." "warning"))
                                     true)))))}]})

(define-card "Pālanā Agroplex"
  (let [ability {:msg "make each player draw 1 card"
                 :label "Make each player draw 1 card (start of turn)"
                 :once :per-turn
                 :async true
                 :effect (req (wait-for (draw state :corp 1 nil)
                                        (draw state :runner eid 1 nil)))}]
    {:derezzed-events [corp-rez-toast]
     :flags {:corp-phase-12 (req true)}
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(define-card "Personalized Portal"
  {:events [{:event :corp-turn-begins
             :async true
             :effect (req (wait-for (draw state :runner 1 nil)
                                    (let [cnt (count (get-in @state [:runner :hand]))
                                          credits (quot cnt 2)]
                                      (gain-credits state :corp credits)
                                      (system-msg state :corp
                                                  (str "uses Personalized Portal to force the runner to draw "
                                                       "1 card and gain " credits " [Credits]"))
                                      (effect-completed state side eid))))}]})

(define-card "Plan B"
  (advance-ambush
    0
    {:req (req (pos? (get-counters (get-card state card) :advancement)))
     :effect (req (show-wait-prompt state :runner "Corp to select an agenda to score with Plan B")
                  (doseq [ag (filter agenda? (:hand corp))]
                    (update-advancement-cost state side ag))
                  (continue-ability
                    state side
                    {:prompt "Select an Agenda in HQ to score"
                     :choices {:card #(and (agenda? %)
                                           (<= (:current-cost %) (get-counters (get-card state card) :advancement))
                                           (in-hand? %))}
                     :msg (msg "score " (:title target))
                     :effect (effect (score (assoc target :advance-counter
                                                   (:current-cost target)))
                                     (clear-wait-prompt :runner))}
                    card nil))}
    "Score an Agenda from HQ?"))

(define-card "Political Dealings"
  (letfn [(pdhelper [agendas n]
            {:optional
             {:prompt (msg "Reveal and install " (:title (nth agendas n)) "?")
              :yes-ability {:async true
                            :msg (msg "reveal " (:title (nth agendas n)))
                            :effect (req (reveal state side (nth agendas n))
                                         (wait-for (corp-install
                                                     state side (nth agendas n) nil
                                                     {:install-state
                                                      (:install-state
                                                        (card-def (nth agendas n))
                                                        :unrezzed)})
                                                   (if (< (inc n) (count agendas))
                                                     (continue-ability state side (pdhelper agendas (inc n)) card nil)
                                                     (effect-completed state side eid))))}
              :no-ability {:async true
                           :effect (req (if (< (inc n) (count agendas))
                                          (continue-ability state side (pdhelper agendas (inc n)) card nil)
                                          (effect-completed state side eid)))}}})]
    {:events [{:event :corp-draw
               :async true
               :req (req (let [drawn (get-in @state [:corp :register :most-recent-drawn])
                               agendas (filter agenda? drawn)]
                           (seq agendas)))
               :effect (req (let [drawn (get-in @state [:corp :register :most-recent-drawn])
                                  agendas (filter agenda? drawn)]
                              (continue-ability state side (pdhelper agendas 0) card nil)))}]}))

(define-card "Prāna Condenser"
  {:events [{:event :pre-resolve-damage
             :async true
             :req (req (and (not (get-in card [:special :prana-disabled]))
                            (= target :net)
                            (pos? (last targets))))
             :effect (req (let [amount (last targets)
                                damagecard (second targets)]
                            (swap! state assoc-in [:damage :damage-replace] true)
                            (show-wait-prompt state :runner (str "Corp to use " (:title card)))
                            (continue-ability
                              state side
                              {:optional
                               {:prompt (str "Prevent 1 net damage to add power token to " (:title card) "?")
                                :player :corp
                                :yes-ability
                                {:async true
                                 :msg "prevent 1 net damage, place 1 power token, and gain 3 [Credits]"
                                 :effect (req (swap! state update-in [:damage] dissoc :damage-replace)
                                              (clear-wait-prompt state :runner)
                                              (add-counter state side (get-card state card) :power 1)
                                              (gain state side :credit 3)
                                              ;temporarily disable prana to not trigger on X-1 net damage
                                              (update! state side (assoc-in (get-card state card) [:special :prana-disabled] true))
                                              (wait-for (damage state side :net (dec amount) {:card damagecard})
                                                        (swap! state assoc-in [:damage :damage-replace] true)
                                                        (update! state side (assoc-in (get-card state card) [:special :prana-disabled] false))
                                                        (effect-completed state side eid)))}
                                :no-ability
                                {:async true
                                 :effect (req (swap! state update-in [:damage] dissoc :damage-replace)
                                              (clear-wait-prompt state :runner)
                                              (effect-completed state side eid))}}}
                              card nil)))}]
   :abilities [{:msg (msg "deal " (get-counters card :power) " net damage")
                :cost [[:click 2] [:trash]]
                :effect (effect (damage eid :net (get-counters card :power) {:card card}))}]})

(define-card "Primary Transmission Dish"
  {:recurring 3
   :interactions {:pay-credits {:req (req (= :trace (:source-type eid)))
                                :type :recurring}}})

(define-card "Private Contracts"
  {:effect (effect (add-counter card :credit 14))
   :events [(trash-on-empty :credit)]
   :abilities [{:cost [:click 1]
                :msg (msg "gain " (min 2 (get-counters card :credit)) " [Credits]")
                :effect (req (let [credits (min 2 (get-counters card :credit))]
                               (add-counter state side card :credit (- credits))
                               (gain-credits state :corp credits)))}]})

(define-card "Project Junebug"
  (advance-ambush 1 {:req (req (pos? (get-counters (get-card state card) :advancement)))
                     :msg (msg "do " (* 2 (get-counters (get-card state card) :advancement)) " net damage")
                     :async true
                     :effect (effect (damage eid :net (* 2 (get-counters (get-card state card) :advancement))
                                             {:card card}))}))

(define-card "Psychic Field"
  (let [ab {:async true
            :req (req installed)
            :effect (req (let [hand (count (:hand runner))
                               message (str "do " hand " net damage")]
                           (continue-ability
                             state side
                             {:psi {:not-equal
                                    {:msg message
                                     :async true
                                     :effect (effect (damage eid :net hand {:card card}))}}}
                             card nil)))}]
    {:expose ab
     :access ab}))

(define-card "Public Health Portal"
  (let [ability {:once :per-turn
                 :label "Reveal top card of R&D and gain 2 [Credits] (start of turn)"
                 :interactive (req true)
                 :msg (msg " reveal " (-> @state :corp :deck first :title)
                           " from the top of R&D"
                           " and gain 2 [Credits]")
                 :effect (effect (reveal (-> @state :corp :deck first))
                                 (gain-credits 2))}]
    {:derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(define-card "Public Support"
  {:effect (effect (add-counter card :power 3))
   :derezzed-events [corp-rez-toast]
   :events [{:event :corp-turn-begins
             :req (req (pos? (get-counters card :power)))
             :effect (effect (add-counter card :power -1))}
            {:event :counter-added
             :req (req (same-card? card target)
                       (not (pos? (get-counters card :power))))
             :async true
             :effect (effect (system-msg "uses Public Support to add it to their score area as an agenda worth 1 agenda point")
                             (as-agenda eid (dissoc card :counter) 1))}]})

(define-card "Quarantine System"
  (letfn [(rez-ice [cnt] {:prompt "Select an ICE to rez"
                          :async true
                          :choices {:card #(and (ice? %)
                                                (not (rezzed? %)))}
                          :msg (msg "rez " (:title target))
                          :effect (req (let [agenda (last (:rfg corp))
                                             ap (:agendapoints agenda 0)]
                                         (rez state side target {:no-warning true :cost-bonus (* ap -2)})
                                         (if (< cnt 3)
                                           (continue-ability state side (rez-ice (inc cnt)) card nil)
                                           (effect-completed state side eid))))})]
    {:abilities [{:label "Forfeit agenda to rez up to 3 ICE with a 2 [Credit] discount per agenda point"
                  :req (req (pos? (count (:scored corp))))
                  :cost [:forfeit]
                  :effect (req (continue-ability state side (rez-ice 1) card nil))}]}))

(define-card "Raman Rai"
  {:abilities [{:once :per-turn
                :label "Lose [Click] and swap a card in HQ you just drew for a card in Archives"
                :req (req (and (pos? (:click corp))
                               (not-empty (turn-events state side :corp-draw))))
                :effect (req (let [drawn (get-in @state [:corp :register :most-recent-drawn])]
                               (lose state :corp :click 1)
                               (resolve-ability
                                 state side
                                 {:prompt "Choose a card in HQ that you just drew to swap for a card of the same type in Archives"
                                  :choices {:card #(some (fn [c] (same-card? c %)) drawn)}
                                  :effect (req (let [hqcard target
                                                     t (:type hqcard)]
                                                 (resolve-ability
                                                   state side
                                                   {:show-discard true
                                                    :prompt (msg "Choose an " t " in Archives to reveal and swap into HQ for " (:title hqcard))
                                                    :choices {:card #(and (corp? %)
                                                                          (= (:type %) t)
                                                                          (in-discard? %))}
                                                    :msg (msg "lose [Click], reveal " (:title hqcard) " from HQ, and swap it for " (:title target) " from Archives")
                                                    :effect (req (let [swappedcard (assoc hqcard :zone [:discard])
                                                                       archndx (ice-index state target)
                                                                       arch (get-in @state [:corp :discard])
                                                                       newarch (apply conj (subvec arch 0 archndx) swappedcard (subvec arch archndx))]
                                                                   (reveal state side hqcard)
                                                                   (swap! state assoc-in [:corp :discard] newarch)
                                                                   (swap! state update-in [:corp :hand]
                                                                          (fn [coll] (remove-once #(same-card? % hqcard) coll)))
                                                                   (move state side target :hand)))}
                                                   card nil)))}
                                 card nil)))}]})

(define-card "Rashida Jaheem"
  (let [ability {:once :per-turn
                 :async true
                 :label "Gain 3 [Credits] and draw 3 cards (start of turn)"
                 :req (req (:corp-phase-12 @state))
                 :effect
                 (effect
                   (continue-ability
                     {:optional
                      {:prompt "Trash Rashida Jaheem to gain 3 [Credits] and draw 3 cards?"
                       :yes-ability
                       {:async true
                        :msg "gain 3 [Credits] and draw 3 cards"
                        :effect (req (wait-for (trash state side card nil)
                                               (gain-credits state side 3)
                                               (draw state side eid 3 nil)))}}}
                     card nil))}]
    {:derezzed-events [corp-rez-toast]
     :flags {:corp-phase-12 (req true)}
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(define-card "Reality Threedee"
  (let [ability {:effect (req (gain-credits state side (if tagged 2 1)))
                 :label "Gain credits (start of turn)"
                 :once :per-turn
                 :msg (msg (if tagged "gain 2 [Credits]" "gain 1 [Credits]"))}]
    {:effect (effect (gain-bad-publicity :corp 1)
                     (system-msg "takes 1 bad publicity"))
     :derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(define-card "Reconstruction Contract"
  {:events [{:event :damage
             :req (req (and (pos? (nth targets 2)) (= :meat target)))
             :effect (effect (add-counter card :advancement 1)
                             (system-msg "adds 1 advancement token to Reconstruction Contract"))}]
   :abilities [{:label "Move advancement tokens to another card"
                :async true
                :trash-icon true
                :prompt "Select a card that can be advanced"
                :choices {:card can-be-advanced?}
                :effect (effect
                          (continue-ability
                            (let [move-to target]
                              {:async true
                               :prompt "Move how many tokens?"
                               :choices {:number (req (get-counters card :advancement))
                                         :default (req (get-counters card :advancement))}
                               :cost [:trash]
                               :effect (effect (add-counter move-to :advancement target {:placed true})
                                               (system-msg (str "trashes Reconstruction Contract to move "
                                                                target (pluralize " advancement token" target)
                                                                " to " (card-str state move-to)))
                                               (effect-completed eid))})
                            card nil))}]})

(define-card "Reversed Accounts"
  {:advanceable :always
   :abilities [{:cost [:click 1 :trash]
                :label "Force the Runner to lose 4 [Credits] per advancement"
                :msg (msg "force the Runner to lose " (min (* 4 (get-counters card :advancement)) (:credit runner)) " [Credits]")
                :effect (effect (lose-credits :runner (* 4 (get-counters card :advancement))))}]})

(define-card "Rex Campaign"
  (let [ability {:once :per-turn
                 :req (req (:corp-phase-12 @state))
                 :label "Remove 1 counter (start of turn)"
                 :effect (effect (add-counter card :power -1))}]
    {:effect (effect (add-counter card :power 3))
     :derezzed-events [corp-rez-toast]
     :events [(trash-on-empty :power)
              (assoc ability :event :corp-turn-begins)]
     :ability [ability]
     :trash-effect {:req (req (zero? (get-counters card :power)))
                    :prompt "Remove 1 bad publicity or gain 5 [Credits]?"
                    :choices ["Remove 1 bad publicity" "Gain 5 [Credits]"]
                    :msg (msg (if (= target "Remove 1 bad publicity")
                                "remove 1 bad publicity" "gain 5 [Credits]"))
                    :effect (req (if (= target "Remove 1 bad publicity")
                                   (lose-bad-publicity state side 1)
                                   (gain-credits state side 5)))}}))

(define-card "Ronald Five"
  (let [ability {:req (req (and (some corp? targets)
                                (pos? (:click runner))))
                 :msg "force the runner to lose 1 [Click]"
                 :effect (effect (lose :runner :click 1))}]
    {:events [(assoc ability :event :runner-trash)]
     :trash-effect ability}))

(define-card "Ronin"
  {:advanceable :always
   :abilities [{:cost [:click 1 :trash]
                :req (req (>= (get-counters card :advancement) 4))
                :msg "do 3 net damage"
                :async true
                :effect (effect (damage eid :net 3 {:card card}))}]})

(define-card "Roughneck Repair Squad"
  {:abilities [{:label "Gain 6 [Credits], may remove 1 bad publicity"
                :cost [:click 3]
                :msg "gain 6 [Credits]"
                :effect (effect (gain-credits 6)
                                (continue-ability
                                  {:optional {:req (req (pos? (count-bad-pub state)))
                                              :player :corp
                                              :prompt "Remove 1 bad publicity?"
                                              :yes-ability {:msg "remove 1 bad publicity"
                                                            :effect (effect (lose-bad-publicity 1))}}}
                                  card nil))}]})

(define-card "Sandburg"
  {:effect (effect (update-all-ice))
   :constant-effects [{:type :ice-strength
                       :req (req (<= 10 (:credit corp)))
                       :value (req (quot (:credit corp) 5))}]
   :events [{:event :corp-gain
             :req (req (= :credit (first target)))
             :effect (effect (update-all-ice))}
            {:event :corp-lose
             :req (req (= :credit (first target)))
             :effect (effect (update-all-ice))}]
   :leave-play (effect (update-all-ice))})

(define-card "Sealed Vault"
  {:abilities [{:label "Store any number of [Credits] on Sealed Vault"
                :cost [:credit 1]
                :prompt "How many [Credits]?"
                :choices {:number (req (- (:credit corp) 1))}
                :msg (msg "store " target " [Credits]")
                :effect (effect (lose-credits target)
                                (add-counter card :credit target))}
               {:label "Move any number of [Credits] to your credit pool"
                :cost [:click 1]
                :prompt "How many [Credits]?"
                :choices {:counter :credit}
                :msg (msg "gain " target " [Credits]")
                :effect (effect (gain-credits target))}
               {:label "Move any number of [Credits] to your credit pool"
                :prompt "How many [Credits]?"
                :choices {:counter :credit}
                :msg (msg "trash it and gain " target " [Credits]")
                :cost [:trash]
                :effect (effect (gain-credits target))}]})

(define-card "Security Subcontract"
  {:abilities [{:cost [:click 1 :ice 1]
                :msg "gain 4 [Credits]"
                :label "Gain 4 [Credits]"
                :effect (effect (gain-credits 4))}]})

(define-card "Sensie Actors Union"
  {:derezzed-events [corp-rez-toast]
   :flags {:corp-phase-12 (req unprotected)}
   :abilities [{:label "Draw 3 cards and add 1 card in HQ to the bottom of R&D"
                :once :per-turn
                :msg "draw 3 cards"
                :effect (effect (draw 3)
                                (resolve-ability
                                  {:prompt "Select a card in HQ to add to the bottom of R&D"
                                   :choices {:card #(and (corp? %)
                                                         (in-hand? %))}
                                   :msg "add 1 card from HQ to the bottom of R&D"
                                   :effect (effect (move target :deck))}
                                  card nil))}]})

(define-card "Server Diagnostics"
  (let [ability {:effect (effect (gain-credits 2))
                 :once :per-turn
                 :label "Gain 2 [Credits] (start of turn)"
                 :msg "gain 2 [Credits]"}]
    {:derezzed-events [corp-rez-toast]
     :abilities [ability]
     :events [(assoc ability :event :corp-turn-begins)
              {:event :corp-install
               :req (req (ice? target))
               :async true
               :effect (req (system-msg state :runner "trashes Server Diagnostics")
                            (trash state side eid card nil))}]}))

(define-card "Shannon Claire"
  {:abilities [{:cost [:click 1]
                :msg "draw 1 card from the bottom of R&D"
                :effect (effect (move (last (:deck corp)) :hand))}
               {:label "Search R&D for an agenda"
                :prompt "Choose an agenda to add to the bottom of R&D"
                :msg (msg "reveal " (:title target) " from R&D and add it to the bottom of R&D")
                :choices (req (cancellable (filter agenda? (:deck corp)) :sorted))
                :cost [:trash]
                :effect (effect (reveal target)
                                (shuffle! :deck)
                                (move target :deck))}
               {:label "Search Archives for an agenda"
                :prompt "Choose an agenda to add to the bottom of R&D"
                :msg (msg "reveal " (:title target) " from Archives and add it to the bottom of R&D")
                :choices (req (cancellable (filter agenda? (:discard corp)) :sorted))
                :cost [:trash]
                :effect (effect (reveal target)
                                (move target :deck))}]})

(define-card "Shattered Remains"
  (advance-ambush 1 {:async true
                     :req (req (pos? (get-counters (get-card state card) :advancement)))
                     :prompt (msg "Select " (quantify (get-counters (get-card state card) :advancement) "piece") " of hardware to trash")
                     :msg (msg "trash " (join ", " (map :title targets)))
                     :cost [:credit 1]
                     :choices {:max (req (get-counters (get-card state card) :advancement))
                               :card #(and (installed? %)
                                           (hardware? %))}
                     :effect (effect (trash-cards eid targets))}))

(define-card "Shi.Kyū"
  {:access
   {:async true
    :req (req (not= (first (:zone card)) :deck))
    :effect (effect (show-wait-prompt :runner "Corp to use Shi.Kyū")
                    (continue-ability
                      {:optional
                       {:prompt "Pay [Credits] to use Shi.Kyū?"
                        :yes-ability
                        {:prompt "How many [Credits] for Shi.Kyū?"
                         :choices :credit
                         :msg (msg "attempt to do " target " net damage")
                         :async true
                         :effect (req (let [dmg target]
                                        (clear-wait-prompt state :runner)
                                        (continue-ability
                                          state :corp
                                          {:player :runner
                                           :prompt (str "Take " dmg " net damage or add Shi.Kyū to your score area as an agenda worth -1 agenda point?")
                                           :choices [(str "Take " dmg " net damage") "Add Shi.Kyū to score area"]
                                           :async true
                                           :effect (req (if (= target "Add Shi.Kyū to score area")
                                                          (do (system-msg state :runner (str "adds " (:title card)
                                                                                             " to their score area as an agenda worth "
                                                                                             (quantify -1 "agenda point")))
                                                              (as-agenda state :runner eid card -1 {:force true}))
                                                          (do (system-msg state :runner (str "takes " dmg " net damage from Shi.Kyū"))
                                                              (damage state :corp eid :net dmg {:card card}))))}
                                          card targets)))}
                        :no-ability {:effect (effect (clear-wait-prompt :runner))}}}
                      card targets))}})

(define-card "Shock!"
  {:flags {:rd-reveal (req true)}
   :access {:msg "do 1 net damage"
            :async true
            :effect (effect (damage eid :net 1 {:card card}))}})

(define-card "SIU"
  {:derezzed-events [corp-rez-toast]
   :flags {:corp-phase-12 (req true)}
   :abilities [{:label "Trace 3 - Give the Runner 1 tag"
                :req (req (:corp-phase-12 @state))
                :async true
                :cost [:trash]
                :effect (effect (continue-ability
                                  {:trace {:base 3
                                           :label "Trace 3 - Give the Runner 1 tag"
                                           :successful {:msg "give the Runner 1 tag"
                                                        :async true
                                                        :effect (effect (gain-tags :runner eid 1))}}}
                                  card nil))}]})

(define-card "Snare!"
  {:flags {:rd-reveal (req true)}
   :access {:req (req (not= (first (:zone card)) :discard))
            :async true
            :effect (effect (show-wait-prompt :runner "Corp to use Snare!")
                            (continue-ability
                              {:optional
                               {:prompt "Pay 4 [Credits] to use Snare! ability?"
                                :end-effect (effect (clear-wait-prompt :runner))
                                :yes-ability {:async true
                                              :cost [:credit 4]
                                              :msg "do 3 net damage and give the Runner 1 tag"
                                              :effect (req (wait-for (damage state side :net 3 {:card card})
                                                                     (gain-tags state :corp eid 1)))}}}
                              card nil))}})

(define-card "Space Camp"
  {:flags {:rd-reveal (req true)}
   :access {:async true
            :effect (effect (show-wait-prompt :runner "Corp to use Space Camp")
                            (continue-ability
                              {:optional
                               {:prompt "Place 1 advancement token with Space Camp?"
                                :cancel-effect (req (clear-wait-prompt state :runner)
                                                    (effect-completed state side eid))
                                :yes-ability {:msg (msg "place 1 advancement token on " (card-str state target))
                                              :prompt "Select a card to place an advancement token on with Space Camp"
                                              :choices {:card can-be-advanced?}
                                              :effect (effect (add-prop target :advance-counter 1 {:placed true})
                                                              (clear-wait-prompt :runner))}
                                :no-ability {:effect (req (clear-wait-prompt state :runner)
                                                          (effect-completed state side eid))}}}
                              card nil))}})

(define-card "Storgotic Resonator"
  {:abilities [{:cost [:click 1 :power 1]
                :label "Do 1 net damage"
                :msg "do 1 net damage"
                :async true
                :effect (effect (damage eid :net 1 {:card card}))}]
   :events [{:event :corp-trash
             :once :per-turn
             :req (req (first-event?
                         state side :corp-trash
                         #(= (:faction (:identity runner)) (:faction (first %)))))
             :effect (effect (system-msg :corp "adds 1 power counter on Storgotic Resonator")
                             (add-counter card :power 1))}]})

(define-card "Student Loans"
  {:constant-effects [{:type :play-additional-cost
                       :req (req (and (event? target)
                                      (seq (filter #(= (:title %) (:title target)) (:discard runner)))))
                       :value [:credit 2]}]})

(define-card "Sundew"
  ; If this a run event then handle in :begin-run as we do not know the server
  ; being run on in :runner-spent-click.
  {:events [{:event :runner-spent-click
             :req (req (first-event? state side :runner-spent-click))
             :msg (req (if (not= :run (get-in @state [:runner :register :click-type]))
                         "gain 2 [Credits]"))
             :effect (req (if (not= :run (get-in @state [:runner :register :click-type]))
                            (gain-credits state :corp 2)))}
            {:event :begin-run
             :once :per-turn
             :req (req (first-event? state side :runner-spent-click))
             :msg (req (if (and (= :run (get-in @state [:runner :register :click-type]))
                                (not this-server))
                         "gain 2 [Credits]"))
             :effect (req (if (and (= :run (get-in @state [:runner :register :click-type]))
                                   (not this-server))
                            (gain-credits state :corp 2)))}]})

(define-card "Synth DNA Modification"
  {:implementation "Manual fire once subroutine is broken"
   :abilities [{:msg "do 1 net damage"
                :label "Do 1 net damage after AP subroutine broken"
                :once :per-turn
                :effect (effect (damage eid :net 1 {:card card}))}]})

(define-card "Team Sponsorship"
  {:events [{:event :agenda-scored
             :label "Install a card from Archives or HQ"
             :prompt "Select a card from Archives or HQ to install"
             :show-discard true
             :interactive (req true)
             :async true
             :choices {:card #(and (not (operation? %))
                                   (corp? %)
                                   (#{[:hand] [:discard]} (:zone %)))}
             :msg (msg (corp-install-msg target))
             :effect (effect (corp-install eid target nil {:ignore-install-cost true}))}]})

(define-card "Tech Startup"
  {:derezzed-events [corp-rez-toast]
   :flags {:corp-phase-12 (req true)}
   :abilities [{:label "Install an asset from R&D"
                :prompt "Choose an asset to install"
                :msg (msg "install " (:title target))
                :req (req (seq (filter asset? (:deck corp))))
                :choices (req (filter asset? (:deck corp)))
                :async true
                :effect (req (wait-for (trash state side card nil)
                                       (reveal state side target)
                                       (shuffle! state side :deck)
                                       (corp-install state side eid target nil nil)))}]})

(define-card "TechnoCo"
  (letfn [(is-techno-target [card]
            (or (program? card)
                (hardware? card)
                (and (resource? card)
                     (has-subtype? card "Virtual"))))]
    {:constant-effects [{:type :install-cost
                         :req (req (and (is-techno-target target)
                                        (not (:facedown (second targets)))))
                         :value 1}]
     :events [{:event :runner-install
               :req (req (and (is-techno-target target)
                              (not (:facedown (second targets)))))
               :msg "gain 1 [Credits]"
               :effect (req (gain-credits state :corp 1))}]}))

(define-card "Tenma Line"
  {:abilities [{:label "Swap 2 pieces of installed ICE"
                :cost [:click 1]
                :prompt "Select two pieces of ICE to swap positions"
                :choices {:card #(and (installed? %)
                                      (ice? %))
                          :max 2
                          :all true}
                :effect (req (when (= (count targets) 2)
                               (swap-ice state side (first targets) (second targets))))
                :msg (msg "swap the positions of "
                          (card-str state (first targets))
                          " and "
                          (card-str state (second targets)))}]})

(define-card "Test Ground"
  (letfn [(derez-card [advancements]
            {:async true
             :prompt "Derez a card"
             :choices {:card #(and (installed? %)
                                   (rezzed? %))}
             :effect (req (derez state side target)
                          (if (pos? (dec advancements))
                            (continue-ability state side (derez-card (dec advancements)) card nil)
                            (effect-completed state side eid)))})]
    {:advanceable :always
     :abilities [{:label "Derez 1 card for each advancement token"
                  :req (req (pos? (get-counters card :advancement)))
                  :msg (msg "derez " (quantify (get-counters card :advancement) "card"))
                  :cost [:trash]
                  :effect (req (let [advancements (get-counters card :advancement)]
                                 (show-wait-prompt state :runner (str "Corp to derez "
                                                                      (quantify advancements "card")))
                                 (wait-for (resolve-ability state side (derez-card advancements) card nil)
                                           (clear-wait-prompt state :runner))))}]}))

(define-card "Tiered Subscription"
  {:events [{:event :run
             :req (req (first-event? state side :run))
             :msg "gain 1 [Credits]"
             :effect (effect (gain-credits :corp 1))}]})

(define-card "The Board"
  {:effect (effect (update-all-agenda-points))
   :leave-play (effect (update-all-agenda-points))
   :trash-effect executive-trash-effect
   :constant-effects [{:type :agenda-value
                       :req (req (= :runner (:scored-side target)))
                       :value -1}]
   :events [{:event :card-moved
             :req (req (or (= :scored (first (:zone target)))
                           (= :scored (first (:zone (second targets))))))
             :effect (effect (update-all-agenda-points))}]})

(define-card "The News Now Hour"
  {:events [{:event :runner-turn-begins
             :effect (req (prevent-current state side))}]
   :effect (req (prevent-current state side))
   :leave-play (req (swap! state assoc-in [:runner :register :cannot-play-current] false))})

(define-card "The Root"
  {:recurring 3
   :interactions {:pay-credits {:req (req (or (= :advance (:source-type eid))
                                              (= :corp-install (:source-type eid))
                                              (= :rez (:source-type eid))))
                                :type :recurring}}})

(define-card "Thomas Haas"
  {:advanceable :always
   :abilities [{:label "Gain credits"
                :msg (msg "gain " (* 2 (get-counters card :advancement)) " [Credits]")
                :cost [:trash]
                :effect (effect (gain-credits (* 2 (get-counters card :advancement))))}]})

(define-card "Toshiyuki Sakai"
  (advance-ambush
    0
    {:async true
     :prompt "Select an asset or agenda in HQ"
     :choices {:card #(and (or (agenda? %)
                               (asset? %))
                           (in-hand? %))}
     :msg "swap it for an asset or agenda from HQ"
     :effect (req (let [c (get-counters card :advancement)
                        target (assoc target :advance-counter c)
                        server (zone->name (butlast (:zone card)))
                        index (:index card)]
                    (move state :corp card :hand)
                    (wait-for (corp-install state :corp target server {:index index})
                              (let [new-card async-result]
                                (continue-ability
                                  state :runner
                                  {:optional
                                   {:prompt "Access the newly installed card?"
                                    :yes-ability {:async true
                                                  :effect (effect (access-card eid new-card))}}}
                                  card nil)))))}
    "Swap Toshiyuki Sakai with an agenda or asset from HQ?"))

(define-card "Turtlebacks"
  {:events [{:event :server-created
             :msg "gain 1 [Credits]"
             :effect (effect (gain-credits 1))}]})

(define-card "Urban Renewal"
  {:effect (effect (add-counter card :power 3))
   :derezzed-events [corp-rez-toast]
   :events [{:event :corp-turn-begins
             :async true
             :effect (req (add-counter state side card :power -1)
                          (if (not (pos? (get-counters (get-card state card) :power)))
                            (wait-for (trash state side card nil)
                                      (system-msg state :corp "uses Urban Renewal to do 4 meat damage")
                                      (damage state side eid :meat 4 {:card card}))
                            (effect-completed state side eid)))}]})

(define-card "Vaporframe Fabricator"
  {:trash-effect {:async true
                  :choices {:card #(and (corp? %)
                                        (in-hand? %)
                                        (not (operation? %)))}
                  :msg (msg (corp-install-msg target))
                  :effect
                  (effect
                    (continue-ability
                      (let [card-to-install target]
                        {:async true
                         :prompt (str "Where to install " (:title card-to-install))
                         :choices (req (remove (set (zone->name (:zone card))) (installable-servers state card-to-install)))
                         :effect (effect (corp-install eid card-to-install target {:ignore-all-cost true}))})
                      card nil))}
   :abilities [{:label "Install 1 card"
                :async true
                :cost [:click 1]
                :once :per-turn
                :choices {:card #(and (corp? %)
                                      (in-hand? %)
                                      (not (operation? %)))}
                :msg (msg (corp-install-msg target))
                :effect (effect (corp-install eid target nil {:ignore-all-cost true}))}]})

(define-card "Victoria Jenkins"
  {:effect (req (lose state :runner :click-per-turn 1))
   :leave-play (req (gain state :runner :click-per-turn 1))
   :trash-effect executive-trash-effect})

(define-card "Wall To Wall"
  (let [all [{:msg "gain 1 [Credits]"
              :effect (effect (gain-credits 1))}
             {:msg "draw 1 card"
              :async true
              :effect (effect (draw eid 1 nil))}
             {:label "place 1 advancement token on a piece of ice"
              :msg (msg "place 1 advancement token on " (card-str state target))
              :prompt "Choose a piece of ice on which to place an advancement"
              :async true
              :choices {:card #(and (ice? %)
                                    (installed? %))}
              :cancel-effect (effect (effect-completed eid))
              :effect (effect (add-prop target :advance-counter 1 {:placed true})
                              (effect-completed eid))}
             {:label "add this asset to HQ"
              :msg "add it to HQ"
              :effect (effect (move card :hand))}
             {:msg "done"}]
        choice (fn choice [abis n]
                 {:prompt "Choose an ability to resolve"
                  :choices (map make-label abis)
                  :async true
                  :effect (req (let [chosen (some #(when (= target (make-label %)) %) abis)]
                                 (wait-for
                                   (resolve-ability state side chosen card nil)
                                   (if (and (pos? (dec n)) (not= "done" (:msg chosen)))
                                     (continue-ability state side (choice (remove-once #(= % chosen) abis) (dec n)) card nil)
                                     (effect-completed state side eid)))))})
        ability {:async true
                 :once :per-turn
                 :effect (effect (continue-ability (choice all (if (< 1 (count (filter asset? (all-active-installed state :corp))))
                                                                 1
                                                                 3)) card nil))}]
    {:derezzed-events [(assoc corp-rez-toast :event :runner-turn-ends)]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(define-card "Warden Fatuma"
  (let [new-sub {:label "[Warden Fatuma] Force the Runner to lose 1 [Click], if able"}]
    (letfn [(all-rezzed-bios [state]
              (filter #(and (ice? %)
                            (has-subtype? % "Bioroid")
                            (rezzed? %))
                      (all-installed state :corp)))
            (remove-one [cid state ice]
              (remove-extra-subs! state :corp ice cid))
            (add-one [cid state ice]
              (add-extra-sub! state :corp ice new-sub cid {:front true}))
            (update-all [state func]
              (doseq [i (all-rezzed-bios state)]
                (func state i)))]
      {:effect (req (system-msg
                      state :corp
                      "uses Warden Fatuma to add \"[Subroutine] The Runner loses [Click], if able\" before all other subroutines")
                 (update-all state (partial add-one (:cid card))))
       :leave-play (req (system-msg state :corp "loses Warden Fatuma additional subroutines")
                     (update-all state (partial remove-one (:cid card))))
       :sub-effect {:msg "force the Runner to lose 1 [Click], if able"
                    :effect (req (lose state :runner :click 1))}
       :events [{:event :rez
                 :req (req (and (ice? target)
                                (has-subtype? target "Bioroid")))
                 :effect (req (add-one (:cid card) state (get-card state target)))}]})))

(define-card "Watchdog"
  (letfn [(not-triggered? [state card] (no-event? state :runner :rez #(ice? (first %))))]
    {:constant-effects [{:type :rez-cost
                         :req (req (and (ice? target)
                                        (not-triggered? state card)))
                         :value (req (- (count-tags state)))}]
     :events [{:event :rez
               :req (req (and (ice? target)
                              (not-triggered? state card)))
               :msg (msg "reduce the rez cost of " (:title target) " by " (count-tags state) " [Credits]")}]}))

(define-card "Whampoa Reclamation"
  {:abilities [{:label "Add 1 card from Archives to the bottom of R&D"
                :once :per-turn
                :req (req (and (pos? (count (:hand corp)))
                               (pos? (count (:discard corp)))))
                :async true
                :cost [:trash-from-hand 1]
                :effect (effect (show-wait-prompt :runner "Corp to use Whampoa Reclamation")
                                (continue-ability
                                  {:prompt "Select a card in Archives to add to the bottom of R&D"
                                   :show-discard true
                                   :choices {:card #(and (in-discard? %)
                                                         (corp? %))}
                                   :msg (msg "trash 1 card from HQ and add "
                                             (if (:seen target) (:title target) "a card") " from Archives to the bottom of R&D")
                                   :effect (effect (move target :deck)
                                                   (clear-wait-prompt :runner))}
                                  card nil))}]})

(define-card "Worlds Plaza"
  {:abilities [{:label "Install an asset on Worlds Plaza"
                :req (req (< (count (:hosted card)) 3))
                :cost [:click 1]
                :prompt "Select an asset to install on Worlds Plaza"
                :choices {:card #(and (asset? %)
                                      (in-hand? %)
                                      (corp? %))}
                :msg (msg "host " (:title target))
                :async true
                :effect (req (wait-for (corp-install state side target card nil) ;; install target onto card
                                       (rez state side eid (last (:hosted (get-card state card)))
                                            {:cost-bonus -2})))}]})

(define-card "Zaibatsu Loyalty"
  {:interactions {:prevent [{:type #{:expose}
                             :req (req true)}]}
   :derezzed-events [{:event :pre-expose
                      :async true
                      :effect (req (let [etarget target]
                                     (continue-ability
                                       state side
                                       {:optional
                                        {:req (req (not (rezzed? card)))
                                         :player :corp
                                         :prompt (msg "The Runner is about to expose " (:title etarget) ". Rez Zaibatsu Loyalty?")
                                         :yes-ability {:effect (effect (rez card))}}}
                                       card nil)))}]
   :abilities [{:msg "prevent 1 card from being exposed"
                :cost [:credit 1]
                :effect (effect (expose-prevent 1))}
               {:msg "prevent 1 card from being exposed"
                :label "Prevent 1 card from being exposed"
                :cost [:trash]
                :effect (effect (expose-prevent 1))}]})

(define-card "Zealous Judge"
  {:rez-req (req tagged)
   :abilities [{:async true
                :label "Give the Runner 1 tag"
                :cost [:click 1 :credit 1]
                :msg (msg "give the Runner 1 tag")
                :effect (effect (gain-tags eid 1))}]})
