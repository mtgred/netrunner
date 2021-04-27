(ns game.cards.assets
  (:require [game.core :refer :all]
            [game.utils :refer :all]
            [jinteki.utils :refer :all]
            [clojure.pprint :as pprint]
            [clojure.string :as string]))

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
   {:access {:optional
             {:req (req (and installed (>= (:credit corp) cost)))
              :waiting-prompt (:waiting-prompt ability)
              :prompt prompt
              :yes-ability (dissoc ability :waiting-prompt)}}}))

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
                  :effect (req (wait-for (gain-credits state side (num-counters card))
                                         (add-counter state side eid card counter-type (- (num-counters card)) nil)))}]
     {:on-rez {:effect (req (add-counter state side card counter-type counters))}
      :derezzed-events [corp-rez-toast]
      :events [(trash-on-empty counter-type)
               (assoc ability :event :corp-turn-begins)]
      :abilities [ability]})))

(def executive-trash-effect
  {:when-inactive true
   :req (req (and (= side :runner)
                  (:accessed target)))
   :msg "add it to the Runner's score area as an agenda worth 2 agenda points"
   :async true
   :effect (req (as-agenda state :runner eid card 2))})

;; Card definitions

(defcard "Adonis Campaign"
  (campaign 12 3))

(defcard "Advanced Assembly Lines"
  {:on-rez {:async true
            :msg "gain 3 [Credits]"
            :effect (effect (gain-credits eid 3))}
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

(defcard "Aggressive Secretary"
  (advance-ambush 2 {:req (req (pos? (get-counters (get-card state card) :advancement)))
                     :waiting-prompt "Corp to use Aggressive Secretary"
                     :prompt (msg "Choose " (quantify (get-counters (get-card state card) :advancement) "program") " to trash")
                     :cost [:credit 2]
                     :choices {:max (req (get-counters (get-card state card) :advancement))
                               :card #(and (installed? %)
                                           (program? %))}
                     :msg (msg "trash " (string/join ", " (map :title targets)))
                     :async true
                     :effect (effect (trash-cards eid targets))}))

(defcard "Alexa Belsky"
  {:abilities [{:label "Shuffle all cards in HQ into R&D"
                :async true
                :cost [:trash]
                :effect (effect
                          (continue-ability
                            {:waiting-prompt "Runner to decide whether or not to prevent Alexa Belsky"
                             :prompt "Prevent Alexa Belsky from shuffling back in 1 card for every 2 [Credits] spent. How many credits?"
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
                                            (shuffle-into-deck state :corp :hand)))}
                            card nil))}]})

(defcard "Alix T4LB07"
  {:events [{:event :corp-install
             :effect (effect (add-counter card :power 1))}]
   :abilities [{:label "Gain 2 [Credits] for each counter on Alix T4LB07"
                :cost [:click 1 :trash]
                :msg (msg "gain " (* 2 (get-counters card :power)) " [Credits]")
                :async true
                :effect (effect (gain-credits eid (* 2 (get-counters card :power))))}]})

(defcard "Allele Repression"
  (letfn [(select-archives-cards [total]
            {:async true
             :show-discard true
             :prompt (str "Select " (quantify total "card") " from Archives")
             :choices {:card #(and (corp? %)
                                   (in-discard? %))
                       :max total
                       :all true}
             :effect (effect (complete-with-result eid targets))})
          (select-hq-cards [total]
            {:async true
             :prompt (str "Select " (quantify total "card") " from HQ")
             :choices {:card #(and (corp? %)
                                   (in-hand? %))
                       :max total
                       :all true}
             :effect (effect (complete-with-result eid targets))})]
    {:advanceable :always
     :abilities [{:label "Swap 1 card in HQ and Archives for each advancement token"
                  :cost [:trash]
                  :msg (msg "swap "
                         (quantify (max (count (:discard corp))
                                        (count (:hand corp))
                                        (get-counters card :advancement))
                                   "card")
                         " in HQ and Archives")
                  :async true
                  :waiting-prompt "Corp to use Allele Repression"
                  :effect (req (let [total (min (count (:discard corp))
                                                (count (:hand corp))
                                                (get-counters card :advancement))]
                                 (wait-for (resolve-ability state side (select-hq-cards total) card nil)
                                           (let [hq-cards async-result]
                                             (wait-for (resolve-ability state side (select-archives-cards total) card nil)
                                                       (let [archives-cards async-result]
                                                         (doseq [[hq-card archives-card] (map vector hq-cards archives-cards)]
                                                           (swap-cards state side hq-card archives-card)))
                                                       (effect-completed state side eid))))))}]}))

(defcard "Amani Senai"
  (letfn [(senai-ability [agenda]
            {:interactive (req true)
             :optional {:prompt "Trace with Amani Senai?"
                        :player :corp
                        :autoresolve (get-autoresolve :auto-fire)
                        :yes-ability {:trace {:base (get-advancement-requirement agenda)
                                              :successful
                                              {:choices {:card #(and (installed? %)
                                                                     (runner? %))}
                                               :label "add an installed card to the Grip"
                                               :msg (msg "add " (:title target) " to the Runner's Grip")
                                               :effect (effect (move :runner target :hand))}}}}})]
    {:events [{:event :agenda-scored
               :interactive (req true)
               :async true
               :effect (effect (continue-ability (senai-ability (:card context)) card nil))}
              {:event :agenda-stolen
               :interactive (req true)
               :async true
               :effect (effect (continue-ability (senai-ability (:card context)) card nil))}]
     :abilities [(set-autoresolve :auto-fire "whether to fire Amani Senai")]}))

(defcard "Anson Rose"
  (let [ability {:label "Place 1 advancement token on Anson Rose (start of turn)"
                 :once :per-turn
                 :effect (effect (system-msg (str "places 1 advancement counter on Anson Rose"))
                                 (add-prop card :advance-counter 1 {:placed true}))}]
    {:derezzed-events [corp-rez-toast]
     :flags {:corp-phase-12 (req true)}
     :events [(assoc ability :event :corp-turn-begins)
              {:event :rez
               :req (req (and (ice? (:card context))
                              (pos? (get-counters card :advancement))))
               :async true
               :effect (req (let [ice (get-card state (:card context))
                                  icename (:title ice)]
                              (continue-ability
                                state side
                                {:optional
                                 {:waiting-prompt "Corp to use Anson Rose"
                                  :prompt (str "Move advancement tokens from Anson Rose to " icename "?")
                                  :yes-ability
                                  {:prompt "Choose how many advancement tokens to remove from Anson Rose"
                                   :choices {:number (req (get-counters card :advancement))}
                                   :effect (effect (add-prop :corp ice :advance-counter target {:placed true})
                                                   (add-prop :corp card :advance-counter (- target) {:placed true})
                                                   (system-msg (str "uses Anson Rose to move " target
                                                                    " advancement tokens to " (card-str state ice))))}}}
                                card nil)))}]
     :abilities [ability]}))

(defcard "API-S Keeper Isobel"
  (letfn [(counters-available? [state] (some #(pos? (get-counters % :advancement)) (all-installed state :corp)))]
    {:flags {:corp-phase-12 (req (counters-available? state))}
     :abilities [{:req (req (and (:corp-phase-12 @state)
                                 (counters-available? state)))
                  :once :per-turn
                  :label "Remove an advancement token (start of turn)"
                  :prompt "Select a card to remove an advancement token from"
                  :choices {:card #(and (pos? (get-counters % :advancement))
                                     (installed? %))}
                  :async true
                  :effect (req (let [cnt (get-counters target :advancement)]
                                 (set-prop state side target :advance-counter (dec cnt))
                                 (system-msg state :corp (str "uses API-S Keeper Isobel to remove an advancement token from "
                                                              (card-str state target) " and gains 3 [Credits]"))
                                 (gain-credits state :corp eid 3)))}]}))

(defcard "Aryabhata Tech"
  {:events [{:event :successful-trace
             :msg "gain 1 [Credit] and force the Runner to lose 1 [Credit]"
             :async true
             :effect (req (wait-for (gain-credits state side 1)
                                    (lose-credits state :runner eid 1)))}]})

(defcard "Bass CH1R180G4"
  {:abilities [{:cost [:click 1 :trash]
                :msg "gain [Click][Click]"
                :effect (effect (gain :click 2))}]})

(defcard "Bio-Ethics Association"
  (let [ability {:req (req unprotected)
                 :async true
                 :label "Do 1 net damage (start of turn)"
                 :once :per-turn
                 :msg "do 1 net damage"
                 :effect (effect (damage eid :net 1 {:card card}))}]
    {:derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Bioroid Work Crew"
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

(defcard "Blacklist"
  {:on-rez {:effect (effect (lock-zone (:cid card) :runner :discard))}
   :leave-play (effect (release-zone (:cid card) :runner :discard))})

(defcard "Brain-Taping Warehouse"
  {:constant-effects [{:type :rez-cost
                       :req (req (and (ice? target)
                                      (has-subtype? target "Bioroid")))
                       :value (req (- (:click runner)))}]})

(defcard "Breached Dome"
  {:flags {:rd-reveal (req true)}
   :access {:async true
            :effect (req (let [c (first (get-in @state [:runner :deck]))]
                           (system-msg state :corp (str "uses Breached Dome to do one meat damage and to trash " (:title c)
                                                        " from the top of the Runner's Stack"))
                           (wait-for (mill state :corp :runner 1)
                                     (damage state side eid :meat 1 {:card card}))))}})

(defcard "Broadcast Square"
  {:events [{:event :pre-bad-publicity
             :async true
             :trace {:base 3
                     :successful {:msg "prevents all bad publicity"
                                  :effect (effect (bad-publicity-prevent Integer/MAX_VALUE))}}}]})

(defcard "C.I. Fund"
  {:derezzed-events [corp-rez-toast]
   :flags {:corp-phase-12 (req (pos? (:credit corp)))}
   :abilities [{:label "Move up to 3 [Credit] from credit pool to C.I. Fund"
                :prompt "Choose how many [Credit] to move"
                :once :per-turn
                :choices {:number (req (min (:credit corp) 3))}
                :async true
                :effect (effect (add-counter card :credit target)
                                (lose-credits eid target))
                :msg (msg "move " target " [Credit] to C.I. Fund")}
               {:label "Take all credits from C.I. Fund"
                :cost [:credit 2 :trash]
                :msg (msg "trash it and gain " (get-counters card :credit) " [Credits]")
                :async true
                :effect (effect (gain-credits eid (get-counters card :credit)))}]
   :events [{:event :corp-turn-begins
             :req (req (>= (get-counters card :credit) 6))
             :effect (effect (add-counter card :credit 2)
                             (system-msg (str "adds 2 [Credits] to C.I. Fund")))}]})

(defcard "Calvin B4L3Y"
  {:abilities [{:cost [:click 1]
                :msg "draw 2 cards"
                :once :per-turn
                :async true
                :effect (effect (draw eid 2 nil))}]
   :on-trash {:interactive (req true)
              :optional
              {:req (req (= :runner side))
               :waiting-prompt "Corp to use Calvin B4L3Y"
               :prompt "Draw 2 cards?"
               :player :corp
               :yes-ability {:msg "draw 2 cards"
                             :effect (effect (draw eid 2 nil))}}}})

(defcard "Capital Investors"
  {:abilities [{:cost [:click 1]
                :msg "gain 2 [Credits]"
                :keep-open :while-clicks-left
                :async true
                :effect (effect (gain-credits eid 2))}]})

(defcard "Cerebral Overwriter"
  (advance-ambush 3 {:async true
                     :waiting-prompt "Corp to use Cerebral Overwriter"
                     :req (req (pos? (get-counters (get-card state card) :advancement)))
                     :msg (msg "do " (get-counters (get-card state card) :advancement) " brain damage")
                     :effect (effect (damage eid :brain (get-counters (get-card state card) :advancement) {:card card}))}))

(defcard "Chairman Hiro"
  {:constant-effects [(runner-hand-size+ -2)]
   :on-trash executive-trash-effect})

(defcard "Chief Slee"
  {:events [{:event :end-of-encounter
             :msg "add 1 power counter to Chief Slee"
             :effect (effect (add-counter :corp card :power (count (remove :broken (:subroutines (:ice context))))))}]
   :abilities [{:cost [:click 1 :power 5]
                :keep-open :while-5-power-tokens-left
                :async true
                :msg "do 5 meat damage"
                :effect (effect (damage eid :meat 5 {:card card}))}]})

(defcard "City Surveillance"
  {:derezzed-events [corp-rez-toast]
   :flags {:runner-phase-12 (req (pos? (:credit runner)))}
   :events [{:event :runner-turn-begins
             :player :runner
             :prompt "Pay 1 [Credits] or take 1 tag"
             :choices (req [(when (pos? (:credit runner))
                              "Pay 1 [Credits]")
                            "Take 1 tag"])
             :msg "make the Runner pay 1 [Credits] or take 1 tag"
             :async true
             :effect (req (case target
                            "Pay 1 [Credits]"
                            (wait-for (pay state :runner card :credit 1)
                                      (when-let [payment-str (:msg async-result)]
                                        (system-msg state :runner payment-str))
                                      (effect-completed state side eid))
                            (do (system-msg state :runner "takes 1 tag")
                                (gain-tags state :corp eid 1))))}]})

(defcard "Clearinghouse"
  (let [ability {:once :per-turn
                 :async true
                 :label "Trash this asset to do 1 meat damage for each hosted advancement counter (start of turn)"
                 :req (req (:corp-phase-12 @state))
                 :effect
                 (effect
                  (continue-ability
                   {:optional
                    {:prompt (msg "Trash Clearinghouse to do " (get-counters card :advancement) " meat damage?")
                     :yes-ability
                     {:async true
                      :msg "do 1 meat damage for each hosted advancement counter"
                      :effect (req (wait-for
                                    (trash state side card nil)
                                    (damage state side eid :meat (get-counters card :advancement) {:card card})))}}}
                   card nil))}]
    {:derezzed-events [corp-rez-toast]
     :flags {:corp-phase-12 (req true)}
     :events [(assoc ability :event :corp-turn-begins)]
     :advanceable :always
     :abilities [ability]}))

(defcard "Clone Suffrage Movement"
  {:derezzed-events [corp-rez-toast]
   :flags {:corp-phase-12 (req (and (some operation? (:discard corp))
                                 unprotected))}
   :abilities [(into
                 (corp-recur operation?)
                 {:label "Add 1 operation from Archives to HQ"
                  :waiting-prompt "Corp to use Clone Suffrage Movement"
                  :prompt "Select an operation in Archives to add to HQ"
                  :once :per-turn})]})

(defcard "Clyde Van Rite"
  (let [ability {:async true
                 :req (req (or (pos? (:credit runner))
                               (pos? (count (:deck runner)))))
                 :player :runner
                 :once :per-turn
                 :prompt "Pay 1 [Credits] or trash the top card of the Stack"
                 :choices (req [(when (pos? (:credit runner))
                                  "Pay 1 [Credits]")
                                (when (pos? (count (:deck runner)))
                                  "Trash top card")])
                 :msg "make the Runner pay 1 [Credits] or trash the top card of the Stack"
                 :effect (req (case target
                                "Pay 1 [Credits]"
                                (wait-for (pay state side card :credit 1)
                                          (when-let [payment-str (:msg async-result)]
                                            (system-msg state side payment-str))
                                          (effect-completed state side eid))
                                "Trash top card"
                                (do (system-msg state side "trashes the top card of the Stack")
                                    (mill state :runner eid :runner 1))))}]
    {:derezzed-events [corp-rez-toast]
     :flags {:corp-phase-12 (req true)}
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Commercial Bankers Group"
  (let [ability {:req (req unprotected)
                 :label "Gain 3 [Credits] (start of turn)"
                 :once :per-turn
                 :msg "gain 3 [Credits]"
                 :async true
                 :effect (effect (gain-credits eid 3))}]
    {:derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Constellation Protocol"
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
                :waiting-prompt "Corp to use Constellation Protocol"
                :choices {:card #(and (ice? %)
                                      (get-counters % :advancement))}
                :effect (effect
                          (continue-ability
                            (let [from-ice target]
                              {:prompt "Move to where?"
                               :choices {:card #(and (ice? %)
                                                     (not (same-card? from-ice %))
                                                     (can-be-advanced? %))}
                               :msg (msg "move an advancement token from "
                                         (card-str state from-ice)
                                         " to "
                                         (card-str state target))
                               :effect (effect (add-prop :corp target :advance-counter 1)
                                               (add-prop :corp from-ice :advance-counter -1))})
                            card nil))}]})

(defcard "Contract Killer"
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

(defcard "Corporate Town"
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

(defcard "CPC Generator"
  {:events [{:event :runner-click-credit
             :req (req (first-event? state side :runner-click-credit))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits :corp eid 1))}]})

(defcard "CSR Campaign"
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

(defcard "Cybernetics Court"
  {:constant-effects [(corp-hand-size+ 4)]})

(defcard "Daily Business Show"
  {:derezzed-events [corp-rez-toast]
   :events [{:event :pre-corp-draw
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
                            (continue-ability
                              state side
                              {:waiting-prompt "Corp to use Daily Business Show"
                               :prompt (str "Select " (quantify dbs "card") " to add to the bottom of R&D")
                               :choices {:max dbs
                                         :card #(some (fn [c] (same-card? c %)) drawn)
                                         :all true}
                               :effect (req (doseq [c (reverse targets)]
                                              (system-msg state side (str "uses Daily Business Show to add the "
                                                                          (pprint/cl-format nil "~:R" (inc (first (keep-indexed #(when (same-card? c %2) %1) drawn))))
                                                                          " card drawn to the bottom of R&D"))
                                              (move state side c :deck)))}
                              card nil)))}]})

(defcard "Daily Quest"
  (let [ability {:req (req (not (some (into #{}
                                            [(second (get-zone card))
                                             (second (get-zone (:host card)))])
                                      (:successful-run runner-reg-last))))
                 :msg "gain 3 [Credits]"
                 :async true
                 :effect (effect (gain-credits :corp eid 3))}]
    {:rez-req (req (= (:active-player @state) :corp))
     :events [{:event :successful-run
               :req (req this-server)
               :async true
               :effect (effect (system-msg :runner (str "gains 2 [Credits] for a successful run "
                                                        "on the Daily Quest server"))
                               (gain-credits :runner eid 2))}
              (assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Dedicated Response Team"
  {:events [{:event :run-ends
             :req (req (and tagged (:successful target)))
             :msg "do 2 meat damage"
             :async true
             :effect (effect (damage eid :meat 2 {:card card}))}]})

(defcard "Dedicated Server"
  {:recurring 2
   :interactions {:pay-credits {:req (req (and (= :rez (:source-type eid))
                                               (ice? target)))
                                :type :recurring}}})

(defcard "Director Haas"
  {:in-play [:click-per-turn 1]
   :on-trash executive-trash-effect})

(defcard "Docklands Crackdown"
  {:abilities [{:cost [:click 2]
                :keep-open :while-2-clicks-left
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
             :msg (msg "increase the install cost of " (:title (:card context))
                       " by " (get-counters card :power) " [Credits]")
             :effect (req (swap! state assoc-in [:per-turn (:cid card)] true))}]})

(defcard "Drudge Work"
  {:on-rez {:effect (effect (add-counter card :power 3))}
   :events [(trash-on-empty :power)]
   :abilities [{:cost [:click 1 :power 1]
                :choices {:card #(and (agenda? %)
                                      (or (in-hand? %)
                                          (in-discard? %)))}
                :label "Reveal an agenda from HQ or Archives"
                :msg (msg "reveal " (:title target) " from " (zone->name (get-zone target))
                          (let [target-agenda-points (get-agenda-points target)]
                            (str ", gain " target-agenda-points " [Credits], "))
                          " and shuffle it into R&D")
                :async true
                :effect (req (wait-for
                               (reveal state side target)
                               (wait-for
                                 (gain-credits state :corp (get-agenda-points target))
                                 (move state :corp target :deck)
                                 (shuffle! state :corp :deck)
                                 (effect-completed state side eid))))}]})

(defcard "Early Premiere"
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

(defcard "Echo Chamber"
  {:abilities [{:label "Add Echo Chamber to your score area as an agenda worth 1 agenda point"
                :cost [:click 3]
                :msg "add it to their score area as an agenda worth 1 agenda point"
                :async true
                :effect (req (as-agenda state :corp eid card 1))}]})

(defcard "Edge of World"
  (letfn [(ice-count [state]
            (count (get-in (:corp @state) [:servers (last (:server (:run @state))) :ices])))]
    (installed-access-trigger 3 {:msg (msg "do " (ice-count state) " brain damage")
                                 :async true
                                 :effect (effect (damage eid :brain (ice-count state)
                                                         {:card card}))})))

(defcard "Eliza's Toybox"
  {:abilities [{:cost [:click 3]
                :keep-open :while-3-clicks-left
                :choices {:card #(not (:rezzed %))}
                :label "Rez a card at no cost" :msg (msg "rez " (:title target) " at no cost")
                :async true
                :effect (effect (rez eid target {:ignore-cost :all-costs}))}]})

(defcard "Elizabeth Mills"
  {:on-rez {:msg "remove 1 bad publicity"
            :effect (effect (lose-bad-publicity 1))}
   :abilities [{:cost [:click 1 :trash]
                :label "Trash a location"
                :msg (msg "trash " (:title target) " and take 1 bad publicity")
                :choices {:card #(has-subtype? % "Location")}
                :async true
                :effect (req (wait-for (trash state side target nil)
                                       (gain-bad-publicity state :corp eid 1)))}]})

(defcard "Encryption Protocol"
  {:constant-effects [{:type :trash-cost
                       :req (req (installed? target))
                       :value 1}]})

(defcard "Estelle Moon"
  {:events [{:event :corp-install
             :req (req (and (or (asset? (:card context))
                                (agenda? (:card context))
                                (upgrade? (:card context)))
                            (is-remote? (second (get-zone (:card context))))))
             :effect (effect (add-counter card :power 1)
                             (system-msg "places 1 power counter on Estelle Moon"))}]
   :abilities [{:label "Draw 1 card and gain 2 [Credits] for each power counter"
                :cost [:trash]
                :async true
                :effect (req (let [counters (get-counters card :power)
                                   credits (* 2 counters)]
                               (system-msg state side (str "uses Estelle Moon to draw " counters
                                                           " cards and gain " credits " [Credits]"))
                               (wait-for (draw state side counters nil)
                                         (gain-credits state side eid credits))))}]})

(defcard "Eve Campaign"
  (campaign 16 2))

(defcard "Executive Boot Camp"
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
                                       (update! state side (assoc card :ebc-rezzed (:cid target)))
                                       (effect-completed state side eid)))}
               {:prompt "Choose an asset to reveal and add to HQ"
                :msg (msg "reveal " (:title target) " and add it to HQ")
                :activatemsg "searches R&D for an asset"
                :choices (req (cancellable (filter asset?
                                                   (:deck corp))
                                           :sorted))
                :cost [:credit 1 :trash]
                :label "Search R&D for an asset"
                :async true
                :effect (req (wait-for
                               (reveal state side target)
                               (shuffle! state side :deck)
                               (move state side target :hand)
                               (effect-completed state side eid)))}]})

(defcard "Executive Search Firm"
  {:abilities [{:prompt "Choose an Executive, Sysop, or Character to add to HQ"
                :msg (msg "add " (:title target) " to HQ and shuffle R&D")
                :activatemsg "searches R&D for an Executive, Sysop, or Character"
                :choices (req (cancellable (filter #(or (has-subtype? % "Executive")
                                                        (has-subtype? % "Sysop")
                                                        (has-subtype? % "Character"))
                                                   (:deck corp))
                                           :sorted))
                :cost [:click 1]
                :keep-open :while-clicks-left
                :label "Search R&D for an Executive, Sysop, or Character"
                :effect (effect (move target :hand)
                                (shuffle! :deck))}]})

(defcard "Exposé"
  {:advanceable :always
   :abilities [{:label "Remove 1 bad publicity for each advancement token on Exposé"
                :msg (msg "remove " (get-counters card :advancement) " bad publicity")
                :cost [:trash]
                :effect (effect (lose-bad-publicity (get-counters card :advancement)))}]})

(defcard "False Flag"
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

(defcard "Franchise City"
  {:events [{:event :access
             :req (req (agenda? target))
             :msg "add it to their score area as an agenda worth 1 agenda point"
             :async true
             :effect (req (as-agenda state :corp eid card 1))}]})

(defcard "Full Immersion RecStudio"
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

(defcard "Fumiko Yamamori"
  {:events [{:event :reveal-spent-credits
             :async true
             :req (req (and (some? (first targets))
                            (some? (second targets))
                            (not= (first targets) (second targets))))
             :msg "do 1 meat damage"
             :effect (effect (damage eid :meat 1 {:card card}))}]})

(defcard "Gene Splicer"
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

(defcard "Genetics Pavilion"
  {:on-rez {:msg "prevent the Runner from drawing more than 2 cards during their turn"
            :effect (req (max-draw state :runner 2)
                         (when (zero? (remaining-draws state :runner))
                           (prevent-draw state :runner)))}
   :events [{:event :runner-turn-begins
             :effect (effect (max-draw :runner 2))}]
   :leave-play (req (swap! state update-in [:runner :register] dissoc :max-draw :cannot-draw))})

(defcard "Ghost Branch"
  (advance-ambush 0 {:async true
                     :waiting-prompt "Corp to use Ghost Branch"
                     :req (req (pos? (get-counters (get-card state card) :advancement)))
                     :msg (msg "give the Runner " (quantify (get-counters (get-card state card) :advancement) "tag"))
                     :effect (effect (gain-tags :corp eid (get-counters (get-card state card) :advancement)))}))

(defcard "GRNDL Refinery"
  {:advanceable :always
   :abilities [{:label "Gain 4 [Credits] for each advancement token on GRNDL Refinery"
                :cost [:click 1 :trash]
                :msg (msg "gain " (* 4 (get-counters card :advancement)) " [Credits]")
                :async true
                :effect (effect (gain-credits eid (* 4 (get-counters card :advancement))))}]})

(defcard "Haas Arcology AI"
  {:advanceable :while-unrezzed
   :abilities [{:label "Gain [Click][Click]"
                :once :per-turn
                :msg "gain [Click][Click]"
                :cost [:click 1 :advancement 1]
                :effect (effect (gain :click 2))}]})

(defcard "Honeyfarm"
  {:flags {:rd-reveal (req true)}
   :access {:msg "force the Runner to lose 1 [Credits]"
            :async true
            :effect (effect (lose-credits :runner eid 1))}})

(defcard "Hostile Infrastructure"
  (let [ability
        {:event :runner-trash
         :async true
         :once-per-instance false
         :req (req (corp? (:card target)))
         :msg "do 1 net damage"
         :effect (effect (damage :corp eid :net 1 {:card card}))}]
    {:on-trash (assoc ability :req (req (= :runner side)))
     :events [ability]}))

(defcard "Hyoubu Research Facility"
  {:events [{:event :reveal-spent-credits
             :req (req (some? (first targets)))
             :once :per-turn
             :msg (msg "gain " target " [Credits]")
             :async true
             :effect (effect (gain-credits :corp eid target))}]})

(defcard "Ibrahim Salem"
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
                        :msg (msg "reveal " (string/join ", " (map :title (:hand runner))))
                        :async true
                        :effect (effect (continue-ability (trash-ability target) card nil))}]
    {:additional-cost [:forfeit]
     :flags {:corp-phase-12 (constantly true)}
     :derezzed-events [corp-rez-toast]
     :abilities [choose-ability]}))

(defcard "Illegal Arms Factory"
  (let [ability {:msg "gain 1 [Credits] and draw 1 card"
                 :label "Gain 1 [Credits] and draw 1 card (start of turn)"
                 :once :per-turn
                 :async true
                 :req (req (:corp-phase-12 @state))
                 :effect (req (wait-for (gain-credits state side 1)
                                        (draw state side eid 1 nil)))}]
    {:derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]
     :on-trash {:req (req (= side :runner))
                :msg "take 1 bad publicity"
                :effect (effect (gain-bad-publicity :corp 1))}}))

(defcard "Indian Union Stock Exchange"
  {:events [{:event :play-operation
             :req (req (not= (:faction (:card context)) (:faction (:identity corp))))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits eid 1))}
            {:event :rez
             :req (req (not= (:faction (:card context)) (:faction (:identity corp))))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits eid 1))}]})

(defcard "Isabel McGuire"
  {:abilities [{:label "Add an installed card to HQ"
                :cost [:click 1]
                :keep-open :while-clicks-left
                :choices {:card installed?}
                :msg (msg "move " (card-str state target) " to HQ")
                :effect (effect (move target :hand))}]})

(defcard "IT Department"
  {:abilities [{:cost [:click 1]
                :keep-open :while-clicks-left
                :msg "add 1 counter"
                :effect (effect (add-counter card :power 1))}
               {:cost [:power 1]
                :keep-open :while-power-tokens-left
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

(defcard "Jackson Howard"
  {:abilities [{:cost [:click 1]
                :keep-open :while-clicks-left
                :msg "draw 2 cards"
                :effect (effect (draw 2))}
               {:label "Shuffle up to 3 cards from Archives into R&D"
                :cost [:remove-from-game]
                :async true
                :effect (effect (shuffle-into-rd-effect eid card 3))}]})

(defcard "Jeeves Model Bioroids"
  (let [ability {:label "Gain [Click]"
                 :msg "gain [Click]"
                 :once :per-turn
                 :effect (effect (gain :click 1))}
        cleanup (effect (update! (dissoc card :seen-this-turn)))]
    {:abilities [ability]
     :leave-play cleanup
     :events [{:event :corp-spent-click
               :effect (req (let [cid (first target)
                                  ability-idx (:ability-idx (:source-info eid))
                                  bac-cid (get-in @state [:corp :basic-action-card :cid])
                                  cause (if (keyword? (first target))
                                          (case (first target)
                                            :play-instant [bac-cid 3]
                                            :corp-click-install [bac-cid 2]
                                            (first target)) ; in clojure there's: (= [1 2 3] '(1 2 3))
                                          [cid ability-idx])
                                  clicks-spent (+ (get-in card [:seen-this-turn cause] 0) (second targets))]
                              (update! state side (assoc-in card [:seen-this-turn cause] clicks-spent))
                              (when (>= clicks-spent 3) ; can be >= 3 because :once :per-turn on ability
                                (resolve-ability state side ability (get-card state card) nil))))}
              {:event :corp-turn-ends
               :effect cleanup}]}))

(defcard "Kala Ghoda Real TV"
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

(defcard "Kuwinda K4H1U3"
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

(defcard "Lady Liberty"
  {:abilities [{:cost [:click 3]
                :keep-open :while-3-clicks-left
                :label "Add agenda from HQ to score area"
                :req (req (let [counters (get-counters (get-card state card) :power)]
                            (some #(and (agenda? %)
                                        (= counters (:agendapoints %)))
                                  (:hand corp))))
                :waiting-prompt "Corp to select an agenda for Lady Liberty"
                :prompt "Select an Agenda in HQ to move to score area"
                :choices {:req (req (and (agenda? target)
                                         (= (:agendapoints target) (get-counters (get-card state card) :power))
                                         (in-hand? target)))}
                :msg (msg "add " (:title target) " to score area")
                :async true
                :effect (effect (as-agenda eid target (:agendapoints target) {:register-events true}))}]
   :events [{:event :corp-turn-begins
             :effect (effect (add-counter card :power 1))}]})

(defcard "Lakshmi Smartfabrics"
  {:events [{:event :rez
             :effect (effect (add-counter card :power 1))}]
   :abilities [{:req (req (seq (filter #(and (agenda? %)
                                             (>= (get-counters card :power)
                                                 (:agendapoints %)))
                                       (:hand corp))))
                :label "Reveal an agenda worth X points from HQ"
                :async true
                :cost [:x-power]
                :keep-open :while-power-tokens-left
                :effect
                (effect
                  (continue-ability
                    {:prompt "Select an agenda in HQ to reveal"
                     :choices {:req (req (and (agenda? target)
                                              (<= (:agendapoints target) (cost-value eid :x-power))))}
                     :msg (msg "reveal " (:title target) " from HQ")
                     :effect (req (wait-for (reveal state side target)
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
                                              (effect-completed state side eid))))}
                    card nil))}]})

(defcard "Launch Campaign"
  (campaign 6 2))

(defcard "Levy University"
  {:abilities [{:prompt "Choose an ICE"
                :msg (msg "adds " (:title target) " to HQ")
                :choices (req (cancellable (filter ice? (:deck corp)) :sorted))
                :label "Search R&D for a piece of ICE"
                :cost [:click 1 :credit 1]
                :keep-open :while-clicks-left
                :effect (effect (move target :hand)
                                (shuffle! :deck))}]})

(defcard "Lily Lockwell"
  {:on-rez {:async true
            :effect (effect (draw eid 3 nil))
            :msg (msg "draw 3 cards")}
   :abilities [{:label "Search R&D for an operation"
                :prompt "Choose an operation to put on top of R&D"
                :cost [:click 1 :tag 1]
                :msg (msg (if (= target "No action")
                            "search R&D, but does not find an operation"
                            (str "put " (:title target) " on top of R&D")))
                :choices (req (conj (vec (sort-by :title (filter operation? (:deck corp)))) "No action"))
                :async true
                :effect (req (if (= target "No action")
                               (do (shuffle! state :corp :deck)
                                   (effect-completed state side eid))
                               (wait-for
                                 (reveal state side target)
                                 (shuffle! state :corp :deck)
                                 (move state :corp target :deck {:front true})
                                 (effect-completed state side eid))))}]})

(defcard "Long-Term Investment"
  {:derezzed-events [corp-rez-toast]
   :abilities [{:label "Move any number of [Credits] to your credit pool"
                :req (req (>= (get-counters card :credit) 8))
                :cost [:click 1]
                :prompt "How many [Credits]?"
                :choices {:counter :credit}
                :msg (msg "gain " target " [Credits]")
                :async true
                :effect (effect (gain-credits eid target))}]
   :events [{:event :corp-turn-begins
             :effect (effect (add-counter card :credit 2)
                             (system-msg (str "adds 2 [Credit] to Long-Term Investment")))}]})

(defcard "Lt. Todachine"
  {:events [{:event :rez
             :req (req (ice? (:card context)))
             :async true
             :msg "to give the Runner 1 tag"
             :effect (req (gain-tags state :runner eid 1))}]})

(defcard "Malia Z0L0K4"
  (let [re-enable-target (req (when-let [malia-target (:malia-target card)]
                                (when (:disabled (get-card state malia-target))
                                  (system-msg state side (str "uses "  (:title card) " to unblank "
                                                              (card-str state malia-target)))
                                  (enable-card state :runner (get-card state malia-target))
                                  (when-let [reactivate-effect (:reactivate (card-def malia-target))]
                                    (resolve-ability state :runner reactivate-effect (get-card state malia-target) nil)))))]
    {:on-rez {:msg (msg (str "blank the text box of " (card-str state target)))
              :choices {:card #(and (runner? %)
                                    (installed? %)
                                    (resource? %)
                                    (not (has-subtype? % "Virtual")))}
              :effect (effect (update! (assoc card :malia-target target))
                              (disable-card :runner target))}
     :leave-play re-enable-target
     :move-zone re-enable-target}))

(defcard "Marilyn Campaign"
  (let [ability {:once :per-turn
                 :interactive (req (>= 2 (get-counters card :credit)))
                 :req (req (:corp-phase-12 @state))
                 :label (str "Gain 2 [Credits] (start of turn)")
                 :msg (msg "gain " (min 2 (get-counters card :credit)) " [Credits]")
                 :async true
                 :effect (req (let [credits (min 2 (get-counters card :credit))]
                                (add-counter state side card :credit (- credits))
                                (wait-for (gain-credits state :corp credits)
                                          (if (not (pos? (get-counters (get-card state card) :credit)))
                                            (trash state :corp eid card {:unpreventable true})
                                            (effect-completed state :corp eid)))))}]
    {:derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :on-rez {:effect (req (add-counter state side card :credit 8))}
     :abilities [(set-autoresolve :auto-reshuffle "Marilyn reshuffle")]
     :on-trash {:interactive (req true)
                :optional
                {:waiting-prompt "Corp to use Marilyn Campaign"
                 :prompt "Shuffle Marilyn Campaign into R&D?"
                 :autoresolve (get-autoresolve :auto-reshuffle)
                 :player :corp
                 :yes-ability {:msg "shuffle it back into R&D"
                               :effect (effect (move :corp card :deck)
                                               (shuffle! :corp :deck))}}}}))

(defcard "Mark Yale"
  {:events [{:event :agenda-counter-spent
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits eid 1))}]
   :abilities [{:label "Gain 2 [Credits]"
                :msg "gain 2 [Credits]"
                :cost [:trash]
                :async true
                :effect (effect (gain-credits eid 2))}
               {:label "Gain 2 [Credits]"
                :msg "gain 2 [Credits]"
                :cost [:any-agenda-counter]
                :async true
                :effect (effect (gain-credits eid 2))}]})

(defcard "Marked Accounts"
  (let [ability {:msg "take 1 [Credits]"
                 :label "Take 1 [Credits] (start of turn)"
                 :once :per-turn
                 :req (req (pos? (get-counters card :credit)))
                 :async true
                 :effect (effect (add-counter card :credit -1)
                                 (gain-credits eid 1))}]
    {:abilities [ability
                 {:cost [:click 1]
                  :msg "store 3 [Credits]"
                  :effect (effect (add-counter card :credit 3))}]
     :events [(assoc ability :event :corp-turn-begins)]}))

(defcard "MCA Austerity Policy"
  {:abilities [{:cost [:click 1]
                :once :per-turn
                :msg "to force the Runner to lose a [Click] next turn and place a power counter on itself"
                :effect (req (swap! state update-in [:runner :extra-click-temp] (fnil dec 0))
                             (add-counter state side card :power 1))}
               {:cost [:click 1 :power 3 :trash]
                :msg "gain 4 [Click] and trash itself"
                :effect (effect (gain :click 4))}]})

(defcard "Melange Mining Corp."
  {:abilities [{:cost [:click 3]
                :keep-open :while-3-clicks-left
                :async true
                :effect (effect (gain-credits eid 7))
                :msg "gain 7 [Credits]"}]})

(defcard "Mental Health Clinic"
  (let [ability {:msg "gain 1 [Credits]"
                 :label "Gain 1 [Credits] (start of turn)"
                 :once :per-turn
                 :async true
                 :effect (effect (gain-credits eid 1))}]
    {:constant-effects [(runner-hand-size+ 1)]
     :derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Mr. Stone"
  {:events [{:event :runner-gain-tag
             :async true
             :msg "do 1 meat damage"
             :effect (effect (damage :corp eid :meat 1 {:card card}))}]})

(defcard "Mumba Temple"
  {:recurring 2
   :interactions {:pay-credits {:req (req (= :rez (:source-type eid)))
                                :type :recurring}}})

(defcard "Mumbad City Hall"
  {:abilities [{:label "Search R&D for an Alliance card"
                :cost [:click 1]
                :keep-open :while-clicks-left
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
                :effect (req (wait-for
                               (reveal state side target)
                               (shuffle! state side :deck)
                               (if (operation? target)
                                 (play-instant state side eid target nil)
                                 (corp-install state side eid target nil nil))))}]})

(defcard "Mumbad Construction Co."
  {:derezzed-events [corp-rez-toast]
   :events [{:event :corp-turn-begins
             :effect (effect (add-prop card :advance-counter 1 {:placed true}))}]
   :abilities [{:cost [:credit 2]
                :keep-open :while-advancement-tokens-left
                :req (req (and (pos? (get-counters card :advancement))
                               (not-empty (all-active-installed state :corp))))
                :label "Move an advancement token to a faceup card"
                :prompt "Select a faceup card"
                :choices {:card rezzed?}
                :msg (msg "move an advancement token to " (card-str state target))
                :effect (effect (add-prop card :advance-counter -1 {:placed true})
                                (add-prop target :advance-counter 1 {:placed true}))}]})

(defcard "Museum of History"
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
                            (str (string/join ", " (map :title seen))
                                 (when (pos? n)
                                   (str (when-not (empty? seen) " and ")
                                        (quantify n "card")))))
                          " into R&D")
                :effect (req (doseq [c targets]
                               (move state side c :deck))
                             (shuffle! state side :deck))}]
   :implementation "Errata from FAQ 3.1: should be unique"})

(defcard "Nanoetching Matrix"
  {:abilities [{:cost [:click 1]
                :once :per-turn
                :msg "gain 2 [Credits]"
                :async true
                :effect (effect (gain-credits eid 2))}]
   :on-trash {:optional
              {:req (req (= :runner side))
               :player :corp
               :waiting-prompt "Corp to use Nanoetching Matrix"
               :prompt "Gain 2 [credits]?"
               :yes-ability
               {:msg (msg "gain 2 [Credits]")
                :async true
                :effect (effect (gain-credits :corp eid 2))}}}})

(defcard "NASX"
  (let [ability {:msg "gain 1 [Credits]"
                 :label "Gain 1 [Credits] (start of turn)"
                 :once :per-turn
                 :async true
                 :effect (effect (gain-credits eid 1))}]
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
                  :async true
                  :effect (effect (gain-credits eid (* 2 (get-counters card :power))))}]}))

(defcard "Net Analytics"
  (let [ability {:optional
                 {:player :corp
                  :waiting-prompt "Corp to use Net Analytics"
                  :prompt "Draw from Net Analytics?"
                  :yes-ability
                  {:msg "draw a card"
                   :effect (effect (draw :corp eid 1 nil))}}}]
    {:events [(-> ability
                  (assoc :event :runner-lose-tag)
                  (assoc-in [:optional :req] (req (= side :runner))))
              (-> ability
                  (assoc :event :runner-prevent)
                  (assoc-in [:optional :req] (req (seq (filter #(some #{:tag} %) targets)))))]}))

(defcard "Net Police"
  {:recurring (req (get-link state))
   :interactions {:pay-credits {:req (req (= :trace (:source-type eid)))
                                :type :recurring}}})

(defcard "Neurostasis"
  (advance-ambush 3 {:req (req (pos? (get-counters (get-card state card) :advancement)))
                     :waiting-prompt "Corp to use Neurostasis"
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
                                       :msg (msg "shuffle " (string/join ", " (map :title targets)) " into the stack")
                                       :effect (req (doseq [c targets]
                                                      (move state :runner c :deck))
                                                    (shuffle! state :runner :deck))}
                                      card nil)))}
                  "Pay 3 [Credits] to use Neurostasis ability?"))

(defcard "News Team"
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

(defcard "NGO Front"
  (letfn [(builder [cost cred]
            {:cost [:advancement cost :trash]
             :async true
             :effect (effect (gain-credits eid cred))
             :label (str "Gain " cred " [Credits]")
             :msg (str "gain " cred " [Credits]")})]
    {:advanceable :always
     :abilities [(builder 1 5)
                 (builder 2 8)]}))

(defcard "Nico Campaign"
  (let [ability
        {:async true
         :interactive (req true)
         :once :per-turn
         :label "Take 3 [Credits] (start of turn)"
         :msg (msg "gain " (min 3 (get-counters card :credit)) " [Credits]")
         :req (req (:corp-phase-12 @state))
         :effect (req (let [credits (min 3 (get-counters card :credit))]
                        (add-counter state side card :credit (- credits))
                        (wait-for
                          (gain-credits state :corp credits)
                          (if (pos? (get-counters (get-card state card) :credit))
                            (effect-completed state side eid)
                            (wait-for
                              (trash state :corp card {:unpreventable true})
                              (system-msg state :corp (str "trashes Nico Campaign"
                                                           (when (not (empty? (:deck corp)))
                                                             " and draws 1 card")))
                              (draw state :corp eid 1 nil))))))}]
    {:data {:counter {:credit 9}}
     :derezzed-events [corp-rez-toast]
     :abilities [ability]
     :events [(assoc ability :event :corp-turn-begins)]}))

(defcard "Open Forum"
  {:events [{:event :corp-mandatory-draw
             :interactive (req true)
             :msg (msg (if (-> corp :deck count pos?)
                         (str "reveal and draw "
                              (-> corp :deck first :title)
                              " from R&D")
                         "reveal and draw from R&D but it is empty"))
             :async true
             :effect (req (wait-for
                            (reveal state side (-> corp :deck first))
                            (wait-for
                              (draw state side 1 nil)
                              (continue-ability
                                state side
                                {:prompt "Choose a card in HQ to put on top of R&D"
                                 :async true
                                 :choices {:card #(and (in-hand? %)
                                                       (corp? %))}
                                 :msg "add 1 card from HQ to the top of R&D"
                                 :effect (effect (move target :deck {:front true})
                                                 (effect-completed eid))}
                                card nil))))}]})

(defcard "PAD Campaign"
  (let [ability {:msg "gain 1 [Credits]"
                 :label "Gain 1 [Credits] (start of turn)"
                 :once :per-turn
                 :async true
                 :effect (effect (gain-credits eid 1))}]
    {:derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "PAD Factory"
  {:abilities [{:cost [:click 1]
                :keep-open :while-clicks-left
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
                                            (<= (get-advancement-requirement card)
                                                (get-counters card :advancement)))
                                     ((constantly false)
                                      (toast state :corp "Cannot score due to PAD Factory." "warning"))
                                     true)))))}]})

(defcard "Pālanā Agroplex"
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

(defcard "Personalized Portal"
  {:events [{:event :corp-turn-begins
             :interactive (req true)
             :async true
             :effect (req (wait-for (draw state :runner 1 nil)
                                    (let [cnt (count (get-in @state [:runner :hand]))
                                          credits (quot cnt 2)]
                                      (system-msg state :corp
                                                  (str "uses Personalized Portal to force the runner to draw "
                                                       "1 card and gain " credits " [Credits]"))
                                      (gain-credits state :corp eid credits))))}]})

(defcard "Plan B"
  (advance-ambush
    0
    {:req (req (pos? (get-counters (get-card state card) :advancement)))
     :waiting-prompt "Corp to select an agenda to score with Plan B"
     :prompt "Select an Agenda in HQ to score"
     :choices {:req (req (and (agenda? target)
                              (<= (get-advancement-requirement target)
                                  (get-counters (get-card state card) :advancement))
                              (in-hand? target)))}
     :msg (msg "score " (:title target))
     :async true
     :effect (effect (score eid target {:no-req true}))}
    "Score an Agenda from HQ?"))

(defcard "Political Dealings"
  (letfn [(pdhelper [agendas n]
            {:optional
             {:prompt (msg "Reveal and install " (:title (nth agendas n)) "?")
              :yes-ability {:async true
                            :msg (msg "reveal " (:title (nth agendas n)))
                            :effect (req (wait-for
                                           (reveal state side (nth agendas n))
                                           (wait-for
                                             (corp-install
                                               state side (nth agendas n) nil
                                               {:install-state
                                                (:install-state
                                                  (card-def (nth agendas n))
                                                  :unrezzed)})
                                             (if (< (inc n) (count agendas))
                                               (continue-ability state side (pdhelper agendas (inc n)) card nil)
                                               (effect-completed state side eid)))))}
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

(defcard "Prāna Condenser"
  {:interactions {:prevent [{:type #{:net}
                             :req (req (= :corp (:side target)))}]}
   :abilities [{:label "Prevent 1 net damage to add power token to Prāna Condenser"
                :msg "prevent 1 net damage, place 1 power token, and gain 3 [Credits]"
                :async true
                :req (req true)
                :effect (req (add-counter state side card :power 1)
                             (gain-credits state :corp eid 3)
                             (damage-prevent state :corp :net 1))}
               {:msg (msg "deal " (get-counters card :power) " net damage")
                :label "deal net damage"
                :cost [[:click 2] [:trash]]
                :effect (effect (damage eid :net (get-counters card :power) {:card card}))}]})

(defcard "Primary Transmission Dish"
  {:recurring 3
   :interactions {:pay-credits {:req (req (= :trace (:source-type eid)))
                                :type :recurring}}})

(defcard "Private Contracts"
  {:on-rez {:effect (effect (add-counter card :credit 14))}
   :events [(trash-on-empty :credit)]
   :abilities [{:cost [:click 1]
                :keep-open :while-clicks-left
                :label "gain credits"
                :msg (msg "gain " (min 2 (get-counters card :credit)) " [Credits]")
                :async true
                :effect (req (let [credits (min 2 (get-counters card :credit))]
                               (add-counter state side card :credit (- credits))
                               (gain-credits state :corp eid credits)))}]})

(defcard "Project Junebug"
  (advance-ambush 1 {:req (req (pos? (get-counters (get-card state card) :advancement)))
                     :waiting-prompt "Corp to use Project Junebug"
                     :msg (msg "do " (* 2 (get-counters (get-card state card) :advancement)) " net damage")
                     :async true
                     :effect (effect (damage eid :net (* 2 (get-counters (get-card state card) :advancement))
                                             {:card card}))}))

(defcard "Psychic Field"
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

(defcard "Public Health Portal"
  (let [ability {:once :per-turn
                 :label "Reveal top card of R&D and gain 2 [Credits] (start of turn)"
                 :interactive (req true)
                 :msg (msg "reveal " (:title (first (:deck corp)))
                           " from the top of R&D"
                           " and gain 2 [Credits]")
                 :async true
                 :effect (req (wait-for
                                (reveal state side (first (:deck corp)))
                                (gain-credits state side eid 2)))}]
    {:derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Public Support"
  {:on-rez {:effect (effect (add-counter card :power 3))}
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

(defcard "Quarantine System"
  (letfn [(rez-ice [cnt] {:prompt "Select an ICE to rez"
                          :async true
                          :choices {:card #(and (ice? %)
                                                (not (rezzed? %)))}
                          :msg (msg "rez " (:title target))
                          :effect (req (let [agenda (last (:rfg corp))
                                             ap (:agendapoints agenda 0)]
                                         (wait-for (rez state side target {:no-warning true :cost-bonus (* ap -2)})
                                                   (if (< cnt 3)
                                                     (continue-ability state side (rez-ice (inc cnt)) card nil)
                                                     (effect-completed state side eid)))))})]
    {:abilities [{:label "Forfeit agenda to rez up to 3 ICE with a 2 [Credit] discount per agenda point"
                  :req (req (pos? (count (:scored corp))))
                  :cost [:forfeit]
                  :effect (req (continue-ability state side (rez-ice 1) card nil))}]}))

(defcard "Raman Rai"
  {:abilities [{:once :per-turn
                :label "Swap drawn card with card in Archives"
                :req (req (and (pos? (:click corp))
                               (not-empty (turn-events state side :corp-draw))))
                :async true
                :effect (effect
                          (lose :corp :click 1)
                          (continue-ability
                            (let [drawn (get-in @state [:corp :register :most-recent-drawn])]
                              {:prompt "Choose a card in HQ that you just drew to swap for a card of the same type in Archives"
                               :choices {:card #(some (fn [c] (same-card? c %)) drawn)}
                               :async true
                               :effect
                               (effect
                                 (continue-ability
                                   (let [hq-card target
                                         t (:type hq-card)]
                                     {:show-discard true
                                      :prompt (msg "Choose an " t " in Archives to reveal and swap into HQ for " (:title hq-card))
                                      :choices {:card #(and (corp? %)
                                                            (= (:type %) t)
                                                            (in-discard? %))}
                                      :msg (msg "lose [Click], reveal " (:title hq-card)
                                                " from HQ, and swap it for " (:title target)
                                                " from Archives")
                                      :async true
                                      :effect (req (wait-for
                                                     (reveal state side target)
                                                     (swap-cards state side hq-card target)
                                                     (effect-completed state side eid)))})
                                   card nil))})
                            card nil))}]})

(defcard "Rashida Jaheem"
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
                        :effect (req (wait-for
                                       (trash state side card nil)
                                       (wait-for
                                         (gain-credits state side 3)
                                         (draw state side eid 3 nil))))}}}
                     card nil))}]
    {:derezzed-events [corp-rez-toast]
     :flags {:corp-phase-12 (req true)}
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Reality Threedee"
  (let [ability {:effect (effect (gain-credits eid (if tagged 2 1)))
                 :async true
                 :label "Gain credits (start of turn)"
                 :once :per-turn
                 :msg (msg (if tagged "gain 2 [Credits]" "gain 1 [Credits]"))}]
    {:on-rez {:effect (effect (gain-bad-publicity :corp 1)
                              (system-msg "takes 1 bad publicity"))}
     :derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Reconstruction Contract"
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

(defcard "Regolith Mining License"
  {:data {:counter {:credit 15}}
   :events [(trash-on-empty :credit)]
   :abilities [{:label "Take 3 [Credits] from this asset"
                :cost [:click 1]
                :keep-open :while-clicks-left
                :msg (msg "gain " (min 3 (get-counters card :credit)) " [Credits]")
                :async true
                :effect (req (let [credits (min 3 (get-counters card :credit))]
                               (wait-for (gain-credits state :corp (make-eid state eid) credits)
                                         (add-counter state side card :credit (- credits) {:placed true})
                                         (effect-completed state side eid))))}]})

(defcard "Reversed Accounts"
  {:advanceable :always
   :abilities [{:cost [:click 1 :trash]
                :label "Force the Runner to lose 4 [Credits] per advancement"
                :msg (msg "force the Runner to lose " (min (* 4 (get-counters card :advancement)) (:credit runner)) " [Credits]")
                :async true
                :effect (effect (lose-credits :runner eid (* 4 (get-counters card :advancement))))}]})

(defcard "Rex Campaign"
  (let [payout-ab {:prompt "Remove 1 bad publicity or gain 5 [Credits]?"
                   :choices ["Remove 1 bad publicity" "Gain 5 [Credits]"]
                   :msg (msg (if (= target "Remove 1 bad publicity")
                               "remove 1 bad publicity" "gain 5 [Credits]"))
                   :async true
                   :effect (req (if (= target "Remove 1 bad publicity")
                                  (lose-bad-publicity state side eid 1)
                                  (gain-credits state side eid 5)))}
        ability {:once :per-turn
                 :req (req (:corp-phase-12 @state))
                 :label "Remove 1 counter (start of turn)"
                 :effect (req (add-counter state side card :power -1)
                              (if (zero? (get-counters (get-card state card) :power))
                                (wait-for (trash state side card nil)
                                          (continue-ability state side payout-ab card nil))))}]
    {:on-rez {:effect (effect (add-counter card :power 3))}
     :derezzed-events [corp-rez-toast]
     :events [(assoc ability :event :corp-turn-begins)]
     :ability [ability]}))

(defcard "Ronald Five"
  (let [ability {:event :runner-trash
                 :once-per-instance false
                 :req (req (and (corp? (:card target))
                                (pos? (:click runner))))
                 :msg "force the runner to lose 1 [Click]"
                 :effect (effect (lose :runner :click 1))}]
    {:events [ability]
     :on-trash ability}))

(defcard "Ronin"
  {:advanceable :always
   :abilities [{:cost [:click 1 :trash]
                :req (req (>= (get-counters card :advancement) 4))
                :msg "do 3 net damage"
                :async true
                :effect (effect (damage eid :net 3 {:card card}))}]})

(defcard "Roughneck Repair Squad"
  {:abilities [{:label "Gain 6 [Credits], may remove 1 bad publicity"
                :cost [:click 3]
                :keep-open :while-3-clicks-left
                :msg "gain 6 [Credits]"
                :async true
                :effect (req (wait-for
                               (gain-credits state side 6)
                               (continue-ability
                                 state side
                                 {:optional
                                  {:req (req (pos? (count-bad-pub state)))
                                   :player :corp
                                   :prompt "Remove 1 bad publicity?"
                                   :yes-ability
                                   {:msg "remove 1 bad publicity"
                                    :effect (effect (lose-bad-publicity 1))}}}
                                 card nil)))}]})

(defcard "Sandburg"
  {:on-rez {:effect (effect (update-all-ice))}
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

(defcard "Sealed Vault"
  {:abilities [{:label "Store any number of [Credits] on Sealed Vault"
                :cost [:credit 1]
                :prompt "How many [Credits]?"
                :choices {:number (req (- (:credit corp) 1))}
                :msg (msg "store " target " [Credits]")
                :async true
                :effect (effect (add-counter card :credit target)
                                (lose-credits eid target))}
               {:label "Move any number of [Credits] to your credit pool"
                :cost [:click 1]
                :prompt "How many [Credits]?"
                :choices {:counter :credit}
                :msg (msg "gain " target " [Credits]")
                :async true
                :effect (effect (gain-credits eid target))}
               {:label "Move any number of [Credits] to your credit pool"
                :prompt "How many [Credits]?"
                :choices {:counter :credit}
                :msg (msg "trash it and gain " target " [Credits]")
                :cost [:trash]
                :async true
                :effect (effect (gain-credits eid target))}]})

(defcard "Security Subcontract"
  {:abilities [{:cost [:click 1 :ice 1]
                :keep-open :while-clicks-left
                :msg "gain 4 [Credits]"
                :label "Gain 4 [Credits]"
                :async true
                :effect (effect (gain-credits eid 4))}]})

(defcard "Sensie Actors Union"
  {:derezzed-events [corp-rez-toast]
   :flags {:corp-phase-12 (req unprotected)}
   :abilities [{:label "Draw 3 cards and add 1 card in HQ to the bottom of R&D"
                :once :per-turn
                :msg "draw 3 cards"
                :async true
                :effect (req (wait-for (draw state side 3 nil)
                                       (continue-ability
                                         state side
                                         {:prompt "Select a card in HQ to add to the bottom of R&D"
                                          :choices {:card #(and (corp? %)
                                                                (in-hand? %))}
                                          :msg "add 1 card from HQ to the bottom of R&D"
                                          :effect (effect (move target :deck))}
                                         card nil)))}]})

(defcard "Server Diagnostics"
  (let [ability {:effect (effect (gain-credits eid 2))
                 :async true
                 :once :per-turn
                 :label "Gain 2 [Credits] (start of turn)"
                 :msg "gain 2 [Credits]"}]
    {:derezzed-events [corp-rez-toast]
     :abilities [ability]
     :events [(assoc ability :event :corp-turn-begins)
              {:event :corp-install
               :req (req (ice? (:card context)))
               :async true
               :effect (effect (system-msg :runner "trashes Server Diagnostics")
                               (trash eid card))}]}))

(defcard "Shannon Claire"
  {:abilities [{:cost [:click 1]
                :keep-open :while-clicks-left
                :msg "draw 1 card from the bottom of R&D"
                :effect (effect (move (last (:deck corp)) :hand))}
               {:label "Search R&D for an agenda"
                :prompt "Choose an agenda to add to the bottom of R&D"
                :msg (msg "reveal " (:title target) " from R&D and add it to the bottom of R&D")
                :choices (req (cancellable (filter agenda? (:deck corp)) :sorted))
                :cost [:trash]
                :async true
                :effect (req (wait-for
                               (reveal state side target)
                               (shuffle! state side :deck)
                               (move state side target :deck)
                               (effect-completed state side eid)))}
               {:label "Search Archives for an agenda"
                :prompt "Choose an agenda to add to the bottom of R&D"
                :msg (msg "reveal " (:title target) " from Archives and add it to the bottom of R&D")
                :choices (req (cancellable (filter agenda? (:discard corp)) :sorted))
                :cost [:trash]
                :async true
                :effect (req (wait-for
                               (reveal state side target)
                               (move state side target :deck)
                               (effect-completed state side eid)))}]})

(defcard "Shattered Remains"
  (advance-ambush 1 {:async true
                     :waiting-prompt "Corp to use Shattered Remains"
                     :req (req (pos? (get-counters (get-card state card) :advancement)))
                     :prompt (msg "Select " (quantify (get-counters (get-card state card) :advancement) "piece") " of hardware to trash")
                     :msg (msg "trash " (string/join ", " (map :title targets)))
                     :cost [:credit 1]
                     :choices {:max (req (get-counters (get-card state card) :advancement))
                               :card #(and (installed? %)
                                           (hardware? %))}
                     :effect (effect (trash-cards eid targets))}))

(defcard "Shi.Kyū"
  {:access
   {:optional
    {:req (req (not (in-deck? card)))
     :waiting-prompt "Corp to use Shi.Kyū"
     :prompt "Pay [Credits] to use Shi.Kyū?"
     :yes-ability
     {:prompt "How many [Credits] for Shi.Kyū?"
      :choices :credit
      :msg (msg "attempt to do " target " net damage")
      :async true
      :effect (effect
                (continue-ability
                  (let [dmg target]
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
                                        (damage state :corp eid :net dmg {:card card}))))})
                  card targets))}}}})

(defcard "Shock!"
  {:flags {:rd-reveal (req true)}
   :access {:msg "do 1 net damage"
            :async true
            :effect (effect (damage eid :net 1 {:card card}))}})

(defcard "SIU"
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

(defcard "Snare!"
  {:flags {:rd-reveal (req true)}
   :access {:optional
            {:req (req (not (in-discard? card)))
             :waiting-prompt "Corp to use Snare!"
             :prompt "Pay 4 [Credits] to use Snare! ability?"
             :yes-ability {:async true
                           :cost [:credit 4]
                           :msg "do 3 net damage and give the Runner 1 tag"
                           :effect (req (wait-for (damage state side :net 3 {:card card})
                                                  (gain-tags state :corp eid 1)))}}}})

(defcard "Space Camp"
  {:flags {:rd-reveal (req true)}
   :access {:optional
            {:waiting-prompt "Corp to use Space Camp"
             :prompt "Place 1 advancement token with Space Camp?"
             :yes-ability {:msg (msg "place 1 advancement token on " (card-str state target))
                           :prompt "Select a card to place an advancement token on with Space Camp"
                           :choices {:card can-be-advanced?}
                           :effect (effect (add-prop target :advance-counter 1 {:placed true}))}}}})

(defcard "Spin Doctor"
  {:on-rez {:async true
            :msg "draw 2 cards"
            :effect (effect (draw eid 2 nil))}
   :abilities [{:label "Shuffle up to 2 cards from Archives into R&D"
                :cost [:remove-from-game]
                :async true
                :effect (effect (shuffle-into-rd-effect eid card 2))}]})

(defcard "Storgotic Resonator"
  {:abilities [{:cost [:click 1 :power 1]
                :keep-open :while-power-tokens-left
                :label "Do 1 net damage"
                :msg "do 1 net damage"
                :async true
                :effect (effect (damage eid :net 1 {:card card}))}]
   :events [{:event :corp-trash
             :once-per-instance true
             :req (req (and (some #(= (:faction (:identity runner)) (:faction (:card %))) targets)
                            (first-event?
                              state side :corp-trash
                              (fn [targets]
                                (some #(= (:faction (:identity runner)) (:faction (:card %))) targets)))))
             :effect (effect (system-msg :corp "adds 1 power counter on Storgotic Resonator")
                             (add-counter card :power 1))}]})

(defcard "Student Loans"
  {:constant-effects [{:type :play-additional-cost
                       :req (req (and (event? target)
                                      (seq (filter #(= (:title %) (:title target)) (:discard runner)))))
                       :value [:credit 2]}]})

(defcard "Sundew"
  {:events [{:event :runner-spent-click
             :req (req (first-event? state side :runner-spent-click))
             :msg "gain 2 [Credits]"
             :async true
             :effect (effect (gain-credits :corp eid 2))}
            {:event :run
             :once :per-turn
             :req (req (and (first-event? state side :runner-spent-click)
                            this-server))
             :msg "loses 2 [Credits]"
             :async true
             :effect (effect (lose-credits :corp eid 2))}]})

(defcard "Synth DNA Modification"
  {:implementation "Manual fire once subroutine is broken"
   :abilities [{:msg "do 1 net damage"
                :label "Do 1 net damage after AP subroutine broken"
                :once :per-turn
                :effect (effect (damage eid :net 1 {:card card}))}]})

(defcard "Team Sponsorship"
  {:events [{:event :agenda-scored
             :prompt "Select a card from Archives or HQ to install"
             :show-discard true
             :interactive (req true)
             :async true
             :choices {:card #(and (not (operation? %))
                                   (corp? %)
                                   (or (in-hand? %)
                                       (in-discard? %)))}
             :msg (msg (corp-install-msg target))
             :effect (effect (corp-install eid target nil {:ignore-install-cost true}))}]})

(defcard "Tech Startup"
  {:derezzed-events [corp-rez-toast]
   :flags {:corp-phase-12 (req true)}
   :abilities [{:label "Install an asset from R&D"
                :prompt "Choose an asset to install"
                :msg (msg "install " (:title target))
                :req (req (seq (filter asset? (:deck corp))))
                :choices (req (filter asset? (:deck corp)))
                :async true
                :effect (req (wait-for
                               (trash state side card nil)
                               (wait-for
                                 (reveal state side target)
                                 (shuffle! state side :deck)
                                 (corp-install state side eid target nil nil))))}]})

(defcard "TechnoCo"
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
               :req (req (and (is-techno-target (:card context))
                              (not (:facedown context))))
               :msg "gain 1 [Credits]"
               :async true
               :effect (effect (gain-credits :corp eid 1))}]}))

(defcard "Tenma Line"
  {:abilities [{:label "Swap 2 pieces of installed ICE"
                :cost [:click]
                :keep-open :while-clicks-left
                :prompt "Select two pieces of ICE to swap positions"
                :req (req (<= 2 (count (filter ice? (all-installed state :corp)))))
                :choices {:card #(and (installed? %)
                                      (ice? %))
                          :max 2
                          :all true}
                :msg (msg "swap the positions of "
                          (card-str state (first targets))
                          " and "
                          (card-str state (second targets)))
                :effect (req (apply swap-installed state side targets))}]})

(defcard "Test Ground"
  (letfn [(derez-card [advancements & wait-msg]
            (when (pos? advancements)
              {:async true
               :waiting-prompt (first wait-msg)
               :prompt "Derez a card"
               :choices {:card #(and (installed? %)
                                     (rezzed? %))}
               :effect (req (derez state side target)
                            (continue-ability state side (derez-card (dec advancements)) card nil))}))]
    {:advanceable :always
     :abilities [{:label "Derez 1 card for each advancement token"
                  :req (req (pos? (get-counters card :advancement)))
                  :msg (msg "derez " (quantify (get-counters card :advancement) "card"))
                  :cost [:trash]
                  :async true
                  :effect (req (let [advancements (get-counters card :advancement)
                                     wait-msg (str "Corp to derez " (quantify advancements "card"))]
                                 (continue-ability state side (derez-card advancements wait-msg) card nil)))}]}))

(defcard "The Board"
  {:on-trash executive-trash-effect
   :constant-effects [{:type :agenda-value
                       :req (req (= :runner (:scored-side target)))
                       :value -1}]})

(defcard "The News Now Hour"
  {:events [{:event :runner-turn-begins
             :effect (req (prevent-current state side))}]
   :on-rez {:effect (req (prevent-current state side))}
   :leave-play (req (swap! state assoc-in [:runner :register :cannot-play-current] false))})

(defcard "The Root"
  {:recurring 3
   :interactions {:pay-credits {:req (req (or (= :advance (:source-type eid))
                                              (= :corp-install (:source-type eid))
                                              (= :rez (:source-type eid))))
                                :type :recurring}}})

(defcard "Thomas Haas"
  {:advanceable :always
   :abilities [{:label "Gain credits"
                :msg (msg "gain " (* 2 (get-counters card :advancement)) " [Credits]")
                :cost [:trash]
                :async true
                :effect (effect (gain-credits eid (* 2 (get-counters card :advancement))))}]})

(defcard "Tiered Subscription"
  {:events [{:event :run
             :req (req (first-event? state side :run))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits :corp eid 1))}]})

(defcard "Toshiyuki Sakai"
  (advance-ambush
    0
    {:async true
     :waiting-prompt "Corp to use Toshiyuki Sakai"
     :prompt "Select an asset or agenda in HQ"
     :choices {:card #(and (or (agenda? %)
                               (asset? %))
                           (in-hand? %))}
     :msg "swap it for an asset or agenda from HQ"
     :effect (req (let [counters (get-counters card :advancement)
                        [moved-card moved-target] (swap-cards state side card target)]
                    (set-prop state side moved-target :advance-counter counters)
                    (continue-ability
                      state :runner
                      {:optional
                       {:prompt "Access the newly installed card?"
                        :yes-ability {:async true
                                      :effect (effect (access-card eid (get-card state moved-target)))}}}
                      moved-card nil)))}
    "Swap Toshiyuki Sakai with an agenda or asset from HQ?"))

(defcard "Turtlebacks"
  {:events [{:event :server-created
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits eid 1))}]})

(defcard "Urban Renewal"
  {:on-rez {:effect (effect (add-counter card :power 3))}
   :derezzed-events [corp-rez-toast]
   :events [{:event :corp-turn-begins
             :async true
             :effect (req (add-counter state side card :power -1)
                          (if (not (pos? (get-counters (get-card state card) :power)))
                            (wait-for (trash state side card nil)
                                      (system-msg state :corp "uses Urban Renewal to do 4 meat damage")
                                      (damage state side eid :meat 4 {:card card}))
                            (effect-completed state side eid)))}]})

(defcard "Urtica Cipher"
  (advance-ambush 0 {:msg (msg "do " (+ 2 (get-counters (get-card state card) :advancement)) " net damage")
                     :async true
                     :effect (effect (damage eid :net (+ 2 (get-counters (get-card state card) :advancement)) {:card card}))}))

(defcard "Vaporframe Fabricator"
  {:on-trash {:req (req (= :runner side))
              :async true
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
                     :choices (req (remove (set (zone->name (get-zone card)))
                                           (installable-servers state card-to-install)))
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

(defcard "Victoria Jenkins"
  {:on-rez {:effect (req (lose state :runner :click-per-turn 1))}
   :leave-play (req (gain state :runner :click-per-turn 1))
   :on-trash executive-trash-effect})

(defcard "Wall to Wall"
  (let [all [{:msg "gain 1 [Credits]"
              :async true
              :effect (effect (gain-credits eid 1))}
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
              :effect (effect (move card :hand))}]
        choice (fn choice [abis n]
                 (let [choices (concat (mapv make-label abis) ["Done"])]
                   {:prompt "Choose an ability to resolve"
                    :choices choices
                    :async true
                    :effect (req (let [chosen (first (filter #(= % target) choices))
                                       chosen-ability (first (filter #(= target (make-label %)) abis))]
                                   (wait-for (resolve-ability state side chosen-ability card nil)
                                     (if (and (pos? (dec n))
                                              (not= "Done" chosen))
                                       (continue-ability state side (choice (remove-once #(= % chosen-ability) abis) (dec n)) card nil)
                                       (effect-completed state side eid)))))}))
        ability {:async true
                 :label "resolve an ability"
                 :once :per-turn
                 :effect (effect (continue-ability (choice all (if (< 1 (count (filter asset? (all-active-installed state :corp))))
                                                                 1
                                                                 3)) card nil))}]
    {:derezzed-events [(assoc corp-rez-toast :event :runner-turn-ends)]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Warden Fatuma"
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
      {:on-rez {:effect (req (system-msg
                               state :corp
                               "uses Warden Fatuma to add \"[Subroutine] The Runner loses [Click], if able\" before all other subroutines")
                          (update-all state (partial add-one (:cid card))))}
       :leave-play (req (system-msg state :corp "loses Warden Fatuma additional subroutines")
                     (update-all state (partial remove-one (:cid card))))
       :sub-effect {:msg "force the Runner to lose 1 [Click], if able"
                    :effect (req (lose state :runner :click 1))}
       :events [{:event :rez
                 :req (req (and (ice? (:card context))
                                (has-subtype? (:card context) "Bioroid")))
                 :effect (req (add-one (:cid card) state (get-card state (:card context))))}]})))

(defcard "Watchdog"
  (letfn [(not-triggered? [state card]
            (no-event? state :runner :rez #(ice? (:card (first %)))))]
    {:constant-effects [{:type :rez-cost
                         :req (req (and (ice? target)
                                        (not-triggered? state card)))
                         :value (req (- (count-tags state)))}]
     :events [{:event :rez
               :req (req (and (ice? (:card context))
                              (not-triggered? state card)))
               :msg (msg "reduce the rez cost of " (:title (:card context))
                      " by " (count-tags state) " [Credits]")}]}))

(defcard "Whampoa Reclamation"
  {:abilities [{:label "Add 1 card from Archives to the bottom of R&D"
                :once :per-turn
                :req (req (and (pos? (count (:hand corp)))
                               (pos? (count (:discard corp)))))
                :async true
                :cost [:trash-from-hand 1]
                :effect (effect (continue-ability
                                  {:waiting-prompt "Corp to use Whampoa Reclamation"
                                   :prompt "Select a card in Archives to add to the bottom of R&D"
                                   :show-discard true
                                   :choices {:card #(and (in-discard? %)
                                                         (corp? %))}
                                   :msg (msg "trash 1 card from HQ and add "
                                             (if (:seen target) (:title target) "a card")
                                             " from Archives to the bottom of R&D")
                                   :effect (effect (move target :deck))}
                                  card nil))}]})

(defcard "Worlds Plaza"
  {:abilities [{:label "Install an asset on Worlds Plaza"
                :req (req (< (count (:hosted card)) 3))
                :cost [:click 1]
                :keep-open :while-clicks-left
                :prompt "Select an asset to install on Worlds Plaza"
                :choices {:card #(and (asset? %)
                                      (in-hand? %)
                                      (corp? %))}
                :msg (msg "host " (:title target))
                :async true
                :effect (req (wait-for (corp-install state side target card nil) ;; install target onto card
                                       (rez state side eid (last (:hosted (get-card state card))) {:cost-bonus -2})))}]})

(defcard "Zaibatsu Loyalty"
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
                                         :yes-ability {:async true
                                                       :effect (effect (rez eid card))}}}
                                       card nil)))}]
   :abilities [{:msg "prevent 1 card from being exposed"
                :cost [:credit 1]
                :effect (effect (expose-prevent 1))}
               {:msg "prevent 1 card from being exposed"
                :label "Prevent 1 card from being exposed"
                :cost [:trash]
                :effect (effect (expose-prevent 1))}]})

(defcard "Zealous Judge"
  {:rez-req (req tagged)
   :abilities [{:async true
                :label "Give the Runner 1 tag"
                :cost [:click 1 :credit 1]
                :keep-open :while-clicks-left
                :msg (msg "give the Runner 1 tag")
                :effect (effect (gain-tags eid 1))}]})
