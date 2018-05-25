(in-ns 'game.core)

(declare expose-prevent runner-loses-click)

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
             :delayed-completion true
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
  [counters per-turn]
  (let [ability {:msg (str "gain " per-turn " [Credits]")
                 :counter-cost [:credit per-turn]
                 :once :per-turn
                 :req (req (:corp-phase-12 @state))
                 :label (str "Gain " per-turn " [Credits] (start of turn)")
                 :effect (req (gain state :corp :credit per-turn)
                              (when (zero? (get-counters card :credit))
                                (trash state :corp card)))}]
    {:effect (effect (add-counter card :credit counters))
     :derezzed-events {:runner-turn-ends corp-rez-toast}
     :events {:corp-turn-begins ability}
     :abilities [ability]}))

(defn as-trashed-agenda
  "Adds the given card to the given side's :scored area as an agenda worth n points after resolving the trash prompt."
  ([state side eid card n] (as-trashed-agenda state side eid card n nil))
  ([state side eid card n options]
  (or
    ; if the runner did not trash the card on access, then this will work
    (move state :runner (assoc (deactivate state side card) :agendapoints n) :scored options)
    ; allow force option in case of Blacklist/News Team
    (move state :runner (assoc (deactivate state side card) :agendapoints n :zone [:discard]) :scored options))
   (when-completed (trigger-event-sync state side :as-agenda (assoc card :as-agenda-side side :as-agenda-points n))
                   (do (gain-agenda-point state side n)
                       (effect-completed state side eid)))))

;;; Card definitions
(declare in-server?)

(def cards-assets
  {"Adonis Campaign"
   (campaign 12 3)

   "Advanced Assembly Lines"
   {:effect (effect (gain :credit 3))
    :msg (msg "gain 3 [Credits]")
    :abilities [{:label "[Trash]: Install a non-agenda card from HQ"
                 :effect (effect (trash card {:cause :ability-cost})
                                 (corp-install target nil))
                 :msg (msg (corp-install-msg target))
                 :prompt "Select a non-agenda card to install from HQ"
                 :priority true
                 :req (req (not (:run @state)))
                 :choices {:req #(and (not (is-type? % "Operation"))
                                      (not (is-type? % "Agenda"))
                                      (= (:zone %) [:hand])
                                      (= (:side %) "Corp"))}}]}

   "Aggressive Secretary"
   (advance-ambush 2 {:req (req (pos? (get-counters (get-card state card) :advancement)))
                      :delayed-completion true
                      :effect (req (let [agg (get-counters (get-card state card) :advancement)
                                         ab (-> trash-program
                                                (assoc-in [:choices :max] agg)
                                                (assoc :prompt (msg "Choose " (quantify agg "program") " to trash")
                                                       :delayed-completion true
                                                       :effect (effect (trash-cards eid targets nil))
                                                       :msg (msg "trash " (join ", " (map :title targets)))))]
                                     (continue-ability state side ab card nil)))})

   "Alexa Belsky"
   {:abilities [{:label "[Trash]: Shuffle all cards in HQ into R&D"
                 :effect (effect (trash card {:cause :ability-cost})
                                 (show-wait-prompt :corp "Runner to decide whether or not to prevent Alexa Belsky")
                                 (resolve-ability
                                   {:prompt "Prevent Alexa Belsky from shuffling back in 1 card for every 2 [Credits] spent. How many credits?"
                                    :choices :credit
                                    :player :runner
                                    :priority 2
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
                                    :end-effect (effect (clear-wait-prompt state :corp))}
                                   card nil))}]}

   "Alix T4LB07"
   {:events {:corp-install {:effect (effect (add-counter card :power 1))}}
    :abilities [{:label "Gain 2 [Credits] for each counter on Alix T4LB07"
                 :cost [:click 1]
                 :msg (msg "gain " (* 2 (get-counters card :power)) " [Credits]")
                 :effect (effect (trash card {:cause :ability-cost})
                                 (gain :credit (* 2 (get-counters card :power))))}]}

   "Allele Repression"
   {:implementation "Card swapping is manual"
    :advanceable :always
    :abilities [{:label "Swap 1 card in HQ and Archives for each advancement token"
                 :effect (effect (trash card {:cause :ability-cost}))
                 :msg (msg "swap " (get-counters card :advancement) " cards in HQ and Archives")}]}

   "Amani Senai"
   (letfn [(get-last-stolen-pts [state]
             (advancement-cost state :corp (last (get-in @state [:runner :scored]))))
           (get-last-scored-pts [state]
             (advancement-cost state :corp (last (get-in @state [:corp :scored]))))
           (senai-ability [trace-base-func]
             {:interactive (req true)
              :optional {:prompt "Trace with Amani Senai?"
                         :player :corp
                         :yes-ability {:trace {:base (req (trace-base-func state))
                                               :choices {:req #(and (installed? %)
                                                                    (card-is? % :side :runner))}
                                               :label "add an installed card to the Grip"
                                               :msg (msg "add " (:title target) " to the Runner's Grip")
                                               :effect (effect (move :runner target :hand true))}}}})]
     {:events {:agenda-scored (senai-ability get-last-scored-pts)
               :agenda-stolen (senai-ability get-last-stolen-pts)}})

   "Anson Rose"
   (let [ability {:label "Place 1 advancement token on Anson Rose (start of turn)"
                  :once :per-turn
                  :effect (effect (system-msg (str "places 1 advancement counter on Anson Rose"))
                                  (add-prop card :advance-counter 1 {:placed true}))}]
     {:derezzed-events {:runner-turn-ends corp-rez-toast}
      :flags {:corp-phase-12 (req true)}
      :events {:corp-turn-begins ability
               :rez {:req (req (and (ice? target)
                                    (pos? (get-counters card :advancement))))
                     :delayed-completion true
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
                                                                          " advancement tokens to " (card-str state ice))))
                                         :end-effect (effect (clear-wait-prompt :runner))}}}
                                      card nil)))}}
      :abilities [ability]})

   "Aryabhata Tech"
   {:events {:successful-trace {:msg "gain 1 [Credit] and force the Runner to lose 1 [Credit]"
                                :effect (effect (gain :credit 1)
                                                (lose :runner :credit 1))}}}

   "Bio-Ethics Association"
   (let [ability {:req (req unprotected)
                  :delayed-completion true
                  :label "Do 1 net damage (start of turn)"
                  :once :per-turn
                  :msg "do 1 net damage"
                  :effect (effect (damage eid :net 1 {:card card}))}]
     {:derezzed-events {:runner-turn-ends corp-rez-toast}
      :events {:corp-turn-begins ability}
      :abilities [ability]})

   "Bioroid Work Crew"
   {:implementation "Timing restriction of ability use not enforced"
    :abilities [{:label "[Trash]: Install 1 card, paying all costs"
                 :req (req (= (:active-player @state) :corp))
                 :prompt "Select a card in HQ to install"
                 :choices {:req #(and (not (is-type? % "Operation"))
                                      (in-hand? %)
                                      (= (:side %) "Corp"))}
                 :effect (effect (trash card {:cause :ability-cost})
                                 (corp-install target nil))
                 :msg (msg (corp-install-msg target))}]}

   "Blacklist"
   {:effect (effect (lock-zone (:cid card) :runner :discard))
    :leave-play (effect (release-zone (:cid card) :runner :discard))}

   "Breached Dome"
   {:flags {:rd-reveal (req true)}
    :access {:delayed-completion true
             :effect (req (let [c (first (get-in @state [:runner :deck]))]
                            (system-msg state :corp (str "uses Breached Dome to do one meat damage and to trash " (:title c)
                                                         " from the top of the Runner's Stack"))
                            (mill state :corp :runner 1)
                            (damage state side eid :meat 1 {:card card})))}}

   "Brain-Taping Warehouse"
   {:events {:pre-rez
             {:req (req (and (ice? target)
                             (has-subtype? target "Bioroid")))
              :effect (effect (rez-cost-bonus (- (:click runner))))}}}

   "Broadcast Square"
   {:events {:pre-bad-publicity {:delayed-completion true
                                 :trace {:base 3
                                         :msg "prevents all bad publicity"
                                         :effect (effect (bad-publicity-prevent Integer/MAX_VALUE))}}}}

   "Capital Investors"
   {:abilities [{:cost [:click 1]
                 :msg "gain 2 [Credits]"
                 :effect (effect (gain :credit 2))}]}

   "Cerebral Overwriter"
   (advance-ambush 3 {:delayed-completion true
                      :req (req (pos? (get-counters (get-card state card) :advancement)))
                      :msg (msg "do " (get-counters (get-card state card) :advancement) " brain damage")
                      :effect (effect (damage eid :brain (get-counters (get-card state card) :advancement) {:card card}))})

   "Chairman Hiro"
   {:effect (effect (lose :runner :hand-size 2))
    :leave-play (effect (gain :runner :hand-size 2))
    :trash-effect {:when-inactive true
                   :req (req (:access @state))
                   :msg "add it to the Runner's score area as an agenda worth 2 agenda points"
                   :delayed-completion true
                   :effect (req (as-agenda state :runner eid card 2))}}

   "Chief Slee"
   {:abilities [{:label "Add 1 power counter"
                 :effect (effect (add-counter card :power 1)
                                 (system-msg (str "adds 1 power counter to Chief Slee")))}
                {:counter-cost [:power 5]
                 :cost [:click 1]
                 :delayed-completion true
                 :msg "do 5 meat damage"
                 :effect (effect (damage eid :meat 5 {:card card}))}]}

   "C.I. Fund"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :flags {:corp-phase-12 (req (pos? (:credit corp)))}
    :abilities [{:label "Move up to 3 [Credit] from credit pool to C.I. Fund"
                 :prompt "Choose how many [Credit] to move"
                 :once :per-turn
                 :choices {:number (req (min (:credit corp) 3))}
                 :effect (effect (lose :credit target)
                                 (add-counter card :credit target))
                 :msg (msg "move " target " [Credit] to C.I. Fund")}
                {:label "Take all credits from C.I. Fund"
                 :cost [:credit 2]
                 :msg (msg "trash it and gain " (get-counters card :credit) " [Credits]")
                 :effect (effect (trash card {:cause :ability-cost})
                                 (gain :credit (get-counters card :credit)))}]
    :events {:corp-turn-begins {:req (req (>= (get-counters card :credit) 6))
                                :effect (effect (add-counter card :credit 2)
                                                (system-msg (str "adds 2[Credits] to C.I. Fund")))}}}

   "City Surveillance"
   {:derezzed-events {:corp-turn-ends corp-rez-toast}
    :flags {:runner-phase-12 (req (pos? (:credit runner)))}
    :events {:runner-turn-begins
             {:player :runner
              :prompt "Pay 1[Credits] or take 1 tag"
              :choices (req [(when (pos? (:credit runner))
                              "Pay 1[Credits]")
                             "Take 1 tag"])
              :msg "make the Runner pay 1[Credits] or take 1 tag"
              :delayed-completion true
              :effect (req (case target
                             "Pay 1[Credits]"
                             (do (system-msg state :runner "pays 1[Credits]")
                                 (pay state :runner card :credit 1)
                                 (effect-completed state side eid))

                             (do (system-msg state :runner "takes 1 tag")
                                 (tag-runner state side eid 1))))}}}

   "Clone Suffrage Movement"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :flags {:corp-phase-12 (req (and (some #(is-type? % "Operation") (:discard corp))
                                     unprotected))}
    :abilities [{:label "Add 1 operation from Archives to HQ"
                 :effect (effect (show-wait-prompt :runner "Corp to use Clone Suffrage Movement")
                                 (continue-ability
                                   {:prompt "Select an operation in Archives to add to HQ"
                                    :once :per-turn
                                    :show-discard true
                                    :choices {:req #(and (is-type? % "Operation")
                                                         (= (:zone %) [:discard]))}
                                    :msg (msg "add "
                                              (if (:seen target)
                                                (:title target)
                                                "a facedown card")
                                              " to HQ")
                                    :effect (effect (move target :hand))
                                    :end-effect (effect (clear-wait-prompt :runner))}
                                   card nil))}]}

   "Clyde Van Rite"
   (let [ability {:req (req (or (pos? (:credit runner))
                                (pos? (count (:deck runner)))))
                  :player :runner
                  :once :per-turn
                  :prompt "Pay 1[Credits] or trash the top card of the Stack"
                  :choices (req (concat (when (pos? (:credit runner))
                                          ["Pay 1[Credits]"])
                                        (when (pos? (count (:deck runner)))
                                          ["Trash top card"])))
                  :msg "make the Runner pay 1[Credits] or trash the top card of the Stack"
                  :effect (req (case target
                                 "Pay 1[Credits]"
                                 (do (system-msg state side "pays 1[Credits]")
                                     (pay state side card :credit 1))
                                 "Trash top card"
                                 (do (system-msg state side "trashes the top card of the Stack")
                                     (mill state :runner))))}]
     {:derezzed-events {:runner-turn-ends corp-rez-toast}
      :flags {:corp-phase-12 (req true)}
      :events {:corp-turn-begins ability}
      :abilities [ability]})

   "Commercial Bankers Group"
   (let [ability {:req (req unprotected)
                  :label "Gain 3[Credits] (start of turn)"
                  :once :per-turn
                  :msg "gain 3[Credits]"
                  :effect (effect (gain :credit 3))}]
     {:derezzed-events {:runner-turn-ends corp-rez-toast}
      :events {:corp-turn-begins ability}
      :abilities [ability]})

   "Constellation Protocol"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
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
                                {:choices {:req #(and (ice? %)
                                                      (get-counters % :advancement))}
                                 :effect (req (let [from-ice target]
                                                (continue-ability
                                                  state side
                                                  {:prompt "Move to where?"
                                                   :choices {:req #(and (ice? %)
                                                                        (not= (:cid from-ice) (:cid %))
                                                                        (can-be-advanced? %))}
                                                   :effect (effect (add-prop :corp target :advance-counter 1)
                                                                   (add-prop :corp from-ice :advance-counter -1)
                                                                   (system-msg
                                                                     (str "uses Constellation Protocol to move an advancement token from "
                                                                          (card-str state from-ice)
                                                                          " to "
                                                                          (card-str state target))))}
                                                  card nil)))
                                 :end-effect (effect (clear-wait-prompt :runner))}
                                card nil))}]}

   "Contract Killer"
   {:advanceable :always
    :abilities [{:label "Trash a connection"
                 :delayed-completion true
                 :cost [:click 1]
                 :req (req (>= (get-counters card :advancement) 2))
                 :choices {:req #(has-subtype? % "Connection")}
                 :msg (msg "trash " (:title target))
                 :effect (effect (trash card {:cause :ability-cost})
                                 (trash eid target nil))}
                {:label "Do 2 meat damage"
                 :delayed-completion true
                 :cost [:click 1]
                 :req (req (>= (get-counters card :advancement) 2))
                 :msg "do 2 meat damage"
                 :effect (effect (trash card {:cause :ability-cost})
                                 (damage eid :meat 2 {:card card}))}]}

   "Corporate Town"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :additional-cost [:forfeit]
    :flags {:corp-phase-12 (req (and (rezzed? card)
                                     (->> (all-active-installed state :runner)
                                          (filter resource?)
                                          count
                                          pos?)))}
    :abilities [{:label "Trash a resource"
                 :once :per-turn
                 :delayed-completion true
                 :prompt "Select a resource to trash with Corporate Town"
                 :choices {:req resource?}
                 :msg (msg "trash " (:title target))
                 :effect (effect (trash eid target {:unpreventable true}))}]}

   "CPC Generator"
   {:events {:runner-click-credit {:once :per-turn
                                   :msg "gain 1[Credits]"
                                   :effect (effect (gain :corp :credit 1))}}}

   "Cybernetics Court"
   {:in-play [:hand-size 4]}

   "Daily Business Show"
   {:events {:pre-corp-draw
             {:msg "draw additional cards"
              ;; The req catches draw events that happened before DBS was rezzed.
              :req (req (first-event? state :corp :pre-corp-draw))
              ;; The once and once-key force a single DBS to act on behalf of all rezzed DBS's.
              :once :per-turn
              :once-key :daily-business-show-draw-bonus
              :effect (req (let [dbs (count (filter #(and (= "06086" (:code %))
                                                          (rezzed? %))
                                                    (all-installed state :corp)))]
                             (draw-bonus state side dbs)))}
             :post-corp-draw
             {:req (req (first-event? state :corp :post-corp-draw))
              :once :per-turn
              :once-key :daily-business-show-put-bottom
              :delayed-completion true
              :effect (req (let [dbs (count (filter #(and (= "06086" (:code %))
                                                          (rezzed? %))
                                                    (all-installed state :corp)))
                                 drawn (get-in @state [:corp :register :most-recent-drawn])]
                             (show-wait-prompt state :runner "Corp to use Daily Business Show")
                             (continue-ability
                               state side
                               {:prompt (str "Select " (quantify dbs "card") " to add to the bottom of R&D")
                                :msg (msg "add " (quantify dbs "card") " to the bottom of R&D")
                                :choices {:max dbs
                                          :req #(some (fn [c] (= (:cid c) (:cid %))) drawn)}
                                :effect (req (doseq [c targets]
                                               (move state side c :deck)))
                                :end-effect (effect (clear-wait-prompt :runner))}
                               card targets)))}}}

   "Dedicated Response Team"
   {:events {:successful-run-ends {:req (req tagged)
                                   :msg "do 2 meat damage"
                                   :delayed-completion true
                                   :effect (effect (damage eid :meat 2 {:card card}))}}}

   "Dedicated Server"
   {:recurring 2}

   "Director Haas"
   {:in-play [:click 1 :click-per-turn 1]
    :trash-effect {:when-inactive true
                   :req (req (:access @state))
                   :msg "add it to the Runner's score area as an agenda worth 2 agenda points"
                   :delayed-completion true
                   :effect (req (as-agenda state :runner eid card 2))}}

   "Docklands Crackdown"
   {:abilities [{:cost [:click 2]
                 :msg "add 1 power counter"
                 :effect (effect (add-counter card :power 1))}]
    :events {:pre-install {:req (req (and (pos? (get-counters card :power))
                                          (not (get-in @state [:per-turn (:cid card)]))))
                           :effect (effect (install-cost-bonus [:credit (get-counters card :power)]))}
             :runner-install {:silent (req true)
                              :req (req (and (pos? (get-counters card :power))
                                             (not (get-in @state [:per-turn (:cid card)]))))
                              :msg (msg "increase the install cost of " (:title target) " by " (get-counters card :power) " [Credits]")
                              :effect (req (swap! state assoc-in [:per-turn (:cid card)] true))}}}

   "Early Premiere"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :flags {:corp-phase-12 (req (some #(and (can-be-advanced? %)
                                            (in-server? %))
                                      (all-installed state :corp)))}
    :abilities [{:cost [:credit 1]
                 :label "Place 1 advancement token on a card that can be advanced in a server"
                 :choices {:req #(and (can-be-advanced? %)
                                      (installed? %)
                                      (in-server? %))} ; should be *in* a server
                 :once :per-turn
                 :msg (msg "place 1 advancement token on " (card-str state target))
                 :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]}

   "Echo Chamber"
   {:abilities [{:label "Add Echo Chamber to your score area as an agenda worth 1 agenda point"
                 :cost [:click 3]
                 :msg "add it to their score area as an agenda worth 1 agenda point"
                 :delayed-completion true
                 :effect (req (as-agenda state :corp eid card 1))}]}

   "Edge of World"
   (letfn [(ice-count [state]
             (count (get-in (:corp @state) [:servers (last (:server (:run @state))) :ices])))]
     (installed-access-trigger 3 {:msg (msg "do " (ice-count state) " brain damage")
                                  :delayed-completion true
                                  :effect (effect (damage eid :brain (ice-count state)
                                                          {:card card}))}))

   "Elizabeth Mills"
   {:effect (effect (lose :bad-publicity 1)) :msg "remove 1 bad publicity"
    :abilities [{:cost [:click 1] :label "Trash a location"
                 :msg (msg "trash " (:title target) " and take 1 bad publicity")
                 :choices {:req #(has-subtype? % "Location")}
                 :effect (effect (trash card {:cause :ability-cost})
                                 (trash target)
                                 (gain-bad-publicity :corp 1))}]}

   "Elizas Toybox"
   {:abilities [{:cost [:click 3] :choices {:req #(not (:rezzed %))}
                 :label "Rez a card at no cost" :msg (msg "rez " (:title target) " at no cost")
                 :effect (effect (rez target {:ignore-cost :all-costs}))}]}

   "Encryption Protocol"
   {:events {:pre-trash {:req (req (installed? target))
                         :effect (effect (trash-cost-bonus 1))}}}

   "Estelle Moon"
   {:events {:corp-install {:req (req (and (#{"Asset" "Agenda" "Upgrade"} (:type target))
                                           (is-remote? (second (:zone target)))))
                            :effect (effect (add-counter card :power 1)
                                            (system-msg (str "places 1 power counter on Estelle Moon")))}}
    :abilities [{:label "Draw 1 card and gain 2 [Credits] for each power counter"
                 :effect (req (let [counters (get-counters card :power)
                                    credits (* 2 counters)]
                                (trash state side card {:cause :ability-cost})
                                (draw state side counters)
                                (gain state side :credit credits)
                                (system-msg state side (str "uses Estelle Moon to draw " counters
                                                            " cards and gain " credits " [Credits]"))))}]}

   "Eve Campaign"
   (campaign 16 2)

   "Executive Boot Camp"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :flags {:corp-phase-12 (req (some #(not (rezzed? %)) (all-installed state :corp)))}
    ; A card rezzed by Executive Bootcamp is ineligible to receive the turn-begins event for this turn.
    :suppress {:corp-turn-begins {:req (req (= (:cid target) (:ebc-rezzed (get-card state card))))}}
    :events {:corp-turn-ends {:req (req (:ebc-rezzed card))
                              :effect (effect (update! (dissoc card :ebc-rezzed)))}}
    :abilities [{:choices {:req (complement rezzed?)}
                 :label "Rez a card, lowering the cost by 1[Credits]"
                 :msg (msg "rez " (:title target))
                 :delayed-completion true
                 :effect (req (rez-cost-bonus state side -1)
                              (when-completed (rez state side target {:no-warning true})
                                              (update! state side (assoc card :ebc-rezzed (:cid target)))))}
                {:prompt "Choose an asset to add to HQ"
                 :msg (msg "add " (:title target) " to HQ")
                 :activatemsg "searches R&D for an asset"
                 :choices (req (cancellable (filter #(is-type? % "Asset")
                                                    (:deck corp))
                                            :sorted))
                 :cost [:credit 1]
                 :label "Search R&D for an asset"
                 :effect (effect (trash card {:cause :ability-cost})
                                 (shuffle! :deck)
                                 (move target :hand))}]}

   "Executive Search Firm"
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
                                 (shuffle! :deck))}]}

   "Exposé"
   {:advanceable :always
    :abilities [{:label "Remove 1 bad publicity for each advancement token on Exposé"
                 :msg (msg "remove " (get-counters card :advancement) " bad publicity")
                 :effect (effect (trash card {:cause :ability-cost})
                                 (lose :bad-publicity (get-counters card :advancement)))}]}

   "False Flag"
   (letfn [(tag-count [false-flag]
             (int (/ (get-counters false-flag :advancement) 2)))]
     {:advanceable :always
      :access {:req (req (pos? (get-counters (get-card state card) :advancement)))
               :msg (msg "give the runner " (quantify (tag-count (get-card state card)) "tag"))
               :delayed-completion true
               :effect (effect (tag-runner :runner eid (tag-count (get-card state card))))}
      :abilities [{:cost [:click 1]
                   :advance-counter-cost 7
                   :label "Add False Flag to your score area as an agenda worth 3 agenda points"
                   :msg "add it to their score area as an agenda worth 3 agenda points"
                   :delayed-completion true
                   :effect (req (as-agenda state :corp eid card 3))}]})

   "Franchise City"
   {:events {:access {:req (req (is-type? target "Agenda"))
                      :msg "add it to their score area as an agenda worth 1 agenda point"
                      :delayed-completion true
                      :effect (req (as-agenda state :corp eid card 1))}}}

   "Full Immersion RecStudio"
   {:can-host (req (and (or (is-type? target "Asset") (is-type? target "Agenda"))
                        (> 2 (count (:hosted card)))))
    :trash-cost-bonus (req (* 3 (count (:hosted card))))
    :abilities [{:label "Install an asset or agenda on Full Immersion RecStudio"
                 :req (req (< (count (:hosted card)) 2))
                 :cost [:click 1]
                 :prompt "Select an asset or agenda to install"
                 :choices {:req #(and (or (is-type? % "Asset") (is-type? % "Agenda"))
                                      (in-hand? %)
                                      (= (:side %) "Corp"))}
                 :msg "install and host an asset or agenda"
                 :effect (req (corp-install state side target card))}
                {:label "Install a previously-installed asset or agenda on Full Immersion RecStudio (fixes only)"
                 :req (req (< (count (:hosted card)) 2))
                 :prompt "Select an installed asset or agenda to host on Full Immersion RecStudio"
                 :choices {:req #(and (or (is-type? % "Asset") (is-type? % "Agenda"))
                                      (installed? %)
                                      (= (:side %) "Corp"))}
                 :msg "install and host an asset or agenda"
                 :effect (req (host state side card target))}]}

   "Fumiko Yamamori"
   {:events {:psi-game-done {:req (req (not= (first targets) (second targets)))
                             :delayed-completion true
                             :msg "do 1 meat damage"
                             :effect (effect (damage eid :meat 1 {:card card}))}}}

   "Gene Splicer"
   {:advanceable :always
    :access {:req (req (pos? (get-counters (get-card state card) :advancement)))
             :msg (msg "do " (get-counters (get-card state card) :advancement) " net damage")
             :delayed-completion true
             :effect (effect (damage eid :net (get-counters (get-card state card) :advancement)
                                      {:card card}))}
    :abilities [{:cost [:click 1]
                 :advance-counter-cost 3
                 :label "Add Gene Splicing to your score area as an agenda worth 1 agenda point"
                 :msg "add it to their score area as an agenda worth 1 agenda point"
                 :delayed-completion true
                 :effect (req (as-agenda state :corp eid card 1))}]}

   "Genetics Pavilion"
   {:msg "prevent the Runner from drawing more than 2 cards during their turn"
    :effect (req (max-draw state :runner 2)
                 (when (zero? (remaining-draws state :runner))
                   (prevent-draw state :runner)))
    :events {:runner-turn-begins {:effect (effect (max-draw :runner 2))}}
    :leave-play (req (swap! state update-in [:runner :register] dissoc :max-draw :cannot-draw))}

   "Ghost Branch"
   (advance-ambush 0 {:delayed-completion true
                      :req (req (pos? (get-counters (get-card state card) :advancement)))
                      :msg (msg "give the Runner " (quantify (get-counters (get-card state card) :advancement) "tag"))
                      :effect (effect (tag-runner :runner eid (get-counters (get-card state card) :advancement)))})

   "GRNDL Refinery"
   {:advanceable :always
    :abilities [{:label "Gain 4 [Credits] for each advancement token on GRNDL Refinery"
                 :cost [:click 1]
                 :msg (msg "gain " (* 4 (get-counters card :advancement)) " [Credits]")
                 :effect (effect (trash card {:cause :ability-cost})
                                 (gain :credit (* 4 (get-counters card :advancement))))}]}

   "Haas Arcology AI"
   {:advanceable :while-unrezzed
    :abilities [{:label "Gain [Click][Click]"
                 :once :per-turn
                 :msg "gain [Click][Click]"
                 :cost [:click 1]
                 :advance-counter-cost 1
                 :effect (effect (gain :click 2))}]}

   "Honeyfarm"
   {:flags {:rd-reveal (req true)}
    :access {:msg "force the Runner to lose 1 [Credits]"
             :effect (effect (lose :runner :credit 1))}}

   "Hostile Infrastructure"
   {:events {:runner-trash {:delayed-completion true
                            :req (req (some #(card-is? % :side :corp) targets))
                            :msg (msg (str "do " (count (filter #(card-is? % :side :corp) targets))
                                           " net damage"))
                            :effect (req (letfn [(do-damage [t]
                                                   (if-not (empty? t)
                                                     (when-completed (damage state side :net 1 {:card card})
                                                                     (do-damage (rest t)))
                                                     (effect-completed state side eid card)))]
                                           (do-damage (filter #(card-is? % :side :corp) targets))))}}
    :abilities [{:msg "do 1 net damage"
                 :delayed-completion true
                 :effect (effect (damage eid :net 1 {:card card}))}]}

   "Hyoubu Research Facility"
   {:events {:psi-bet-corp {:once :per-turn
                            :msg (msg "gain " target " [Credits]")
                            :effect (effect (gain :corp :credit target))}}}

   "Ibrahim Salem"
   (let [trash-ability (fn [card-type]
                         {:req (req (seq (filter #(is-type? % card-type) (:hand runner))))
                          :prompt (str "Choose a " card-type " to trash")
                          :choices (req (filter #(is-type? % card-type) (:hand runner)))
                          :effect (effect (trash target))
                          :msg (msg " trash " (:title target) " from the Runner's Grip")})
         choose-ability {:label "Trash 1 card in the Runner's Grip of a named type"
                         :once :per-turn
                         :req (req (seq (:hand runner)))
                         :prompt "Choose a card type"
                         :choices ["Event" "Hardware" "Program" "Resource"]
                         :msg (msg "reveal " (join ", " (map :title (:hand runner))) " and trash a " target)
                         :effect (effect (resolve-ability (trash-ability target) card nil))}]
     {:additional-cost [:forfeit]
      :flags {:corp-phase-12 (constantly true)}
      :derezzed-events {:runner-turn-ends corp-rez-toast}
      :abilities [choose-ability]})

   "Illegal Arms Factory"
   (let [ability {:msg "gain 1 [Credits] and draw 1 card"
                  :label "Gain 1 [Credits] and draw 1 card (start of turn)"
                  :once :per-turn
                  :delayed-completion true
                  :req (req (:corp-phase-12 @state))
                  :effect (effect (gain :credit 1)
                                  (draw eid 1 nil))}]
     {:derezzed-events {:runner-turn-ends corp-rez-toast}
      :events {:corp-turn-begins ability}
      :abilities [ability]
      :trash-effect {:req (req (= :servers (first (:previous-zone card)))
                               (= side :runner))
                     :effect (effect (gain-bad-publicity :corp 1)
                                     (system-msg :corp (str "takes 1 bad publicity from Illegal Arms Factory")))}})

   "Indian Union Stock Exchange"
   (let [iuse {:req (req (not= (:faction target) (:faction (:identity corp))))
               :msg "gain 1 [Credits]"
               :effect (effect (gain :credit 1))}]
     {:events {:play-operation iuse
               :rez iuse}})

   "Isabel McGuire"
   {:abilities [{:label "Add an installed card to HQ"
                 :cost [:click 1]
                 :choices {:req installed?}
                 :msg (msg "move " (card-str state target) " to HQ")
                 :effect (effect (move target :hand))}]}

   "IT Department"
   {:abilities [{:counter-cost [:power 1]
                 :label "Add strength to a rezzed ICE"
                 :choices {:req #(and (ice? %) (:rezzed %))}
                 :req (req (pos? (get-counters card :power)))
                 :msg (msg "add strength to a rezzed ICE")
                 :effect (req (update! state side (update-in card [:it-targets (keyword (str (:cid target)))]
                                                             (fnil inc 0)))
                              (update-ice-strength state side target))}
                {:cost [:click 1]
                 :msg "add 1 counter"
                 :effect (effect (add-counter card :power 1))}]
    :events (let [it {:req (req (:it-targets card))
                      :effect (req (update! state side (dissoc card :it-targets))
                                   (update-all-ice state side))}]
              {:pre-ice-strength {:req (req (get-in card [:it-targets (keyword (str (:cid target)))]))
                                  :effect (effect (ice-strength-bonus
                                                    (* (get-in card [:it-targets (keyword (str (:cid target)))])
                                                       (inc (get-counters card :power))) target))}
               :runner-turn-ends it
               :corp-turn-ends it})}

   "Jackson Howard"
   {:abilities [{:cost [:click 1]
                 :msg "draw 2 cards"
                 :effect (effect (draw 2))}
                {:label "Shuffle up to 3 cards from Archives into R&D"
                 :activatemsg "removes Jackson Howard from the game"
                 :effect (effect (rfg-and-shuffle-rd-effect card 3))}]}

   "Jeeves Model Bioroids"
   (let [jeeves (effect (gain :click 1))
         ability {:label "Gain [Click]"
                  :msg "gain [Click]"
                  :once :per-turn
                  :effect jeeves}
         cleanup (effect (update! (dissoc card :seen-this-turn)))]
     {:abilities [ability]
      :leave-play cleanup
      :trash-effect {:effect cleanup}
      :events {:corp-spent-click
               {:effect (req (when-not target
                               (print-stack-trace (Exception. (str "WHY JEEVES WHY: " targets))))
                             (update! state side (update-in card [:seen-this-turn (or target :this-is-a-hack)]
                                                            (fnil + 0) (second targets)))
                             (when (>= (get-in (get-card state card) [:seen-this-turn (or target :this-is-a-hack)]) 3)
                               (resolve-ability state side ability card nil)))}
               :corp-turn-ends {:effect cleanup}}})

   "Kala Ghoda Real TV"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :flags {:corp-phase-12 (req true)}
    :abilities [{:msg "look at the top card of the Runner's Stack"
                  :effect (effect (prompt! card (str "The top card of the Runner's Stack is "
                                                     (:title (first (:deck runner)))) ["OK"] {}))}
                {:label "[Trash]: Trash the top card of the Runner's Stack"
                 :msg (msg "trash " (:title (first (:deck runner))) " from the Runner's Stack")
                 :effect (effect (trash card {:cause :ability-cost})
                                 (mill :runner))}]}

   "Kuwinda K4H1U3"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :flags {:corp-phase-12 (req true)}
    :abilities [{:label "Trace X - do 1 brain damage (start of turn)"
                 :trace {:base (req (get-counters card :power))
                         :delayed-completion true
                         :msg "do 1 brain damage"
                         :effect (effect (damage :runner eid :brain 1 {:card card})
                                         (trash card))
                         :unsuccessful {:effect (effect (add-counter card :power 1)
                                                        (system-msg "adds 1 power counter to Kuwinda K4H1U3"))}}}]}

   "Lakshmi Smartfabrics"
   {:events {:rez {:effect (effect (add-counter card :power 1))}}
    :abilities [{:req (req (seq (filter #(and (is-type? % "Agenda")
                                              (>= (get-counters card :power)
                                                  (:agendapoints %)))
                                        (:hand corp))))
                 :label "X power counters: Reveal an agenda worth X points from HQ"
                 :effect (req (let [c (get-counters card :power)]
                                (resolve-ability
                                  state side
                                  {:prompt "Select an agenda in HQ to reveal"
                                   :choices {:req #(and (is-type? % "Agenda")
                                                        (>= c (:agendapoints %)))}
                                   :msg (msg "reveal " (:title target) " from HQ")
                                   :effect (req (let [title (:title target)
                                                      pts (:agendapoints target)]
                                                  (register-turn-flag! state side
                                                    card :can-steal
                                                    (fn [state side card]
                                                      (if (= (:title card) title)
                                                        ((constantly false)
                                                         (toast state :runner "Cannot steal due to Lakshmi Smartfabrics." "warning"))
                                                        true)))
                                                  (add-counter state side card :power (- pts))))} card nil)))}]}

   "Launch Campaign"
   (campaign 6 2)

   "Levy University"
   {:abilities [{:prompt "Choose an ICE"
                 :msg (msg "adds " (:title target) " to HQ")
                 :choices (req (cancellable (filter ice? (:deck corp)) :sorted))
                 :label "Search R&D for a piece of ICE"
                 :cost [:click 1 :credit 1]
                 :effect (effect (move target :hand)
                                 (shuffle! :deck))}]}

   "Lily Lockwell"
   {:delayed-completion true
    :effect (effect (draw eid 3 nil))
    :msg (msg "draw 3 cards")
    :abilities [{:label "Remove a tag to search R&D for an operation"
                 :prompt "Choose an operation to put on top of R&D"
                 :cost [:click 1]
                 :choices (req (cancellable (filter #(is-type? % "Operation") (:deck corp)) :sorted))
                 :req (req (pos? (get-in @state [:runner :tag])))
                 :effect (req (lose state :runner :tag 1)
                              (let [c (move state :corp target :play-area)]
                                (shuffle! state :corp :deck)
                                (move state :corp c :deck {:front true})
                                (system-msg state side (str "uses Lily Lockwell to put " (:title c) " on top of R&D"))))
                 :cancel-effect (effect (lose :runner :tag 1)
                                        (shuffle! :corp :deck)
                                        (system-msg (str "uses Lily Lockwell, but did not find an Operation in R&D")))}]}

   "Long-Term Investment"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :abilities [{:label "Move any number of [Credits] to your credit pool"
                 :req (req (>= (get-counters card :credit) 8))
                 :cost [:click 1]
                 :prompt "How many [Credits]?"
                 :choices {:counter :credit}
                 :msg (msg "gain " target " [Credits]")
                 :effect (effect (gain :credit target))}]
    :events {:corp-turn-begins {:effect (effect (add-counter card :credit 2)
                                                (system-msg (str "adds 2 [Credit] to Long-Term Investment")))}}}

   "Malia Z0L0K4"
   (let [re-enable-target (req (when-let [malia-target (:malia-target card)]
                                 (system-msg state side (str "uses "  (:title card) " to unblank "
                                                             (card-str state malia-target)))
                                 (enable-card state :runner (get-card state malia-target))
                                 (when-let [reactivate-effect (:reactivate (card-def malia-target))]
                                   (resolve-ability state :runner reactivate-effect (get-card state malia-target) nil))))]
     {:effect (effect (update! (assoc card :malia-target target))
                      (disable-card :runner target))
      :msg (msg (str "blank the text box of " (card-str state target)))
      :choices {:req #(and (= (:side %) "Runner") (installed? %) (resource? %)
                           (not (has-subtype? % "Virtual")))}
      :leave-play re-enable-target
      :move-zone re-enable-target})

   "Marilyn Campaign"
   (let [ability {:msg "gain 2 [Credits]"
                  :counter-cost [:credit 2]
                  :once :per-turn
                  :req (req (:corp-phase-12 @state))
                  :label (str "Gain 2 [Credits] (start of turn)")
                  :effect (req (gain state :corp :credit 2)
                               (when (zero? (get-counters (get-card state card) :credit))
                                 (trash state :corp card)))}]
     {:effect (effect (add-counter card :credit 8))
      :flags {:corp-phase-12 (req (= 2 (get-counters card :credit)))}
      :derezzed-events {:runner-turn-ends corp-rez-toast}
      :events {:corp-turn-begins ability}
      :abilities [ability]
      :trash-effect {:req (req (= :servers (first (:previous-zone card))))
                     :delayed-completion true
                     :effect (effect (show-wait-prompt :runner "Corp to use Marilyn Campaign")
                                     (continue-ability :corp
                                       {:optional
                                        {:prompt "Shuffle Marilyn Campaign into R&D?"
                                         :player :corp
                                         :yes-ability {:msg "shuffle it back into R&D"
                                                       :effect (req (move state :corp card :deck)
                                                                    (shuffle! state :corp :deck)
                                                                    (effect-completed state side eid))}
                                         :end-effect (effect (clear-wait-prompt :runner))}}
                                      card nil))}})

   "Mark Yale"
   {:events {:agenda-counter-spent {:msg "gain 1 [Credits]"
                                    :effect (effect (gain :credit 1))}}
    :abilities [{:label "Trash to gain 2 [Credits]"
                 :msg "gain 2 [Credits]"
                 :effect (effect (trash card {:cause :ability-cost})
                                 (gain :credit 2))}
                {:label "Spend an agenda counter to gain 2 [Credits]"
                 :effect (effect (continue-ability
                                   {:prompt "Select an agenda with a counter"
                                    :choices {:req #(and (is-type? % "Agenda")
                                                         (pos? (get-counters % :agenda)))}
                                    :msg (msg "spend an agenda token on " (:title target) " and gain 2 [Credits]")
                                    :effect (effect (add-counter target :agenda -1)
                                                    (gain :credit 2)
                                                    (trigger-event :agenda-counter-spent card))}
                                   card nil))}]}

   "Marked Accounts"
   (let [ability {:msg "gain 1 [Credits]"
                  :label "Gain 1 [Credits] (start of turn)"
                  :once :per-turn
                  :counter-cost [:credit 1]
                  :effect (effect (gain :credit 1))}]
   {:abilities [ability
                {:cost [:click 1]
                 :msg "store 3 [Credits]"
                 :effect (effect (add-counter card :credit 3))}]
    :events {:corp-turn-begins ability}})

   "MCA Austerity Policy"
   {:abilities [{:cost [:click 1]
                 :once :per-turn
                 :msg "to force the Runner to lose a [Click] next turn and place a power counter on itself"
                 :effect (req (swap! state update-in [:runner :extra-click-temp] (fnil dec 0))
                              (add-counter state side card :power 1))}
                {:cost [:click 1]
                 :counter-cost [:power 3]
                 :msg "gain 4 [Click] and trash itself"
                 :effect (effect (trash card {:cause :ability-cost
                                              :unpreventable true})
                                 (gain :click 4))}]}

   "Melange Mining Corp."
   {:abilities [{:cost [:click 3]
                 :msg "gain 7 [Credits]"
                 :effect (effect (gain :credit 7))}]}

   "Mental Health Clinic"
   (let [ability {:msg "gain 1 [Credits]"
                  :label "Gain 1 [Credits] (start of turn)"
                  :once :per-turn
                  :effect (effect (gain :credit 1))}]
     {:effect (effect (gain :runner :hand-size 1))
      :leave-play (effect (lose :runner :hand-size 1))
      :derezzed-events {:runner-turn-ends corp-rez-toast}
      :events {:corp-turn-begins ability}
      :abilities [ability]})

   "Mr. Stone"
   {:events {:runner-gain-tag {:delayed-completion true
                               :msg "do 1 meat damage"
                               :effect (effect (damage :corp eid :meat 1 {:card card}))}}}

   "Mumba Temple"
   {:recurring 2}

   "Mumbad City Hall"
   {:abilities [{:label "Search R&D for an Alliance card"
                 :cost [:click 1]
                 :prompt "Choose an Alliance card to play or install"
                 :choices (req (cancellable (filter #(and (has-subtype? % "Alliance")
                                                          (if (is-type? % "Operation")
                                                            (<= (:cost %) (:credit corp))
                                                            true))
                                                    (:deck corp))
                                            :sorted))
                 :msg (msg "reveal " (:title target)
                           " from R&D and "
                           (if (= (:type target) "Operation") "play" "install")
                           " it")
                 :effect (req (shuffle! state side :deck)
                              (if (= (:type target) "Operation")
                                (play-instant state side target)
                                (corp-install state side target nil nil)))}]}

   "Mumbad Construction Co."
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :events {:corp-turn-begins {:effect (effect (add-prop card :advance-counter 1 {:placed true}))}}
    :abilities [{:cost [:credit 2]
                 :req (req (and (pos? (get-counters card :advancement))
                                (not-empty (all-active-installed state :corp))))
                 :label "Move an advancement token to a faceup card"
                 :prompt "Select a faceup card"
                 :choices {:req rezzed?}
                 :msg (msg "move an advancement token to " (card-str state target))
                 :effect (effect (add-prop card :advance-counter -1 {:placed true})
                                 (add-prop target :advance-counter 1 {:placed true}))}]}

   "Museum of History"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :flags {:corp-phase-12 (req (pos? (count (get-in @state [:corp :discard]))))}
    :abilities [{:label "Shuffle cards in Archives into R&D"
                 :prompt (msg (let [mus (count (filter #(and (= "10019" (:code %))
                                                             (rezzed? %))
                                                       (all-installed state :corp)))]
                                (str "Select "
                                     (if (> mus 1) "a card " (str mus " cards "))
                                     "in Archives to shuffle into R&D")))
                 :choices {:req #(and (card-is? % :side :corp)
                                      (= (:zone %) [:discard]))
                           :max (req (count (filter #(and (= "10019" (:code %))
                                                          (rezzed? %))
                                                    (all-installed state :corp))))}
                 :show-discard true
                 :priority 1
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
                              (shuffle! state side :deck))}]}

   "NASX"
   (let [ability {:msg "gain 1 [Credits]"
                  :label "Gain 1 [Credits] (start of turn)"
                  :once :per-turn
                  :effect (effect (gain :credit 1))}]
     {:implementation "Manual - click NASX to add power counters"
      :derezzed-events {:runner-turn-ends corp-rez-toast}
      :events {:corp-turn-begins ability}
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
                   :cost [:click 1]
                   :msg (msg "gain " (* 2 (get-counters card :power)) " [Credits]")
                   :effect (effect (trash card {:cause :ability-cost})
                                   (gain :credit (* 2 (get-counters card :power))))}]})

   "Net Analytics"
   (let [ability {:req (req (seq (filter #(some #{:tag} %) targets)))
                  :effect (effect (show-wait-prompt :runner "Corp to use Net Analytics")
                                  (continue-ability :corp
                                    {:optional
                                     {:prompt "Draw from Net Analytics?"
                                      :yes-ability {:msg (msg "draw a card")
                                                    :effect (effect (draw :corp 1))}
                                      :end-effect (effect (clear-wait-prompt :runner))}}
                                    card nil))}]
     {:events {:runner-loss ability
               :runner-prevent ability}})

   "Net Police"
   {:recurring (effect (set-prop card :rec-counter (:link runner)))
    :effect (effect (set-prop card :rec-counter (:link runner)))}

   "News Team"
   {:flags {:rd-reveal (req true)}
    :access {:msg (msg "force the Runner take 2 tags or add it to their score area as an agenda worth -1 agenda point")
             :delayed-completion true
             :effect (effect (continue-ability
                               {:player :runner
                                :delayed-completion true
                                :prompt "Take 2 tags or add News Team to your score area as an agenda worth -1 agenda point?"
                                :choices ["Take 2 tags" "Add News Team to score area"]
                                :effect (req (if (= target "Add News Team to score area")
                                               (do (system-msg state :runner (str "adds News Team to their score area as an agenda worth -1 agenda point"))
                                                   (trigger-event state side :no-trash card)
                                                   (as-trashed-agenda state :runner eid card -1 {:force true}))
                                               (do (system-msg state :runner (str "takes 2 tags from News Team"))
                                                   (tag-runner state :runner eid 2))))}
                               card targets))}}

   "NGO Front"
   (letfn [(builder [cost cred]
             {:advance-counter-cost cost
              :effect (effect (trash card {:cause :ability-cost})
                              (gain :credit cred))
              :label (str "[Trash]: Gain " cred " [Credits]")
              :msg (str "gain " cred " [Credits]")})]
     {:advanceable :always
      :abilities [(builder 1 5)
                  (builder 2 8)]})

   "Open Forum"
   {:events {:corp-mandatory-draw
             {:interactive (req true)
              :msg (msg (if (-> corp :deck count pos?)
                          (str "reveal and draw " (-> corp :deck first :title) " from R&D")
                          "reveal and draw from R&D but it is empty"))
              :delayed-completion true
              :effect (effect (draw 1)
                              (continue-ability
                                {:prompt "Choose a card in HQ to put on top of R&D"
                                 :delayed-completion true
                                 :choices {:req #(and (in-hand? %)
                                                      (= (:side %) "Corp"))}
                                 :msg "add 1 card from HQ to the top of R&D"
                                 :effect (effect (move target :deck {:front true})
                                                 (effect-completed eid))}
                                card nil))}}}

   "PAD Campaign"
   (let [ability {:msg "gain 1 [Credits]"
                  :label "Gain 1 [Credits] (start of turn)"
                  :once :per-turn
                  :effect (effect (gain :credit 1))}]
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :events {:corp-turn-begins ability}
    :abilities [ability]})

   "PAD Factory"
   {:abilities [{:cost [:click 1]
                 :label "Place 1 advancement token on a card"
                 :choices {:req installed?}
                 :msg (msg "place 1 advancement token on " (card-str state target))
                 :effect (req (add-prop state :corp target :advance-counter 1 {:placed true})
                              (let [tgtcid (:cid target)]
                                (register-turn-flag! state side
                                  target :can-score
                                  (fn [state side card]
                                    (if (and (= tgtcid
                                                (:cid card))
                                             (>= (get-counters card :advancement)
                                                 (or (:current-cost card)
                                                     (:advancementcost card))))
                                      ((constantly false) (toast state :corp "Cannot score due to PAD Factory." "warning"))
                                      true)))))}]}

   "Pālanā Agroplex"
   (let [ability {:msg "make each player draw 1 card"
                  :label "Make each player draw 1 card (start of turn)"
                  :once :per-turn
                  :effect (effect (draw 1) (draw :runner))}]
     {:derezzed-events {:runner-turn-ends corp-rez-toast}
      :flags {:corp-phase-12 (req true)}
      :events {:corp-turn-begins ability}
      :abilities [ability]})

   "Personalized Portal"
   {:events {:corp-turn-begins
             {:effect (req (draw state :runner 1)
                           (let [cnt (count (get-in @state [:runner :hand]))
                                 credits (quot cnt 2)]
                             (gain state :corp :credit credits)
                             (system-msg state :corp
                                         (str "uses Personalized Portal to force the runner to draw "
                                              "1 card and gains " credits " [Credits]"))))}}}

   "Plan B"
   (advance-ambush
    0
    {:req (req (pos? (get-counters (get-card state card) :advancement)))
     :effect (req (show-wait-prompt state :runner "Corp to select an agenda to score with Plan B")
                  (doseq [ag (filter #(is-type? % "Agenda") (:hand corp))]
                    (update-advancement-cost state side ag))
                  (resolve-ability
                    state side
                    {:prompt "Select an Agenda in HQ to score"
                     :choices {:req #(and (is-type? % "Agenda")
                                          (<= (:current-cost %) (get-counters (get-card state card) :advancement))
                                          (in-hand? %))}
                     :msg (msg "score " (:title target))
                     :effect (effect (score (assoc target :advance-counter
                                                   (:current-cost target)))
                                     (clear-wait-prompt :runner))}
                    card nil))}
    "Score an Agenda from HQ?")

   "Political Dealings"
   (letfn [(pdhelper [agendas n]
             {:optional
              {:prompt (msg "Reveal and install " (:title (nth agendas n)) "?")
               :yes-ability {:delayed-completion true
                             :msg (msg "reveal " (:title (nth agendas n)))
                             :effect (req (when-completed (corp-install
                                                            state side (nth agendas n) nil
                                                            {:install-state
                                                             (:install-state
                                                               (card-def (nth agendas n))
                                                               :unrezzed)})
                                            (if (< (inc n) (count agendas))
                                              (continue-ability state side (pdhelper agendas (inc n)) card nil)
                                              (effect-completed state side eid))))}
               :no-ability {:delayed-completion true
                            :effect (req (if (< (inc n) (count agendas))
                                           (continue-ability state side (pdhelper agendas (inc n)) card nil)
                                           (effect-completed state side eid)))}}})]
     {:events
      {:corp-draw
       {:delayed-completion true
        :req (req (let [drawn (get-in @state [:corp :register :most-recent-drawn])
                        agendas (filter #(is-type? % "Agenda") drawn)]
                    (seq agendas)))
        :effect (req (let [drawn (get-in @state [:corp :register :most-recent-drawn])
                           agendas (filter #(is-type? % "Agenda") drawn)]
                       (continue-ability state side (pdhelper agendas 0) card nil)))}}})

   "Primary Transmission Dish"
   {:recurring 3}

   "Private Contracts"
   {:effect (effect (add-counter card :credit 14))
    :abilities [{:cost [:click 1]
                 :counter-cost [:credit 2]
                 :msg "gain 2 [Credits]"
                 :effect (req (gain state :corp :credit 2)
                              (when (zero? (get-counters (get-card state card) :credit))
                                (trash state :corp card)))}]}

   "Project Junebug"
   (advance-ambush 1 {:req (req (pos? (get-counters (get-card state card) :advancement)))
                      :msg (msg "do " (* 2 (get-counters (get-card state card) :advancement)) " net damage")
                      :delayed-completion true
                      :effect (effect (damage eid :net (* 2 (get-counters (get-card state card) :advancement))
                                              {:card card}))})

   "Psychic Field"
   (let [ab {:psi {:req (req installed)
                   :not-equal {:msg (msg "do " (count (:hand runner)) " net damage")
                               :delayed-completion true
                               :effect (effect (damage eid :net (count (:hand runner)) {:card card}))}}}]
     {:expose ab :access ab})

   "Public Support"
   {:effect (effect (add-counter card :power 3))
    :derezzed-events {:runner-turn-ends corp-rez-toast}
    :events {:corp-turn-begins
             {:delayed-completion true
              :effect (req (add-counter state side card :power -1)
                           (if (zero? (get-counters (get-card state card) :power))
                             (do (system-msg state :corp "uses Public Support to add it to their score area as an agenda worth 1 agenda point")
                                 (as-agenda state :corp eid (dissoc card :counter) 1))
                             (effect-completed state side eid)))} }}

   "Quarantine System"
   (letfn [(rez-ice [cnt] {:prompt "Select an ICE to rez"
                           :delayed-completion true
                           :choices {:req #(and (ice? %) (not (rezzed? %)))}
                           :msg (msg "rez " (:title target))
                           :effect (req (let [agenda (last (:rfg corp))
                                              ap (:agendapoints agenda 0)]
                                          (rez-cost-bonus state side (* ap -2))
                                          (rez state side target {:no-warning true})
                                          (if (< cnt 3) (continue-ability state side (rez-ice (inc cnt)) card nil)
                                                        (effect-completed state side eid))))})]
     {:abilities [{:label "Forfeit agenda to rez up to 3 ICE with a 2 [Credit] discount per agenda point"
                   :req (req (pos? (count (:scored corp))))
                   :cost [:forfeit]
                   :effect (req (continue-ability state side (rez-ice 1) card nil))}]})

   "Raman Rai"
   {:abilities [{:once :per-turn
                 :label "Lose [Click] and swap a card in HQ you just drew for a card in Archives"
                 :req (req (and (pos? (:click corp))
                                (not-empty (turn-events state side :corp-draw))))
                 :effect (req (let [drawn (get-in @state [:corp :register :most-recent-drawn])]
                                (lose state :corp :click 1)
                                (resolve-ability state side
                                  {:prompt "Choose a card in HQ that you just drew to swap for a card of the same type in Archives"
                                   :choices {:req #(some (fn [c] (= (:cid c) (:cid %))) drawn)}
                                   :effect (req (let [hqcard target
                                                      t (:type hqcard)]
                                                  (resolve-ability state side
                                                    {:show-discard true
                                                     :prompt (msg "Choose an " t " in Archives to reveal and swap into HQ for " (:title hqcard))
                                                     :choices {:req #(and (= (:side %) "Corp")
                                                                          (= (:type %) t)
                                                                          (= (:zone %) [:discard]))}
                                                     :msg (msg "lose [Click], reveal " (:title hqcard) " from HQ, and swap it for " (:title target) " from Archives")
                                                     :effect (req (let [swappedcard (assoc hqcard :zone [:discard])
                                                                        archndx (ice-index state target)
                                                                        arch (get-in @state [:corp :discard])
                                                                        newarch (apply conj (subvec arch 0 archndx) swappedcard (subvec arch archndx))]
                                                                     (swap! state assoc-in [:corp :discard] newarch)
                                                                     (swap! state update-in [:corp :hand]
                                                                            (fn [coll] (remove-once #(= (:cid %) (:cid hqcard)) coll)))
                                                                     (move state side target :hand)))}
                                                   card nil)))}
                                 card nil)))}]}

   "Rashida Jaheem"
   (let [ability {:once :per-turn
                  :label "Gain 3 [Credits] and draw 3 cards (start of turn)"
                  :effect (effect (resolve-ability
                                    {:optional
                                     {:prompt "Trash Rashida Jaheem to gain 3 [Credits] and draw 3 cards?"
                                      :yes-ability {:delayed-completion true
                                                    :msg "gain 3 [Credits] and draw 3 cards"
                                                    :effect (req (when-completed (trash state side card nil)
                                                                                 (do (gain state side :credit 3)
                                                                                     (draw state side eid 3 nil))))}}}
                                    card nil))}]
     {:derezzed-events {:runner-turn-ends corp-rez-toast}
      :flags {:corp-phase-12 (req true)}
      :events {:corp-turn-begins ability}
      :abilities [ability]})

   "Reality Threedee"
   (let [ability {:effect (req (gain state side :credit (if tagged 2 1)))
                  :label "Gain credits (start of turn)"
                  :once :per-turn
                  :msg (msg (if tagged "gain 2 [Credits]" "gain 1 [Credits]"))}]
   {:effect (effect (gain-bad-publicity :corp 1)
                    (system-msg "takes 1 bad publicity"))
    :derezzed-events {:runner-turn-ends corp-rez-toast}
    :events {:corp-turn-begins ability}
    :abilities [ability]})

   "Reconstruction Contract"
   {:events {:damage {:req (req (and (pos? (nth targets 2)) (= :meat target)))
                      :effect (effect (add-counter card :advancement 1)
                                      (system-msg "adds 1 advancement token to Reconstruction Contract"))}}
    :abilities [{:label "[Trash]: Move advancement tokens to another card"
                 :prompt "Select a card that can be advanced"
                 :choices {:req can-be-advanced?}
                 :effect (req (let [move-to target]
                                (resolve-ability
                                  state side
                                  {:prompt "Move how many tokens?"
                                   :choices {:number (req (get-counters card :advancement))
                                             :default (req (get-counters card :advancement))}
                                   :effect (effect (trash card {:cause :ability-cost})
                                                   (add-counter move-to :advancement target {:placed true})
                                                   (system-msg (str "trashes Reconstruction Contract to move " target
                                                                    (pluralize " advancement token" target) " to "
                                                                    (card-str state move-to))))}
                                  card nil)))}]}

   "Reversed Accounts"
   {:advanceable :always
    :abilities [{:cost [:click 1]
                 :label "Force the Runner to lose 4 [Credits] per advancement"
                 :msg (msg "force the Runner to lose " (min (* 4 (get-counters card :advancement)) (:credit runner)) " [Credits]")
                 :effect (effect (trash card {:cause :ability-cost})
                                 (lose :runner :credit (* 4 (get-counters card :advancement))))}]}

   "Rex Campaign"
   (let [ability {:once :per-turn
                  :label "Remove 1 counter (start of turn)"
                  :effect (req (add-counter state side card :power -1)
                               (when (zero? (get-counters (get-card state card) :power))
                                 (trash state side card)
                                 (resolve-ability
                                   state side
                                   {:prompt "Remove 1 bad publicity or gain 5 [Credits]?"
                                    :choices ["Remove 1 bad publicity" "Gain 5 [Credits]"]
                                    :msg (msg (if (= target "Remove 1 bad publicity")
                                                "remove 1 bad publicity" "gain 5 [Credits]"))
                                    :effect (req (if (= target "Remove 1 bad publicity")
                                                   (lose state side :bad-publicity 1)
                                                   (gain state side :credit 5)))}
                                   card targets)))}]
   {:effect (effect (add-counter card :power 3))
    :derezzed-events {:runner-turn-ends corp-rez-toast}
    :events {:corp-turn-begins ability}
    :ability [ability]})

   "Ronald Five"
   {:events {:runner-trash {:req (req (and (= (:side target) "Corp")
                                           (pos? (:click runner))))
                            :msg "force the runner to lose 1 [Click]"
                            :effect (effect (lose :runner :click 1))}}}

   "Ronin"
   {:advanceable :always
    :abilities [{:cost [:click 1]
                 :req (req (>= (get-counters card :advancement) 4))
                 :msg "do 3 net damage"
                 :delayed-completion true
                 :effect (effect (trash card {:cause :ability-cost})
                                 (damage eid :net 3 {:card card}))}]}

   "Sandburg"
   {:effect (req (add-watch state :sandburg
                            (fn [k ref old new]
                              (let [credit (get-in new [:corp :credit])]
                                (when (not= (get-in old [:corp :credit]) credit)
                                  (update-all-ice ref side)))))
                 (update-all-ice state side))
    :events {:pre-ice-strength {:req (req (and (ice? target)
                                               (>= (:credit corp) 10)))
                                :effect (effect (ice-strength-bonus (quot (:credit corp) 5) target))}}
    :leave-play (req (remove-watch state :sandburg)
                     (update-all-ice state side))}

   "Sealed Vault"
   {:abilities [{:label "Store any number of [Credits] on Sealed Vault"
                 :cost [:credit 1]
                 :prompt "How many [Credits]?"
                 :choices {:number (req (- (:credit corp) 1))}
                 :msg (msg "store " target " [Credits]")
                 :effect (effect (lose :credit target)
                                 (add-counter card :credit target))}
                {:label "Move any number of [Credits] to your credit pool"
                 :cost [:click 1]
                 :prompt "How many [Credits]?"
                 :choices {:counter :credit}
                 :msg (msg "gain " target " [Credits]")
                 :effect (effect (gain :credit target))}
                {:label "[Trash]: Move any number of [Credits] to your credit pool"
                 :prompt "How many [Credits]?"
                 :choices {:counter :credit}
                 :msg (msg "trash it and gain " target " [Credits]")
                 :effect (effect (trash card {:cause :ability-cost})
                                 (gain :credit target))}]}

   "Security Subcontract"
   {:abilities [{:choices {:req #(and (ice? %)
                                      (rezzed? %))}
                 :cost [:click 1]
                 :msg (msg "trash " (:title target) " to gain 4 [Credits]")
                 :label "Trash a rezzed ICE to gain 4 [Credits]"
                 :effect (effect (trash target {:cause :ability-cost})
                                 (gain :credit 4))}]}

   "Sensie Actors Union"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :flags {:corp-phase-12 (req unprotected)}
    :abilities [{:label "Draw 3 cards and add 1 card in HQ to the bottom of R&D"
                 :once :per-turn
                 :msg "draw 3 cards"
                 :effect (effect (draw 3)
                                 (resolve-ability
                                   {:prompt "Select a card in HQ to add to the bottom of R&D"
                                    :choices {:req #(and (= (:side %) "Corp")
                                                         (in-hand? %))}
                                    :msg "add 1 card from HQ to the bottom of R&D"
                                    :effect (effect (move target :deck))}
                                  card nil))}]}

   "Server Diagnostics"
   (let [ability {:effect (effect (gain :credit 2))
                  :once :per-turn
                  :label "Gain 2 [Credits] (start of turn)"
                  :msg "gain 2 [Credits]"}]
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :abilities [ability]
    :events {:corp-turn-begins ability
             :corp-install {:req (req (ice? target))
                            :effect (effect (trash card)
                                            (system-msg "trashes Server Diagnostics"))}}})

   "Shannon Claire"
   {:abilities [{:cost [:click 1]
                 :msg "draw 1 card from the bottom of R&D"
                 :effect (effect (move (last (:deck corp)) :hand))}
                {:label "[Trash]: Search R&D for an agenda"
                 :prompt "Choose an agenda to add to the bottom of R&D"
                 :msg (msg "reveal " (:title target) " from R&D and add it to the bottom of R&D")
                 :choices (req (cancellable (filter #(is-type? % "Agenda") (:deck corp)) :sorted))
                 :effect (effect (trash card {:cause :ability-cost})
                                 (shuffle! :deck)
                                 (move target :deck))}
                {:label "[Trash]: Search Archives for an agenda"
                 :prompt "Choose an agenda to add to the bottom of R&D"
                 :msg (msg "reveal " (:title target) " from Archives and add it to the bottom of R&D")
                 :choices (req (cancellable (filter #(is-type? % "Agenda") (:discard corp)) :sorted))
                 :effect (effect (trash card {:cause :ability-cost})
                                 (move target :deck))}]}

   "Shattered Remains"
   (advance-ambush 1 {:delayed-completion true
                      :req (req (pos? (get-counters (get-card state card) :advancement)))
                      :effect (req (let [counters (get-counters (get-card state card) :advancement)]
                                     (continue-ability
                                       state side
                                       (-> trash-hardware
                                           (assoc-in [:choices :max] counters)
                                           (assoc :prompt (msg "Select " (quantify counters "piece") " of hardware to trash")
                                                  :effect (effect (trash-cards targets))
                                                  :msg (msg "trash " (join ", " (map :title targets)))))
                                       card nil)))})

   "Shi.Kyū"
   {:access
    {:delayed-completion true
     :req (req (not= (first (:zone card)) :deck))
     :effect (effect (show-wait-prompt :runner "Corp to use Shi.Kyū")
                     (continue-ability
                       {:optional
                        {:prompt "Pay [Credits] to use Shi.Kyū?"
                         :yes-ability
                         {:prompt "How many [Credits] for Shi.Kyū?"
                          :choices :credit
                          :msg (msg "attempt to do " target " net damage")
                          :delayed-completion true
                          :effect (req (let [dmg target]
                                         (clear-wait-prompt state :runner)
                                         (continue-ability
                                           state :corp
                                           {:player :runner
                                            :prompt (str "Take " dmg " net damage or add Shi.Kyū to your score area as an agenda worth -1 agenda point?")
                                            :choices [(str "Take " dmg " net damage") "Add Shi.Kyū to score area"]
                                            :delayed-completion true
                                            :effect (req (if (= target "Add Shi.Kyū to score area")
                                                           (do (system-msg state :runner (str "adds Shi.Kyū to their score area as as an agenda worth -1 agenda point"))
                                                               (trigger-event state side :no-trash card)
                                                               (as-trashed-agenda state :runner eid card -1 {:force true}))
                                                           (do (damage state :corp eid :net dmg {:card card})
                                                               (system-msg state :runner (str "takes " dmg " net damage from Shi.Kyū")))))}
                                           card targets)))}
                         :no-ability {:effect (effect (clear-wait-prompt :runner))}}}
                       card targets))}}

   "Shock!"
   {:flags {:rd-reveal (req true)}
    :access {:msg "do 1 net damage"
             :delayed-completion true
             :effect (effect (damage eid :net 1 {:card card}))}}

   "Snare!"
   {:flags {:rd-reveal (req true)}
    :access {:req (req (not= (first (:zone card)) :discard))
             :delayed-completion true
             :effect (effect (show-wait-prompt :runner "Corp to use Snare!")
                             (continue-ability
                               {:optional
                                {:prompt "Pay 4 [Credits] to use Snare! ability?"
                                 :end-effect (effect (clear-wait-prompt :runner))
                                 :yes-ability {:delayed-completion true
                                               :cost [:credit 4]
                                               :msg "do 3 net damage and give the Runner 1 tag"
                                               :effect (req (when-completed (damage state side :net 3 {:card card})
                                                                            (tag-runner state :runner eid 1)))}}}
                               card nil))}}

   "Space Camp"
   {:flags {:rd-reveal (req true)}
    :access {:delayed-completion true
             :effect (effect (show-wait-prompt :runner "Corp to use Space Camp")
                             (continue-ability
                               {:optional
                                {:prompt "Place 1 advancement token with Space Camp?"
                                 :cancel-effect (req (clear-wait-prompt state :runner)
                                                     (effect-completed state side eid))
                                 :yes-ability {:msg (msg "place 1 advancement token on " (card-str state target))
                                               :prompt "Select a card to place an advancement token on with Space Camp"
                                               :choices {:req can-be-advanced?}
                                               :effect (effect (add-prop target :advance-counter 1 {:placed true})
                                                               (clear-wait-prompt :runner))}
                                 :no-ability {:effect (req (clear-wait-prompt state :runner)
                                                           (effect-completed state side eid))}}}
                               card nil))}}

   "Student Loans"
   {:events {:pre-play-instant
             {:req (req (and (is-type? target "Event") (seq (filter #(= (:title %) (:title target)) (:discard runner)))))
              :effect (effect (system-msg :corp (str "makes the runner pay an extra 2 [Credits] due to Student Loans"))
                              (play-cost-bonus [:credit 2]))}}}

   "Sundew"
   {:events {:runner-spent-click {:once :per-turn
                                  :msg (req (when (not this-server) "gain 2 [Credits]"))
                                  :effect (req (when (not this-server)
                                                 (gain state :corp :credit 2)))}}}

   "Synth DNA Modification"
   {:implementation "Manual fire once subroutine is broken"
    :abilities [{:msg "do 1 net damage"
                 :label "Do 1 net damage after AP subroutine broken"
                 :once :per-turn
                 :effect (effect (damage eid :net 1 {:card card}))}]}

   "Team Sponsorship"
   {:events {:agenda-scored {:label "Install a card from Archives or HQ"
                             :prompt "Select a card from Archives or HQ to install"
                             :show-discard true
                             :interactive (req true)
                             :delayed-completion true
                             :choices {:req #(and (not (is-type? % "Operation"))
                                                  (= (:side %) "Corp")
                                                  (#{[:hand] [:discard]} (:zone %)))}
                             :msg (msg (corp-install-msg target))
                             :effect (effect (corp-install eid target nil {:no-install-cost true}))}}}

   "Tech Startup"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :flags {:corp-phase-12 (req true)}
    :abilities [{:label "Install an asset from R&D"
                 :prompt "Choose an asset to install"
                 :msg (msg "install " (:title target))
                 :choices (req (filter #(is-type? % "Asset") (:deck corp)))
                 :effect (effect (trash card)
                                 (shuffle! :deck)
                                 (corp-install target nil))}]}

   "TechnoCo"
   (letfn [(is-techno-target [card]
             (or (is-type? card "Program")
                 (is-type? card "Hardware")
                 (and (is-type? card "Resource") (has-subtype? card "Virtual"))))]
     {:events {:pre-install {:req (req (and (is-techno-target target)
                                            (not (second targets)))) ; not facedown
                             :effect (effect (install-cost-bonus [:credit 1]))}
               :runner-install {:req (req (and (is-techno-target target)
                                               (not (second targets)))) ; not facedown
                                :msg "gain 1 [Credits]"
                                :effect (req (gain state :corp :credit 1))}}})

   "Tenma Line"
   {:abilities [{:label "Swap 2 pieces of installed ICE"
                 :cost [:click 1]
                 :prompt "Select two pieces of ICE to swap positions"
                 :choices {:req #(and (installed? %)
                                      (ice? %))
                           :max 2}
                 :effect (req (when (= (count targets) 2)
                                (swap-ice state side (first targets) (second targets))))
                 :msg (msg "swap the positions of "
                           (card-str state (first targets))
                           " and "
                           (card-str state (second targets)))}]}

   "Test Ground"
   {:implementation "Derez is manual"
    :advanceable :always
    :abilities [{:label "Derez 1 card for each advancement token"
                 :msg (msg "derez " (get-counters card :advancement))
                 :effect (effect (trash card {:cause :ability-cost}))}]}

   "The Board"
   (let [the-board {:req (req (and (= :runner (:as-agenda-side target))
                                   (not= (:cid target) (:cid card))))
                    :effect (effect (lose :runner :agenda-point 1))}]
         {:effect (effect (lose :runner :agenda-point (count (:scored runner))))
          :leave-play (effect (gain :runner :agenda-point (count (:scored runner))))
          :trash-effect {:when-inactive true
                         :req (req (:access @state))
                         :msg "add it to the Runner's score area as an agenda worth 2 agenda points"
                         :delayed-completion true
                         :effect (req (as-agenda state :runner eid card 2))}
          :events {:agenda-stolen (dissoc the-board :req)
                   :as-agenda the-board
                   :pre-card-moved {:req (req (let [c (first targets)
                                                    c-cid (:cid c)]
                                                (some #(when (= c-cid (:cid %)) %) (:scored runner))))
                                    :effect (req (gain state :runner :agenda-point 1))}}})

   "The News Now Hour"
   {:events {:runner-turn-begins {:effect (req (prevent-current state side))}}
    :effect (req (prevent-current state side))
    :leave-play (req (swap! state assoc-in [:runner :register :cannot-play-current] false))}

   "The Root"
   {:recurring 3}

   "Thomas Haas"
   {:advanceable :always
    :abilities [{:label "Gain credits"
                 :msg (msg "gain " (* 2 (get-counters card :advancement)) " [Credits]")
                 :effect (effect (trash card {:cause :ability-cost})
                                 (gain :credit (* 2 (get-counters card :advancement))))}]}

   "Toshiyuki Sakai"
   (advance-ambush 0
    {:effect (effect (resolve-ability
                       {:prompt "Select an asset or agenda in HQ"
                        :choices {:req #(and (or (is-type? % "Agenda")
                                                 (is-type? % "Asset"))
                                             (in-hand? %))}
                        :msg "swap it for an asset or agenda from HQ"
                        :effect (req (let [tidx (ice-index state card)
                                           srvcont (get-in @state (cons :corp (:zone card)))
                                           c (get-counters (get-card state card) :advancement)
                                           newcard (assoc target :zone (:zone card) :advance-counter c)
                                           newcont (apply conj (subvec srvcont 0 tidx) newcard (subvec srvcont tidx))]
                                       (resolve-ability state side
                                         {:effect (req (swap! state assoc-in (cons :corp (:zone card)) newcont)
                                                       (swap! state update-in [:corp :hand]
                                                         (fn [coll] (remove-once #(= (:cid %) (:cid newcard)) coll)))
                                                       (trigger-event state side :corp-install newcard)
                                                       (move state side card :hand))} card nil)
                                       (resolve-prompt state :runner {:choice "No"})
                                       ; gets rid of prompt to trash Toshiyuki since it's back in HQ now
                                       (resolve-ability state :runner
                                         {:optional
                                          {:player :runner
                                           :priority true
                                           :prompt "Access the newly installed card?"
                                           :yes-ability {:effect (effect (access-card newcard))}}}
                                         card nil)))}
                      card nil))}
    "Swap Toshiyuki Sakai with an agenda or asset from HQ?")

   "Turtlebacks"
   {:events {:server-created {:msg "gain 1 [Credits]"
                              :effect (effect (gain :credit 1))}}}

   "Urban Renewal"
   {:effect (effect (add-counter card :power 3))
    :derezzed-events {:runner-turn-ends corp-rez-toast}
    :events {:corp-turn-begins
             {:delayed-completion true
              :effect (req (add-counter state side card :power -1)
                           (if (zero? (get-counters (get-card state card) :power))
                             (when-completed (trash state side card nil)
                               (do (system-msg state :corp "uses Urban Renewal to do 4 meat damage")
                                   (damage state side eid :meat 4 {:card card})))
                             (effect-completed state side eid)))}}}

   "Victoria Jenkins"
   {:effect (req (lose state :runner :click-per-turn 1)
                 (when (= (:active-player @state) :runner)
                   (lose state :runner :click 1)))
    :leave-play (req (gain state :runner :click-per-turn 1)
                     (when (= (:active-player @state) :runner)
                       (gain state :runner :click 1)))
    :trash-effect {:when-inactive true
                   :req (req (:access @state))
                   :msg "add it to the Runner's score area as an agenda worth 2 agenda points"
                   :delayed-completion true
                   :effect (req (as-agenda state :runner eid card 2))}}

   "Warden Fatuma"
   (let [new-sub {:label "[Warden Fatuma] Force the Runner to lose 1 [Click], if able"}]

     (letfn [(all-rezzed-bios [state]
               (filter #(and (ice? %)
                             (has-subtype? % "Bioroid")
                             (rezzed? %))
                       (all-installed state :corp)))

             (remove-one [cid state ice]
               (remove-extra-subs state :corp cid ice))

             (add-one [cid state ice]
               (add-extra-sub state :corp cid ice 0 new-sub))

             (update-all [state func] (doseq [i (all-rezzed-bios state)] (func state i)))
             ]
       {:effect (req (system-msg
                       state :corp
                       "uses Warden Fatuma to add \"[Subroutine] The Runner loses [Click], if able\" before all other subroutines")
                  (update-all state (partial add-one (:cid card))))
        :leave-play (req (system-msg state :corp "loses Warden Fatuma additional subroutines")
                      (update-all state (partial remove-one (:cid card))))
        :sub-effect {:msg "force the Runner to lose 1 [Click], if able"
                     :effect (req (lose state :runner :click 1))}
        :events {:rez {:req (req (and (ice? target) (has-subtype? target "Bioroid")))
                       :effect (req (add-one (:cid card) state (get-card state target)))}}}))

   "Watchdog"
   {:events {:pre-rez {:req (req (and (ice? target) (not (get-in @state [:per-turn (:cid card)]))))
                       :effect (effect (rez-cost-bonus (- (:tag runner))))}
             :rez {:req (req (and (ice? target) (not (get-in @state [:per-turn (:cid card)]))))
                              :effect (req (swap! state assoc-in [:per-turn (:cid card)] true))}}}

   "Whampoa Reclamation"
   {:abilities [{:label "Trash 1 card from HQ: Add 1 card from Archives to the bottom of R&D"
                 :once :per-turn
                 :req (req (and (pos? (count (:hand corp)))
                                (pos? (count (:discard corp)))))
                 :delayed-completion true
                 :effect (req (show-wait-prompt state :runner "Corp to use Whampoa Reclamation")
                              (when-completed (resolve-ability state side
                                                {:prompt "Choose a card in HQ to trash"
                                                 :choices {:req #(and (in-hand? %) (= (:side %) "Corp"))}
                                                 :effect (effect (trash target))}
                                               card nil)
                                              (continue-ability state side
                                                {:prompt "Select a card in Archives to add to the bottom of R&D"
                                                 :show-discard true
                                                 :choices {:req #(and (in-discard? %) (= (:side %) "Corp"))}
                                                 :msg (msg "trash 1 card from HQ and add "
                                                           (if (:seen target) (:title target) "a card") " from Archives to the bottom of R&D")
                                                 :effect (effect (move target :deck)
                                                                 (clear-wait-prompt :runner))}
                                               card nil)))}]}

   "Worlds Plaza"
   {:abilities [{:label "Install an asset on Worlds Plaza"
                 :req (req (< (count (:hosted card)) 3))
                 :cost [:click 1]
                 :prompt "Select an asset to install on Worlds Plaza"
                 :choices {:req #(and (is-type? % "Asset")
                                      (in-hand? %)
                                      (= (:side %) "Corp"))}
                 :msg (msg "host " (:title target))
                 :effect (req (corp-install state side target card) ;; install target onto card
                              (rez-cost-bonus state side -2) (rez state side (last (:hosted (get-card state card)))))}]}

   "Zaibatsu Loyalty"
   {:prevent {:expose [:all]}
    :derezzed-events
    {:pre-expose
     {:delayed-completion true
      :effect (req (let [etarget target]
                     (continue-ability state side
                       {:optional {:req (req (not (rezzed? card)))
                                   :player :corp
                                   :prompt (msg "The Runner is about to expose " (:title etarget) ". Rez Zaibatsu Loyalty?")
                                   :yes-ability {:effect (effect (rez card))}}}
                       card nil)))}}
    :abilities [{:msg "prevent 1 card from being exposed"
                 :cost [:credit 1]
                 :effect (effect (expose-prevent 1))}
                {:msg "prevent 1 card from being exposed"
                 :label "[Trash]: Prevent 1 card from being exposed"
                 :effect (effect (trash card {:cause :ability-cost})
                                 (expose-prevent 1))}]}

   "Zealous Judge"
   {:rez-req (req tagged)
    :abilities [{:delayed-completion true
                 :label "Give the Runner 1 tag"
                 :cost [:click 1 :credit 1]
                 :msg (msg "give the Runner 1 tag")
                 :effect (effect (tag-runner eid 1))}]}})
