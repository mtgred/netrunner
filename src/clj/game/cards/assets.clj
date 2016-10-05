(in-ns 'game.core)

(declare expose-prevent)

;;; Asset-specific helpers
(defn installed-access-trigger
  "Effect for triggering ambush on access.
  Ability is what happends upon access. If cost is specified Corp needs to pay that to trigger."
  ([cost ability]
   (let [ab (if (> cost 0) (assoc ability :cost [:credit cost]) ability)
         prompt (if (> cost 0)
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
  ([cost ability prompt] (assoc (installed-access-trigger cost ability prompt)
                           :advanceable :always)))

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
                              (when (zero? (get-in card [:counter :credit]))
                                (trash state :corp card)))}]
    {:effect (effect (add-counter card :credit counters))
     :derezzed-events {:runner-turn-ends corp-rez-toast}
     :events {:corp-turn-begins ability}
     :abilities [ability]}))

(defn as-trashed-agenda
  "Adds the given card to the given side's :scored area as an agenda worth n points after resolving the trash prompt."
  [state side card n]
  (or (move state :runner (assoc (deactivate state side card) :agendapoints n) :scored) ; if the runner did not trash the card on access, then this will work
      (move state :runner (assoc (deactivate state side card) :agendapoints n :zone [:discard]) :scored)) ; if the runner did trash it, then this will work
  (gain-agenda-point state side n))

;;; Card definitions
(declare in-server?)

(def cards-assets
  {"Adonis Campaign"
   (campaign 12 3)

   "Advanced Assembly Lines"
   {:effect (effect (gain :credit 3))
    :msg (msg "gain 3 [Credits]")
    :abilities [{:label "[Trash]: Install a non-agenda card from HQ"
                 :effect (effect (trash card) (corp-install target nil))
                 :msg (msg (corp-install-msg target))
                 :prompt "Choose a non-agenda card to install from HQ"
                 :priority true
                 :req (req (not (:run @state)))
                 :choices {:req #(and (not (is-type? % "Operation"))
                                      (not (is-type? % "Agenda"))
                                      (= (:zone %) [:hand])
                                      (= (:side %) "Corp"))}}]}

   "Aggressive Secretary"
   (advance-ambush 2 {:req (req (< 0 (:advance-counter (get-card state card) 0)))
                      :delayed-completion true
                      :effect
                      (req (let [agg (get-card state card)
                                 n (:advance-counter agg 0)
                                 ab (-> trash-program
                                        (assoc-in [:choices :max] n)
                                        (assoc :prompt (msg "Choose " n " program" (when (> n 1) "s") " to trash")
                                               :delayed-completion true
                                               :effect (effect (trash-cards eid targets nil))
                                               :msg (msg "trash " (join ", " (map :title targets)))))]
                             (continue-ability state side ab agg nil)))})

   "Alexa Belsky"
   {:abilities [{:label "[Trash]: Shuffle all cards in HQ into R&D"
                 :effect (req (trash state side card)
                              (if (pos? (:credit runner))
                                (do (show-wait-prompt state :corp "Runner to decide whether or not to prevent Alexa Belsky")
                                    (resolve-ability
                                      state side
                                      {:prompt "How many credits?"
                                       :choices :credit :player :runner
                                       :msg (msg "shuffle " (- (count (:hand corp)) (int (/ target 2))) " card"
                                                 (when-not (= 1 (- (count (:hand corp)) (int (/ target 2)))) "s")
                                                 " in HQ into R&D")
                                       :effect (req (if (pos? (int (/ target 2)))
                                                      (do (doseq [c (take (- (count (:hand corp)) (int (/ target 2)))
                                                                          (shuffle (:hand corp)))]
                                                            (move state :corp c :deck))
                                                          (shuffle! state :corp :deck)
                                                          (system-msg state :runner
                                                                      (str "pays " target " [Credits] to prevent "
                                                                           (int (/ target 2)) " random card"
                                                                           (when (> (int (/ target 2)) 1) "s")
                                                                           " in HQ from being shuffled into R&D"))
                                                          (clear-wait-prompt state :corp))
                                                      (do (shuffle-into-deck state :corp :hand)
                                                          (clear-wait-prompt state :corp))))} card nil))
                                (resolve-ability
                                  state side
                                  {:msg "shuffle all cards in HQ into R&D"
                                   :effect (effect (shuffle-into-deck :hand))} card nil)))}]}

   "Alix T4LB07"
   {:events {:corp-install {:effect (effect (add-counter card :power 1))}}
    :abilities [{:cost [:click 1] :label "Gain 2 [Credits] for each counter on Alix T4LB07"
                 :msg (msg "gain " (* 2 (get-in card [:counter :power] 0)) " [Credits]")
                 :effect (effect (gain :credit (* 2 (get-in card [:counter :power] 0)))
                                 (trash card))}]}

   "Allele Repression"
   {:advanceable :always
    :abilities [{:label "Swap 1 card in HQ and Archives for each advancement token"
                 :effect (effect (trash card))
                 :msg (msg "swap " (:advance-counter card 0) " cards in HQ and Archives")}]}

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

   "Blacklist"
   {:effect (effect (lock-zone (:cid card) :runner :discard))
    :leave-play (effect (release-zone (:cid card) :runner :discard))}

   "Brain-Taping Warehouse"
   {:events {:pre-rez
             {:req (req (and (ice? target) (has-subtype? target "Bioroid")))
              :effect (effect (rez-cost-bonus (- (:click runner))))}}}

   "Broadcast Square"
   {:abilities [{:label "Trace 3 - Avoid taking a bad publicity"
                 :trace {:base 3 :msg "avoid taking a bad publicity"
                         :effect (effect (lose :bad-publicity 1))}}]}

   "Capital Investors"
   {:abilities [{:cost [:click 1] :effect (effect (gain :credit 2)) :msg "gain 2 [Credits]"}]}

   "Cerebral Overwriter"
   (advance-ambush 3 {:req (req (< 0 (:advance-counter (get-card state card) 0)))
                      :msg (msg "do " (:advance-counter (get-card state card) 0) " brain damage")
                      :delayed-completion true
                      :effect (effect (damage eid :brain (:advance-counter (get-card state card) 0) {:card card}))})

   "Chairman Hiro"
   {:effect (effect (lose :runner :hand-size-modification 2))
    :leave-play (effect (gain :runner :hand-size-modification 2))
    :trash-effect {:when-unrezzed true
                   :req (req (:access @state))
                   :msg "add it to the Runner's score area as an agenda worth 2 agenda points"
                   :effect (effect (as-agenda :runner card 2))}}

   "C.I. Fund"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :flags {:corp-phase-12 (req (> (:credit corp) 0))}
    :abilities [{:label "Move up to 3 [Credit] from credit pool to C.I. Fund"
                 :prompt "Choose how many [Credit] to move" :once :per-turn
                 :choices {:number (req (min (:credit corp) 3))}
                 :effect (effect (lose :credit target)
                                 (add-counter card :credit target))
                 :msg (msg "move " target " [Credit] to C.I. Fund")}
                {:label "Take all credits from C.I. Fund"
                 :cost [:credit 2]
                 :msg (msg "trash it and gain " (get-in card [:counter :credit] 0) " [Credits]")
                 :effect (effect (gain :credit (get-in card [:counter :credit] 0))
                                 (trash card {:cause :ability-cost}))}]
    :events {:corp-turn-begins {:req (req (>= (get-in card [:counter :credit] 0) 6))
                                :effect (effect (add-counter card :credit 2)
                                                (system-msg (str "adds 2 [Credit] to C.I. Fund")))}}}

   "City Surveillance"
   {:events {:runner-turn-begins
             {:prompt "Pay 1 [Credits] or take 1 tag" :choices ["Pay 1 [Credits]" "Take 1 tag"]
              :player :runner :msg "make the Runner pay 1 [Credits] or take 1 tag"
              :effect (req (if-not (and (= target "Pay 1 [Credits]") (pay state side card :credit 1))
                             (do (tag-runner state side 1) (system-msg state side "takes 1 tag"))
                             (system-msg state side "pays 1 [Credits]")))}}}

   "Clone Suffrage Movement"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :flags {:corp-phase-12 (req (and (some #(is-type? % "Operation") (:discard corp))
                                     unprotected))}
    :abilities [{:label "Add 1 operation from Archives to HQ"
                 :prompt "Choose an operation in Archives to add to HQ" :show-discard true
                 :choices {:req #(and (is-type? % "Operation")
                                      (= (:zone %) [:discard]))}
                 :effect (effect (move target :hand)) :once :per-turn
                 :msg (msg "add " (if (:seen target) (:title target) "a facedown card") " to HQ")}]}

   "Commercial Bankers Group"
   (let [ability {:req (req unprotected)
                  :label "Gain 3 [Credits] (start of turn)"
                  :once :per-turn
                  :msg "gain 3 [Credits]"
                  :effect (effect (gain :credit 3))}]
     {:derezzed-events {:runner-turn-ends corp-rez-toast}
      :events {:corp-turn-begins ability}
      :abilities [ability]})

   "Constellation Protocol"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :flags {:corp-phase-12
            (req (let [tokens (filter #(pos? (:advance-counter % 0)) (all-installed state :corp))
                       advanceable (filter #(can-be-advanced? %) (all-installed state :corp))]
                   (when (and (not-empty tokens) (not-empty (clojure.set/difference (set advanceable) (set tokens))))
                     true)))}
    :once :per-turn
    :abilities [{:label "Move an advancement token between ICE"
                 :choices {:req #(and (ice? %) (:advance-counter %))}
                 :priority true
                 :effect (req (let [fr target]
                                (resolve-ability
                                  state side
                                  {:priority true
                                   :prompt "Move to where?"
                                   :choices {:req #(and (ice? %)
                                                        (not= (:cid fr) (:cid %))
                                                        (can-be-advanced? %))}
                                   :effect (effect (add-prop :corp target :advance-counter 1)
                                                   (add-prop :corp fr :advance-counter -1)
                                                   (system-msg (str "uses Constellation Protocol to move an advancement token from "
                                                                    (card-str state fr) " to " (card-str state target))))} card nil)
                                card nil))}]}

   "Contract Killer"
   {:advanceable :always
    :abilities [{:label "Trash a connection" :cost [:click 1] :req (req (>= (:advance-counter card) 2))
                 :choices {:req #(has-subtype? % "Connection")}
                 :msg (msg "to trash " (:title target)) :effect (effect (trash card) (trash target))}
                {:cost [:click 1] :req (req (>= (:advance-counter card) 2))
                 :delayed-completion true
                 :msg "do 2 meat damage"
                 :effect (effect (trash card) (damage eid :meat 2 {:card card}))}]}

   "Corporate Town"
   {:additional-cost [:forfeit]
    :derezzed-events {:runner-turn-ends corp-rez-toast}
    ; not-empty doesn't work for the next line, because it does not return literal true; it returns the collection.
    ; flags need exact equality of value to work.
    :flags {:corp-phase-12 (req (and (pos? (count (filter #(card-is? % :type "Resource") (all-installed state :runner))))
                                     (:rezzed card)))}
    :abilities [{:label "Trash a resource"
                 :prompt "Choose a resource to trash with Corporate Town"
                 :choices {:req #(is-type? % "Resource")}
                 :msg (msg "trash " (:title target))
                 :effect (effect (trash target {:unpreventable true}))}]}

   "Cybernetics Court"
   {:in-play [:hand-size-modification 4]}

   "Daily Business Show"
   {:events {:pre-corp-draw
             {:msg "draw additional cards"
              :once :per-turn
              :once-key :daily-business-show-draw-bonus
              :req (req (first-event state side :pre-corp-draw))
              :effect (req (let [dbs (count (filter #(and (= "06086" (:code %)) (rezzed? %)) (all-installed state :corp)))]
                             (draw-bonus state side dbs)))}
             :post-corp-draw
             {:once :per-turn
              :once-key :daily-business-show-put-bottom
              :delayed-completion true
              :effect (req (let [dbs (count (filter #(and (= "06086" (:code %)) (rezzed? %)) (all-installed state :corp)))
                                 drawn (get-in @state [:corp :register :most-recent-drawn])]
                             (continue-ability
                               state side
                               {:prompt (str "Choose " dbs " card" (when (> dbs 1) "s") " to add to the bottom of R&D")
                                :msg (msg "add " dbs " card" (when (> dbs 1) "s") " to the bottom of R&D")
                                :choices {:max dbs
                                          :req #(some (fn [c] (= (:cid c) (:cid %))) drawn)}
                                :effect (req (doseq [c targets] (move state side c :deck)))} card targets)))}}}

   "Dedicated Response Team"
   {:events {:successful-run-ends {:req (req tagged)
                                   :msg "do 2 meat damage"
                                   :delayed-completion true
                                   :effect (effect (damage eid :meat 2 {:card card}))}}}

   "Dedicated Server"
   {:recurring 2}

   "Director Haas"
   {:in-play [:click 1 :click-per-turn 1]
    :trash-effect {:when-unrezzed true
                   :req (req (:access @state))
                   :msg "add it to the Runner's score area as an agenda worth 2 agenda points"
                   :effect (effect (as-agenda :runner card 2))}}

   "Docklands Crackdown"
   {:abilities [{:cost [:click 2]
                 :msg "add 1 power counter"
                 :effect (effect (add-counter card :power 1))}]
    :events {:pre-install {:req (req (and (not (zero? (get-in  card [:counter :power])))
                                          (not (get-in @state [:per-turn (:cid card)]))))
                           :effect (effect (install-cost-bonus [:credit (get-in card [:counter :power])]))}
             :runner-install {:silent (req true)
                              :req (req (and (not (zero? (get-in card [:counter :power])))
                                             (not (get-in @state [:per-turn (:cid card)]))))
                              :msg (msg "increase the install cost of " (:title target) " by " (get-in card [:counter :power]) " [Credits]")
                              :effect (req (swap! state assoc-in [:per-turn (:cid card)] true))}}}

   "Early Premiere"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :flags {:corp-phase-12 (req (some #(and (can-be-advanced? %) (in-server? %)) (all-installed state :corp)))}
    :abilities [{:cost [:credit 1] :label "Place 1 advancement token on a card that can be advanced in a server"
                 :choices {:req #(and (can-be-advanced? %)
                                      (installed? %)
                                      (in-server? %))} ; should be *in* a server
                 :effect (effect (add-prop target :advance-counter 1 {:placed true})) :once :per-turn
                 :msg (msg "place 1 advancement token on " (card-str state target))}]}

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
                 :effect (effect (trash card) (trash target) (gain :bad-publicity 1))}]}

   "Elizas Toybox"
   {:abilities [{:cost [:click 3] :choices {:req #(not (:rezzed %))}
                 :label "Rez a card at no cost" :msg (msg "rez " (:title target) " at no cost")
                 :effect (effect (rez target {:ignore-cost :all-costs}))}]}

   "Encryption Protocol"
   {:events {:pre-trash {:req (req (installed? target))
                         :effect (effect (trash-cost-bonus 1))}}}

   "Eve Campaign"
   (campaign 16 2)

   "Executive Boot Camp"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :flags {:corp-phase-12 (req (some #(not (rezzed? %)) (all-installed state :corp)))}
    :abilities [{:choices {:req (complement rezzed?)}
                 :label "Rez a card, lowering the cost by 1 [Credits]"
                 :msg (msg "rez " (:title target))
                 :effect (effect (rez-cost-bonus -1)
                                 (rez target {:no-warning true})
                                 (update! (assoc card :ebc-rezzed (:cid target))))}
                {:prompt "Choose an asset to add to HQ"
                 :msg (msg "add " (:title target) " to HQ")
                 :activatemsg "searches R&D for an asset"
                 :choices (req (cancellable (filter #(is-type? % "Asset")
                                                    (:deck corp))
                                            :sorted))
                 :cost [:credit 1]
                 :label "Search R&D for an asset"
                 :effect (effect (trash card)
                                 (shuffle! :deck)
                                 (move target :hand))}]

    ; A card rezzed by Executive Bootcamp is ineligible to receive the turn-begins event for this turn.
    :suppress {:corp-turn-begins {:req (req (= (:cid target) (:ebc-rezzed (get-card state card))))}}
    :events {:corp-turn-ends {:req (req (:ebc-rezzed card))
                              :effect (effect (update! (dissoc card :ebc-rezzed)))}}}

   "Executive Search Firm"
   {:abilities [{:prompt "Choose an executive, sysop, or character to add to HQ"
                 :msg (msg "add " (:title target) " to HQ and shuffle R&D")
                 :activatemsg "searches R&D for an executive, sysop, or character"
                 :choices (req (cancellable (filter #(or (has-subtype? % "Executive")
                                                         (has-subtype? % "Sysop")
                                                         (has-subtype? % "Character"))
                                                    (:deck corp))
                                            :sorted))
                 :cost [:click 1]
                 :label "Search R&D for an executive, sysop, or character"
                 :effect (effect (move target :hand) (shuffle! :deck))}]}

   "Exposé"
   {:advanceable :always
    :abilities [{:label "Remove 1 bad publicity for each advancement token on Exposé"
                 :msg (msg "remove " (:advance-counter card) " bad publicity")
                 :effect (effect (trash card) (lose :bad-publicity (:advance-counter card)))}]}

   "Franchise City"
   {:events {:access {:req (req (is-type? target "Agenda"))
                      :msg "add it to their score area as an agenda worth 1 agenda point"
                      :effect (effect (as-agenda :corp card 1))}}}

   "Full Immersion RecStudio"
   {:can-host (req (and (or (is-type? target "Asset") (is-type? target "Agenda"))
                        (> 2 (count (:hosted card)))))
    :trash-cost-bonus (req (* 3 (count (:hosted card))))
    :abilities [{:label "Install an asset or agenda on Full Immersion RecStudio"
                 :req (req (< (count (:hosted card)) 2))
                 :cost [:click 1]
                 :prompt "Choose an asset or agenda to install"
                 :choices {:req #(and (or (is-type? % "Asset") (is-type? % "Agenda"))
                                      (in-hand? %)
                                      (= (:side %) "Corp"))}
                 :msg "install and host an asset or agenda"
                 :effect (req (corp-install state side target card))}]}

   "Genetics Pavilion"
   {:msg "prevent the Runner from drawing more than 2 cards during their turn"
    :effect (req (max-draw state :runner 2)
                 (when (= 0 (remaining-draws state :runner))
                   (prevent-draw state :runner)))
    :events {:runner-turn-begins {:effect (effect (max-draw :runner 2))}}
    :leave-play (req (swap! state update-in [:runner :register] dissoc :max-draw :cannot-draw))}

   "Ghost Branch"
   (advance-ambush 0 {:req (req (< 0 (:advance-counter (get-card state card) 0)))
                      :msg (msg "give the Runner " (:advance-counter (get-card state card) 0) " tag"
                                (when (> (:advance-counter (get-card state card) 0) 1) "s"))
                      :effect (effect (tag-runner :runner (:advance-counter (get-card state card) 0)))})

   "GRNDL Refinery"
   {:advanceable :always
    :abilities [{:label "Gain 4 [Credits] for each advancement token on GRNDL Refinery"
                 :cost [:click 1] :msg (msg "gain " (* 4 (get card :advance-counter 0)) " [Credits]")
                 :effect (effect (trash card) (gain :credit (* 4 (get card :advance-counter 0))))}]}

   "Haas Arcology AI"
   {:advanceable :while-unrezzed
    :abilities [{:label "Gain [Click]" :once :per-turn :msg "gain [Click]"
                 :cost [:click 1] :advance-counter-cost 1 :effect (effect (gain :click 2))}]}

   "Hostile Infrastructure"
   {:events {:runner-trash {:delayed-completion true
                            :req (req (some #(card-is? % :side :corp) targets))
                            :msg (msg (str "do " (count (filter #(card-is? % :side :corp) targets))
                                           " net damage"))
                            :effect (effect (damage eid :net (count (filter #(card-is? % :side :corp) targets))
                                                    {:card card}))}}
    :abilities [{:msg "do 1 net damage"
                 :delayed-completion true
                 :effect (effect (damage eid :net 1 {:card card}))}]}

   "Hyoubu Research Facility"
   {:events {:psi-bet-corp {:once :per-turn
                            :msg (msg "gain " target " [Credits]")
                            :effect (effect (gain :corp :credit target))}}}

   "Ibrahim Salem"
   (let [trash-ability (fn [type] {:req (req (seq (filter #(is-type? % type) (:hand runner))))
                                   :prompt (str "Choose a " type " to trash")
                                   :choices (req (filter #(is-type? % type) (:hand runner)))
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

   "Indian Union Stock Exchange"
   (let [iuse {:req (req (not= (:faction target) (:faction (:identity corp))))
               :msg "gain 1 [Credits]"
               :effect (effect (gain :credit 1))}]
     {:events {:play-operation iuse :rez iuse}})

   "Isabel McGuire"
   {:abilities [{:cost [:click 1] :label "Add an installed card to HQ"
                 :choices {:req installed?}
                 :msg (msg "move " (card-str state target) " to HQ")
                 :effect (effect (move target :hand))}]}

   "IT Department"
   {:abilities [{:counter-cost [:power 1]
                 :label "Add strength to a rezzed ICE"
                 :choices {:req #(and (ice? %) (:rezzed %))}
                 :req (req (< 0 (get-in card [:counter :power] 0)))
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
                                                       (inc (get-in card [:counter :power]))) target))}
               :runner-turn-ends it :corp-turn-ends it})}

   "Jackson Howard"
   {:abilities [{:cost [:click 1] :effect (effect (draw 2)) :msg "draw 2 cards"}
                {:label "Shuffle up to 3 cards from Archives into R&D"
                 :activatemsg "removes Jackson Howard from the game"
                 :effect (effect
                          (move card :rfg)
                          (resolve-ability
                           {:show-discard true
                            :choices {:max 3 :req #(and (:side % "Corp") (= (:zone %) [:discard]))}
                            :msg (msg "shuffle "
                                      (let [seen (filter :seen targets)]
                                        (str (join ", " (map :title seen))
                                             (let [n (count (filter #(not (:seen %)) targets))]
                                               (when (pos? n)
                                                 (str (when-not (empty? seen) " and ") n " card"
                                                      (when (> n 1) "s"))))))
                                      " into R&D")
                            :effect (req (doseq [c targets] (move state side c :deck))
                                         (shuffle! state side :deck))}
                           card nil))}]}

   "Jeeves Model Bioroids"
   {:abilities [{:label "Gain [Click]"
                 :msg "gain [Click]" :once :per-turn
                 :effect (effect (gain :click 1))}]}

   "Kala Ghoda Real TV"
   {:flags {:corp-phase-12 (req true)}
    :abilities [{:msg "look at the top card of the Runner's Stack"
                  :effect (effect (prompt! card (str "The top card of the Runner's Stack is "
                                                     (:title (first (:deck runner)))) ["OK"] {}))}
                {:label "[Trash]: Trash the top card of the Runner's Stack"
                 :msg "trash the top card of the Runner's Stack"
                 :effect (effect (mill :runner)
                                 (trash card {:cause :ability-cost}))}]}

   "Lakshmi Smartfabrics"
   {:events {:rez {:effect (effect (add-counter card :power 1))}}
    :abilities [{:req (req (seq (filter #(and (is-type? % "Agenda")
                                              (>= (get-in card [:counter :power] 0)
                                                  (:agendapoints %)))
                                        (:hand corp))))
                 :label "X power counters: Reveal an agenda worth X points from HQ"
                 :effect (req (let [c (get-in card [:counter :power])]
                                (resolve-ability
                                  state side
                                  {:prompt "Choose an agenda in HQ to reveal"
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
                 :effect (effect (shuffle! :deck)
                                 (move target :hand))}]}

   "Lily Lockwell"
   {:effect (effect (draw 3))
    :msg (msg "draw 3 cards")
    :abilities [{:label "Remove a tag to search R&D for an operation"
                 :prompt "Choose an operation to put on top of R&D"
                 :cost [:click 1]
                 :choices (req (let [ops (filter #(is-type? % "Operation") (:deck corp))]
                                 (if (empty? ops) ["No Operation in R&D"] ops)))
                 :req (req (pos? (get-in @state [:runner :tag])))
                 :effect (req (if (not= target "No Operation found")
                                (let [c (move state :corp target :play-area)]
                                  (shuffle! state :corp :deck)
                                  (move state :corp c :deck {:front true})
                                  (system-msg state side (str "uses Lily Lockwell to put " (:title c) " on top of R&D")))
                                (do (shuffle! state :corp :deck)
                                    (system-msg state side (str "uses Lily Lockwell, but did not find an Operation in R&D"))))
                              (lose state :runner :tag 1))}]}

   "Mark Yale"
   {:events {:agenda-counter-spent {:effect (effect (gain :credit 1))
                                    :msg "gain 1 [Credits]"}}
    :abilities [{:label "Trash to gain 2 [Credits]"
                 :msg "gain 2 [Credits]"
                 :effect (effect (gain :credit 2) (trash card))}
                {:label "Spend an agenda counter to gain 2 [Credits]"
                 :effect (req (resolve-ability
                                state side
                                {:prompt "Select an agenda with a counter"
                                 :choices {:req #(and (is-type? % "Agenda")
                                                      (pos? (get-in % [:counter :agenda] 0)))}
                                 :effect (req (add-counter state side target :agenda -1)
                                              (gain state :corp :credit 2)
                                              (trigger-event state side :agenda-counter-spent card))
                                 :msg (msg "spend an agenda token on " (:title target) " and gain 2 [Credits]")}
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

   "Melange Mining Corp."
   {:abilities [{:cost [:click 3] :effect (effect (gain :credit 7)) :msg "gain 7 [Credits]"}]}

   "Mental Health Clinic"
   (let [ability {:msg "gain 1 [Credits]"
                  :label "Gain 1 [Credits] (start of turn)"
                  :once :per-turn
                  :effect (effect (gain :credit 1))}]
     {:effect (effect (gain :runner :hand-size-modification 1))
      :leave-play (effect (lose :runner :hand-size-modification 1))
      :derezzed-events {:runner-turn-ends corp-rez-toast}
      :events {:corp-turn-begins ability}
      :abilities [ability]})

   "Mumba Temple"
   {:recurring 2}

   "Mumbad City Hall"
   {:abilities [{:label "Search R&D for an Alliance card"
                 :cost [:click 1]
                 :prompt "Choose an Alliance card to play or install"
                 :choices (req (cancellable (filter #(and (has-subtype? % "Alliance")
                                                          (if (is-type? % "Operation")
                                                            (<= (:cost %) (:credit corp)) true)) (:deck corp)) :sorted))
                 :msg (msg "reveal " (:title target) " from R&D and "
                           (if (= (:type target) "Operation") "play " "install ") " it")
                 :effect (req (shuffle! state side :deck)
                              (if (= (:type target) "Operation")
                                (play-instant state side target)
                                (corp-install state side target nil nil)))}]}

   "Mumbad Construction Co."
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :events {:corp-turn-begins {:effect (effect (add-prop card :advance-counter 1 {:placed true}))}}
    :abilities [{:cost [:credit 2]
                 :req (req (and (> (get card :advance-counter 0) 0)
                                (some #(rezzed? %) (all-installed state :corp))))
                 :label "Move an advancement token to a faceup card"
                 :prompt "Choose a faceup card"
                 :choices {:req #(rezzed? %)}
                 :msg (msg "move an advancement token to " (card-str state target))
                 :effect (effect (add-prop card :advance-counter -1 {:placed true})
                                 (add-prop target :advance-counter 1 {:placed true}))}]}

   "Museum of History"
   {:flags {:corp-phase-12 (req (pos? (count (get-in @state [:corp :discard]))))}
    :abilities [{:label "Shuffle cards in Archives into R&D"
                 :prompt (msg (let [mus (count (filter #(and (= "10019" (:code %)) (rezzed? %)) (all-installed state :corp)))]
                                (str "Choose " (if (< 1 mus) (str mus " cards") "a card")
                                     " in Archives to shuffle into R&D")))
                 :choices {:req #(and (card-is? % :side :corp) (= (:zone %) [:discard]))
                           :max (req (count (filter #(and (= "10019" (:code %)) (rezzed? %)) (all-installed state :corp))))}
                 :show-discard true
                 :priority 1
                 :once :per-turn
                 :once-key :museum-of-history
                 :msg (msg "shuffle "
                           (let [seen (filter :seen targets)]
                             (str (join ", " (map :title seen))
                                  (let [n (count (filter #(not (:seen %)) targets))]
                                    (when (pos? n)
                                      (str (when-not (empty? seen) " and ") n " card"
                                           (when (> n 1) "s"))))))
                           " into R&D")
                 :effect (req (doseq [c targets] (move state side c :deck))
                              (shuffle! state side :deck))}]}

   "Net Police"
   {:recurring (effect (set-prop card :rec-counter (:link runner)))
    :effect (effect (set-prop card :rec-counter (:link runner)))}

   "News Team"
   {:access {:msg (msg "force the Runner take 2 tags or add it to their score area as an agenda worth -1 agenda point")
             :delayed-completion true
             :effect (effect (continue-ability
                               {:player :runner
                                :prompt "Take 2 tags or add News Team to your score area as an agenda worth -1 agenda point?"
                                :choices ["Take 2 tags" "Add News Team to score area"]
                                :effect (req (if (= target "Add News Team to score area")
                                               (do (as-trashed-agenda state :runner card -1)
                                                   (system-msg state :runner (str "adds News Team to their score area as an agenda worth -1 agenda point")))
                                               (do (tag-runner state :runner 2)
                                                   (system-msg state :runner (str "takes 2 tags from News Team")))))}
                               card targets))}}

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
                 :choices {:req #(and (:side % "Corp") (installed? %))}
                 :msg (msg "place 1 advancement token on " (card-str state target))
                 :effect (req (add-prop state :corp target :advance-counter 1 {:placed true})
                              (let [tgtcid (:cid target)]
                                (register-turn-flag! state side
                                  target :can-score
                                  (fn [state side card]
                                    (if (and (= (:cid card) tgtcid)
                                             (>= (:advance-counter card) (or (:current-cost card) (:advancementcost card))))
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

   "Plan B"
   (advance-ambush
    0
    {:req (req (pos? (:advance-counter (get-card state card) 0)))
     :effect
     (effect (resolve-ability
              {:prompt "Choose an Agenda in HQ to score"
               :choices {:req #(and (is-type? % "Agenda")
                                    (<= (:advancementcost %) (:advance-counter (get-card state card) 0))
                                    (in-hand? %))}
               :msg (msg "score " (:title target))
               :effect (effect (score (assoc target :advance-counter
                                             (:advancementcost target))))}
              card nil))}
    "Score an Agenda from HQ?")

   "Political Dealings"
   (let [pdhelper (fn pd [agendas n]
                    {:optional
                     {:prompt (msg "Reveal and install " (:title (nth agendas n)) "?")
                      :yes-ability {:delayed-completion true
                                    :msg (msg "reveal " (:title (nth agendas n)))
                                    :effect (req (when-completed
                                                   (corp-install state side (nth agendas n) nil
                                                                 {:install-state
                                                                  (:install-state (card-def (nth agendas n))
                                                                    :rezzed-no-cost)})
                                                   (if (< (inc n) (count agendas))
                                                     (continue-ability state side (pd agendas (inc n)) card nil)
                                                     (effect-completed state side eid))))}
                      :no-ability {:delayed-completion true
                                   :effect (req (if (< (inc n) (count agendas))
                                                  (continue-ability state side (pd agendas (inc n)) card nil)
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
                              (when (= (get-in card [:counter :credit]) 0) (trash state :corp card)))}]}

   "Project Junebug"
   (advance-ambush 1 {:req (req (< 0 (:advance-counter (get-card state card) 0)))
                      :msg (msg "do " (* 2 (:advance-counter (get-card state card) 0)) " net damage")
                      :delayed-completion true
                      :effect (effect (damage eid :net (* 2 (:advance-counter (get-card state card) 0))
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
             {:effect (req (add-counter state side card :power -1)
                           (when (<= (get-in card [:counter :power]) 1)
                             (system-msg state :corp "uses Public Support to add it to their score area as an agenda worth 1 agenda point")
                             (as-agenda state :corp (dissoc card :counter) 1)))} }}

   "Reality Threedee"
   (let [ability {:effect (req (gain state side :credit (if tagged 2 1)))
                  :label "Gain credits (start of turn)"
                  :once :per-turn
                  :msg (msg (if tagged "gain 2 [Credits]" "gain 1 [Credits]"))}]
   {:effect (effect (gain :bad-publicity 1) (system-msg "takes 1 bad publicity"))
    :derezzed-events {:runner-turn-ends corp-rez-toast}
    :events {:corp-turn-begins ability}
    :abilities [ability]})

   "Reversed Accounts"
   {:advanceable :always
    :abilities [{:cost [:click 1]
                 :label "Force the Runner to lose 4 [Credits] per advancement"
                 :msg (msg "force the Runner to lose " (min (* 4 (get card :advance-counter 0)) (:credit runner)) " [Credits]")
                 :effect (effect (lose :runner :credit (* 4 (get card :advance-counter 0))) (trash card))}]}

   "Rex Campaign"
   (let [ability {:once :per-turn
                  :label "Remove 1 counter (start of turn)"
                  :effect (req (add-counter state side card :power -1)
                               (when (<= (get-in card [:counter :power]) 1)
                                 (trash state side card)
                                 (resolve-ability state side
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
   {:events {:runner-trash {:req (req (and (= (:side target) "Corp") (> (:click runner) 0)))
                            :msg "force the runner to lose 1 [Click]" :effect (effect (lose :runner :click 1))}}}

   "Ronin"
   {:advanceable :always
    :abilities [{:cost [:click 1] :req (req (>= (:advance-counter card) 4))
                 :msg "do 3 net damage"
                 :delayed-completion true
                 :effect (effect (trash card) (damage eid :net 3 {:card card}))}]}

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
                 :effect (effect (gain :credit target) (trash card))}]}

   "Security Subcontract"
   {:abilities [{:choices {:req #(and (ice? %) (rezzed? %))} :cost [:click 1]
                 :msg (msg "trash " (:title target) " to gain 4 [Credits]")
                 :label "Trash a rezzed ICE to gain 4 [Credits]"
                 :effect (effect (trash target) (gain :credit 4))}]}

   "Sensie Actors Union"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :flags {:corp-phase-12 (req unprotected)}
    :abilities [{:label "Draw 3 cards and add 1 card in HQ to the bottom of R&D"
                 :once :per-turn
                 :msg "draw 3 cards"
                 :effect (effect (draw 3)
                                 (resolve-ability
                                   {:prompt "Choose a card in HQ to add to the bottom of R&D"
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
                 :effect (effect (shuffle! :deck) (move target :deck)
                                 (trash card {:cause :ability-cost}))}
                {:label "[Trash]: Search Archives for an agenda"
                 :prompt "Choose an agenda to add to the bottom of R&D"
                 :msg (msg "reveal " (:title target) " from Archives and add it to the bottom of R&D")
                 :choices (req (cancellable (filter #(is-type? % "Agenda") (:discard corp)) :sorted))
                 :effect (effect (move target :deck) (trash card {:cause :ability-cost}))}]}

   "Shattered Remains"
   (advance-ambush 1 {:delayed-completion true
                      :effect (req (let [shat (get-card state card)]
                                     (when (< 0 (:advance-counter shat 0))
                                       (continue-ability
                                         state side
                                         (-> trash-hardware
                                             (assoc-in [:choices :max] (:advance-counter shat))
                                             (assoc :prompt (msg "Choose " (:advance-counter shat) " pieces of hardware to trash")
                                                    :effect (effect (trash-cards targets))
                                                    :msg (msg "trash " (join ", " (map :title targets)))))
                                        shat nil))))})

   "Shi.Kyū"
   {:access
    {:delayed-completion true
     :req (req (not= (first (:zone card)) :deck))
     :effect (effect (show-wait-prompt :runner "Corp to use Shi.Kyū")
                     (continue-ability
                       {:optional
                        {:prompt "Pay [Credits] to use Shi.Kyū?"
                         :yes-ability {:prompt "How many [Credits] for Shi.Kyū?" :choices :credit
                                       :msg (msg "attempt to do " target " net damage")
                                       :delayed-completion true
                                       :effect (effect (clear-wait-prompt :runner)
                                                       (continue-ability
                                                         {:player :runner
                                                          :prompt (str "Take " target " net damage or add Shi.Kyū to your score area as an agenda worth -1 agenda point?")
                                                          :choices [(str "Take " target " net damage") "Add Shi.Kyū to score area"]
                                                          :delayed-completion true
                                                          :effect (let [dmg target]
                                                                    (req (if (= target "Add Shi.Kyū to score area")
                                                                           (do (as-trashed-agenda state :runner card -1)
                                                                               (system-msg state :runner (str "adds Shi.Kyū to their score area as as an agenda worth -1 agenda point"))
                                                                               (effect-completed state side eid))
                                                                           (do (damage state :corp eid :net dmg {:card card})
                                                                               (system-msg state :runner (str "takes " dmg " net damage from Shi.Kyū"))))))}
                                                        card targets))}
                         :no-ability {:effect (effect (clear-wait-prompt :runner))}}}
                      card targets))}}

   "Shock!"
   {:access {:msg "do 1 net damage"
             :delayed-completion true
             :effect (effect (damage eid :net 1 {:card card}))}}

   "Snare!"
   {:access {:req (req (not= (first (:zone card)) :discard))
             :delayed-completion true
             :effect (effect (show-wait-prompt :runner "Corp to use Snare!")
                             (continue-ability
                               {:optional
                                {:prompt "Pay 4 [Credits] to use Snare! ability?"
                                 :end-effect (effect (clear-wait-prompt :runner))
                                 :yes-ability {:cost [:credit 4]
                                               :msg "do 3 net damage and give the Runner 1 tag"
                                               :delayed-completion true
                                               :effect (effect (damage eid :net 3 {:card card})
                                                               (tag-runner :runner 1))}}}
                               card nil))}}

   "Space Camp"
   {:access {:delayed-completion true
             :effect (effect (show-wait-prompt :runner "Corp to use Space Camp")
                             (continue-ability
                               {:optional
                                {:prompt "Place 1 advancement token?"
                                 :end-effect (effect (clear-wait-prompt :runner))
                                 :yes-ability {:msg (msg "place 1 advancement token on " (card-str state target))
                                               :choices {:req can-be-advanced?}
                                               :effect (effect (add-prop target :advance-counter 1 {:placed true}))}}}
                               card nil))}}

   "Sundew"
   {:events {:runner-spent-click {:once :per-turn
                                  :msg (req (when (not this-server) "gain 2 [Credits]"))
                                  :effect (req (when (not this-server)
                                                 (gain state :corp :credit 2)))}}}

   "Team Sponsorship"
   {:events {:agenda-scored {:label "Install a card from Archives or HQ"
                             :prompt "Choose a card from Archives or HQ to install"
                             :show-discard true
                             :interactive (req true)
                             :delayed-completion true
                             :choices {:req #(and (not (is-type? % "Operation"))
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

   "Tenma Line"
   {:abilities [{:label "Swap 2 pieces of installed ICE"
                 :cost [:click 1]
                 :prompt "Select two pieces of ICE to swap positions"
                 :choices {:req #(and (installed? %) (ice? %)) :max 2}
                 :effect (req (when (= (count targets) 2)
                                (swap-ice state side (first targets) (second targets))))
                 :msg "swap the positions of two ICE"}]}

   "Test Ground"
   {:advanceable :always
    :abilities [{:label "Derez 1 card for each advancement token"
                 :msg (msg "derez " (:advance-counter card)) :effect (effect (trash card))}]}

   "The Board"
   {:effect (effect (lose :runner :agenda-point (count (:scored runner))))
    :leave-play (effect (gain :runner :agenda-point (count (:scored runner))))
    :trash-effect {:when-unrezzed true
                   :req (req (:access @state))
                   :msg "add it to the Runner's score area as an agenda worth 2 agenda points"
                   :effect (effect (as-agenda :runner card 2))}
    :events {:agenda-stolen {:effect (effect (lose :runner :agenda-point 1))}
             :card-moved {:req (req (or (some #{:scored} (:zone (first targets)))
                                        (some #{:scored} (:zone (second targets)))))
                          :effect (effect ((if (some #{:scored} (:zone (first targets))) gain lose) :runner :agenda-point 1))}}}

   "The News Now Hour"
   {:events {:runner-turn-begins {:effect (req (prevent-current state side))}}
    :effect (req (prevent-current state side))
    :leave-play (req (swap! state assoc-in [:runner :register :cannot-play-current] false))}

   "The Root"
   {:recurring 3}

   "Thomas Haas"
   {:advanceable :always
    :abilities [{:label "Gain credits" :msg (msg "gain " (* 2 (get card :advance-counter 0)) " [Credits]")
                 :effect (effect (gain :credit (* 2 (get card :advance-counter 0))) (trash card))}]}

   "Toshiyuki Sakai"
   (advance-ambush 0
    {:effect (effect (resolve-ability
                       {:prompt "Choose an asset or agenda in HQ"
                        :choices {:req #(and (or (is-type? % "Agenda")
                                                 (is-type? % "Asset"))
                                             (in-hand? %))}
                        :msg "swap it for an asset or agenda from HQ"
                        :effect (req (let [tidx (ice-index state card)
                                           srvcont (get-in @state (cons :corp (:zone card)))
                                           c (:advance-counter (get-card state card) 0)
                                           newcard (assoc target :zone (:zone card) :advance-counter c)
                                           newcont (apply conj (subvec srvcont 0 tidx) newcard (subvec srvcont tidx))]
                                       (resolve-ability state side
                                         {:effect (req (swap! state assoc-in (cons :corp (:zone card)) newcont)
                                                       (swap! state update-in [:corp :hand]
                                                         (fn [coll] (remove-once #(not= (:cid %) (:cid newcard)) coll)))
                                                       (trigger-event state side :corp-install newcard)
                                                       (move state side card :hand))} card nil)
                                       (resolve-prompt state :runner {:choice "No"})
                                       ; gets rid of prompt to trash Toshiyuki since it's back in HQ now
                                       (resolve-ability state :runner
                                         {:optional
                                          {:prompt "Access the newly installed card?" :player :runner
                                           :priority true
                                           :yes-ability {:effect (effect (handle-access [newcard]))}}} card nil)))}
                      card nil))}
    "Swap Toshiyuki Sakai with an agenda or asset from HQ?")

   "Turtlebacks"
   {:events {:server-created {:msg "gain 1 [Credits]" :effect (effect (gain :credit 1))}}}

   "Victoria Jenkins"
   {:effect (req (lose state :runner :click-per-turn 1)
                 (when (= (:active-player @state) :runner)
                   (lose state :runner :click 1)))
    :leave-play (req (gain state :runner :click-per-turn 1)
                     (when (= (:active-player @state) :runner)
                       (gain state :runner :click 1)))
    :trash-effect {:when-unrezzed true
                   :req (req (:access @state))
                   :msg "add it to the Runner's score area as an agenda worth 2 agenda points"
                   :effect (effect (as-agenda :runner card 2))}}

   "Watchdog"
   {:events {:pre-rez {:req (req (and (ice? target) (not (get-in @state [:per-turn (:cid card)]))))
                       :effect (effect (rez-cost-bonus (- (:tag runner))))}
             :rez {:req (req (and (ice? target) (not (get-in @state [:per-turn (:cid card)]))))
                              :effect (req (swap! state assoc-in [:per-turn (:cid card)] true))}}}

   "Worlds Plaza"
   {:abilities [{:label "Install an asset on Worlds Plaza"
                 :req (req (< (count (:hosted card)) 3))
                 :cost [:click 1]
                 :prompt "Choose an asset to install on Worlds Plaza"
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
    :abilities [{:label "Give the Runner 1 tag"
                :cost [:click 1 :credit 1]
                :msg (msg "give the Runner 1 tag")
                :effect (effect (tag-runner 1))}]}})
