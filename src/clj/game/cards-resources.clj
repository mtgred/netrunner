(in-ns 'game.core)

(def cards-resources
  {"Access to Globalsec"
   {:in-play [:link 1]}

   "Activist Support"
   {:events
    {:corp-turn-begins {:req (req (not tagged))
                        :msg "take 1 tag"
                        :effect (effect (tag-runner :runner 1))}
     :runner-turn-begins {:req (req (zero? (:bad-publicity corp)))
                          :msg "give the Corp 1 bad publicity"
                          :effect (effect (gain :corp :bad-publicity 1))}}}

   "Adjusted Chronotype"
   {:events {:runner-loss {:req (req (and (some #{:click} target)
                                          (let [click-losses (filter #(= :click %) (mapcat first (turn-events state side :runner-loss)))]
                                            (or (empty? click-losses)
                                                (and (= (count click-losses) 1)
                                                     (persistent-flag? state side card :triggers-twice))))))
                           :msg "gain [Click]" :effect (effect (gain :runner :click 1))}}}

   "Aesops Pawnshop"
   {:flags {:runner-phase-12 (req (>= 2 (count (all-installed state :runner))))}
    :abilities [{:msg (msg "trash " (:title target) " and gain 3 [Credits]")
                 :choices {:req #(and (= (:side %) "Runner") (:installed %))}
                 :effect (effect (gain :credit 3) (trash target {:unpreventable true}))}]}

   "Always Be Running"
   {:abilities [{:once :per-turn
                 :cost [:click 2]
                 :msg (msg "break 1 subroutine")}]}

   "All-nighter"
   {:abilities [{:cost [:click 1] :effect (effect (trash card {:cause :ability-cost}) (gain :click 2))
                  :msg "gain [Click][Click]"}]}

   "Angel Arena"
   {:prompt "How many power counters?" :choices :credit :msg (msg "add " target " power counters")
    :effect (effect (set-prop card :counter target))
    :abilities [{:counter-cost 1 :msg "look at the top card of Stack"
                 :effect (req (when (zero? (:counter card)) (trash state :runner card {:unpreventable true})))
                 :optional {:prompt (msg "Add " (:title (first (:deck runner))) " to bottom of Stack?")
                            :yes-ability {:msg "add the top card of Stack to the bottom"
                                          :effect (req (move state side (first (:deck runner)) :deck))}}}]}

   "Armitage Codebusting"
   {:data {:counter 12
           :count-type "Credit"}
    :abilities [{:cost [:click 1] :counter-cost 2 :msg "gain 2 [Credits]"
                 :effect (req (gain state :runner :credit 2)
                              (when (zero? (:counter card)) (trash state :runner card {:unpreventable true})))}]}

   "Artist Colony"
   {:abilities [{:prompt "Choose a card to install"
                 :msg (msg "install " (:title target))
                 :cost [:forfeit]
                 :choices (req (cancellable (filter #(not (is-type? % "Event")) (:deck runner)) :sorted))
                 :effect (effect (runner-install target) (shuffle! :deck))}]}

   "Bank Job"
   {:data {:counter 8
           :counter-type "Credit"}
    :events {:no-action {:effect (req (toast state :runner "Click Bank Job to take credits from it instead of accessing" "info"))
                         :req (req (and (is-remote? (:server run)) (not current-ice)))}}
    :abilities [{:req (req (and (:run @state) (= (:position run) 0)))
                 :label "Take any number of [Credits] on Bank Job"
                 :prompt "How many [Credits]?" :choices :counter :msg (msg "gain " target " [Credits]")
                 :effect (req (gain state side :credit target)
                              (register-successful-run state side (:server run))
                              (swap! state update-in [:runner :prompt] rest)
                              (handle-end-run state side)
                              (when (= target (:counter card))
                                (trash state :runner card {:unpreventable true})))}]}

   "Beach Party"
   {:in-play [:hand-size-modification 5]
    :events {:runner-turn-begins {:msg "lose [Click]" :effect (effect (lose :click 1))}}}

   "Borrowed Satellite"
   {:in-play [:hand-size-modification 1 :link 1]}

   "Chatterjee University"
   {:abilities [{:cost [:click 1] :label "Place 1 power counter"
                 :msg "place 1 power counter on it"
                 :effect (effect (add-prop card :counter 1))}
                {:cost [:click 1] :label "Install a program from your Grip"
                 :prompt "Choose a program to install from your Grip"
                 :choices {:req #(and (is-type? % "Program") (in-hand? %))}
                 :msg (msg "install " (:title target))
                 :effect (req (install-cost-bonus state side [:credit (* -1 (:counter card))])
                              (runner-install state side target)
                              (when (pos? (:counter card))
                                (add-prop state side card :counter -1)))}]}

   "Chrome Parlor"
   {:events
    {:pre-damage {:req (req (has-subtype? (second targets) "Cybernetic"))
                  :effect (effect (damage-prevent target Integer/MAX_VALUE))}}}

   "Compromised Employee"
   {:recurring 1
    :events {:rez {:req (req (ice? target))
                   :msg "gain 1 [Credits]"
                   :effect (effect (gain :runner :credit 1))}}}

   "Crash Space"
   {:prevent {:damage [:meat]}
    :recurring 2
    :abilities [{:label "Trash to prevent up to 3 meat damage"
                 :msg "prevent up to 3 meat damage"
                 :effect (effect (trash card {:cause :ability-cost}) (damage-prevent :meat 3))}]}

   "Daily Casts"
   (let [ability {:once :per-turn
                  :label "Take 2 [Credits] (start of turn)"
                  :msg "gain 2 [Credits]"
                  :req (req (:runner-phase-12 @state))
                  :counter-cost 2
                  :effect (req (gain state :runner :credit 2)
                               (when (zero? (:counter card))
                                 (trash state :runner card {:unpreventable true})))}]
   {:data {:counter 8
           :counter-type "Credit"}
    :flags {:drip-economy true}
    :abilities [ability]
    :events {:runner-turn-begins ability}})

   "Data Dealer"
   {:abilities [{:cost [:click 1 :forfeit] :effect (effect (gain :credit 9))
                 :msg (msg "gain 9 [Credits]")}]}

   "Data Folding"
   (let [ability {:label "Gain 1 [Credits] (start of turn)"
                  :msg "gain 1 [Credits]"
                  :once :per-turn
                  :req (req (and (>= (:memory runner) 2) (:runner-phase-12 @state)))
                  :effect (effect (gain :credit 1))}]
    {:flags {:drip-economy true}
    :abilities [ability]
    :events {:runner-turn-begins ability}})

   "Data Leak Reversal"
   {:req (req (some #{:hq :rd :archives} (:successful-run runner-reg)))
    :abilities [{:req (req tagged) :cost [:click 1] :effect (effect (mill :corp))
                 :msg "force the Corp to trash the top card of R&D"}]}

   "DDoS"
   {:abilities [{:msg "prevent the corp from rezzing the outermost piece of ice during a run on any server this turn"
                 :effect (effect
                           (register-turn-flag!
                             card :can-rez
                             (fn [state side card]
                               (if (and (ice? card)
                                        (= (count (get-in @state [:run :ices])) (get-in @state [:run :position])))
                                 ((constantly false) (toast state :corp "Cannot rez any outermost ICE due to DDoS." "warning"))
                                 true)))
                           (trash card {:cause :ability-cost}))}]}

   "Decoy"
   {:prevent {:tag [:all]}
    :abilities [{:msg "avoid 1 tag" :effect (effect (tag-prevent 1) (trash card {:cause :ability-cost}))}]}

   "Dr. Lovegood"
   {:flags {:runner-phase-12 (req (>= 2 (count (all-installed state :runner))))}
    :abilities [{:prompt "Choose an installed card to make its text box blank for the remainder of the turn" :once :per-turn
                 :choices {:req #(:installed %)}
                 :msg (msg "make the text box of " (:title target) " blank for the remainder of the turn")
                 :effect (req (let [c target]
                                (update! state side (dissoc target :events :abilities))
                                (deactivate state side target)
                                (register-events state side
                                                 {:runner-turn-ends
                                                  {:effect (effect (card-init (get-card state c))
                                                                   (unregister-events card))}} card)))}]
    :events {:runner-turn-ends nil}}

   "Drug Dealer"
   {:flags {:runner-phase-12 (req (some #(card-flag? % :drip-economy true) (all-installed state :runner)))}
    :abilities [{:label "Lose 1 [Credits] (start of turn)"
                 :msg "lose 1 [Credits]"
                 :req (req (:runner-phase-12 @state))
                 :once :per-turn
                 :effect (effect (lose :credit 1))}]
    :events {:corp-turn-begins {:msg "draw 1 card" :effect (effect (draw :runner 1))}
             :runner-turn-begins {:msg "lose 1 [Credits]"
                                  :once :per-turn
                                  :effect (effect (lose :credit 1))}}}

   "Duggars"
   {:abilities [{:cost [:click 4] :effect (effect (draw 10)) :msg "draw 10 cards"}]}

   "Earthrise Hotel"
   (let [ability {:msg "draw 2 cards"
                  :once :per-turn
                  :counter-cost 1
                  :effect (req (draw state :runner 2)
                               (when (zero? (:counter card))
                                 (trash state :runner card {:unpreventable true})))}]
   {:flags {:runner-turn-draw true
            :runner-phase-12 (req (< 1 (count (filter #(card-flag? % :runner-turn-draw true)
                                                      (cons (get-in @state [:runner :identity])
                                                            (all-installed state :runner))))))}
    :data [:counter 3]
    :events {:runner-turn-begins ability}
    :abilities [ability]})

   "Eden Shard"
   {:abilities [{:effect (effect (trash card {:cause :ability-cost}) (draw :corp 2))
                 :msg "force the Corp to draw 2 cards"}]
    :install-cost-bonus (req (if (and run (= (:server run) [:rd]) (zero? (:position run)))
                               [:credit -7 :click -1] nil))
    :effect (req (when (and run (= (:server run) [:rd]) (zero? (:position run)))
                   (register-successful-run state side (:server run))
                   (swap! state update-in [:runner :prompt] rest)
                   (handle-end-run state side)))}

   "Enhanced Vision"
   {:events {:successful-run {:msg (msg "force the Corp to reveal " (:title (first (shuffle (:hand corp)))))
                              :req (req (or (first-event state side :successful-run)
                                            (and (second-event state side :successful-run)
                                                 (persistent-flag? state side card :triggers-twice))))}}}

   "Fall Guy"
   {:prevent {:trash [:resource]}
    :abilities [{:label "Prevent a resource from being trashed"
                 :effect (effect (trash-prevent :resource 1) (trash card {:unpreventable true :cause :ability-cost}))}
                {:effect (effect (trash card {:cause :ability-cost}) (gain :credit 2)) :msg "gain 2 [Credits]"}]}

   "Fan Site"
   {:events {:agenda-scored {:msg "add it to their score area as an agenda worth 0 agenda points"
                             :effect (effect (as-agenda :runner card 0))}}}

   "Fester"
   {:events {:purge {:msg "force the Corp to lose 2 [Credits] if able"
                     :effect (effect (pay :corp card :credit 2))}}}

   "Film Critic"
   {:abilities [{:req (req (and (empty? (:hosted card))
                                (is-type? (:card (first (get-in @state [side :prompt]))) "Agenda")))
                 :label "Host an agenda being accessed"
                 :effect (req (when-let [agenda (:card (first (get-in @state [side :prompt])))]
                                (host state side card (move state side agenda :play-area))
                                (swap! state update-in [side :prompt] rest)
                                (when-let [run (:run @state)]
                                  (when (and (:ended run) (empty? (get-in @state [:runner :prompt])) )
                                    (handle-end-run state :runner)))))
                 :msg (msg "host " (:title (:card (first (get-in @state [side :prompt])))) " instead of accessing it")}
                {:cost [:click 2] :label "Add hosted agenda to your score area"
                 :req (req (not (empty? (:hosted card))))
                 :effect (req (let [c (move state :runner (first (:hosted card)) :scored)]
                                (gain-agenda-point state :runner (get-agenda-points state :runner c))))
                 :msg (msg (let [c (first (:hosted card))]
                             (str "add " (:title c) " to their score area and gain " (get-agenda-points state :runner c)
                                  " agenda point" (when (> (get-agenda-points state :runner c) 1) "s"))))}]}

   "Gang Sign"
   {:events {:agenda-scored {:effect (req (system-msg state :runner (str "can access cards in HQ by clicking on Gang Sign"))
                                          (update! state side (assoc card :access-hq true)))}}
    :abilities [{:req (req (:access-hq card))
                 :msg (msg "access " (get-in @state [:runner :hq-access]) " card from HQ")
                 :effect (req (let [c (take (get-in @state [:runner :hq-access]) (shuffle (:hand corp)))]
                                (resolve-ability state :runner (choose-access c '(:hq)) card nil)
                                (update! state side (dissoc (get-card state card) :access-hq))))}]}

   "Gene Conditioning Shoppe"
   {:msg "make Genetics trigger a second time each turn"
    :effect (effect (register-persistent-flag! card :triggers-twice
                                               (fn [state side card]
                                                 (has? card :subtype "Genetics"))))
    :leave-play (effect (clear-persistent-flag! card :triggers-twice))}

   "Ghost Runner"
   {:data {:counter 3}
    :abilities [{:counter-cost 1 :msg "gain 1 [Credits]" :req (req (:run @state))
                 :effect (req (gain state side :credit 1)
                              (when (zero? (:counter card)) (trash state :runner card {:unpreventable true})))}]}

   "Globalsec Security Clearance"
   {:req (req (> (:link runner) 1))
    :flags {:runner-phase-12 (req true)}
    :abilities [{:msg "lose [Click] and look at the top card of R&D"
                 :once :per-turn
                 :effect (effect (lose :click 1)
                                 (prompt! card (str "The top card of R&D is "
                                 (:title (first (:deck corp)))) ["OK"] {}))}]}

   "Grifter"
   {:events {:runner-turn-ends
             {:effect (req (let [ab (if (get-in @state [:runner :register :successful-run])
                                      {:effect (effect (gain :credit 1)) :msg "gain 1 [Credits]"}
                                      {:effect (effect (trash card)) :msg "trash Grifter"})]
                             (resolve-ability state side ab card targets)))}}}

   "Hades Shard"
   {:abilities [{:msg "access all cards in Archives"
                 :effect (req (trash state side card {:cause :ability-cost})
                              (swap! state update-in [:corp :discard] #(map (fn [c] (assoc c :seen true)) %))
                              (resolve-ability state :runner (choose-access (get-in @state [:corp :discard]) '(:archives)) card nil))}]
    :install-cost-bonus (req (if (and run (= (:server run) [:archives]) (= 0 (:position run)))
                               [:credit -7 :click -1] nil))
    :effect (req (when (and run (= (:server run) [:archives]) (= 0 (:position run)))
                   (register-successful-run state side (:server run))
                   (swap! state update-in [:runner :prompt] rest)
                   (handle-end-run state side)))}

   "Hard at Work"
   (let [ability {:msg "gain 2 [Credits] and lose [Click]"
                  :once :per-turn
                  :effect (effect (lose :click 1) (gain :credit 2))}]
   {:flags {:drip-economy true}
    :events {:runner-turn-begins ability}
    :abilities [ability]})

   "Human First"
   {:events {:agenda-scored {:msg (msg "gain " (get-agenda-points state :corp target) " [Credits]")
                             :effect (effect (gain :runner :credit (get-agenda-points state :corp target)))}
             :agenda-stolen {:msg (msg "gain " (get-agenda-points state :runner target) " [Credits]")
                             :effect (effect (gain :credit (get-agenda-points state :runner target)))}}}

   "Hunting Grounds"
   {:abilities [{:label "Prevent a \"when encountered\" ability on a piece of ICE"
                 :msg "prevent a \"when encountered\" ability on a piece of ICE"
                 :once :per-turn}
                 {:label "[Trash]: Install the top 3 cards of your Stack facedown"
                  :msg "install the top 3 cards of their Stack facedown"
                  :effect (req (trash state side card {:cause :ability-cost})
                               (doseq [c (take 3 (:deck runner))]
                                  (runner-install state side c {:facedown true})))}]}

   "Ice Analyzer"
   {:events {:rez {:req (req (ice? target))
                   :msg "place 1 [Credits] on Ice Analyzer"
                   :effect (effect (add-prop :runner card :counter 1))}}
    :abilities [{:counter-cost 1 :effect (effect (gain :credit 1))
                 :msg "take 1 [Credits] to install programs"}]}

   "Ice Carver"
   {:events {:pre-ice-strength
             {:req (req (and (= (:cid target) (:cid current-ice)) (:rezzed target)))
              :effect (effect (ice-strength-bonus -1 target))}}}

   "Inside Man"
   {:recurring 2}

   "Investigative Journalism"
   {:req (req (> (:bad-publicity corp) 0))
    :abilities [{:cost [:click 4] :msg "give the Corp 1 bad publicity"
                 :effect (effect (gain :corp :bad-publicity 1) (trash card {:cause :ability-cost}))}]}

   "Jak Sinclair"
   (let [ability {:label "Make a run (start of turn)"
                  :prompt "Choose a server"
                  :choices (req servers)
                  :msg (msg "make a run on " target " during which no programs can be used")
                  :effect (effect (run target))}]
   {:install-cost-bonus (req [:credit (* -1 (:link runner))])
    :events {:runner-turn-begins
              {:optional {:prompt "Use Jak Sinclair to make a run?"
                          :once :per-turn
                          :yes-ability ability}}}
    :abilities [ability]})

   "John Masanori"
   {:events {:successful-run {:req (req (first-event state side :successful-run))
                              :msg "draw 1 card" :once-key :john-masanori-draw
                              :effect (effect (draw))}
             :unsuccessful-run {:req (req (first-event state side :unsuccessful-run))
                                :msg "take 1 tag" :once-key :john-masanori-tag
                                :effect (effect (tag-runner :runner 1))}}}

   "Joshua B."
   (let [ability {:msg "gain [Click]"
                  :once :per-turn
                  :label "Gain [Click] (start of turn)"
                  :effect (effect (gain :click 1))
                  :end-turn {:effect (effect (tag-runner 1))
                             :msg "gain 1 tag"}}]
   {:events {:runner-turn-begins
             {:optional {:prompt "Use Joshua B. to gain [Click]?"
                         :once :per-turn
                         :yes-ability ability}}}
    :abilities [ability]})

   "Kati Jones"
   {:data {:counter-type "Credit"}
    :abilities [{:cost [:click 1] :msg "store 3 [Credits]" :once :per-turn
                 :effect (effect (add-prop card :counter 3))}
                {:cost [:click 1] :msg (msg "gain " (:counter card) " [Credits]") :once :per-turn
                 :label "Take all credits"
                 :effect (effect (gain :credit (:counter card)) (set-prop card :counter 0))}]}

   "Liberated Account"
   {:data {:counter 16
           :counter-type "Credit"}
    :abilities [{:cost [:click 1] :counter-cost 4 :msg "gain 4 [Credits]"
                 :effect (req (gain state :runner :credit 4)
                              (when (<= (:counter card) 0) (trash state :runner card {:unpreventable true})))}]}

   "London Library"
   {:abilities [{:label "Install a non-virus program on London Library"
                 :cost [:click 1]
                 :prompt "Choose a non-virus program to install on London Library from your grip"
                 :choices {:req #(and (is-type? % "Program")
                                      (not (has-subtype? % "Virus"))
                                      (in-hand? %))}
                 :msg (msg "host " (:title target))
                 :effect (effect (runner-install target {:host-card card :no-cost true}))}
                {:label "Add a program hosted on London Library to your Grip"
                 :cost [:click 1]
                 :choices {:req #(:host %)}
                 :msg (msg "add " (:title target) " to their Grip")
                 :effect (effect (move target :hand))}]
    :events {:runner-turn-ends {:effect (req (doseq [c (:hosted card)]
                                               (trash state side c)))}}}

   "Motivation"
   (let [ability {:msg "look at the top card of their Stack"
                  :label "Look at the top card of Stack (start of turn)"
                  :once :per-turn
                  :req (req (:runner-phase-12 @state))
                  :effect (effect (prompt! card (str "The top card of your Stack is "
                                                     (:title (first (:deck runner)))) ["OK"] {}))}]
   {:events {:runner-turn-begins ability}
    :abilities [ability]})

   "Mr. Li"
   {:abilities [{:cost [:click 1] :prompt "Card to keep?"
                 :choices (req (take 2 (:deck runner))) :not-distinct true :msg "choose 1 card to draw"
                 :effect (req (move state side target :hand)
                              (if (= target (first (:deck runner)))
                                (move state side (second (:deck runner)) :deck)
                                (move state side (first (:deck runner)) :deck)))}]}

   "Muertos Gang Member"
   {:effect (req (resolve-ability
                   state :corp
                   {:prompt "Choose a card to derez"
                    :choices {:req #(and (= (:side %) "Corp") (:rezzed %))}
                    :effect (req (derez state side target))}
                  card nil))
    :leave-play (req (resolve-ability
                       state :corp
                       {:prompt "Choose a card to rez, ignoring the rez cost"
                        :choices {:req #(not (:rezzed %))}
                        :effect (req (rez state side target {:ignore-cost :rez-cost})
                                     (system-msg state side (str "rezzes " (:title target) " at no cost")))}
                      card nil))
    :abilities [{:msg "draw 1 card"
                 :effect (effect (trash card {:cause :ability-cost}) (draw))}]}

   "Neutralize All Threats"
   {:in-play [:hq-access 1]
    :events {:access {:effect (req (swap! state assoc-in [:runner :register :force-trash] false))}
             :pre-trash {:req (req (let [cards (map first (turn-events state side :pre-trash))]
                                     (empty? (filter #(not (nil? (:trash %))) cards))))
                         :effect (req (swap! state assoc-in [:runner :register :force-trash] true))}}}

   "New Angeles City Hall"
   {:prevent {:tag [:all]}
    :events {:agenda-stolen {:msg "trash itself" :effect (effect (trash card))}}
    :abilities [{:cost [:credit 2] :msg "avoid 1 tag" :effect (effect (tag-prevent 1))}]}

   "Off-Campus Apartment"
   {:abilities [{:label "Install and host a connection on Off-Campus Apartment"
                 :cost [:click 1] :prompt "Choose a connection in your Grip to install on Off-Campus Apartment"
                 :choices {:req #(and (has-subtype? % "Connection")
                                      (in-hand? %))}
                 :msg (msg "host " (:title target) " and draw 1 card")
                 :effect (effect (runner-install target {:host-card card}) (draw))}
                {:label "Host an installed connection"
                 :prompt "Choose a connection to host on Off-Campus Apartment"
                 :choices {:req #(and (has-subtype? % "Connection")
                                      (installed? %))}
                 :msg (msg "host " (:title target) " and draw 1 card")
                 :effect (effect (host card target) (draw))}]}

   "Oracle May"
   {:abilities [{:cost [:click 1]
                 :once :per-turn
                 :prompt "Choose card type"
                 :choices ["Event" "Hardware" "Program" "Resource"]
                 :effect (req (let [c (first (get-in @state [:runner :deck]))]
                                (system-msg state side (str "uses Oracle May, names " target
                                                            " and reveals " (:title c)))
                                (if (is-type? c target)
                                  (do (system-msg state side (str "gains 2 [Credits] and draws " (:title c)))
                                      (gain state side :credit 2) (draw state side))
                                  (do (system-msg state side (str "trashes " (:title c))) (mill state side)))))}]}

   "Order of Sol"
   {:effect (req (add-watch state :order-of-sol
                            (fn [k ref old new]
                              (when (and (not (zero? (get-in old [:runner :credit])))
                                         (zero? (get-in new [:runner :credit])))
                                (resolve-ability ref side {:msg "gain 1 [Credits]" :once :per-turn
                                                           :effect (effect (gain :credit 1))} card nil)))))
    :events {:runner-turn-begins {:req (req (= (:credit runner) 0)) :msg "gain 1 [Credits]"
                                  :effect (req (gain state :runner :credit 1)
                                               (swap! state assoc-in [:per-turn (:cid card)] true))}
             :corp-turn-begins {:req (req (= (:credit runner) 0)) :msg "gain 1 [Credits]"
                                :effect (req (gain state :runner :credit 1)
                                             (swap! state assoc-in [:per-turn (:cid card)] true))}
             :runner-install {:req (req (and (= target card) (= (:credit runner) 0))) :msg "gain 1 [Credits]"
                              :effect (req (gain state :runner :credit 1)
                                           (swap! state assoc-in [:per-turn (:cid card)] true))}}
    :leave-play (req (remove-watch state :order-of-sol))}

   "Paige Piper"
   (let [pphelper (fn [title cards]
                    (let [num (count cards)]
                      {:optional
                       {:prompt (str "Use Paige Piper to trash copies of " title "?")
                        :yes-ability {:prompt "How many would you like to trash?"
                                      :choices {:number (req num)}
                                      :msg "shuffle their Stack"
                                      :effect (req (doseq [c (take (int target) cards)]
                                                     (trash state side c {:unpreventable true}))
                                                   (shuffle! state :runner :deck)
                                                   (when (> (int target) 0)
                                                     (system-msg state side (str "trashes " (int target)
                                                                                 " cop" (if (> (int target) 1) "ies" "y")
                                                                                 " of " title))))}}}))]
     {:events {:runner-install {:req (req (first-event state side :runner-install))
                                :effect (effect (resolve-ability
                                                 (pphelper (:title target)
                                                           (->> (:deck runner)
                                                                (filter #(has? % :title (:title target)))
                                                                (vec)))
                                                 card nil))}}})

   "Paparazzi"
   {:effect (req (swap! state update-in [:runner :tagged] inc))
    :events {:pre-damage {:req (req (= target :meat)) :msg "prevent all meat damage"
                          :effect (effect (damage-prevent :meat Integer/MAX_VALUE))}}
    :leave-play (req (swap! state update-in [:runner :tagged] dec))}

   "Personal Workshop"
   (let [remove-counter
         {:req (req (not (empty? (:hosted card))))
          :once :per-turn
          :msg (msg "remove 1 counter from " (:title target)) :choices {:req #(:host %)}
          :effect (req (if (<= (:counter target) 1)
                         (runner-install state side (dissoc target :counter) {:no-cost true})
                         (add-prop state side target :counter -1)))}]
     {:flags {:drip-economy true}
      :abilities [{:label "Host a program or piece of hardware" :cost [:click 1]
                   :prompt "Choose a card to host on Personal Workshop"
                   :choices {:req #(and (#{"Program" "Hardware"} (:type %))
                                        (in-hand? %)
                                        (= (:side %) "Runner"))}
                   :effect (req (if (zero? (:cost target))
                                  (runner-install state side target)
                                  (host state side card (assoc target :counter (:cost target)))))
                   :msg (msg "host " (:title target) "")}
                  (assoc remove-counter
                         :label "Remove 1 counter from a hosted card (start of turn)" :cost [:credit 1])
                  {:label "X[Credit]:Remove counters from a hosted card" :choices {:req #(:host %)}
                   :req (req (not (empty? (:hosted card))))
                   :effect (req (let [paydowntarget target]
                                  (resolve-ability
                                    state side
                                    {:prompt "How many counters to remove?"
                                     :choices {:number (req (min (:credit runner) (:counter paydowntarget)))}
                                     :msg (msg "remove " target " counters from " (:title paydowntarget))
                                     :effect (req (do
                                                    (lose state side :credit target)
                                                    (if (= (:counter paydowntarget) target)
                                                      (runner-install state side (dissoc paydowntarget :counter) {:no-cost true})
                                                      (add-prop state side paydowntarget :counter (- target)))))}
                                    card nil)))}]
      :events {:runner-turn-begins remove-counter}})

   "Political Operative"
   {:req (req (some #{:hq} (:successful-run runner-reg)))
    :abilities [{:prompt "Choose a rezzed card with a trash cost"
                 :choices {:req #(and (:trash %) (rezzed? %))}
                 :effect (req (let [c target]
                                (trigger-event state side :pre-trash c)
                                (let [cost (trash-cost state :runner c)]
                                  (when (can-pay? state side nil [:credit cost])
                                    (resolve-ability
                                      state side
                                      {:msg (msg "pay " cost " [Credit] and trash " (:title c))
                                       :effect (effect (lose :credit cost)
                                                       (trash card {:cause :ability-cost})
                                                       (trash c))}
                                     card nil)))
                                (swap! state update-in [:bonus] dissoc :trash)))}]}

   "Power Tap"
   {:events {:trace {:msg "gain 1 [Credits]" :effect (effect (gain :runner :credit 1))}}}

   "Professional Contacts"
   {:abilities [{:cost [:click 1] :effect (effect (gain :credit 1) (draw))
                 :msg "gain 1 [Credits] and draw 1 card"}]}

   "Public Sympathy"
   {:in-play [:hand-size-modification 2]}

   "Rachel Beckman"
   {:in-play [:click 1 :click-per-turn 1]
    :effect (req (add-watch state :rachel-beckman
                            (fn [k ref old new]
                              (when (is-tagged? new)
                                (remove-watch ref :rachel-beckman)
                                (trash ref :runner card)
                                (system-msg ref side "trashes Rachel Beckman for being tagged")))))}

   "Raymond Flint"
   {:effect (req (add-watch state :raymond-flint
                            (fn [k ref old new]
                              (when (< (get-in old [:corp :bad-publicity]) (get-in new [:corp :bad-publicity]))
                                (resolve-ability
                                 ref side
                                 {:msg (msg "access " (get-in @state [:runner :hq-access]) " card from HQ")
                                  :effect (req (let [c (take (get-in @state [:runner :hq-access]) (shuffle (:hand corp)))]
                                                 (resolve-ability state :runner (choose-access c '(:hq)) card nil)))}
                                 card nil)))))
    :leave-play (req (remove-watch state :raymond-flint))
    :abilities [{:label "Expose 1 card"
                 :effect (effect (resolve-ability
                                   {:choices {:req installed?}
                                    :effect (effect (expose target) (trash card {:cause :ability-cost}))
                                    :msg (msg "expose " (:title target))} card nil))}]}

   "Rolodex"
   {:msg "look at the top 5 cards of their Stack"
    :effect (req (prompt! state side card
                          (str "Drag cards from the Temporary Zone  back onto your Stack") ["OK"] {})
                 (doseq [c (take 5 (:deck runner))] (move state side c :play-area)))}

   "Sacrificial Clone"
   {:prevent {:damage [:meat :net :brain]}
    :abilities [{:effect (req (doseq [c (concat (get-in runner [:rig :hardware])
                                                (filter #(not (has-subtype? % "Virtual"))
                                                        (get-in runner [:rig :resource]))
                                                (:hand runner))]
                                (trash state side c {:cause :ability-cost}))
                              (lose state side :credit :all :tag :all)
                              (damage-prevent state side :net Integer/MAX_VALUE)
                              (damage-prevent state side :meat Integer/MAX_VALUE)
                              (damage-prevent state side :brain Integer/MAX_VALUE))}]}

   "Sacrificial Construct"
   {:prevent {:trash [:program :hardware]}
    :abilities [{:effect (effect (trash-prevent :program 1) (trash-prevent :hardware 1)
                                 (trash card {:cause :ability-cost}))}]}

   "Safety First"
   {:in-play [:hand-size-modification -2]
    :events {:runner-turn-ends {:req (req (< (count (:hand runner)) (hand-size state :runner)))
                                :msg (msg "draw a card")
                                :effect (effect (draw 1))}}}

   "Same Old Thing"
   {:abilities [{:cost [:click 2]
                 :req (req (not (seq (get-in @state [:runner :locked :discard]))))
                 :prompt "Choose an event to play"
                 :msg (msg "play " (:title target))
                 :show-discard true
                 :choices {:req #(and (is-type? % "Event")
                                      (= (:zone %) [:discard]))}
                 :effect (effect (trash card {:cause :ability-cost}) (play-instant target))}]}

   "Scrubber"
   {:recurring 2}

   "Security Testing"
   (let [ability {:prompt "Choose a server for Security Testing" :choices (req servers)
                  :msg (msg "target " target)
                  :effect (effect (update! (assoc card :testing-target (vec (next (server->zone state target))))))}]
   {:events {:runner-turn-begins ability
             :successful-run
             {:req (req (= (get-in @state [:run :server]) (get (get-card state card) :testing-target)))
              :once :per-turn
              :effect (req (let [st card]
                             (swap! state assoc-in [:run :run-effect :replace-access]
                                    {:mandatory true
                                     :effect (effect (resolve-ability
                                                      {:msg "gain 2 [Credits] instead of accessing"
                                                       :effect (effect (gain :credit 2))} st nil))})))}}
    :abilities [ability]})

   "Spoilers"
   {:events {:agenda-scored {:msg "trash the top card of R&D" :effect (effect (mill :corp))}}}

   "Starlight Crusade Funding"
   {:msg "ignore additional costs on Double events"
    :effect (req (swap! state assoc-in [:runner :register :double-ignore-additional] true))
    :events {:runner-turn-begins
             {:msg "lose [Click] and ignore additional costs on Double events"
              :effect (req (lose state :runner :click 1)
                           (swap! state assoc-in [:runner :register :double-ignore-additional] true))}}
    :leave-play (req (swap! state update-in [:runner :register] dissoc :double-ignore-additional))}

   "Stim Dealer"
   {:events {:runner-turn-begins
             {:effect (req (if (>= (:counter card) 2)
                             (do (set-prop state side card :counter 0)
                                 (damage state side :brain 1 {:unpreventable true :card card})
                                 (system-msg state side "takes 1 brain damage from Stim Dealer"))
                             (do (add-prop state side card :counter 1)
                                 (gain state side :click 1)
                                 (system-msg state side "uses Stim Dealer to gain [Click]"))))}}}

   "Street Peddler"
   {:effect (req (doseq [c (take 3 (:deck runner))]
                   (host state side (get-card state card) c {:facedown true})))
    :abilities [{:prompt "Choose a card on Street Peddler to install"
                 :choices (req (cancellable (filter #(and (not (is-type? % "Event"))
                                                          (can-pay? state side nil (modified-install-cost state side % [:credit -1])))
                                                    (:hosted card))))
                 :msg (msg "install " (:title target) " lowering its install cost by 1 [Credits]")
                 :effect (req
                           (when (can-pay? state side nil (modified-install-cost state side target [:credit -1]))
                             (install-cost-bonus state side [:credit -1])
                             (runner-install state side (dissoc target :facedown))
                             (trash state side (update-in card [:hosted]
                                                          (fn [coll]
                                                            (remove-once #(not= (:cid %) (:cid target)) coll)))
                                    {:cause :ability-cost})))}]}

   "Symmetrical Visage"
   {:events {:runner-click-draw {:req (req (or (first-event state side :runner-click-draw)
                                               (and (second-event state side :runner-click-draw)
                                                    (persistent-flag? state side card :triggers-twice))))
                                 :msg "gain 1 [Credits]"
                                 :effect (effect (gain :credit 1))}}}

   "Synthetic Blood"
   {:events {:damage {:req (req (or (first-event state side :damage)
                                    (and (second-event state side :damage)
                                         (persistent-flag? state side card :triggers-twice))))
                      :msg "draw 1 card"
                      :effect (effect (draw :runner))}}}

   "Tallie Perrault"
   {:abilities [{:label "Draw 1 card for each Corp bad publicity"
                 :effect (effect (trash card {:cause :ability-cost}) (draw (:bad-publicity corp)))
                 :msg (msg "draw " (:bad-publicity corp) " cards")}]
    :events {:play-operation
             {:req (req (or (has-subtype? target "Black Ops")
                            (has-subtype? target "Gray Ops")))
              :effect (req (show-wait-prompt state :corp "Runner to use Tallie Perrault")
                           (resolve-ability
                             state :runner
                             {:optional
                              {:prompt "Use Tallie Perrault to give the Corp 1 bad publicity and take 1 tag?"
                               :player :runner
                               :yes-ability {:msg "give the Corp 1 bad publicity and take 1 tag"
                                             :effect (effect (gain :corp :bad-publicity 1)
                                                             (tag-runner :runner 1)
                                                             (clear-wait-prompt :corp))}
                               :no-ability {:effect (effect (clear-wait-prompt :corp))}}}
                            card nil))}}}

   "Technical Writer"
   {:data {:counter-type "Credit"}
    :events {:runner-install {:req (req (some #(= % (:type target)) '("Hardware" "Program")))
                              :effect (effect (add-prop :runner card :counter 1)
                                              (system-msg (str "places 1 [Credits] on Technical Writer")))}}
    :abilities [{:cost [:click 1] :msg (msg "gain " (:counter card) " [Credits]")
                 :effect (effect (gain :credit (:counter card)) (trash card {:cause :ability-cost}))}]}

   "The Helpful AI"
   {:in-play [:link 1]
    :abilities [{:msg (msg "give +2 strength to " (:title target))
                 :choices {:req #(and (has-subtype? % "Icebreaker")
                                      (installed? %))}
                 :effect (effect (update! (assoc card :hai-target target))
                                 (trash (get-card state card) {:cause :ability-cost})
                                 (update-breaker-strength target))}]
    :events {:runner-turn-ends nil :corp-turn-ends nil :pre-breaker-strength nil}
    :trash-effect {:effect
                   (effect (register-events
                             (let [hai {:effect (effect (unregister-events card)
                                                        (update! (dissoc card :hai-target))
                                                        (update-breaker-strength (:hai-target card)))}]
                               {:runner-turn-ends hai :corp-turn-ends hai
                                :pre-breaker-strength {:req (req (= (:cid target)(:cid (:hai-target card))))
                                                       :effect (effect (breaker-strength-bonus 2))}}) card))}}

   "The Supplier"
   (let [ability  {:label "Install a hosted card (start of turn)"
                   :prompt "Choose a card hosted on The Supplier to install"
                   :once :per-turn
                   :req (req (some #(can-pay? state side nil (modified-install-cost state side % [:credit -2]))
                                   (:hosted card)))
                   :choices {:req #(= "The Supplier" (:title (:host %)))}
                   :msg (msg "install " (:title target) " lowering its install cost by 2")
                   :effect (req (when (can-pay? state side nil (modified-install-cost state side target [:credit -2]))
                                  (install-cost-bonus state side [:credit -2])
                                  (runner-install state side target)
                                  (update! state side (-> card
                                                          (assoc :supplier-installed (:cid target))
                                                          (update-in [:hosted]
                                                                     (fn [coll]
                                                                       (remove-once #(not= (:cid %) (:cid target)) coll)))))))}]
   {:flags {:drip-economy true}  ; not technically drip economy, but has an interaction with Drug Dealer
    :abilities [{:label "Host a resource or piece of hardware" :cost [:click 1]
                 :prompt "Choose a card to host on The Supplier"
                 :choices {:req #(and (#{"Resource" "Hardware"} (:type %))
                                      (in-hand? %))}
                 :effect (effect (host card target)) :msg (msg "host " (:title target) "")}
                ability]

    ; A card installed by The Supplier is ineligible to receive the turn-begins event for this turn.
    :suppress {:runner-turn-begins {:req (req (= (:cid target) (:supplier-installed (get-card state card))))}}
    :events {:runner-turn-begins ability
             :runner-turn-ends {:req (req (:supplier-installed card))
                                :effect (effect (update! (dissoc card :supplier-installed)))}}})

   "The Source"
   {:effect (effect (update-all-advancement-costs))
    :leave-play (effect (update-all-advancement-costs))
    :events {:agenda-scored {:effect (effect (trash card))}
             :agenda-stolen {:effect (effect (trash card))}
             :pre-advancement-cost {:effect (effect (advancement-cost-bonus 1))}
             :pre-steal-cost {:effect (effect (steal-cost-bonus [:credit 3]))}}}

   "Theophilius Bagbiter"
   {:effect (req (lose state :runner :credit :all)
                 (add-watch state :theophilius-bagbiter
                            (fn [k ref old new]
                              (let [credit (get-in new [:runner :credit])]
                                (when (not= (get-in old [:runner :credit]) credit)
                                  (swap! ref assoc-in [:runner :hand-size-base] credit))))))
    :leave-play (req (remove-watch state :theophilius-bagbiter)
                     (swap! state assoc-in [:runner :hand-size-base] 5))}

   "Tri-maf Contact"
   {:abilities [{:cost [:click 1] :msg "gain 2 [Credits]" :once :per-turn
                 :effect (effect (gain :credit 2))}]
    :leave-play (effect (damage :meat 3 {:unboostable true :card card}))}

   "Tyson Observatory"
   {:abilities [{:prompt "Choose a piece of Hardware" :msg (msg "adds " (:title target) " to their Grip")
                 :choices (req (cancellable (filter #(is-type? % "Hardware") (:deck runner)) :sorted))
                 :cost [:click 2] :effect (effect (move target :hand) (shuffle! :deck))}]}

   "Underworld Contact"
   (let [ability {:label "Gain 1 [Credits] (start of turn)"
                  :msg "gain 1 [Credits]"
                  :once :per-turn
                  :req (req (and (>= (:link runner) 2) (:runner-phase-12 @state)))
                  :effect (effect (gain :credit 1))}]
   {:flags {:drip-economy true}
    :abilities [ability]
    :events {:runner-turn-begins ability}})

   "Utopia Shard"
   {:abilities [{:effect (effect (trash-cards :corp (take 2 (shuffle (:hand corp))))
                                 (trash card {:cause :ability-cost}))
                 :msg "force the Corp to discard 2 cards from HQ at random"}]
    :install-cost-bonus (req (if (and run (= (:server run) [:hq]) (zero? (:position run)))
                               [:credit -7 :click -1] nil))
    :effect (req (when (and run (= (:server run) [:hq]) (zero? (:position run)))
                   (register-successful-run state side (:server run))
                   (swap! state update-in [:runner :prompt] rest)
                   (handle-end-run state side)))}

   "Virus Breeding Ground"
   {:data {:counter-type "Virus"}
    :events {:runner-turn-begins {:effect (effect (add-prop card :counter 1))}}
    :abilities [{:cost [:click 1]
                 :msg (msg "move 1 virus counter to " (:title target))
                 :req (req (pos? (get card :counter 0)))
                 :choices {:req #(and (has-subtype? % "Virus")
                                      (pos? (get % :counter 0)))}
                 :effect (req (when (pos? (get-virus-counters state side target))
                                (add-prop state side card :counter -1)
                                (add-prop state side target :counter 1)))}]}

   "Wasteland"
   {:events {:runner-trash {:req (req (and (first-event state :runner :runner-trash) (:installed target)))
                     :effect (effect (gain :credit 1))
                     :msg "to gain 1[Credit]"}}}

   "Wireless Net Pavilion"
   {:effect (effect (trash-resource-bonus -2))
    :leave-play (effect (trash-resource-bonus 2))}

   "Woman in the Red Dress"
   (let [ability {:msg (msg "reveal " (:title (first (:deck corp))) " on the top of R&D")
                  :label "Reveal the top card of R&D (start of turn)"
                  :once :per-turn
                  :req (req (:runner-phase-12 @state))
                  :effect (effect (show-wait-prompt :runner "Corp to decide whether or not to draw with Woman in the Red Dress")
                                  (resolve-ability
                                    {:optional
                                     {:player :corp
                                      :prompt (msg "Draw " (:title (first (:deck corp))) "?")
                                      :yes-ability {:effect (effect (clear-wait-prompt :runner)
                                                                    (system-msg (str "draws " (:title (first (:deck corp)))))
                                                                    (draw))}
                                      :no-ability {:effect (effect (clear-wait-prompt :runner)
                                                                   (system-msg "doesn't draw with Woman in the Red Dress"))}}}
                                    card nil))}]
   {:events {:runner-turn-begins ability}
    :abilities [ability]})

   "Wyldside"
   {:flags {:runner-turn-draw true
            :runner-phase-12 (req (< 1 (count (filter #(card-flag? % :runner-turn-draw true)
                                                      (cons (get-in @state [:runner :identity])
                                                            (all-installed state :runner))))))}

    :events {:runner-turn-begins {:effect (req (lose state side :click 1)
                                               (when-not (get-in @state [:per-turn (:cid card)])
                                                 (system-msg state side "uses Wyldside to draw 2 cards and lose [Click]")
                                                 (draw state side 2)))}}
    :abilities [{:msg "draw 2 cards and lose [Click]"
                 :once :per-turn
                 :effect (effect (draw 2))}]}

   "Xanadu"
   {:events {:pre-rez-cost {:req (req (ice? target))
                            :effect (effect (rez-cost-bonus 1))}}}

   "Zona Sul Shipping"
   {:events {:runner-turn-begins {:effect (effect (add-prop card :counter 1))}}
    :abilities [{:cost [:click 1] :msg (msg "gain " (:counter card) " [Credits]")
                 :label "Take all credits"
                 :effect (effect (gain :credit (:counter card)) (set-prop card :counter 0))}]
    :effect (req (add-watch state (keyword (str "zona-sul-shipping" (:cid card)))
                            (fn [k ref old new]
                              (when (is-tagged? new)
                                (remove-watch ref (keyword (str "zona-sul-shipping" (:cid card))))
                                (trash ref :runner card)
                                (system-msg ref side "trashes Zona Sul Shipping for being tagged")))))}})
