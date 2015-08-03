(in-ns 'game.core)

(def cards-resources
  {"Access to Globalsec"
   {:effect (effect (gain :link 1)) :leave-play (effect (lose :link 1))}

   "Activist Support"
   {:events
    {:corp-turn-begins {:req (req (not tagged)) :msg "take 1 tag" :effect (effect (gain :runner :tag 1))}
     :runner-turn-begins {:req (req (zero? (:bad-publicity corp))) :msg "give the Corp 1 bad publicity"
                           :effect (effect (gain :corp :bad-publicity 1))}}}

   "Adjusted Chronotype"
   {:events {:runner-loss {:req (req (and (some #{:click} target)
                                           (empty? (filter #(= :click %)
                                                           (mapcat first (turn-events state side :runner-loss))))))
                            :msg "gain [Click]" :effect (effect (gain :runner :click 1))}}}

   "Aesops Pawnshop"
   {:abilities [{:msg (msg "trash " (:title target) " and gain 3 [Credits]")
                  :choices {:req #(and (= (:side %) "Runner") (:installed %))}
                  :effect (effect (gain :credit 3) (trash target))}]}

   "All-nighter"
   {:abilities [{:cost [:click 1] :effect (effect (trash card {:cause :ability-cost}) (gain :click 2))
                  :msg "gain [Click][Click]"}]}

   "Angel Arena"
   {:prompt "How many power counters?" :choices :credit :msg (msg "add " target " power counters")
    :effect (effect (set-prop card :counter target))
    :abilities [{:counter-cost 1 :msg "look at the top card of Stack"
                 :effect (req (when (zero? (:counter card)) (trash state :runner card)))
                 :optional {:prompt (msg "Add " (:title (first (:deck runner))) " to bottom of Stack?")
                            :msg "add the top card of Stack to the bottom"
                            :effect (req (move state side (first (:deck runner)) :deck))}}]}

   "Armitage Codebusting"
   {:data {:counter 12}
    :abilities [{:cost [:click 1] :counter-cost 2 :msg "gain 2 [Credits]"
                 :effect (req (gain state :runner :credit 2)
                              (when (zero? (:counter card)) (trash state :runner card)))}]}

   "Bank Job"
   {:data {:counter 8}
    :abilities [{:label "Take any number of [Credits] on Bank Job"
                 :prompt "How many [Credits]?" :choices :counter :msg (msg "gain " target " [Credits]")
                 :effect (req (gain state side :credit target)
                              (when (= target (:counter card)) (trash state :runner card)))}]}

   "Beach Party"
   {:effect (effect (gain :max-hand-size 5)) :leave-play (effect (lose :max-hand-size 5))
    :events {:runner-turn-begins {:msg "lose [Click]" :effect (effect (lose :click 1))}}}

   "Borrowed Satellite"
   {:effect (effect (gain :link 1 :max-hand-size 1))
    :leave-play (effect (lose :link 1 :max-hand-size 1))}

   "Chrome Parlor"
   {:events
    {:pre-damage {:req (req (has? (second targets) :subtype "Cybernetic"))
                  :effect (effect (damage-prevent target Integer/MAX_VALUE))}}}

   "Compromised Employee"
   {:recurring 1
    :events {:rez {:req (req (= (:type target) "ICE")) :msg "gain 1 [Credits]"
                   :effect (effect (gain :runner :credit 1))}}}

   "Crash Space"
   {:prevent {:damage [:meat]}
    :recurring 2
    :abilities [{:label "Trash to prevent up to 3 meat damage"
                 :msg "prevent up to 3 meat damage"
                 :effect (effect (trash card {:cause :ability-cost}) (damage-prevent :meat 3))}]}

   "Daily Casts"
   {:data {:counter 8}
    :events {:runner-turn-begins {:msg "gain 2 [Credits]" :counter-cost 2
                                  :effect (req (gain state :runner :credit 2)
                                               (when (zero? (:counter card)) (trash state :runner card)))}}}

   "Data Dealer"
   {:abilities [{:cost [:click 1 :forfeit] :effect (effect (gain :credit 9))
                 :msg (msg "gain 9 [Credits]")}]}

   "Data Folding"
   {:events {:runner-turn-begins {:req (req (>= (:memory runner) 2)) :msg "gain 1 [Credits]"
                                  :effect (effect (gain :credit 1))}}}

   "Data Leak Reversal"
   {:req (req (some #{:hq :rd :archives} (:successful-run runner-reg)))
    :abilities [{:req (req tagged) :cost [:click 1] :effect (effect (mill :corp))
                 :msg "force the Corp to trash the top card of R&D"}]}

   "Decoy"
   {:abilities [{:msg "avoid 1 tag" :effect (effect (trash card {:cause :ability-cost}))}]}

   "Duggars"
   {:abilities [{:cost [:click 4] :effect (effect (draw 10)) :msg "draw 10 cards"}]}

   "Earthrise Hotel"
   {:data {:counter 3}
    :events {:runner-turn-begins {:msg "draw 2 cards" :counter-cost 1
                                  :effect (req (draw state :runner 2)
                                               (when (zero? (:counter card))
                                                 (trash state :runner card)))}}}

   "Eden Shard"
   {:abilities [{:effect (effect (trash card {:cause :ability-cost}) (draw :corp 2))
                 :msg "force the Corp to draw 2 cards"}]}

   "Enhanced Vision"
   {:events {:successful-run {:msg (msg "force the Corp to reveal " (:title (first (shuffle (:hand corp)))))
                              :req (req (first-event state side :successful-run))}}}

   "Fall Guy"
   {:prevent {:trash [:resource]}
    :abilities [{:label "Prevent a resource from being trashed"
                 :effect (effect (trash-prevent :resource 1) (trash card {:unpreventable true :cause :ability-cost}))}
                {:effect (effect (trash card {:cause :ability-cost}) (gain :credit 2)) :msg "gain 2 [Credits]"}]}

   "Fester"
   {:events {:purge {:msg "force the Corp to lose 2 [Credits] if able"
                     :effect (effect (pay :corp card :credit 2))}}}

   "Gang Sign"
   {:events {:agenda-scored {:msg "access 1 card from HQ"
                             :effect (req (doseq [c (take (get-in @state [:runner :hq-access]) (shuffle (:hand corp)))]
                                            (system-msg state :runner (str "accesses " (:title c)))
                                            (handle-access state :runner [c])))}}}

   "Ghost Runner"
   {:data {:counter 3}
    :abilities [{:counter-cost 1 :msg "gain 1 [Credits]" :req (req (:run @state))
                 :effect (req (gain state side :credit 1)
                              (when (zero? (:counter card)) (trash state :runner card)))}]}

   "Grifter"
   {:events {:runner-turn-ends
             {:effect #(let [ab (if (get-in @%1 [:runner :register :successful-run])
                                  {:effect (effect (gain :credit 1)) :msg "gain 1 [Credits]"}
                                  {:effect (effect (trash %3)) :msg "trash Grifter"})]
                        (resolve-ability %1 %2 ab %3 %4))}}}

   "Hades Shard"
   {:abilities [{:msg "access all cards in Archives"
                 :effect (effect (trash card {:cause :ability-cost}) (handle-access (access state side [:archives])))}]}

   "Hard at Work"
   {:events {:runner-turn-begins {:msg "gain 2 [Credits] and lose [Click]"
                                  :effect (effect (lose :click 1) (gain :credit 2))}}}

   "Human First"
   {:events {:agenda-scored {:msg (msg "gain " (:agendapoints target) " [Credits]")
                             :effect (effect (gain :runner :credit (:agendapoints target)))}
             :agenda-stolen {:msg (msg "gain " (:agendapoints target) " [Credits]")
                             :effect (effect (gain :credit (:agendapoints target)))}}}

   "Ice Analyzer"
   {:events {:rez {:req (req (= (:type target) "ICE")) :msg "place 1 [Credits] on Ice Analyzer"
                   :effect (effect (add-prop :runner card :counter 1))}}
    :abilities [{:counter-cost 1 :effect (effect (gain :credit 1))
                 :msg "take 1 [Credits] to install programs"}]}

   "Ice Carver"
   {:events {:pre-ice-strength
             {:req (req (and (= (:cid target) (:cid current-ice)) (:rezzed target)))
              :effect (effect (ice-strength-bonus -1))}}}

   "Inside Man"
   {:recurring 2}

   "Investigative Journalism"
   {:req (req (> (:bad-publicity corp) 0))
    :abilities [{:cost [:click 4] :msg "give the Corp 1 bad publicity"
                 :effect (effect (gain :corp :bad-publicity 1) (trash card {:cause :ability-cost}))}]}

   "John Masanori"
   {:events {:successful-run {:req (req (first-event state side :successful-run))
                              :msg "draw 1 card" :once-key :john-masanori-draw
                              :effect (effect (draw))}
             :unsuccessful-run {:req (req (first-event state side :unsuccessful-run))
                                :msg "take 1 tag" :once-key :john-masanori-tag
                                :effect (effect (gain :runner :tag 1))}}}

   "Joshua B."
   {:events {:runner-turn-begins
             {:optional {:prompt "Use Joshua B. to gain [Click]?" :msg "gain [Click]"
                         :effect (effect (gain :click 1))
                         :end-turn {:effect (effect (gain :tag 1)) :msg "gain 1 tag"}}}}}

   "Kati Jones"
   {:abilities
    [{:cost [:click 1] :msg "store 3 [Credits]" :once :per-turn
      :effect (effect (add-prop card :counter 3))}
     {:cost [:click 1] :msg (msg "gain " (:counter card) " [Credits]") :once :per-turn
      :label "Take all credits"
      :effect (effect (gain :credit (:counter card)) (set-prop card :counter 0))}]}

   "Liberated Account"
   {:data {:counter 16}
    :abilities [{:cost [:click 1] :counter-cost 4 :msg "gain 4 [Credits]"
                 :effect (req (gain state :runner :credit 4)
                              (when (= (:counter card) 0) (trash state :runner card)))}]}

   "London Library"
   {:abilities [{:label "Install a non-virus program on London Library" :cost [:click 1]
                 :prompt "Choose a non-virus program to install on London Library"
                 :choices (req (filter #(and (= (:type %) "Program")
                                             (not (has? % :subtype "Virus"))
                                             (<= (:memoryunits %) (:memory runner)))
                                       (:hand runner)))
                 :msg (msg "host " (:title target))
                 :effect (effect (runner-install target {:host-card card :no-cost true}))}
                {:label "Add a program hosted on London Library to your Grip" :cost [:click 1]
                 :choices {:req #(:host %)} :msg (msg "add " (:title target) "to his or her Grip")
                 :effect (effect (move target :hand))}]
    :events {:runner-turn-ends {:effect (req (doseq [c (:hosted card)]
                                               (trash state side c)))}}}

   "Motivation"
   {:events
    {:runner-turn-begins
     {:msg "look at the top card of his Stack"
      :effect (effect (prompt! card (str "The top card of your Stack is "
                                         (:title (first (:deck runner)))) ["OK"] {}))}}}
   "Mr. Li"
   {:abilities [{:cost [:click 1] :prompt "Card to keep?"
                 :choices (req (take 2 (:deck runner))) :msg "choose 1 card to draw"
                 :effect (req (move state side target :hand)
                              (if (= target (first (:deck runner)))
                                (move state side (second (:deck runner)) :deck)
                                (move state side (first (:deck runner)) :deck)))}]}

   "Muertos Gang Member"
   {:abilities [{:msg "draw 1 card"
                 :effect (effect (trash card {:cause :ability-cost}) (draw))}] }

   "New Angeles City Hall"
   {:events {:agenda-stolen {:msg "trash itself" :effect (effect (trash card))}}
    :abilities [{:cost [:credit 2] :msg "avoid 1 tag" :effect (effect (lose :tag 1))}]}

   "Off-Campus Apartment"
   {:abilities [{:label "Install and host a connection on Off-Campus Apartment"
                 :cost [:click 1] :prompt "Choose a connection to install on Off-Campus Apartment"
                 :choices (req (filter #(and (has? % :subtype "Connection")
                                             (<= (:cost %) (:credit runner)))
                                       (:hand runner)))
                 :msg (msg "host " (:title target) " and draw 1 card")
                 :effect (effect (runner-install target {:host-card card}) (draw))}
                {:label "Host an installed connection"
                 :prompt "Choose a connection to host on Off-Campus Apartment"
                 :choices {:req #(and (has? % :subtype "Connection") (:installed %))}
                 :msg (msg "host " (:title target) " and draw 1 card")
                 :effect (effect (host card target) (draw))}]}

   "Oracle May"
   {:abilities [{:cost [:click 1] :once :per-turn :prompt "Choose card type"
                 :choices ["Event" "Hardware" "Program" "Resource"]
                 :effect #(let [c (first (get-in @%1 [:runner :deck]))]
                           (system-msg %1 %2 (str "uses Oracle May, names " (first %4)
                                                  " and reveals " (:title c)))
                           (if (= (:type c) (first %4))
                             (do (system-msg %1 %2 (str "gains 2 [Credits] and draws " (:title c)))
                                 (gain %1 %2 :credit 2) (draw %1 %2))
                             (do (system-msg %1 %2 (str "trashes " (:title c))) (mill %1 %2))))}]}

   "Order of Sol"
   {:effect (req (add-watch state :order-of-sol
                            (fn [k ref old new]
                              (when (and (not (zero? (get-in old [:runner :credit])))
                                         (zero? (get-in new [:runner :credit])))
                                (resolve-ability ref side {:msg "gain 1 [Credits]" :once :per-turn
                                                           :effect (effect (gain :credit 1))} card nil)))))
    :leave-play (req (remove-watch state :order-of-sol))}

   "Personal Workshop"
   (let [remove-counter
         {:req (req (not (empty? (:hosted card))))
          :msg (msg "remove 1 counter from " (:title target)) :choices {:req #(:host %)}
          :effect (req (if (= (:counter target) 1)
                         (runner-install state side (dissoc target :counter) {:no-cost true})
                         (add-prop state side target :counter -1)))}]
     {:abilities [{:label "Host a program or piece of hardware" :cost [:click 1]
                   :prompt "Choose a card to host on Personal Workshop"
                   :choices (req (filter #(#{"Program" "Hardware"} (:type %)) (:hand runner)))
                   :effect (effect (host card (assoc target :counter (:cost target))))
                   :msg (msg "host " (:title target) "")}
                  (assoc remove-counter
                         :label "Remove 1 counter from a hosted card" :cost [:credit 1])]
      :events {:runner-turn-begins remove-counter}})

   "Power Tap"
   {:events {:trace {:msg "gain 1 [Credits]" :effect (effect (gain :runner :credit 1))}}}

   "Professional Contacts"
   {:abilities [{:cost [:click 1] :effect (effect (gain :credit 1) (draw))
                 :msg "gain 1 [Credits] and draw 1 card"}]}

   "Public Sympathy"
   {:effect (effect (gain :max-hand-size 2)) :leave-play (effect (lose :max-hand-size 2))}

   "Rachel Beckman"
   {:effect (req (gain state :runner :click 1 :click-per-turn 1)
                 (add-watch state :rachel-beckman
                            (fn [k ref old new]
                              (when (> (get-in new [:runner :tag]) 0)
                                (remove-watch state :rachel-beckman)
                                (trash ref :runner card)
                                (system-msg ref side "trashes Rachel Beckman for being tagged")))))
    :leave-play (effect (lose :click 1 :click-per-turn 1))}

   "Raymond Flint"
   {:effect (req (add-watch state :raymond-flint
                            (fn [k ref old new]
                              (when (< (get-in old [:corp :bad-publicity]) (get-in new [:corp :bad-publicity]))
                                (resolve-ability
                                 ref side
                                 {:msg "access 1 card from HQ"
                                  :effect (req (doseq [c (take (get-in @state [:runner :hq-access]) (shuffle (:hand corp)))]
                                                 (system-msg state side (str "accesses " (:title c)))
                                                 (handle-access state side [c])))} card nil)))))
    :leave-play (req (remove-watch state :raymond-flint))
    :abilities [{:label "Expose 1 card"
                 :effect (effect (resolve-ability
                                   {:choices {:req #(= (first (:zone %)) :servers)}
                                    :effect (effect (expose target) (trash card {:cause :ability-cost}))
                                    :msg (msg "expose " (:title target))} card nil))}]}

   "Sacrificial Clone"
   {:prevent {:damage [:meat :net :brain]}
    :abilities [{:effect (req (doseq [c (concat (get-in runner [:rig :hardware])
                                                (filter #(not (has? % :subtype "Virtual"))
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

   "Same Old Thing"
   {:abilities [{:cost [:click 2]
                 :prompt "Choose an event to play" :msg (msg "play " (:title target))
                 :choices (req (filter #(and (has? % :type "Event")
                                             (<= (:cost %) (:credit runner))) (:discard runner)))
                 :effect (effect (trash card {:cause :ability-cost}) (play-instant target))}]}

   "Scrubber"
   {:recurring 2}

   "Security Testing"
   {:events {:runner-turn-begins
             {:prompt "Choose a server for Security Testing" :choices (req servers)
              :msg (msg "target " target)
              :effect (effect (update! (assoc card :testing-target (vec (next (server->zone state target))))))}
             :successful-run
             {:req (req (= (get-in @state [:run :server]) (get (get-card state card) :testing-target)))
              :once :per-turn
              :effect (req (let [st card]
                             (swap! state assoc-in [:run :run-effect :replace-access]
                                    {:mandatory true
                                     :effect (effect (resolve-ability
                                                      {:msg "gain 2 [Credits] instead of accessing"
                                                       :effect (effect (gain :credit 2))} st nil))})))}}}

   "Spoilers"
   {:events {:agenda-scored {:msg "trash the top card of R&D" :effect (effect (mill :corp))}}}

   "Starlight Crusade Funding"
   {:events {:runner-turn-begins {:msg "lose [Click]" :effect (effect (lose :click 1))}}}

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
                 :choices (req (filter #(and (not= (:type %) "Event")
                                             (<= (:cost %) (inc (:credit runner))))
                                       (:hosted card)))
                 :msg (msg "install " (:title target) " lowering its install cost by 1 [Credits]")
                 :effect (effect
                           (install-cost-bonus -1) (runner-install (dissoc target :facedown))
                           (trash (update-in card [:hosted]
                                             (fn [coll]
                                               (remove-once #(not= (:cid %) (:cid target)) coll)))
                                  {:cause :ability-cost}))}]}

   "Symmetrical Visage"
   {:events {:runner-click-draw {:req (req (first-event state side :runner-click-draw))
                                 :msg "gain 1 [Credits]"
                                 :effect (effect (gain :credit 1))}}}

   "Synthetic Blood"
   {:events {:damage {:req (req (first-event state side :damage)) :msg "draw 1 card"
                      :effect (effect (draw :runner))}}}

   "Tallie Perrault"
   {:abilities [{:label "Draw 1 card for each Corp bad publicity"
                 :effect (effect (trash card {:cause :ability-cost}) (draw (:bad-publicity corp)))
                 :msg (msg "draw " (:bad-publicity corp) " cards")}]
    :events {:play-operation {:msg "give the Corp 1 bad publicity and take 1 tag"
                              :effect (effect (gain :bad-publicity 1) (gain :runner :tag 1))
                              :req (req (or (has? target :subtype "Black Ops")
                                            (has? target :subtype "Gray Ops")))}}}
   "The Helpful AI"
   {:effect (effect (gain :link 1)) :leave-play (effect (lose :link 1))
    :abilities [{:msg (msg "give +2 strength to " (:title target))
                 :choices {:req #(and (has? % :subtype "Icebreaker") (:installed %))}
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
   {:abilities [{:label "Host a resource or piece of hardware" :cost [:click 1]
                 :prompt "Choose a card to host on The Supplier"
                 :choices (req (filter #(#{"Resource" "Hardware"} (:type %)) (:hand runner)))
                 :effect (effect (host card target)) :msg (msg "host " (:title target) "")}]
    :events {:runner-turn-begins
             {:prompt "Choose a card on The Supplier to install"
              :choices (req (let [hosted (filter #(<= (- (or (:cost %) 0) 2) (:credit runner)) (:hosted card))]
                              (if (empty? hosted)
                                hosted (conj hosted "No install"))))
              :req (req (not (string? target)))
              :msg (msg "install " (:title target) " lowering its install cost by 2")
              :effect (effect (gain :credit (min 2 (:cost target))) (runner-install target))}}}

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
                                  (swap! ref assoc-in [:runner :max-hand-size] credit))))))
    :leave-play (req (remove-watch state :theophilius-bagbiter))}

   "Tri-maf Contact"
   {:abilities [{:cost [:click 1] :msg "gain 2 [Credits]" :once :per-turn
                 :effect (effect (gain :credit 2))}]
    :leave-play (effect (damage :meat 3 {:unboostable true :card card}))}

   "Tyson Observatory"
   {:abilities [{:prompt "Choose a piece of Hardware" :msg (msg "adds " (:title target) " to his Grip")
                 :choices (req (filter #(has? % :type "Hardware") (:deck runner)))
                 :cost [:click 2] :effect (effect (move target :hand) (shuffle! :deck))}]}

   "Underworld Contact"
   {:events {:runner-turn-begins {:msg "gain 1 [Credits]" :req (req (>= (:link runner) 2))
                                  :effect (effect (gain :credit 1))}}}

   "Utopia Shard"
   {:abilities [{:effect (effect (trash-cards :corp (take 2 (shuffle (:hand corp))))
                                 (trash card {:cause :ability-cost}))
                 :msg "force the Corp to discard 2 cards from HQ at random"}]}

   "Virus Breeding Ground"
   {:data {:counter-type "Virus"}
    :events {:runner-turn-begins {:effect (effect (add-prop card :counter 1))}}
    :abilities [{:cost [:click 1] :counter-cost 1 :msg (msg "move 1 virus counter to " (:title target))
                 :choices {:req #(and (has? % :subtype "Virus") (>= (:counter %) 1))}
                 :effect (effect (add-prop target :counter 1))}]}

   "Woman in the Red Dress"
   {:events {:runner-turn-begins
             {:msg (msg "reveal " (:title (first (:deck corp))) " on the top of R&D")
              :optional {:prompt (msg "Draw " (:title (first (:deck corp))) "?")
                         :msg (msg "draw " (:title (first (:deck corp))))
                         :no-msg "doesn't draw with Woman in the Red Dress"
                         :player :corp :effect (effect (draw))}}}}

   "Wyldside"
   {:events {:runner-turn-begins {:msg "draw 2 cards and lose [Click]"
                                  :effect (effect (lose :click 1) (draw 2))}}}

   "Xanadu"
   {:events {:pre-rez {:req (req (= (:type target) "ICE"))
                       :effect (effect (rez-cost-bonus 1))}}}

   "Zona Sul Shipping"
   {:events {:runner-turn-begins {:effect (effect (add-prop card :counter 1))}}
    :abilities [{:cost [:click 1] :msg (msg "gain " (:counter card) " [Credits]")
                 :label "Take all credits"
                 :effect (effect (gain :credit (:counter card)) (set-prop card :counter 0))}]
    :effect (req (add-watch state (keyword (str "zona-sul-shipping" (:cid card)))
                            (fn [k ref old new]
                              (when (> (get-in new [:runner :tag]) 0)
                                (remove-watch state (keyword (str "zona-sul-shipping" (:cid card))))
                                (trash ref :runner card)
                                (system-msg ref side "trash Zona Sul Shipping for being tagged")))))}})
