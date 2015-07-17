(in-ns 'game.core)

(def cards-hardware
  {"Akamatsu Mem Chip"
   {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))}

   "Astrolabe"
   {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))
    :events {:server-created {:msg "draw 1 card" :effect (effect (draw :runner))}}}

   "Autoscripter"
   {:events {:runner-install {:req (req (and (has? target :type "Program")
                                             (= (:active-player @state) :runner)
                                             (empty? (filter #(has? % :type "Program")
                                                             (map first (turn-events state side :runner-install))))))
                              :msg "gain [Click]" :effect (effect (gain :click 1))}
             :unsuccessful-run {:effect (effect (trash card)
                                                (system-msg "trashes Autoscripter"))}}}

   "Blackguard"
   {:effect (effect (gain :memory 2)) :leave-play (effect (lose :memory 2))
    :events {:expose {:msg (msg "attempt to force the rez of " (:title target))
                      :effect (effect (rez :corp target))}}}

   "Box-E"
   {:effect (effect (gain :memory 2 :max-hand-size 2))
    :leave-play (effect (lose :memory 2 :max-hand-size 2))}

   "Brain Cage"
   {:effect (effect (damage :brain 1 {:card card}) (gain :max-hand-size 3))
    :leave-play (effect (lose :max-hand-size 3))}

   "Chop Bot 3000"
   {:abilities [{:msg (msg "trash " (:title target))
                 :choices {:req #(and (= (:side %) "Runner") (:installed %))}
                 :effect (effect (trash target)
                                 (resolve-ability
                                   {:prompt "Draw 1 card or remove 1 tag" :msg (msg (.toLowerCase target))
                                    :choices ["Draw 1 card" "Remove 1 tag"]
                                    :effect (req (if (= target "Draw 1 card")
                                                   (draw state side)
                                                   (lose state side :tag 1)))} card nil))}]}

   "Clone Chip"
   {:abilities [{:prompt "Choose a program to install" :msg (msg "install " (:title target))
                 :priority true
                 :choices (req (filter #(has? % :type "Program") (:discard runner)))
                 :effect (effect (trash card {:cause :ability-cost}) (runner-install target))}]}

   "Comet"
   {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))
    :events {:play-event
             {:optional {:prompt "Play another event?" :once :per-turn
                         :effect (effect (resolve-ability
                                           {:prompt "Choose an Event to play"
                                            :choices (req (filter #(has? % :type "Event") (:hand runner)))
                                            :msg (msg "play " (:title target))
                                            :effect (effect (play-instant target))} card nil))}}}}

   "Cortez Chip"
   {:abilities [{:label "increase cost to rez a piece of ice by 2 [Credits]"
                 :prompt "Choose a piece of ice" :choices {:req #(and (not (:rezzed %)) (= (:type %) "ICE"))}
                 :effect (effect (update! (assoc card :cortez-target target))
                                 (trash (get-card state card) {:cause :ability-cost}))}]
    :trash-effect {:effect (effect (register-events {:pre-rez {:req (req (= (:cid target) (:cid (:cortez-target card))))
                                                               :effect (effect (rez-cost-bonus 2))}
                                                     :runner-turn-ends {:effect (effect (unregister-events card))}
                                                     :corp-turn-ends {:effect (effect (unregister-events card))}}
                                                    (get-card state card)))}
    :events {:pre-rez nil :runner-turn-ends nil :corp-turn-ends nil}}

   "Cyberfeeder"
   {:recurring 1}

   "CyberSolutions Mem Chip"
   {:effect (effect (gain :memory 2)) :leave-play (effect (lose :memory 2))}

   "Cybsoft MacroDrive"
   {:recurring 1}

   "Deep Red"
   {:effect (effect (gain :memory 3)) :leave-play (effect (lose :memory 3))}

   "Desperado"
   {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))
    :events {:successful-run {:msg "gain 1 [Credits]" :effect (effect (gain :credit 1))}}}

   "Dinosaurus"
   {:abilities [{:label "Install a non-AI icebreaker on Dinosaurus"
                 :req (req (empty? (:hosted card))) :cost [:click 1]
                 :prompt "Choose a non-AI icebreaker to install on Dinosaurus"
                 :choices (req (filter #(and (has? % :subtype "Icebreaker")
                                             (not (has? % :subtype "AI")))
                                       (:hand runner)))
                 :msg (msg "host " (:title target))
                 :effect (effect (gain :memory (:memoryunits target))
                                 (runner-install target {:host-card card})
                                 (update-breaker-strength target))}
                {:label "Host an installed non-AI icebreaker on Dinosaurus"
                 :req (req (empty? (:hosted card)))
                 :prompt "Choose a program to host on Dinosaurus"
                 :choices {:req #(and (has? % :subtype "Icebreaker")
                                      (not (has? % :subtype "AI"))
                                      (:installed %))}
                 :msg (msg "host " (:title target)) :effect (effect (host card target))}]
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (first (:hosted card)))))
                                    :effect (effect (breaker-strength-bonus 2))}}}

   "Doppelgänger"
   {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))
    :events {:successful-run-ends
             {:optional
              {:once :per-turn :prompt "Use Doppelgänger to run again?" :player :runner
               :effect (effect (resolve-ability {:prompt "Choose a server" :choices (req servers)
                                                 :msg (msg "to make a run on " target)
                                                 :effect (effect (run target))} card targets))}}}}

   "Dorm Computer"
   {:data {:counter 4}
    :abilities [{:counter-cost 1 :cost [:click 1]
                 :prompt "Choose a server" :choices (req servers)
                 :msg "make a run and avoid all tags for the remainder of the run"
                 :effect (effect (run target))}]}

   "Dyson Fractal Generator"
   {:recurring 1}

   "Dyson Mem Chip"
   {:effect (effect (gain :link 1 :memory 1)) :leave-play (effect (lose :link 1 :memory 1))}

   "e3 Feedback Implants"
   {:abilities [{:cost [:credit 1] :msg "break 1 additional subroutine"}]}

   "Ekomind"
   {:effect (req (swap! state assoc-in [:runner :memory] (count (get-in @state [:runner :hand])))
                 (add-watch state :ekomind (fn [k ref old new]
                                             (let [hand-size (count (get-in new [:runner :hand]))]
                                               (when (not= (count (get-in old [:runner :hand])) hand-size)
                                                 (swap! ref assoc-in [:runner :memory] hand-size))))))
    :leave-play (req (remove-watch state :ekomind))}

   "Feedback Filter"
   {:prevent {:damage [:net :brain]}
    :abilities [{:cost [:credit 3] :msg "prevent 1 net damage" :effect (effect (damage-prevent :net 1))}
                {:msg "prevent 2 brain damage" :effect (effect (trash card {:cause :ability-cost})
                                                               (damage-prevent :brain 2)) }]}

   "Forger"
   {:effect (effect (gain :link 1)) :leave-play (effect (lose :link 1))
    :abilities [{:msg "remove 1 tag"
                 :effect (effect (trash card {:cause :ability-cost}) (lose :tag 1))}]}

   "HQ Interface"
   {:effect (effect (gain :hq-access 1)) :leave-play (effect (lose :hq-access 1))}

   "Lemuria Codecracker"
   {:abilities [{:cost [:click 1 :credit 1] :req (req (some #{:hq} (:successful-run runner-reg)))
                 :choices {:req #(= (first (:zone %)) :servers)} :effect (effect (expose target))
                 :msg "expose 1 card"}]}

   "LLDS Processor"
   {:events
             (let [llds {:effect (req (let [cards (:llds-target card)]
                                           (update! state side (dissoc card :llds-target))
                                           (doseq [c cards]
                                             (update-breaker-strength state side c))))}]
               {:runner-turn-ends llds :corp-turn-ends llds
                :runner-install {:req (req (has? target :subtype "Icebreaker"))
                                 :effect (effect (update! (update-in card [:llds-target] #(conj % target)))
                                                 (update-breaker-strength target))}
                :pre-breaker-strength {:req (req (some #(= (:cid target) (:cid %)) (:llds-target card)))
                                       :effect (effect (breaker-strength-bonus 1))}})}

   "Lockpick"
   {:recurring 1}

   "Logos"
   {:effect (effect (gain :memory 1 :max-hand-size 1))
    :leave-play (effect (lose :memory 1 :max-hand-size 1))
    :events {:agenda-scored
             {:player :runner :prompt "Choose a card" :msg (msg "add 1 card to Grip from Stack")
              :choices (req (:deck runner)) :effect (effect (move target :hand) (shuffle! :deck))}}}

   "MemStrips"
   {:effect (effect (gain :memory 3))}

   "Monolith"
   {:prevent {:damage [:net :brain]}
    :effect (effect (gain :memory 3)) :leave-play (effect (lose :memory 3))
    :abilities [{:msg (msg "prevent 1 brain or net damage by trashing " (:title target))
                 :priority true
                 :choices (req (filter #(= (:type %) "Program") (:hand runner)))
                 :prompt "Choose a program to trash" :effect (effect (trash target)
                                                                     (damage-prevent :brain 1)
                                                                     (damage-prevent :net 1))}]}

   "Muresh Bodysuit"
   {:events {:pre-damage {:once :per-turn :once-key :muresh-bodysuit
                          :req (req (= target :meat))
                          :msg "prevent the first meat damage this turn"
                          :effect (effect (damage-prevent :meat 1))}}}

   "Net-Ready Eyes"
   {:effect (effect (damage :meat 2 {:unboostable true :card card})) :msg "suffer 2 meat damage"
    :events {:run {:choices {:req #(and (:installed %) (has? % :subtype "Icebreaker"))}
                   :msg (msg "give " (:title target) " +1 strength")
                   :effect (effect (pump target 1 :all-run))}}}

   "Omni-Drive"
   {:recurring 1
    :abilities [{:label "Install and host a program of 1[Memory Unit] or less on Omni-Drive"
                 :req (req (empty? (:hosted card))) :cost [:click 1]
                 :prompt "Choose a program of 1[Memory Unit] or less to install on Omni-Drive"
                 :choices (req (filter #(and (= (:type %) "Program")
                                             (<= (:memoryunits %) 1)
                                             (<= (:cost %) (:credit runner)))
                                       (:hand runner)))
                 :msg (msg "host " (:title target))
                 :effect (effect (gain :memory (:memoryunits target))
                                 (runner-install target {:host-card card}))}
                {:label "Host an installed program of 1[Memory Unit] or less on Omni-Drive"
                 :prompt "Choose an installed program of 1[Memory Unit] or less to host on Omni-Drive"
                 :choices {:req #(and (= (:type %) "Program") (:installed %))}
                 :msg (msg "host " (:title target)) :effect (effect (host card target))}]}

   "Plascrete Carapace"
   {:data [:counter 4]
    :prevent {:damage [:meat]}
    :abilities [{:counter-cost 1 :msg "prevent 1 meat damage"
                 :effect (req (damage-prevent state side :meat 1)
                              (when (= (:counter card) 0) (trash state side card)))}]}

   "Prepaid VoicePAD"
   {:recurring 1}

   "Public Terminal"
   {:recurring 1}

   "Q-Coherence Chip"
   {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))
    :events {:trash {:msg "trash itself" :req (req (= (last (:zone target)) :program))
                     :effect (effect (trash card))}}}

   "R&D Interface"
   {:effect (effect (gain :rd-access 1)) :leave-play (effect (lose :rd-access 1))}

   "Rabbit Hole"
   {:effect
                (effect (gain :link 1)
                        (resolve-ability
                          {:optional {:req (req (some #(when (= (:title %) "Rabbit Hole") %) (:deck runner)))
                                      :prompt "Install another Rabbit Hole?" :msg "install another Rabbit Hole"
                                      :effect (req (when-let [c (some #(when (= (:title %) "Rabbit Hole") %)
                                                                      (:deck runner))]
                                                     (runner-install state side c)
                                                     (shuffle! state :runner :deck)))}} card nil))
    :leave-play (effect (lose :link 1))}

   "Replicator"
   {:events {:runner-install
             {:optional {:req (req (= (:type target) "Hardware"))
                         :prompt "Use Replicator to add a copy?"
                         :msg (msg "add a copy of " (:title target) " to his Grip")
                         :effect (effect (move (some #(when (= (:title %) (:title target)) %)
                                                     (:deck runner)) :hand)
                                         (shuffle! :deck))}}}}

   "Silencer"
   {:recurring 1}

   "Skulljack"
   {:effect (effect (damage :brain 1 {:card card}))
    :events {:pre-trash {:effect (effect (trash-cost-bonus -1))}}}

   "Spinal Modem"
   {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1)) :recurring 2
    :events {:successful-trace {:req (req run) :effect (effect (damage :brain 1 {:card card}))}}}

   "The Personal Touch"
   {:hosting {:req #(and (has? % :subtype "Icebreaker") (:installed %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}

   "The Toolbox"
   {:effect (effect (gain :link 2 :memory 2)) :leave-play (effect (lose :link 2 :memory 2))
    :recurring 2}

   "Titanium Ribs"
   {:effect (effect (damage :meat 2 {:card card}))}

   "Turntable"
   {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))}

   "Unregistered S&W 35"
   {:abilities
    [{:cost [:click 2] :req (req (some #{:hq} (:successful-run runner-reg)))
      :label "trash a Bioroid, Clone, Executive or Sysop" :prompt "Choose a card to trash"
      :choices (req (filter #(and (:rezzed %)
                                  (or (has? % :subtype "Bioroid") (has? % :subtype "Clone")
                                      (has? % :subtype "Executive") (has? % :subtype "Sysop")))
                            (mapcat :content (flatten (seq (:servers corp))))))
      :msg (msg "trash " (:title target)) :effect (effect (trash target))}]}

   "Vigil"
   {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))
    :events {:runner-turn-begins {:req (req (= (count (:hand corp)) (:max-hand-size corp)))
                                  :msg "draw 1 card" :effect (effect (draw 1))}}}

   "Window"
   {:abilities [{:cost [:click 1] :msg "draw 1 card from the bottom of his Stack"
                 :effect (effect (move (last (:deck runner)) :hand))}]}})