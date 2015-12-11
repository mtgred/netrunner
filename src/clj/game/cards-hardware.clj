(in-ns 'game.core)

(def cards-hardware
  {"Akamatsu Mem Chip"
   {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))}

   "Archives Interface"
   {:events {:no-action {:effect (req (system-msg state :runner (str "may remove 1 card in Archives from the game instead of"
                                                                     " accessing it by clicking on Archives Interface"))
                                      (update! state side (assoc card :ai-active true)))
                         :req (req (and run (= [:archives] (:server run)) (not current-ice)))}}
    :abilities [{:req (req (and run (= [:archives] (:server run)) (not current-ice) (:ai-active card)))
                 :effect (req (swap! state update-in [:corp :discard] #(map (fn [c] (assoc c :seen true)) %))
                              (resolve-ability
                                state side
                                {:prompt "Choose a card in Archives to remove from the game instead of accessing"
                                 :choices (req (:discard corp))
                                 :msg (msg "remove " (:title target) " from the game")
                                 :effect (req (move state :corp target :rfg)
                                              (update! state side (dissoc (get-card state card) :ai-active)))}
                               card nil))}]}

   "Astrolabe"
   {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))
    :events {:server-created {:msg "draw 1 card" :effect (effect (draw :runner))}}}

   "Autoscripter"
   {:events {:runner-install {:req (req (and (has? target :type "Program")
                                             (= (:active-player @state) :runner)
                                             ;; only trigger when played a programm from grip
                                             (some #{:hand} (:previous-zone target))
                                             ;; check if didn't played a program from the grip this turn
                                             (empty? (let [cards (map first (turn-events state side :runner-install))
                                                           progs (filter #(has? % :type "Program") cards)]
                                                          (filter #(some #{:hand} (:previous-zone %)) progs)))))
                              :msg "gain [Click]" :effect (effect (gain :click 1))}
             :unsuccessful-run {:effect (effect (trash card)
                                                (system-msg "trashes Autoscripter"))}}}

   "Blackguard"
   {:effect (effect (gain :memory 2)) :leave-play (effect (lose :memory 2))
    :events {:expose {:msg (msg "attempt to force the rez of " (:title target))
                      :effect (effect (rez :corp target))}}}

   "Bookmark"
   {:abilities [{:label "Host up to 3 cards from your Grip facedown"
                 :cost [:click 1] :msg "host up to 3 cards from their Grip facedown"
                 :choices {:max 3 :req #(and (:side % "Runner") (= (:zone %) [:hand]))}
                 :effect (req (doseq [c targets]
                                 (host state side (get-card state card) c {:facedown true})))}
                {:label "Add all hosted cards to Grip" :cost [:click 1] :msg "add all hosted cards to their Grip"
                 :effect (req (doseq [c (:hosted card)]
                                (move state side c :hand)))}
                {:label "[Trash]: Add all hosted cards to Grip" :msg "add all hosted cards to their Grip"
                 :effect (req (doseq [c (:hosted card)]
                                (move state side c :hand))
                              (update! state side (dissoc card :hosted))
                              (trash state side (get-card state card) {:cause :ability-cost}))}]}

   "Box-E"
   {:effect (effect (gain :memory 2 :max-hand-size 2))
    :leave-play (effect (lose :memory 2 :max-hand-size 2))}

   "Brain Cage"
   {:effect (effect (damage :brain 1 {:card card}) (gain :max-hand-size 3))
    :leave-play (effect (lose :max-hand-size 3))}

   "Brain Chip"
   (let [runner-points (fn [s] (or (get-in s [:runner :agenda-point]) 0))]
     {:effect (req (gain state :runner :memory (:agenda-point runner) :max-hand-size (:agenda-point runner))
                   (add-watch state (keyword (str "brainchip" (:cid card)))
                          (fn [k ref old new]
                            (let [bonus (- (runner-points new) (runner-points old))]
                              (when (not= 0 bonus)
                               (gain state :runner :memory bonus :max-hand-size bonus))))))
      :leave-play (req (remove-watch state (keyword (str "brainchip" (:cid card))))
                       (lose state :runner :memory (runner-points @state) :max-hand-size (runner-points @state)))})

   "Capstone"
   {:abilities [{:req (req (> (count (:hand runner)) 0))
                 :cost [:click 1]
                 :effect (req (let [handsize (count (:hand runner))]
                                (resolve-ability state side
                                  {:prompt "Choose any number of cards to trash from your Grip"
                                   :choices {:max handsize :req #(and (= (:side %) "Runner") (= (:zone %) [:hand]))}
                                   :effect (req (let [trashed (count targets)
                                                      remaining (- handsize trashed)]
                                                  (doseq [c targets]
                                                    (when (not (empty? (filter #(= (:title c) (:title %))
                                                                               (all-installed state :runner))))
                                                      (draw state side)))
                                                  (trash-cards state side targets)
                                                  (system-msg state side
                                                    (str "spends [Click] to use Capstone to trash "
                                                      (join ", " (map :title targets)) " and draw "
                                                      (- (count (get-in @state [:runner :hand])) remaining) " cards"))))}
                                 card nil)))}]}

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
   {:abilities [{:prompt "Choose a program to install from your Heap" :msg (msg "install " (:title target))
                 :priority true :show-discard true
                 :req (req (not (seq (get-in @state [:runner :locked :discard]))))
                 :choices {:req #(and (= (:type %) "Program") (= (:zone %) [:discard]))}
                 :effect (effect (trash card {:cause :ability-cost}) (runner-install target))}]}

   "Comet"
   {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))
    :events {:play-event {:req (req (first-event state side :play-event))
                          :effect (req (system-msg state :runner
                                                   (str "can play another event without spending a [Click] by clicking on Comet"))
                                       (update! state side (assoc card :comet-event true)))}}
    :abilities [{:req (req (:comet-event card))
                 :prompt "Choose an Event in your Grip to play"
                 :choices {:req #(and (= (:type %) "Event") (= (:zone %) [:hand]))}
                 :msg (msg "play " (:title target))
                 :effect (effect (play-instant target)
                                 (update! (dissoc (get-card state card) :comet-event)))}]}

   "Cortez Chip"
   {:abilities [{:prompt "Choose a piece of ICE" :choices {:req #(and (not (:rezzed %)) (= (:type %) "ICE"))}
                 :effect (req (let [ice target
                                    serv (zone->name (second (:zone ice)))]
                                (update! state side (assoc card :cortez-target ice))
                                (trash state side (get-card state card) {:cause :ability-cost})
                                (system-msg state side
                                  (str "increases the cost to rez the ICE at position "
                                    (ice-index state ice) " of " serv " by 2 [Credits] until the end of the turn"))))}]
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
                 :prompt "Choose a non-AI icebreaker in your Grip to install on Dinosaurus"
                 :choices {:req #(and (has? % :subtype "Icebreaker")
                                      (not (has? % :subtype "AI"))
                                      (= (:zone %) [:hand]))}
                 :effect (effect (gain :memory (:memoryunits target))
                                 (runner-install target {:host-card card})
                                 (update! (assoc (get-card state card) :dino-breaker (:cid target)))
                                 (update-breaker-strength target))}
                {:label "Host an installed non-AI icebreaker on Dinosaurus"
                 :req (req (empty? (:hosted card)))
                 :prompt "Choose an installed non-AI icebreaker to host on Dinosaurus"
                 :choices {:req #(and (has? % :subtype "Icebreaker")
                                      (not (has? % :subtype "AI"))
                                      (:installed %))}
                 :msg (msg "host " (:title target))
                 :effect (effect (host card target)
                                 (update! (assoc (get-card state card) :dino-breaker (:cid target)))
                                 (gain :memory (:memoryunits target)))}]
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (first (:hosted card)))))
                                    :effect (effect (breaker-strength-bonus 2))}
             :card-moved {:req (req (= (:cid target) (:dino-breaker (get-card state card))))
                          :effect (effect (update! (dissoc card :dino-breaker))
                                          (lose :memory (:memoryunits target)))}}}

   "Doppelgänger"
   {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))
    :events {:runner-install
             {:req (req (= card target))
              :effect (effect (update! (assoc card :dopp-active true)))}
             :runner-turn-begins
             {:effect (effect (update! (assoc card :dopp-active true)))}
             :successful-run-ends
             {:optional
              {:req (req (:dopp-active card))
               :prompt "Use Doppelgänger to run again?" :player :runner
               :yes-ability {:prompt "Choose a server"
                             :choices (req servers)
                             :msg (msg "make a run on " target)
                             :effect (effect (update! (dissoc card :dopp-active)) (run target))}}}}}

   "Dorm Computer"
   {:data {:counter 4}
    :abilities [{:counter-cost 1 :cost [:click 1]
                 :req (req (not run))
                 :prompt "Choose a server" :choices (req servers)
                 :msg "make a run and avoid all tags for the remainder of the run"
                 :effect (effect (update! (assoc card :dorm-active true))
                                 (run target))}]
    :events {:pre-tag {:req (req (:dorm-active card))
                       :effect (effect (tag-prevent Integer/MAX_VALUE))
                       :msg "avoid all tags during the run"}
             :run-ends {:effect (effect (update! (dissoc card :dorm-active)))}}}

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
   {:prevent {:tag [:all]}
    :effect (effect (gain :link 1)) :leave-play (effect (lose :link 1))
    :abilities [{:msg "avoid 1 tag" :label "[Trash]: Avoid 1 tag"
                 :effect (effect (tag-prevent 1) (trash card {:cause :ability-cost}))}
                {:msg "remove 1 tag" :label "[Trash]: Remove 1 tag"
                 :effect (effect (trash card {:cause :ability-cost}) (lose :tag 1))}]}

   "Grimoire"
   {:effect (effect (gain :memory 2)) :leave-play (effect (lose :memory 2))
    :events {:runner-install {:req (req (has? target :subtype "Virus"))
                              :effect (effect (add-prop target :counter 1))}}}

   "Heartbeat"
   {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))
    :prevent {:damage [:meat :net :brain]}
    :abilities [{:msg "prevent 1 damage"
                 :choices {:req #(and (= (:side %) "Runner") (:installed %))}
                 :priority true
                 :effect (effect (trash target {:cause :ability-cost})
                                 (damage-prevent :brain 1)
                                 (damage-prevent :meat 1)
                                 (damage-prevent :net 1))}]}

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
   (let [mhelper (fn mh [n] {:prompt "Choose a program to install"
                             :choices {:req #(and (= (:type %) "Program") (= (:zone %) [:hand]))}
                             :effect (req (install-cost-bonus state side [:credit -4])
                                          (runner-install state side target nil)
                                            (when (< n 3)
                                              (resolve-ability state side (mh (inc n)) card nil)))})]
     {:prevent {:damage [:net :brain]}
      :effect (effect (gain :memory 3)
                      (resolve-ability (mhelper 1) card nil))
      :leave-play (effect (lose :memory 3))
      :abilities [{:msg (msg "prevent 1 brain or net damage by trashing " (:title target))
                   :priority true
                   :choices {:req #(and (= (:type %) "Program") (= [:hand] (:zone %)))}
                   :prompt "Choose a program to trash from your grip" :effect (effect (trash target)
                                                                       (damage-prevent :brain 1)
                                                                       (damage-prevent :net 1))}]})

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
                 :prompt "Choose a program of 1[Memory Unit] or less to install on Omni-Drive from your grip"
                 :choices {:req #(and (= (:type %) "Program")
                                      (<= (:memoryunits %) 1)
                                      (= [:hand] (:zone %)))}
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
                              (when (= (:counter card) 0) (trash state side card {:unpreventable true})))}]}

   "Prepaid VoicePAD"
   {:recurring 1}

   "Public Terminal"
   {:recurring 1}

   "Q-Coherence Chip"
   {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))
    :events (let [e {:msg "trash itself" :req (req (= (last (:zone target)) :program))
                     :effect (effect (trash card))}]
              {:runner-trash e :corp-trash e})}

   "Qianju PT"
   {:events {:runner-turn-begins
             {:optional {:prompt "Lose [Click] to avoid the first tag until next turn?"
                         :yes-ability {:effect (effect (lose :click 1)
                                                       (update! (assoc card :qianju-active true)))
                                       :msg "avoid the first tag received until his/her next turn"}
                         :no-ability {:effect (effect (update! (dissoc card :qianju-active)))}}}
             :pre-tag {:req (req (:qianju-active card))
                       :msg "to avoid the first tag received"
                       :effect (effect (tag-prevent 1) (update! (dissoc card :qianju-active)))}}}

   "R&D Interface"
   {:effect (effect (gain :rd-access 1)) :leave-play (effect (lose :rd-access 1))}

   "Rabbit Hole"
   {:effect
    (effect (gain :link 1)
            (resolve-ability
             {:optional {:req (req (some #(when (= (:title %) "Rabbit Hole") %) (:deck runner)))
                         :prompt "Install another Rabbit Hole?" :msg "install another Rabbit Hole"
                         :yes-ability {:effect (req (when-let [c (some #(when (= (:title %) "Rabbit Hole") %)
                                                                      (:deck runner))]
                                                     (runner-install state side c)
                                                     (shuffle! state :runner :deck)))}}} card nil))
    :leave-play (effect (lose :link 1))}

   "Record Reconstructor"
   {:events
    {:successful-run
     {:req (req (= (get-in @state [:run :server]) [:archives]))
      :effect (req (let [rr card]
                     (swap! state assoc-in [:run :run-effect :replace-access]
                       {:effect (effect (resolve-ability
                                          {:prompt "Choose one faceup card to add to the top of R&D"
                                           :choices (req (filter #(:seen %) (:discard corp)))
                                           :msg (msg "add " (:title target) " to the top of R&D")
                                           :effect (req (move state :corp target :deck {:front true}))}
                                         rr nil))})))}}}

   "Replicator"
   {:events {:runner-install
             {:optional {:req (req (= (:type target) "Hardware"))
                         :prompt "Use Replicator to add a copy?"
                         :yes-ability {:msg (msg "add a copy of " (:title target) " to their Grip")
                                       :effect (effect (move (some #(when (= (:title %) (:title target)) %)
                                                                  (:deck runner)) :hand)
                                                      (shuffle! :deck))}}}}}

   "Security Chip"
   {:abilities [{:label "[Trash]: Add [Link] strength to a non-Cloud icebreaker until the end of the run"
                 :msg (msg "add " (:link runner) " strength to " (:title target) " until the end of the run")
                 :req (req (:run @state))
                 :prompt "Choose one non-Cloud icebreaker"
                 :choices {:req #(and (has? % :subtype "Icebreaker")
                                      (not (has? % :subtype "Cloud"))
                                      (:installed %))}
                 :effect (effect (pump target (:link runner) :all-run)
                                 (trash (get-card state card) {:cause :ability-cost}))}
                {:label "[Trash]: Add [Link] strength to any Cloud icebreakers until the end of the run"
                 :msg (msg "add " (:link runner) " strength to " (count targets) " Cloud icebreakers until the end of the run")
                 :req (req (:run @state))
                 :prompt "Choose any number of Cloud icebreakers"
                 :choices {:max 50 :req #(and (has? % :subtype "Icebreaker")
                                              (has? % :subtype "Cloud")
                                              (:installed %))}
                 :effect (req (doseq [t targets]
                                (pump state side t (:link runner) :all-run)
                                (update-breaker-strength state side t))
                              (trash state side (get-card state card) {:cause :ability-cost}))}]}

   "Security Nexus"
   {:effect (effect (gain :link 1) (gain :memory 1))
    :leave-play (effect (lose :link 1) (lose :memory 1))
    :abilities [{:req (req (:run @state))
                 :msg "force the Corp to initiate a trace"
                 :label "Trace 5 - Give the Runner 1 tag and end the run"
                 :trace {:once :per-turn :base 5 :msg "give the Runner 1 tag and end the run"
                         :effect (effect (tag-runner :runner 1) (end-run))
                         :unsuccessful {:msg "bypass the current ICE"}}}]}

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
   {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))
    :events {:agenda-stolen {:effect (req (system-msg state :runner (str "can swap for a scored Corp agenda by clicking on Turntable"))
                                          (update! state side (assoc card :swap true)))}}
    :abilities [{:optional
                  {:req (req (:swap card))
                   :prompt "Swap for a scored Corp agenda?"
                   :yes-ability
                   {:effect (req (let [agendas (get-in corp [:scored])]
                                   (resolve-ability
                                     state side
                                     {:prompt "Choose a scored Corp agenda"
                                      :choices {:req #(some (fn [c] (= (:cid %) (:cid c))) agendas)}
                                      :effect (req (let [st (last (get-in runner [:scored]))
                                                         sw target
                                                         stpts-corp (get-agenda-points state :corp st)
                                                         swpts-corp (get-agenda-points state :corp sw)
                                                         stpts-runner (get-agenda-points state :runner st)
                                                         swpts-runner (get-agenda-points state :runner sw)]
                                                     (swap! state update-in [:corp :scored]
                                                       (fn [coll] (conj (remove-once #(not= (:cid %) (:cid sw)) coll) st)))
                                                     (swap! state update-in [:runner :scored]
                                                       (fn [coll] (conj (remove-once #(not= (:cid %) (:cid st)) coll)
                                                                        (dissoc sw :abilities :events))))
                                                     (gain-agenda-point state :runner (- swpts-runner stpts-runner))
                                                     (gain-agenda-point state :corp (- stpts-corp swpts-corp))
                                                     (doseq [c (get-in @state [:corp :scored])]
                                                       (let [abilities (:abilities (card-def c))
                                                             c (merge c {:abilities abilities})]
                                                         (update! state :corp c)
                                                         (when-let [events (:events (card-def c))]
                                                           (register-events state side events c))))
                                                     (doseq [r (get-in @state [:runner :scored])]
                                                       (desactivate state :corp r))
                                                     (system-msg state side
                                                       (str "uses Turntable to swap " (:title st) " for " (:title sw)))
                                                     (update! state side (dissoc (get-card state card) :swap))))}
                                    card nil)))}}}]}

   "Unregistered S&W 35"
   {:abilities
    [{:cost [:click 2] :req (req (some #{:hq} (:successful-run runner-reg)))
      :label "trash a Bioroid, Clone, Executive or Sysop" :prompt "Choose a Bioroid, Clone, Executive, or Sysop to trash"
      :choices {:req #(and (:rezzed %)
                           (or (has? % :subtype "Bioroid") (has? % :subtype "Clone")
                               (has? % :subtype "Executive") (has? % :subtype "Sysop"))
                           (or (= (last (:zone %)) :content) (= (last (:zone %)) :onhost)))}
      :msg (msg "trash " (:title target)) :effect (effect (trash target))}]}

   "Vigil"
   {:effect (effect (gain :memory 1)) :leave-play (effect (lose :memory 1))
    :events {:runner-turn-begins {:req (req (= (count (:hand corp)) (:max-hand-size corp)))
                                  :msg "draw 1 card" :effect (effect (draw 1))}}}

   "Window"
   {:abilities [{:cost [:click 1] :msg "draw 1 card from the bottom of their Stack"
                 :effect (effect (move (last (:deck runner)) :hand))}]}})
