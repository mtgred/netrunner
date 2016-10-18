(in-ns 'game.core)

(def cards-hardware
  {"Akamatsu Mem Chip"
   {:in-play [:memory 1]}

   "Archives Interface"
   {:events
    {:successful-run
     {:silent (req true)
      :delayed-completion true
      :req (req (= target :archives))
      :effect (effect (continue-ability
                        {:optional
                         {:prompt "Use Archives Interface to remove a card from the game instead of accessing it?"
                          :yes-ability
                          {:delayed-completion true
                           :effect (req (swap! state update-in [:corp :discard] #(map (fn [c] (assoc c :seen true)) %))
                                        (continue-ability
                                          state side
                                          {:prompt "Choose a card in Archives to remove from the game instead of accessing"
                                           :choices (req (:discard corp))
                                           :msg (msg "remove " (:title target) " from the game")
                                           :effect (effect (move :corp target :rfg))} card nil))}}} card nil))}}}

   "Astrolabe"
   {:in-play [:memory 1]
    :events {:server-created {:msg "draw 1 card"
                              :effect (effect (draw :runner))}}}

   "Autoscripter"
   {:events {:runner-install {:silent (req true)
                              :req (req (and (is-type? target "Program")
                                             (= (:active-player @state) :runner)
                                             ;; only trigger when played a programm from grip
                                             (some #{:hand} (:previous-zone target))
                                             ;; check if didn't played a program from the grip this turn
                                             (empty? (let [cards (map first (turn-events state side :runner-install))
                                                           progs (filter #(is-type? % "Program") cards)]
                                                          (filter #(some #{:hand} (:previous-zone %)) progs)))))
                              :msg "gain [Click]" :effect (effect (gain :click 1))}
             :unsuccessful-run {:effect (effect (trash card)
                                                (system-msg "trashes Autoscripter"))}}}

   "Blackguard"
   {:in-play [:memory 2]
    :events {:expose {:msg (msg "attempt to force the rez of " (:title target))
                      :effect (effect (rez :corp target))}}}

   "Bookmark"
   {:abilities [{:label "Host up to 3 cards from your Grip facedown"
                 :cost [:click 1] :msg "host up to 3 cards from their Grip facedown"
                 :choices {:max 3
                           :req #(and (= (:side %) "Runner")
                                      (in-hand? %))}
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
   {:in-play [:memory 2 :hand-size-modification 2]}

   "Brain Cage"
   {:in-play [:hand-size-modification 3]
    :effect (effect (damage eid :brain 1 {:card card}))}

   "Brain Chip"
   (let [runner-points (fn [s] (max (or (get-in s [:runner :agenda-point]) 0) 0))]
     {:effect (req (gain state :runner
                         :memory (runner-points @state)
                         :hand-size-modification (runner-points @state))
                   (add-watch state (keyword (str "brainchip" (:cid card)))
                          (fn [k ref old new]
                            (let [bonus (- (runner-points new) (runner-points old))]
                              (when (not= 0 bonus)
                               (gain state :runner
                                     :memory bonus
                                     :hand-size-modification bonus))))))
      :leave-play (req (remove-watch state (keyword (str "brainchip" (:cid card))))
                       (lose state :runner
                             :memory (runner-points @state)
                             :hand-size-modification (runner-points @state)))})

   "Capstone"
   {:abilities [{:req (req (> (count (:hand runner)) 0))
                 :cost [:click 1]
                 :effect (req (let [handsize (count (:hand runner))]
                                (resolve-ability state side
                                  {:prompt "Choose any number of cards to trash from your Grip"
                                   :choices {:max handsize :req #(and (= (:side %) "Runner")
                                                                      (in-hand? %))}
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
   {:flags {:runner-phase-12 (req (>= 2 (count (all-installed state :runner))))}
    :abilities [{:msg (msg "trash " (:title target))
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
                 :choices {:req #(and (is-type? % "Program")
                                      (= (:zone %) [:discard]))}
                 :effect (effect (trash card {:cause :ability-cost}) (runner-install target))}]}

   "Comet"
   {:in-play [:memory 1]
    :events {:play-event {:req (req (first-event state side :play-event))
                          :effect (req (system-msg state :runner
                                                   (str "can play another event without spending a [Click] by clicking on Comet"))
                                       (update! state side (assoc card :comet-event true)))}}
    :abilities [{:req (req (:comet-event card))
                 :prompt "Choose an Event in your Grip to play"
                 :choices {:req #(and (is-type? % "Event")
                                      (in-hand? %))}
                 :msg (msg "play " (:title target))
                 :effect (effect (play-instant target)
                                 (update! (dissoc (get-card state card) :comet-event)))}]}

   "Cortez Chip"
   {:abilities [{:prompt "Choose a piece of ICE"
                 :choices {:req ice?}
                 :effect (req (let [ice target]
                                (update! state side (assoc card :cortez-target ice))
                                (trash state side (get-card state card) {:cause :ability-cost})
                                (system-msg state side
                                  (str "trashes Cortez Chip to increase the rez cost of " (card-str state ice)
                                       " by 2 [Credits] until the end of the turn"))))}]
    :trash-effect {:effect (effect (register-events {:pre-rez {:req (req (= (:cid target) (:cid (:cortez-target card))))
                                                               :effect (effect (rez-cost-bonus 2))}
                                                     :runner-turn-ends {:effect (effect (unregister-events card))}
                                                     :corp-turn-ends {:effect (effect (unregister-events card))}}
                                                    (get-card state card)))}
    :events {:pre-rez nil :runner-turn-ends nil :corp-turn-ends nil}}

   "Cyberfeeder"
   {:recurring 1}

   "CyberSolutions Mem Chip"
   {:in-play [:memory 2]}

   "Cybsoft MacroDrive"
   {:recurring 1}

   "Deep Red"
   {:in-play [:memory 3]
    :events {:runner-install
             {:optional
              {:req (req (has-subtype? target "Caïssa"))
               :delayed-completion true
               :prompt "Use Deep Red to trigger the [Click] ability of the installed Caïssa?"
               :yes-ability {:effect (req (let [cid (:cid (first (map last (turn-events state :runner :pre-install))))
                                                caissa (find-cid cid (all-installed state :runner))]
                                            (continue-ability state side
                                              {:msg (msg "trigger the [Click] ability of " (:title caissa)
                                                         " without spending [Click]")
                                               :effect (effect (gain :click 1)
                                                               (play-ability {:card (get-card state caissa) :ability 0}))}
                                             card nil)))}
               :no-ability {:effect (req (effect-completed state side eid card))}}}}}

   "Desperado"
   {:in-play [:memory 1]
    :events {:successful-run {:silent (req true)
                              :msg "gain 1 [Credits]" :effect (effect (gain :credit 1))}}}

   "Dinosaurus"
   {:abilities [{:label "Install a non-AI icebreaker on Dinosaurus"
                 :req (req (empty? (:hosted card))) :cost [:click 1]
                 :prompt "Choose a non-AI icebreaker in your Grip to install on Dinosaurus"
                 :choices {:req #(and (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "AI"))
                                      (in-hand? %))}
                 :effect (effect (gain :memory (:memoryunits target))
                                 (runner-install target {:host-card card})
                                 (update! (assoc (get-card state card) :dino-breaker (:cid target))))}
                {:label "Host an installed non-AI icebreaker on Dinosaurus"
                 :req (req (empty? (:hosted card)))
                 :prompt "Choose an installed non-AI icebreaker to host on Dinosaurus"
                 :choices {:req #(and (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "AI"))
                                      (installed? %))}
                 :msg (msg "host " (:title target))
                 :effect (req (update-breaker-strength state side (host state side card target))
                              (update! state side (assoc (get-card state card) :dino-breaker (:cid target)))
                              (gain state side :memory (:memoryunits target)))}]
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (first (:hosted card)))))
                                    :effect (effect (breaker-strength-bonus 2))}
             :card-moved {:req (req (= (:cid target) (:dino-breaker (get-card state card))))
                          :effect (effect (update! (dissoc card :dino-breaker))
                                          (lose :memory (:memoryunits target)))}}}

   "Doppelgänger"
   {:in-play [:memory 1]
    :events {:runner-install
             {:req (req (= card target))
              :silent (req true)
              :effect (effect (update! (assoc card :dopp-active true)))}
             :runner-turn-begins
             {:effect (effect (update! (assoc card :dopp-active true)))}
             :successful-run-ends
             {:optional
              {:req (req (:dopp-active card))
               :player :runner
               :prompt "Use Doppelgänger to run again?"
               :yes-ability {:prompt "Choose a server"
                             :choices (req runnable-servers)
                             :msg (msg "make a run on " target)
                             :makes-run true
                             :effect (effect (update! (dissoc card :dopp-active))
                                             (run target))}}}}}

   "Dorm Computer"
   {:data {:counter {:power 4}}
    :abilities [{:counter-cost [:power 1]
                 :cost [:click 1]
                 :req (req (not run))
                 :prompt "Choose a server"
                 :choices (req runnable-servers)
                 :msg "make a run and avoid all tags for the remainder of the run"
                 :makes-run true
                 :effect (effect (update! (assoc card :dorm-active true))
                                 (run target))}]
    :events {:pre-tag {:req (req (:dorm-active card))
                       :effect (effect (tag-prevent Integer/MAX_VALUE))
                       :msg "avoid all tags during the run"}
             :run-ends {:effect (effect (update! (dissoc card :dorm-active)))}}}

   "Dyson Fractal Generator"
   {:recurring 1}

   "Dyson Mem Chip"
   {:in-play [:memory 1 :link 1]}

   "e3 Feedback Implants"
   {:abilities [{:cost [:credit 1] :msg "break 1 additional subroutine"}]}

   "Ekomind"
   {:effect (req (swap! state assoc-in [:runner :memory] (count (get-in @state [:runner :hand])))
                 (add-watch state :ekomind (fn [k ref old new]
                                             (let [hand-size (count (get-in new [:runner :hand]))]
                                               (when (not= (count (get-in old [:runner :hand])) hand-size)
                                                 (swap! ref assoc-in [:runner :memory] hand-size))))))
    :leave-play (req (remove-watch state :ekomind))}

   "EMP Device"
   {:abilities [{:req (req (:run @state))
                 :msg "prevent the Corp from rezzing more than 1 piece of ICE for the remainder of the run"
                 :effect (effect (register-events
                                   {:rez {:req (req (ice? target))
                                          :effect (effect (register-run-flag!
                                                            card :can-rez
                                                            (fn [state side card]
                                                              (if (ice? card)
                                                                ((constantly false)
                                                                 (toast state :corp "Cannot rez ICE the rest of this run due to EMP Device"))
                                                                true))))}
                                    :run-ends {:effect (effect (unregister-events card))}} (assoc card :zone '(:discard)))
                                 (trash card {:cause :ability-cost}))}]}

   "Feedback Filter"
   {:prevent {:damage [:net :brain]}
    :abilities [{:cost [:credit 3] :msg "prevent 1 net damage" :effect (effect (damage-prevent :net 1))}
                {:label "[Trash]: Prevent up to 2 brain damage"
                 :msg "prevent up to 2 brain damage"
                 :effect (effect (trash card {:cause :ability-cost})
                                 (damage-prevent :brain 2))}]}

   "Forger"
   {:prevent {:tag [:all]}
    :in-play [:link 1]
    :abilities [{:msg "avoid 1 tag" :label "[Trash]: Avoid 1 tag"
                 :effect (effect (tag-prevent 1) (trash card {:cause :ability-cost}))}
                {:msg "remove 1 tag" :label "[Trash]: Remove 1 tag"
                 :effect (effect (trash card {:cause :ability-cost}) (lose :tag 1))}]}

   "GPI Net Tap"
   {:abilities [{:req (req (and (ice? current-ice) (not (rezzed? current-ice))))
                 :delayed-completion true
                 :effect (req (when-completed (expose state side current-ice)
                                              (continue-ability
                                                state side
                                                {:optional {:prompt "Trash GPI Net Tap to jack out?"
                                                            :yes-ability {:msg "trash it and jack out"
                                                                          :effect (effect (trash card {:unpreventable true})
                                                                                          (jack-out nil))}}}
                                                card nil)))}]}

   "Grimoire"
   {:in-play [:memory 2]
    :events {:runner-install {:silent (req true)
                              :req (req (has-subtype? target "Virus"))
                              :effect (effect (add-counter target :virus 1))}}}

   "Heartbeat"
   {:in-play [:memory 1]
    :prevent {:damage [:meat :net :brain]}
    :abilities [{:msg (msg "prevent 1 damage, trashing a facedown " (:title target))
                 :choices {:req #(and (= (:side %) "Runner") (:installed %))}
                 :priority 50
                 :effect (effect (trash target {:unpreventable true})
                                 (damage-prevent :brain 1)
                                 (damage-prevent :meat 1)
                                 (damage-prevent :net 1))}]}

   "HQ Interface"
   {:in-play [:hq-access 1]}

   "Lemuria Codecracker"
   {:abilities [{:cost [:click 1 :credit 1] :req (req (some #{:hq} (:successful-run runner-reg)))
                 :choices {:req installed?} :effect (effect (expose eid target))
                 :msg "expose 1 card"}]}

   "LLDS Processor"
   {:events
     (let [llds {:effect (req (let [cards (:llds-target card)]
                                (update! state side (dissoc card :llds-target))
                                (doseq [c cards]
                                (update-breaker-strength state side
                                                         (find-cid (:cid c) (all-installed state :runner))))))}]
       {:runner-turn-ends llds :corp-turn-ends llds
        :runner-install {:silent (req true)
                         :req (req (has-subtype? target "Icebreaker"))
                         :effect (effect (update! (update-in card [:llds-target] #(conj % target)))
                                         (update-breaker-strength target))}
        :pre-breaker-strength {:req (req (some #(= (:cid target) (:cid %)) (:llds-target card)))
                               :effect (effect (breaker-strength-bonus 1))}})}

   "Lockpick"
   {:recurring 1}

   "Logos"
   {:in-play [:memory 1 :hand-size-modification 1]
    :events {:agenda-scored
             {:player :runner :prompt "Choose a card" :msg (msg "add 1 card to their Grip from their Stack")
              :choices (req (cancellable (:deck runner)))
              :effect (effect (trigger-event :searched-stack nil)
                              (shuffle! :deck)
                              (move target :hand))}}}

   "Maya"
   {:in-play [:memory 2]
    :abilities [{:once :per-turn
                 :label "Move this accessed card to bottom of R&D"
                 :req (req (when-let [c (:card (first (get-in @state [:runner :prompt])))]
                             (in-deck? c)))
                 :msg "move the card just accessed to the bottom of R&D"
                 :effect (req (let [c (:card (first (get-in @state [:runner :prompt])))]
                                (when (is-type? c "Agenda") ; trashing before the :access events actually fire; fire them manually
                                  (resolve-steal-events state side c))
                                (move state :corp c :deck)
                                (tag-runner state :runner 1)
                                (close-access-prompt state side)
                                (effect-completed state side eid card)))}
                {:once :per-turn
                 :label "Move a previously accessed card to bottom of R&D"
                 :effect (effect (resolve-ability
                                   {; only allow targeting cards that were accessed this turn -- not perfect, but good enough?
                                    :choices {:req #(some (fn [c] (= (:cid %) (:cid c)))
                                                          (map first (turn-events state side :access)))}
                                    :msg (msg "move " (:title target) " to the bottom of R&D")
                                    :effect (req (move state :corp target :deck)
                                                 (tag-runner state :runner 1)
                                                 (swap! state update-in [side :prompt] rest)
                                                 (when-let [run (:run @state)]
                                                   (when (and (:ended run) (empty? (get-in @state [:runner :prompt])))
                                                     (handle-end-run state :runner))))} card nil))}]}

   "MemStrips"
   {:in-play [:memory 3]}

   "Mirror"
   {:in-play [:memory 2]
    :events {:successful-run
             {:delayed-completion true
              :req (req (= target :rd))
              :effect (effect (continue-ability
                                {:prompt "Choose a card and replace 1 spent [Recurring Credits] on it"
                                 :choices {:req #(< (:rec-counter % 0) (:recurring (card-def %) 0))}
                                 :msg (msg "replace 1 spent [Recurring Credits] on " (:title target))
                                 :effect (effect (add-prop target :rec-counter 1))}
                               card nil))}}}

   "Monolith"
   (let [mhelper (fn mh [n] {:prompt "Choose a program to install"
                             :choices {:req #(and (is-type? % "Program")
                                                  (in-hand? %))}
                             :effect (req (install-cost-bonus state side [:credit -4])
                                          (runner-install state side target nil)
                                            (when (< n 3)
                                              (resolve-ability state side (mh (inc n)) card nil)))})]
     {:prevent {:damage [:net :brain]}
      :in-play [:memory 3]
      :effect (effect (resolve-ability (mhelper 1) card nil))
      :abilities [{:msg (msg "prevent 1 brain or net damage by trashing " (:title target))
                   :priority 50
                   :choices {:req #(and (is-type? % "Program")
                                        (in-hand? %))}
                   :prompt "Choose a program to trash from your Grip"
                   :effect (effect (trash target)
                                   (damage-prevent :brain 1)
                                   (damage-prevent :net 1))}]})

   "Muresh Bodysuit"
   {:events {:pre-damage {:once :per-turn :once-key :muresh-bodysuit
                          :req (req (= target :meat))
                          :msg "prevent the first meat damage this turn"
                          :effect (effect (damage-prevent :meat 1))}}}

   "Net-Ready Eyes"
   {:effect (effect (damage eid :meat 2 {:unboostable true :card card})) :msg "suffer 2 meat damage"
    :events {:run {:choices {:req #(and (installed? %)
                                        (has-subtype? % "Icebreaker"))}
                   :msg (msg "give " (:title target) " +1 strength")
                   :effect (effect (pump target 1 :all-run))}}}

   "NetChip"
   {:abilities [{:label "Install a program on NetChip"
                 :req (req (empty? (:hosted card)))
                 :effect (req (let [n (count (filter #(= (:title %) (:title card)) (all-installed state :runner)))]
                                (resolve-ability state side
                                  {:prompt "Choose a program in your Grip to install on NetChip"
                                   :cost [:click 1]
                                   :choices {:req #(and (is-type? % "Program")
                                                        (runner-can-install? state side % false)
                                                        (<= (:memoryunits %) n)
                                                        (in-hand? %))}
                                   :msg (msg "host " (:title target))
                                   :effect (effect (gain :memory (:memoryunits target))
                                                   (runner-install target {:host-card card})
                                                   (update! (assoc (get-card state card)
                                                                   :hosted-programs
                                                                   (cons (:cid target) (:hosted-programs card)))))}
                                 card nil)))}
                {:label "Host an installed program on NetChip"
                 :req (req (empty? (:hosted card)))
                 :effect (req (let [n (count (filter #(= (:title %) (:title card)) (all-installed state :runner)))]
                                (resolve-ability state side
                                  {:prompt "Choose an installed program to host on NetChip"
                                   :choices {:req #(and (is-type? % "Program")
                                                        (<= (:memoryunits %) n)
                                                        (installed? %))}
                                   :msg (msg "host " (:title target))
                                   :effect (effect (host card target)
                                                   (gain :memory (:memoryunits target))
                                                   (update! (assoc (get-card state card)
                                                                   :hosted-programs
                                                                   (cons (:cid target) (:hosted-programs card)))))}
                                 card nil)))}]
    :events {:card-moved {:req (req (some #{(:cid target)} (:hosted-programs card)))
                          :effect (effect (update! (assoc card
                                                          :hosted-programs
                                                          (remove #(= (:cid target) %) (:hosted-programs card))))
                                          (lose :memory (:memoryunits target)))}}}

   "Obelus"
   {:in-play [:memory 1]
    :effect (req (gain state :runner :hand-size-modification (:tag runner))
                 (add-watch state :obelus
                   (fn [k ref old new]
                     (let [tagnew (get-in new [:runner :tag])
                           tagold (get-in old [:runner :tag])]
                       (when (> tagnew tagold)
                         (gain state :runner :hand-size-modification (- tagnew tagold)))
                       (when (< tagnew tagold)
                         (lose state :runner :hand-size-modification (- tagold tagnew)))))))
    :leave-play (req (remove-watch state :obelus)
                     (lose state :runner :hand-size-modification (:tag runner)))
    :events {:successful-run-ends {:once :per-turn
                                   :req (req (let [successes (rest (turn-events state side :successful-run))]
                                               (and (#{[:rd] [:hq]} (:server target))
                                                    (empty? (filter #(#{'(:rd) '(:hq)} %) successes)))))
                                   :msg (msg "draw " (:cards-accessed target 0) " cards")
                                   :effect (effect (draw (:cards-accessed target 0)))}}}

   "Omni-drive"
   {:recurring 1
    :abilities [{:label "Install and host a program of 1[Memory Unit] or less on Omni-drive"
                 :req (req (empty? (:hosted card)))
                 :cost [:click 1]
                 :prompt "Choose a program of 1[Memory Unit] or less to install on Omni-drive from your grip"
                 :choices {:req #(and (is-type? % "Program")
                                      (<= (:memoryunits %) 1)
                                      (in-hand? %))}
                 :msg (msg "host " (:title target))
                 :effect (effect (gain :memory (:memoryunits target))
                                 (runner-install target {:host-card card}))}
                {:label "Host an installed program of 1[Memory Unit] or less on Omni-drive"
                 :prompt "Choose an installed program of 1[Memory Unit] or less to host on Omni-drive"
                 :choices {:req #(and (is-type? % "Program")
                                      (installed? %))}
                 :msg (msg "host " (:title target))
                 :effect (effect (host card target))}]}

   "Plascrete Carapace"
   {:data [:counter {:power 4}]
    :prevent {:damage [:meat]}
    :abilities [{:counter-cost [:power 1]
                 :msg "prevent 1 meat damage"
                 :effect (req (damage-prevent state side :meat 1)
                              (when (= (get-in card [:counter :power]) 0)
                                (trash state side card {:unpreventable true})))}]}

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
   {:flags {:runner-phase-12 (req true)}
    :abilities [{:label "Lose [Click], avoid 1 tag (start of turn)"
                 :once :per-turn
                 :req (req (:runner-phase-12 @state))
                 :effect (effect (update! (assoc card :qianju-active true)))
                 :msg "lose [Click] and avoid the first tag received until their next turn"}]
    :events {:corp-turn-ends {:effect (effect (update! (dissoc card :qianju-active)))}
             :runner-turn-begins {:req (req (:qianju-active card))
                                  :effect (effect (lose :click 1))}
             :pre-tag {:req (req (:qianju-active card))
                       :msg "avoid the first tag received"
                       :effect (effect (tag-prevent 1)
                                       (update! (dissoc card :qianju-active)))}}}

   "R&D Interface"
   {:in-play [:rd-access 1]}

   "Rabbit Hole"
   {:in-play [:link 1]
    :effect
    (effect (resolve-ability
             {:optional {:req (req (some #(when (= (:title %) "Rabbit Hole") %) (:deck runner)))
                         :prompt "Install another Rabbit Hole?" :msg "install another Rabbit Hole"
                         :yes-ability {:effect (req (when-let [c (some #(when (= (:title %) "Rabbit Hole") %)
                                                                      (:deck runner))]
                                                     (trigger-event state side :searched-stack nil)
                                                     (shuffle! state :runner :deck)
                                                     (runner-install state side c)))}}} card nil))}

   "Ramujan-reliant 550 BMI"
   {:prevent {:damage [:net :brain]}
    :abilities [{:req (req (not-empty (:deck runner)))
                 :effect (req (let [n (count (filter #(= (:title %) (:title card)) (all-installed state :runner)))]
                                (resolve-ability state side
                                  {:prompt "Choose how much damage to prevent"
                                   :priority 50
                                   :choices {:number (req (min n (count (:deck runner))))}
                                   :msg (msg "trash " target " cards from their Stack and prevent " target " damage")
                                   :effect (effect (damage-prevent :net target)
                                                   (damage-prevent :brain target)
                                                   (mill :runner target)
                                                   (trash card {:cause :ability-cost}))} card nil)))}]}

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

   "Reflection"
   {:in-play [:memory 1 :link 1]
    :events {:jack-out {:msg (msg "force the Corp to reveal " (:title (first (shuffle (:hand corp)))) " from HQ")}}}

   "Replicator"
   {:events {:runner-install
             {:interactive (req (and (is-type? target "Hardware")
                                     (some #(= (:title %) (:title target)) (:deck runner))))
              :silent (req (not (and (is-type? target "Hardware")
                                     (some #(= (:title %) (:title target)) (:deck runner)))))
              :optional {:prompt "Use Replicator to add a copy?"
                         :req (req (and (is-type? target "Hardware") (some #(= (:title %) (:title target)) (:deck runner))))
                         :yes-ability {:msg (msg "add a copy of " (:title target) " to their Grip")
                                       :effect (effect (trigger-event :searched-stack nil)
                                                       (shuffle! :deck)
                                                       (move (some #(when (= (:title %) (:title target)) %)
                                                                   (:deck runner)) :hand))}}}}}

   "Security Chip"
   {:abilities [{:label "[Trash]: Add [Link] strength to a non-Cloud icebreaker until the end of the run"
                 :msg (msg "add " (:link runner) " strength to " (:title target) " until the end of the run")
                 :req (req (:run @state))
                 :prompt "Choose one non-Cloud icebreaker"
                 :choices {:req #(and (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "Cloud"))
                                      (installed? %))}
                 :effect (effect (pump target (:link runner) :all-run)
                                 (trash (get-card state card) {:cause :ability-cost}))}
                {:label "[Trash]: Add [Link] strength to any Cloud icebreakers until the end of the run"
                 :msg (msg "add " (:link runner) " strength to " (count targets) " Cloud icebreakers until the end of the run")
                 :req (req (:run @state))
                 :prompt "Choose any number of Cloud icebreakers"
                 :choices {:max 50 :req #(and (has-subtype? % "Icebreaker")
                                              (has-subtype? % "Cloud")
                                              (installed? %))}
                 :effect (req (doseq [t targets]
                                (pump state side t (:link runner) :all-run)
                                (update-breaker-strength state side t))
                              (trash state side (get-card state card) {:cause :ability-cost}))}]}

   "Security Nexus"
   {:in-play [:memory 1 :link 1]
    :abilities [{:req (req (:run @state))
                 :msg "force the Corp to initiate a trace"
                 :label "Trace 5 - Give the Runner 1 tag and end the run"
                 :trace {:once :per-turn :base 5 :msg "give the Runner 1 tag and end the run"
                         :effect (effect (tag-runner :runner 1) (end-run))
                         :unsuccessful {:msg "bypass the current ICE"}}}]}

   "Silencer"
   {:recurring 1}

   "Skulljack"
   {:effect (effect (damage eid :brain 1 {:card card}))
    :events {:pre-trash {:effect (effect (trash-cost-bonus -1))}}}

   "Spinal Modem"
   {:in-play [:memory 1]
    :recurring 2
    :events {:successful-trace {:req (req run) :effect (effect (damage eid :brain 1 {:card card}))}}}

   "Sports Hopper"
   {:in-play [:link 1]
    :abilities [{:label "Draw 3 cards"
                 :msg "draw 3 cards"
                 :effect (effect (trash card {:cause :ability-cost}) (draw 3))}]}

   "Spy Camera"
   {:abilities [{:cost [:click 1]
                 :delayed-completion true
                 :label "Look at the top X cards of your Stack"
                 :msg "look at the top X cards of their Stack and rearrange them"
                 :effect (req (show-wait-prompt state :corp "Runner to rearrange the top cards of their stack")
                              (let [n (count (filter #(= (:title %) (:title card))
                                                     (all-installed state :runner)))
                                    from (take n (:deck runner))]
                                (if (pos? (count from))
                                  (continue-ability state side (reorder-choice :runner :corp from '()
                                                                               (count from) from) card nil)
                                  (do (clear-wait-prompt state :corp)
                                      (effect-completed state side eid card)))))}
                {:label "[Trash]: Look at the top card of R&D"
                 :msg "trash it and look at the top card of R&D"
                 :effect (effect (prompt! card (str "The top card of R&D is " (:title (first (:deck corp)))) ["OK"] {})
                                 (trash card {:cause :ability-cost}))}]}

   "The Personal Touch"
   {:hosting {:req #(and (has-subtype? % "Icebreaker")
                         (installed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}

   "The Toolbox"
   {:in-play [:link 2 :memory 2]
    :recurring 2}

   "Titanium Ribs"
   {:events
    {:pre-resolve-damage
     {:delayed-completion true
      :req (req (and (> (last targets) 0)
                     (runner-can-choose-damage? state)
                     (not (get-in @state [:damage :damage-replace]))))
      :effect (req (let [dtype target
                         dmg (last targets)]
                     (when (> dmg (count (:hand runner)))
                       (flatline state))
                     (when (= dtype :brain)
                       (swap! state update-in [:runner :brain-damage] #(+ % dmg))
                       (swap! state update-in [:runner :hand-size-modification] #(- % dmg)))
                     (show-wait-prompt state :corp "Runner to use Titanium Ribs to choose cards to be trashed")
                     (continue-ability state side
                       {:prompt (msg "Choose " dmg " cards to trash for the " (name dtype) " damage") :player :runner
                        :choices {:max dmg :req #(and (in-hand? %) (= (:side %) "Runner"))}
                        :msg (msg "trash " (join ", " (map :title targets)))
                        :effect (req (clear-wait-prompt state :corp)
                                     (doseq [c targets]
                                       (trash state side c {:cause dtype :unpreventable true}))
                                     (trigger-event state side :damage-chosen)
                                     (damage-defer state side :meat 0)
                                     (effect-completed state side eid card))}
                      card nil)
                      (trigger-event state side :damage dtype nil)))}
     :damage-chosen {:effect (effect (enable-runner-damage-choice))}}
    :delayed-completion true
    :effect (effect (enable-runner-damage-choice)
                    (system-msg (str "suffers 2 meat damage from installing Titanium Ribs"))
                    (damage eid :meat 2 {:card card}))
    :leave-play (req (swap! state update-in [:damage] dissoc :damage-choose-runner))}

   "Turntable"
   {:in-play [:memory 1]
    :events {:agenda-stolen
             {:interactive (req true)
              :req (req (not (empty? (:scored corp))))
              :delayed-completion true
              :effect (req
                        (let [stolen target]
                          (continue-ability
                            state side
                            {:optional
                             {:prompt (msg "Swap " (:title stolen) " for an agenda in the Corp's score area?")
                              :yes-ability
                              {:delayed-completion true
                               :effect (req
                                         (continue-ability
                                           state side
                                           {:prompt (str "Choose a scored Corp agenda to swap with " (:title stolen))
                                            :choices {:req #(in-corp-scored? state side %)}
                                            :effect (req (let [scored target]
                                                           (swap-agendas state side scored stolen)
                                                           (system-msg state side (str "uses Turntable to swap "
                                                                                       (:title stolen) " for " (:title scored)))
                                                           (effect-completed state side eid card)))}
                                           card targets))}}}
                            card targets)))}}}

   "Unregistered S&W 35"
   {:abilities
    [{:cost [:click 2] :req (req (some #{:hq} (:successful-run runner-reg)))
      :label "trash a Bioroid, Clone, Executive or Sysop" :prompt "Choose a Bioroid, Clone, Executive, or Sysop to trash"
      :choices {:req #(and (rezzed? %)
                           (or (has-subtype? % "Bioroid")
                               (has-subtype? % "Clone")
                               (has-subtype? % "Executive")
                               (has-subtype? % "Sysop"))
                           (or (and (= (last (:zone %)) :content) (is-remote? (second (:zone %))))
                               (= (last (:zone %)) :onhost)))}
      :msg (msg "trash " (:title target)) :effect (effect (trash target))}]}

   "Vigil"
   (let [ability {:req (req (and (:runner-phase-12 @state) (= (count (:hand corp)) (hand-size state :corp))))
                  :msg "draw 1 card"
                  :label "Draw 1 card (start of turn)"
                  :once :per-turn
                  :effect (effect (draw 1))}]
   {:in-play [:memory 1]
    :events {:runner-turn-begins ability}
    :abilities [ability]})

   "Window"
   {:abilities [{:cost [:click 1] :msg "draw 1 card from the bottom of their Stack"
                 :effect (effect (move (last (:deck runner)) :hand))}]}})
