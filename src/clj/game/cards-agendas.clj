(in-ns 'game.core)

(declare is-scored?)

(def cards-agendas

  {"15 Minutes"
     {:abilities [{:cost [:click 1] :msg "shuffle 15 Minutes into R&D"
                   :effect (req (let [corp-agendas (get-in corp [:scored])
                                      agenda-owner (if (some #(= (:cid %) (:cid card)) corp-agendas) :corp :runner)]
                                  (gain-agenda-point state agenda-owner (- (:agendapoints card))))
                                ; refresh agendapoints to 1 before shuffle in case it was modified by e.g. The Board
                                (move state :corp (dissoc (assoc card :agendapoints 1) :seen :rezzed) :deck {:front true})
                                (shuffle! state :corp :deck))}]
      :flags {:has-abilities-when-stolen true}}

   "Accelerated Beta Test"
   (letfn [(abt [n i]
             {:req (req (pos? i))
              :prompt "Select a piece of ICE from the Temporary Zone to install"
              :choices {:req #(and (:side % "Corp")
                                   (ice? %)
                                   (= (:zone %) [:play-area]))}
              :effect (req (corp-install state side target nil
                                         {:no-install-cost true :install-state :rezzed-no-cost})
                           (trigger-event state side :rez target)
                           (when (< n i)
                             (resolve-ability state side (abt (inc n) i) card nil)))})]
     {:optional {:prompt "Look at the top 3 cards of R&D?"
                 :yes-ability {:effect (req (let [n (count (filter ice? (take 3 (:deck corp))))]
                                              (resolve-ability state side
                                                               {:msg "look at the top 3 cards of R&D"
                                                                :effect (req (doseq [c (take 3 (:deck corp))]
                                                                               (move state side c :play-area))
                                                                             (resolve-ability state side (abt 1 n) card nil))}
                                                               card nil)))}}})

   "Advanced Concept Hopper"
   {:events
    {:run
     {:req (req (first-event state side :run))
      :effect (effect (show-wait-prompt :runner "Corp to use Advanced Concept Hopper")
                      (resolve-ability
                        {:player :corp
                         :prompt "Use Advanced Concept Hopper to draw 1 card or gain 1 [Credits]?" :once :per-turn
                         :choices ["Draw 1 card" "Gain 1 [Credits]" "No action"]
                         :effect (req (case target
                                        "Gain 1 [Credits]"
                                        (do (gain state :corp :credit 1)
                                            (system-msg state :corp (str "uses Advanced Concept Hopper to gain 1 [Credits]")))
                                        "Draw 1 card"
                                        (do (draw state :corp)
                                            (system-msg state :corp (str "uses Advanced Concept Hopper to draw 1 card")))
                                        "No action"
                                        (system-msg state :corp (str "doesn't use Advanced Concept Hopper")))
                                      (clear-wait-prompt state :runner))} card nil))}}}

   "Ancestral Imager"
   {:events {:jack-out {:msg "do 1 net damage" :effect (effect (damage :net 1))}}}

   "AstroScript Pilot Program"
   {:data {:counter 1}
    :abilities [{:counter-cost 1 :msg (msg "place 1 advancement token on "
                                           (card-str state target))
                 :choices {:req can-be-advanced?}
                 :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]}

   "Award Bait"
   {:access {:choices ["0", "1", "2"] :prompt "How many advancement tokens?"
             :effect (req (let [c (Integer/parseInt target)]
                            (resolve-ability
                             state side
                             {:choices {:req can-be-advanced?}
                              :msg (msg "place " c " advancement tokens on " (card-str state target))
                              :effect (effect (add-prop :corp target :advance-counter c {:placed true}))} card nil)))}}

   "Bifrost Array"
   {:req (req (not (empty? (filter #(not= (:title %) "Bifrost Array") (:scored corp)))))
    :optional {:prompt "Trigger the ability of a scored agenda?"
               :yes-ability {:prompt "Choose an agenda to trigger its \"when scored\" ability"
                             :choices (req (filter #(not= (:title %) "Bifrost Array") (:scored corp)))
                             :msg (msg "trigger the \"when scored\" ability of " (:title target))
                             :effect (effect (card-init target))}}}

   "Braintrust"
   {:effect (effect (set-prop card :counter (quot (- (:advance-counter card) 3) 2)))
    :events {:pre-rez-cost {:req (req (ice? target))
                            :effect (effect (rez-cost-bonus (- (:counter (get-card state card)))))}}}

   "Breaking News"
   {:effect (effect (tag-runner :runner 2))
    :msg "give the Runner 2 tags"
    :end-turn {:effect (effect (lose :runner :tag 2))
               :msg "make the Runner lose 2 tags"}}

   "Character Assassination"
   {:prompt "Choose a resource to trash"
    :choices {:req #(and (installed? %)
                         (is-type? % "Resource"))}
    :msg (msg "trash " (:title target))
    :effect (effect (trash target))}

   "Chronos Project"
   {:msg "remove all cards in the Runner's Heap from the game"
    :effect (effect (move-zone :runner :discard :rfg))}

   "Clone Retirement"
   {:msg "remove 1 bad publicity" :effect (effect (lose :bad-publicity 1))
    :stolen {:msg "force the Corp to take 1 bad publicity"
             :effect (effect (gain :corp :bad-publicity 1))}}

   "Corporate War"
   {:msg (msg (if (> (:credit corp) 6) "gain 7 [Credits]" "lose all credits"))
    :effect (req (if (> (:credit corp) 6)
                   (gain state :corp :credit 7) (lose state :corp :credit :all)))}

   "Corporate Sales Team"
   (let [e {:msg "gain 1 [Credit]"  :counter-cost 1
            :effect (req (gain state :corp :credit 1)
                         (when (zero? (:counter card))
                           (unregister-events state :corp card)))}]
    {:effect (effect (add-prop card :counter 10))
     :events {:runner-turn-begins e
              :corp-turn-begins   e}})

   "Director Haas Pet Project"
   (let [dhelper (fn dpp [n] {:prompt "Select a card to install"
                              :show-discard true
                              :choices {:req #(and (= (:side %) "Corp")
                                                   (not (is-type? % "Operation"))
                                                   (#{[:hand] [:discard]} (:zone %)))}
                              :effect (req (corp-install state side target
                                                         (last (get-remote-names @state))
                                                         {:no-install-cost true})
                                           (when (< n 2)
                                             (resolve-ability state side
                                                              (dpp (inc n)) card nil)))
                              :msg (msg (corp-install-msg target))})]
     {:optional {:prompt "Create a new remote server?"
                 :yes-ability {:prompt "Select a card to install"
                               :show-discard true
                               :choices {:req #(and (:side % "Corp")
                                                    (not (is-type? % "Operation"))
                                                    (#{[:hand] [:discard]} (:zone %)))}
                               :effect (req (corp-install state side target "New remote"
                                                          {:no-install-cost true})
                                            (resolve-ability state side (dhelper 1) card nil))
                               :msg "create a new remote server, installing cards at no cost"}}})

   "Domestic Sleepers"
   {:agendapoints-runner (req (do 0))
    :abilities [{:cost [:click 3] :msg "place 1 agenda counter on Domestic Sleepers"
                 :effect (req (when (zero? (:counter card))
                                (gain-agenda-point state side 1))
                              (set-prop state side card :counter 1 :agendapoints 1))}]}

   "Eden Fragment"
   {:events {:pre-corp-install
               {:req (req (and (is-type? target "ICE")
                               (empty? (let [cards (map first (turn-events state side :corp-install))]
                                         (filter #(is-type? % "ICE") cards)))))
                :effect (effect (ignore-install-cost true))}
             :corp-install
               {:req (req (and (is-type? target "ICE")
                               (empty? (let [cards (map first (turn-events state side :corp-install))]
                                         (filter #(is-type? % "ICE") cards)))))
                :msg (msg "ignore the install cost of the first ICE this turn")}}}

   "Efficiency Committee"
   {:effect (effect (add-prop card :counter 3))
    :abilities [{:cost [:click 1] :counter-cost 1 :effect (effect (gain :click 2))
                 :msg "gain [Click][Click]"}]}

   "Encrypted Portals"
   {:msg (msg "gain " (reduce (fn [c server]
                                (+ c (count (filter #(and (has-subtype? % "Code Gate")
                                                          (rezzed? %)) (:ices server)))))
                              0 (flatten (seq (:servers corp))))
              " [Credits]")
    :effect (effect (gain :credit
                          (reduce (fn [c server]
                                    (+ c (count (filter #(and (has-subtype? % "Code Gate")
                                                              (rezzed? %)) (:ices server)))))
                                  0 (flatten (seq (:servers corp)))))
                    (update-all-ice))
    :events {:pre-ice-strength {:req (req (has-subtype? target "Code Gate"))
                                :effect (effect (ice-strength-bonus 1 target))}}}

   "Executive Retreat"
   {:data {:counter 1} :effect (effect (shuffle-into-deck :hand))
    :abilities [{:cost [:click 1] :counter-cost 1 :msg "draw 5 cards" :effect (effect (draw 5))}]}

   "Explode-a-palooza"
   {:access {:optional {:prompt "Gain 5 [Credits] with Explode-a-palooza ability?"
                       :yes-ability {:msg "gain 5 [Credits]"
                                     :effect (effect (gain :corp :credit 5))}}}}

   "False Lead"
   {:abilities [{:req (req (>= (:click runner) 2)) :msg "force the Runner to lose [Click][Click]"
                 :effect (effect (forfeit card) (lose :runner :click 2))}]}

   "Fetal AI"
   {:access {:req (req (not= (first (:zone card)) :discard)) :msg "do 2 net damage"
             :effect (effect (damage :net 2 {:card card}))}
    :steal-cost-bonus (req [:credit 2])}

   "Firmware Updates"
   {:data [:counter 3]
    :abilities [{:counter-cost 1 :choices {:req #(and (ice? %) (can-be-advanced? %))}
                 :req (req (< 0 (:counter card 0)))
                 :msg (msg "place 1 advancement token on " (card-str state target))
                 :once :per-turn :effect (effect (add-prop target :advance-counter 1))}]}

   "Genetic Resequencing"
   {:choices {:req #(= (last (:zone %)) :scored)} :msg (msg "add 1 agenda counter on " (:title target))
    :effect (effect (add-prop target :counter 1))}

   "Geothermal Fracking"
   {:data {:counter 2}
    :abilities [{:cost [:click 1] :counter-cost 1 :msg "gain 7 [Credits] and take 1 bad publicity"
                 :effect (effect (gain :credit 7 :bad-publicity 1))}]}

   "Gila Hands Arcology"
   {:abilities [{:cost [:click 2] :effect (effect (gain :credit 3)) :msg "gain 3 [Credits]"}]}

   "Global Food Initiative"
   {:agendapoints-runner (req (do 2))}

   "Glenn Station"
   {:abilities [{:label "Host a card from HQ on Glenn Station" :cost [:click 1]
                 :prompt "Choose a card to host on Glenn Station" :choices (req (:hand corp))
                 :msg "host a card from HQ" :effect (effect (host card target {:facedown true}))}
                {:label "Add a card on Glenn Station to HQ" :cost [:click 1]
                 :prompt "Choose a card on Glenn Station" :choices (req (:hosted card))
                 :msg "add a hosted card to HQ" :effect (effect (move target :hand))}]}

   "Government Contracts"
   {:abilities [{:cost [:click 2] :effect (effect (gain :credit 4)) :msg "gain 4 [Credits]"}]}

   "Government Takeover"
   {:abilities [{:cost [:click 1] :effect (effect (gain :credit 3)) :msg "gain 3 [Credits]"}]}

   "Hades Fragment"
   {:flags {:corp-phase-12 (req (and (not-empty (get-in @state [:corp :discard])) (is-scored? state :corp card)))}
    :abilities [{:prompt "Choose a card to add to the bottom of R&D"
                 :show-discard true
                 :choices {:req #(and (= (:side %) "Corp") (= (:zone %) [:discard]))}
                 :effect (effect (move target :deck))
                 :msg (msg "add " (if (:seen target) (:title target) "a card") " to the bottom of R&D")}]}

   "Helium-3 Deposit"
   {:choices ["0", "1", "2"] :prompt "How many power counters?"
    :effect (req (let [c (Integer/parseInt target)]
                   (resolve-ability
                     state side
                     {:choices {:req #(contains? % :counter)}
                      :msg (msg "add " c " power counters on " (:title target))
                      :effect (effect (add-prop target :counter c))} card nil)))}

   "High-Risk Investment"
   {:data {:counter 1}
    :abilities [{:cost [:click 1] :counter-cost 1 :msg (msg "gain " (:credit runner) " [Credits]")
                 :effect (effect (gain :credit (:credit runner)))}]}

   "Hostile Takeover"
   {:msg "gain 7 [Credits] and take 1 bad publicity"
    :effect (effect (gain :credit 7 :bad-publicity 1))}

   "Hollywood Renovation"
   {:install-state :face-up
    :events {:advance
             {:req (req (= (:cid card) (:cid target)))
              :effect (req (let [n (if (>= (:advance-counter (get-card state card)) 6) 2 1)]
                             (resolve-ability
                              state side
                              {:choices {:req #(and (not= (:cid %) (:cid card))
                                                    (can-be-advanced? %))}
                               :msg (msg "place " n " advancement tokens on "
                                         (card-str state target))
                               :effect (effect (add-prop :corp target :advance-counter n {:placed true}))} card nil)))}}}

   "House of Knives"
   {:data {:counter 3}
    :abilities [{:counter-cost 1 :msg "do 1 net damage" :req (req (:run @state)) :once :per-run
                 :effect (effect (damage :net 1 {:card card}))}]}

   "Improved Protein Source"
     {:msg (msg "make the Runner gain 4 [Credits]")
      :effect (effect (gain :runner :credit 4))
      :stolen {:msg "make the Runner gain 4 [Credits]"
               :effect (effect (gain :credit 4))}}

   "Improved Tracers"
   {:effect (req (update-all-ice state side))
    :events {:pre-ice-strength {:req (req (has-subtype? target "Tracer"))
                                :effect (effect (ice-strength-bonus 1 target))}
             :pre-init-trace {:req (req (ice? target))
                              :effect (effect (init-trace-bonus 1))}}}

   "Labyrinthine Servers"
   {:data {:counter 2}
    :abilities [{:counter-cost 1 :effect (effect (prevent-jack-out))
                 :msg "prevent the Runner from jacking out"}]}

   "License Acquisition"
   {:prompt "Choose an asset or upgrade to install from Archives or HQ" :show-discard true
    :msg (msg "install and rez " (:title target))
    :choices {:req #(and (#{"Asset" "Upgrade"} (:type %))
                         (#{[:hand] [:discard]} (:zone %))
                         (= (:side %) "Corp"))}
    :effect (effect (corp-install target nil {:install-state :rezzed-no-cost}))}

   "Mandatory Upgrades"
   {:msg "gain an additional [Click] per turn"
    :effect (effect (gain :click 1 :click-per-turn 1))
    :leave-play (req (lose state :corp :click 1 :click-per-turn 1))}

   "Market Research"
   {:req (req tagged) :effect (effect (set-prop card :counter 1 :agendapoints 3))}

   "Medical Breakthrough"
   {:effect (effect (update-all-advancement-costs))
    :stolen (effect (update-all-advancement-costs))
    :advancement-cost-bonus (req (- (count (filter #(= (:title %) "Medical Breakthrough")
                                                   (concat (:scored corp) (:scored runner))))))}

   "Merger"
   {:agendapoints-runner (req (do 3))}

   "NAPD Contract"
   {:steal-cost-bonus (req [:credit 4])
    :advancement-cost-bonus (req (:bad-publicity corp))}

   "Nisei MK II"
   {:data {:counter 1}
    :abilities [{:req (req (:run @state)) :counter-cost 1 :msg "end the run"
                 :effect (effect (end-run))}]}

   "Oaktown Renovation"
   {:install-state :face-up
    :events {:advance {:req (req (= (:cid card) (:cid target)))
                       :msg (msg "gain " (if (>= (:advance-counter (get-card state card)) 5) "3" "2") " [Credits]")
                       :effect (req (gain state side :credit
                                          (if (>= (:advance-counter (get-card state card)) 5) 3 2)))}}}

   "Philotic Entanglement"
   {:req (req (> (count (:scored runner)) 0))
    :msg (msg "do " (count (:scored runner)) " net damage")
    :effect (effect (damage :net (count (:scored runner)) {:card card}))}

   "Posted Bounty"
   {:optional {:prompt "Forfeit Posted Bounty to give the Runner 1 tag and take 1 bad publicity?"
               :yes-ability {:msg "give the Runner 1 tag and take 1 bad publicity"
                             :effect (effect (gain :bad-publicity 1) (tag-runner :runner 1) (forfeit card))}}}

   "Priority Requisition"
   {:choices {:req #(and (ice? %) (not (rezzed? %)))}
    :msg (msg "rez " (:title target) " at no cost")
    :effect (effect (rez target {:ignore-cost :all-costs}))}

   "Private Security Force"
   {:abilities [{:req (req tagged) :cost [:click 1] :effect (effect (damage :meat 1 {:card card}))
                 :msg "do 1 meat damage"}]}

   "Profiteering"
   {:choices ["0" "1" "2" "3"] :prompt "How many bad publicity?"
    :msg (msg "take " target " bad publicity and gain " (* 5 (Integer/parseInt target)) " [Credits]")
    :effect (effect (gain :credit (* 5 (Integer/parseInt target))
                          :bad-publicity (Integer/parseInt target)))}

   "Project Ares"
     {:req (req #(and (> (:advance-counter card) 4) (> (count (all-installed state :runner)) 0)))
      :msg (msg "force the Runner to trash " (- (:advance-counter card) 4) " installed cards and take 1 bad publicity")
      :effect (req (let [ares card]
                     (resolve-ability
                       state :runner
                       {:prompt (msg "Choose " (- (:advance-counter ares) 4) " installed cards to trash")
                        :choices {:max (- (:advance-counter ares) 4) :req #(and (:installed %) (= (:side %) "Runner"))}
                        :effect (effect (trash-cards targets)
                                        (system-msg (str "trashes " (join ", " (map :title targets)))))}
                       card nil))
                   (gain state :corp :bad-publicity 1))}

   "Project Atlas"
   {:effect (effect (set-prop card :counter (max 0 (- (:advance-counter card) 3))))
    :abilities [{:counter-cost 1 :prompt "Choose a card" :label "Search R&D and add 1 card to HQ"
                 :req (req (< 0 (:counter card 0))) ;; we need the req or the prompt will still show
                 :msg (msg "add " (:title target) " to HQ from R&D")
                 :choices (req (cancellable (:deck corp) :sorted))
                 :cancel-effect (effect (system-msg "cancels the effect of Project Atlas"))
                 :effect (effect (move target :hand) (shuffle! :deck))}]}

   "Project Beale"
   {:agendapoints-runner (req (do 2))
    :effect (effect (set-prop card :counter (quot (- (:advance-counter card) 3) 2)
                              :agendapoints (+ 2 (quot (- (:advance-counter card) 3) 2))))}

   "Project Vitruvius"
   {:effect (effect (set-prop card :counter (- (:advance-counter card) 3)))
    :abilities [{:counter-cost 1 :prompt "Choose a card"
                 :req (req (< 0 (:counter card 0)))
                 :msg (msg "add " (if (:seen target)
                                    (:title target) "an unseen card ") " to HQ from Archives")
                 :choices (req (:discard corp)) :effect (effect (move target :hand))}]}

   "Project Wotan"
   {:data [:counter 3]
    :abilities [{:counter-cost 1 :msg "add an 'End the run' subroutine to the approached ICE"}]}

   "Quantum Predictive Model"
   {:steal-req (req (not tagged))
    :access {:req (req tagged)
             :effect (effect (as-agenda card 1))
             :msg "add it to their score area and gain 1 agenda point"}}

   "Rebranding Team"
   {:effect (req (doseq [c (filter #(is-type? % "Asset")
                                   (concat (all-installed state :corp)
                                           (:deck corp)
                                           (:hand corp)
                                           (:discard corp)))]
                   (update! state side (assoc c :subtype
                                              (->> (vec (.split (or (:subtype c) "") " - "))
                                                   (cons "Advertisement")
                                                   distinct
                                                   (join " - "))))))
    :msg "make all assets gain Advertisement"}

   "Research Grant"
   {:req (req (not (empty? (filter #(= (:title %) "Research Grant") (all-installed state :corp)))))
    :prompt "Choose another installed copy of Research Grant to score"
    :choices {:req #(= (:title %) "Research Grant")}
    :effect (effect (score (assoc target :advance-counter (:advancementcost target))))
    :msg (msg "score another copy of Research Grant")}

   "Restructured Datapool"
   {:abilities [{:cost [:click 1]
                 :trace {:base 2 :msg "give the Runner 1 tag" :effect (effect (tag-runner :runner 1))}}]}

   "Self-Destruct Chips"
   {:effect (effect (lose :runner :hand-size-modification 1))
    :leave-play (effect (gain :runner :hand-size-modification 1))}

   "Sentinel Defense Program"
   {:events {:damage {:req (req (= target :brain)) :msg "to do 1 net damage"
                      :effect (effect (damage :net 1 {:card card})) }}}

   "Superior Cyberwalls"
   {:msg (msg "gain " (reduce (fn [c server]
                                (+ c (count (filter (fn [ice] (and (has? ice :subtype "Barrier")
                                                                   (:rezzed ice))) (:ices server)))))
                              0 (flatten (seq (:servers corp))))
              " [Credits]")
    :effect (req (do (gain state :corp :credit
                           (reduce (fn [c server]
                                     (+ c (count (filter #(and (has-subtype? % "Barrier")
                                                               (rezzed? %)) (:ices server)))))
                                   0 (flatten (seq (:servers corp)))))
                     (update-all-ice state side)))
    :events {:pre-ice-strength {:req (req (has? target :subtype "Barrier"))
                                :effect (effect (ice-strength-bonus 1 target))}}}

   "TGTBT"
   {:access {:msg "give the Runner 1 tag" :effect (effect (tag-runner :runner 1))}}

   "The Cleaners"
   {:events {:pre-damage {:req (req (= target :meat)) :msg "do 1 additional meat damage"
                          :effect (effect (damage-bonus :meat 1))}}}

   "The Future is Now"
   {:prompt "Choose a card to add to HQ" :choices (req (:deck corp))
    :msg (msg "add a card from R&D to HQ and shuffle R&D")
    :effect (effect (move target :hand) (shuffle! :deck))}

   "The Future Perfect"
   {:steal-req (req installed)
    :access {:psi {:req (req (not installed)) :equal {:effect (effect (steal card))}}}}

   "Underway Renovation"
   {:install-state :face-up
    :events {:advance {:req (req (= (:cid card) (:cid target)))
                       :msg (msg "trash the top " (if (>= (:advance-counter (get-card state card)) 4) "2 cards" "card")
                                 " of the Runner's Stack")
                       :effect (effect (mill :runner
                                             (if (>= (:advance-counter (get-card state card)) 4) 2 1)))}}}

   "Unorthodox Predictions"
   {:prompt "Choose an ICE type for Unorthodox Predictions" :choices ["Sentry", "Code Gate", "Barrier"]
    :msg (msg "prevent subroutines on " target " ICE from being broken until next turn.")}

   "Utopia Fragment"
   {:events {:pre-steal-cost {:req (req (pos? (or (:advance-counter target) 0)))
                              :effect (req (let [counter (:advance-counter target)]
                                             (steal-cost-bonus state side [:credit (* 2 counter)])))}}}

   "Veterans Program"
   {:msg "lose 2 bad publicity"
    :effect (effect (lose :bad-publicity 2))}

   "Vulcan Coverup"
   {:msg "do 2 meat damage" :effect (effect (damage :meat 2 {:card card}))
    :stolen {:msg "force the Corp to take 1 bad publicity"
             :effect (effect (gain :corp :bad-publicity 1))}}})
