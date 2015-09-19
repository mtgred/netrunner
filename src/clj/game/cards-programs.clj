(in-ns 'game.core)

(def cards-programs
  {"Analog Dreamers"
   {:abilities [{:cost [:click 1] :msg "make a run on R&D"
                 :effect (effect (run :rd {:req (req (= target :rd))
                                           :replace-access
                                           {:prompt "Choose a card to shuffle into R&D"
                                            :choices {:req #(and (not (= (:type %) "ICE"))
                                                                 (not (:rezzed %))
                                                                 (not (:advance-counter %)))}
                                            :effect (req (move state :corp target :deck)
                                                         (shuffle! state :corp :deck)
                                                         (swap! state update-in [:runner :prompt] rest)
                                                         (handle-end-run state side)) ; remove the replace-access prompt
                                            :msg "shuffle a card into R&D"}} card))}]}



   "Au Revoir"
   {:events {:jack-out {:effect (effect (gain :credit 1)) :msg "gain 1 [Credits]"}}}


   "Bishop"
   {:abilities [{:label "Host Bishop on a piece of ICE" :cost [:click 1]
                 :choices {:req #(and (= (:type %) "ICE")
                                      (= (last (:zone %)) :ices)
                                      (not (some (fn [c] (has? c :subtype "Caïssa")) (:hosted %))))}
                 :msg (msg "host it on " (if (:rezzed target) (:title target) "a piece of ICE"))
                 :effect (effect (host target card))}]
    :events {:pre-ice-strength
             {:req (req (and (= (:cid target) (:cid (:host card))) (:rezzed target)))
              :effect (effect (ice-strength-bonus -2))}}}



   "Bug"
   {:req (req (some #{:hq} (:successful-run runner-reg)))}

   "Cache"
   {:abilities [{:counter-cost 1 :effect (effect (gain :credit 1)) :msg "gain 1 [Credits]"}]
    :data {:counter 3}}

   "Chakana"
   {:leave-play (effect (update-all-advancement-costs))
    :events {:successful-run {:effect (effect (add-prop card :counter 1)) :req (req (= target :rd))}
             :pre-advancement-cost {:req (req (>= (get-virus-counters state side card) 3))
                                    :effect (effect (advancement-cost-bonus 1))}
             :counter-added
             {:req (req (or (= (:title target) "Hivemind") (= (:cid target) (:cid card))))
              :effect (effect (update-all-advancement-costs))}}}

   "Cloak"
   {:recurring 1}

   "Clot"
   {:events {:purge {:effect (effect (trash card))}}}

   "Collective Consciousness"
   {:events {:rez {:req (req (= (:type target) "ICE")) :msg "draw 1 card"
                   :effect (effect (draw :runner))}}}

   "Copycat"
   {:abilities [{:req (req (and (:run @state)
                                (:rezzed current-ice)))
                 :effect (req (let [icename (:title current-ice)]
                                (resolve-ability
                                  state side
                                  {:prompt (msg "Choose a rezzed copy of " icename)
                                   :choices {:req #(and (:rezzed %) (= (:type %) "ICE") (= (:title %) icename))}
                                   :msg "redirect the run"
                                   :effect (req (let [dest (second (:zone target))
                                                      tgtndx (ice-index state target)]
                                                  (swap! state update-in [:run]
                                                         #(assoc % :position tgtndx :server [dest]))
                                                  (trash state side card {:cause :ability-cost})))}
                                 card nil)))}]}

   "Crescentus"
   {:abilities [{:req (req current-ice) :msg (msg "derez " (:title current-ice))
                 :effect (effect (trash card {:cause :ability-cost}) (derez current-ice))}]}

   "D4v1d"
   {:data {:counter 3} :abilities [{:counter-cost 1 :msg "break 1 subroutine"}]}

   "DaVinci"
   {:events {:successful-run {:effect (effect (add-prop card :counter 1))}}
    :abilities [{:prompt "Choose a card to install"
                 :choices (req (filter #(and (<= (:cost %) (or (:counter card) 0))
                                             (#{"Hardware" "Program" "Resource"} (:type %)))
                                       (:hand runner)))
                 :msg (msg "install " (:title target) " at no cost")
                 :effect (effect (trash card) (runner-install target {:no-cost true}))}]}

   "Datasucker"
   {:events (let [ds {:effect (req (update! state side (dissoc card :datasucker-count)))}]
              {:successful-run {:effect (effect (add-prop card :counter 1))
                                :req (req (#{:hq :rd :archives} target))}
               :pre-ice-strength {:req (req (and (= (:cid target) (:cid current-ice))
                                                 (:datasucker-count card)))
                                  :effect (req (let [c (:datasucker-count (get-card state card))]
                                                 (ice-strength-bonus state side (- c))))}
               :pass-ice ds :run-ends ds})
    :abilities [{:counter-cost 1 :msg (msg "give -1 strength to " (:title current-ice))
                 :req (req (and current-ice (:rezzed current-ice)))
                 :effect (req (update! state side (update-in card [:datasucker-count] (fnil #(+ % 1) 0)))
                              (update-ice-strength state side current-ice))}]}

   "Deep Thought"
   {:events {:successful-run {:effect (effect (add-prop card :counter 1)) :req (req (= target :rd))}
             :runner-turn-begins
                             {:req (req (>= (get-virus-counters state side card) 3)) :msg "look at the top card of R&D"
                              :effect (effect (prompt! card (str "The top card of your R&D is "
                                                                 (:title (first (:deck corp)))) ["OK"] {}))}}}

   "Djinn"
   {:abilities [{:label "Add a virus program to your Grip from your Stack"
                 :prompt "Choose a Virus" :msg (msg "adds " (:title target) " to their Grip")
                 :choices (req (filter #(and (= (:type %) "Program")
                                             (has? % :subtype "Virus"))
                                       (:deck runner)))
                 :cost [:click 1 :credit 1] :effect (effect (move target :hand) (shuffle! :deck))}
                {:label "Install a non-Icebreaker program on Djinn" :cost [:click 1]
                 :prompt "Choose a non-Icebreaker program to install on Djinn"
                 :choices (req (filter #(and (= (:type %) "Program")
                                             (not (has? % :subtype "Icebreaker"))
                                             (<= (:cost %) (:credit runner)))
                                       (:hand runner)))
                 :msg (msg "install and host " (:title target))
                 :effect (effect (gain :memory (:memoryunits target))
                                 (runner-install target {:host-card card}))}
                {:label "Host an installed non-Icebreaker program on Djinn"
                 :prompt "Choose an installed non-Icebreaker program to host on Djinn"
                 :choices {:req #(and (= (:type %) "Program")
                                      (not (has? % :subtype "Icebreaker"))
                                      (:installed %))}
                 :msg (msg "host " (:title target)) :effect (effect (host card target))}]}

   "Expert Schedule Analyzer"
   {:abilities
    [{:cost [:click 1] :msg "make a run on HQ"
      :effect (effect (run :hq {:req (req (= target :hq))
                                :replace-access
                                {:msg (msg "reveal cards in HQ: " (map :title (:hand corp)))}} card))}]}

   "False Echo"
   {:abilities [{:req (req (and (:run @state)
                                (< (:position run) (count (:ices run)))
                                (not (:rezzed (nth (get-in @state
                                                     (vec (concat [:corp :servers] (:server run) [:ices]))) (:position run))))))
                 :msg "make the Corp rez the passed ICE or add it to HQ"
                 :effect (req (let [s (:server run)
                                    ice (nth (get-in @state (vec (concat [:corp :servers] s [:ices]))) (:position run))
                                    icename (:title ice)
                                    icecost (rez-cost state side ice)]
                                (resolve-ability
                                  state side
                                  {:prompt (msg "Rez " icename " or add it to HQ?") :player :corp
                                   :choices (req (if (< (:credit corp) icecost)
                                                     ["Add to HQ"]
                                                     ["Rez" "Add to HQ"]))
                                   :effect (req (if (= target "Rez")
                                                  (rez state side ice)
                                                  (do (move state :corp ice :hand nil)
                                                      (system-msg state :corp (str "chooses to add the passed ICE to HQ"))))
                                                (trash state side card))}
                                 card nil)))}]}

   "Gorman Drip v1"
   {:abilities [{:cost [:click 1] :effect (effect (gain :credit (get-virus-counters state side card))
                                                  (trash card {:cause :ability-cost}))
                 :msg (msg "gain " (get-virus-counters state side card) " [Credits]")}]
    :events {:corp-click-credit {:effect (effect (add-prop :runner card :counter 1))}
             :corp-click-draw {:effect (effect (add-prop :runner card :counter 1))}}}

   "Grappling Hook"
   {:abilities [{:msg "break all but 1 subroutine" :effect (effect (trash card {:cause :ability-cost}))}]}

   "Gravedigger"
   {:events (let [e {:req (req (and (= (first (:zone target)) :servers) (= (:side target) "Corp")))
                               :effect (effect (add-prop :runner card :counter 1))}]
              {:runner-trash e :corp-trash e})
    :abilities [{:counter-cost 1 :cost [:click 1] :msg "force the Corp to trash the top card of R&D"
                 :effect (effect (mill :corp))}]}

    "Harbinger"
    {:trash-effect
      {:req (req (not (some #{:facedown} (:previous-zone card))))
       :effect (effect (runner-install card {:facedown true}))}}

   "Hemorrhage"
   {:events {:successful-run {:effect (effect (add-prop card :counter 1))}}
    :abilities [{:counter-cost 2 :cost [:click 1] :msg "force the Corp to trash 1 card from HQ"
                 :effect (req (resolve-ability
                                state :corp
                                {:prompt "Choose a card to trash"
                                 :choices (req (filter #(:hand corp)))
                                 :effect (effect (trash target))}
                               card nil))}]}

   "Hivemind"
   {:data {:counter 1 :counter-type "Virus"}
    :abilities [{:req (req (> (:counter card) 0)) :priority true
                 :prompt "Move a virus counter to which card?"
                 :choices {:req #(has? % :subtype "Virus")}
                 :effect (req (let [abilities (:abilities (card-def target))
                                    virus target]
                                (add-prop state :runner virus :counter 1)
                                (add-prop state :runner card :counter -1)
                                (if (= (count abilities) 1)
                                  (do (swap! state update-in [side :prompt] rest) ; remove the Hivemind prompt so Imp works
                                      (resolve-ability state side (first abilities) (get-card state virus) nil))
                                  (resolve-ability
                                    state side
                                    {:prompt "Choose an ability to trigger"
                                     :choices (vec (map :msg abilities))
                                     :effect (req (swap! state update-in [side :prompt] rest)
                                                  (resolve-ability
                                                    state side
                                                    (first (filter #(= (:msg %) target) abilities))
                                                    card nil))}
                                    (get-card state virus) nil))))
                 :msg (msg "to trigger an ability on " (:title target))}]}

   "Hyperdriver"
   {:abilities [{:label "Remove Hyperdriver from the game to gain [Click] [Click] [Click]"
                 :effect (effect (move card :rfg) (gain :memory 3 :click 3))
                 :msg "gain [Click] [Click] [Click]"}]}

   "Imp"
   {:data {:counter 2}
    :abilities [{:counter-cost 1 :msg "trash at no cost" :once :per-turn
                 :effect (effect (trash-no-cost))}]}

   "Incubator"
   {:events {:runner-turn-begins {:effect (effect (add-prop card :counter 1))}}
    :abilities [{:cost [:click 1]
                 :msg (msg "move " (:counter card) " virus counter to " (:title target))
                 :choices {:req #(and (:installed %) (has? % :subtype "Virus"))}
                 :effect (effect (trash card {:cause :ability-cost}) (add-prop target :counter (:counter card)))}]}

   "Ixodidae"
   {:events {:corp-loss {:req (req (= (first target) :credit)) :msg "to gain 1 [Credits]"
                         :effect (effect (gain :runner :credit 1))}
             :purge {:effect (effect (trash card))}}}

   "Keyhole"
   {:abilities [{:cost [:click 1] :msg "make a run on R&D"
                 :effect (effect (run :rd
                                   {:req (req (= target :rd))
                                    :replace-access
                                    {:prompt "Choose a card to trash" :not-distinct true
                                     :msg (msg "trash " (:title target))
                                     :choices (req (take 3 (:deck corp))) :mandatory true
                                     :effect (effect (trash (assoc target :seen true))
                                                     (shuffle! :corp :deck))}} card))}]}

   "Lamprey"
   {:events {:successful-run {:req (req (= target :hq)) :msg "to force the Corp to lose 1 [Credits]"
                              :effect (effect (lose :corp :credit 1))}
             :purge {:effect (effect (trash card))}}}

   "Leprechaun"
   {:abilities [{:label "Install a program on Leprechaun"
                 :req (req (<= (count (:hosted card)) 2)) :cost [:click 1]
                 :prompt "Choose a program to install on Leprechaun"
                 :choices (req (filter #(and (= (:type %) "Program")
                                             (<= (:cost %) (:credit runner)))
                                       (:hand runner)))
                 :msg (msg "host " (:title target))
                 :effect (effect (gain :memory (:memoryunits target))
                                 (runner-install target {:host-card card}))}
                {:label "Host an installed program on Leprechaun"
                 :req (req (<= (count (:hosted card)) 2))
                 :prompt "Choose a program to host on Leprechaun"
                 :choices {:req #(and (= (:type %) "Program") (:installed %))}
                 :msg (msg "host " (:title target)) :effect (effect (host card target))}]}

   "LLDS Energy Regulator"
   {:prevent {:trash [:hardware]}
    :abilities [{:cost [:credit 3] :msg "prevent a hardware from being trashed"}
                {:effect (effect (trash card {:cause :ability-cost})) :msg "prevent a hardware from being trashed"}]}

   "Magnum Opus"
   {:abilities [{:cost [:click 1] :effect (effect (gain :credit 2)) :msg "gain 2 [Credits]"}]}

   "Medium"
   {:events
    {:successful-run {:req (req (= target :rd))
                      :effect (effect (add-prop card :counter 1))}
     :pre-access {:req (req (= target :rd))
                  :effect (effect (access-bonus (max 0 (dec (get-virus-counters state side (get-card state card))))))}}}

   "Multithreader"
   {:recurring 2}

   "Nerve Agent"
   {:events
    {:successful-run {:req (req (= target :hq))
                      :effect (effect (add-prop card :counter 1))}
     :pre-access {:req (req (= target :hq))
                  :effect (effect (access-bonus (max 0 (dec (get-virus-counters state side (get-card state card))))))}}}

   "Net Shield"
   {:prevent {:damage [:net]}
    :abilities [{:cost [:credit 1] :once :per-turn :msg "prevent the first net damage this turn"
                 :effect (effect (damage-prevent :net 1))}]}

   "Origami"
   {:effect (effect (gain :max-hand-size
                          (dec (* 2 (count (filter #(= (:title %) "Origami")
                                                   (all-installed state :runner)))))))
    :leave-play (effect (lose :max-hand-size
                              (dec (* 2 (count (filter #(= (:title %) "Origami")
                                                       (all-installed state :runner)))))))}

   "Paintbrush"
   {:abilities [{:cost [:click 1]
                 :choices {:req #(and (= (first (:zone %)) :servers) (has? % :type "ICE") (:rezzed %))}
                 :effect (req (let [ice target
                                    stypes (:subtype ice)]
                           (resolve-ability
                              state :runner
                              {:prompt (msg "Choose a subtype")
                               :choices ["Sentry" "Code Gate" "Barrier"]
                               :msg (msg "give " (:title ice) " " (.toLowerCase target) " until the end of the next run this turn")
                               :effect (effect (update! (assoc ice :subtype
                                                                   (->> (vec (.split (:subtype ice) " - "))
                                                                        (cons target)
                                                                        distinct
                                                                        (join " - "))))
                                               (register-events {:run-ends
                                                                 {:effect (effect (update! (assoc ice :subtype stypes))
                                                                                  (unregister-events card))}} card))}
                              card nil)))}]
    :events {:run-ends nil}}

   "Parasite"
   {:hosting {:req #(and (= (:type %) "ICE") (:rezzed %))}
    :effect (req (when-let [h (:host card)]
                   (update! state side (assoc-in card [:special :installing] true))
                   (update-ice-strength state side h)
                   (when-let [card (get-card state card)]
                     (update! state side (update-in card [:special] dissoc :installing)))))
    :events {:runner-turn-begins
             {:effect (req (add-prop state side card :counter 1))}
             :counter-added
             {:req (req (or (= (:title target) "Hivemind") (= (:cid target) (:cid card))))
              :effect (effect (update-ice-strength (:host card)))}
             :pre-ice-strength
             {:req (req (= (:cid target) (:cid (:host card))))
              :effect (effect (ice-strength-bonus (- (get-virus-counters state side card))))}
             :ice-strength-changed
             {:req (req (and (= (:cid target) (:cid (:host card))) (<= (:current-strength target) 0)))
              :effect (req (unregister-events state side card)
                           (when (get-in card [:special :installing])
                             (update! state side (update-in card [:special] dissoc :installing))
                             (trigger-event state side :runner-install card))
                           (trash state side target))
              :msg (msg "trash " (:title target))}}}

   "Paricia"
   {:recurring 2}

   "Pawn"
   {:abilities [{:label "Host Pawn on a piece of ICE" :cost [:click 1]
                 :choices {:req #(and (= (:type %) "ICE")
                                      (= (last (:zone %)) :ices)
                                      (not (some (fn [c] (has? c :subtype "Caïssa")) (:hosted %))))}
                 :msg (msg "host it on " (if (:rezzed target) (:title target) "a piece of ICE"))
                 :effect (effect (host target card))}]}

   "Pheromones"
   {:recurring (effect (set-prop card :rec-counter (:counter card)))
    :events {:successful-run {:req (req (= target :hq))
                              :effect (effect (add-prop card :counter 1))}}}

   "Progenitor"
   {:abilities [{:label "Install a virus program on Progenitor"
                 :cost [:click 1] :req (req (empty? (:hosted card)))
                 :prompt "Choose a Virus program to install on Progenitor"
                 :choices (req (filter #(and (= (:type %) "Program")
                                             (has? % :subtype "Virus")
                                             (<= (:cost %) (:credit runner)))
                                       (:hand runner)))
                 :msg (msg "host " (:title target))
                 :effect (effect (gain :memory (:memoryunits target))
                                 (runner-install target {:host-card card}))}
                {:label "Host an installed virus on Progenitor" :req (req (empty? (:hosted card)))
                 :prompt "Choose an installed virus program to host on Progenitor"
                 :choices {:req #(and (= (:type %) "Program")
                                      (has? % :subtype "Virus")
                                      (:installed %))}
                 :msg (msg "host " (:title target)) :effect (effect (host card target))}]
    :events {:pre-purge {:effect (req (when-let [c (first (:hosted card))]
                                        (update! state side (assoc-in card [:special :numpurged] (:counter c)))))}
             :purge {:req (req (pos? (or (get-in card [:special :numpurged]) 0)))
                     :effect (req (when-let [c (first (:hosted card))]
                                    (add-prop state side c :counter 1)))}}}


   "Rook"
   {:abilities [{:label "Host Rook on a piece of ICE" :cost [:click 1]
                 :choices {:req #(and (= (:type %) "ICE")
                                      (= (last (:zone %)) :ices)
                                      (not (some (fn [c] (has? c :subtype "Caïssa")) (:hosted %))))}
                 :msg (msg "host it on " (if (:rezzed target) (:title target) "a piece of ICE"))
                 :effect (effect (host target card))}]
    :events {:pre-rez-cost {:req (req (= (:zone (:host card)) (:zone target)))
                            :effect (effect (rez-cost-bonus 2))}}}

   "Sahasrara"
   {:recurring 2}

   "Savoir-faire"
   {:abilities [{:cost [:credit 2] :once :per-turn :msg (msg "install " (:title target))
                 :prompt "Choose a program to install"
                 :choices (req (filter #(= (:type %) "Program") (:hand runner)))
                 :effect (effect (runner-install target))}]}

   "Scheherazade"
   {:abilities [{:label "Install and host a program from Grip"
                 :cost [:click 1] :prompt "Choose a program to install on Scheherazade"
                 :choices (req (filter #(and (has? % :type "Program")
                                             (<= (:cost %) (:credit runner))
                                             (<= (:memoryunits %) (:memory runner)))
                                       (:hand runner)))
                 :msg (msg "host " (:title target) " and gain 1 [Credits]")
                 :effect (effect (runner-install target {:host-card card}) (gain :credit 1))}
                {:label "Host an installed program"
                 :prompt "Choose a program to host on Scheherazade"
                 :choices {:req #(and (= (:type %) "Program") (:installed %))}
                 :msg (msg "host " (:title target) " and gain 1 [Credits]")
                 :effect (effect (host card target) (gain :credit 1))}]}

   "Self-modifying Code"
   {:abilities [{:prompt "Choose a program to install" :msg (msg "install " (:title target))
                 :priority true
                 :choices (req (filter #(has? % :type "Program") (:deck runner)))
                 :cost [:credit 2]
                 :effect (effect (trash card {:cause :ability-cost}) (runner-install target) (shuffle! :deck))}]}

   "Sneakdoor Beta"
   {:abilities [{:cost [:click 1] :msg "make a run on Archives"
                 :effect (effect (run :archives
                                   {:req (req (= target :archives))
                                    :successful-run
                                    {:msg "make a successful run on HQ"
                                     :effect (req (swap! state assoc-in [:run :server] [:hq])
                                                  (update-run-ice state side))}} card))}]}

   "Snitch"
   {:abilities [{:once :per-run :req (req current-ice) :msg (msg "expose " (:title current-ice))
                 :effect (effect (expose current-ice)
                                 (resolve-ability {:optional {:prompt "Jack out?"
                                                              :yes-ability {:msg "jack out"
                                                                            :effect (effect (jack-out nil))}}}
                                                  card nil))}]}

   "Surfer"
   {:abilities [{:cost [:credit 2]
                 :req (req (and (:run @state) (:rezzed current-ice) (has? current-ice :subtype "Barrier")))
                 :label "Swap the barrier ICE currently being encountered with a piece of ICE directly before or after it"
                 :effect (req (let [cice current-ice]
                                (resolve-ability
                                  state side
                                  {:prompt (msg "Choose an ICE before or after " (:title cice))
                                   :choices {:req #(and (= (:type %) "ICE")
                                                        (= (:zone %) (:zone cice))
                                                        (= 1 (abs (- (ice-index state %) (ice-index state cice)))))}
                                   :msg "swap a piece of barrier ICE"
                                   :effect (req (let [tgtndx (ice-index state target)
                                                      oldndx (ice-index state cice)]
                                                  (swap! state update-in (cons :corp (:zone cice))
                                                      #(assoc % tgtndx cice))
                                                  (swap! state update-in (cons :corp (:zone cice))
                                                      #(assoc % oldndx target))
                                                  (swap! state update-in [:run]
                                                      #(assoc % :position (inc tgtndx)))
                                                  (update-ice-strength state side (cons :corp (:zone cice)))
                                                  (update-run-ice state side)))}
                                 card nil)))}]}

   "Trope"
   {:events {:runner-turn-begins {:effect (effect (add-prop card :counter 1))}}
    :abilities [{:label "Remove Trope from the game to reshuffle cards from Heap back into Stack"
                 :effect (effect
                          (move card :rfg)
                          (resolve-ability
                           {:show-discard true
                            :choices {:max (:counter card) :req #(and (:side % "Runner") (= (:zone %) [:discard]))}
                            :msg (msg "shuffle " (join ", " (map :title targets))
                                      " into their Stack")
                            :effect (req (doseq [c targets] (move state side c :deck))
                                         (shuffle! state side :deck))}
                           card nil))}]}})
