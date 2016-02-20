(in-ns 'game.core)

(def cards-programs
  {"Analog Dreamers"
   {:abilities [{:cost [:click 1] :msg "make a run on R&D"
                 :effect (effect (run :rd {:req (req (= target :rd))
                                           :replace-access
                                           {:prompt "Choose a card to shuffle into R&D"
                                            :choices {:req #(and (not (ice? %))
                                                                 (not (rezzed? %))
                                                                 (not (:advance-counter %)))}
                                            :effect (req (move state :corp target :deck)
                                                         (shuffle! state :corp :deck)
                                                         (swap! state update-in [:runner :prompt] rest)
                                                         (handle-end-run state side)) ; remove the replace-access prompt
                                            :msg "shuffle a card into R&D"}} card))}]}

   "Au Revoir"
   {:events {:jack-out {:effect (effect (gain :credit 1)) :msg "gain 1 [Credits]"}}}

   "Bishop"
   {:abilities [{:cost [:click 1]
                 :effect (req (let [b (get-card state card)
                                    hosted? (ice? (:host b))
                                    remote? (is-remote? (second (:zone (:host b))))]
                                (resolve-ability state side
                                 {:prompt (msg "Host Bishop on a piece of ICE protecting "
                                            (if hosted? (if remote? "a central" "a remote") "any") " server")
                                  :choices {:req #(if hosted?
                                                    (and (if remote?
                                                           (is-central? (second (:zone %)))
                                                           (is-remote? (second (:zone %))))
                                                         (ice? %)
                                                         (= (last (:zone %)) :ices)
                                                         (not (some (fn [c] (has-subtype? c "Caïssa"))
                                                                    (:hosted %))))
                                                    (and (ice? %)
                                                         (= (last (:zone %)) :ices)
                                                         (not (some (fn [c] (has-subtype? c :subtype "Caïssa"))
                                                                    (:hosted %)))))}
                                  :msg (msg "host it on " (card-str state target))
                                  :effect (effect (host target card))} card nil)))}]
    :events {:pre-ice-strength
             {:req (req (and (= (:cid target) (:cid (:host card))) (:rezzed target)))
              :effect (effect (ice-strength-bonus -2 target))}}}

   "Bug"
   {:req (req (some #{:hq} (:successful-run runner-reg)))
    :events {:corp-draw {:optional
                         {:prompt (msg "Pay 2 [Credits] to reveal card just drawn?") :player :runner
                          :yes-ability {:msg (msg "reveal the card just drawn: " (:title (last (:hand corp))))
                                        :cost [:credit 2]}}}}}

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
              :effect (effect (update-all-advancement-costs))}
             :purge {:effect (effect (update-all-advancement-costs))}}}

   "Cloak"
   {:recurring 1}

   "Clot"
   {:effect (req (let [agendas (map first (filter #(is-type? (first %) "Agenda")
                                                  (turn-events state :corp :corp-install)))]
                   (swap! state assoc-in [:corp :register :cannot-score] agendas)))
    :events {:purge {:effect (req (swap! state update-in [:corp :register] dissoc :cannot-score)
                                  (trash state side card))}
             :corp-install {:req (req (is-type? target "Agenda"))
                            :effect (req (swap! state update-in [:corp :register :cannot-score] #(cons target %)))}}
    :leave-play (req (swap! state update-in [:corp :register] dissoc :cannot-score))}

   "Collective Consciousness"
   {:events {:rez {:req (req (ice? target)) :msg "draw 1 card"
                   :effect (effect (draw :runner))}}}

   "Copycat"
   {:abilities [{:req (req (and (:run @state)
                                (:rezzed current-ice)))
                 :effect (req (let [icename (:title current-ice)]
                                (resolve-ability
                                  state side
                                  {:prompt (msg "Choose a rezzed copy of " icename)
                                   :choices {:req #(and (rezzed? %)
                                                        (ice? %)
                                                        (= (:title %) icename))}
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
   {:data {:counter 3}
    :abilities [{:counter-cost 1 :msg "break 1 subroutine"}]}

   "DaVinci"
   {:events {:successful-run {:effect (effect (add-prop card :counter 1))}}
    :abilities [{:effect
                 (req (let [c card]
                        (resolve-ability state side
                                         {:prompt "Choose a card to install from your Grip"
                                          :choices {:req #(and (<= (:cost %) (get c :counter 0))
                                                               (#{"Hardware" "Program" "Resource"} (:type %))
                                                               (in-hand? %))}
                                          :msg (msg "install " (:title target) " at no cost")
                                          :effect (effect (trash card {:cause :ability-cost})
                                                          (runner-install target {:no-cost true}))}
                                         card nil)))}]}

   "Datasucker"
   {:events (let [ds {:effect (req (update! state side (dissoc card :datasucker-count)))}]
              {:successful-run {:effect (effect (add-prop card :counter 1))
                                :req (req (#{:hq :rd :archives} target))}
               :pre-ice-strength {:req (req (and (= (:cid target) (:cid current-ice))
                                                 (:datasucker-count card)))
                                  :effect (req (let [c (:datasucker-count (get-card state card))]
                                                 (ice-strength-bonus state side (- c) target)))}
               :pass-ice ds :run-ends ds})
    :abilities [{:counter-cost 1 :msg (msg "give -1 strength to " (:title current-ice))
                 :req (req (and current-ice (:rezzed current-ice)))
                 :effect (req (update! state side (update-in card [:datasucker-count] (fnil #(+ % 1) 0)))
                              (update-ice-strength state side current-ice))}]}

   "Deep Thought"
   {:events {:successful-run {:effect (effect (add-prop card :counter 1)) :req (req (= target :rd))}
             :runner-turn-begins
                             {:req (req (>= (get-virus-counters state side card) 3)) :msg "look at the top card of R&D"
                              :effect (effect (prompt! card (str "The top card of R&D is "
                                                                 (:title (first (:deck corp)))) ["OK"] {}))}}}

   "Diwan"
   {:prompt "Choose the server that this copy of Diwan is targeting:"
    :choices (req servers)
    :effect (effect (update! (assoc card :named-target target)))
    :leave-play (effect (update! (dissoc card :named-target)))
    :events {:purge {:effect (effect (trash card))}}}

   "Djinn"
   {:abilities [{:label "Add a virus program to your Grip from your Stack"
                 :prompt "Choose a Virus"
                 :msg (msg "adds " (:title target) " to their Grip")
                 :choices (req (cancellable (filter #(and (is-type? % "Program")
                                                          (has-subtype? % "Virus"))
                                                    (:deck runner)) :sorted))
                 :cost [:click 1 :credit 1] :effect (effect (move target :hand) (shuffle! :deck))}
                {:label "Install a non-Icebreaker program on Djinn"
                 :cost [:click 1]
                 :prompt "Choose a non-Icebreaker program in your Grip to install on Djinn"
                 :choices {:req #(and (is-type? % "Program")
                                      (not (has-subtype? % "Icebreaker"))
                                      (in-hand? %))}
                 :msg (msg "install and host " (:title target))
                 :effect (effect (gain :memory (:memoryunits target))
                                 (runner-install target {:host-card card})
                                 (update! (assoc (get-card state card)
                                                 :hosted-programs (cons (:cid target) (:hosted-programs card)))))}
                {:label "Host an installed non-Icebreaker program on Djinn"
                 :prompt "Choose an installed non-Icebreaker program to host on Djinn"
                 :choices {:req #(and (is-type? % "Program")
                                      (not (has-subtype? % "Icebreaker"))
                                      (installed? %))}
                 :msg (msg "host " (:title target))
                 :effect (effect (host card target)
                                 (gain :memory (:memoryunits target))
                                 (update! (assoc (get-card state card)
                                                 :hosted-programs (cons (:cid target) (:hosted-programs card)))))}]
    :events {:card-moved {:req (req (some #{(:cid target)} (:hosted-programs card)))
                          :effect (effect (update! (assoc card
                                                          :hosted-programs (remove #(= (:cid target) %) (:hosted-programs card))))
                                          (lose :memory (:memoryunits target)))}}}

   "Expert Schedule Analyzer"
   {:abilities
    [{:cost [:click 1] :msg "make a run on HQ"
      :effect (effect (run :hq {:req (req (= target :hq))
                                :replace-access
                                {:msg (msg "reveal cards in HQ: "
                                           (join ", " (map :title (:hand corp))))}} card))}]}

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
   {:events (let [e {:req (req (and (installed? target) (= (:side target) "Corp")))
                               :effect (effect (add-prop :runner card :counter 1))}]
              {:runner-trash e :corp-trash e})
    :abilities [{:counter-cost 1 :cost [:click 1] :msg "force the Corp to trash the top card of R&D"
                 :effect (effect (mill :corp))}]}

   "Harbinger"
   {:trash-effect
     {:req (req (not (some #{:facedown :hand} (:previous-zone card))))
      :effect (req (resolve-ability state :runner
                     {:effect (effect (runner-install card {:facedown true}))}
                    card nil))}}

   "Hemorrhage"
   {:events {:successful-run {:effect (effect (add-prop card :counter 1))}}
    :abilities [{:counter-cost 2 :cost [:click 1] :msg "force the Corp to trash 1 card from HQ"
                 :effect (req (resolve-ability
                                state :corp
                                {:prompt "Choose a card to trash"
                                 :choices (req (filter #(= (:side %) "Corp") (:hand corp)))
                                 :effect (effect (trash target))}
                               card nil))}]}

   "Hivemind"
   {:data {:counter 1 :counter-type "Virus"}
    :abilities [{:req (req (> (:counter card) 0)) :priority true
                 :prompt "Move a virus counter to which card?"
                 :choices {:req #(has-subtype? % "Virus")}
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
                 :choices {:req #(and (installed? %)
                                      (has-subtype? % "Virus"))}
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
   {:events {:successful-run {:req (req (= target :hq)) :msg "force the Corp to lose 1 [Credits]"
                              :effect (effect (lose :corp :credit 1))}
             :purge {:effect (effect (trash card))}}}

   "Leprechaun"
   {:abilities [{:label "Install a program on Leprechaun"
                 :req (req (< (count (:hosted card)) 2))
                 :cost [:click 1]
                 :prompt "Choose a program in your Grip to install on Leprechaun"
                 :choices {:req #(and (is-type? % "Program")
                                      (in-hand? %))}
                 :msg (msg "host " (:title target))
                 :effect (effect (gain :memory (:memoryunits target))
                                 (runner-install target {:host-card card})
                                 (update! (assoc (get-card state card)
                                                 :hosted-programs (cons (:cid target) (:hosted-programs card)))))}
                {:label "Host an installed program on Leprechaun"
                 :req (req (< (count (:hosted card)) 2))
                 :prompt "Choose an installed program to host on Leprechaun"
                 :choices {:req #(and (is-type? % "Program")
                                      (installed? %))}
                 :msg (msg "host " (:title target))
                 :effect (effect (host card target)
                                 (gain :memory (:memoryunits target))
                                 (update! (assoc (get-card state card)
                                                 :hosted-programs (cons (:cid target) (:hosted-programs card)))))}]
    :events {:card-moved {:req (req (some #{(:cid target)} (:hosted-programs card)))
                          :effect (effect (update! (assoc card
                                                          :hosted-programs (remove #(= (:cid target) %) (:hosted-programs card))))
                                          (lose :memory (:memoryunits target)))}}}

   "LLDS Energy Regulator"
   {:prevent {:trash [:hardware]}
    :abilities [{:cost [:credit 3] :msg "prevent a hardware from being trashed"}
                {:effect (effect (trash card {:cause :ability-cost})) :msg "prevent a hardware from being trashed"}]}

   "Magnum Opus"
   {:abilities [{:cost [:click 1] :effect (effect (gain :credit 2)) :msg "gain 2 [Credits]"}]}

   "Medium"
   {:effect (req (when (and run
                            (= (first (get-in @state [:run :server])) :rd)
                            (not current-ice))
                   (update! state side (assoc card :medium-active true))))
    :events {:no-action {:req (req (and run
                                        (= (first (get-in @state [:run :server])) :rd)
                                        (not current-ice)
                                        (> (get-virus-counters state side card) 0)))
                         :effect (req (toast state :runner "Click Medium to choose fewer than all additional R&D accesses." "info")
                                      (update! state side (assoc card :medium-active true)))}
             :successful-run {:req (req (and (= target :rd)
                                             (or (:medium-active card) (nil? (:counter card)) (= 0 (:counter card)))))
                              :effect (effect (add-prop card :counter 1))}
             :pre-access {:req (req (and (= target :rd) (:medium-active card)))
                          :effect (effect (access-bonus (max 0 (dec (get-virus-counters state side (get-card state card))))))}}
    :abilities [{:req (req (and run
                                (= (first (get-in @state [:run :server])) :rd)
                                (not current-ice)
                                (:medium-active card)
                                (empty? (get-in @state [:runner :prompt]))))
                 :effect (effect (add-prop card :counter 1)
                                 (resolve-ability
                                   {:prompt "Choose how many additional R&D accesses to make"
                                    :choices {:number (req (get-virus-counters state side card))}
                                    :msg (msg "do " target " additional accesses from R&D")
                                    :effect (effect (access-bonus (max 0 target))
                                                    (update! (dissoc (get-card state card) :medium-active)))} card nil))}]}

   "Multithreader"
   {:recurring 2}

   "Nerve Agent"
   {:effect (req (when (and run
                            (= (first (get-in @state [:run :server])) :hq)
                            (not current-ice))
                   (update! state side (assoc card :nerve-active true))))
    :events {:no-action {:req (req (and run
                                        (= (first (get-in @state [:run :server])) :hq)
                                        (not current-ice)
                                        (> (get-virus-counters state side card) 0)))
                         :effect (req (toast state :runner "Click Nerve Agent to choose fewer than all additional HQ accesses." "info")
                                      (update! state side (assoc card :nerve-active true)))}
             :successful-run {:req (req (and (= target :hq)
                                             (or (:nerve-active card) (nil? (:counter card)) (= 0 (:counter card)))))
                              :effect (effect (add-prop card :counter 1))}
             :pre-access {:req (req (and (= target :hq) (:nerve-active card)))
                          :effect (effect (access-bonus (max 0 (dec (get-virus-counters state side (get-card state card))))))}}
    :abilities [{:req (req (and run
                                (= (first (get-in @state [:run :server])) :hq)
                                (not current-ice)
                                (:nerve-active card)
                                (empty? (get-in @state [:runner :prompt]))))
                 :effect (effect (add-prop card :counter 1)
                                 (resolve-ability
                                   {:prompt "Choose how many additional HQ accesses to make"
                                    :choices {:number (req (get-virus-counters state side card))}
                                    :msg (msg "do " target " additional accesses from HQ")
                                    :effect (effect (access-bonus (max 0 target))
                                                    (update! (dissoc (get-card state card) :nerve-active)))} card nil))}]}

   "Net Shield"
   {:prevent {:damage [:net]}
    :abilities [{:cost [:credit 1] :once :per-turn :msg "prevent the first net damage this turn"
                 :effect (effect (damage-prevent :net 1))}]}

   "Origami"
   {:effect (effect (gain :hand-size-modification
                          (dec (* 2 (count (filter #(= (:title %) "Origami")
                                                   (all-installed state :runner)))))))
    :leave-play (effect (lose :hand-size-modification
                              (dec (* 2 (count (filter #(= (:title %) "Origami")
                                                       (all-installed state :runner)))))))}

   "Paintbrush"
   {:abilities [{:cost [:click 1]
                 :choices {:req #(and (installed? %) (ice? %) (rezzed? %))}
                 :effect (req (let [ice target
                                    stypes (:subtype ice)]
                           (resolve-ability
                              state :runner
                              {:prompt (msg "Choose a subtype")
                               :choices ["Sentry" "Code Gate" "Barrier"]
                               :msg (msg "make " (card-str state ice) " gain " (.toLowerCase target) " until the end of the next run this turn")
                               :effect (effect (update! (assoc ice :subtype
                                                                   (->> (vec (.split (:subtype ice) " - "))
                                                                        (cons target)
                                                                        distinct
                                                                        (join " - "))))
                                               (update-ice-strength (get-card state ice))
                                               (register-events {:run-ends
                                                                 {:effect (effect (update! (assoc ice :subtype stypes))
                                                                                  (unregister-events card)
                                                                                  (update-ice-strength (get-card state ice)))}} card))}
                            card nil)))}]
    :events {:run-ends nil}}

   "Panchatantra"
   {:abilities [{:msg "add a custom subtype to currently encountered ICE"
                 :once :per-turn}]}

   "Parasite"
   {:hosting {:req #(and (ice? %) (rezzed? %))}
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
              :effect (effect (ice-strength-bonus (- (get-virus-counters state side card)) target))}
             :ice-strength-changed
             {:req (req (and (= (:cid target) (:cid (:host card)))
                             (not (card-flag? (:host card) :untrashable-while-rezzed true))
                             (<= (:current-strength target) 0)))
              :effect (req (unregister-events state side card)
                           (when (get-in card [:special :installing])
                             (update! state side (update-in card [:special] dissoc :installing))
                             (trigger-event state side :runner-install card))
                           (trash state side target)
                           (trash-ice-in-run state))
              :msg (msg "trash " (:title target))}}}

   "Paricia"
   {:recurring 2}

   "Pawn"
   {:abilities [{:label "Host Pawn on the outermost ICE of a central server"
                 :prompt "Host Pawn on the outermost ICE of a central server" :cost [:click 1]
                 :choices {:req #(and (ice? %)
                                      (= (last (:zone %)) :ices)
                                      (is-central? (second (:zone %))))}
                 :msg (msg "host it on " (card-str state target))
                 :effect (effect (host target card))}
                {:label "Advance to next ICE"
                 :prompt "Choose the next innermost ICE to host Pawn on it"
                 :choices {:req #(and (ice? %)
                                      (= (last (:zone %)) :ices)
                                      (is-central? (second (:zone %))))}
                 :msg (msg "host it on " (card-str state target))
                 :effect (effect (host target card))}
                {:label "Trash Pawn and install a Caïssa from your Grip or Heap, ignoring all costs"
                 :effect (req (let [this-pawn (:cid card)]
                                (resolve-ability
                                  state side
                                  {:prompt "Choose a Caïssa program to install from your Grip or Heap"
                                   :show-discard true
                                   :choices {:req #(and (has-subtype? % "Caïssa")
                                                        (not= (:cid %) this-pawn)
                                                        (#{[:hand] [:discard]} (:zone %)))}
                                   :msg (msg "install " (:title target))
                                   :effect (effect (runner-install target {:no-cost true}))} card nil)
                                (trash state side card)))}]}

   "Pheromones"
   {:recurring (req (when (< (get card :rec-counter 0) (:counter card))
                      (set-prop state side card :rec-counter (:counter card))))
    :events {:successful-run {:req (req (= target :hq))
                              :effect (effect (add-prop card :counter 1))}}}

   "Progenitor"
   {:abilities [{:label "Install a virus program on Progenitor"
                 :cost [:click 1]
                 :req (req (empty? (:hosted card)))
                 :prompt "Choose a Virus program to install on Progenitor"
                 :choices {:req #(and (is-type? % "Program")
                                      (has-subtype? % "Virus")
                                      (in-hand? %))}
                 :msg (msg "host " (:title target))
                 :effect (effect (gain :memory (:memoryunits target))
                                 (runner-install target {:host-card card}))}
                {:label "Host an installed virus on Progenitor"
                 :req (req (empty? (:hosted card)))
                 :prompt "Choose an installed virus program to host on Progenitor"
                 :choices {:req #(and (is-type? % "Program")
                                      (has-subtype? % "Virus")
                                      (installed? %))}
                 :msg (msg "host " (:title target))
                 :effect (effect (host card target))}]
    :events {:pre-purge {:effect (req (when-let [c (first (:hosted card))]
                                        (update! state side (assoc-in card [:special :numpurged] (:counter c)))))}
             :purge {:req (req (pos? (or (get-in card [:special :numpurged]) 0)))
                     :effect (req (when-let [c (first (:hosted card))]
                                    (add-prop state side c :counter 1)))}}}


   "Rook"
   {:abilities [{:label "Host Rook on a piece of ICE" :cost [:click 1]
                 :choices {:req #(and (ice? %)
                                      (= (last (:zone %)) :ices)
                                      (not (some (fn [c] (has-subtype? c "Caïssa"))
                                                 (:hosted %))))}
                 :msg (msg "host it on " (card-str state target))
                 :effect (effect (host target card))}]
    :events {:pre-rez-cost {:req (req (= (:zone (:host card)) (:zone target)))
                            :effect (effect (rez-cost-bonus 2))}}}

   "Sahasrara"
   {:recurring 2}

   "Savoir-faire"
   {:abilities [{:cost [:credit 2]
                 :once :per-turn
                 :msg (msg "install " (:title target))
                 :prompt "Choose a program to install from your grip"
                 :choices {:req #(and (is-type? % "Program")
                                      (in-hand? %))}
                 :effect (effect (runner-install target))}]}

   "Scheherazade"
   {:abilities [{:label "Install and host a program from Grip"
                 :cost [:click 1]
                 :prompt "Choose a program to install on Scheherazade from your grip"
                 :choices {:req #(and (is-type? % "Program")
                                      (in-hand? %))}
                 :msg (msg "host " (:title target) " and gain 1 [Credits]")
                 :effect (effect (runner-install target {:host-card card}) (gain :credit 1))}
                {:label "Host an installed program"
                 :prompt "Choose a program to host on Scheherazade"
                 :choices {:req #(and (is-type? % "Program")
                                      (installed? %))}
                 :msg (msg "host " (:title target) " and gain 1 [Credits]")
                 :effect (req (when (host state side card target)
                                (gain state side :credit 1)))}]}

   "Self-modifying Code"
   {:abilities [{:prompt "Choose a program to install" :msg (msg "install " (:title target))
                 :priority true
                 :choices (req (cancellable (filter #(is-type? % "Program") (:deck runner)) :sorted))
                 :cost [:credit 2]
                 :effect (effect (trash card {:cause :ability-cost}) (runner-install target) (shuffle! :deck))}]}

   "Sneakdoor Beta"
   {:abilities [{:cost [:click 1] :msg "make a run on Archives"
                 :effect (effect (run :archives
                                   {:req (req (= target :archives))
                                    :successful-run
                                    {:effect (req (swap! state assoc-in [:run :server] [:hq])
                                                  (update-run-ice state side)
                                                  (system-msg state side
                                                              (str "uses Sneakdoor Beta to make a successful run on HQ")))}}
                                  card))}]}

   "Snitch"
   {:abilities [{:once :per-run :req (req current-ice) :msg (msg "expose " (:title current-ice))
                 :effect (effect (expose current-ice)
                                 (resolve-ability {:optional {:prompt "Jack out?"
                                                              :yes-ability {:msg "jack out"
                                                                            :effect (effect (jack-out nil))}}}
                                                  card nil))}]}

   "Surfer"
   (letfn [(surf [state cice]
             {:prompt (msg "Choose an ICE before or after " (:title cice))
              :choices {:req #(and (ice? %)
                                   (= (:zone %) (:zone cice))
                                   (= 1 (abs (- (ice-index state %)
                                                (ice-index state cice)))))}
              :msg "swap a piece of barrier ICE"
              :effect (req (let [tgtndx (ice-index state target)
                                 cidx (ice-index state cice)] 
                             (swap! state update-in (cons :corp (:zone cice))
                                    #(assoc % tgtndx cice))
                             (swap! state update-in (cons :corp (:zone cice))
                                    #(assoc % cidx target))
                             (swap! state update-in [:run] #(assoc % :position (inc tgtndx)))
                             (update-all-ice state side)
                             (update-run-ice state side)
                             (trigger-event state side :approach-ice current-ice)))})]
     {:abilities [{:cost [:credit 2]
                   :req (req (and (:run @state)
                                  (rezzed? current-ice)
                                  (has-subtype? current-ice "Barrier")))
                   :label "Swap the barrier ICE currently being encountered with a piece of ICE directly before or after it"
                   :effect (effect (resolve-ability (surf state current-ice) card nil))}]})

   "Trope"
   {:events {:runner-turn-begins {:effect (effect (add-prop card :counter 1))}}
    :abilities [{:cost [:click 1] :label "[Click], remove Trope from the game: Reshuffle cards from Heap back into Stack"
                 :effect (effect
                          (move card :rfg)
                          (gain :memory 1)
                          (resolve-ability
                           {:show-discard true
                            :choices {:max (:counter card) :req #(and (:side % "Runner") (= (:zone %) [:discard]))}
                            :msg (msg "shuffle " (join ", " (map :title targets))
                                      " into their Stack")
                            :effect (req (doseq [c targets] (move state side c :deck))
                                         (shuffle! state side :deck))}
                           card nil))}]}})
