(in-ns 'game.core)

(def draft-points-target
  "Set each side's agenda points target at 6, per draft format rules"
  (req (swap! state assoc-in [:runner :agenda-point-req] 6)
       (swap! state assoc-in [:corp :agenda-point-req] 6)))

(def cards-identities
  {"Adam: Compulsive Hacker"
   (let [titles #{"Safety First" "Always Be Running" "Neutralize All Threats"}
         one-of-each   (fn [cards] (->> cards (group-by :title) (map second) (map first)))
         get-directives (fn [source] (filter #(some #{(:title %)} titles) source))]
   {:events {:pre-start-game
             {:req (req (= side :runner))
              :effect (req (let [directives (-> (:deck runner) (concat (:hand runner)) (get-directives) one-of-each)]
                             (when (not= 3 (count directives))
                               (toast state :runner
                                      (str "Your deck doesn't contain enough directives for Adam's ability. The deck "
                                           "needs to contain at least one copy of each directive. They are not counted "
                                           "against the printed decksize limit, so Adam's minimum decksize on this "
                                           "site is 48 cards.")
                                      "warning"
                                      {:time-out 0 :close-button true}))
                             (doseq [c directives]
                               (runner-install state side c {:no-cost true
                                                             :custom-message (str "starts with " (:title c) " in play")}))
                             (draw state :runner (count (filter in-hand? directives)) {:suppress-event true})))}}})

   "Andromeda: Dispossessed Ristie"
   {:events {:pre-start-game {:req (req (= side :runner))
                              :effect (effect (gain :link 1)
                                              (draw 4 {:suppress-event true}))}}
    :mulligan (effect (draw 4 {:suppress-event true}))}

   "Apex: Invasive Predator"
   (let [ability {:prompt "Select a card to install facedown"
                  :label "Install a card facedown (start of turn)"
                  :once :per-turn
                  :choices {:max 1 :req #(and (:side % "Runner")
                                              (in-hand? %))}
                  :req (req (> (count (:hand runner)) 0))
                  :effect (effect (runner-install target {:facedown true}))}]
   {:events {:runner-turn-begins ability}
    :abilities [ability]})

   "Argus Security: Protection Guaranteed"
   {:events {:agenda-stolen
             {:prompt "Take 1 tag or suffer 2 meat damage?"
              :choices ["1 tag" "2 meat damage"] :player :runner
              :msg "make the Runner take 1 tag or suffer 2 meat damage"
              :effect (req (if (= target "1 tag")
                             (do (tag-runner state :runner 1)
                                 (system-msg state side "takes 1 tag")
                                 (effect-completed state side eid nil))
                             (do (damage state :runner eid :meat 2 {:unboostable true :card card})
                                 (system-msg state side "suffers 2 meat damage"))))}}}

   "Armand \"Geist\" Walker: Tech Lord"
   {:events {:pre-start-game {:req (req (= side :runner))
                              :effect (effect (gain :link 1))}
             :runner-trash {:req (req (and (= side :runner) (= (second targets) :ability-cost)))
                            :msg "draw a card"
                            :effect (effect (draw 1))}}}

   "Blue Sun: Powering the Future"
   {:flags {:corp-phase-12 (req (and (not (:disabled card))
                                     (some #(rezzed? %) (all-installed state :corp))))}
    :abilities [{:choices {:req #(:rezzed %)}
                 :effect (req (trigger-event state side :pre-rez-cost target)
                              (let [cost (rez-cost state side target)]
                                (gain state side :credit cost)
                                (move state side target :hand)
                                (system-msg state side (str "adds " (:title target) " to HQ and gains " cost " [Credits]"))
                                (swap! state update-in [:bonus] dissoc :cost)))}]}

   "Boris \"Syfr\" Kovac: Crafty Veteran"
   {:events (let [bor {:req (req (let [facs (frequencies (map :faction (all-installed state :runner)))
                                       an (get facs "Anarch" 0)
                                       sh (get facs "Shaper" 0)
                                       cr (get facs "Criminal" 0)]
                                   (and (> cr sh) (> cr an) (pos? (:tag runner)))))
                       :msg "remove 1 tag"
                       :effect (effect (lose :tag 1))}]
              {:runner-turn-begins bor
               :pre-start-game {:effect draft-points-target}})}

   "Cerebral Imaging: Infinite Frontiers"
   {:effect (req (when (> (:turn @state) 1)
                   (swap! state assoc-in [:corp :hand-size-base] (:credit corp)))
                 (add-watch state :cerebral-imaging
                            (fn [k ref old new]
                              (let [credit (get-in new [:corp :credit])]
                                (when (not= (get-in old [:corp :credit]) credit)
                                  (swap! ref assoc-in [:corp :hand-size-base] credit))))))
    :leave-play (req (remove-watch state :cerebral-imaging)
                     (swap! state assoc-in [:corp :hand-size-base] 5))}

   "Chaos Theory: Wünderkind"
   {:effect (effect (gain :memory 1))
    :leave-play (effect (lose :runner :memory 1))}

   "Chronos Protocol: Selective Mind-mapping"
   {:events
    {:corp-turn-begins {:effect (effect (enable-corp-damage-choice))}
     :runner-turn-begins {:effect (effect (enable-corp-damage-choice))}
     :pre-resolve-damage
     {:once :per-turn
      :delayed-completion true
      :req (req (and (= target :net)
                     (corp-can-choose-damage? state)
                     (> (last targets) 0)))
      :effect (req (damage-defer state side :net (last targets))
                   (if (= 0 (count (:hand runner)))
                     (do (swap! state update-in [:damage] dissoc :damage-choose-corp)
                         (damage state side eid :net (get-defer-damage state side :net nil)
                                 {:unpreventable true :card card}))
                     (do (show-wait-prompt state :runner "Corp to use Chronos Protocol: Selective Mind-mapping")
                         (continue-ability
                           state side
                           {:optional
                            {:prompt (str "Use Chronos Protocol: Selective Mind-mapping to reveal the Runner's "
                                          "Grip to select the first card trashed?")
                             :priority 10
                             :player :corp
                             :yes-ability {:prompt (msg "Choose a card to trash")
                                           :choices (req (:hand runner)) :not-distinct true
                                           :priority 10
                                           :msg (msg "trash " (:title target)
                                                     (when (pos? (dec (or (get-defer-damage state side :net nil) 0)))
                                                       (str " and deal " (- (get-defer-damage state side :net nil) 1)
                                                            " more net damage")))
                                           :effect (req (clear-wait-prompt state :runner)
                                                        (swap! state update-in [:damage] dissoc :damage-choose-corp)
                                                        (trash state side target {:cause :net :unpreventable true})
                                                        (let [more (dec (or (get-defer-damage state side :net nil) 0))]
                                                          (damage-defer state side :net more)))}
                             :no-ability {:effect (req (clear-wait-prompt state :runner)
                                                       (swap! state update-in [:damage] dissoc :damage-choose-corp))}}}
                           card nil))))}}
    :leave-play (req (swap! state update-in [:damage] dissoc :damage-choose-corp))}

   "Cybernetics Division: Humanity Upgraded"
   {:effect (effect (lose :hand-size-modification 1)
                    (lose :runner :hand-size-modification 1))
    :leave-play (effect (gain :hand-size-modification 1)
                        (gain :runner :hand-size-modification 1))}

   "Edward Kim: Humanitys Hammer"
   {:events {:pre-start-game {:req (req (= side :runner))
                              :effect (effect (gain :link 1))}
             :access {:once :per-turn
                      :req (req (and (is-type? target "Operation")
                                     (turn-flag? state side card :can-trash-operation)))
                      :effect (effect (trash target))
                      :msg (msg "trash " (:title target))}
             :successful-run-ends {:req (req (and (= target :archives)
                                                  (not= (:max-access run) 0)
                                                  (seq (filter #(is-type? % "Operation") (:discard corp)))))
                                   :effect (effect (register-turn-flag! card :can-trash-operation (constantly false)))}}}

   "Exile: Streethawk"
   {:events {:pre-start-game {:req (req (= side :runner))
                              :effect (effect (gain :link 1))}
             :runner-install {:req (req (and (is-type? target "Program")
                                             (some #{:discard} (:previous-zone target))))
                              :msg (msg "draw a card")
                              :effect (effect (draw 1))}}}

   "Fringe Applications: Tomorrow, Today"
   {:events (let [fap {:req (req (let [facs (frequencies (map :faction (filter :rezzed (all-installed state :corp))))
                                       hb (get facs "Haas-Bioroid" 0)
                                       ji (get facs "Jinteki" 0)
                                       nb (get facs "NBN" 0)
                                       we (get facs "Weyland Consortium" 0)]
                                   (and (> we ji) (> we hb) (> we nb))))
                       :msg (msg "place 1 advancement token on " (card-str state target))
                       :prompt "Choose a piece of ICE to place 1 advancement token on it" :player :corp
                       :choices {:req #(and (installed? %) (ice? %))}
                       :effect (req (add-prop state :corp target :advance-counter 1 {:placed true}))}]
              {:runner-turn-begins fap
               :pre-start-game {:effect draft-points-target}})}

   "Gabriel Santiago: Consummate Professional"
   {:events {:successful-run {:msg "gain 2 [Credits]" :once :per-turn
                              :effect (effect (gain :credit 2)) :req (req (= target :hq))}}}

   "Gagarin Deep Space: Expanding the Horizon"
   {:flags {:slow-remote-access (req (not (:disabled card)))}
    :events {:pre-access-card {:req (req (is-remote? (second (:zone target))))
                               :effect (effect (access-cost-bonus [:credit 1]))
                               :msg "make the Runner spend 1 [Credits] to access"}}}

   "GRNDL: Power Unleashed"
   {:events {:pre-start-game {:req (req (= :corp side))
                              :effect (req (gain state :corp :credit 5)
                                           (when (= 0 (:bad-publicity corp))
                                             (gain state :corp :bad-publicity 1)))}}}

   "Haarpsichord Studios: Entertainment Unleashed"
   (let [haarp (fn [state side card]
                 (if (is-type? card "Agenda")
                   ((constantly false)
                     (toast state :runner "Cannot steal due to Haarpsichord Studios." "warning"))
                   true))]
   {:events {:agenda-stolen
             {:effect (effect (register-turn-flag! card :can-steal haarp))}}
    :effect (req (when-not (first-event state side :agenda-stolen)
                   (register-turn-flag! state side card :can-steal haarp)))
    :leave-play (effect (clear-turn-flag! card :can-steal))})

   "Haas-Bioroid: Engineering the Future"
   {:events {:corp-install {:once :per-turn :msg "gain 1 [Credits]"
                            :effect (effect (gain :credit 1))}}}

   "Haas-Bioroid: Stronger Together"
   {:events {:pre-ice-strength {:req (req (and (ice? target) (has-subtype? target "Bioroid")))
                                :effect (effect (ice-strength-bonus 1 target))}}}

   "Harishchandra Ent.: Where Youre the Star"
   {:effect (req (when tagged
                   (reveal-hand state :runner))
                 (add-watch state :harishchandra
                            (fn [k ref old new]
                              (when (and (is-tagged? new) (not (is-tagged? old)))
                                (system-msg ref side (str "uses Harishchandra Ent.: Where You're the Star to"
                                                          " make the Runner play with their Grip revealed"))
                                (reveal-hand state :runner))
                              (when (and (is-tagged? old) (not (is-tagged? new)))
                                (conceal-hand state :runner)))))
    :leave-play (req (when tagged
                       (conceal-hand state :runner))
                     (remove-watch state :harishchandra))}

   "Harmony Medtech: Biomedical Pioneer"
   {:effect (effect (lose :agenda-point-req 1) (lose :runner :agenda-point-req 1))
    :leave-play (effect (gain :agenda-point-req 1) (gain :runner :agenda-point-req 1))}

   "Hayley Kaplan: Universal Scholar"
   {:events {:runner-install
             {:silent (req (not (and (first-event state side :runner-install)
                                     (some #(is-type? % (:type target)) (:hand runner)))))
              :req (req (and (first-event state side :runner-install)
                             (some #(is-type? % (:type target)) (:hand runner))))
              :once :per-turn
              :delayed-completion true
              :effect
              (req (let [itarget target
                         type (:type itarget)]
                     (continue-ability
                       state side
                       {:optional {:prompt (msg "Install another " type " from your Grip?")
                                  :yes-ability
                                  {:prompt (msg "Choose another " type " to install from your grip")
                                   :choices {:req #(and (is-type? % type)
                                                        (in-hand? %))}
                                   :msg (msg "install " (:title target))
                                   :effect (effect (runner-install eid target nil))}}}
                       card nil)))}}}

   "Iain Stirling: Retired Spook"
   (let [ability {:req (req (> (:agenda-point corp) (:agenda-point runner)))
                  :once :per-turn
                  :msg "gain 2 [Credits]"
                  :effect (effect (gain :credit 2))}]
   {:flags {:drip-economy true}
    :events {:pre-start-game {:req (req (= side :runner))
                              :effect (effect (gain :link 1))}
             :runner-turn-begins ability}
    :abilities [ability]})

   "Industrial Genomics: Growing Solutions"
   {:events {:pre-trash {:effect (effect (trash-cost-bonus
                                           (count (filter #(not (:seen %)) (:discard corp)))))}}}

   "Information Dynamics: All You Need To Know"
   {:events (let [inf {:req (req (let [facs (frequencies (map :faction (filter :rezzed (all-installed state :corp))))
                                       hb (get facs "Haas-Bioroid" 0)
                                       ji (get facs "Jinteki" 0)
                                       nb (get facs "NBN" 0)
                                       we (get facs "Weyland Consortium" 0)]
                                   (and (> nb ji) (> nb hb) (> nb we))))
                       :msg "give the Runner 1 tag"
                       :effect (effect (tag-runner :runner 1))}]
              {:agenda-scored inf :agenda-stolen inf
               :pre-start-game {:effect draft-points-target}})}

   "Jamie \"Bzzz\" Micken: Techno Savant"
   {:events (let [jam {:req (req (let [facs (frequencies (map :faction (all-installed state :runner)))
                                       an (get facs "Anarch" 0)
                                       sh (get facs "Shaper" 0)
                                       cr (get facs "Criminal" 0)]
                                   (and (> sh an) (> sh cr) (pos? (count (:deck runner))))))
                       :msg "draw 1 card"
                       :once :per-turn
                       :effect (effect (draw 1))}]
              {:runner-install jam
               :pre-start-game {:effect draft-points-target}})}

   "Jesminder Sareen: Girl Behind the Curtain"
   {:events {:pre-tag {:once :per-run
                       :req (req (:run @state))
                       :msg "avoid the first tag during this run"
                       :effect (effect (tag-prevent 1))}}}

   "Jinteki: Personal Evolution"
   {:events {:agenda-scored {:interactive (req true)
                             :delayed-completion true
                             :msg "do 1 net damage"
                             :effect (effect (damage eid :net 1 {:card card}))}
             :agenda-stolen {:msg "do 1 net damage" :effect (effect (damage eid :net 1 {:card card}))}}}

   "Jinteki: Replicating Perfection"
   {:events
    {:runner-phase-12 {:effect (req (apply prevent-run-on-server
                                           state card (map first (get-remotes @state))))}
     :run {:once :per-turn
           :req (req (is-central? (:server run)))
           :effect (req (apply enable-run-on-server
                               state card (map first (get-remotes @state))))}}
    :leave-play (req (apply enable-run-on-server state card (map first (get-remotes @state))))}

   "Jinteki Biotech: Life Imagined"
   {:events {:pre-first-turn {:req (req (= side :corp))
                              :prompt "Choose a copy of Jinteki Biotech to use this game"
                              :choices ["[The Brewery~brewery]" "[The Tank~tank]" "[The Greenhouse~greenhouse]"]
                              :effect (effect (update! (assoc card :biotech-target target))
                                              (system-msg (str "has chosen a copy of Jinteki Biotech for this game ")))}}
    :abilities [{:label "Check chosen flip identity"
                 :effect (req (case (:biotech-target card)
                                "[The Brewery~brewery]"
                                (toast state :corp "Flip to: The Brewery (Do 2 net damage)" "info")
                                "[The Tank~tank]"
                                (toast state :corp "Flip to: The Tank (Shuffle Archives into R&D)" "info")
                                "[The Greenhouse~greenhouse]"
                                (toast state :corp "Flip to: The Greenhouse (Place 4 advancement tokens on a card)" "info")))}
                {:cost [:click 3]
                 :req (req (not (:biotech-used card)))
                 :label "Flip this identity"
                 :effect (req (let [flip (:biotech-target card)]
                                (case flip
                                  "[The Brewery~brewery]"
                                  (do (system-msg state side "uses [The Brewery~brewery] to do 2 net damage")
                                      (damage state side eid :net 2 {:card card})
                                      (update! state side (assoc card :code "brewery")))
                                  "[The Tank~tank]"
                                  (do (system-msg state side "uses [The Tank~tank] to shuffle Archives into R&D")
                                      (shuffle-into-deck state side :discard)
                                      (update! state side (assoc card :code "tank")))
                                  "[The Greenhouse~greenhouse]"
                                  (do (system-msg state side (str "uses [The Greenhouse~greenhouse] to place 4 advancement tokens "
                                                                  "on a card that can be advanced"))
                                      (update! state side (assoc card :code "greenhouse"))
                                      (resolve-ability
                                        state side
                                        {:prompt "Choose a card that can be advanced"
                                         :choices {:req can-be-advanced?}
                                         :effect (effect (add-prop target :advance-counter 4 {:placed true}))} card nil)))
                                (update! state side (assoc (get-card state card) :biotech-used true))))}]}

   "Kate \"Mac\" McCaffrey: Digital Tinker"
   {:events {:pre-start-game {:req (req (= side :runner))
                              :effect (effect (gain :link 1))}
             :pre-install {:req (req (and (#{"Hardware" "Program"} (:type target))
                                          (not (get-in @state [:per-turn (:cid card)]))))
                           :effect (effect (install-cost-bonus [:credit -1]))}
             :runner-install {:req (req (and (#{"Hardware" "Program"} (:type target))
                                             (not (get-in @state [:per-turn (:cid card)]))))
                              :silent (req true)
                              :msg (msg "reduce the install cost of " (:title target) " by 1 [Credits]")
                              :effect (req (swap! state assoc-in [:per-turn (:cid card)] true))}}}

   "Ken \"Express\" Tenma: Disappeared Clone"
   {:events {:play-event {:req (req (has-subtype? target "Run")) :once :per-turn
                          :msg "gain 1 [Credits]" :effect (effect (gain :credit 1))}}}

   "Khan: Savvy Skiptracer"
   {:events {:pass-ice
             {:once :per-turn
              :effect (req (when (some (fn [c] (has? c :subtype "Icebreaker")) (:hand runner))
                             (resolve-ability state side
                               {:prompt "Choose an icebreaker to install from your Grip"
                                :choices {:req #(and (in-hand? %) (has-subtype? % "Icebreaker"))}
                                :msg (msg "install " (:title target))
                                :effect (effect (runner-install target))}
                              card nil)))}}}

   "Laramy Fisk: Savvy Investor"
   {:events {:successful-run {:delayed-completion true
                              :req (req (and (is-central? (:server run))
                                             (empty? (let [successes (turn-events state side :successful-run)]
                                                       (filter #(is-central? %) successes)))))
                              :effect (effect (continue-ability
                                                {:optional
                                                 {:prompt "Force the Corp to draw a card?"
                                                  :yes-ability {:msg "force the Corp to draw 1 card"
                                                                :effect (effect (draw :corp))}}} card nil))}}}

   "Leela Patel: Trained Pragmatist"
   (let [leela {:interactive (req true)
                :prompt  "Select an unrezzed card to return to HQ"
                :choices {:req #(and (not (:rezzed %)) (card-is? % :side :corp))}
                :msg     (msg "add " (card-str state target) " to HQ")
                :effect  (final-effect (move :corp target :hand))}]
   {:flags {:slow-hq-access (req true)}
    :events {:agenda-scored leela
             :agenda-stolen leela}})

   "MaxX: Maximum Punk Rock"
   (let [ability {:msg "trash the top 2 cards from Stack and draw 1 card"
                  :once :per-turn
                  :effect (effect (mill 2) (draw))}]
   {:flags {:runner-turn-draw true
            :runner-phase-12 (req (and (not (:disabled card))
                                       (some #(card-flag? % :runner-turn-draw true) (all-installed state :runner))))}
    :events {:runner-turn-begins ability}
    :abilities [ability]})

   "Nasir Meidan: Cyber Explorer"
   {:events {:pre-start-game {:req (req (= side :runner))
                              :effect (effect (gain :link 1))}
             :rez {:req (req (and (:run @state)
                                  ;; check that the rezzed item is the encountered ice
                                  (= (:cid target)
                                     (:cid (get-card state current-ice)))))
                   :effect (req (toast state :runner "Click Nasir Meidan: Cyber Explorer to lose all credits and gain credits equal to the rez cost of the newly rezzed ice." "info"))}}
    :abilities [{:req (req (and (:run @state)
                                (:rezzed (get-card state current-ice))))
                 :effect (req (let [current-ice (get-card state current-ice)]
                                (trigger-event state side :pre-rez-cost current-ice)
                                (let [cost (rez-cost state side current-ice)]
                                  (lose state side :credit (:credit runner))
                                  (gain state side :credit cost)
                                  (system-msg state side (str "loses all credits and gains " cost
                                                              " [Credits] from the rez of " (:title current-ice)))
                                  (swap! state update-in [:bonus] dissoc :cost))))}]}

   "NBN: Controlling the Message"
   {:events {:runner-trash
             {:delayed-completion true
              :once :per-turn
              :req (req (and (card-is? target :side :corp)
                             (installed? target)))
              :effect (req (show-wait-prompt state :runner "Corp to use NBN: Controlling the Message")
                           (continue-ability
                             state :corp
                             {:optional
                              {:delayed-completion true
                               :prompt "Trace the Runner with NBN: Controlling the Message?"
                               :yes-ability {:trace {:base 4
                                                     :msg "give the Runner 1 tag"
                                                     :effect (effect (tag-runner :runner 1 {:unpreventable true})
                                                                     (clear-wait-prompt :runner))}}
                               :no-ability {:effect (effect (clear-wait-prompt :runner))}}}
                            card nil))}}}

   "NBN: Making News"
   {:recurring 2}

   "NBN: The World is Yours*"
   {:effect (effect (gain :hand-size-modification 1))
    :leave-play (effect (lose :hand-size-modification 1))}

   "Near-Earth Hub: Broadcast Center"
   {:events {:server-created {:msg "draw 1 card" :once :per-turn :effect (effect (draw 1))}}}

   "Nero Severn: Information Broker"
   {:events {:pre-start-game {:req (req (= side :runner))
                              :effect (effect (gain :link 1))}}
    :abilities [{:req (req (has-subtype? current-ice "Sentry"))
                 :once :per-turn
                 :msg "jack out when encountering a sentry"
                 :effect (effect (jack-out nil))}]}

   "New Angeles Sol: Your News"
   (let [nasol {:optional
                {:prompt "Play a Current?" :player :corp
                 :req (req (not (empty? (filter #(has-subtype? % "Current")
                                                (concat (:hand corp) (:discard corp))))))
                 :yes-ability {:prompt "Choose a Current to play from HQ or Archives"
                               :show-discard true
                               :choices {:req #(and (has-subtype? % "Current")
                                                    (= (:side %) "Corp")
                                                    (#{[:hand] [:discard]} (:zone %)))}
                               :msg (msg "play a current from " (name-zone "Corp" (:zone target)))
                               :effect (effect (play-instant eid target))}}}]
     {:events {:agenda-scored nasol :agenda-stolen nasol}})

   "NEXT Design: Guarding the Net"
   (let [ndhelper (fn nd [n] {:prompt (msg "When finished, click NEXT Design: Guarding the Net to draw back up to 5 cards in HQ. "
                                           "Choose a piece of ICE in HQ to install:")
                              :choices {:req #(and (:side % "Corp")
                                                   (ice? %)
                                                   (in-hand? %))}
                              :effect (req (corp-install state side target nil)
                                           (when (< n 3)
                                             (resolve-ability state side (nd (inc n)) card nil)))})]
     {:events {:pre-first-turn {:req (req (= side :corp))
                                :msg "install up to 3 pieces of ICE and draw back up to 5 cards"
                                :effect (effect (resolve-ability (ndhelper 1) card nil)
                                                (update! (assoc card :fill-hq true)))}}
      :abilities [{:req (req (:fill-hq card))
                   :msg (msg "draw " (- 5 (count (:hand corp))) " cards")
                   :effect (effect (draw (- 5 (count (:hand corp))))
                                   (update! (dissoc card :fill-hq)))}]})

   "Nisei Division: The Next Generation"
   {:events {:psi-game {:msg "gain 1 [Credits]" :effect (effect (gain :corp :credit 1))}}}

   "Noise: Hacker Extraordinaire"
   {:events {:runner-install {:msg "force the Corp to trash the top card of R&D"
                              :effect (effect (mill :corp))
                              :req (req (has-subtype? target "Virus"))}}}

   "Null: Whistleblower"
   {:abilities [{:once :per-turn
                 :req (req (and (:run @state) (rezzed? current-ice)))
                 :prompt "Choose a card in your Grip to trash"
                 :choices {:req in-hand?}
                 :msg (msg "trash " (:title target) " and reduce the strength of " (:title current-ice)
                           " by 2 for the remainder of the run")
                 :effect (effect (update! (assoc card :null-target current-ice))
                                 (update-ice-strength current-ice))}]
    :events {:pre-ice-strength
             {:req (req (= (:cid target) (get-in card [:null-target :cid])))
              :effect (effect (ice-strength-bonus -2 target))}
             :run-ends
             {:effect (effect (update! (dissoc card :null-target)))}}}

   "Pālanā Foods: Sustainable Growth"
   {:events {:runner-draw {:msg "gain 1 [Credits]"
                           :once :per-turn
                           :effect (effect (gain :corp :credit 1))}}}

   "Quetzal: Free Spirit"
   {:abilities [{:once :per-turn :msg "break 1 barrier subroutine"}]}

   "Reina Roja: Freedom Fighter"
   {:events {:pre-start-game {:req (req (= side :runner))
                              :effect (effect (gain :link 1))}
             :pre-rez {:req (req (and (ice? target) (not (get-in @state [:per-turn (:cid card)]))))
                       :effect (effect (rez-cost-bonus 1))}
             :rez {:req (req (and (ice? target) (not (get-in @state [:per-turn (:cid card)]))))
                              :effect (req (swap! state assoc-in [:per-turn (:cid card)] true))}}}

   "Rielle \"Kit\" Peddler: Transhuman"
   {:abilities [{:req (req (and (:run @state)
                                (:rezzed (get-card state current-ice))))
                 :once :per-turn :msg (msg "make " (:title current-ice) " gain code gate until the end of the run")
                 :effect (req (let [ice current-ice
                                    stypes (:subtype ice)]
                                (update! state side (assoc ice :subtype
                                                               (->> (vec (.split (:subtype ice) " - "))
                                                                    (cons "Code Gate")
                                                                    distinct
                                                                    (join " - "))))
                                (register-events state side {:run-ends
                                                             {:effect (effect (update! (assoc ice :subtype stypes))
                                                                              (unregister-events card))}} card)
                                (update-ice-strength state side ice)
                                (trigger-event state side :ice-subtype-changed)))}]
    :events {:run-ends nil}}

   "Silhouette: Stealth Operative"
   {:events {:successful-run
             {:delayed-completion true
              :req (req (= target :hq)) :once :per-turn
              :effect (effect (continue-ability {:choices {:req #(and installed? (not (rezzed? %)))}
                                                :effect (effect (expose target)) :msg "expose 1 card"}
                                               card nil))}}}

   "Spark Agency: Worldswide Reach"
   {:events
    {:rez {:req (req (has-subtype? target "Advertisement"))
           :once :per-turn
           :effect (effect (lose :runner :credit 1))
           :msg (msg "make the Runner lose 1 [Credits] by rezzing an advertisement")}}}

   "Strategic Innovations: Future Forward"
   {:events {:pre-start-game {:effect draft-points-target}
             :runner-turn-ends
             {:req (req (let [facs (frequencies (map :faction (filter :rezzed (all-installed state :corp))))
                              hb (get facs "Haas-Bioroid" 0)
                              ji (get facs "Jinteki" 0)
                              nb (get facs "NBN" 0)
                              we (get facs "Weyland Consortium" 0)]
                          (and (not (:disabled card))
                               (> hb ji) (> hb nb) (> hb we) (pos? (count (:discard corp))))))
              :prompt "Choose a card in Archives to shuffle into R&D"
              :choices {:req #(and (card-is? % :side :corp) (= (:zone %) [:discard]))}
              :player :corp :show-discard true :priority true
              :msg (msg "to shuffle " (if (:seen target) (:title target) "a card")
                        " into R&D")
              :effect (effect (move :corp target :deck)
                              (shuffle! :corp :deck))}}}

   "Sunny Lebeau: Security Specialist"
   {:events {:pre-start-game {:req (req (= side :runner))
                              :effect (effect (gain :link 2))}}}

   "SYNC: Everything, Everywhere"
   {:effect (req (when (> (:turn @state) 1)
                   (if (:sync-front card)
                     (tag-remove-bonus state side -1)
                     (trash-resource-bonus state side 2))))
    :events {:pre-first-turn {:req (req (= side :corp))
                              :effect (effect (update! (assoc card :sync-front true)) (tag-remove-bonus -1))}}
    :abilities [{:cost [:click 1]
                 :effect (req (if (:sync-front card)
                           (do (tag-remove-bonus state side 1)
                               (trash-resource-bonus state side 2)
                               (update! state side (-> card (assoc :sync-front false) (assoc :code "sync"))))
                           (do (tag-remove-bonus state side -1)
                               (trash-resource-bonus state side -2)
                               (update! state side (-> card (assoc :sync-front true)(assoc :code "09001"))))))
                 :msg (msg "flip their ID")}]
    :leave-play (req (if (:sync-front card)
                       (tag-remove-bonus state side 1)
                       (trash-resource-bonus state side -2)))}

   "Synthetic Systems: The World Re-imagined"
   {:events {:pre-start-game {:effect draft-points-target}}
    :flags {:corp-phase-12 (req (let [facs (frequencies (map :faction (filter :rezzed (all-installed state :corp))))
                                      hb (get facs "Haas-Bioroid" 0)
                                      ji (get facs "Jinteki" 0)
                                      nb (get facs "NBN" 0)
                                      we (get facs "Weyland Consortium" 0)]
                                  (and (not (:disabled (get-card state card)))
                                       (> ji hb) (> ji nb) (> ji we)
                                       (> (count (filter #(ice? %) (all-installed state :corp))) 1))))}
    :abilities [{:prompt "Select two pieces of ICE to swap positions"
                 :choices {:req #(and (installed? %) (ice? %)) :max 2}
                 :once :per-turn
                 :effect (req (when (= (count targets) 2)
                                (swap-ice state side (first targets) (second targets))))
                 :msg (msg "swap the positions of " (card-str state (first targets))
                           " and " (card-str state (second targets)))}]}

   "Tennin Institute: The Secrets Within"
   {:flags {:corp-phase-12 (req (and (not (:disabled (get-card state card)))
                                     (not= 1 (:turn @state)) (not (:successful-run runner-reg))))}
    :abilities [{:msg (msg "place 1 advancement token on " (card-str state target))
                 :choices {:req installed?}
                 :req (req (not (:successful-run runner-reg)))
                 :once :per-turn
                 :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]}

   "The Foundry: Refining the Process"
   {:events
    {:rez {:req (req (and (ice? target) ;; Did you rez and ice just now
                          (some #(= (:title %) (:title target)) (:deck corp)) ;; Are there more copies in the dec
                          (empty? (let [rezzed-this-turn (map first (turn-events state side :rez))]
                                    (filter ice? rezzed-this-turn))))) ;; Is this the first ice you've rezzed this turn
           :optional
           {:prompt "Add another copy to HQ?" :priority 1
            :yes-ability {:msg (msg "add a copy of " (:title target) " from R&D to HQ")
                          :effect (effect (move (some #(when (= (:title %) (:title target)) %) (:deck corp)) :hand)
                                          (shuffle! :deck))}}}}}

   "The Masque: Cyber General"
   {:events {:pre-start-game {:effect draft-points-target}}}

   "The Shadow: Pulling the Strings"
   {:events {:pre-start-game {:effect draft-points-target}}}

   "Titan Transnational: Investing In Your Future"
   {:events {:agenda-scored {:msg (msg "add 1 agenda counter to " (:title target))
                             :effect (effect (add-counter (get-card state target) :agenda 1))}}}

   "Valencia Estevez: The Angel of Cayambe"
   {:events {:pre-start-game
             {:req (req (and (= side :runner)
                             (zero? (get-in @state [:corp :bad-publicity]))))
              :effect (effect (gain :corp :bad-publicity 1))}}}

   "Weyland Consortium: Because We Built It"
   {:recurring 1}

   "Weyland Consortium: Building a Better World"
   {:events {:play-operation {:msg "gain 1 [Credits]"
                              :effect (effect (gain :credit 1))
                              :req (req (has-subtype? target "Transaction"))}}}

   "Whizzard: Master Gamer"
   {:recurring 3}

   "Wyvern: Chemically Enhanced"
   {:events (let [wyv {:req (req (let [facs (frequencies (map :faction (all-installed state :runner)))
                                       an (get facs "Anarch" 0)
                                       sh (get facs "Shaper" 0)
                                       cr (get facs "Criminal" 0)]
                                   (and (card-is? target :side :corp) (> an sh) (> an cr)
                                        (pos? (count (:discard runner))))))
                       :msg (msg "shuffle " (:title (last (:discard runner))) " into their Stack")
                       :effect (effect (move :runner (last (:discard runner)) :deck)
                                       (shuffle! :runner :deck))}]
              {:pre-start-game {:effect draft-points-target}
               :runner-trash wyv})}})
