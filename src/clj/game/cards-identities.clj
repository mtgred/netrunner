(in-ns 'game.core)

(def cards-identities
  {"Adam: Compulsive Hacker"
   (let [titles #{"Safety First" "Always Be Running" "Neutralize All Threats"}
         one-of-each   (fn [cards] (->> cards (group-by :title) (map second) (map first)))
         get-directives (fn [source] (filter #(some #{(:title %)} titles) source))]
   {:effect (req (let [directives (-> (:deck runner) (concat (:hand runner)) (get-directives) one-of-each)]
                   (when (not= 3 (count directives))
                     (toast state :runner
                            "Your deck doesn't contain enough directives for Adam's ability. The deck needs to contain at least one copy of each directive. They are not counted against the printed decksize limit, so minimal Adam's decksize on this site is 48 cards."
                            "warning"
                            {:time-out 0 :close-button true}))
                   (doseq [c directives]
                     (runner-install state side c {:no-cost true
                                                   :custom-message (str "starts with " (:title c) " in play")}))
                   (draw state :runner (count (filter in-hand? directives)))))})

   "Andromeda: Dispossessed Ristie"
   {:effect (effect (gain :link 1) (draw 4)) :mulligan (effect (draw 4))}

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
                             (do (tag-runner state :runner 1) (system-msg state side "takes 1 tag"))
                             (do (damage state :runner :meat 2 {:unboostable true :card card})
                                 (system-msg state side "suffers 2 meat damage"))))}}}

   "Armand \"Geist\" Walker: Tech Lord"
   {:effect (effect (gain :link 1))
    :events {:runner-trash {:req (req (and (= side :runner) (= (second targets) :ability-cost)))
                            :msg "draw a card"
                            :effect (effect (draw 1))}}}

   "Blue Sun: Powering the Future"
   {:flags {:corp-phase-12 (req (some #(rezzed? %) (all-installed state :corp)))}
    :abilities [{:choices {:req #(:rezzed %)}
                 :effect (req (trigger-event state side :pre-rez-cost target)
                              (let [cost (rez-cost state side target)]
                                (gain state side :credit cost)
                                (move state side target :hand)
                                (system-msg state side (str "adds " (:title target) " to HQ and gains " cost " [Credits]"))
                                (swap! state update-in [:bonus] dissoc :cost)))}]}

   "Cerebral Imaging: Infinite Frontiers"
   {:effect (req (add-watch state :cerebral-imaging
                            (fn [k ref old new]
                              (let [credit (get-in new [:corp :credit])]
                                (when (not= (get-in old [:corp :credit]) credit)
                                  (swap! ref assoc-in [:corp :hand-size-base] credit))))))}

   "Chaos Theory: Wünderkind"
   {:effect (effect (gain :memory 1))}

   "Chronos Protocol: Selective Mind-mapping"
   {:events
    {:pre-resolve-damage
     {:once :per-turn
      :req (req (and (= target :net) (> (last targets) 0)))
      :effect (effect (damage-defer :net (last targets))
                      (show-wait-prompt :runner "Corp to use Chronos Protocol: Selective Mind-mapping")
                      (resolve-ability
                        {:optional {:prompt (str "Use Chronos Protocol: Selective Mind-mapping to reveal the Runner's "
                                                 "grip to select the first card trashed?") :player :corp
                                    :yes-ability {:prompt (msg "Choose a card to trash")
                                                  :choices (req (:hand runner)) :not-distinct true
                                                  :msg (msg "trash " (:title target)
                                                         (when (> (- (get-defer-damage state side :net nil) 1) 0)
                                                           (str " and deal "
                                                                (- (get-defer-damage state side :net nil) 1)
                                                                " more net damage")))
                                                  :effect (effect (clear-wait-prompt :runner)
                                                                  (trash target {:cause :net :unpreventable true})
                                                                  (damage :net (- (get-defer-damage state side :net nil) 1)
                                                                          {:unpreventable true :card card}))}
                                    :no-ability {:effect (effect (clear-wait-prompt :runner)
                                                                 (damage :net (get-defer-damage state side :net nil)
                                                                         {:unpreventable true :card card}))}}} card nil))}}}

   "Cybernetics Division: Humanity Upgraded"
   {:effect (effect (lose :hand-size-modification 1)
                    (lose :runner :hand-size-modification 1))}

   "Edward Kim: Humanitys Hammer"
   {:effect (effect (gain :link 1))
    :events {:access {:once :per-turn
                      :req (req (is-type? target "Operation"))
                      :effect (effect (trash target))
                      :msg (msg "trash " (:title target) (if (some #{:discard} (:zone target)) ", but it is already trashed."))}}}

   "Exile: Streethawk"
   {:effect (effect (gain :link 1))
    :events {:runner-install {:req (req (and (is-type? target "Program")
                                             (some #{:discard} (:previous-zone target))))
                              :msg (msg "draw a card")
                              :effect (effect (draw 1))}}}

   "Gabriel Santiago: Consummate Professional"
   {:events {:successful-run {:msg "gain 2 [Credits]" :once :per-turn
                              :effect (effect (gain :credit 2)) :req (req (= target :hq))}}}

   "Gagarin Deep Space: Expanding the Horizon"
   {:events {:pre-access-card {:req (req (is-remote? (second (:zone target))))
                               :effect (effect (access-cost-bonus [:credit 1]))
                               :msg  (msg (if
                                      (= (get-in @state [:runner :credit]) 0)
                                      "prevent access"
                                      "make the Runner spend 1 [Credits] to access"
                                      ))}}}

   "GRNDL: Power Unleashed"
   {:effect (effect (gain :credit 5 :bad-publicity 1))}

   "Haarpsichord Studios: Entertainment Unleashed"
   {:events {:pre-steal-cost {:req (req (:stole-agenda runner-reg))
                              :effect (effect (prevent-steal))}}}

   "Haas-Bioroid: Engineering the Future"
   {:events {:corp-install {:once :per-turn :msg "gain 1 [Credits]"
                            :effect (effect (gain :credit 1))}}}

   "Haas-Bioroid: Stronger Together"
   {:events {:pre-ice-strength {:req (req (and (ice? target) (has-subtype? target "Bioroid")))
                                :effect (effect (ice-strength-bonus 1 target))}}}

   "Harmony Medtech: Biomedical Pioneer"
   {:effect (effect (lose :agenda-point-req 1) (lose :runner :agenda-point-req 1))}

   "Hayley Kaplan: Universal Scholar"
   {:events {:runner-install
             {:optional {:prompt (msg "Install another " (:type target) " from your Grip?")
                         :req (req (and (first-event state side :runner-install)
                                        (some #(is-type? % (:type target)) (:hand runner))))
                         :yes-ability {:effect (req (let [type (:type target)]
                                              (resolve-ability
                                               state side
                                               {:prompt (msg "Choose another " type " to install from your grip")
                                                :choices {:req #(and (is-type? % type)
                                                                     (in-hand? %))}
                                                :msg (msg "install " (:title target))
                                                :effect (effect (runner-install target))} card nil)))}}}}}

   "Iain Stirling: Retired Spook"
   (let [ability {:req (req (> (:agenda-point corp) (:agenda-point runner)))
                  :once :per-turn
                  :msg "gain 2 [Credits]"
                  :effect (effect (gain :credit 2))}]
   {:flags {:drip-economy true}
    :effect (effect (gain :link 1))
    :events {:runner-turn-begins ability}
    :abilities [ability]})

   "Industrial Genomics: Growing Solutions"
   {:events {:pre-trash {:effect (effect (trash-cost-bonus
                                           (count (filter #(not (:seen %)) (:discard corp)))))}}}

   "Jesminder Sareen: Girl Behind the Curtain"
   {:events {:pre-tag {:once :per-run
                       :req (req (:run @state))
                       :msg "avoid the first tag during this run"
                       :effect (effect (tag-prevent 1))}}}

   "Jinteki: Personal Evolution"
   {:events {:agenda-scored {:msg "do 1 net damage" :effect (effect (damage :net 1 {:card card}))}
             :agenda-stolen {:msg "do 1 net damage" :effect (effect (damage :net 1 {:card card}))}}}

   "Jinteki Biotech: Life Imagined"
   {:events {:pre-first-turn {:req (req (= side :corp))
                              :prompt "Choose a copy of Jinteki Biotech to use this game"
                              :choices ["[The Brewery~brewery]" "[The Tank~tank]" "[The Greenhouse~greenhouse]"]
                              :effect (effect (update! (assoc card :biotech-target target))
                                              (system-msg (str "has chosen a copy of Jinteki Biotech for this game ")))}}
    :abilities [{:cost [:click 3]
                 :req (req (not (:biotech-used card)))
                 :effect (req (let [flip (:biotech-target card)]
                                (update! state side (assoc card :biotech-used true))
                                (case flip
                                  "[The Brewery~brewery]"
                                  (do (system-msg state side "uses [The Brewery~brewery] to do 2 net damage")
                                      (damage state side :net 2 {:card card})
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
                                         :effect (effect (add-prop target :advance-counter 4 {:placed true}))} card nil)))))}]}

   "Kate \"Mac\" McCaffrey: Digital Tinker"
   {:effect (effect (gain :link 1))
    :events {:pre-install {:req (req (and (#{"Hardware" "Program"} (:type target))
                                          (not (get-in @state [:per-turn (:cid card)]))))
                           :effect (effect (install-cost-bonus [:credit -1]))}
             :runner-install {:req (req (and (#{"Hardware" "Program"} (:type target))
                                             (not (get-in @state [:per-turn (:cid card)]))))
                              :msg (msg "reduce the install cost of " (:title target) " by 1 [Credits]")
                              :effect (req (swap! state assoc-in [:per-turn (:cid card)] true))}}}

   "Ken \"Express\" Tenma: Disappeared Clone"
   {:events {:play-event {:req (req (has-subtype? target "Run")) :once :per-turn
                          :msg "gain 1 [Credits]" :effect (effect (gain :credit 1))}}}

   "Laramy Fisk: Savvy Investor"
   {:events {:no-action {:effect (req (toast state :runner "Click Laramy Fisk: Savvy Investor to force the Corp to draw a card." "info"))
                         :req (req (and run
                                        (is-central? (:server run))
                                        (not current-ice)
                                        (not (get-in @state [:per-turn (:cid card)]))
                                        (empty? (let [successes (turn-events state side :successful-run)]
                                                  (filter #(is-central? %) successes)))))}}
    :abilities [{:msg "force the Corp to draw 1 card"
                 :req (req (and run
                                (is-central? (:server run))
                                (:no-action run)
                                (not current-ice)
                                (not (get-in @state [:per-turn (:cid card)]))
                                (empty? (let [successes (turn-events state side :successful-run)]
                                          (filter #(is-central? %) successes)))))
                 :effect (req (draw state :corp) (swap! state assoc-in [:per-turn (:cid card)] true))}]}

   "Leela Patel: Trained Pragmatist"
   {:events {:agenda-scored
             {:effect (req (toast state :runner
                                  (str "Click Leela Patel: Trained Pragmatist to add 1 unrezzed card to HQ.") "info")
                           (update! state :runner (assoc card :bounce-hq true)))}
             :agenda-stolen
             {:effect (req (toast state :runner
                                  (str "Click Leela Patel: Trained Pragmatist to add 1 unrezzed card to HQ.") "info")
                           (update! state side (assoc card :bounce-hq true)))}}
    :abilities [{:req (req (:bounce-hq card))
                 :choices {:req #(and (not (:rezzed %)) (= (:side %) "Corp"))} :player :runner
                 :priority true
                 :msg (msg "add " (card-str state target) " to HQ")
                 :effect (effect (move :corp target :hand)
                                 (update! (dissoc (get-card state card) :bounce-hq)))}]}

   "MaxX: Maximum Punk Rock"
   (let [ability {:msg "trash the top 2 cards from Stack and draw 1 card"
                  :once :per-turn
                  :effect (effect (mill 2) (draw))}]
   {:flags {:runner-turn-draw true
            :runner-phase-12 (req (some #(card-flag? % :runner-turn-draw true) (all-installed state :runner)))}
    :events {:runner-turn-begins ability}
    :abilities [ability]})

   "Nasir Meidan: Cyber Explorer"
   {:effect (effect (gain :link 1))
    :events {:rez {:req (req (and (:run @state)
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

   "NBN: Making News"
   {:recurring 2}

   "NBN: The World is Yours*"
   {:effect (effect (gain :hand-size-modification 1))}

   "Near-Earth Hub: Broadcast Center"
   {:events {:server-created {:msg "draw 1 card" :once :per-turn :effect (effect (draw 1))}}}

   "Nero Severn: Information Broker"
   {:effect (effect (gain :link 1))
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
                               :effect (effect (play-instant target))}}}]
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

   "Pālanā Foods: Sustainable Growth"
   {:events {:runner-draw {:msg "gain 1 [Credits]"
                           :once :per-turn
                           :effect (effect (gain [:credit 1]))}}}

   "Quetzal: Free Spirit"
   {:abilities [{:once :per-turn :msg "break 1 barrier subroutine"}]}

   "Reina Roja: Freedom Fighter"
   {:effect (effect (gain :link 1))
    :events {:pre-rez {:req (req (and (ice? target) (not (get-in @state [:per-turn (:cid card)]))))
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
                                (update-run-ice state side)
                                (trigger-event state side :ice-subtype-changed)))}]
    :events {:run-ends nil}}

   "Silhouette: Stealth Operative"
   {:events {:successful-run
             {:req (req (= target :hq)) :once :per-turn
              :effect (effect (resolve-ability {:choices {:req installed?}
                                                :effect (effect (expose target)) :msg "expose 1 card"}
                                               card nil))}}}

   "Spark Agency: Worldswide Reach"
   {:events
    {:rez {:req (req (has-subtype? target "Advertisement"))
           :once :per-turn
           :effect (effect (lose :runner :credit 1))
           :msg (msg "make the Runner lose 1 [Credits] by rezzing an advertisement")}}}

   "Strategic Innovations: Future Forward"
   {:events {:runner-turn-ends
                      {:req (req (let [facs (frequencies (map :faction (filter :rezzed (all-installed state :corp))))
                                       hb (get facs "Haas-Bioroid" 0)
                                       ji (get facs "Jinteki" 0)
                                       nb (get facs "NBN" 0)
                                       we (get facs "Weyland" 0)]
                                   (and (> hb ji) (> hb nb) (> hb we) (pos? (count (:discard corp))))))
                       :prompt "Choose a card in Archives to shuffle into R&D"
                       :choices {:req #(and (card-is? % :side :corp) (= (:zone %) [:discard]))}
                       :player :corp :show-discard true :priority true
                       :msg (msg "to shuffle " (if (:seen target) (:title target) "a card")
                                 " into R&D")
                       :effect (effect (move :corp target :deck)
                                       (shuffle! :corp :deck))}}}

   "Sunny Lebeau: Security Specialist"
   {:effect (effect (gain :link 2))}

   "SYNC: Everything, Everywhere"
   {:events {:pre-first-turn {:req (req (= side :corp))
                              :effect (effect (update! (assoc card :sync-front true)) (tag-remove-bonus -1))}}
    :abilities [{:cost [:click 1]
                 :effect (req (if (:sync-front card)
                           (do (tag-remove-bonus state side 1)
                               (trash-resource-bonus state side 2)
                               (update! state side (-> card (assoc :sync-front false) (assoc :code "sync"))))
                           (do (tag-remove-bonus state side -1)
                               (trash-resource-bonus state side -2)
                               (update! state side (-> card (assoc :sync-front true)(assoc :code "09001"))))))
                 :msg (msg "flip their ID")}]}

   "Tennin Institute: The Secrets Within"
   {:flags {:corp-phase-12 (req (and (not= 1 (:turn @state)) (not (:successful-run runner-reg))))}
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
           {:prompt "Add another copy to HQ?"
            :yes-ability {:msg (msg "add a copy of " (:title target) " from R&D to HQ")
                          :effect (effect (move (some #(when (= (:title %) (:title target)) %) (:deck corp)) :hand)
                                          (shuffle! :deck))}}}}}

   "Titan Transnational: Investing In Your Future"
   {:events {:agenda-scored {:msg (msg "add 1 agenda counter to " (:title target))
                             :effect (effect (add-prop target :counter 1))}}}

   "Valencia Estevez: The Angel of Cayambe"
   {:req (req (zero? (get-in @state [:corp :bad-publicity])))
    :effect (effect (gain :corp :bad-publicity 1))}

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
                       :msg (msg "shuffle " (:title (last (:discard runner))) " into their stack")
                       :effect (effect (move :runner (last (:discard runner)) :deck)
                                       (shuffle! :runner :deck))}]
              {:runner-trash wyv})}})
