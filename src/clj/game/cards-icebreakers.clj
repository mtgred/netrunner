(in-ns 'game.core)

; Updates an icebreaker's abilities with a pseudo-ability to trigger the auto-pump routine in core, IF we are
; encountering a rezzed ice with a subtype we can break.
(def breaker-auto-pump
  {:effect
   (req (let [abs (filter #(not (:dynamic %)) (:abilities card)) pumpabi (some #(when (:pump %) %) abs)
              pumpcst (when pumpabi (second (drop-while #(and (not= % :credit) (not= % "credit")) (:cost pumpabi))))
              current-ice (when-not (get-in @state [:run :ending]) (get-card state current-ice))
              strdif (when current-ice (max 0 (- (or (:current-strength current-ice) (:strength current-ice))
                                                 (or (:current-strength card) (:strength card)))))
              pumpnum (when strdif (int (Math/ceil (/ strdif (:pump pumpabi)))))]
          (update! state side (assoc card :abilities
                                          (if (and pumpcst pumpnum (:rezzed current-ice)
                                                   (or (some #(has? current-ice :subtype %) (:breaks card))
                                                       (= (first (:breaks card)) "All"))
                                                   (> strdif 0))
                                            (vec (cons {:dynamic :auto-pump :cost [:credit (* pumpcst pumpnum)]
                                                        :label (str "Match strength of " (:title current-ice))} abs))
                                            abs)))))})

; IMPORTANT: Icebreakers can only use this shortcut method if they do NOT handle any of the events shown below in the
; merge below. Wyrm, for example, handles :run-ends and therefore can't use this shortcut.
; Takes a vector of ice subtypes that can be broken (or ["All"] for AI breakers) and a card definition, and returns
; a new card definition that hooks up breaker-auto-pump to the necessary events.
(defn auto-icebreaker [breaks cdef]
  (assoc cdef :data (merge (:data cdef) {:breaks breaks})
              :events (merge (:events cdef)
                             {:run breaker-auto-pump :pass-ice breaker-auto-pump
                              :run-ends breaker-auto-pump :ice-strength-changed breaker-auto-pump
                              :ice-subtype-changed breaker-auto-pump :breaker-strength-changed breaker-auto-pump
                              :approach-ice breaker-auto-pump })))

(defn cloud-icebreaker [cdef]
  (assoc cdef :effect (req (add-watch state (keyword (str "cloud" (:cid card)))
                        (fn [k ref old new]
                          (when (and (< (get-in old [:runner :link]) 2)
                                     (> (get-in new [:runner :link]) 1))
                            (gain state :runner :memory (:memoryunits card)))
                          (when (and (> (get-in old [:runner :link]) 1)
                                     (< (get-in new [:runner :link]) 2))
                            (gain state :runner :memory (* -1 (:memoryunits card)))))))
              :leave-play (req (remove-watch state (keyword (str "cloud" (:cid card))))
                               (when (> (get-in @state [:runner :link]) 1)
                                 (lose state :runner :memory (:memoryunits card))))
              :install-cost-bonus (req (if (> (get-in @state [:runner :link]) 1) [:memory (* -1 (:memoryunits card))]))))

(def cards-icebreakers
  {"Alpha"
   (auto-icebreaker ["All"]
                    {:abilities [{:cost [:credit 1] :req (req (= (:position run) (count (:ices run))))
                                  :msg "break 1 subroutine on the outermost ICE protecting this server"}
                                 {:cost [:credit 1] :msg "add 1 strength" :effect (effect (pump card 1)) :pump 1}]})

   "Alias"
   (auto-icebreaker ["Sentry"]
                    {:abilities [{:cost [:credit 1] :req (req (#{:hq :rd :archives} (first (:server run))))
                                  :msg "break 1 sentry subroutine"}
                                 {:cost [:credit 2] :msg "add 3 strength" :effect (effect (pump card 3)) :pump 3}]})

   "Atman"
   {:prompt "How many power counters?" :choices :credit :msg (msg "add " target " power counters")
    :effect (effect (add-prop card :counter target))
    :abilities [{:cost [:credit 1] :msg "break 1 subroutine"}]
    :strength-bonus (req (or (:counter card) 0))
    :events {:counter-added {:req (req (= :cid target) (:cid card))
                             :effect (effect (update-breaker-strength card))}}}

   "Aurora"
   (auto-icebreaker ["Barrier"]
                    {:abilities [{:cost [:credit 2] :msg "break 1 barrier subroutine"}
                                 {:cost [:credit 2] :msg "add 3 strength" :effect (effect (pump card 3)) :pump 3}]})

   "Battering Ram"
   (auto-icebreaker ["Barrier"]
                    {:abilities [{:cost [:credit 2] :msg "break up to 2 barrier subroutines"}
                                 {:cost [:credit 1] :msg "add 1 strength for the remainder of this run"
                                  :effect (effect (pump card 1 :all-run)) :pump 1}]})

   "BlacKat"
   {:abilities [{:cost [:credit 1] :msg "break 1 barrier subroutine"}
                {:cost [:credit 1] :msg "break up to 3 barrier subroutines (using a stealth [Credits])"}
                {:cost [:credit 2] :msg "add 1 strength" :effect (effect (pump card 1)) :pump 1}
                {:cost [:credit 2] :msg "add 2 strength (using at least 1 stealth [Credits])" :effect (effect (pump card 2)) :pump 2}]}

   "Breach"
   (auto-icebreaker ["Barrier"]
                    {:abilities [{:cost [:credit 2] :req (req (#{:hq :rd :archives} (first (:server run))))
                                  :msg "break 3 barrier subroutines"}
                                 {:cost [:credit 2] :msg "add 4 strength" :effect (effect (pump card 4)) :pump 4}]})

   "Cerberus \"Cuj.0\" H3"
   (auto-icebreaker ["Sentry"]
                    {:data {:counter 4}
                     :abilities [{:counter-cost 1 :msg "break up to 2 sentry subroutines"}
                                 {:cost [:credit 1] :msg "add 1 strength" :effect (effect (pump card 1)) :pump 1}]})

   "Cerberus \"Rex\" H2"
   (auto-icebreaker ["Code Gate"]
                    {:data {:counter 4}
                     :abilities [{:counter-cost 1 :msg "break up to 2 code gate subroutines"}
                                 {:cost [:credit 1] :msg "add 1 strength" :effect (effect (pump card 1)) :pump 1}]})

   "Cerberus \"Lady\" H1"
   (auto-icebreaker ["Barrier"]
                    {:data {:counter 4}
                     :abilities [{:counter-cost 1 :msg "break up to 2 barrier subroutines"}
                                 {:cost [:credit 1] :msg "add 1 strength" :effect (effect (pump card 1)) :pump 1}]})

   "Chameleon"
   {:prompt "Choose one subtype" :choices ["Barrier" "Code Gate" "Sentry"]
    :msg (msg "choose " target) :effect (effect (set-prop card :ice-type target))
    :end-turn {:msg "add itself to Grip" :effect (effect (move card :hand))}
    :abilities [{:cost [:credit 1] :msg (msg "break 1 " (:ice-type card) " subroutine")}]}

   "Corroder"
   (auto-icebreaker ["Barrier"]
                    {:abilities [{:cost [:credit 1] :msg "break 1 barrier subroutine"}
                                 {:cost [:credit 1] :msg "add 1 strength" :effect (effect (pump card 1)) :pump 1}]})

   "Creeper"
   (cloud-icebreaker
     (auto-icebreaker ["Sentry"]
                      {:abilities [{:cost [:credit 2] :msg "break 1 sentry subroutine"}
                                   {:cost [:credit 1] :msg "add 1 strength" :effect (effect (pump card 1)) :pump 1}]}))

   "Crowbar"
   (cloud-icebreaker {:abilities [{:msg "break up to 3 code gate subroutines" :effect (effect (trash card {:cause :ability-cost}))}]
                      :events (let [cloud {:req (req (has? target :subtype "Icebreaker"))
                                           :effect (effect (update-breaker-strength card))}]
                                {:runner-install cloud :trash cloud :card-moved cloud})
                      :strength-bonus (req (count (filter #(has? % :subtype "Icebreaker") (all-installed state :runner))))})

   "Crypsis"
   (auto-icebreaker ["All"]
                    {:abilities [{:cost [:credit 1] :msg "break ICE subroutine"}
                                 {:cost [:credit 1] :msg "add 1 strength" :effect (effect (pump card 1)) :pump 1}
                                 {:cost [:click 1] :msg "place 1 virus counter"
                                  :effect (effect (add-prop card :counter 1))}
                                 {:counter-cost 1 :label "Remove 1 hosted virus counter" :msg "remove 1 virus counter"}]})

   "Cyber-Cypher"
   (auto-icebreaker ["Code Gate"]
                    {:prompt "Choose a server where this copy of Cyber-Cypher can be used:" :choices (req servers)
                     :effect (effect (update! (assoc card :named-target target)))
                     :leave-play (effect (update! (dissoc card :named-target)))
                     :abilities [{:cost [:credit 1] :msg "break 1 code gate subroutine"}
                                 {:cost [:credit 1] :msg "add 1 strength" :effect (effect (pump card 1)) :pump 1}]})

   "Dagger"
   (auto-icebreaker ["Sentry"]
                    {:abilities [{:cost [:credit 1] :msg "break 1 sentry subroutine"}
                                 {:cost [:credit 1] :msg "add 5 strength" :effect (effect (pump card 5)) :pump 5}]})

   "Darwin"
   {:events {:runner-turn-begins
             {:optional {:prompt "Place 1 virus counter on Darwin?"
                         :yes-ability {:cost [:credit 1]
                                       :msg "place 1 virus counter"
                                       :effect (effect (add-prop card :counter 1)
                                                       (update-breaker-strength card))}}}
             :purge {:effect (effect (update-breaker-strength card))}}
    :abilities [{:cost [:credit 2] :msg "break ICE subroutine"}]
    :strength-bonus (req (or (get-virus-counters state side card) 0))}

   "Deus X"
   {:prevent {:damage [:net]}
    :abilities [{:msg "break any number of AP subroutines" :effect (effect (trash card {:cause :ability-cost}))}
                {:msg "prevent any amount of net damage"
                 :effect (effect (trash card {:cause :ability-cost}) (damage-prevent :net Integer/MAX_VALUE))}]}

   "Eater"
   (auto-icebreaker ["All"]
                    {:abilities [{:cost [:credit 1] :msg "break ICE subroutine and access 0 cards this run"
                                  :effect (effect (max-access 0))}
                                 {:cost [:credit 1] :msg "add 1 strength" :effect (effect (pump card 1)) :pump 1}]})

   "Endless Hunger"
   {:abilities [{:label "Trash 1 installed card to break 1 \"End the run.\" subroutine"
                 :prompt "Choose a card to trash for Endless Hunger"
                 :choices {:req #(and (= (:side %) "Runner") (:installed %))}
                 :msg (msg "trash " (:title target) " and break 1 \"[Subroutine] End the run.\" subroutine")
                 :effect (effect (trash target {:cause :ability-cost}))}]}

   "Faerie"
   (auto-icebreaker ["Sentry"]
                    {:abilities [{:msg "break any number of sentry subroutines" :effect (effect (trash card))}
                                 {:cost [:credit 1] :msg "add 1 strength" :effect (effect (pump card 1)) :pump 1}]})

   "Faust"
   {:abilities [{:label "Trash 1 card from Grip to break 1 subroutine"
                 :prompt "Choose a card from your grip to trash for Faust"
                 :choices {:req #(= (:zone %) [:hand])}
                 :msg (msg "trash " (:title target) " and break 1 subroutine")
                 :effect (effect (trash target {:unpreventable true}))}
                {:label "Trash 1 card from Grip to add 2 strength"
                 :prompt "Choose a card from your grip to trash for Faust"
                 :choices {:req #(= (:zone %) [:hand])}
                 :msg (msg "trash " (:title target) " and add 2 strength")
                 :effect (effect (trash target {:unpreventable true}) (pump card 2))}]}

   "Femme Fatale"
   (auto-icebreaker ["Sentry"]
                    {:prompt "Choose a piece of ICE to target for bypassing" :choices {:req #(= (:type %) "ICE")}
                     :effect (req (let [ice target
                                        serv (zone->name (second (:zone ice)))]
                                    (system-msg state side
                                      (str "chooses " (if (:rezzed ice) (:title ice) "the ICE") " at position "
                                        (ice-index state ice) " of " serv " for Femme Fatale's bypass ability"))))
                     :abilities [{:cost [:credit 1] :msg "break 1 sentry subroutine"}
                                 {:cost [:credit 2] :msg "add 1 strength" :effect (effect (pump card 1)) :pump 1}]})

   "Force of Nature"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [{:cost [:credit 2] :msg "break up to 2 code gate subroutines"}
                                 {:cost [:credit 1] :msg "add 1 strength" :effect (effect (pump card 1)) :pump 1}]})

   "Garrote"
   (auto-icebreaker ["Sentry"]
                    {:abilities [{:cost [:credit 1] :msg "break 1 sentry subroutine"}
                                 {:cost [:credit 1] :msg "add 1 strength" :effect (effect (pump card 1)) :pump 1}]})

   "Gordian Blade"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [{:cost [:credit 1] :msg "break 1 code gate subroutine"}
                                 {:cost [:credit 1] :msg "add 1 strength for the remainder of this run" :pump 1
                                  :effect (effect (pump card 1 :all-run))}]})

   "Gingerbread"
   (auto-icebreaker ["Tracer"]
                    {:abilities [{:cost [:credit 1] :msg "break 1 tracer subroutine"}
                                 {:cost [:credit 2] :msg "add 3 strength" :effect (effect (pump card 3)) :pump 3}]})

   "GS Sherman M3"
   (cloud-icebreaker
     (auto-icebreaker ["Barrier"]
                      {:abilities [{:cost [:credit 2] :msg "break any number of barrier subroutines"}
                                   {:cost [:credit 2] :msg "add 3 strength" :effect (effect (pump card 3)) :pump 3}]}))

   "GS Shrike M2"
   (cloud-icebreaker
     (auto-icebreaker ["Sentry"]
                      {:abilities [{:cost [:credit 2] :msg "break any number of sentry subroutines"}
                                   {:cost [:credit 2] :msg "add 3 strength" :effect (effect (pump card 3)) :pump 3}]}))

   "GS Striker M1"
   (cloud-icebreaker
     (auto-icebreaker ["Code Gate"]
                      {:abilities [{:cost [:credit 2] :msg "break any number of code gate subroutines"}
                                   {:cost [:credit 2] :msg "add 3 strength" :effect (effect (pump card 3)) :pump 3}]}))

   "Inti"
   (auto-icebreaker ["Barrier"]
                    {:abilities [{:cost [:credit 1] :msg "break 1 barrier subroutine"}
                                 {:cost [:credit 2] :msg "add 1 strength for the remainder of this run"
                                  :effect (effect (pump card 1 :all-run)) :pump 1}]})

   "Knight"
   {:abilities [{:label "Host Knight on a piece of ICE" :cost [:click 1]
                 :choices {:req #(and (= (:type %) "ICE")
                                      (= (last (:zone %)) :ices)
                                      (not (some (fn [c] (has? c :subtype "Caïssa")) (:hosted %))))}
                 :msg (msg "host it on " (if (:rezzed target) (:title target) "a piece of ICE"))
                 :effect (effect (host target card))}
                {:cost [:credit 2] :msg "break 1 subroutine on the host ICE"}]}

   "Leviathan"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [{:cost [:credit 3] :msg "break up to 3 code gate subroutines"}
                                 {:cost [:credit 3] :msg "add 5 strength" :effect (effect (pump card 5)) :pump 5}]})

   "Morning Star"
   {:abilities [{:cost [:credit 1] :msg "break any number of barrier subroutines"}]}

   "Mimic"
   {:abilities [{:cost [:credit 1] :msg "break 1 sentry subroutine"}]}

   "Ninja"
   (auto-icebreaker ["Sentry"]
                    {:abilities [{:cost [:credit 1] :msg "break 1 sentry subroutine"}
                                 {:cost [:credit 3] :msg "add 5 strength" :effect (effect (pump card 5)) :pump 5}]})

   "Passport"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [{:cost [:credit 1] :req (req (#{:hq :rd :archives} (first (:server run))))
                                  :msg "break 1 code gate subroutine"}
                                 {:cost [:credit 2] :msg "add 2 strength" :effect (effect (pump card 2)) :pump 2}]})

   "Omega"
   (auto-icebreaker ["All"]
                    {:abilities [{:cost [:credit 1] :req (req (= 1 (:position run)))
                                  :msg "break 1 subroutine on the innermost ICE protecting this server"}
                                 {:cost [:credit 1] :msg "add 1 strength" :effect (effect (pump card 1)) :pump 1}]})

   "Overmind"
   (auto-icebreaker ["All"]
                    {:effect (effect (set-prop card :counter (:memory runner)))
                     :abilities [{:counter-cost 1 :msg "break 1 subroutine"}
                                 {:cost [:credit 1] :msg "add 1 strength" :effect (effect (pump card 1)) :pump 1}]})

   "Peacock"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [{:cost [:credit 2] :msg "break 1 code gate subroutine"}
                                 {:cost [:credit 2] :msg "add 3 strength" :effect (effect (pump card 3)) :pump 3}]})

   "Pipeline"
   (auto-icebreaker ["Sentry"]
                    {:abilities [{:cost [:credit 1] :msg "break 1 sentry subroutine"}
                                 {:cost [:credit 2] :msg "add 1 strength for the remainder of this run"
                                  :effect (effect (pump card 1 :all-run)) :pump 1}]})

   "Refractor"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [{:cost [:credit 1] :msg "break 1 code gate subroutine"}
                                 {:cost [:credit 1] :msg "add 3 strength" :effect (effect (pump card 3)) :pump 3}]})

   "Sage"
   {:abilities [{:cost [:credit 2] :req (req (or (has? current-ice :subtype "Barrier")
                                                 (has? current-ice :subtype "Code Gate")))
                 :msg "break 1 code gate or barrier subroutine"}]
    :effect (req (add-watch state (keyword (str "sage" (:cid card)))
                            (fn [k ref old new]
                              (when (not= (get-in old [:runner :memory]) (get-in new [:runner :memory]))
                                (update-breaker-strength ref side card))))
                 (update-breaker-strength state side card))
    :leave-play (req (remove-watch state (keyword (str "sage" (:cid card)))))
    :strength-bonus (req (:memory runner))}

   "Snowball"
   (auto-icebreaker ["Barrier"]
                    {:abilities [{:cost [:credit 1] :msg "break 1 barrier subroutine"
                                  :effect (effect (pump card 1 :all-run))}
                                 {:cost [:credit 1] :msg "add 1 strength" :effect (effect (pump card 1)) :pump 1}]})

   "Sharpshooter"
   (auto-icebreaker ["Destroyer"]
                    {:abilities [{:msg "break any number of destroyer subroutines" :effect (effect (trash card {:cause :ability-cost}))}
                                 {:cost [:credit 1] :msg "add 2 strength" :effect (effect (pump card 2)) :pump 2}]})

   "Shiv"
   (cloud-icebreaker {:abilities [{:msg "break up to 3 sentry subroutines" :effect (effect (trash card {:cause :ability-cost}))}]
                      :events (let [cloud {:req (req (has? target :subtype "Icebreaker"))
                                           :effect (effect (update-breaker-strength card))}]
                                {:runner-install cloud :trash cloud :card-moved cloud})
                      :strength-bonus (req (count (filter #(has? % :subtype "Icebreaker") (all-installed state :runner))))})

   "Spike"
   (cloud-icebreaker {:abilities [{:msg "break up to 3 barrier subroutines" :effect (effect (trash card {:cause :ability-cost}))}]
                      :events (let [cloud {:req (req (has? target :subtype "Icebreaker"))
                                           :effect (effect (update-breaker-strength card))}]
                                {:runner-install cloud :trash cloud :card-moved cloud})
                      :strength-bonus (req (count (filter #(has? % :subtype "Icebreaker") (all-installed state :runner))))})

   "Study Guide"
   {:abilities [{:cost [:credit 1] :msg "break 1 code gate subroutine"}
                {:cost [:credit 2] :msg "place 1 power counter" :effect (effect (add-prop card :counter 1)
                                                                                (update-breaker-strength card))}]
    :strength-bonus (req (or (:counter card) 0))}

   "Switchblade"
   (auto-icebreaker ["Sentry"]
                    {:abilities [{:cost [:credit 1] :msg "break any number of sentry subroutines"}
                                 {:cost [:credit 1] :msg "add 7 strength" :effect (effect (pump card 7)) :pump 7}]})

   "Torch"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [{:cost [:credit 1] :msg "break 1 code gate subroutine"}
                                 {:cost [:credit 1] :msg "add 1 strength" :effect (effect (pump card 1)) :pump 1}]})

   "Wyrm"
   {:abilities [{:cost [:credit 3] :msg "break 1 subroutine on ICE with 0 or less strength"}
                {:cost [:credit 1]
                 :label "give -1 strength to current ice" :msg (msg "give -1 strength to " (:title current-ice))
                 :req (req current-ice)
                 :effect (req (update! state side (update-in card [:wyrm-count] (fnil #(+ % 1) 0)))
                              (update-ice-strength state side current-ice))}
                {:cost [:credit 1] :msg "add 1 strength" :effect (effect (pump card 1)) :pump 1}]
    :events (let [wy {:effect (req (update! state side (dissoc card :wyrm-count)))}]
              {:pre-ice-strength {:req (req (and (= (:cid target) (:cid current-ice)) (:wyrm-count card)))
                                  :effect (req (let [c (:wyrm-count (get-card state card))]
                                                 (ice-strength-bonus state side (- c) target)))}
               :pass-ice wy :run-ends wy})}

   "Yog.0"
   {:abilities [{:msg "break 1 code gate subroutine"}]}

   "ZU.13 Key Master"
   (cloud-icebreaker
     (auto-icebreaker ["Code Gate"]
                      {:abilities [{:cost [:credit 1] :msg "break 1 code gate subroutine"}
                                   {:cost [:credit 1] :msg "add 1 strength" :effect (effect (pump card 1)) :pump 1}]}))})
