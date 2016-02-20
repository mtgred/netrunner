(in-ns 'game.core)

(declare add-icon remove-icon)

(def breaker-auto-pump
  "Updates an icebreaker's abilities with a pseudo-ability to trigger the auto-pump routine in
  core, IF we are encountering a rezzed ice with a subtype we can break."
  {:effect
   (req (let [abs (filter #(not (:auto-pump %)) (:abilities card))
              pumpabi (some #(when (:pump %) %) abs)
              pumpcst (when pumpabi (second (drop-while #(and (not= % :credit)
                                                              (not= % "credit"))
                                                        (:cost pumpabi))))
              current-ice (when-not (get-in @state [:run :ending]) (get-card state current-ice))
              strdif (when current-ice (max 0 (- (or (:current-strength current-ice)
                                                     (:strength current-ice))
                                                 (or (:current-strength card)
                                                     (:strength card)))))
              pumpnum (when strdif (int (Math/ceil (/ strdif (:pump pumpabi)))))]
          (update! state side
                   (assoc card :abilities
                          (if (and pumpcst
                                   pumpnum
                                   (rezzed? current-ice)
                                   (or (some #(has-subtype? current-ice %) (:breaks card))
                                       (= (first (:breaks card)) "All"))
                                   (pos? strdif))
                            (vec (cons {:auto-pump true
                                        :cost [:credit (* pumpcst pumpnum)]
                                        :label (str "Match strength of " (:title current-ice))}
                                       abs))
                            abs)))))})

;; IMPORTANT: Icebreakers can only use this shortcut method if they do NOT handle any of the
;; events shown below in the merge below. Wyrm, for example, handles :run-ends and therefore
;; can't use this shortcut. Takes a vector of ice subtypes that can be broken (or ["All"] for
;; AI breakers) and a card definition, and returns a new card definition that hooks up
;; breaker-auto-pump to the necessary events.
(defn auto-icebreaker [breaks cdef]
  (assoc cdef :data (merge (:data cdef) {:breaks breaks})
              :events (merge (:events cdef)
                             {:run breaker-auto-pump
                              :pass-ice breaker-auto-pump
                              :run-ends breaker-auto-pump
                              :ice-strength-changed breaker-auto-pump
                              :ice-subtype-changed breaker-auto-pump
                              :breaker-strength-changed breaker-auto-pump
                              :approach-ice breaker-auto-pump })))

(defn cloud-icebreaker [cdef]
  (assoc cdef :effect (req (add-watch state (keyword (str "cloud" (:cid card)))
                        (fn [k ref old new]
                          (when (and (< (get-in old [:runner :link]) 2)
                                     (> (get-in new [:runner :link]) 1))
                            (gain state :runner :memory (:memoryunits card)))
                          (when (and (> (get-in old [:runner :link]) 1)
                                     (< (get-in new [:runner :link]) 2))
                            (lose state :runner :memory (:memoryunits card))))))
              :leave-play (req (remove-watch state (keyword (str "cloud" (:cid card))))
                               (when (> (get-in @state [:runner :link]) 1)
                                 (lose state :runner :memory (:memoryunits card))))
              :install-cost-bonus (req (when (> (get-in @state [:runner :link]) 1)
                                         [:memory (* -1 (:memoryunits card))]))))

(defn- strength-pump
  "Creates a strength pump ability.
  Cost can be a credit amount or a list of costs e.g. [:credit 2]."
  ([cost strength] (strength-pump cost strength nil))
  ([cost strength all-run]
   {:msg (str "add " strength " strength" (when all-run " for the remainder of the run"))
    :cost [:credit cost]
    :effect (effect (pump card strength (or all-run :encounter)))
    :pump strength}))

(defn- break-sub
  "Creates a break subroutine ability.
  If num = 0 then any number of subs are broken."
  ([cost num] (break-sub cost num nil))
  ([cost num subtype]
   {:msg (str "break " (when (> num 1) "up to ")
              (if (pos? num) num "any number of")
              (when subtype (str " " subtype))
              " subroutine" (when-not (= num 1) "s"))
    :cost [:credit cost]}))

;;; Breaker sets
(defn- cerberus
  "Breaker from the dog set"
  [type]
  (auto-icebreaker [type]
                   {:data {:counter 4}
                    :abilities [{:counter-cost 1
                                 :msg (str "break up to 2 " (lower-case type) " subroutines")}
                                (strength-pump 1 1)]}))

(defn- break-and-enter
  "Breakers from the Break and Entry set"
  [type]
  (cloud-icebreaker {:abilities [{:msg (str "break up to 3 " (lower-case type) " subroutines")
                                  :effect (effect (trash card {:cause :ability-cost}))}]
                      :events (let [cloud {:req (req (has-subtype? target "Icebreaker"))
                                           :effect (effect (update-breaker-strength card))}]
                                {:runner-install cloud :trash cloud :card-moved cloud})
                      :strength-bonus (req (count (filter #(has-subtype? % "Icebreaker")
                                                          (all-installed state :runner))))}))

(defn- global-sec-breaker
  "GlobalSec breakers for Sunny"
  [type]
  (cloud-icebreaker (auto-icebreaker [type] {:abilities [(break-sub 2 0 (lower-case type))
                                                         (strength-pump 2 3)]})))

;;; Icebreaker definitions
(def cards-icebreakers
  {"Alpha"
   (auto-icebreaker ["All"]
                    {:abilities [{:cost [:credit 1]
                                  :req (req (= (:position run) (count (:ices run))))
                                  :msg "break 1 subroutine on the outermost ICE protecting this server"}
                                 (strength-pump 1 1)]})

   "Alias"
   (auto-icebreaker ["Sentry"]
                    {:abilities [{:cost [:credit 1]
                                  :req (req (#{:hq :rd :archives} (first (:server run))))
                                  :msg "break 1 sentry subroutine"}
                                 (strength-pump 2 3)]})

   "Atman"
   {:prompt "How many power counters?"
    :choices :credit
    :msg (msg "add " target " power counters")
    :effect (effect (add-prop card :counter target))
    :abilities [(break-sub 1 1)]
    :strength-bonus (req (or (:counter card) 0))
    :events {:counter-added {:req (req (= :cid target) (:cid card))
                             :effect (effect (update-breaker-strength card))}}}

   "Aurora"
   (auto-icebreaker ["Barrier"]
                    {:abilities [(break-sub 2 1 "barrier")
                                 (strength-pump 2 3)]})

   "Battering Ram"
   (auto-icebreaker ["Barrier"]
                    {:abilities [(break-sub 2 2 "barrier")
                                 (strength-pump 1 1 :all-run)]})

   "BlacKat"
   {:abilities [(break-sub 1 1 "barrier")
                {:cost [:credit 1]
                 :msg "break up to 3 barrier subroutines (using a stealth [Credits])"}
                (strength-pump 2 1)
                {:cost [:credit 2]
                 :msg "add 2 strength (using at least 1 stealth [Credits])"
                 :effect (effect (pump card 2)) :pump 2}]}

   "Breach"
   (auto-icebreaker ["Barrier"]
                    {:abilities [{:cost [:credit 2]
                                  :req (req (#{:hq :rd :archives} (first (:server run))))
                                  :msg "break 3 barrier subroutines"}
                                 (strength-pump 2 4)]})

   "Cerberus \"Cuj.0\" H3"
   (cerberus "Sentry")

   "Cerberus \"Rex\" H2"
   (cerberus "Code Gate")

   "Cerberus \"Lady\" H1"
   (cerberus "Barrier")

   "Chameleon"
   {:prompt "Choose one subtype"
    :choices ["Barrier" "Code Gate" "Sentry"]
    :msg (msg "choose " target)
    :effect (effect (set-prop card :ice-type target))
    :events {:runner-turn-ends {:msg "add itself to Grip" :effect (effect (move card :hand))}}
    :abilities [{:cost [:credit 1] :msg (msg "break 1 " (:ice-type card) " subroutine")}]}

   "Corroder"
   (auto-icebreaker ["Barrier"]
                    {:abilities [(break-sub 1 1 "barrier")
                                 (strength-pump 1 1)]})

   "Creeper"
   (cloud-icebreaker
     (auto-icebreaker ["Sentry"]
                      {:abilities [(break-sub 2 1 "sentry")
                                   (strength-pump 1 1)]}))

   "Crowbar"
   (break-and-enter "Code Gate")

   "Crypsis"
   (auto-icebreaker ["All"]
                    {:abilities [(break-sub 1 1 "ice")
                                 (strength-pump 1 1)
                                 {:cost [:click 1] :msg "place 1 virus counter"
                                  :effect (effect (add-prop card :counter 1))}
                                 {:counter-cost 1
                                  :label "Remove 1 hosted virus counter"
                                  :msg "remove 1 virus counter"}]})

   "Cyber-Cypher"
   (auto-icebreaker ["Code Gate"]
                    {:prompt "Choose a server where this copy of Cyber-Cypher can be used:"
                     :choices (req servers)
                     :effect (effect (update! (assoc card :named-target target)))
                     :leave-play (effect (update! (dissoc card :named-target)))
                     :abilities [(break-sub 1 1 "code gate")
                                 (strength-pump 1 1)]})

   "Dagger"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 1 1 "sentry")
                                 (strength-pump 1 5)]})

   "Darwin"
   {:flags {:runner-phase-12 true}
    :events {:purge {:effect (effect (update-breaker-strength card))}}
    :abilities [(break-sub 2 1 "ice")
                {:label "Place 1 virus counter (start of turn)"
                 :cost [:credit 1]
                 :msg "place 1 virus counter"
                 :req (req (:runner-phase-12 @state))
                 :effect (effect (add-prop card :counter 1)
                                 (update-breaker-strength card))}]
    :strength-bonus (req (or (get-virus-counters state side card) 0))}

   "Deus X"
   {:prevent {:damage [:net]}
    :abilities [{:msg "break any number of AP subroutines"
                 :effect (effect (trash card {:cause :ability-cost}))}
                {:msg "prevent any amount of net damage"
                 :effect (effect (trash card {:cause :ability-cost})
                                 (damage-prevent :net Integer/MAX_VALUE))}]}

   "Eater"
   (auto-icebreaker ["All"]
                    {:abilities [{:cost [:credit 1]
                                  :msg "break ICE subroutine and access 0 cards this run"
                                  :effect (effect (max-access 0))}
                                 (strength-pump 1 1)]})

   "Endless Hunger"
   {:abilities [{:label "Trash 1 installed card to break 1 \"End the run.\" subroutine"
                 :prompt "Choose a card to trash for Endless Hunger"
                 :choices {:req #(and (= (:side %) "Runner") (:installed %))}
                 :msg (msg "trash " (:title target)
                           " and break 1 \"[Subroutine] End the run.\" subroutine")
                 :effect (effect (trash target {:unpreventable true}))}]}

   "Faerie"
   (auto-icebreaker ["Sentry"]
                    {:abilities [{:msg "break any number of sentry subroutines"
                                  :effect (effect (trash card))}
                                 (strength-pump 1 1)]})

   "Faust"
   {:abilities [{:label "Trash 1 card from Grip to break 1 subroutine"
                 :prompt "Choose a card from your grip to trash for Faust"
                 :choices {:req in-hand?}
                 :msg (msg "trash " (:title target) " and break 1 subroutine")
                 :effect (effect (trash target {:unpreventable true}))}
                {:label "Trash 1 card from Grip to add 2 strength"
                 :prompt "Choose a card from your grip to trash for Faust"
                 :choices {:req in-hand?}
                 :msg (msg "trash " (:title target) " and add 2 strength")
                 :effect (effect (trash target {:unpreventable true}) (pump card 2))}]}

   "Femme Fatale"
   (auto-icebreaker ["Sentry"]
                    {:prompt "Choose a piece of ICE to target for bypassing"
                     :choices {:req ice?}
                     :leave-play (req (remove-icon state side card))
                     :effect (req (let [ice target
                                        serv (zone->name (second (:zone ice)))]
                                    (add-icon state side card ice "F" "blue")
                                    (system-msg state side
                                                (str "chooses " (card-str state ice)
                                                     " for Femme Fatale's bypass ability"))))
                     :abilities [(break-sub 1 1 "sentry")
                                 (strength-pump 2 1)]})

   "Force of Nature"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 2 2 "code gate")
                                 (strength-pump 1 1)]})

   "Garrote"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 1 1 "sentry")
                                 (strength-pump 1 1)]})

   "Gordian Blade"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 1 1 "code gate")
                                 (strength-pump 1 1 :all-run)]})

   "Gingerbread"
   (auto-icebreaker ["Tracer"]
                    {:abilities [(break-sub 1 1 "tracer")
                                 (strength-pump 2 3)]})

   "GS Sherman M3"
   (global-sec-breaker "Barrier")

   "GS Shrike M2"
   (global-sec-breaker "Sentry")

   "GS Striker M1"
   (global-sec-breaker "Code Gate")

   "Inti"
   (auto-icebreaker ["Barrier"]
                    {:abilities [(break-sub 1 1 "barrier")
                                 (strength-pump 2 1 :all-run)]})

   "Knight"
   {:abilities [{:label "Host Knight on a piece of ICE" :cost [:click 1]
                 :choices {:req #(and (ice? %)
                                      (= (last (:zone %)) :ices)
                                      (not (some (fn [c] (has-subtype? c "Caïssa"))
                                                 (:hosted %))))}
                 :msg (msg "host it on " (card-str state target))
                 :effect (effect (host target card))}
                {:cost [:credit 2] :msg "break 1 subroutine on the host ICE"}]}

   "Leviathan"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 3 3 "code gate")
                                 (strength-pump 3 5)]})

   "Morning Star"
   {:abilities [(break-sub 1 0 "barrier")]}

   "Mimic"
   {:abilities [(break-sub 1 1 "sentry")]}

   "Mongoose"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 1 2 "sentry")
                                 (strength-pump 2 2)]})

   "Ninja"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 1 1 "sentry")
                                 (strength-pump 3 5)]})

   "Passport"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [{:cost [:credit 1]
                                  :req (req (#{:hq :rd :archives} (first (:server run))))
                                  :msg "break 1 code gate subroutine"}
                                 (strength-pump 2 2)]})

   "Omega"
   (auto-icebreaker ["All"]
                    {:abilities [{:cost [:credit 1] :req (req (= 1 (:position run)))
                                  :msg "break 1 subroutine on the innermost ICE protecting this server"}
                                 (strength-pump 1 1)]})

   "Overmind"
   (auto-icebreaker ["All"]
                    {:effect (effect (set-prop card :counter (:memory runner)))
                     :abilities [{:counter-cost 1 :msg "break 1 subroutine"}
                                 (strength-pump 1 1)]})

   "Peacock"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 2 1 "code gate")
                                 (strength-pump 2 3)]})

   "Pipeline"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 1 1 "sentry")
                                 (strength-pump 2 1 :all-run)]})

   "Refractor"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 1 1 "code gate")
                                 (strength-pump 1 3)]})

   "Sage"
   {:abilities [{:cost [:credit 2] :req (req (or (has-subtype? current-ice "Barrier")
                                                 (has-subtype? current-ice "Code Gate")))
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
                                 (strength-pump 1 1)]})

   "Sharpshooter"
   (auto-icebreaker ["Destroyer"]
                    {:abilities [{:msg "break any number of destroyer subroutines" :effect (effect (trash card {:cause :ability-cost}))}
                                 (strength-pump 1 2)]})

   "Shiv"
   (break-and-enter "Sentry")

   "Spike"
   (break-and-enter "Barrier")

   "Study Guide"
   {:abilities [(break-sub 1 1 "code gate")
                {:cost [:credit 2] :msg "place 1 power counter"
                 :effect (effect (add-prop card :counter 1)
                                 (update-breaker-strength card))}]
    :strength-bonus (req (:counter card 0))}

   "Switchblade"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 1 0 "sentry")
                                 (strength-pump 1 7)]})

   "Torch"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 1 1 "code gate")
                                 (strength-pump 1 1)]})

   "Wyrm"
   {:abilities [{:cost [:credit 3] :msg "break 1 subroutine on ICE with 0 or less strength"}
                {:cost [:credit 1]
                 :label "Give -1 strength to current ice"
                 :msg (msg "give -1 strength to " (:title current-ice))
                 :req (req current-ice)
                 :effect (req (update! state side
                                       (update-in card [:wyrm-count] (fnil inc 0)))
                              (update-ice-strength state side current-ice))}
                (strength-pump 1 1)]
    :events (let [wy {:effect (req (update! state side (dissoc card :wyrm-count)))}]
              {:pre-ice-strength {:req (req (and (= (:cid target) (:cid current-ice))
                                                 (:wyrm-count card)))
                                  :effect (req (let [c (:wyrm-count (get-card state card))]
                                                 (ice-strength-bonus state side (- c) target)))}
               :pass-ice wy :run-ends wy})}

   "Yog.0"
   {:abilities [(break-sub 0 1 "code gate")]}

   "ZU.13 Key Master"
   (cloud-icebreaker
     (auto-icebreaker ["Code Gate"]
                      {:abilities [(break-sub 1 1 "code gate")
                                   (strength-pump 1 1)]}))})
