(in-ns 'game.core)

(declare add-icon remove-icon)

(def breaker-auto-pump
  "Updates an icebreaker's abilities with a pseudo-ability to trigger the
  auto-pump routine in core, IF we are encountering a rezzed ice with a subtype
  we can break."
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

;; Takes a vector of ice subtypes that can be broken (or ["All"] for
;; AI breakers) and a card definition, and returns a new card definition that
;; hooks up breaker-auto-pump to the necessary events.
;; IMPORTANT: Events on cdef take precedence, and should call
;; (:effect breaker-auto-pump) themselves.
(defn auto-icebreaker [breaks cdef]
  (assoc cdef :data (merge {:breaks breaks} (:data cdef))
              :events (merge {:run breaker-auto-pump
                              :pass-ice breaker-auto-pump
                              :run-ends breaker-auto-pump
                              :ice-strength-changed breaker-auto-pump
                              :ice-subtype-changed breaker-auto-pump
                              :breaker-strength-changed breaker-auto-pump
                              :approach-ice breaker-auto-pump }
                             (:events cdef))))

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
  ([cost num subtype] (break-sub cost num subtype nil))
  ([cost num subtype effect]
   {:msg (str "break " (when (> num 1) "up to ")
              (if (pos? num) num "any number of")
              (when subtype (str " " subtype))
              " subroutine" (when-not (= num 1) "s"))
    :cost [:credit cost]
    :effect effect}))

;;; Breaker sets
(defn- cerberus
  "Breaker from the dog set"
  [type]
  (auto-icebreaker [type]
                   {:data {:counter {:power 4}}
                    :abilities [{:counter-cost [:power 1]
                                 :msg (str "break up to 2 " (lower-case type) " subroutines")}
                                (strength-pump 1 1)]}))

(defn- break-and-enter
  "Breakers from the Break and Entry set"
  [type]
  (cloud-icebreaker {:abilities [{:msg (str "break up to 3 " (lower-case type) " subroutines")
                                  :effect (effect (trash card {:cause :ability-cost}))}]
                      :events (let [cloud {:silent (req true)
                                           :req (req (has-subtype? target "Icebreaker"))
                                           :effect (effect (update-breaker-strength card))}]
                                {:runner-install cloud :trash cloud :card-moved cloud})
                      :strength-bonus (req (count (filter #(has-subtype? % "Icebreaker")
                                                          (all-installed state :runner))))}))

(defn- global-sec-breaker
  "GlobalSec breakers for Sunny"
  [type]
  (cloud-icebreaker (auto-icebreaker [type] {:abilities [(break-sub 2 0 (lower-case type))
                                                         (strength-pump 2 3)]})))

(defn- deva
  "Deva breakers"
  [name]
  (auto-icebreaker ["All"]
                   {:abilities [(break-sub 1 1 "ICE")
                                (strength-pump 1 1)
                                {:req (req (seq (filter #(has-subtype? % "Deva") (:hand runner))))
                                 :label "Swap with a deva program from your Grip" :cost [:credit 2]
                                 :prompt (str "Choose a deva program in your Grip to swap with " name)
                                 :choices {:req #(and in-hand? (has-subtype? % "Deva"))}
                                 :msg (msg "swap in " (:title target) " from their Grip")
                                 :effect (req (if-let [hostcard (:host card)]
                                                (let [hosted (host state side (get-card state hostcard) target)]
                                                  (card-init state side hosted false))
                                                (let [devavec (get-in @state [:runner :rig :program])
                                                      devaindex (first (keep-indexed #(when (= (:cid %2) (:cid card)) %1) devavec))
                                                      newdeva (assoc target :zone (:zone card) :installed true)
                                                      newvec (apply conj (subvec devavec 0 devaindex) newdeva (subvec devavec devaindex))]
                                                  (lose state :runner :memory (:memoryunits card))
                                                  (swap! state assoc-in [:runner :rig :program] newvec)
                                                  (swap! state update-in [:runner :hand] (fn [coll] (remove-once #(not= (:cid %) (:cid target)) coll)))
                                                  (card-init state side newdeva false)))
                                              (move state side card :hand))}]}))

;;; Icebreaker definitions
(def cards-icebreakers
  {"Aghora"
   (deva "Aghora")

   "Alpha"
   (auto-icebreaker ["All"]
                    {:abilities [{:cost [:credit 1]
                                  :req (req (= (:position run) (count run-ices)))
                                  :msg "break 1 subroutine on the outermost ICE protecting this server"}
                                 (strength-pump 1 1)]})

   "Alias"
   (auto-icebreaker ["Sentry"]
                    {:abilities [{:cost [:credit 1]
                                  :req (req (#{:hq :rd :archives} (first (:server run))))
                                  :msg "break 1 Sentry subroutine"}
                                 (strength-pump 2 3)]})

   "Ankusa"
   (auto-icebreaker ["Barrier"]
                    {:abilities [(break-sub 2 1 "Barrier")
                                 (strength-pump 1 1)
                                 {:label "Add Barrier to HQ"
                                  :req (req (and (has-subtype? current-ice "Barrier")
                                                 (rezzed? current-ice)))
                                  :msg (msg "add " (:title current-ice) " to HQ after breaking all its subroutines")
                                  :effect (req (let [c current-ice]
                                                 (move state :corp c :hand nil)
                                                 (continue state side nil)))}]})

   "Atman"
   {:prompt "How many power counters?"
    :choices :credit
    :msg (msg "add " target " power counters")
    :effect (effect (add-counter card :power target))
    :abilities [(break-sub 1 1)]
    :strength-bonus (req (get-in card [:counter :power] 0))
    :events {:counter-added {:req (req (= :cid target) (:cid card))
                             :effect (effect (update-breaker-strength card))}}}

   "Aurora"
   (auto-icebreaker ["Barrier"]
                    {:abilities [(break-sub 2 1 "Barrier")
                                 (strength-pump 2 3)]})

   "Battering Ram"
   (auto-icebreaker ["Barrier"]
                    {:abilities [(break-sub 2 2 "Barrier")
                                 (strength-pump 1 1 :all-run)]})

   "BlacKat"
   {:abilities [(break-sub 1 1 "Barrier")
                {:cost [:credit 1]
                 :msg "break up to 3 Barrier subroutines (using a stealth [Credits])"}
                (strength-pump 2 1)
                {:cost [:credit 2]
                 :msg "add 2 strength (using at least 1 stealth [Credits])"
                 :effect (effect (pump card 2)) :pump 2}]}

   "Black Orchestra"
   (let [install {:req (req (and (= (:zone card) [:discard])
                                 (rezzed? current-ice)
                                 (has-subtype? current-ice "Code Gate")))
                  :optional {:player :runner
                             :prompt "Install Black Orchestra?"
                             :yes-ability {:effect (effect (unregister-events card)
                                                           (runner-install :runner card))}}}
         heap-event (req (when (= (:zone card) [:discard])
                           (unregister-events state side card)
                           (register-events state side
                                            (:events (card-def card))
                                            (assoc card :zone [:discard]))))]
   {:move-zone heap-event
    :abilities [{:cost [:credit 3]
                 :effect (effect (pump card 2)) :pump 2
                 :msg "add 2 strength and break up to 2 subroutines"}]
    :events {:rez install
             :approach-ice install
             :run install}})

   "Blackstone"
   {:abilities [(break-sub 1 1 "Barrier")
                {:cost [:credit 3]
                 :msg "add 4 strength (using at least 1 stealth [Credits])"
                 :effect (effect (pump card 4 :all-run)) :pump 4}]}

   "Brahman"
   (auto-icebreaker ["All"]
                    {:abilities [(break-sub 1 2 "ICE")
                                 (strength-pump 2 1)]})

   "Breach"
   (auto-icebreaker ["Barrier"]
                    {:abilities [{:cost [:credit 2]
                                  :req (req (#{:hq :rd :archives} (first (:server run))))
                                  :msg "break 3 Barrier subroutines"}
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
    :effect (effect (update! (assoc card :subtype-target target)))
    :events {:runner-turn-ends {:msg "add itself to Grip" :effect (effect (move card :hand))}}
    :abilities [{:cost [:credit 1] :msg (msg "break 1 " (:subtype-target card) " subroutine")}]}

   "Corroder"
   (auto-icebreaker ["Barrier"]
                    {:abilities [(break-sub 1 1 "Barrier")
                                 (strength-pump 1 1)]})

   "Creeper"
   (cloud-icebreaker
     (auto-icebreaker ["Sentry"]
                      {:abilities [(break-sub 2 1 "Sentry")
                                   (strength-pump 1 1)]}))

   "Crowbar"
   (break-and-enter "Code Gate")

   "Crypsis"
   (auto-icebreaker ["All"]
                    {:abilities [(break-sub 1 1 "ICE" (effect (update! (assoc card :crypsis-broke true))))
                                 (strength-pump 1 1)
                                 {:cost [:click 1]
                                  :msg "place 1 virus counter"
                                  :effect (effect (add-counter card :virus 1))}]
                     :events (let [encounter-ends-effect {:req (req (:crypsis-broke card))
                                                          :effect (req ((:effect breaker-auto-pump) state side eid card targets)
                                                                       (if (pos? (get-in card [:counter :virus] 0))
                                                                         (add-counter state side card :virus -1)
                                                                         (trash state side card {:cause :self-trash}))
                                                                       (update! state side (dissoc (get-card state card) :crypsis-broke)))}]
                               {:pass-ice encounter-ends-effect
                                :run-ends encounter-ends-effect})})

   "Cyber-Cypher"
   (auto-icebreaker ["Code Gate"]
                    {:prompt "Choose a server where this copy of Cyber-Cypher can be used:"
                     :msg (msg "target " target)
                     :choices (req servers)
                     :effect (effect (update! (assoc card :server-target target)))
                     :leave-play (effect (update! (dissoc card :server-target)))
                     :abilities [(break-sub 1 1 "Code Gate")
                                 (strength-pump 1 1)]})

   "Dagger"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 1 5)]})

   "Dai V"
   (auto-icebreaker ["All"]
                    {:abilities [{:cost [:credit 2]
                                  :msg "break all ICE subroutines (using stealth [Credits])"}
                                 (strength-pump 1 1)]})

   "Darwin"
   {:flags {:runner-phase-12 (req true)}
    :events {:purge {:effect (effect (update-breaker-strength card))}}
    :abilities [(break-sub 2 1 "ICE")
                {:label "Place 1 virus counter (start of turn)"
                 :cost [:credit 1]
                 :msg "place 1 virus counter"
                 :req (req (:runner-phase-12 @state))
                 :effect (effect (add-counter card :virus 1)
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
                    {:abilities [{:msg "break a Sentry subroutine"
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
                     :abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 2 1)]})

   "Force of Nature"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 2 2 "Code Gate")
                                 (strength-pump 1 1)]})

   "Garrote"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 1 1)]})

   "Golden"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 2 2 "Sentry")
                                 (strength-pump 2 4)
                                 {:label "Derez a Sentry and return Golden to your Grip"
                                  :cost [:credit 2]
                                  :req (req (and (rezzed? current-ice) (has-subtype? current-ice "Sentry")))
                                  :msg (msg "derez " (:title current-ice) " and return Golden to their Grip")
                                  :effect (effect (derez current-ice)
                                                  (move card :hand))}]})

   "Gordian Blade"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 1 1 "Code Gate")
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

   "Houdini"
   {:abilities [(break-sub 1 1 "Code Gate")
                {:cost [:credit 2]
                 :msg "add 4 strength (using at least 1 stealth [Credits])"
                 :effect (effect (pump card 4 :all-run)) :pump 4}]}

   "Inti"
   (auto-icebreaker ["Barrier"]
                    {:abilities [(break-sub 1 1 "Barrier")
                                 (strength-pump 2 1 :all-run)]})

   "Knight"
   {:abilities [{:cost [:click 1] :label "Host Knight on a piece of ICE"
                 :effect (req (let [k (get-card state card)
                                    hosted (ice? (:host k))
                                    icepos (ice-index state (get-card state (:host k)))]
                                (resolve-ability state side
                                 {:prompt (msg "Host Knight on a piece of ICE" (when hosted " not before or after the current host ICE"))
                                  :choices {:req #(if hosted
                                                    (and (or (when (= (:zone %) (:zone (:host k)))
                                                               (not= 1 (abs (- (ice-index state %) icepos))))
                                                             (not= (:zone %) (:zone (:host k))))
                                                         (ice? %)
                                                         (installed? %)
                                                         (not (some (fn [c] (has? c :subtype "Caïssa")) (:hosted %))))
                                                    (and (ice? %)
                                                         (installed? %)
                                                         (not (some (fn [c] (has? c :subtype "Caïssa")) (:hosted %)))))}
                                  :msg (msg "host it on " (card-str state target))
                                  :effect (effect (host target card))} card nil)))}
                {:cost [:credit 2] :msg "break 1 subroutine on the host ICE"}]}

   "Leviathan"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 3 3 "Code Gate")
                                 (strength-pump 3 5)]})

   "Morning Star"
   {:abilities [(break-sub 1 0 "Barrier")]}

   "Mimic"
   {:abilities [(break-sub 1 1 "Sentry")]}

   "Mongoose"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 1 2 "Sentry")
                                 (strength-pump 2 2)]})

   "Nfr"
   {:abilities [{:label "Place 1 power counter on Nfr"
                 :msg "place 1 power counter on it"
                 :effect (effect (add-counter card :power 1)
                                 (update-breaker-strength card))}
                (break-sub 1 1 "Barrier")]
    :strength-bonus (req (get-in card [:counter :power] 0))}

   "Ninja"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 3 5)]})

   "Paperclip"
   (let [install {:req (req (and (= (:zone card) [:discard])
                                 (rezzed? current-ice)
                                 (has-subtype? current-ice "Barrier")))
                  :optional {:player :runner
                             :prompt "Install Paperclip?"
                             :yes-ability {:effect (effect (unregister-events card)
                                                           (runner-install :runner card))}}}
         heap-event (req (when (= (:zone card) [:discard])
                           (unregister-events state side card)
                           (register-events state side
                                            (:events (card-def card))
                                            (assoc card :zone [:discard]))))]
   {:move-zone heap-event
    :abilities [{:label (str "X [Credits]: +X strength, break X subroutines")
                 :choices :credit
                 :prompt "How many credits?"
                 :effect (effect (pump card target))
                 :msg (msg "increase strength by " target " and break " target " Barrier subroutine"
                           (when (not= target 1) "s"))}]
    :events {:rez install
             :approach-ice install
             :run install}})


   "Passport"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [{:cost [:credit 1]
                                  :req (req (#{:hq :rd :archives} (first (:server run))))
                                  :msg "break 1 Code Gate subroutine"}
                                 (strength-pump 2 2)]})

   "Omega"
   (auto-icebreaker ["All"]
                    {:abilities [{:cost [:credit 1] :req (req (= 1 (:position run)))
                                  :msg "break 1 subroutine on the innermost ICE protecting this server"}
                                 (strength-pump 1 1)]})

   "Overmind"
   (auto-icebreaker ["All"]
                    {:effect (effect (add-counter card :power (:memory runner)))
                     :abilities [{:counter-cost [:power 1]
                                  :msg "break 1 subroutine"}
                                 (strength-pump 1 1)]})

   "Peacock"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 2 1 "Code Gate")
                                 (strength-pump 2 3)]})

   "Peregrine"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 1 1 "Code Gate")
                                 (strength-pump 3 3)
                                 {:label "Derez a Code Gate and return Peregrine to your Grip"
                                  :cost [:credit 2]
                                  :req (req (and (rezzed? current-ice) (has-subtype? current-ice "Code Gate")))
                                  :msg (msg "derez " (:title current-ice) " and return Peregrine to their Grip")
                                  :effect (effect (derez current-ice)
                                                  (move card :hand))}]})

   "Pipeline"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 2 1 :all-run)]})

   "Refractor"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 1 1 "Code Gate")
                                 (strength-pump 1 3)]})
   "Sadyojata"
   (deva "Sadyojata")

   "Sage"
   {:abilities [{:cost [:credit 2] :req (req (or (has-subtype? current-ice "Barrier")
                                                 (has-subtype? current-ice "Code Gate")))
                 :msg "break 1 Code Gate or Barrier subroutine"}]
    :effect (req (add-watch state (keyword (str "sage" (:cid card)))
                            (fn [k ref old new]
                              (when (not= (get-in old [:runner :memory]) (get-in new [:runner :memory]))
                                (update-breaker-strength ref side card))))
                 (update-breaker-strength state side card))
    :leave-play (req (remove-watch state (keyword (str "sage" (:cid card)))))
    :strength-bonus (req (:memory runner))}

   "Saker"
   (auto-icebreaker ["Barrier"]
                    {:abilities [(break-sub 1 1 "barrier")
                                 (strength-pump 2 2)
                                 {:label "Derez a barrier and return Saker to your Grip"
                                  :cost [:credit 2]
                                  :req (req (and (rezzed? current-ice) (has-subtype? current-ice "Barrier")))
                                  :msg (msg "derez " (:title current-ice) " and return Saker to their Grip")
                                  :effect (effect (derez current-ice)
                                                  (move card :hand))}]})

   "Snowball"
   (auto-icebreaker ["Barrier"]
                    {:abilities [{:cost [:credit 1] :msg "break 1 Barrier subroutine"
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
   {:abilities [(break-sub 1 1 "Code Gate")
                {:cost [:credit 2] :msg "place 1 power counter"
                 :effect (effect (add-counter card :power 1)
                                 (update-breaker-strength card))}]
    :strength-bonus (req (get-in card [:counter :power] 0))}

   "Switchblade"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 1 0 "Sentry")
                                 (strength-pump 1 7)]})

   "Torch"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 1 1 "Code Gate")
                                 (strength-pump 1 1)]})

   "Vamadeva"
   (deva "Vamadeva")

   "Wyrm"
   (auto-icebreaker ["All"]
                    {:abilities [{:cost [:credit 3]
                                  :msg "break 1 subroutine on ICE with 0 or less strength"}
                                 {:cost [:credit 1]
                                  :label "Give -1 strength to current ice"
                                  :req (req current-ice)
                                  :msg (msg "give -1 strength to " (:title current-ice))
                                  :effect (effect (update! (update-in card [:wyrm-count] (fnil inc 0)))
                                                  (update-ice-strength current-ice))}
                                 (strength-pump 1 1)]
                     :events (let [auto-pump (fn [state side eid card targets]
                                               ((:effect breaker-auto-pump) state side eid card targets))
                                   wy {:effect (effect (update! (dissoc card :wyrm-count))
                                                       (auto-pump eid card targets))}]
                               {:pre-ice-strength {:req (req (and (= (:cid target) (:cid current-ice))
                                                                  (:wyrm-count card)))
                                                   :effect (effect (ice-strength-bonus (- (:wyrm-count (get-card state card))) target)
                                                                   (auto-pump eid card targets))}
                                :pass-ice wy
                                :run-ends wy})})

   "Yog.0"
   {:abilities [(break-sub 0 1 "Code Gate")]}

   "ZU.13 Key Master"
   (cloud-icebreaker
     (auto-icebreaker ["Code Gate"]
                      {:abilities [(break-sub 1 1 "Code Gate")
                                   (strength-pump 1 1)]}))})
