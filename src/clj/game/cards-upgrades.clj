(in-ns 'game.core)

(def cards-upgrades
  {"Akitaro Watanabe"
   {:events {:pre-rez-cost {:req (req (and (ice? target)
                                           (= (card->server state card) (card->server state target))))
                            :effect (effect (rez-cost-bonus -2))}}}

   "Amazon Industrial Zone"
   {:events
     {:corp-install  {:optional {:req (req (and (ice? target)
                                                (= (card->server state card) (card->server state target))))
                                 :prompt "Rez ICE with rez cost lowered by 3?"
                                 :yes-ability {:effect (effect (rez-cost-bonus -3) (rez target))}}}}}

   "Ash 2X3ZB9CY"
   {:abilities [{:label "Trace 4 - Prevent the Runner from accessing cards other than Ash 2X3ZB9CY"
                 :trace {:base 4
                         :effect (req (max-access state side 0)
                                      (let [ash card]
                                        (swap! state update-in [:run :run-effect]
                                               #(assoc % :successful-run
                                                         {:effect (effect (handle-access [ash])) :card ash}))))
                         :msg "prevent the Runner from accessing cards other than Ash 2X3ZB9CY"}}]}

   "Awakening Center"
   {:abilities [{:label "Host a piece of bioroid ICE"
                 :cost [:click 1]
                 :prompt "Choose a piece of bioroid ICE to host on Awakening Center"
                 :choices {:req #(and (ice? %)
                                      (has-subtype? % "Bioroid")
                                      (in-hand? %))}
                 :msg "host a piece of bioroid ICE"
                 :effect (effect (trigger-event :corp-install target)
                                 (host card target {:facedown true}))}
                {:req (req (and this-server (= (get-in @state [:run :position]) 0)))
                 :label "Rez a hosted piece of bioroid ICE"
                 :prompt "Choose a piece of bioroid ICE to rez" :choices (req (:hosted card))
                 :msg (msg "lower the rez cost of " (:title target) " by 7 [Credits] and force the Runner to encounter it")
                 :effect (effect (rez-cost-bonus -7) (rez target)
                                 (update! (dissoc (get-card state target) :facedown))
                                 (register-events {:run-ends
                                                    {:effect (req (doseq [c (:hosted card)]
                                                                    (when (:rezzed c)
                                                                      (trash state side c)))
                                                                  (unregister-events state side card))}} card))}]
    :events {:run-ends nil}}

   "Bernice Mai"
   {:events {:successful-run {:req (req this-server)
                              :trace {:base 5 :msg "give the Runner 1 tag"
                                      :effect (effect (tag-runner :runner 1))}}}}

   "Breaker Bay Grid"
   {:events {:pre-rez-cost {:req (req (or (= (:zone card) (:zone target))
                                          (= (:zone card) (:zone (get-card state (:host target))))))
                            :effect (effect (rez-cost-bonus -5))}}}

   "Caprice Nisei"
   {:abilities [{:msg "start a Psi game"
                 :psi {:not-equal {:msg "end the run" :effect (effect (end-run))}}}]}

   "ChiLo City Grid"
   {:events {:successful-trace {:req (req this-server)
                                :effect (effect (tag-runner :runner 1))
                                :msg "give the Runner 1 tag"}}}

   "Corporate Troubleshooter"
   {:abilities [{:label "[Trash]: Add strength to a rezzed ICE protecting this server" :choices :credit
                 :prompt "How many credits?"
                 :effect (req (let [boost target]
                                (resolve-ability
                                  state side
                                  {:choices {:req #(and (ice? %)
                                                        (rezzed? %))}
                                   :msg (msg "add " boost " strength to " (:title target))
                                   :effect (req (update! state side (assoc card :troubleshooter-target target
                                                                                :troubleshooter-amount boost))
                                                (trash state side (get-card state card))
                                                (update-ice-strength state side target))} card nil)))}]
    :events {:pre-ice-strength nil :runner-turn-ends nil :corp-turn-ends nil}
    :trash-effect
               {:effect (req (register-events
                               state side
                               (let [ct {:effect (req (unregister-events state side card)
                                                      (update! state side (dissoc card :troubleshooter-target))
                                                      (update-ice-strength state side (:troubleshooter-target card)))}]
                                 {:pre-ice-strength
                                                    {:req (req (= (:cid target) (:cid (:troubleshooter-target card))))
                                                     :effect (effect (ice-strength-bonus (:troubleshooter-amount card) target))}
                                  :runner-turn-ends ct :corp-turn-ends ct}) card))}}

   "Crisium Grid"
   {:suppress {:successful-run {:req (req (and this-server (not= (:cid target) (:cid card))))}}
    :events {:successful-run {:req (req this-server)
                              :effect (req (swap! state update-in [:run :run-effect] dissoc :replace-access)
                                           (swap! state update-in [:run] dissoc :successful)
                                           (swap! state update-in [:runner :register :successful-run] #(rest %)))}}}

   "Cyberdex Virus Suite"
   {:access {:optional {:prompt "Purge viruses with Cyberdex Virus Suite?"
                        :yes-ability {:msg (msg "purge viruses")
                                      :effect (effect (purge))}}}
    :abilities [{:label "[Trash]: Purge virus counters"
                 :msg "purge viruses" :effect (effect (trash card) (purge))}]}

   "Dedicated Technician Team"
   {:recurring 2}

   "Experiential Data"
   {:effect (req (update-ice-in-server state side (card->server state card)))
    :events {:pre-ice-strength {:req (req (= (card->server state card) (card->server state target)))
                                :effect (effect (ice-strength-bonus 1 target))}}
    :derez-effect {:effect (req (update-ice-in-server state side (card->server state card)))}
    :trash-effect {:effect (req (update-all-ice state side))}}

   "Expo Grid"
   (let [ability {:req (req (some #(and (is-type? % "Asset")
                                        (rezzed? %))
                                  (get-in corp (:zone card))))
                  :msg "gain 1 [Credits]"
                  :once :per-turn
                  :label "Gain 1 [Credits] (start of turn)"
                  :effect (effect (gain :credit 1))}]
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :events {:corp-turn-begins ability}
    :abilities [ability]})

   "Heinlein Grid"
   {:abilities [{:req (req this-server)
                 :label "Force the Runner to lose all [Credits] from spending or losing a [Click]"
                 :msg (msg "force the Runner to lose all " (:credit runner) " [Credits]") :once :per-run
                 :effect (effect (lose :runner :credit :all))}]}

   "Hokusai Grid"
   {:events {:successful-run {:req (req this-server) :msg "do 1 net damage"
                              :effect (req (damage state side :net 1 {:card card}))}}}

   "Keegan Lane"
   {:abilities [{:label "[Trash], remove a tag: Trash a program"
                 :req (req (and this-server
                                (pos? (get-in @state [:runner :tag]))
                                (not (empty? (filter #(is-type? % "Program")
                                                     (all-installed state :runner))))))
                 :msg (msg "remove 1 tag")
                 :effect (req (resolve-ability state side trash-program card nil)
                              (trash state side card {:cause :ability-cost})
                              (lose state :runner :tag 1))}]}

   "Marcus Batty"
   {:abilities [{:req (req this-server)
                 :label "[Trash]: Start a Psi game"
                 :msg "start a Psi game"
                 :psi {:not-equal {:prompt "Choose a rezzed piece of ICE to resolve one of its subroutines"
                                   :choices {:req #(and (ice? %)
                                                        (rezzed? %))}
                                   :msg (msg "resolve a subroutine on " (:title target))}}
                 :effect (effect (trash card))}]}

   "Midori"
   {:abilities
    [{:req (req this-server)
      :label "Swap the ICE being approached with a piece of ICE from HQ"
      :prompt "Choose a piece of ICE"
      :choices {:req #(and (ice? %)
                           (in-hand? %))}
      :once :per-run
      :msg (msg "swap " (card-str state current-ice) " with a piece of ICE from HQ")
      :effect (req (let [hqice target
                         c current-ice]
                     (resolve-ability state side
                       {:effect (req (let [newice (assoc hqice :zone (:zone c))
                                           cndx (ice-index state c)
                                           ices (get-in @state (cons :corp (:zone c)))
                                           newices (apply conj (subvec ices 0 cndx) newice (subvec ices cndx))]
                                       (swap! state assoc-in (cons :corp (:zone c)) newices)
                                       (swap! state update-in [:corp :hand]
                                              (fn [coll] (remove-once #(not= (:cid %) (:cid hqice)) coll)))
                                       (trigger-event state side :corp-install newice)
                                       (move state side c :hand)
                                       (update-run-ice state side)))} card nil)))}]}

   "Mumbad City Grid"
   {:abilities [{:req (req (and this-server
                                (< (:position run) (count (:ices run)))
                                (> (count (:ices run)) 1)))
                 :label "Swap the ICE just passed with another piece of ICE protecting this server"
                 :effect (req (let [passed-ice (nth (get-in @state (vec (concat [:corp :servers] (:server run) [:ices])))
                                                                                (:position run))
                                    ice-zone (:zone passed-ice)]
                                 (resolve-ability state :corp
                                   {:prompt (msg "Select a piece of ICE to swap with " (:title passed-ice))
                                    :choices {:req #(and (= ice-zone (:zone %)) (ice? %))}
                                    :effect (req (let [fndx (ice-index state passed-ice)
                                                       sndx (ice-index state target)
                                                       fnew (assoc passed-ice :zone (:zone target))
                                                       snew (assoc target :zone (:zone passed-ice))]
                                                   (swap! state update-in (cons :corp ice-zone)
                                                          #(assoc % fndx snew))
                                                   (swap! state update-in (cons :corp ice-zone)
                                                          #(assoc % sndx fnew))
                                                   (update-ice-strength state side fnew)
                                                   (update-ice-strength state side snew)))} card nil)
                                 (system-msg state side (str "uses Mumbad City Grid to swap " (card-str state passed-ice)
                                                             " with " (card-str state target)))))}]}

   "NeoTokyo Grid"
   {:events {:advance {:req (req (= (butlast (:zone target)) (butlast (:zone card)))) :once :per-turn
                       :msg "gain 1 [Credits]" :effect (effect (gain :credit 1))}}}

   "Oaktown Grid"
   {:events {:pre-trash {:req (req (= (:zone card) (:zone target)))
                         :effect (effect (trash-cost-bonus 3))}}}

   "Off the Grid"
   {:events {:successful-run {:req (req (= target :hq))
                              :effect (req (trash state :corp card)
                                           (system-msg state :corp (str "trashes Off the Grid")))}}}

   "Old Hollywood Grid"
   (let [ab {:req (req (or (= (:zone card) (:zone target)) (= (central->zone (:zone target)) (butlast (:zone card)))))
             :effect (req (if-not (some #(= (:title %) (:title target)) (:scored runner))
                            (prevent-steal state side)
                            (swap! state update-in [:runner :register] dissoc :cannot-steal)))}
         un {:effect (req (swap! state update-in [:runner :register] dissoc :cannot-steal))}]
     {:trash-effect
      {:req (req (and (= :servers (first (:previous-zone card))) (:run @state)))
       :effect (effect (register-events {:pre-steal-cost (assoc ab :req (req (or (= (:zone target) (:previous-zone card))
                                                                                 (= (central->zone (:zone target))
                                                                                    (butlast (:previous-zone card))))))
                                         :run-ends {:effect (req (unregister-events state side card)
                                                                 (swap! state update-in [:runner :register] dissoc
                                                                        :cannot-steal))}}
                                        (assoc card :zone '(:discard))))}
      :events {:pre-steal-cost ab :run-ends un}})

   "Panic Button"
   {:init {:root "HQ"} :abilities [{:cost [:credit 1] :label "Draw 1 card" :effect (effect (draw))
                                    :req (req (and run (= (first (:server run)) :hq)))}]}

   "Port Anson Grid"
   {:msg "prevent the Runner from jacking out unless they trash an installed program"
    :effect (req (when this-server
                   (prevent-jack-out state side)))
    :events {:run {:req (req this-server)
                   :msg "prevent the Runner from jacking out unless they trash an installed program"
                   :effect (effect (prevent-jack-out))}
             :runner-trash {:req (req (and this-server (is-type? target "Program")))
                            :effect (req (swap! state update-in [:run] dissoc :cannot-jack-out))}}}

   "Product Placement"
   {:access {:req (req (not= (first (:zone card)) :discard))
             :msg "gain 2 [Credits]" :effect (effect (gain :corp :credit 2))}}

   "Red Herrings"
   (let [ab {:req (req (or (= (:zone card) (:zone target)) (= (central->zone (:zone target)) (butlast (:zone card)))))
             :effect (effect (steal-cost-bonus [:credit 5]))}]
     {:trash-effect
      {:req (req (and (= :servers (first (:previous-zone card))) (:run @state)))
       :effect (effect (register-events {:pre-steal-cost (assoc ab :req (req (or (= (:zone target) (:previous-zone card))
                                                                                 (= (central->zone (:zone target))
                                                                                    (butlast (:previous-zone card))))))
                                         :run-ends {:effect (effect (unregister-events card))}}
                                        (assoc card :zone '(:discard))))}
      :events {:pre-steal-cost ab :run-ends nil}})

   "Research Station"
   {:init {:root "HQ"}
    :in-play [:hand-size-modification 2]}

   "Rutherford Grid"
   {:events {:pre-init-trace {:req (req this-server)
                              :effect (effect (init-trace-bonus 2))}}}

   "Ryon Knight"
   {:abilities [{:label "[Trash]: Do 1 brain damage"
                 :msg "do 1 brain damage" :req (req (and this-server (zero? (:click runner))))
                 :effect (effect (trash card) (damage :brain 1 {:card card}))}]}

   "SanSan City Grid"
   {:effect (req (when-let [agenda (some #(when (is-type? % "Agenda") %)
                                         (:content (card->server state card)))]
                   (update-advancement-cost state side agenda)))
    :events {:corp-install {:req (req (and (is-type? target "Agenda")
                                           (= (:zone card) (:zone target))))
                            :effect (effect (update-advancement-cost target))}
             :pre-advancement-cost {:req (req (= (:zone card) (:zone target)))
                                    :effect (effect (advancement-cost-bonus -1))}}}

   "Self-destruct"
   {:abilities [{:req (req this-server)
                 :label "[Trash]: Trace X - Do 3 net damage"
                 :effect (req (let [serv (card->server state card)
                                    cards (concat (:ices serv) (:content serv))]
                                (trash state side card)
                                (doseq [c cards] (trash state side c))
                                (resolve-ability
                                  state side
                                  {:trace {:base (req (dec (count cards)))
                                           :effect (effect (damage :net 3 {:card card}))
                                           :msg "do 3 net damage"}} card nil)))}]}

   "Shell Corporation"
   {:abilities
    [{:cost [:click 1] :msg "store 3 [Credits]" :once :per-turn
      :effect (effect (add-prop card :counter 3))}
     {:cost [:click 1] :msg (msg "gain " (:counter card) " [Credits]") :once :per-turn
      :label "Take all credits"
      :effect (effect (gain :credit (:counter card)) (set-prop card :counter 0))}]}

   "Simone Diego"
   {:recurring 2}

   "Strongbox"
   (let [ab {:req (req (or (= (:zone card) (:zone target)) (= (central->zone (:zone target)) (butlast (:zone card)))))
             :effect (effect (steal-cost-bonus [:click 1]))}]
     {:trash-effect
      {:req (req (and (= :servers (first (:previous-zone card))) (:run @state)))
       :effect (effect (register-events {:pre-steal-cost (assoc ab :req (req (or (= (:zone target) (:previous-zone card))
                                                                                 (= (central->zone (:zone target))
                                                                                    (butlast (:previous-zone card))))))
                                         :run-ends {:effect (effect (unregister-events card))}}
                                        (assoc card :zone '(:discard))))}
      :events {:pre-steal-cost ab :run-ends nil}})

   "The Twins"
   {:abilities [{:label "Reveal and trash a copy of the ICE just passed from HQ"
                 :req (req (and this-server
                                (> (count (:ices run)) (:position run))
                                (:rezzed (get-in (:ices (card->server state card)) [(:position run)]))))
                 :effect (req (let [icename (:title (get-in (:ices (card->server state card)) [(:position run)]))]
                                (resolve-ability
                                  state side
                                  {:prompt "Choose a copy of the ICE just passed"
                                   :choices {:req #(and (in-hand? %)
                                                        (ice? %)
                                                        (= (:title %) icename))}
                                   :effect (req (trash state side (assoc target :seen true))
                                                (swap! state update-in [:run]
                                                       #(assoc % :position (inc (:position run)))))
                                   :msg (msg "trash a copy of " (:title target) " from HQ and force the Runner to encounter it again")}
                                 card nil)))}]}

   "Tyrs Hand"
   {:abilities [{:label "[Trash]: Prevent a subroutine on a Bioroid from being broken"
                 :req (req (and (= (butlast (:zone current-ice)) (butlast (:zone card)))
                                (has-subtype? current-ice "Bioroid")))
                 :effect (effect (trash card))
                 :msg (msg "prevent a subroutine on " (:title current-ice) " from being broken")}]}

   "Valley Grid"
   {:abilities [{:req (req this-server)
                 :label "Reduce Runner's maximum hand size by 1 until start of next Corp turn"
                 :msg "reduce the Runner's maximum hand size by 1 until the start of the next Corp turn"
                 :effect (req (update! state side (assoc card :times-used (inc (get card :times-used 0))))
                              (lose state :runner :hand-size-modification 1))}]
    :trash-effect {:req (req (and (= :servers (first (:previous-zone card))) (:run @state)))
                   :effect (req (when-let [n (:times-used card)]
                                  (register-events state side
                                                   {:corp-turn-begins
                                                    {:msg (msg "increase the Runner's maximum hand size by " n)
                                                     :effect (effect (gain :runner :hand-size-modification n)
                                                                     (unregister-events card)
                                                                     (update! (dissoc card :times-used)))}}
                                                   (assoc card :zone '(:discard)))))}
    :events {:corp-turn-begins {:req (req (:times-used card))
                                :msg (msg "increase the Runner's maximum hand size by "
                                          (:times-used card))
                                :effect (effect (gain :runner :hand-size-modification
                                                      (:times-used card))
                                                (update! (dissoc card :times-used)))}}}

   "Will-o-the-Wisp"
   {:abilities [{:label "[Trash]: Add an icebreaker to the bottom of Stack"
                 :choices {:req #(has-subtype? % "Icebreaker")}
                 :msg (msg "add " (:title target) " to the bottom of Stack")
                 :effect (effect (trash card) (move :runner target :deck))}]}})
