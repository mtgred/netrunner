(in-ns 'game.core)

(def cards-upgrades
  {"Akitaro Watanabe"
   {:events {:pre-rez-cost {:req (req (and (= (:type target) "ICE")
                                           (= (card->server state card) (card->server state target))))
                            :effect (effect (rez-cost-bonus -2))}}}

   "Amazon Industrial Zone"
   {:events
     {:corp-install  {:optional {:req (req (and (= (:type target) "ICE")
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

   "Bernice Mai"
   {:events {:successful-run {:req (req this-server)
                              :trace {:base 5 :msg "give the Runner 1 tag"
                                      :effect (effect (tag-runner :runner 1))}}}}

   "Breaker Bay Grid"
   {:events {:pre-rez-cost {:req (req (= (:zone card) (:zone target)))
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
                                  {:choices {:req #(and (has? % :type "ICE") (:rezzed %))}
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
                                                     :effect (effect (ice-strength-bonus (:troubleshooter-amount card)))}
                                  :runner-turn-ends ct :corp-turn-ends ct}) card))}}

   "Crisium Grid"
   {:suppress {:successful-run {:req (req (and this-server (not= (:cid target) (:cid card))))}}
    :events {:successful-run {:req (req this-server)
                              :effect (req (swap! state update-in [:run :run-effect] dissoc :replace-access)
                                           (swap! state update-in [:run] dissoc :successful)
                                           (swap! state update-in [:runner :register :successful-run] #(butlast %)))}}}

   "Cyberdex Virus Suite"
   {:access {:optional {:prompt "Purge viruses with Cyberdex Virus Suite?"
                        :yes-ability {:msg (msg "purge viruses")
                                      :effect (effect (purge))}}}
    :abilities [{:label "[Trash]: Purge virus counters"
                 :msg "purge viruses" :effect (effect (purge) (trash card))}]}

   "Dedicated Technician Team"
   {:recurring 2}

   "Experiential Data"
   {:effect (req (update-ice-in-server state side (card->server state card)))
    :events {:pre-ice-strength {:req (req (= (card->server state card) (card->server state target)))
                                :effect (effect (ice-strength-bonus 1))}}
    :derez-effect {:effect (req (update-ice-in-server state side (card->server state card)))}
    :trash-effect {:effect (req (update-all-ice state side))}}

   "Expo Grid"
   {:events {:corp-turn-begins {:req (req (not (empty? (filter #(and (= (:type %) "Asset") (:rezzed %))
                                                               (get-in corp (:zone card))))))
                                :msg "gain 1 [Credits]" :effect (effect (gain :credit 1))}}}

   "Heinlein Grid"
   {:effect (req (add-watch state (keyword (str "heinlein" (:cid card)))
                   (fn [k ref old new]
                     (let [clicknew (get-in new [:runner :click])
                           clickold (get-in old [:runner :click])]
                       (when (< clicknew clickold)
                         (resolve-ability ref side
                           {:req (req this-server)
                            :msg (msg "force the Runner to lose all " (:credit runner) " [Credits]") :once :per-run
                            :effect (effect (lose :runner :credit :all))}
                          card nil))))))
    :leave-play (req (remove-watch state (keyword (str "heinlein" (:cid card)))))}

   "Hokusai Grid"
   {:events {:successful-run {:req (req this-server) :msg "do 1 net damage"
                              :effect (req (damage state side :net 1 {:card card}))}}}

   "Keegan Lane"
   {:abilities [{:label "[Trash], remove a tag: Trash a program"
                 :req (req (and this-server
                                (> (get-in @state [:runner :tag]) 0)
                                (not (empty? (filter #(has? % :type "Program") (all-installed state :runner))))))
                 :msg (msg "remove 1 tag")
                 :effect (req (resolve-ability state side trash-program card nil)
                              (trash state side card {:cause :ability-cost})
                              (lose state :runner :tag 1))}]}
                            
   "Marcus Batty"
   {:abilities [{:label "[Trash]: Start a Psi game" :msg "start a Psi game"
                 :psi {:not-equal {:req (req this-server)
                                   :choices {:req #(and (has? % :type "ICE") (:rezzed %))}
                                   :msg (msg "resolve a subroutine on " (:title target))
                                   :effect (effect (trash card {:cause :ability-cost}))}}}]}

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

   "Panic Button"
   {:init {:root "HQ"} :abilities [{:cost [:credit 1] :effect (effect (draw))
                                    :req (req (and run (= (first (:server run)) :hq)))}]}

   "Product Placement"
   {:access {:req (req (not= (first (:zone card)) :discard))
             :msg "gain 2 [Credits]" :effect (effect (gain :corp :credit 2))}}

   "Red Herrings"
   {:events {:pre-steal-cost {:req (req (= (:zone card) (:zone target)))
                              :effect (effect (steal-cost-bonus [:credit 5]))}}}

   "Research Station"
   {:init {:root "HQ"}
    :effect (effect (gain :max-hand-size 2)) :leave-play (effect (lose :max-hand-size 2))}

   "Rutherford Grid"
   {:events {:pre-init-trace {:req (req this-server)
                              :effect (effect (init-trace-bonus 2))}}}
    
   "Ryon Knight"
   {:abilities [{:label "[Trash]: Do 1 brain damage"
                 :msg "do 1 brain damage" :req (req (and this-server (zero? (:click runner))))
                 :effect (effect (trash card) (damage :brain 1 {:card card}))}]}

   "SanSan City Grid"
   {:effect (req (when-let [agenda (some #(when (= (:type %) "Agenda") %) (:content (card->server state card)))]
                   (update-advancement-cost state side agenda)))
    :events {:corp-install {:req (req (and (= (:type target) "Agenda") (= (:zone card) (:zone target))))
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
   {:events {:pre-steal-cost {:req (req (= (:zone card) (:zone target)))
                              :effect (effect (steal-cost-bonus [:click 1]))}}}

   "The Twins"
   {:abilities [{:label "Reveal and trash a copy of the ICE just passed from HQ"
                 :req (req (and this-server
                                (> (count (:ices run)) (:position run))
                                (:rezzed (get-in (:ices (card->server state card)) [(:position run)]))))
                 :effect (req (let [icename (:title (get-in (:ices (card->server state card)) [(:position run)]))]
                                (resolve-ability
                                  state side
                                  {:prompt "Choose a copy of the ICE just passed"
                                   :choices {:req #(and (= (:zone %) [:hand])
                                                        (= (:type %) "ICE")
                                                        (= (:title %) icename))}
                                   :effect (req (trash state side (assoc target :seen true))
                                                (swap! state update-in [:run]
                                                       #(assoc % :position (inc (:position run)))))
                                   :msg (msg "trash a copy of " (:title target) " from HQ and force the Runner to encounter it again")}
                                 card nil)))}]}

   "Tyrs Hand"
   {:abilities [{:label "[Trash]: Prevent a subroutine on a Bioroid from being broken"
                 :req (req (and (= (butlast (:zone current-ice)) (butlast (:zone card)))
                                (has? current-ice :subtype "Bioroid"))) :effect (effect (trash card))
                 :msg (msg "prevent a subroutine on " (:title current-ice) " from being broken")}]}

   "Will-o-the-Wisp"
   {:abilities [{:label "[Trash]: Add an icebreaker to the bottom of Stack"
                 :choices {:req #(has? % :subtype "Icebreaker")}
                 :msg (msg "add " (:title target) " to the bottom of Stack")
                 :effect (effect (trash card) (move :runner target :deck))}]}})
