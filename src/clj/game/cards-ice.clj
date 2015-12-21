(in-ns 'game.core)

(declare trash-program trash-hardware trash-installed)

(def end-the-run {:msg "end the run" :effect (effect (end-run))})

(def cards-ice
  {"Archangel"
   {:access {:optional
             {:req (req (not= (first (:zone card)) :discard))
              :prompt "Pay 3 [Credits] to force Runner to encounter Archangel?"
              :yes-ability {:cost [:credit 3]
                            :effect (req (system-msg state :corp "pays 3 [Credits] to force the Runner to encounter Archangel"))}}}
    :abilities [{:label "Trace 6 - Add 1 installed card to the Runner's Grip"
                 :trace {:base 6 :choices {:req #(:installed %)}
                         :msg "add 1 installed card to the Runner's Grip"
                         :effect (effect (move :runner target :hand true) (system-msg (str "adds " (:title target) " to the Runner's Grip")))}}]}

   "Archer"
   {:additional-cost [:forfeit]
    :abilities [{:msg "gain 2 [Credits]" :effect (effect (gain :credit 2))}
                trash-program end-the-run]}

   "Architect"
   {:flags {
            :untrashable-while-rezzed true
            }
    :abilities [{:msg "look at the top 5 cards of R&D"
                 :prompt "Choose a card to install" :priority true
                 :activatemsg "uses Architect to look at the top 5 cards of R&D"
                 :req (req (and (not (string? target))
                                (not= (:type target) "Operation"))) :not-distinct true
                 :choices (req (conj (take 5 (:deck corp)) "No install"))
                 :effect (effect (corp-install (move state side target :play-area) nil {:no-install-cost true}))}
                {:label "Install a card from HQ or Archives"
                 :prompt "Choose a card to install from Archives or HQ"
                 :show-discard true :priority true
                 :choices {:req #(and (not= (:type %) "Operation")
                                      (#{[:hand] [:discard]} (:zone %))
                                      (= (:side %) "Corp"))}
                 :effect (effect (corp-install target nil))
                 :msg (msg (corp-install-msg target))}]}

   "Ashigaru"
   {:abilities [end-the-run]}

   "Assassin"
   {:abilities [{:label "Trace 5 - Do 3 net damage"
                 :trace {:base 5 :msg "do 3 net damage" :effect (effect (damage :net 3 {:card card}))}}
                {:label "Trace 4 - Trash a program"
                 :trace (assoc trash-program :base 4 :msg "trash a program")}]}

   "Asteroid Belt"
   {:advanceable :always :abilities [end-the-run]
    :rez-cost-bonus (req (* -3 (or (:advance-counter card) 0)))}

   "Bandwidth"
   {:abilities [{:msg "give the Runner 1 tag"
                 :effect (effect (tag-runner :runner 1)
                                 (register-events
                                   {:successful-run {:effect (effect (lose :runner :tag 1))
                                                     :msg "make the Runner lose 1 tag"}
                                    :run-ends {:effect (effect (unregister-events card))}}
                                  card))}]
    :events {:successful-run nil :run-ends nil}}

   "Bastion"
   {:abilities [end-the-run]}

   "Builder"
   {:abilities [{:label "Move Builder to the outermost position of any server"
                 :cost [:click 1] :prompt "Choose a server" :choices (req servers)
                 :msg (msg "move it to the outermost position of " target)
                 :effect (effect (move card (conj (server->zone state target) :ices)))}
                {:label "Place 1 advancement token on an ICE that can be advanced protecting this server"
                 :msg (msg "place 1 advancement token on " (if (:rezzed target) (:title target) "a card"))
                 :choices {:req #(and (= (last (:zone %)) :ices)
                                      (or (= (:advanceable %) "always")
                                          (and (= (:advanceable %) "while-rezzed") (:rezzed %))))}
                 :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]}

   "Bullfrog"
   {:abilities [{:msg "start a Psi game"
                 :psi {:not-equal
                       {:player :corp :prompt "Choose a server" :choices (req servers)
                        :msg (msg "move it to the outermost position of " target)
                        :effect (req (let [dest (server->zone state target)]
                                       (swap! state update-in [:run]
                                              #(assoc % :position (count (get-in corp (conj dest :ices)))
                                                        :server (rest dest))))
                                     (move state side card (conj (server->zone state target) :ices))
                                     (update-run-ice state side))}}}]}

   "Burke Bugs"
   {:abilities [{:label "Trace 0 - Force the Runner to trash a program"
                 :trace (assoc trash-program :base 0 :not-distinct true
                                             :player :runner
                                             :msg "force the Runner to trash a program")}]}

   "Caduceus"
   {:abilities [{:label "Trace 3 - Gain 3 [Credits]"
                 :trace {:base 3 :msg "gain 3 [Credits]" :effect (effect (gain :credit 3))}}
                {:label "Trace 2 - End the run"
                 :trace {:base 2 :msg "end the run" :effect (effect (end-run))}}]}

   "Cell Portal"
   {:abilities [{:msg "make the Runner approach the outermost ICE"
                 :effect (req (swap! state assoc-in [:run :position] 0) (derez state side card))}]}

   "Changeling"
   (let [ab {:req (req (= (:cid card) (:cid target)))
             :effect (req (if (odd? (:advance-counter (get-card state card)))
                            (morph state side card "Sentry" "Barrier")
                            (morph state side card "Barrier" "Sentry")))}]
     {:advanceable :always
      :effect (req (if (odd? (get card :advance-counter 0))
                     (morph state side card "Sentry" "Barrier")
                     (morph state side card "Barrier" "Sentry")))
      :abilities [end-the-run]
      :events {:advance ab :advancement-placed ab}})

   "Checkpoint"
   {:effect (effect (gain :bad-publicity 1) (system-msg "takes 1 bad publicity"))
    :abilities [{:label "Trace 5 - Do 3 meat damage when this run is successful"
                 :trace {:base 5 :msg "do 3 meat damage when this run is successful"
                         :effect (req (swap! state assoc-in [:run :run-effect :end-run]
                                             {:req (req (:successful run)) :msg "do 3 meat damage"
                                              :effect (effect (damage :meat 3 {:card card}))})
                                      (swap! state assoc-in [:run :run-effect :card] card))}}]}

   "Chimera"
   {:prompt "Choose one subtype" :choices ["Barrier" "Code Gate" "Sentry"]
    :msg (msg "change its subtype to " target) :end-turn {:effect (effect (derez card))}
    :abilities [end-the-run]}

   "Clairvoyant Monitor"
   {:abilities [{:msg "start a Psi game"
                 :psi {:not-equal {:player :corp
                                   :prompt "Choose a target for Clairvoyant Monitor"
                                   :msg (msg "place 1 advancement token on "
                                             (if (:rezzed target) (:title target) "a card") " and end the run")
                                   :choices {:req #(= (first (:zone %)) :servers)}
                                   :effect (effect (add-prop target :advance-counter 1 {:placed true}) (end-run))}}}]}

   "Chum"
   {:abilities [{:msg "do 3 net damage" :effect (effect (damage :net 3 {:card card}))}]}

   "Cortex Lock"
   {:abilities [{:label "Do 1 net damage for each unused memory units the Runner has"
                 :msg (msg "do " (:memory runner) " net damage")
                 :effect (effect (damage :net (:memory runner) {:card card}))}]}

   "Crick"
   {:abilities [{:label "install a card from Archives"
                 :msg (msg (corp-install-msg target))
                 :prompt "Choose a card to install from Archives"
                 :show-discard true :priority true
                 :choices {:req #(and (not= (:type %) "Operation")
                                      (= (:zone %) [:discard])
                                      (= (:side %) "Corp"))}
                 :effect (effect (corp-install target nil))}]
    :strength-bonus (req (if (= (second (:zone card)) :archives) 3 0))}

   "Curtain Wall"
   {:abilities [end-the-run]
    :strength-bonus (req (let [ices (:ices (card->server state card))]
                           (if (= (:cid card) (:cid (last ices))) 4 0)))
    :events (let [cw {:req (req (and (not= (:cid card) (:cid target))
                                     (= (card->server state card) (card->server state target))))
                      :effect (effect (update-ice-strength card))}]
              {:corp-install cw :trash cw :card-moved cw})}

   "Data Hound"
   {:abilities [{:label "Trace 2 - Look at the top of Stack"
                 :trace {:base 2 :msg "look at top X cards of Stack"
                         :effect (req (doseq [c (take (- target (second targets)) (:deck runner))]
                                        (move state side c :play-area))
                                      (system-msg state :corp (str "looks at the top " (- target (second targets)) " cards of Stack"))
                                      )}}]}

   "Data Mine"
   {:abilities [{:msg "do 1 net damage" :effect (effect (trash card) (damage :net 1 {:card card}))}]}

   "Datapike"
   {:abilities [{:msg "force the Runner to pay 2 [Credits] if able"
                 :effect (effect (pay :runner card :credit 2))}
                end-the-run]}

   "Data Raven"
   {:abilities [{:msg "give the Runner 1 tag" :effect (effect (tag-runner 1))}
                {:msg "give the Runner 1 tag using 1 power counter"
                 :counter-cost 1 :effect (effect (tag-runner 1))}
                {:label "Trace 3 - Add 1 power counter"
                 :trace {:base 3 :msg "add 1 power counter" :effect (effect (add-prop card :counter 1))}}]}

   "Dracō"
   {:prompt "How many power counters?" :choices :credit :msg (msg "add " target " power counters")
    :effect (effect (set-prop card :counter target))
    :strength-bonus (req (or (:counter card) 0))
    :abilities [{:label "Trace 2 - Give the Runner 1 tag and end the run"
                 :trace {:base 2 :msg "give the Runner 1 tag and end the run"
                         :effect (effect (tag-runner :runner 1) (end-run))}}]}

   "Eli 1.0"
   {:abilities [end-the-run]}

   "Enforcer 1.0"
   {:additional-cost [:forfeit]
    :abilities [trash-program
                {:msg "do 1 brain damage" :effect (effect (damage :brain 1 {:card card}))}
                {:label "Trash a console" :effect (effect (trash target))
                 :prompt "Choose a console to trash" :msg (msg "trash " (:title target))
                 :choices {:req #(has? % :subtype "Console")}}
                {:msg "trash all virtual resources"
                 :effect (req (doseq [c (filter #(has? % :subtype "Virtual") (all-installed state :runner))]
                                (trash state side c)))}]}

   "Enigma"
   {:abilities [{:msg "force the Runner to lose 1 [Click] if able"
                 :effect (effect (lose :runner :click 1))}
                end-the-run]}

   "Errand Boy"
   {:abilities [{:msg "gain 1 [Credits]" :effect (effect (gain :credit 1))}
                {:msg "draw 1 card" :effect (effect (draw))}]}

   "Excalibur"
   {:abilities [{:label "The Runner cannot make another run this turn"
                 :msg "prevent the Runner from making another run" :effect (effect (prevent-run))}]}

   "Fenris"
   {:effect (effect (gain :bad-publicity 1) (system-msg "takes 1 bad publicity"))
    :abilities [{:msg "do 1 brain damage" :effect (effect (damage :brain 1 {:card card}))} end-the-run]}

   "Fire Wall"
   {:advanceable :always :abilities [end-the-run]
    :strength-bonus (req (or (:advance-counter card) 0))}

   "Flare"
   {:abilities [{:label "Trace 6 - Trash 1 hardware, do 2 meat damage, and end the run"
                 :trace {:base 6 :msg "trash 1 hardware, do 2 meat damage, and end the run"
                         :effect (effect (resolve-ability
                                           {:prompt "Choose a piece of hardware to trash"
                                            :label "Trash a piece of hardware"
                                            :msg (msg "trash " (:title target))
                                            :choices {:req #(= (:type %) "Hardware")}
                                            :effect (effect (trash target {:cause :subroutine}))} card nil)
                                         (damage :meat 2 {:unpreventable true :card card})
                                         (end-run))}}]}

   "Galahad"
   {:abilities [{:label "End the run" :msg "end the run" :effect (effect (end-run))}
                {:label "Reveal up to 2 Grail ICE from HQ"
                 :choices {:max 2 :req #(and (:side % "Corp") (= (:zone %) [:hand]) (has? % :subtype "Grail"))}
                 :msg (msg "reveal "
                           (join ", " (map #(str (:title %) " ("
                                                 (:label (first (:abilities (card-def %)))) ")") targets)))}
                {:label "Resolve a Grail ICE subroutine from HQ"
                 :choices {:req #(and (:side % "Corp") (= (:zone %) [:hand]) (has? % :subtype "Grail"))}
                 :effect (req (doseq [ice targets]
                                (resolve-ability state side (first (:abilities (card-def ice))) card nil)))}]}

   "Gemini"
   {:abilities [{:label "Trace 2 - Do 1 net damage"
                 :trace {:base 2 :msg "do 1 net damage" :effect (effect (damage :net 1 {:card card}))
                         :kicker {:min 5 :msg "do 1 net damage" :effect (effect (damage :net 1 {:card card}))}}}]}

   "Grim"
   {:effect (effect (gain :bad-publicity 1) (system-msg "takes 1 bad publicity"))
    :abilities [trash-program]}

   "Guard"
   {:abilities [end-the-run]}

   "Gutenberg"
   {:abilities [{:label "Trace 7 - Give the Runner 1 tag"
                 :trace {:base 7 :msg "give the Runner 1 tag" :effect (effect (tag-runner :runner 1))}}]
    :strength-bonus (req (if (= (second (:zone card)) :rd) 3 0))}

   "Gyri Labyrinth"
   {:abilities [{:req (req (:run @state))
                 :label "Reduce Runner's maximum hand size by 2 until start of next Corp turn"
                 :msg "reduce the Runner's maximum hand size by 2 until the start of the next Corp turn"
                 :effect (effect (lose :runner :max-hand-size 2)
                                 (register-events {:corp-turn-begins
                                                   {:msg "increase the Runner's maximum hand size by 2"
                                                    :effect (effect (gain :runner :max-hand-size 2)
                                                                    (unregister-events card))}} card))}]
    :events {:corp-turn-begins nil}}

   "Hadrians Wall"
   {:advanceable :always
    :abilities [end-the-run]
    :strength-bonus (req (or (:advance-counter card) 0))}

   "Himitsu-Bako"
   {:abilities [end-the-run {:msg "add it to HQ" :cost [:credit 1] :effect (effect (move card :hand))}]}

   "Hive"
   {:abilities [end-the-run]}

   "Heimdall 1.0"
   {:abilities [{:msg "do 1 brain damage" :effect (effect (damage :brain 1 {:card card}))} end-the-run]}

   "Heimdall 2.0"
   {:abilities [{:msg "do 1 brain damage" :effect (effect (damage :brain 1 {:card card}))}
                {:msg "do 1 brain damage and end the run" :effect (effect (damage :brain 1 {:card card}) (end-run))}
                end-the-run]}

   "Hourglass"
   {:abilities [{:msg "force the Runner to lose 1 [Click] if able"
                 :effect (effect (lose :runner :click 1))}]}

   "Howler"
   (let [ice-index (fn [state i] (first (keep-indexed #(when (= (:cid %2) (:cid i)) %1)
                                                      (get-in @state (cons :corp (:zone i))))))]
   {:abilities
    [{:label "Install a piece of Bioroid ICE from HQ or Archives"
      :prompt "Install ICE from HQ or Archives?"
      :choices ["HQ" "Archives"]
      :effect (req (let [fr target]
                     (resolve-ability state side
                       {:prompt "Choose a Bioroid ICE to install"
                        :choices (req (filter #(and (= (:type %) "ICE") (has? % :subtype "Bioroid"))
                                              ((if (= fr "HQ") :hand :discard) corp)))
                        :effect (req (let [newice (assoc target :zone (:zone card) :rezzed true)
                                           hndx (ice-index state card)
                                           ices (get-in @state (cons :corp (:zone card)))
                                           newices (apply conj (subvec ices 0 hndx) newice (subvec ices hndx))]
                                       (swap! state assoc-in (cons :corp (:zone card)) newices)
                                       (swap! state update-in (cons :corp (:zone target))
                                              (fn [coll] (remove-once #(not= (:cid %) (:cid target)) coll)))
                                       (update! state side (assoc card :howler-target newice))
                                       (trigger-event state side :corp-install newice)))} card nil)))}]
    :events {:run-ends {:req (req (:howler-target card))
                        :effect (effect (trash card {:cause :self-trash})
                                        (derez (get-card state (:howler-target card))))}}})

   "Hudson 1.0"
   {:abilities [{:msg "prevent the Runner from accessing more than 1 card during this run"
                 :effect (effect (max-access 1))}]}

   "Hunter"
   {:abilities [{:label "Trace 3 - Give the Runner 1 tag"
                 :trace {:base 3 :msg "give the Runner 1 tag" :effect (effect (tag-runner :runner 1))}}]}

   "Ice Wall"
   {:advanceable :always :abilities [end-the-run]
    :strength-bonus (req (or (:advance-counter card) 0))}

   "Ichi 1.0"
   {:abilities [trash-program
                {:label "Trace 1 - Give the Runner 1 tag and do 1 brain damage"
                 :trace {:base 1 :msg "give the Runner 1 tag and do 1 brain damage"
                         :effect (effect (damage :brain 1 {:card card}) (tag-runner :runner 1))}}]}

   "Ichi 2.0"
   {:abilities [trash-program
                {:label "Trace 3 - Give the Runner 1 tag and do 1 brain damage"
                 :trace {:base 3 :msg "give the Runner 1 tag and do 1 brain damage"
                         :effect (effect (damage :brain 1 {:card card}) (tag-runner :runner 1))}}]}

   "IQ"
   {:effect (req (add-watch state (keyword (str "iq" (:cid card)))
                   (fn [k ref old new]
                     (let [handsize (count (get-in new [:corp :hand]))]
                       (when (not= (count (get-in old [:corp :hand])) handsize)
                         (update! ref side (assoc (get-card ref card) :strength-bonus handsize))
                         (update-ice-strength ref side (get-card ref card)))))))
    :abilities [end-the-run]
    :strength-bonus (req (count (:hand corp)))
    :rez-cost-bonus (req (count (:hand corp)))
    :leave-play (req (remove-watch state (keyword (str "iq" (:cid card)))))}

   "Information Overload"
   {:abilities [{:label "Trace 1 - Give the Runner 1 tag"
                 :trace {:base 1 :msg "give the Runner 1 tag" :effect (effect (tag-runner :runner 1))}}
                trash-installed]}

   "Ireress"
   {:abilities [{:msg "make the Runner lose 1 [Credits]" :effect (effect (lose :runner :credit 1))}]}

   "Its a Trap!"
   {:expose {:msg "do 2 net damage" :effect (effect (damage :net 2 {:card card}))}
    :abilities [(assoc trash-installed :effect (effect (trash card) (trash target {:cause :subroutine})))]}

   "Janus 1.0"
   {:abilities [{:msg "do 1 brain damage" :effect (effect (damage :brain 1 {:card card}))}]}

   "Kitsune"
   {:abilities [{:prompt "Choose a card in HQ to force access"
                 :choices {:req #(= (:zone %) [:hand])}
                 :label "Force the Runner to access a card in HQ"
                 :msg (msg "force the Runner to access " (:title target))
                 :effect (effect (handle-access targets) (trash card))}]}

   "Komainu"
   {:abilities [{:msg "do 1 net damage" :effect (effect (damage :net 1 {:card card}))}]}

   "Lab Dog"
   {:abilities [(assoc trash-hardware :label "Force the Runner to trash an installed piece of hardware"
                                      :player :runner
                                      :msg (msg "force the runner to trash " (:title target))
                                      :effect (effect (trash target) (trash card)))]}

   "Lancelot"
   {:abilities [trash-program
                {:label "Reveal up to 2 Grail ICE from HQ"
                 :choices {:max 2 :req #(and (:side % "Corp") (= (:zone %) [:hand]) (has? % :subtype "Grail"))}
                 :msg (msg "reveal "
                           (join ", " (map #(str (:title %) " ("
                                                 (:label (first (:abilities (card-def %)))) ")") targets)))}
                {:label "Resolve a Grail ICE subroutine from HQ"
                 :choices {:req #(and (:side % "Corp") (= (:zone %) [:hand]) (has? % :subtype "Grail"))}
                 :effect (req (doseq [ice targets]
                                (resolve-ability state side (first (:abilities (card-def ice))) card nil)))}]}

   "Little Engine"
   {:abilities [end-the-run
                {:msg "make the Runner gain 5 [Credits]" :effect (effect (gain :runner :credit 5))}]}

   "Lockdown"
   {:abilities [{:label "The Runner cannot draw cards for the remainder of this turn"
                 :msg "prevent the Runner from drawing cards" :effect (effect (prevent-draw))}]}

   "Lotus Field"
   {:abilities [end-the-run]
    :flags {:cannot-lower-strength true}}

   "Lycan"
   (let [ab {:req (req (= (:cid card) (:cid target)))
             :effect (req (if (odd? (:advance-counter (get-card state card)))
                            (morph state side card "Code Gate" "Sentry")
                            (morph state side card "Sentry" "Code Gate")))}]
     {:advanceable :always
      :effect (req (if (odd? (get card :advance-counter 0))
                     (morph state side card "Code Gate" "Sentry")
                     (morph state side card "Sentry" "Code Gate")))
      :abilities [trash-program]
      :events {:advance ab :advancement-placed ab}})

   "Mamba"
   {:abilities [{:msg "do 1 net damage" :effect (effect (damage :net 1 {:card card}))}
                {:msg "do 1 net damage using 1 power counter"
                 :counter-cost 1 :effect (effect (damage :net 1 {:card card}))}
                {:msg "start a Psi game"
                 :psi {:not-equal {:msg "add 1 power counter"
                                   :effect (effect (add-prop :runner card :counter 1))}}}]}

   "Markus 1.0"
   {:abilities [trash-installed end-the-run]}

   "Matrix Analyzer"
   {:abilities [{:label "Place 1 advancement token on a card that can be advanced"
                 :msg (msg "place 1 advancement token on " (if (:rezzed target) (:title target) "a card"))
                 :choices {:req #(or (= (:type %) "Agenda") (:advanceable %))}
                 :cost [:credit 1] :effect (effect (add-prop target :advance-counter 1))}
                {:label "Trace 2 - Give the Runner 1 tag"
                 :trace {:base 2 :msg "give the Runner 1 tag" :effect (effect (tag-runner :runner 1))}}]}

   "Merlin"
   {:abilities [{:label "Do 2 net damage" :msg "do 2 net damage" :effect (effect (damage :net 2 {:card card}))}
                {:label "Reveal up to 2 Grail ICE from HQ"
                 :choices {:max 2 :req #(and (:side % "Corp") (= (:zone %) [:hand]) (has? % :subtype "Grail"))}
                 :msg (msg "reveal "
                           (join ", " (map #(str (:title %) " ("
                                                 (:label (first (:abilities (card-def %)))) ")") targets)))}
                {:label "Resolve a Grail ICE subroutine from HQ"
                 :choices {:req #(and (:side % "Corp") (= (:zone %) [:hand]) (has? % :subtype "Grail"))}
                 :effect (req (doseq [ice targets]
                                (resolve-ability state side (first (:abilities (card-def ice))) card nil)))}]}

   "Meru Mati"
   {:abilities [end-the-run]
    :strength-bonus (req (if (= (second (:zone card)) :hq) 3 0))}

   "Minelayer"
   {:abilities [{:msg "install an ICE from HQ"
                 :choices {:req #(and (= (:type %) "ICE") (= (:zone %) [:hand]))}
                 :prompt "Choose an ICE to install from HQ"
                 :effect (req (corp-install state side target (:server run) {:no-install-cost true}))}]}

   "Mother Goddess"
   (let [ab {:req (req (= (:type target) "ICE"))
             :effect (effect (update! (assoc card :subtype
                                                  (->> (mapcat :ices (flatten (seq (:servers corp))))
                                                       (filter #(and (:rezzed %) (not= (:cid card) (:cid %))))
                                                       (mapcat #(vec (.split (:subtype %) " - ")))
                                                       (cons "Mythic")
                                                       distinct
                                                       (join " - ")))))}]
     {:abilities [end-the-run]
      :events {:rez ab :trash ab :derez ab}})

   "Muckraker"
   {:effect (effect (gain :bad-publicity 1))
    :abilities [{:label "Trace 1 - Give the Runner 1 tag"
                 :trace {:base 1 :msg "give the Runner 1 tag" :effect (effect (tag-runner :runner 1))}}
                {:label "Trace 2 - Give the Runner 1 tag"
                 :trace {:base 2 :msg "give the Runner 1 tag" :effect (effect (tag-runner :runner 1))}}
                {:label "Trace 3 - Give the Runner 1 tag"
                 :trace {:base 3 :msg "give the Runner 1 tag" :effect (effect (tag-runner :runner 1))}}
                {:msg "end the run if the Runner is tagged" :req (req tagged)
                 :effect (effect (end-run))}]}

   "Nebula"
   {:advanceable :always
    :abilities [trash-program]
    :rez-cost-bonus (req (* -3 (or (:advance-counter card) 0)))}

   "Negotiator"
   {:abilities [{:msg "gain 2 [Credits]" :effect (effect (gain :credit 2))} trash-program]}

   "Neural Katana"
   {:abilities [{:msg "do 3 net damage" :effect (effect (damage :net 3 {:card card}))}]}

   "News Hound"
   {:abilities [{:label "Trace 3 - Give the Runner 1 tag"
                 :trace {:base 3 :msg "give the Runner 1 tag" :effect (effect (tag-runner :runner 1))}}
                {:label "End the run if a Current is active"
                 :req (req (or (not (empty? (runner :current)))
                               (not (empty? (corp :current)))))
                 :effect (effect (end-run)) :msg "end the run"}]}

   "NEXT Bronze"
   {:abilities [end-the-run]
    :strength-bonus (req (reduce (fn [c server]
                                   (+ c (count (filter (fn [ice] (and (:rezzed ice) (has? ice :subtype "NEXT")))
                                                       (:ices server)))))
                                 0 (flatten (seq (:servers corp)))))
    :events (let [nb {:req (req (and (not= (:cid target) (:cid card)) (has? target :subtype "NEXT")))
                      :effect (effect (update-ice-strength card))}]
              {:rez nb :derez nb :trash nb :card-moved nb})}

   "NEXT Gold"
   {:abilities [{:label "Do 1 net damage for each rezzed NEXT ice"
                 :msg (msg "do "
                           (reduce (fn [c server]
                                     (+ c (count (filter (fn [ice]
                                                           (and (:rezzed ice) (has? ice :subtype "NEXT")))
                                                         (:ices server)))))
                                   0 (flatten (seq (:servers corp)))) " net damage")
                 :effect (effect (damage :net (reduce (fn [c server]
                                                        (+ c (count (filter (fn [ice]
                                                                              (and (:rezzed ice) (has? ice :subtype "NEXT")))
                                                                            (:ices server)))))
                                                      0 (flatten (seq (:servers corp)))) {:card card}))}
                trash-program]}

   "NEXT Silver"
   {:abilities [end-the-run]}

   "Orion"
   {:advanceable :always :abilities [trash-program end-the-run]
    :rez-cost-bonus (req (* -3 (or (:advance-counter card) 0)))}

   "Pachinko"
   {:abilities [{:label "End the run if the Runner is tagged"
                 :req (req tagged) :msg "end the run" :effect (effect (end-run))}]}

   "Paper Wall"
   {:abilities [end-the-run]}

   "Pop-up Window"
   {:abilities [{:msg "gain 1 [Credits]" :effect (effect (gain :credit 1))} end-the-run]}

   "Pup"
   {:abilities [{:msg "do 1 net damage" :effect (effect (damage :net 1 {:card card}))}]}

   "Quandary"
   {:abilities [end-the-run]}

   "Quicksand"
   {:abilities [{:req (req (and this-server (= (dec (:position run)) (ice-index state card))))
                 :label "Add 1 power counter"
                 :effect (effect (add-prop card :counter 1)
                                 (update-all-ice))}
                 end-the-run]
    :strength-bonus (req (or (:counter card) 0))}

   "Rainbow"
   {:abilities [end-the-run]}

   "Resistor"
   {:strength-bonus (req (get-in @state [:runner :tag]))
    :abilities [{:label "Trace 4 - End the run"
                 :trace {:base 4 :msg "end the run" :effect (effect (end-run))}}]}

   "Rototurret"
   {:abilities [trash-program end-the-run]}

   "Sagittarius"
   {:abilities [{:label "Trace 2 - Trash a program"
                 :trace (assoc trash-program :base 2 :not-distinct true :msg "trash 1 program"
                                             :kicker (assoc trash-program :min 5))}]}

   "Salvage"
   {:advanceable :while-rezzed
    :abilities [{:label "Trace 2 - Give the Runner 1 tag"
                 :trace {:base 2 :msg "give the Runner 1 tag" :effect (effect (tag-runner :runner 1))}}]}

   "Searchlight"
   {:advanceable :always
    :abilities [{:label "Trace X - Give the Runner 1 tag"
                 :trace {:base (req (or (:advance-counter card) 0)) :effect (effect (tag-runner :runner 1))
                         :msg "give the Runner 1 tag"}}]}

   "Sensei"
   {:abilities [{:label "Give each other ICE encountered \"End the run\" for the remainder of the run"
                 :msg (msg "give each other ICE encountered \"[Subroutine] End the run\" after all its other subroutines for the remainder of the run")}]}

   "Shadow"
   {:advanceable :always
    :abilities [{:msg "gain 2 [Credits]" :effect (effect (gain :credit 2))}
                {:label "Trace 3 - Give the Runner 1 tag"
                 :trace {:base 3 :msg "give the Runner 1 tag" :effect (effect (tag-runner :runner 1))}}]
    :strength-bonus (req (or (:advance-counter card) 0))}

   "Sherlock 1.0"
   {:abilities [{:label "Trace 4 - Add an installed program to the top of Stack"
                 :trace {:base 4 :choices {:req #(and (:installed %) (= (:type %) "Program"))}
                         :msg (msg "add " (:title target) " to the top of Stack")
                         :effect (effect (move :runner target :deck {:front true}))}}]}

   "Shinobi"
   {:effect (effect (gain :bad-publicity 1) (system-msg "takes 1 bad publicity"))
    :abilities [{:label "Trace 1 - Do 1 net damage"
                 :trace {:base 1 :msg "do 1 net damage" :effect (effect (damage :net 1 {:card card}))}}
                {:label "Trace 2 - Do 2 net damage"
                 :trace {:base 2 :msg "do 2 net damage" :effect (effect (damage :net 2 {:card card}))}}
                {:label "Trace 3 - Do 3 net damage and end the run"
                 :trace {:base 3 :msg "do 3 net damage and end the run"
                         :effect (effect (damage :net 3 {:card card}) (end-run))}}]}

   "Shiro"
   {:abilities [{:label "Rearrange the top 3 cards of R&D"
                 :msg "rearrange the top 3 cards of R&D"
                 :effect (req (doseq [c (take 3 (:deck corp))]
                                (move state side c :play-area)))}
                {:label "Force the Runner to access the top card of R&D"
                 :effect (req (doseq [c (take (get-in @state [:runner :rd-access]) (:deck corp))]
                                (system-msg state :runner (str "accesses " (:title c)))
                                (handle-access state side [c])))}]}

   "Snoop"
   {:abilities [{:req (req (= current-ice card)) :label "Reveal all cards in the Runner's Grip"
                 :msg (msg "reveal " (join ", " (map :title (:hand runner))))}
                {:label "Trace 3 - Place 1 power counter on Snoop"
                 :trace {:base 3 :msg "place 1 power counter on Snoop" :effect (effect (add-prop card :counter 1))}}
                {:counter-cost 1 :label "Hosted power counter: Reveal all cards in Grip and trash 1 card"
                 :msg (msg "look at all cards in Grip and trash " (:title target))
                 :choices (req (:hand runner)) :prompt "Choose a card to trash"
                 :effect (effect (trash target))}]}

   "Snowflake"
   {:abilities [{:msg "start a Psi game"
                 :psi {:not-equal end-the-run}}]}

   "Special Offer"
   {:abilities [{:label "Gain 5 [Credits] and trash Special Offer"
                 :effect (effect (gain :corp :credit 5) (trash card)
                                 (system-msg (str "gains 5 [Credits] and trashes Special Offer")))}]}

   "Spiderweb"
   {:abilities [end-the-run]}

   "Susanoo-No-Mikoto"
   {:abilities [{:req (req (not= (:server run) [:discard]))
                 :msg "make the Runner continue the run on Archives"
                 :effect (req (swap! state update-in [:run]
                                     #(assoc % :position (count (get-in corp [:servers :archives :ices]))
                                               :server [:archives]))
                              (update-run-ice state side))}]}

   "Swarm"
   {:effect (effect (gain :bad-publicity 1))
    :advanceable :always
    :abilities [trash-program]}

   "Swordsman"
   {:abilities [{:msg "do 1 net damage" :effect (effect (damage :net 1 {:card card}))}
                {:prompt "Choose an AI program to trash" :msg (msg "trashes " (:title target))
                 :label "Trash an AI program" :effect (effect (trash target))
                 :choices {:req #(and (:installed %) (= (:type %) "Program") (has? % :subtype "AI"))}}]}

   "Taurus"
   {:abilities [{:label "Trace 2 - Trash a piece of hardware"
                 :trace (assoc trash-hardware :base 2 :not-distinct true :msg "trash 1 hardware"
                                              :kicker (assoc trash-hardware :min 5))}]}

   "TMI"
   {:trace {:base 2 :msg "keep TMI rezzed" :unsuccessful {:effect (effect (derez card))}} :abilities [end-the-run]}

   "Tollbooth"
   {:abilities [{:msg "force the Runner to lose 3 [Credits]"
                 :effect (effect (lose :runner :credit 3))}
                end-the-run]}

   "Tour Guide"
   {:abilities [end-the-run]}

   "Troll"
   {:abilities [{:label "Trace 2 - Force the Runner to lose [Click] or end the run"
                 :trace {:base 2 :player :runner
                         :msg "force the Runner to lose [Click] or end the run"
                         :prompt "Choose one" :choices ["Lose [Click]" "End the run"]
                         :effect (req (if-not (and (= target "Lose [Click]") (pay state side card :click 1))
                                        (do (end-run state side) (system-msg state side "ends the run"))
                                        (system-msg state side "loses [Click]")))}}]}

   "Tsurugi"
   {:abilities [end-the-run {:msg "do 1 net damage" :effect (effect (damage :net 1 {:card card}))}]}

   "Turing"
   {:abilities [end-the-run]
    :strength-bonus (req (if (is-remote? (second (:zone card))) 3 0))}

   "Turnpike"
   {:abilities [{:msg "force the Runner to lose 1 [Credits]"
                 :effect (effect (lose :runner :credit 1))}
                {:label "Trace 5 - Give the Runner 1 tag"
                 :trace {:base 5 :msg "give the Runner 1 tag" :effect (effect (tag-runner :runner 1))}}]}

   "Tyrant"
   {:advanceable :while-rezzed :abilities [end-the-run]}

   "Universal Connectivity Fee"
   {:abilities [{:msg (msg "force the Runner to lose " (if (> (:tag runner) 0) "all credits" "1 [Credits]"))
                 :effect (req (if (> (get-in @state [:runner :tag]) 0)
                                (do (lose state :runner :credit :all) (trash state side card))
                                (lose state :runner :credit 1)))}]}

   "Uroboros"
   {:abilities [{:label "Trace 4 - Prevent the Runner from making another run"
                 :trace {:base 4 :msg "prevent the Runner from making another run"
                         :effect (effect (prevent-run))}}
                {:label "Trace 4 - End the run"
                 :trace {:base 4 :msg "end the run" :effect (effect (end-run))}}]}

   "Viktor 1.0"
   {:abilities [{:msg "do 1 brain damage" :effect (effect (damage :brain 1 {:card card}))} end-the-run]}

   "Viktor 2.0"
   {:abilities [{:msg "do 1 brain damage using 1 power counter" :counter-cost 1
                 :effect (effect (damage :brain 1 {:card card}))}
                {:label "Trace 2 - Add 1 power counter"
                 :trace {:base 2 :msg "add 1 power counter" :effect (effect (add-prop card :counter 1))}}
                end-the-run]}

   "Viper"
   {:abilities [{:label "Trace 3 - The Runner loses 1 [Click] if able"
                 :trace {:base 3 :msg  "force the Runner to lose 1 [Click] if able"
                         :effect (effect (lose :runner :click 1))}}
                {:label "Trace 3 - End the run"
                 :trace {:base 3 :msg "end the run" :effect (effect (end-run))}}]}

   "Virgo"
   {:abilities [{:label "Trace 2 - Give the Runner 1 tag"
                 :trace {:base 2 :msg "give the Runner 1 tag" :effect (effect (tag-runner :runner 1))
                         :kicker {:min 5 :msg "give the Runner 1 tag"
                                  :effect (effect (tag-runner :runner 1))}}}]}

   "Wall of Static"
   {:abilities [end-the-run]}

   "Wall of Thorns"
   {:abilities [end-the-run {:msg "do 2 net damage" :effect (effect (damage :net 2 {:card card}))}]}

   "Wendigo"
   (let [ab {:req (req (= (:cid card) (:cid target)))
             :effect (req (if (odd? (:advance-counter (get-card state card)))
                            (morph state side card "Barrier" "Code Gate")
                            (morph state side card "Code Gate" "Barrier")))}]
     {:advanceable :always
      :effect (req (if (odd? (get card :advance-counter 0))
                     (morph state side card "Barrier" "Code Gate")
                     (morph state side card "Code Gate" "Barrier")))
      :abilities [{:msg "prevent the Runner from using a chosen program for the remainder of this run"}]
      :events {:advance ab :advancement-placed ab}})

   "Whirlpool"
   {:abilities [{:msg "prevent the Runner from jacking out"
                 :effect (req (when (and (is-remote? (second (:zone card)))
                                         (> (count (concat (:ices (card->server state card))
                                                           (:content (card->server state card)))) 1))
                                (prevent-jack-out state side))
                              (trash state side card)
                              (when (:run @state)
                                (swap! state update-in [:run] #(assoc % :position (dec (:position run))))))}]}

   "Woodcutter"
   {:advanceable :while-rezzed
    :abilities [{:msg "do 1 net damage" :effect (effect (damage :net 1 {:card card}))}]}

   "Wormhole"
   {:advanceable :always
    :rez-cost-bonus (req (* -3 (or (:advance-counter card) 0)))}

   "Wotan"
   {:abilities [end-the-run]}

   "Wraparound"
   {:abilities [end-the-run]
    :strength-bonus (req (if (some #(has? % :subtype "Fracter") (all-installed state :runner))
                           0 7))
    :events (let [wr {:req (req (and (not= (:cid target) (:cid card)) (has? target :subtype "Fracter")))
                      :effect (effect (update-ice-strength card))}]
              {:runner-install wr :trash wr :card-moved wr})}

   "Yagura"
   {:abilities [{:msg "do 1 net damage" :effect (effect (damage :net 1 {:card card}))}
                {:msg "look at the top card of R&D"
                 :optional {:prompt (msg "Add " (:title (first (:deck corp))) " to bottom of R&D?")
                            :msg "add the top card of R&D to the bottom"
                            :yes-ability {:effect (effect (move (first (:deck corp)) :deck))}}}]}

   "Zed 1.0"
   {:abilities [{:msg "do 1 brain damage" :effect (effect (damage :brain 1 {:card card}))}]}})
