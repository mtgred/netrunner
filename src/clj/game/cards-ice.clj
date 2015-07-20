(in-ns 'game.core)

(declare trash-program trash-hardware)

(def cards-ice
  {"Archer"
   {:additional-cost [:forfeit]
    :abilities [{:msg "gain 2 [Credits]" :effect (effect (gain :credit 2))}
                trash-program
                {:msg "end the run" :effect (effect (end-run))}]}

   "Architect"
   {:abilities [{:msg "look at the top 5 cards of R&D"
                 :prompt "Choose a card to install"
                 :activatemsg "uses Architect to look at the top 5 cards of R&D"
                 :req (req (not (string? target))) :not-distinct true
                 :choices (req (conj (take 5 (:deck corp)) "No install"))
                 :effect (effect (corp-install target nil {:no-install-cost true}))}
                {:msg "install a card from Archives" :choices (req (:discard corp))
                 :prompt "Choose a card to install" :effect (effect (corp-install target nil))}
                {:msg "install a card from HQ" :choices (req (:hand corp))
                 :prompt "Choose a card to install" :effect (effect (corp-install target nil))}]}

   "Ashigaru"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Asteroid Belt"
   {:advanceable :always :abilities [{:msg "end the run" :effect (effect (end-run))}]
    :rez-cost-bonus (req (* -3 (or (:advance-counter card) 0)))}

   "Bandwidth"
   {:abilities [{:msg "give the Runner 1 tag" :effect (effect (gain :runner :tag 1))}]}

   "Bastion"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Builder"
   {:abilities [{:label "Move Builder to the outermost position of any server"
                 :cost [:click 1] :prompt "Choose a server" :choices (req servers)
                 :msg (msg "move it to the outermost position of " target)
                 :effect (effect (move card (conj (server->zone state target) :ices)))}
                {:label "Place 1 advancement token on an ICE that can be advanced on this server"
                 :msg (msg "place 1 advancement token on " (if (:rezzed target) (:title target) "a card"))
                 :choices {:req #(or (= (:type %) "Agenda") (:advanceable %))}
                 :effect (effect (add-prop target :advance-counter 1))}]}

   "Bullfrog"
   {:abilities [{:msg "start a Psi game"
                 :psi {:not-equal
                       {:player :corp :prompt "Choose a server" :choices (req servers)
                        :msg (msg "move it to the outermost position of " target)
                        :effect (req (let [dest (server->zone state target)]
                                       (swap! state update-in [:run]
                                              #(assoc % :position (count (get-in corp (conj dest :ices)))
                                                        :server (rest dest))))
                                     (move state side card (conj (server->zone state target) :ices)))}}}]}

   "Burke Bugs"
   {:abilities [trash-program]}

   "Caduceus"
   {:abilities [{:label "Trace 3 - Gain 3 [Credits]"
                 :trace {:base 3 :msg "gain 3 [Credits]" :effect (effect (gain :credit 3))}}
                {:label "Trace 2 - End the run"
                 :trace {:base 2 :msg "end the run" :effect (effect (end-run))}}]}

   "Cell Portal"
   {:abilities [{:msg "make the Runner approach the outermost ICE"
                 :effect #(do (swap! %1 assoc-in [:run :position] 0) (derez %1 %2 %3))}]}

   "Changeling"
   {:advanceable :always :abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Checkpoint"
   {:effect (effect (gain :bad-publicity 1) (system-msg "takes 1 bad publicity"))
    :abilities [{:label "Trace 5 - Do 3 meat damage when this run is successful"
                 :trace {:base 5
                         :effect #(do (swap! %1 assoc-in [:run :run-effect :end-run]
                                             {:req (req (:successful run)) :msg "do 3 meat damage"
                                              :effect (effect (damage :meat 3 {:card card}))})
                                      (swap! %1 assoc-in [:run :run-effect :card] %3))}}]}

   "Chimera"
   {:prompt "Choose one subtype" :choices ["Barrier" "Code Gate" "Sentry"]
    :msg (msg "change its subtype to " target) :end-turn {:effect (effect (derez card))}
    :abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Clairvoyant Monitor"
   {:abilities [{:msg "start a Psi game"
                 :psi {:not-equal {:player :corp
                                   :prompt "Choose a target for Clairvoyant Monitor"
                                   :msg (msg "place 1 advancement token on "
                                             (if (:rezzed target) (:title target) "a card") " and end the run")
                                   :choices {:req #(or (= (:type %) "Agenda") (:advanceable %))}
                                   :effect (effect (add-prop target :advance-counter 1) (end-run))}}}]}

   "Chum"
   {:abilities [{:msg "do 3 net damage" :effect (effect (damage :net 3 {:card card}))}]}

   "Cortex Lock"
   {:abilities [{:label "Do 1 net damage for each unused memory units the Runner has"
                 :msg (msg "do " (:memory runner) " net damage")
                 :effect (effect (damage :net (:memory runner) {:card card}))}]}

   "Crick"
   {:abilities [{:msg "install a card from Archives" :choices (req (:discard corp))
                 :prompt "Choose a card to install" :effect (effect (corp-install target nil))}]
    :strength-bonus (req (if (= (second (:zone card)) :archives) 3 0))}

   "Curtain Wall"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]
    :strength-bonus (req (let [ices (:ices (card->server state card))]
                           (if (= (:cid card) (:cid (last ices))) 4 0)))
    :events (let [cw {:req (req (and (not= (:cid card) (:cid target))
                                     (= (card->server state card) (card->server state target))))
                      :effect (effect (update-ice-strength card))}]
              {:corp-install cw :trash cw :card-moved cw})}

   "Data Hound"
   {:abilities [{:label "Trace 2 - Look at the top of Stack"
                 :trace {:base 2 :msg (msg "look at the top " (- target (second targets)) " cards of Stack")
                         :effect (req (doseq [c (take (- target (second targets)) (:deck runner))]
                                        (move state side c :play-area false true)))}}]}

   "Data Mine"
   {:abilities [{:msg "do 1 net damage" :effect (effect (trash card) (damage :net 1 {:card card}))}]}

   "Datapike"
   {:abilities [{:msg "force the Runner to pay 2 [Credits] if able"
                 :effect (effect (pay :runner card :credit 2))}
                {:msg "end the run" :effect (effect (end-run))}]}

   "Data Raven"
   {:abilities [{:msg "give the Runner 1 tag" :effect (effect (gain :runner :tag 1))}
                {:msg "give the Runner 1 tag using 1 power counter"
                 :counter-cost 1 :effect (effect (gain :runner :tag 1))}
                {:label "Trace 3 - Add 1 power counter"
                 :trace {:base 3 :msg "add 1 power counter" :effect (effect (add-prop card :counter 1))}}]}

   "Dracō"
   {:prompt "How many power counters?" :choices :credit :msg (msg "add " target " power counters")
    :effect (effect (set-prop card :counter target))
    :strength-bonus (req (or (:counter card) 0))
    :abilities [{:label "Trace 2"
                 :trace {:base 2 :msg "give the Runner 1 tag and end the run"
                         :effect (effect (gain :runner :tag 1) (end-run))}}]}

   "Eli 1.0"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Enigma"
   {:abilities [{:msg "force the Runner to lose 1 [Click] if able"
                 :effect (effect (lose :runner :click 1))}
                {:msg "end the run" :effect (effect (end-run))}]}

   "Errand Boy"
   {:abilities [{:msg "gain 1 [Credits]" :effect (effect (gain :credit 1))}
                {:msg "draw 1 card" :effect (effect (draw))}]}

   "Excalibur"
   {:abilities [{:label "The Runner cannot make another run this turn"
                 :msg "prevent the Runner from making another run" :effect (effect (prevent-run))}]}

   "Fenris"
   {:effect (effect (gain :bad-publicity 1) (system-msg "takes 1 bad publicity"))
    :abilities [{:msg "do 1 brain damage" :effect (effect (damage :brain 1 {:card card}))}
                {:msg "end the run" :effect (effect (end-run))}]}

   "Fire Wall"
   {:advanceable :always :abilities [{:msg "end the run" :effect (effect (end-run))}]
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
   {:abilities [{:label "Trace 2"
                 :trace {:base 2 :msg "do 1 net damage" :effect (effect (damage :net 1) {:card card})
                         :kicker {:min 5 :msg "do 1 net damage" :effect (effect (damage :net 1 {:card card}))}}}]}

   "Grim"
   {:effect (effect (gain :bad-publicity 1) (system-msg "takes 1 bad publicity"))
    :abilities [trash-program]}

   "Guard"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Gutenberg"
   {:abilities [{:label "Trace 7 - Give the Runner 1 tag"
                 :trace {:base 7 :msg "give the Runner 1 tag" :effect (effect (gain :runner :tag 1))}}]
    :strength-bonus (req (if (= (second (:zone card)) :rd) 3 0))}

   "Hadrians Wall"
   {:advanceable :always
    :abilities [{:msg "end the run" :effect (effect (end-run))}]
    :strength-bonus (req (or (:advance-counter card) 0))}

   "Himitsu-Bako"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}
                {:msg "add it to HQ" :cost [:credit 1] :effect (effect (move card :hand))}]}

   "Hive"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Heimdall 1.0"
   {:abilities [{:msg "do 1 brain damage" :effect (effect (damage :brain 1 {:card card}))}
                {:msg "end the run" :effect (effect (end-run))}]}

   "Heimdall 2.0"
   {:abilities [{:msg "do 1 brain damage" :effect (effect (damage :brain 1 {:card card}))}
                {:msg "do 1 brain damage and end the run" :effect (effect (damage :brain 1 {:card card}) (end-run))}
                {:msg "end the run" :effect (effect (end-run))}]}

   "Hourglass"
   {:abilities [{:msg "force the Runner to lose 1 [Click] if able"
                 :effect (effect (lose :runner :click 1))}]}

   "Hudson 1.0"
   {:abilities [{:msg "prevent the Runner from accessing more than 1 card during this run"
                 :effect (effect (max-access 1))}]}

   "Hunter"
   {:abilities [{:label "Trace 3 - Give the Runner 1 tag"
                 :trace {:base 3 :msg "give the Runner 1 tag" :effect (effect (gain :runner :tag 1))}}]}

   "Ice Wall"
   {:advanceable :always :abilities [{:msg "end the run" :effect (effect (end-run))}]
    :strength-bonus (req (or (:advance-counter card) 0))}

   "Ichi 1.0"
   {:abilities [trash-program
                {:label "Trace 1 - Give the Runner 1 tag and do 1 brain damage"
                 :trace {:base 1 :msg "give the Runner 1 tag and do 1 brain damage"
                         :effect (effect (damage :brain 1 {:card card}) (gain :runner :tag 1))}}]}

   "Ichi 2.0"
   {:abilities [trash-program
                {:label "Trace 3 - Give the Runner 1 tag and do 1 brain damage"
                 :trace {:base 3 :msg "give the Runner 1 tag and do 1 brain damage"
                         :effect (effect (damage :brain 1 {:card card}) (gain :runner :tag 1))}}]}

   "IQ"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]
    :strength-bonus (req (count (:hand corp)))
    :rez-cost-bonus (req (count (:hand corp)))}

   "Information Overload"
   {:abilities [{:label "Trace 1 - Give the Runner 1 tag"
                 :trace {:base 1 :msg "give the Runner 1 tag" :effect (effect (gain :runner :tag 1))}}]}

   "Ireress"
   {:abilities [{:msg "make the Runner lose 1 [Credits]" :effect (effect (lose :runner :credit 1))}]}

   "Janus 1.0"
   {:abilities [{:msg "do 1 brain damage" :effect (effect (damage :brain 1 {:card card}))}]}

   "Kitsune"
   {:abilities [{:prompt "Choose a card in HQ" :choices (req (:hand corp))
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
   {:abilities [{:msg "end the run" :effect (effect (end-run))}
                {:msg "make the Runner gain 5 [Credits]" :effect (effect (gain :runner :credit 5))}]}

   "Lotus Field"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Lycan"
   {:advanceable :always
    :abilities [trash-program]}

   "Mamba"
   {:abilities [{:msg "do 1 net damage" :effect (effect (damage :net 1 {:card card}))}
                {:msg "do 1 net damage using 1 power counter"
                 :counter-cost 1 :effect (effect (damage :net 1 {:card card}))}
                {:msg "start a Psi game"
                 :psi {:not-equal {:msg "add 1 power counter"
                                   :effect (effect (add-prop :runner card :counter 1))}}}]}

   "Markus 1.0"
   {:abilities [{:msg "force the Runner to trash a card"}
                {:msg "end the run" :effect (effect (end-run))}]}

   "Matrix Analyzer"
   {:abilities [{:label "Place 1 advancement token on a card that can be advanced"
                 :msg (msg "place 1 advancement token on " (if (:rezzed target) (:title target) "a card"))
                 :choices {:req #(or (= (:type %) "Agenda") (:advanceable %))}
                 :cost [:credit 1] :effect (effect (add-prop target :advance-counter 1))}
                {:label "Trace 2 - Give the Runner 1 tag"
                 :trace {:base 2 :msg "give the Runner 1 tag" :effect (effect (gain :runner :tag 1))}}]}

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
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]
    :strength-bonus (req (if (= (second (:zone card)) :hq) 3 0))}

   "Minelayer"
   {:abilities [{:msg "install an ICE from HQ"
                 :choices (req (filter #(has? % :type "ICE") (:hand corp)))
                 :prompt "Choose an ICE to install"
                 :effect (req (corp-install state side target (:server run)))}]}

   "Mother Goddess"
   (let [ab {:req (req (= (:type target) "ICE"))
             :effect (effect (update! (assoc card :subtype
                                                  (->> (mapcat :ices (flatten (seq (:servers corp))))
                                                       (filter #(and (:rezzed %) (not= (:cid card) (:cid %))))
                                                       (mapcat #(vec (.split (:subtype %) " - ")))
                                                       (cons "Mythic")
                                                       distinct
                                                       (join " - ")))))}]
     {:abilities [{:msg "end the run" :effect (effect (end-run))}]
      :events {:rez ab :trash ab :derez ab}})

   "Muckraker"
   {:effect (effect (gain :bad-publicity 1))
    :abilities [{:label "Trace 1 - Give the Runner 1 tag"
                 :trace {:base 1 :msg "give the Runner 1 tag" :effect (effect (gain :runner :tag 1))}}
                {:label "Trace 2 - Give the Runner 1 tag"
                 :trace {:base 2 :msg "give the Runner 1 tag" :effect (effect (gain :runner :tag 1))}}
                {:label "Trace 3 - Give the Runner 1 tag"
                 :trace {:base 3 :msg "give the Runner 1 tag" :effect (effect (gain :runner :tag 1))}}
                {:msg "end the run if the Runner is tagged" :req (req tagged)
                 :effect (effect (end-run))}]}

   "Nebula"
   {:advanceable :always
    :abilities [trash-program]
    :rez-cost-bonus (req (* -3 (or (:advance-counter card) 0)))}

   "Negotiator"
   {:abilities [{:msg "gain 2 [Credits]" :effect (effect (gain :credit 2))}
                trash-program]}

   "Neural Katana"
   {:abilities [{:msg "do 3 net damage" :effect (effect (damage :net 3 {:card card}))}]}

   "NEXT Bronze"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]
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
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Orion"
   {:advanceable :always
    :abilities [trash-program
                {:msg "end the run" :effect (effect (end-run))}]
    :rez-cost-bonus (req (* -3 (or (:advance-counter card) 0)))}

   "Pachinko"
   {:abilities [{:label "End the run if the Runner is tagged"
                 :req (req tagged) :msg "end the run" :effect (effect (end-run))}]}

   "Paper Wall"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Pop-up Window"
   {:abilities [{:msg "gain 1 [Credits]" :effect (effect (gain :credit 1))}
                {:msg "end the run" :effect (effect (end-run))}]}

   "Pup"
   {:abilities [{:msg "do 1 net damage" :effect (effect (damage :net 1 {:card card}))}]}

   "Quandary"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Quicksand"
   {:events {:encounter-ice {:req (req (= (:cid target) (:cid card)))
                             :effect (effect (add-prop card :counter 1))}}
    :strength-bonus (req (or (:counter card) 0))
    :abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Rainbow"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Rototurret"
   {:abilities [trash-program
                {:msg "end the run" :effect (effect (end-run))}]}

   "Sagittarius"
   {:abilities [{:label "Trace 2 - Trash a program"
                 :trace (assoc trash-program :base 2 :not-distinct true
                                             :kicker (assoc trash-program :min 5))}]}

   "Salvage"
   {:advanceable :while-rezzed
    :abilities [{:label "Trace 2 - Give the Runner 1 tag"
                 :trace {:base 2 :msg "give the Runner 1 tag" :effect (effect (gain :runner :tag 1))}}]}

   "Searchlight"
   {:advanceable :always
    :abilities [{:label "Trace X - Give the runner 1 tag"
                 :trace {:base (req (or (:advance-counter card) 0)) :effect (effect (gain :runner :tag 1))
                         :msg "give the Runner 1 tag"}}]}

   "Shadow"
   {:advanceable :always
    :abilities [{:msg "gain 2 [Credits]" :effect (effect (gain :credit 2))}
                {:label "Trace 3 - Give the Runner 1 tag"
                 :trace {:base 3 :msg "give the Runner 1 tag" :effect (effect (gain :runner :tag 1))}}]
    :strength-bonus (req (or (:advance-counter card) 0))}

   "Sherlock 1.0"
   {:abilities [{:label "Trace 4 - Add an installed program to the top of Stack"
                 :trace {:base 4 :choices {:req #(and (:installed %) (= (:type %) "Program"))}
                         :msg (msg "add " (:title target) " to the top of Stack")
                         :effect (effect (move :runner target :deck true))}}]}

   "Shinobi"
   {:effect (effect (gain :bad-publicity 1) (system-msg "takes 1 bad publicity"))
    :abilities [{:label "Trace 1 - Do 1 net damage"
                 :trace {:base 1 :msg "do 1 net damage" :effect (effect (damage :net 1 {:card card}))}}
                {:label "Trace 2 - Do 2 net damage"
                 :trace {:base 2 :msg "do 2 net damage" :effect (effect (damage :net 2 {:card card}))}}
                {:label "Trace 3 - Do 3 net damage"
                 :trace {:base 3 :msg "do 3 net damage" :effect (effect (damage :net 3 {:card card}))}}]}

   "Snoop"
   {:abilities [{:msg "place 1 power counter on Snoop" :effect (effect (add-prop card :counter 1))}
                {:counter-cost 1 :label "Look at all cards in Grip and trash 1 card"
                 :msg (msg "Look at all cards in Grip and trashes " (:title target))
                 :choices (req (:hand runner)) :prompt "Choose a card to trash"
                 :effect (effect (trash target))}]}

   "Snowflake"
   {:abilities [{:msg "start a Psi game"
                 :psi {:not-equal {:msg "end the run" :effect (effect (end-run))}}}]}

   "Spiderweb"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Susanoo-No-Mikoto"
   {:abilities [{:req (req (not= (:server run) [:discard]))
                 :msg "make the Runner continue the run on Archives"
                 :effect (req (swap! state update-in [:run]
                                     #(assoc % :position (count (get-in corp [:servers :archives :ices]))
                                               :server [:archives])))}]}

   "Swarm"
   {:advanceable :always
    :abilities [trash-program]}

   "Swordsman"
   {:abilities [{:msg "do 1 net damage" :effect (effect (damage :net 1 {:card card}))}
                {:prompt "Choose an AI program to trash" :msg (msg "trashes " (:title target))
                 :label "Trash an AI program" :effect (effect (trash target))
                 :choices {:req #(and (:installed %) (= (:type %) "Program") (has? % :subtype "AI"))}}]}

   "Taurus"
   {:abilities [{:label "Trace 2 - Trash a piece of hardware"
                 :trace (assoc trash-hardware :base 2 :not-distinct true
                                              :kicker (assoc trash-hardware :min 5))}]}

   "TMI"
   {:trace {:base 2 :unsuccessful {:effect (effect (derez card))}}
    :abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Tollbooth"
   {:abilities [{:msg "force the Runner to lose 3 [Credits]"
                 :effect (effect (lose :runner :credit 3))}
                {:msg "end the run" :effect (effect (end-run))}]}

   "Troll"
   {:abilities [{:player :runner :prompt "Choose one" :choices ["Lose [Click]" "End the run"]
                 :label "Force the Runner to lose [Click] or end the run"
                 :effect (req (if-not (and (= target "Lose [Click]") (pay state side card :click 1))
                                (do (end-run state side) (system-msg state side "ends the run"))
                                (system-msg state side "loses [Click]")))}]}

   "Tsurugi"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}
                {:msg "do 1 net damage" :effect (effect (damage :net 1 {:card card}))}]}

   "Turing"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]
    :strength-bonus (req (if (= (second (:zone card)) :remote) 3 0))}

   "Tyrant"
   {:advanceable :while-rezzed
    :abilities [{:msg "end the run" :effect (effect (end-run))}]}

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
   {:abilities [{:msg "do 1 brain damage" :effect (effect (damage :brain 1 {:card card}))}
                {:msg "end the run" :effect (effect (end-run))}]}

   "Viktor 2.0"
   {:abilities [{:msg "do 1 brain damage using 1 power counter" :counter-cost 1
                 :effect (effect (damage :brain 1 {:card card}))}
                {:label "Trace 2 - Add 1 power counter"
                 :trace {:base 2 :msg "add 1 power counter" :effect (effect (add-prop card :counter 1))}}
                {:msg "end the run" :effect (effect (end-run))}]}

   "Viper"
   {:abilities [{:label "Trace 3 - The Runner loses 1 [Click] if able"
                 :trace {:base 3 :msg  "force the Runner to lose 1 [Click] if able"
                         :effect (effect (lose :runner :click 1))}}
                {:label "Trace 3 - End the run"
                 :trace {:base 3 :msg "end the run" :effect (effect (end-run))}}]}

   "Virgo"
   {:abilities [{:label "Trace 2"
                 :trace {:base 2 :msg "give the Runner 1 tag" :effect (effect (gain :runner :tag 1))
                         :kicker {:min 5 :msg "give the Runner 1 tag"
                                  :effect (effect (gain :runner :tag 1))}}}]}

   "Wall of Static"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Wall of Thorns"
   {:abilities [{:msg "do 2 net damage" :effect (effect (damage :net 2 {:card card}))}
                {:msg "end the run" :effect (effect (end-run))}]}

   "Wendigo"
   {:advanceable :always
    :abilities [{:msg "prevent the Runner from using a chosen program for the remainder of this run"}]}

   "Whirlpool"
   {:abilities [{:msg "prevent the Runner from jacking out"
                 :effect (effect (trash card) (prevent-jack-out))}]}

   "Woodcutter"
   {:advanceable :while-rezzed
    :abilities [{:msg "do 1 net damage" :effect (effect (damage :net 1 {:card card}))}]}

   "Wormhole"
   {:advanceable :always
    :rez-cost-bonus (req (* -3 (or (:advance-counter card) 0)))}

   "Wotan"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]}

   "Wraparound"
   {:abilities [{:msg "end the run" :effect (effect (end-run))}]
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
                            :effect (effect (move (first (:deck corp)) :deck))}}]}})