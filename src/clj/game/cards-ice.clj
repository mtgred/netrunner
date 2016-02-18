(in-ns 'game.core)

(declare trash-program trash-hardware trash-installed)

;;;; Helper functions specific for ICE

;;; General subroutines
(def end-the-run
  "Basic ETR subroutine"
  {:label "End the run"
   :msg "end the run"
   :effect (effect (end-run))})

(def give-tag
  "Basic give runner 1 tag subroutine
   Mostly used with tag-trace"
  {:label "Give the Runner 1 tag"
   :msg "give the Runner 1 tag"
   :effect (effect (tag-runner :runner 1))}) 

(def add-power-counter
  "Adds 1 power counter to the card."
  {:label "Add 1 power counter"
   :msg "add 1 power counter"
   :effect (effect (add-prop card :counter 1))})

(defn trace-ability
  "Run a trace with specified base strength.
   If successful trigger specified ability"
  [base ability]
  {:label (str "Trace " base " - " (:label ability))
   :trace (assoc ability :base base)})

(defn tag-trace
  "Trace ability for giving a tag, at specified base strength"
  [base]
  (trace-ability base give-tag))

(defn do-net-damage
  "Do specified amount of net-damage."
  [dmg]
  {:label (str "Do " dmg " net damage")
   :msg (str "do " dmg " net damage")
   :effect (effect (damage :net dmg {:card card}))})

(defn do-brain-damage
  "Do specified amount of brain damage."
  [dmg]
  {:label (str "Do " dmg " brain damage")
   :msg (str "do " dmg " brain damage")
   :effect (effect (damage :brain dmg {:card card}))})

(defn gain-credits
  "Gain specified amount of credits"
  [credits]
  {:label (str "Gain " credits " [Credits]")
   :msg (str "gain " credits " [Credits]")
   :effect (effect (gain :credit credits))})

(defn power-counter-ability
  "Does specified ability using a power counter."
  [{:keys [label msg] :as ability}]
  (assoc ability :label (str "Hosted power counter: " label)
                 :msg (str msg " using 1 power counter")
                 :counter-cost 1))

(defn do-psi
  "Start a psi game, if not equal do ability"
  [{:keys [label] :as ability}]
  {:label (str "Psi Game - " label)
   :msg (str "start a psi game (" label ")")
   :psi {:not-equal ability}})

(def take-bad-pub
  "Bad pub on rez effect."
  (effect (gain :bad-publicity 1)
          (system-msg (str "takes 1 bad publicity from " (:title card)))))


;;; For Advanceable ICE
(def advance-counters
  "Number of advancement counters - for advanceable ICE."
  (req (:advance-counter card 0)))

(def space-ice-rez-bonus
  "Amount of rez reduction for the Space ICE."
  (req (* -3 (:advance-counter card 0))))

(defn space-ice
  "Creates data for Space ICE with specified abilities."
  [& abilities]
  {:advanceable :always
   :abilities (vec abilities)
   :rez-cost-bonus space-ice-rez-bonus})


;;; For Grail ICE
(defn grail-in-hand
  "Req that specified card is a Grail card in the Corp's hand."
  [card]
  (and (= (:side card) "Corp")
       (in-hand? card)
       (has-subtype? card "Grail")))

(def reveal-grail
  "Ability for revealing Grail ICE from HQ."
  {:label "Reveal up to 2 Grail ICE from HQ"
   :choices {:max 2
             :req grail-in-hand}
   :msg (let [sub-label #(:label (first (:abilities (card-def %))))]
         (msg "reveal " (join ", " (map #(str (:title %) " (" (sub-label %) ")") targets))))})

(def resolve-grail
  "Ability for resolving a subroutine on a Grail ICE in HQ."
  {:label "Resolve a Grail ICE subroutine from HQ"
   :choices {:req grail-in-hand}
   :effect (req (doseq [ice targets]
                  (let [subroutine (first (:abilities (card-def ice)))]
                    (resolve-ability state side subroutine card nil))))})

(defn grail-ice
  "Creates data for grail ICE"
  [ability]
  {:abilities [ability reveal-grail resolve-grail]})


;;; For NEXT ICE
(defn next-ice-count
  "Counts number of rezzed NEXT ICE - for use with NEXT Bronze and NEXT Gold"
  [corp]
  (let [servers (flatten (seq (:servers corp)))
        rezzed-next? #(and (rezzed? %) (has-subtype? % "NEXT"))]
    (reduce (fn [c server] (+ c (count (filter rezzed-next? (:ices server))))) 0 servers)))


;;; For Morph ICE
(defn morph [state side card new old]
  (update! state side (assoc card :subtype
                             (->> (remove #(= old %) (.split (:subtype card) " - "))
                                  vec (concat [new]) distinct (join " - "))))
  (update-ice-strength state side card)
  (update-run-ice state side))

(defn morph-effect
  "Creates morph effect for ICE. Morphs from base type to other type"
  [base other]
  (req (if (odd? (get (get-card state card) :advance-counter 0))
         (morph state side card other base)
         (morph state side card base other))))

(defn morph-ice
  "Creates the data for morph ICE with specified types and ability."
  [base other ability]
  (let [ab {:req (req (= (:cid card) (:cid target)))
            :effect (morph-effect base other)}]
    {:advanceable :always
     :effect (morph-effect base other)
     :abilities [ability]
     :events {:advance ab :advancement-placed ab}}))


;;; For Constellation ICE
(defn constellation-ice
  "Generates map for Constellation ICE with specified effect."
  [ability]
  {:abilities [(trace-ability 2 (assoc ability :kicker (assoc ability :min 5)))]})


;;;; Card definitions
(def cards-ice
  {"Archangel"
   {:access {:optional
             {:req (req (not= (first (:zone card)) :discard))
              :prompt "Pay 3 [Credits] to force Runner to encounter Archangel?"
              :yes-ability {:cost [:credit 3]
                            :effect (req (system-msg state :corp "pays 3 [Credits] to force the Runner to encounter Archangel"))}
              :no-ability {:effect (req (system-msg state :corp "declines to force the Runner to encounter Archangel"))}}}
    :abilities [(trace-ability 6 {:choices {:req installed?}
                                  :label "Add 1 installed card to the Runner's Grip"
                                  :msg "add 1 installed card to the Runner's Grip"
                                  :effect (effect (move :runner target :hand true)
                                                  (system-msg (str "adds " (:title target)
                                                                   " to the Runner's Grip")))})]}

   "Archer"
   {:additional-cost [:forfeit]
    :abilities [(gain-credits 2)
                trash-program
                end-the-run]}

   "Architect"
   {:flags {:untrashable-while-rezzed true}
    :abilities [{:msg "look at the top 5 cards of R&D"
                 :prompt "Choose a card to install"
                 :priority true
                 :activatemsg "uses Architect to look at the top 5 cards of R&D"
                 :req (req (and (not (string? target))
                                (not (is-type? target "Operation"))))
                 :not-distinct true
                 :choices (req (conj (take 5 (:deck corp)) "No install"))
                 :effect (effect (corp-install (move state side target :play-area)
                                               nil {:no-install-cost true}))}
                {:label "Install a card from HQ or Archives"
                 :prompt "Choose a card to install from Archives or HQ"
                 :show-discard true
                 :priority true
                 :choices {:req #(and (not (is-type? % "Operation"))
                                      (#{[:hand] [:discard]} (:zone %))
                                      (= (:side %) "Corp"))}
                 :effect (effect (corp-install target nil))
                 :msg (msg (corp-install-msg target))}]}

   "Ashigaru"
   {:abilities [end-the-run]}

   "Assassin"
   {:abilities [(trace-ability 5 (do-net-damage 3))
                (trace-ability 4 trash-program)]}

   "Asteroid Belt"
   (space-ice end-the-run)

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

   "Brainstorm"
   {:abilities [(do-brain-damage 1)]}

   "Builder"
   {:abilities [{:label "Move Builder to the outermost position of any server"
                 :cost [:click 1] :prompt "Choose a server" :choices (req servers)
                 :msg (msg "move it to the outermost position of " target)
                 :effect (effect (move card (conj (server->zone state target) :ices)))}
                {:label "Place 1 advancement token on an ICE that can be advanced protecting this server"
                 :msg (msg "place 1 advancement token on " (card-str state target))
                 :choices {:req #(and (ice? %)
                                      (can-be-advanced? %))}
                 :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]}

   "Bullfrog"
   {:abilities [(do-psi {:label "Move Bullfrog to another server"
                         :player :corp
                         :prompt "Choose a server"
                         :choices (req servers)
                         :msg (msg "move it to the outermost position of " target)
                         :effect (req (let [dest (server->zone state target)]
                                        (swap! state update-in [:run]
                                               #(assoc % :position (count (get-in corp (conj dest :ices)))
                                                       :server (rest dest))))
                                      (move state side card
                                            (conj (server->zone state target) :ices))
                                      (update-run-ice state side))})]}

   "Burke Bugs"
   {:abilities [(trace-ability 0 (assoc trash-program :not-distinct true
                                        :player :runner
                                        :msg "force the Runner to trash a program"
                                        :label "Force the Runner to trash a program"))]}

   "Caduceus"
   {:abilities [(trace-ability 3 (gain-credits 3))
                (trace-ability 2 end-the-run)]}

   "Cell Portal"
   {:abilities [{:msg "make the Runner approach the outermost ICE"
                 :effect (req (swap! state assoc-in [:run :position] 0) (derez state side card))}]}

   "Changeling"
   (morph-ice "Barrier" "Sentry" end-the-run)

   "Checkpoint"
   {:effect take-bad-pub
    :abilities [(trace-ability 5 {:label "Do 3 meat damage when this run is successful"
                                  :msg "do 3 meat damage when this run is successful"
                                  :effect (req (swap! state assoc-in [:run :run-effect :end-run]
                                                      {:req (req (:successful run))
                                                       :msg "do 3 meat damage"
                                                       :effect (effect (damage :meat 3
                                                                               {:card card}))})
                                               (swap! state assoc-in [:run :run-effect :card]
                                                      card))})]}

   "Chimera"
   (let [ab {:effect (effect (derez :corp card)
                             (update! (assoc (get-card state card) :subtype "Mythic")))}]
     {:prompt "Choose one subtype"
      :choices ["Barrier" "Code Gate" "Sentry"]
      :msg (msg "make it gain " target " until the end of the turn")
      :effect (effect (update! (assoc card :subtype
                                      (->> (vec (.split (:subtype card) " - "))
                                           (concat [target])
                                           (join " - "))))
                      (update-ice-strength card))
      :events {:runner-turn-ends ab
               :corp-turn-ends ab}
      :abilities [end-the-run]})

   "Clairvoyant Monitor"
   {:abilities [(do-psi {:label "Place 1 advancement token and end the run"
                         :player :corp
                         :prompt "Choose a target for Clairvoyant Monitor"
                         :msg (msg "place 1 advancement token on "
                                   (card-str state target) " and end the run")
                         :choices {:req installed?}
                         :effect (effect (add-prop target :advance-counter 1 {:placed true})
                                         (end-run))})]}

   "Chum"
   {:abilities [(do-net-damage 3)]}

   "Cortex Lock"
   {:abilities [{:label "Do 1 net damage for each unused memory units the Runner has"
                 :msg (msg "do " (:memory runner) " net damage")
                 :effect (effect (damage :net (:memory runner) {:card card}))}]}

   "Crick"
   {:abilities [{:label "install a card from Archives"
                 :msg (msg (corp-install-msg target))
                 :prompt "Choose a card to install from Archives"
                 :show-discard true :priority true
                 :choices {:req #(and (not (is-type? % "Operation"))
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
   {:abilities [(trace-ability 2 {:label "Look at the top of Stack"
                                  :msg "look at top X cards of Stack"
                                  :effect (req (doseq [c (take (- target (second targets))
                                                               (:deck runner))]
                                                 (move state side c :play-area))
                                               (system-msg state :corp
                                                           (str "looks at the top "
                                                                (- target (second targets))
                                                                " cards of Stack")))})]}

   "Data Mine"
   {:abilities [{:msg "do 1 net damage"
                 :effect (req (damage state :runner :net 1 {:card card})
                              (when current-ice
                                (trash-ice-in-run state))
                              (trash state side card))}]}

   "Datapike"
   {:abilities [{:msg "force the Runner to pay 2 [Credits] if able"
                 :effect (effect (pay :runner card :credit 2))}
                end-the-run]}

   "Data Raven"
   {:abilities [give-tag
                (power-counter-ability give-tag)
                (trace-ability 3 add-power-counter)]}

   "Dracō"
   {:prompt "How many power counters?"
    :choices :credit
    :msg (msg "add " target " power counters")
    :effect (effect (set-prop card :counter target)
                    (update-ice-strength card))
    :strength-bonus (req (or (:counter card) 0))
    :abilities [(trace-ability 2 {:label "Give the Runner 1 tag and end the run"
                                  :msg "give the Runner 1 tag and end the run"
                                  :effect (effect (tag-runner :runner 1)
                                                  (end-run))})]}

   "Eli 1.0"
   {:abilities [end-the-run]}

   "Enforcer 1.0"
   {:additional-cost [:forfeit]
    :abilities [trash-program
                (do-brain-damage 1)
                {:label "Trash a console" :effect (effect (trash target))
                 :prompt "Choose a console to trash" :msg (msg "trash " (:title target))
                 :choices {:req #(has-subtype? % "Console")}}
                {:msg "trash all virtual resources"
                 :effect (req (doseq [c (filter #(has-subtype? % "Virtual") (all-installed state :runner))]
                                (trash state side c)))}]}

   "Enigma"
   {:abilities [{:msg "force the Runner to lose 1 [Click] if able"
                 :effect (effect (lose :runner :click 1))}
                end-the-run]}

   "Errand Boy"
   {:abilities [(gain-credits 1)
                {:msg "draw 1 card" :effect (effect (draw))}]}

   "Excalibur"
   {:abilities [{:label "The Runner cannot make another run this turn"
                 :msg "prevent the Runner from making another run" :effect (effect (prevent-run))}]}

   "Fenris"
   {:effect take-bad-pub
    :abilities [(do-brain-damage 1)
                end-the-run]}

   "Fire Wall"
   {:advanceable :always
    :abilities [end-the-run]
    :strength-bonus advance-counters}

   "Flare"
   {:abilities [(trace-ability 6 {:label "Trash 1 hardware, do 2 meat damage, and end the run"
                                  :msg "trash 1 hardware, do 2 meat damage, and end the run"
                                  :effect (effect (resolve-ability
                                                   {:prompt "Choose a piece of hardware to trash"
                                                    :label "Trash a piece of hardware"
                                                    :msg (msg "trash " (:title target))
                                                    :choices {:req #(is-type? % "Hardware")}
                                                    :effect (effect (trash target {:cause :subroutine}))}
                                                   card nil)
                                                  (damage :meat 2 {:unpreventable true
                                                                   :card card})
                                                  (end-run))})]}

   "Galahad"
   (grail-ice end-the-run)

   "Gemini"
   (constellation-ice (do-net-damage 1))

   "Grim"
   {:effect take-bad-pub
    :abilities [trash-program]}

   "Guard"
   {:abilities [end-the-run]}

   "Gutenberg"
   {:abilities [(tag-trace 7)]
    :strength-bonus (req (if (= (second (:zone card)) :rd) 3 0))}

   "Gyri Labyrinth"
   {:abilities [{:req (req (:run @state))
                 :label "Reduce Runner's maximum hand size by 2 until start of next Corp turn"
                 :msg "reduce the Runner's maximum hand size by 2 until the start of the next Corp turn"
                 :effect (effect (lose :runner :hand-size-modification 2)
                                 (register-events {:corp-turn-begins
                                                   {:msg "increase the Runner's maximum hand size by 2"
                                                    :effect (effect (gain :runner :hand-size-modification 2)
                                                                    (unregister-events card))}} card))}]
    :events {:corp-turn-begins nil}}

   "Hadrians Wall"
   {:advanceable :always
    :abilities [end-the-run]
    :strength-bonus advance-counters}

   "Harvester"
   {:abilities [{:label "Runner draws 3 cards and discards down to maximum hand size"
                 :msg "make the Runner draw 3 cards and discard down to their maximum hand size"
                 :effect (req (draw state :runner 3)
                              (let [delta (- (count (get-in @state [:runner :hand])) (hand-size state :runner))]
                                (when (> delta 0)
                                  (resolve-ability
                                    state :runner
                                    {:prompt (msg "Choose " delta " cards to discard")
                                     :player :runner
                                     :choices {:max delta :req #(in-hand? %)}
                                     :effect (req (doseq [c targets]
                                                    (trash state :runner c))
                                                  (system-msg state :runner
                                                              (str "trashes " (join ", " (map :title targets)))))}
                                   card nil))))}]}

   "Himitsu-Bako"
   {:abilities [end-the-run {:msg "add it to HQ" :cost [:credit 1] :effect (effect (move card :hand))}]}

   "Hive"
   {:abilities [end-the-run]}

   "Heimdall 1.0"
   {:abilities [(do-brain-damage 1)
                end-the-run]}

   "Heimdall 2.0"
   {:abilities [(do-brain-damage 1)
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
                        :choices (req (filter #(and (ice? %)
                                                    (has-subtype? % "Bioroid"))
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
   {:abilities [(tag-trace 3)]}

   "Ice Wall"
   {:advanceable :always :abilities [end-the-run]
    :strength-bonus advance-counters}

   "Ichi 1.0"
   {:abilities [trash-program
                (trace-ability 1 {:label "Give the Runner 1 tag and do 1 brain damage"
                                  :msg "give the Runner 1 tag and do 1 brain damage"
                                  :effect (effect (damage :brain 1 {:card card})
                                                  (tag-runner :runner 1))})]}

   "Ichi 2.0"
   {:abilities [trash-program
                (trace-ability 3 {:label "Give the Runner 1 tag and do 1 brain damage"
                                  :msg "give the Runner 1 tag and do 1 brain damage"
                                  :effect (effect (damage :brain 1 {:card card})
                                                  (tag-runner :runner 1))})]}

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
   {:abilities [(tag-trace 1)
                trash-installed]}

   "Ireress"
   {:abilities [{:msg "make the Runner lose 1 [Credits]" :effect (effect (lose :runner :credit 1))}]}

   "Its a Trap!"
   {:expose {:msg "do 2 net damage" :effect (effect (damage :net 2 {:card card}))}
    :abilities [(assoc trash-installed :effect (req (trash state side target {:cause :subroutine})
                                                    (when current-ice
                                                      (trash-ice-in-run state))
                                                    (trash state side card)))]}

   "Janus 1.0"
   {:abilities [(do-brain-damage 1)]}

   "Kitsune"
   {:abilities [{:prompt "Choose a card in HQ to force access"
                 :choices {:req in-hand?}
                 :label "Force the Runner to access a card in HQ"
                 :msg (msg "force the Runner to access " (:title target))
                 :effect (effect (handle-access targets) (trash card))}]}

   "Komainu"
   {:abilities [(do-net-damage 1)]}

   "Lab Dog"
   {:abilities [(assoc trash-hardware :label "Force the Runner to trash an installed piece of hardware"
                                      :player :runner
                                      :msg (msg "force the Runner to trash " (:title target))
                                      :effect (req (trash state side target)
                                                   (when current-ice
                                                     (trash-ice-in-run state))
                                                   (trash state side card)))]}

   "Lancelot"
   (grail-ice trash-program)

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
   (morph-ice "Sentry" "Code Gate" trash-program)

   "Mamba"
   {:abilities [(power-counter-ability (do-net-damage 1))
                (do-net-damage 1)
                (do-psi add-power-counter)]}

   "Markus 1.0"
   {:abilities [trash-installed end-the-run]}

   "Matrix Analyzer"
   {:abilities [{:label "Place 1 advancement token on a card that can be advanced"
                 :msg (msg "place 1 advancement token on " (card-str state target))
                 :choices {:req can-be-advanced?}
                 :cost [:credit 1] :effect (effect (add-prop target :advance-counter 1))}
                (tag-trace 2)]}

   "Merlin"
   (grail-ice (do-net-damage 2))

   "Meru Mati"
   {:abilities [end-the-run]
    :strength-bonus (req (if (= (second (:zone card)) :hq) 3 0))}

   "Minelayer"
   {:abilities [{:msg "install an ICE from HQ"
                 :choices {:req #(and (ice? %)
                                      (in-hand? %))}
                 :prompt "Choose an ICE to install from HQ"
                 :effect (req (corp-install state side target (:server run) {:no-install-cost true}))}]}

   "Mother Goddess"
   (let [ab {:req (req (ice? target))
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
   {:effect take-bad-pub
    :abilities [(tag-trace 1)
                (tag-trace 2)
                (tag-trace 3)
                {:msg "end the run if the Runner is tagged" :req (req tagged)
                 :effect (effect (end-run))}]}

   "Nebula"
   (space-ice trash-program)

   "Negotiator"
   {:abilities [(gain-credits 2)
                trash-program]}

   "Neural Katana"
   {:abilities [(do-net-damage 3)]}

   "News Hound"
   {:abilities [(tag-trace 3)
                {:label "End the run if a Current is active"
                 :req (req (or (not (empty? (runner :current)))
                               (not (empty? (corp :current)))))
                 :effect (effect (end-run)) :msg "end the run"}]}

   "NEXT Bronze"
   {:abilities [end-the-run]
    :strength-bonus (req (next-ice-count corp))
    :events (let [nb {:req (req (and (not= (:cid target) (:cid card))
                                     (has-subtype? target "NEXT")))
                      :effect (effect (update-ice-strength card))}]
              {:rez nb :derez nb :trash nb :card-moved nb})}

   "NEXT Gold"
   {:abilities [{:label "Do 1 net damage for each rezzed NEXT ice"
                 :msg (msg "do " (next-ice-count corp) " net damage")
                 :effect (effect (damage :net (next-ice-count corp) {:card card}))}
                trash-program]}

   "NEXT Silver"
   {:abilities [end-the-run]}

   "Orion"
   ;; TODO: wormhole subroutine
   (space-ice trash-program end-the-run)

   "Pachinko"
   {:abilities [{:label "End the run if the Runner is tagged"
                 :req (req tagged)
                 :msg "end the run"
                 :effect (effect (end-run))}]}

   "Paper Wall"
   {:abilities [end-the-run]}

   "Pop-up Window"
   {:abilities [(gain-credits 1)
                end-the-run]}

   "Pup"
   {:abilities [(do-net-damage 1)]}

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
    :abilities [(trace-ability 4 end-the-run)]}

   "Rototurret"
   {:abilities [trash-program end-the-run]}

   "Sagittarius"
   (constellation-ice trash-program)

   "Salvage"
   {:advanceable :while-rezzed
    :abilities [(tag-trace 2)]}

   "Searchlight"
   {:advanceable :always
    ;; Could replace this with (tag-trace advance-counters).
    :abilities [{:label "Trace X - Give the Runner 1 tag"
                 :trace {:base advance-counters :effect (effect (tag-runner :runner 1))
                         :msg "give the Runner 1 tag"}}]}

   "Sensei"
   {:abilities [{:label "Give each other ICE encountered \"End the run\" for the remainder of the run"
                 :msg (msg "give each other ICE encountered \"[Subroutine] End the run\" after all its other subroutines for the remainder of the run")}]}

   "Shadow"
   {:advanceable :always
    :abilities [(gain-credits 2)
                (tag-trace 3)]
    :strength-bonus advance-counters}

   "Sherlock 1.0"
   {:abilities [{:label "Trace 4 - Add an installed program to the top of Stack"
                 :trace {:base 4
                         :choices {:req #(and (installed? %)
                                              (is-type? % "Program"))}
                         :msg (msg "add " (:title target) " to the top of Stack")
                         :effect (effect (move :runner target :deck {:front true}))}}]}

   "Shinobi"
   {:effect take-bad-pub
    :abilities [(trace-ability 1 (do-net-damage 1))
                (trace-ability 2 (do-net-damage 2))
                (trace-ability 3 {:label "Do 3 net damage and end the run"
                                  :msg "do 3 net damage and end the run"
                                  :effect (effect (damage :net 3 {:card card}) (end-run))})]}

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
   {:abilities [{:req (req (= current-ice card))
                 :label "Reveal all cards in the Runner's Grip"
                 :msg (msg "reveal " (join ", " (map :title (:hand runner))))}
                {:req (req (> (:counter card 0) 0))
                 :counter-cost 1
                 :label "Hosted power counter: Reveal all cards in Grip and trash 1 card"
                 :msg (msg "look at all cards in Grip and trash " (:title target)
                           " using 1 power counter")
                 :choices (req (:hand runner))
                 :prompt "Choose a card to trash"
                 :effect (effect (trash target))}
                (trace-ability 3 add-power-counter)]}

   "Snowflake"
   {:abilities [(do-psi end-the-run)]}

   "Special Offer"
   {:abilities [{:label "Gain 5 [Credits] and trash Special Offer"
                 :effect (req (gain state :corp :credit 5)
                              (when current-ice
                                (trash-ice-in-run state))
                              (trash state side card)
                              (system-msg state side (str "gains 5 [Credits] and trashes Special Offer")))}]}

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
   {:effect take-bad-pub
    :advanceable :always
    :abilities [trash-program]}

   "Swordsman"
   {:abilities [(do-net-damage 1)
                {:prompt "Choose an AI program to trash"
                 :msg (msg "trash " (:title target))
                 :label "Trash an AI program"
                 :effect (effect (trash target))
                 :choices {:req #(and (installed? %)
                                      (is-type? % "Program")
                                      (has-subtype? % "AI"))}}]}

   "Taurus"
   (constellation-ice trash-hardware)

   "TMI"
   {:trace {:base 2
            :msg "keep TMI rezzed"
            :unsuccessful {:effect (effect (derez card))}}
    :abilities [end-the-run]}

   "Tollbooth"
   {:abilities [{:msg "make the Runner pay 3 [Credits], if able"
                 :effect (effect (pay :runner card :credit 3))}
                end-the-run]}

   "Tour Guide"
   {:abilities [end-the-run]}

   "Troll"
   {:abilities [(trace-ability 2 {:label "Force the Runner to lose [Click] or end the run"
                                  :msg "force the Runner to lose [Click] or end the run"
                                  :player :runner
                                  :prompt "Choose one"
                                  :choices ["Lose [Click]" "End the run"]
                                  :effect (req (if-not (and (= target "Lose [Click]")
                                                            (pay state side card :click 1))
                                                 (do (end-run state side)
                                                     (system-msg state side "ends the run"))
                                                 (system-msg state side "loses [Click]")))})]}

   "Tsurugi"
   {:abilities [end-the-run
                (do-net-damage 1)]}

   "Turing"
   {:abilities [end-the-run]
    :strength-bonus (req (if (is-remote? (second (:zone card))) 3 0))}

   "Turnpike"
   {:abilities [{:msg "force the Runner to lose 1 [Credits]"
                 :effect (effect (lose :runner :credit 1))}
                (tag-trace 5)]}

   "Tyrant"
   {:advanceable :while-rezzed
    :abilities [end-the-run]}

   "Universal Connectivity Fee"
   {:abilities [{:msg (msg "force the Runner to lose " (if (pos? (:tag runner)) "all credits" "1 [Credits]"))
                 :effect (req (if (pos? (get-in @state [:runner :tag]))
                                (do (lose state :runner :credit :all) (trash state side card))
                                (lose state :runner :credit 1)))}]}

   "Uroboros"
   {:abilities [(trace-ability 4 {:label "Prevent the Runner from making another run"
                                  :msg "prevent the Runner from making another run"
                                  :effect (effect (prevent-run))})
                (trace-ability 4 end-the-run)]}

   "Vanilla"
   {:abilities [end-the-run]}

   "Vikram 1.0"
   {:abilities [{:msg "prevent the Runner from using programs for the remainder of this run"}
                (trace-ability 4 (do-brain-damage 1))]}

   "Viktor 1.0"
   {:abilities [(do-brain-damage 1)
                end-the-run]}

   "Viktor 2.0"
   {:abilities [(power-counter-ability (do-brain-damage 1))
                (trace-ability 2 add-power-counter)
                end-the-run]}

   "Viper"
   {:abilities [(trace-ability 3 {:label "The Runner loses 1 [Click] if able"
                                  :msg "force the Runner to lose 1 [Click] if able"
                                  :effect (effect (lose :runner :click 1))})
                (trace-ability 3 end-the-run)]}

   "Virgo"
   (constellation-ice give-tag)

   "Waiver"
   {:abilities [(trace-ability 5 {:label "Reveal the Runner's Grip and trash cards"
                                  :msg (msg "reveal all cards in the Runner's Grip: " (join ", " (map :title (:hand runner)))
                                            ". Cards with a play/install cost less than or equal to " (- target (second targets))
                                            " will be trashed")
                                  :effect (req (let [delta (- target (second targets))]
                                                 (doseq [c (:hand runner)]
                                                   (when (<= (:cost c) delta)
                                                     (resolve-ability
                                                       state side
                                                       {:msg (msg "trash " (:title c))
                                                        :effect (effect (trash c))}
                                                      card nil)))))})]}

   "Wall of Static"
   {:abilities [end-the-run]}

   "Wall of Thorns"
   {:abilities [end-the-run
                (do-net-damage 2)]}

   "Wendigo"
   (morph-ice "Code Gate" "Barrier" {:msg "prevent the Runner from using a chosen program for the remainder of this run"})

   "Whirlpool"
   {:abilities [{:msg "prevent the Runner from jacking out"
                 :effect (req (when (and (is-remote? (second (:zone card)))
                                         (> (count (concat (:ices (card->server state card))
                                                           (:content (card->server state card)))) 1))
                                (prevent-jack-out state side))
                              (when current-ice
                                (trash-ice-in-run state))
                              (trash state side card))}]}

   "Woodcutter"
   {:advanceable :while-rezzed
    :abilities [(do-net-damage 1)]}

   "Wormhole"
   ;; TODO: create an ability for wormhole
   (space-ice)

   "Wotan"
   {:abilities [end-the-run]}

   "Wraparound"
   {:abilities [end-the-run]
    :strength-bonus (req (if (some #(has-subtype? % "Fracter") (all-installed state :runner))
                           0 7))
    :events (let [wr {:req (req (and (not= (:cid target) (:cid card))
                                     (has-subtype? target "Fracter")))
                      :effect (effect (update-ice-strength card))}]
              {:runner-install wr :trash wr :card-moved wr})}

   "Yagura"
   {:abilities [(do-net-damage 1)
                {:msg "look at the top card of R&D"
                 :optional {:prompt (msg "Move " (:title (first (:deck corp))) " to the bottom of R&D?")

                            :yes-ability {:effect (effect (move (first (:deck corp)) :deck)
                                                          (do (system-msg state side "uses Yagura to move the top card of R&D to the bottom")))}}}]}

   "Zed 1.0"
   {:abilities [(do-brain-damage 1)]}})
