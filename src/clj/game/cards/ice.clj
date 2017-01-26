(in-ns 'game.core)

(declare trash-program trash-hardware trash-installed)

;;;; Helper functions specific for ICE

;;; Runner abilites for breaking subs
(defn runner-break
  "Ability to break a subroutine by spending a resource (bioroids, Negotiator, Turing etc)"
  [cost subs]
  (let [cost-str (build-cost-str [cost])
        subs-str (str subs " subroutine" (when (< 1 subs) "s"))]
    {:cost cost
     :label (str "break " subs-str)
     :effect (req (system-msg state :runner (str "spends " cost-str " to break " subs-str " on " (:title card))))}))

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
   :delayed-completion true
   :effect (effect (tag-runner :runner eid 1))})

(def add-power-counter
  "Adds 1 power counter to the card."
  {:label "Add 1 power counter"
   :msg "add 1 power counter"
   :effect (effect (add-counter card :power 1))})

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
   :delayed-completion true
   :msg (str "do " dmg " net damage")
   :effect (effect (damage eid :net dmg {:card card}))})

(defn do-brain-damage
  "Do specified amount of brain damage."
  [dmg]
  {:label (str "Do " dmg " brain damage")
   :delayed-completion true
   :msg (str "do " dmg " brain damage")
   :effect (effect (damage eid :brain dmg {:card card}))})

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
                 :counter-cost [:power 1]))

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
  (req (+ (:advance-counter card 0) (:extra-advance-counter card 0))))

(def space-ice-rez-bonus
  "Amount of rez reduction for the Space ICE."
  (req (* -3 (+ (:advance-counter card 0) (:extra-advance-counter card 0)))))

(defn space-ice
  "Creates data for Space ICE with specified abilities."
  [& abilities]
  {:advanceable :always
   :subroutines (vec abilities)
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
   :msg (let [sub-label #(:label (first (:subroutines (card-def %))))]
          (msg "reveal " (join ", " (map #(str (:title %) " (" (sub-label %) ")") targets))))})

(def resolve-grail
  "Ability for resolving a subroutine on a Grail ICE in HQ."
  {:label "Resolve a Grail ICE subroutine from HQ"
   :choices {:req grail-in-hand}
   :effect (req (doseq [ice targets]
                  (let [subroutine (first (:subroutines (card-def ice)))]
                    (resolve-ability state side subroutine card nil))))})

(defn grail-ice
  "Creates data for grail ICE"
  [ability]
  {:abilities [reveal-grail]
   :subroutines [ability resolve-grail]})


;;; For NEXT ICE
(defn next-ice-count
  "Counts number of rezzed NEXT ICE - for use with NEXT Bronze and NEXT Gold"
  [corp]
  (let [servers (flatten (seq (:servers corp)))
        rezzed-next? #(and (rezzed? %) (has-subtype? % "NEXT"))]
    (reduce (fn [c server] (+ c (count (filter rezzed-next? (:ices server))))) 0 servers)))


;;; For Morph ICE
(defn morph [state side card new old]
  (update! state side (assoc card
                        :subtype-target new
                        :subtype (combine-subtypes true
                                                   (remove-subtypes (:subtype card) old)
                                                   new)))
  (update-ice-strength state side card))

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
     :subroutines [ability]
     :events {:advance ab :advancement-placed ab}}))


;;; For Constellation ICE
(defn constellation-ice
  "Generates map for Constellation ICE with specified effect."
  [ability]
  {:subroutines [(trace-ability 2 (assoc ability :kicker (assoc ability :min 5)))]})

;;; Helper function for adding implementation notes to ICE defined with functions
(defn- implementation-note [note ice-def]
  "Adds an implementation note to the ice-definition"
  (assoc ice-def :implementation note))


;;;; Card definitions
(def cards-ice
  {"Aiki"
   {:subroutines [(do-psi {:label "Runner draws 2 cards"
                           :msg "make the Runner draw 2 cards"
                           :effect (effect (draw :runner 2))})
                  (do-net-damage 1)]}

   "Archangel"
   {:access
    {:delayed-completion true
     :req (req (not= (first (:zone card)) :discard))
     :effect (effect (show-wait-prompt :runner "Corp to decide to trigger Archangel")
                     (continue-ability
                       {:optional
                        {:prompt "Pay 3 [Credits] to force Runner to encounter Archangel?"
                         :yes-ability {:cost [:credit 3]
                                       :delayed-completion true
                                       :effect (effect (system-msg :corp "pays 3 [Credits] to force the Runner to encounter Archangel")
                                                       (clear-wait-prompt :runner)
                                                       (continue-ability
                                                         :runner {:optional
                                                                  {:player :runner
                                                                   :prompt "You are encountering Archangel. Allow its subroutine to fire?"
                                                                   :priority 1
                                                                   :yes-ability {:delayed-completion true
                                                                                 :effect (effect (play-subroutine eid {:card card :subroutine 0}))}
                                                                   :no-ability {:effect (effect (effect-completed eid))}}}
                                                         card nil))}
                         :no-ability {:effect (effect (system-msg :corp "declines to force the Runner to encounter Archangel")
                                                      (clear-wait-prompt :runner))}}}
                       card nil))}
   :subroutines [(trace-ability 6 {:delayed-completion true
                                   :effect (effect (show-wait-prompt :runner "Corp to select Archangel target")
                                                   (continue-ability {:choices {:req #(and (installed? %)
                                                                                           (card-is? % :side :runner))}
                                                                      :label "Add 1 installed card to the Runner's Grip"
                                                                      :msg "add 1 installed card to the Runner's Grip"
                                                                      :effect (effect (clear-wait-prompt :runner)
                                                                                      (move :runner target :hand true)
                                                                                      (system-msg (str "adds " (:title target)
                                                                                                       " to the Runner's Grip")))
                                                                      :cancel-effect (effect (clear-wait-prompt :runner)
                                                                                             (effect-completed eid))}
                                                                     card nil))})]}

   "Archer"
   {:additional-cost [:forfeit]
    :subroutines [(gain-credits 2)
                  trash-program
                  end-the-run]}

   "Architect"
   {:flags {:untrashable-while-rezzed true}
    :subroutines [{:msg "look at the top 5 cards of R&D"
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
   {:abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (count (:hand corp)) " subroutines")}]
    :subroutines [end-the-run]}

   "Assassin"
   {:subroutines [(trace-ability 5 (do-net-damage 3))
                  (trace-ability 4 trash-program)]}

   "Asteroid Belt"
   (space-ice end-the-run)

   "Bailiff"
   {:implementation "Gain credit is manual"
    :abilities [(gain-credits 1)]
    :subroutines [end-the-run]}

   "Bandwidth"
   {:subroutines [{:msg "give the Runner 1 tag"
                   :delayed-completion true
                   :effect (effect (tag-runner :runner eid 1)
                                   (register-events
                                     {:successful-run {:effect (effect (lose :runner :tag 1))
                                                       :msg "make the Runner lose 1 tag"}
                                      :run-ends {:effect (effect (unregister-events card))}}
                                     card))}]
    :events {:successful-run nil :run-ends nil}}

   "Bastion"
   {:subroutines [end-the-run]}

   "Brainstorm"
   {:abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (count (:hand runner)) " subroutines")}]
    :subroutines [(do-brain-damage 1)]}

   "Builder"
   {:abilities [{:label "Move Builder to the outermost position of any server"
                 :cost [:click 1] :prompt "Choose a server" :choices (req servers)
                 :msg (msg "move it to the outermost position of " target)
                 :effect (effect (move card (conj (server->zone state target) :ices)))}]
    :subroutines [{:label "Place 1 advancement token on an ICE that can be advanced protecting this server"
                   :msg (msg "place 1 advancement token on " (card-str state target))
                   :choices {:req #(and (ice? %)
                                        (can-be-advanced? %))}
                   :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]}

   "Bullfrog"
   {:subroutines [(do-psi {:label "Move Bullfrog to another server"
                           :player :corp
                           :prompt "Choose a server"
                           :choices (req servers)
                           :msg (msg "move it to the outermost position of " target)
                           :effect (req (let [dest (server->zone state target)]
                                          (swap! state update-in [:run]
                                                 #(assoc % :position (count (get-in corp (conj dest :ices)))
                                                           :server (rest dest))))
                                        (move state side card
                                              (conj (server->zone state target) :ices)))})]}

   "Bulwark"
   {:effect take-bad-pub
    :abilities [{:msg "gain 2 [Credits] if there is an installed AI"
                 :req (req (some #(has-subtype? % "AI") (all-installed state :runner)))
                 :effect (effect (gain :credit 2))}]
    :subroutines [(assoc trash-program :player :runner
                                       :msg "force the Runner to trash 1 program"
                                       :label "The Runner trashes 1 program")
                  {:msg "gain 2 [Credits] and end the run"
                   :effect (effect (gain :credit 2)
                                   (end-run))}]}


   "Burke Bugs"
   {:subroutines [(trace-ability 0 (assoc trash-program :not-distinct true
                                                        :player :runner
                                                        :msg "force the Runner to trash a program"
                                                        :label "Force the Runner to trash a program"))]}

   "Caduceus"
   {:subroutines [(trace-ability 3 (gain-credits 3))
                  (trace-ability 2 end-the-run)]}

   "Cell Portal"
   {:subroutines [{:msg "make the Runner approach the outermost ICE"
                   :effect (req (let [srv (first (:server run))
                                      n (count (get-in @state [:corp :servers srv :ices]))]
                                  (swap! state assoc-in [:run :position] n)
                                  (derez state side card)))}]}

   "Changeling"
   (morph-ice "Barrier" "Sentry" end-the-run)

   "Checkpoint"
   {:effect take-bad-pub
    :subroutines [(trace-ability 5 {:label "Do 3 meat damage when this run is successful"
                                    :msg "do 3 meat damage when this run is successful"
                                    :effect (effect (register-events
                                                      {:successful-run
                                                       {:delayed-completion true
                                                        :msg "do 3 meat damage"
                                                        :effect (effect (damage eid :meat 3 {:card card}))}
                                                       :run-ends {:effect (effect (unregister-events card))}}
                                                     card))})]
    :events {:successful-run nil :run-ends nil}}

   "Chetana"
   {:subroutines [{:msg "make each player gain 2 [Credits]" :effect (effect (gain :runner :credit 2)
                                                                            (gain :corp :credit 2))}
                  (do-psi {:label "Do 1 net damage for each card in the Runner's grip"
                           :effect (effect (damage eid :net (count (get-in @state [:runner :hand])) {:card card}))
                           :msg (msg (str "do " (count (get-in @state [:runner :hand])) " net damage"))})]}

   "Chimera"
   (let [turn-end-ability {:effect (effect (derez :corp card)
                                           (update! (assoc (get-card state card) :subtype "Mythic")))}]
     {:prompt "Choose one subtype"
      :choices ["Barrier" "Code Gate" "Sentry"]
      :msg (msg "make it gain " target " until the end of the turn")
      :effect (effect (update! (assoc card
                                 :subtype-target target
                                 :subtype (combine-subtypes true (:subtype card) target)))
                      (update-ice-strength card))
      :events {:runner-turn-ends turn-end-ability
               :corp-turn-ends turn-end-ability}
      :subroutines [end-the-run]})

   "Chiyashi"
   {:abilities [{:label "Trash the top 2 cards of the Runner's Stack"
                 :req (req (some #(has-subtype? % "AI") (all-installed state :runner)))
                 :effect (effect (mill :runner 2))}]
    :subroutines [(do-net-damage 2)
                  end-the-run]}

   "Chrysalis"
   {:subroutines [(do-net-damage 2)]
    :access {:delayed-completion true
             :req (req (not= (first (:zone card)) :discard))
             :effect (effect (show-wait-prompt :corp "Runner to decide to break Chrysalis subroutine")
                             (continue-ability
                               :runner {:optional
                                        {:player :runner
                                         :prompt "You are encountering Chrysalis. Allow its subroutine to fire?"
                                         :priority 1
                                         :yes-ability {:effect (effect (clear-wait-prompt :corp)
                                                                       (play-subroutine eid {:card card :subroutine 0}))}
                                         :no-ability {:effect (effect (clear-wait-prompt :corp)
                                                                      (effect-completed eid))}}}
                              card nil))}}

   "Chum"
   {:subroutines [{:label "Give +2 strength to next ICE Runner encounters"
                   :req (req this-server)
                   :prompt "Choose the ICE the Runner is encountering"
                   :choices {:req #(and (rezzed? %) (ice? %))}
                   :msg (msg "give " (:title target) " +2 strength")
                   :effect (req (let [ice (:cid target)]
                                  (register-events state side
                                    {:pre-ice-strength {:req (req (= (:cid target) ice))
                                                        :effect (effect (ice-strength-bonus 2 target))}
                                     :run-ends {:effect (effect (unregister-events card))}}
                                   card)
                                  (update-all-ice state side)))}
                  (do-net-damage 3)]
    :events {:pre-ice-strength nil :run-ends nil}}

   "Clairvoyant Monitor"
   {:subroutines [(do-psi {:label "Place 1 advancement token and end the run"
                           :player :corp
                           :prompt "Choose a target for Clairvoyant Monitor"
                           :msg (msg "place 1 advancement token on "
                                     (card-str state target) " and end the run")
                           :choices {:req installed?}
                           :effect (effect (add-prop target :advance-counter 1 {:placed true})
                                           (end-run))})]}

   "Cobra"
   {:subroutines [trash-program (do-net-damage 2)]}

   "Cortex Lock"
   {:subroutines [{:label "Do 1 net damage for each unused memory unit the Runner has"
                   :msg (msg "do " (:memory runner) " net damage")
                   :effect (effect (damage eid :net (:memory runner) {:card card}))}]}

   "Crick"
   {:subroutines [{:label "install a card from Archives"
                   :msg (msg (corp-install-msg target))
                   :prompt "Choose a card to install from Archives"
                   :show-discard true :priority true
                   :choices {:req #(and (not (is-type? % "Operation"))
                                        (= (:zone %) [:discard])
                                        (= (:side %) "Corp"))}
                   :effect (effect (corp-install target nil))}]
    :strength-bonus (req (if (= (second (:zone card)) :archives) 3 0))}

   "Curtain Wall"
   {:subroutines [end-the-run]
    :strength-bonus (req (let [ices (:ices (card->server state card))]
                           (if (= (:cid card) (:cid (last ices))) 4 0)))
    :events (let [cw {:req (req (and (not= (:cid card) (:cid target))
                                     (= (card->server state card) (card->server state target))))
                      :effect (effect (update-ice-strength card))}]
              {:corp-install cw :trash cw :card-moved cw})}

   "Data Hound"
   (letfn [(dh-trash [cards]
             {:prompt "Choose a card to trash"
              :choices cards
              :delayed-completion true
              :msg (msg "trash " (:title target))
              :effect (req (do (trash state side target {:unpreventable true})
                               (continue-ability state side (reorder-choice
                                                              :runner :runner (remove-once #(not= % target) cards)
                                                              '() (count (remove-once #(not= % target) cards))
                                                              (remove-once #(not= % target) cards)) card nil)))})]
     {:subroutines [(trace-ability 2 {:delayed-completion true
                                      :label "Look at the top of Stack"
                                      :msg "look at top X cards of Stack"
                                      :effect (req (show-wait-prompt state :runner "Corp to rearrange the top cards of the Runner's Stack")
                                                   (let [c (- target (second targets))
                                                         from (take c (:deck runner))]
                                                     (system-msg state :corp
                                                                 (str "looks at the top " c " cards of Stack"))
                                                     (if (< 1 c)
                                                       (continue-ability state side (dh-trash from) card nil)
                                                       (do (system-msg state :corp (str "trashes " (:title (first from))))
                                                           (trash state side (first from) {:unpreventable true})
                                                           (clear-wait-prompt state :runner)
                                                           (effect-completed state side eid card)))))})]})

   "Data Mine"
   {:subroutines [{:msg "do 1 net damage"
                   :effect (req (damage state :runner eid :net 1 {:card card})
                                (when current-ice
                                  (no-action state side nil)
                                  (continue state side nil))
                                (trash state side card))}]}

   "Datapike"
   {:subroutines [{:msg "force the Runner to pay 2 [Credits] if able"
                   :effect (effect (pay :runner card :credit 2))}
                  end-the-run]}

   "Data Raven"
   {:implementation "Encounter effect is manual"
    :abilities [give-tag
                (power-counter-ability give-tag)]
    :runner-abilities [{:label "End the run"
                        :effect (req (end-run state :runner)
                                     (system-msg state :runner "chooses to end the run on encountering Data Raven"))}
                       {:label "Take 1 tag"
                        :delayed-completion true
                        :effect (req (system-msg state :runner "chooses to take 1 tag on encountering Data Raven")
                                     (tag-runner state :runner eid 1))}]
    :subroutines [(trace-ability 3 add-power-counter)]}

   "Data Ward"
   {:runner-abilities [{:label "Pay 3 [Credits]"
                        :effect (req (pay state :runner card :credit 3)
                                     (system-msg state :runner "chooses to pay 3 [Credits] on encountering Data Ward"))}
                       {:label "Take 1 tag"
                        :delayed-completion true
                        :effect (req (system-msg state :runner "chooses to take 1 tag on encountering Data Ward")
                                     (tag-runner state :runner eid 1))}]
    :subroutines [{:label "End the run if the Runner is tagged"
                   :req (req tagged)
                   :msg "end the run"
                   :effect (effect (end-run))}]}

   "DNA Tracker"
   {:abilities [{:msg "do 1 net damage and make the Runner lose 2 [Credits]"
                 :effect (req (when-completed (damage state side :net 1 {:card card})
                                              (lose state :runner :credit 2)))}]}

   "Dracō"
   {:prompt "How many power counters?"
    :choices :credit
    :msg (msg "add " target " power counters")
    :effect (effect (add-counter card :power target)
                    (update-ice-strength card))
    :strength-bonus (req (get-in card [:counter :power] 0))
    :subroutines [(trace-ability 2 {:label "Give the Runner 1 tag and end the run"
                                    :msg "give the Runner 1 tag and end the run"
                                    :delayed-completion true
                                    :effect (effect (tag-runner :runner eid 1)
                                                    (end-run))})]}

   "Eli 1.0"
   {:subroutines [end-the-run]
    :runner-abilities [(runner-break [:click 1] 1)]}

   "Enforcer 1.0"
   {:additional-cost [:forfeit]
    :subroutines [trash-program
                  (do-brain-damage 1)
                  {:label "Trash a console" :effect (effect (trash target))
                   :prompt "Choose a console to trash" :msg (msg "trash " (:title target))
                   :choices {:req #(has-subtype? % "Console")}}
                  {:msg "trash all virtual resources"
                   :effect (req (doseq [c (filter #(has-subtype? % "Virtual") (all-installed state :runner))]
                                  (trash state side c)))}]
    :runner-abilities [(runner-break [:click 1] 1)]}

   "Enigma"
   {:subroutines [{:msg "force the Runner to lose 1 [Click] if able"
                   :effect (effect (lose :runner :click 1))}
                  end-the-run]}

   "Errand Boy"
   {:subroutines [(gain-credits 1)
                  {:msg "draw 1 card" :effect (effect (draw))}]}

   "Excalibur"
   {:subroutines [{:label "The Runner cannot make another run this turn"
                   :msg "prevent the Runner from making another run" :effect (effect (prevent-run))}]}

   "Fairchild"
   {:subroutines [end-the-run
                  (do-brain-damage 1)]
    :runner-abilities [(runner-break [:credit 4] 1)]}

   "Fairchild 1.0"
   {:subroutines [{:label "Force the Runner to pay 1 [Credits] or trash an installed card"
                   :msg "force the Runner to pay 1 [Credits] or trash an installed card"
                   :player :runner
                   :prompt "Choose one"
                   :choices ["Pay 1 [Credits]" "Trash an installed card"]
                   :effect (req (if (= target "Pay 1 [Credits]")
                                  (do (pay state side card :credit 1)
                                      (system-msg state side "pays 1 [Credits]"))
                                  (resolve-ability state :runner trash-installed card nil)))}]
    :runner-abilities [(runner-break [:click 1] 1)]}

   "Fairchild 2.0"
   {:subroutines [{:label "Force the Runner to pay 2 [Credits] or trash an installed card"
                   :msg "force the Runner to pay 2 [Credits] or trash an installed card"
                   :player :runner
                   :prompt "Choose one"
                   :choices ["Pay 2 [Credits]" "Trash an installed card"]
                   :effect (req (if (= target "Pay 2 [Credits]")
                                  (do (pay state side card :credit 2)
                                      (system-msg state side "pays 2 [Credits]"))
                                  (resolve-ability state :runner trash-installed card nil)))}
                  (do-brain-damage 1)]
    :runner-abilities [(runner-break [:click 2] 2)]}

   "Fairchild 3.0"
   {:subroutines [{:label "Force the Runner to pay 3 [Credits] or trash an installed card"
                   :msg "force the Runner to pay 3 [Credits] or trash an installed card"
                   :player :runner
                   :prompt "Choose one"
                   :choices ["Pay 3 [Credits]" "Trash an installed card"]
                   :effect (req (if (= target "Pay 3 [Credits]")
                                  (do (pay state side card :credit 3)
                                      (system-msg state side "pays 3 [Credits]"))
                                  (resolve-ability state :runner trash-installed card nil)))}
                  {:label "Do 1 brain damage or end the run"
                   :prompt "Choose one"
                   :choices ["Do 1 brain damage" "End the run"]
                   :msg (msg (lower-case target))
                   :effect (req (if (= target "Do 1 brain damage")
                                  (damage state side eid :brain 1 {:card card})
                                  (end-run state side)))}]
    :runner-abilities [(runner-break [:click 3] 3)]}

   "Fenris"
   {:effect take-bad-pub
    :subroutines [(do-brain-damage 1)
                  end-the-run]}

   "Fire Wall"
   {:advanceable :always
    :subroutines [end-the-run]
    :strength-bonus advance-counters}

   "Flare"
   {:subroutines [(trace-ability 6 {:label "Trash 1 hardware, do 2 meat damage, and end the run"
                                    :msg "trash 1 hardware, do 2 meat damage, and end the run"
                                    :delayed-completion true
                                    :effect (effect (continue-ability
                                                     {:prompt "Choose a piece of hardware to trash"
                                                      :label "Trash a piece of hardware"
                                                      :msg (msg "trash " (:title target))
                                                      :choices {:req #(is-type? % "Hardware")}
                                                      :effect (req (when-completed
                                                                     (trash state side target {:cause :subroutine})
                                                                     (do (damage state side eid :meat 2 {:unpreventable true
                                                                                              :card card})
                                                                         (end-run state side))))
                                                      :cancel-effect (effect (damage eid :meat 2 {:unpreventable true :card card})
                                                                             (end-run))}
                                                     card nil))})]}

   "Galahad"
   (grail-ice end-the-run)

   "Gemini"
   (constellation-ice (do-net-damage 1))

   "Grim"
   {:effect take-bad-pub
    :subroutines [trash-program]}

   "Guard"
   {:implementation "Prevent bypass is manual"
    :subroutines [end-the-run]}

   "Gutenberg"
   {:subroutines [(tag-trace 7)]
    :strength-bonus (req (if (= (second (:zone card)) :rd) 3 0))}

   "Gyri Labyrinth"
   {:implementation "Hand size is not restored if trashed or derezzed after firing"
    :subroutines [{:req (req (:run @state))
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
    :subroutines [end-the-run]
    :strength-bonus advance-counters}

   "Harvester"
   {:subroutines [{:label "Runner draws 3 cards and discards down to maximum hand size"
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
   {:abilities [{:msg "add it to HQ"
                :cost [:credit 1]
                :effect (effect (move card :hand))}]
    :subroutines [end-the-run]}

   "Hive"
   {:abilities [{:label "Gain subroutines"
                 :msg   (msg "gain " (min 5 (max 0 (- 5 (:agenda-point corp 0)))) " subroutines")}]
    :subroutines [end-the-run]}

   "Heimdall 1.0"
   {:subroutines [(do-brain-damage 1)
                  end-the-run]
    :runner-abilities [(runner-break [:click 1] 1)]}

   "Heimdall 2.0"
   {:subroutines [(do-brain-damage 1)
                  {:msg "do 1 brain damage and end the run" :effect (effect (damage eid :brain 1 {:card card}) (end-run))}
                  end-the-run]
    :runner-abilities [(runner-break [:click 2] 2)]}

   "Herald"
   {:subroutines [(gain-credits 2)
                  {:label "Pay 1 [Credits] to place 1 advancement token on a card that can be advanced"
                   :msg (msg "place 1 advancement token on " (card-str state target))
                   :choices {:req can-be-advanced?}
                   :cost [:credit 1] :effect (effect (add-prop target :advance-counter 1))}]
    :access {:delayed-completion true
             :req (req (not= (first (:zone card)) :discard))
             :effect (effect (show-wait-prompt :corp "Runner to decide to break Herald subroutines")
                             (continue-ability
                               :runner {:optional
                                        {:player :runner
                                         :prompt "You are encountering Heralds. Allow its subroutines to fire?"
                                         :priority 1
                                         :yes-ability {:effect (effect (clear-wait-prompt :corp)
                                                                       (play-subroutine eid {:card card :subroutine 0}))}
                                         :no-ability {:effect (effect (clear-wait-prompt :corp)
                                                                      (effect-completed eid))}}}
                              card nil))}}

   "Hourglass"
   {:subroutines [{:msg "force the Runner to lose 1 [Click] if able"
                   :effect (effect (lose :runner :click 1))}]}

   "Howler"
   (let [ice-index (fn [state i] (first (keep-indexed #(when (= (:cid %2) (:cid i)) %1)
                                                      (get-in @state (cons :corp (:zone i))))))]
     {:subroutines
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
                                                        (card-init state side newice false)
                                                        (trigger-event state side :corp-install newice)))} card nil)))}]
      :events {:run-ends {:req (req (:howler-target card))
                          :effect (effect (trash card {:cause :self-trash})
                                          (derez (get-card state (:howler-target card))))}}})

   "Hudson 1.0"
   {:subroutines [{:msg "prevent the Runner from accessing more than 1 card during this run"
                   :effect (effect (max-access 1))}]
    :runner-abilities [(runner-break [:click 1] 1)]}

   "Hunter"
   {:subroutines [(tag-trace 3)]}

   "Ice Wall"
   {:advanceable :always
    :subroutines [end-the-run]
    :strength-bonus advance-counters}

   "Ichi 1.0"
   {:subroutines [trash-program
                  (trace-ability 1 {:label "Give the Runner 1 tag and do 1 brain damage"
                                    :msg "give the Runner 1 tag and do 1 brain damage"
                                    :delayed-completion true
                                    :effect (req (when-completed (damage state :runner :brain 1 {:card card})
                                                                 (tag-runner state :runner eid 1)))})]
    :runner-abilities [(runner-break [:click 1] 1)]}

   "Ichi 2.0"
   {:subroutines [trash-program
                  (trace-ability 3 {:label "Give the Runner 1 tag and do 1 brain damage"
                                    :msg "give the Runner 1 tag and do 1 brain damage"
                                    :delayed-completion true
                                    :effect (req (when-completed (damage state :runner :brain 1 {:card card})
                                                                 (tag-runner state :runner eid 1)))})]
    :runner-abilities [(runner-break [:click 2] 2)]}

   "Information Overload"
   {:implementation "Encounter effect is manual"
    :abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (:tag runner 0) " subroutines")}
                (tag-trace 1)]
    :subroutines [trash-installed]}

   "IP Block"
   {:abilities [(assoc give-tag :req (req (not-empty (filter #(has-subtype? % "AI") (all-installed state :runner))))
                                :label "Give the Runner 1 tag if there is an installed AI")
                (tag-trace 3)
                {:label "End the run if the Runner is tagged"
                 :req (req tagged)
                 :msg "end the run"
                 :effect (effect (end-run))}]}

   "IQ"
   {:effect (req (add-watch state (keyword (str "iq" (:cid card)))
                            (fn [k ref old new]
                              (let [handsize (count (get-in new [:corp :hand]))]
                                (when (not= (count (get-in old [:corp :hand])) handsize)
                                  (update! ref side (assoc (get-card ref card) :strength-bonus handsize))
                                  (update-ice-strength ref side (get-card ref card)))))))
    :subroutines [end-the-run]
    :strength-bonus (req (count (:hand corp)))
    :rez-cost-bonus (req (count (:hand corp)))
    :leave-play (req (remove-watch state (keyword (str "iq" (:cid card)))))}

   "Ireress"
   {:abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (:bad-publicity corp 0) " subroutines")}]
    :subroutines [{:msg "make the Runner lose 1 [Credits]"
                   :effect (effect (lose :runner :credit 1))}]}

   "Its a Trap!"
   {:expose {:msg "do 2 net damage"
             :delayed-completion true
             :effect (effect (damage eid :net 2 {:card card}))}
    :subroutines [(assoc trash-installed :effect (req (trash state side target {:cause :subroutine})
                                                      (when current-ice
                                                        (no-action state side nil)
                                                        (continue state side nil))
                                                      (trash state side card)))]}

   "Janus 1.0"
   {:subroutines [(do-brain-damage 1)]
    :runner-abilities [(runner-break [:click 1] 1)]}

   "Kitsune"
   {:subroutines [{:prompt "Choose a card in HQ to force access"
                   :choices {:req in-hand?}
                   :label "Force the Runner to access a card in HQ"
                   :msg (msg "force the Runner to access " (:title target))
                   :effect (effect (handle-access targets) (trash card))}]}

   "Komainu"
   {:abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (count (:hand runner)) " subroutines")}]
    :subroutines [(do-net-damage 1)]}

   "Lab Dog"
   {:subroutines [(assoc trash-hardware :label "Force the Runner to trash an installed piece of hardware"
                                        :player :runner
                                        :msg (msg "force the Runner to trash " (:title target))
                                        :effect (req (trash state side target)
                                                     (when current-ice
                                                       (no-action state side nil)
                                                       (continue state side nil))
                                                     (trash state side card)))]}

   "Lancelot"
   (grail-ice trash-program)

   "Little Engine"
   {:subroutines [end-the-run
                  {:msg "make the Runner gain 5 [Credits]" :effect (effect (gain :runner :credit 5))}]}

   "Lockdown"
   {:subroutines [{:label "The Runner cannot draw cards for the remainder of this turn"
                   :msg "prevent the Runner from drawing cards" :effect (effect (prevent-draw))}]}

   "Lotus Field"
   {:subroutines [end-the-run]
    :flags {:cannot-lower-strength true}}

   "Lycan"
   (morph-ice "Sentry" "Code Gate" trash-program)

   "Macrophage"
   {:subroutines [(trace-ability 4 {:label "Purge virus counters"
                                    :msg "purge virus counters"
                                    :effect (effect (purge))})
                  (trace-ability 3 {:label "Trash a virus"
                                    :prompt "Choose a virus to trash"
                                    :msg (msg "trash " (:title target))
                                    :choices {:req #(and (installed? %)
                                                         (has? % :subtype "Virus"))}
                                    :effect (effect (trash target {:cause :subroutine})
                                                    (clear-wait-prompt :runner))})
                  (trace-ability 2 {:label "Remove a virus in the Heap from the game"
                                    :prompt "Choose a virus in the Heap to remove from the game"
                                    :choices (req (cancellable (filter #(has? % :subtype "Virus") (:discard runner)) :sorted))
                                    :msg (msg "remove " (:title target) " from the game")
                                    :effect (effect (move :runner target :rfg))})
                  (trace-ability 1 end-the-run)]}

   "Magnet"
   {:delayed-completion true
    :effect (req (let [magnet card]
                   (continue-ability
                     state side
                     {:req (req (some #(some (fn [h] (card-is? h :type "Program")) (:hosted %))
                                      (remove-once #(not= (:cid %) (:cid magnet)) (all-installed state corp))))
                      :prompt "Select a Program to host on Magnet"
                      :choices {:req #(and (card-is? % :type "Program")
                                           (ice? (:host %))
                                           (not= (:cid (:host %)) (:cid magnet)))}
                      :effect (req (let [hosted (host state side card target)]
                                     (unregister-events state side hosted)
                                     (update! state side (dissoc hosted :abilities))))}
                     card nil)))
    :events {:runner-install {:req (req (= (:cid card) (:cid (:host target))))
                              :effect (req (doseq [c (get-in card [:hosted])]
                                             (unregister-events state side c)
                                             (update! state side (dissoc c :abilities)))
                                           (update-ice-strength state side card))}}
    :subroutines [end-the-run]}

   "Mamba"
   {:abilities [(power-counter-ability (do-net-damage 1))]
    :subroutines [(do-net-damage 1)
                  (do-psi add-power-counter)]}

   "Markus 1.0"
   {:subroutines [trash-installed end-the-run]
    :runner-abilities [(runner-break [:click 1] 1)]}

   "Matrix Analyzer"
   {:implementation "Encounter effect is manual"
    :abilities [{:label "Place 1 advancement token on a card that can be advanced"
                 :msg (msg "place 1 advancement token on " (card-str state target))
                 :choices {:req can-be-advanced?}
                 :cost [:credit 1] :effect (effect (add-prop target :advance-counter 1))}]
    :subroutines [(tag-trace 2)]}

   "Mausolus"
   {:advanceable :always
    :subroutines [{:label "Gain 1 [Credits] (Gain 3 [Credits])"
                   :msg (msg "gain " (if (> 3 (+ (:advance-counter card 0) (:extra-advance-counter card 0))) 1 3) " [Credits]")
                   :effect (effect (gain :credit (if (> 3 (+ (:advance-counter card 0) (:extra-advance-counter card 0))) 1 3)))}
                  {:label "Do 1 net damage (Do 3 net damage)"
                   :delayed-completion true
                   :msg (msg "do " (if (> 3 (+ (:advance-counter card 0) (:extra-advance-counter card 0))) 1 3) " net damage")
                   :effect (effect (damage eid :net (if (> 3 (+ (:advance-counter card 0) (:extra-advance-counter card 0))) 1 3) {:card card}))}
                  {:label "Give the Runner 1 tag (and end the run)"
                   :delayed-completion true
                   :msg (msg "give the Runner 1 tag"
                             (when (<= 3 (+ (:advance-counter card 0) (:extra-advance-counter card 0))) " and end the run"))
                   :effect (req (tag-runner state :runner eid 1)
                                (when (<= 3 (+ (:advance-counter card 0) (:extra-advance-counter card 0)))
                                  (end-run state side)))}]}

   "Merlin"
   (grail-ice (do-net-damage 2))

   "Meru Mati"
   {:subroutines [end-the-run]
    :strength-bonus (req (if (= (second (:zone card)) :hq) 3 0))}

   "Mind Game"
   {:subroutines [(do-psi {:label "Redirect the run to another server"
                           :player :corp
                           :prompt "Choose a server"
                           :choices (req servers)
                           :msg (msg "redirect the run to " target)
                           :effect (req (let [dest (server->zone state target)]
                                          (swap! state update-in [:run]
                                                 #(assoc % :position (count (get-in corp (conj dest :ices)))
                                                           :server (rest dest)))))})]
    :runner-abilities [{:label "Add an installed card to the bottom of your Stack"
                        :prompt "Choose one of your installed cards"
                        :choices {:req #(and (installed? %)
                                             (= (:side %) "Runner"))}
                        :effect (effect (move target :deck)
                                        (system-msg :runner (str "adds " (:title target) " to the bottom of their Stack")))}]}

   "Minelayer"
   {:subroutines [{:msg "install an ICE from HQ"
                   :choices {:req #(and (ice? %)
                                        (in-hand? %))}
                   :prompt "Choose an ICE to install from HQ"
                   :effect (req (corp-install state side target (zone->name (first (:server run))) {:no-install-cost true}))}]}

   "Mother Goddess"
   (let [ab (effect (update! (let [subtype (->> (mapcat :ices (flatten (seq (:servers corp))))
                                                (filter #(and (rezzed? %) (not= (:cid card) (:cid %))))
                                                (mapcat #(split (:subtype %) #" - "))
                                                (cons "Mythic")
                                                distinct
                                                (join " - "))]
                               (assoc card :subtype-target (remove-subtypes subtype "Mythic")
                                           :subtype subtype))))
         mg {:req (req (ice? target))
             :effect ab}]
     {:effect ab
      :subroutines [end-the-run]
      :events {:rez mg :card-moved mg :derez mg}})

   "Muckraker"
   {:effect take-bad-pub
    :subroutines [(tag-trace 1)
                  (tag-trace 2)
                  (tag-trace 3)
                  {:msg "end the run if the Runner is tagged" :req (req tagged)
                   :effect (effect (end-run))}]}

   "Nebula"
   (space-ice trash-program)

   "Negotiator"
   {:subroutines [(gain-credits 2)
                  trash-program]
    :runner-abilities [(runner-break [:credit 2] 1)]}

   "Neural Katana"
   {:subroutines [(do-net-damage 3)]}

   "News Hound"
   {:subroutines [(tag-trace 3)
                  {:label "End the run if a Current is active"
                   :req (req (or (not (empty? (runner :current)))
                                 (not (empty? (corp :current)))))
                   :effect (effect (end-run)) :msg "end the run"}]}

   "NEXT Bronze"
   {:subroutines [end-the-run]
    :strength-bonus (req (next-ice-count corp))
    :events (let [nb {:req (req (and (not= (:cid target) (:cid card))
                                     (has-subtype? target "NEXT")))
                      :effect (effect (update-ice-strength card))}]
              {:rez nb :derez nb :trash nb :card-moved nb})}

   "NEXT Gold"
   {:subroutines [{:label "Do 1 net damage for each rezzed NEXT ice"
                   :msg (msg "do " (next-ice-count corp) " net damage")
                   :effect (effect (damage eid :net (next-ice-count corp) {:card card}))}
                  trash-program]}

   "NEXT Silver"
   {:abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (count (filter #(and (is-type? % "ICE")
                                                        (has-subtype? % "NEXT")
                                                        (rezzed? %))
                                                  (all-installed state :corp))) " subroutines")}]
    :subroutines [end-the-run]}

   "Orion"
   ;; TODO: wormhole subroutine
   (implementation-note "\"Resolve a subroutine...\" subroutine is not implemented"
                        (space-ice trash-program end-the-run))

   "Pachinko"
   {:subroutines [{:label "End the run if the Runner is tagged"
                   :req (req tagged)
                   :msg "end the run"
                   :effect (effect (end-run))}]}

   "Paper Wall"
   {:implementation "Trash on break is manual"
    :subroutines [end-the-run]}

   "Pop-up Window"
   {:implementation "Encounter effect is manual. Runner choice is not implemented"
    :abilities [(gain-credits 1)]
    :subroutines [end-the-run]
    :runner-abilities [(runner-break [:credit 1] 1)]}

   "Pup"
   {:subroutines [(do-net-damage 1)]
    :runner-abilities [(runner-break [:credit 1] 1)]}

   "Quandary"
   {:subroutines [end-the-run]}

   "Quicksand"
   {:implementation "Encounter effect is manual"
    :abilities [{:req (req (and this-server (= (dec (:position run)) (ice-index state card))))
                 :label "Add 1 power counter"
                 :effect (effect (add-counter card :power 1)
                                 (update-all-ice))}]
    :subroutines [end-the-run]
    :strength-bonus (req (get-in card [:counter :power] 0))}

   "Rainbow"
   {:subroutines [end-the-run]}

   "Ravana 1.0"
   {:subroutines [{:label "Resolve a subroutine on another piece of rezzed bioroid ICE"
                   :choices {:req #(and (rezzed? %) (ice? %) (has-subtype? % "Bioroid"))}
                   :msg (msg "resolve a subroutine on " (:title target))}]
    :runner-abilities [(runner-break [:click 1] 1)]}

   "Red Tape"
   {:subroutines [{:label "Give +3 strength to all ICE for the remainder of the run"
                   :msg "give +3 strength to all ICE for the remainder of the run"
                   :effect (effect (register-events
                                     {:pre-ice-strength {:effect (effect (ice-strength-bonus 3 target))}
                                      :run-ends {:effect (effect (unregister-events card))}}
                                     card)
                                   (update-all-ice))}]
    :events {:pre-ice-strength nil :run-ends nil}}

   "Resistor"
   {:effect (req (add-watch state (keyword (str "resistor" (:cid card)))
                            (fn [k ref old new]
                              (let [tags (get-in new [:runner :tag])]
                                (when (not= (get-in old [:runner :tag]) tags)
                                  (update! ref side (assoc (get-card ref card) :strength-bonus tags))
                                  (update-ice-strength ref side (get-card ref card)))))))
    :strength-bonus (req (:tag runner))
    :leave-play (req (remove-watch state (keyword (str "resistor" (:cid card)))))
    :subroutines [(trace-ability 4 end-the-run)]}

   "Rototurret"
   {:subroutines [trash-program end-the-run]}

   "Sagittarius"
   (constellation-ice trash-program)

   "Salvage"
   {:advanceable :while-rezzed
    :abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (:advance-counter card 0) " subroutines")}]
    :subroutines [(tag-trace 2)]}

   "Sapper"
   {:subroutines [trash-program]
    :access {:delayed-completion true
             :req (req (and (not= (first (:zone card)) :discard)
                            (some #(is-type? % "Program") (all-installed state :runner))))
             :effect (effect (show-wait-prompt :corp "Runner to decide to break Sapper subroutine")
                             (continue-ability
                               :runner {:optional
                                        {:player :runner
                                         :prompt "Allow Sapper subroutine to fire?"
                                         :priority 1
                                         :yes-ability {:effect (req (clear-wait-prompt state :corp)
                                                                    (show-wait-prompt state :runner "Corp to trash a program with Sapper")
                                                                    (play-subroutine state :corp eid {:card card :subroutine 0}))}
                                         :no-ability {:effect (effect (clear-wait-prompt :corp)
                                                                      (effect-completed eid))}}}
                              card nil))}}

   "Searchlight"
   {:advanceable :always
    ;; Could replace this with (tag-trace advance-counters).
    :subroutines [{:label "Trace X - Give the Runner 1 tag"
                   :trace {:base advance-counters
                           :delayed-completion true
                           :effect (effect (tag-runner :runner eid 1))
                           :msg "give the Runner 1 tag"}}]}

   "Sensei"
   {:subroutines [{:label "Give each other ICE encountered \"End the run\" for the remainder of the run"
                   :msg (msg "give each other ICE encountered \"[Subroutine] End the run\" after all its other subroutines for the remainder of the run")}]}

   "Shadow"
   {:advanceable :always
    :subroutines [(gain-credits 2)
                  (tag-trace 3)]
    :strength-bonus advance-counters}

   "Sherlock 1.0"
   {:subroutines [{:label "Trace 4 - Add an installed program to the top of the Runner's Stack"
                   :trace {:base 4
                           :choices {:req #(and (installed? %)
                                                (is-type? % "Program"))}
                           :msg (msg "add " (:title target) " to the top of the Runner's Stack")
                           :effect (effect (move :runner target :deck {:front true}))}}]
    :runner-abilities [(runner-break [:click 1] 1)]}

   "Sherlock 2.0"
   {:subroutines [{:label "Trace 4 - Add an installed program to the bottom of the Runner's Stack"
                   :trace {:base 4
                           :choices {:req #(and (installed? %)
                                                (is-type? % "Program"))}
                           :msg     (msg "add " (:title target) " to the bottom of the Runner's Stack")
                           :effect  (effect (move :runner target :deck))}}
                  {:label  "Give the Runner 1 tag"
                   :msg    "give the Runner 1 tag"
                   :delayed-completion true
                   :effect (effect (tag-runner :runner eid 1))}]
    :runner-abilities [(runner-break [:click 2] 2)]}

   "Shinobi"
   {:effect take-bad-pub
    :subroutines [(trace-ability 1 (do-net-damage 1))
                  (trace-ability 2 (do-net-damage 2))
                  (trace-ability 3 {:label "Do 3 net damage and end the run"
                                    :msg "do 3 net damage and end the run"
                                    :effect (effect (damage eid :net 3 {:card card}) (end-run))})]}

   "Shiro"
   {:subroutines [{:label "Rearrange the top 3 cards of R&D"
                   :msg "rearrange the top 3 cards of R&D"
                   :delayed-completion true
                   :effect (req (show-wait-prompt state :runner "Corp to rearrange the top cards of R&D")
                                (let [from (take 3 (:deck corp))]
                                  (if (pos? (count from))
                                    (continue-ability state side (reorder-choice :corp :runner from '()
                                                                                 (count from) from) card nil)
                                    (do (clear-wait-prompt state :runner)
                                        (effect-completed state side eid card)))))}
                  {:label "Force the Runner to access the top card of R&D"
                   :effect (req (doseq [c (take (get-in @state [:runner :rd-access]) (:deck corp))]
                                  (system-msg state :runner (str "accesses " (:title c)))
                                  (handle-access state side [c])))}]}

   "Snoop"
   {:implementation "Encounter effect is manual"
    :abilities [{:req (req (= current-ice card))
                 :label "Reveal all cards in the Runner's Grip"
                 :msg (msg "reveal the Runner's Grip ( " (join ", " (map :title (:hand runner))) " )")}
                {:req (req (> (get-in card [:counter :power] 0) 0))
                 :counter-cost [:power 1]
                 :label "Hosted power counter: Reveal all cards in Grip and trash 1 card"
                 :msg (msg "look at all cards in Grip and trash " (:title target)
                           " using 1 power counter")
                 :choices (req (cancellable (:hand runner) :sorted))
                 :prompt "Choose a card to trash"
                 :effect (effect (trash target))}]
    :subroutines [(trace-ability 3 add-power-counter)]}

   "Snowflake"
   {:subroutines [(do-psi end-the-run)]}

   "Special Offer"
   {:subroutines [{:label "Gain 5 [Credits] and trash Special Offer"
                   :effect (req (gain state :corp :credit 5)
                                (when current-ice
                                  (no-action state side nil)
                                  (continue state side nil))
                                (trash state side card)
                                (system-msg state side (str "gains 5 [Credits] and trashes Special Offer")))}]}

   "Spiderweb"
   {:subroutines [end-the-run]}

   "Susanoo-no-Mikoto"
   {:subroutines [{:req (req (not= (:server run) [:discard]))
                   :msg "make the Runner continue the run on Archives"
                   :effect (req (swap! state update-in [:run]
                                       #(assoc % :position (count (get-in corp [:servers :archives :ices]))
                                                 :server [:archives])))}]}

   "Swarm"
   {:effect take-bad-pub
    :advanceable :always
    :abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (:advance-counter card 0) " subroutines")}]
    :subroutines [trash-program]
    :runner-abilities [(runner-break [:credit 3] 1)]}

   "Swordsman"
   {:implementation "AI restriction not implemented"
    :subroutines [(do-net-damage 1)
                  {:prompt "Choose an AI program to trash"
                   :msg (msg "trash " (:title target))
                   :label "Trash an AI program"
                   :effect (effect (trash target))
                   :choices {:req #(and (installed? %)
                                        (is-type? % "Program")
                                        (has-subtype? % "AI"))}}]}

   "Taurus"
   (constellation-ice trash-hardware)

   "Thoth"
   {:implementation "Encounter effect is manual"
    :abilities [give-tag]
    :runner-abilities [{:label "Take 1 tag"
                        :delayed-completion true
                        :effect (req (system-msg state :runner "takes 1 tag on encountering Thoth")
                                     (tag-runner state :runner eid 1))}]
    :subroutines [(trace-ability 4 {:label "Do 1 net damage for each Runner tag"
                                    :delayed-completion true
                                    :msg (msg "do " (:tag runner) " net damage")
                                    :effect (effect (damage eid :net (:tag runner) {:card card}))})
                  (trace-ability 4 {:label "Runner loses 1 [Credits] for each tag"
                                    :delayed-completion true
                                    :msg (msg "force the Runner to lose " (:tag runner) " [Credits]")
                                    :effect (effect (lose :runner :credit (:tag runner)))})]}

   "TL;DR"
   {:subroutines [{:msg "duplicate subroutines on next piece of ICE encountered this run"}]}

   "TMI"
   {:trace {:base 2
            :msg "keep TMI rezzed"
            :unsuccessful {:effect (effect (derez card))}}
    :subroutines [end-the-run]}

   "Tollbooth"
   {:implementation "Encounter effect is manual"
    :abilities [{:msg "make the Runner pay 3 [Credits], if able"
                 :effect (effect (pay :runner card :credit 3))}]
    :subroutines [end-the-run]}

   "Tour Guide"
   {:abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (count (filter #(and (is-type? % "Asset") (rezzed? %))
                                                  (all-installed state :corp))) " subroutines")}]
    :subroutines [end-the-run]}

   "Tribunal"
   {:subroutines [{:msg "force the Runner to trash 1 installed card"
                   :effect (effect (resolve-ability :runner trash-installed card nil))}]}

   "Troll"
   {:implementation "Encounter effect is manual"
    :abilities [(trace-ability 2 {:label "Force the Runner to lose [Click] or end the run"
                                  :msg "force the Runner to lose [Click] or end the run"
                                  :player :runner
                                  :prompt "Choose one"
                                  :choices ["Lose [Click]" "End the run"]
                                  :effect (req (if-not (and (= target "Lose [Click]")
                                                            (can-pay? state :runner nil [:click 1]))
                                                 (do (end-run state side)
                                                     (system-msg state side "ends the run"))
                                                 (do (lose state side :click 1)
                                                     (system-msg state side "loses [Click]"))))})]}

   "Tsurugi"
   {:subroutines [end-the-run
                  (do-net-damage 1)]}

   "Turing"
   {:implementation "AI restriction not implemented"
    :subroutines [end-the-run]
    :strength-bonus (req (if (is-remote? (second (:zone card))) 3 0))
    :runner-abilities [(runner-break [:click 3] 1)]}

   "Turnpike"
   {:implementation "Encounter effect is manual"
    :abilities [{:msg "force the Runner to lose 1 [Credits]"
                 :effect (effect (lose :runner :credit 1))}]
    :subroutines [(tag-trace 5)]}

   "Tyrant"
   {:advanceable :while-rezzed
    :abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (:advance-counter card 0) " subroutines")}]
    :subroutines [end-the-run]}

   "Universal Connectivity Fee"
   {:subroutines [{:label "Force the Runner to lose credits"
                   :msg (msg "force the Runner to lose " (if tagged "all credits" "1 [Credits]"))
                   :effect (req (if tagged
                                  (do (lose state :runner :credit :all)
                                      (when current-ice
                                        (no-action state side nil)
                                        (continue state side nil))
                                      (trash state side card))
                                  (lose state :runner :credit 1)))}]}

   "Upayoga"
   {:implementation "\"Resolve a subroutine...\" subroutine is not implemented"
    :subroutines [(do-psi {:label "Make the Runner lose 2 [Credits]"
                           :msg "make the Runner lose 2 [Credits]"
                           :effect (effect (lose :runner :credit 2))})
                  {:msg "resolve a subroutine on a piece of rezzed psi ICE"}]}

   "Uroboros"
   {:subroutines [(trace-ability 4 {:label "Prevent the Runner from making another run"
                                    :msg "prevent the Runner from making another run"
                                    :effect (effect (prevent-run))})
                  (trace-ability 4 end-the-run)]}

   "Vanilla"
   {:subroutines [end-the-run]}

   "Veritas"
   {:subroutines [{:label "Corp gains 2 [Credits]"
                   :msg "gain 2 [Credits]"
                   :effect (effect (gain :corp :credit 2))}
                  {:label "Runner loses 2 [Credits]"
                   :msg "force the Runner to lose 2 [Credits]"
                   :effect (effect (lose :runner :credit 2))}
                  (trace-ability 2 give-tag)]}

   "Vikram 1.0"
   {:implementation "Program prevention is not implemented"
    :subroutines [{:msg "prevent the Runner from using programs for the remainder of this run"}
                  (trace-ability 4 (do-brain-damage 1))]
    :runner-abilities [(runner-break [:click 1] 1)]}

   "Viktor 1.0"
   {:subroutines [(do-brain-damage 1)
                  end-the-run]
    :runner-abilities [(runner-break [:click 1] 1)]}

   "Viktor 2.0"
   {:abilities [(power-counter-ability (do-brain-damage 1))]
    :subroutines [(trace-ability 2 add-power-counter)
                  end-the-run]
    :runner-abilities [(runner-break [:click 2] 2)]}

   "Viper"
   {:subroutines [(trace-ability 3 {:label "The Runner loses 1 [Click] if able"
                                    :msg "force the Runner to lose 1 [Click] if able"
                                    :effect (effect (lose :runner :click 1))})
                  (trace-ability 3 end-the-run)]}

   "Virgo"
   (constellation-ice give-tag)

   "Waiver"
   {:subroutines [(trace-ability 5 {:label "Reveal the Runner's Grip and trash cards"
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
   {:subroutines [end-the-run]}

   "Wall of Thorns"
   {:subroutines [end-the-run
                  (do-net-damage 2)]}

   "Wendigo"
   (implementation-note
     "Program prevention is not implemented"
     (morph-ice "Code Gate" "Barrier"
                {:msg "prevent the Runner from using a chosen program for the remainder of this run"}))

   "Whirlpool"
   {:subroutines [{:msg "prevent the Runner from jacking out"
                   :effect (req (when (and (is-remote? (second (:zone card)))
                                           (> (count (concat (:ices (card->server state card))
                                                             (:content (card->server state card)))) 1))
                                  (prevent-jack-out state side))
                                (when current-ice
                                  (no-action state side nil)
                                  (continue state side nil))
                                (trash state side card))}]}

   "Woodcutter"
   {:advanceable :while-rezzed
    :abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (:advance-counter card 0) " subroutines")}]
    :subroutines [(do-net-damage 1)]}

   "Wormhole"
   ;; TODO: create an ability for wormhole
   (implementation-note "Wormhole subroutine is not implemented"
                        (space-ice))

   "Wotan"
   {:subroutines [end-the-run
                  (do-brain-damage 1)]
    :runner-abilities [(runner-break [:click 2] 1)
                       (runner-break [:credit 3] 1)]}

   "Wraparound"
   {:subroutines [end-the-run]
    :strength-bonus (req (if (some #(has-subtype? % "Fracter") (all-installed state :runner))
                           0 7))
    :events (let [wr {:silent (req true)
                      :req (req (and (not= (:cid target) (:cid card))
                                     (has-subtype? target "Fracter")))
                      :effect (effect (update-ice-strength card))}]
              {:runner-install wr :trash wr :card-moved wr})}

   "Yagura"
   {:subroutines [(do-net-damage 1)
                  {:msg "look at the top card of R&D"
                   :optional {:prompt (msg "Move " (:title (first (:deck corp))) " to the bottom of R&D?")
                              :yes-ability {:effect (effect (move (first (:deck corp)) :deck)
                                                            (do (system-msg state side "uses Yagura to move the top card of R&D to the bottom")))}
                              :no-ability {:effect (req (system-msg state :corp (str "does not use Yagura to move the top card of R&D to the bottom")))}}}]}

   "Zed 1.0"
   {:implementation "Restriction on having spent [click] is not implemented"
    :subroutines [(do-brain-damage 1)]
    :runner-abilities [(runner-break [:click 1] 1)]}})
