(in-ns 'game.core)

(def cards-events
  {"Account Siphon"
   {:effect (effect (run :hq {:req (req (= target :hq))
                              :replace-access
                              {:msg (msg "force the Corp to lose " (min 5 (:credit corp))
                                         " [Credits], gain " (* 2 (min 5 (:credit corp)))
                                         " [Credits] and take 2 tags")
                               :effect (effect (tag-runner 2)
                                               (gain :runner :credit (* 2 (min 5 (:credit corp))))
                                               (lose :corp :credit (min 5 (:credit corp))))}} card))}

   "Amped Up"
   {:msg "gain [Click][Click][Click] and suffer 1 brain damage"
    :effect (effect (gain :click 3) (damage eid :brain 1 {:unpreventable true :card card}))}

   "Another Day, Another Paycheck"
   {:events {:agenda-stolen
             {:trace {:base 0
                      :msg (msg (str "Runner gains " (+ (:agenda-point runner) (:agenda-point corp)) " [Credits]"))
                      :unsuccessful {:effect (effect (gain :runner :credit
                                                           (+ (:agenda-point runner) (:agenda-point corp))))
                                     :msg (msg (str "gain " (+ (:agenda-point runner) (:agenda-point corp)) " [Credits]"))}}}}}

   "Apocalypse"
   {:req (req (and (some #{:hq} (:successful-run runner-reg))
                   (some #{:rd} (:successful-run runner-reg))
                   (some #{:archives} (:successful-run runner-reg))))
                           ;; trash cards from right to left
                           ;; otherwise, auto-killing servers would move the cards to the next server
                           ;; so they could no longer be trashed in the same loop
    :msg "trash all installed Corp cards and turn all installed Runner cards facedown"
    :effect (req (let [allcorp (->> (all-installed state :corp)
                                    (sort-by #(vec (:zone %)))
                                    (reverse))]
                   (doseq [c allcorp]
                     (trash state side c)))

                 ;; do hosted cards first so they don't get trashed twice
                 (doseq [c (all-installed state :runner)]
                   (when (or (= ["onhost"] (get c :zone)) (= '(:onhost) (get c :zone)))
                     (move state side c [:rig :facedown])
                     (if (:memoryunits c)
                       (gain state :runner :memory (:memoryunits c)))))
                 (doseq [c (all-installed state :runner)]
                   (when (not (or (= ["onhost"] (get c :zone)) (= '(:onhost) (get c :zone))))
                     (move state side c [:rig :facedown])
                     (if (:memoryunits c)
                       (gain state :runner :memory (:memoryunits c))))))}

   "Blackmail"
   {:req (req has-bad-pub) :prompt "Choose a server" :choices (req runnable-servers)
    :msg "prevent ICE from being rezzed during this run"
    :effect (effect (register-run-flag!
                      card
                      :can-rez
                      (fn [state side card]
                        (if (ice? card)
                          ((constantly false) (system-msg state side (str "is prevented from rezzing ICE on this run by Blackmail")))
                          true)))
                    (run target nil card))}

   "Bribery"
   {:prompt "How many [Credits]?" :choices :credit
    :msg (msg "increase the rez cost of the 1st unrezzed ICE approached by " target " [Credits]")
    :effect (effect (resolve-ability {:prompt "Choose a server" :choices (req runnable-servers)
                                      :effect (effect (run target nil card))} card nil))}

   "Calling in Favors"
   {:msg (msg "gain " (count (filter #(has-subtype? % "Connection") (all-installed state :runner)))
              " [Credits]")
    :effect (effect (gain :credit (count (filter #(has-subtype? % "Connection")
                                                 (all-installed state :runner)))))}

   "Career Fair"
   {:prompt "Choose a resource to install from your Grip"
    :choices {:req #(and (is-type? % "Resource")
                         (in-hand? %))}
    :effect (effect (install-cost-bonus [:credit -3]) (runner-install target))}

   "CBI Raid"
   (letfn [(cbi-final [chosen original]
             {:prompt (str "The top cards of R&D will be " (clojure.string/join  ", " (map :title chosen)) ".")
              :choices ["Done" "Start over"]
              :delayed-completion true
              :effect (req (if (= target "Done")
                             (do (doseq [c (reverse chosen)] (move state :corp c :deck {:front true}))
                                 (clear-wait-prompt state :runner)
                                 (effect-completed state side eid card))
                             (continue-ability state side (cbi-choice original '() (count original) original)
                                               card nil)))})
           (cbi-choice [remaining chosen n original]
             {:prompt "Choose a card to move next onto R&D"
              :choices remaining
              :delayed-completion true
              :effect (req (let [chosen (cons target chosen)]
                             (if (< (count chosen) n)
                               (continue-ability state side (cbi-choice (remove-once #(not= target %) remaining)
                                                                        chosen n original) card nil)
                               (continue-ability state side (cbi-final chosen original) card nil))))})]
     {:delayed-completion true
      :effect (effect (run :hq {:replace-access
                                {:msg "force the Corp to add all cards in HQ to the top of R&D"
                                 :delayed-completion true
                                 :effect (req (show-wait-prompt state :runner "Corp to add all cards in HQ to the top of R&D")
                                              (let [from (:hand corp)]
                                                (if (pos? (count from))
                                                  (continue-ability state :corp (cbi-choice from '() (count from) from) card nil)
                                                  (do (clear-wait-prompt state :runner)
                                                      (effect-completed state side eid card)))))}} card))})

   "Code Siphon"
   {:effect (effect (run :rd
                         {:replace-access
                          {:prompt "Choose a program to install"
                           :msg (msg "install " (:title target) " and take 1 tag")
                           :choices (req (filter #(is-type? % "Program") (:deck runner)))
                           :effect (effect (trigger-event :searched-stack nil)
                                           (shuffle! :deck)
                                           (install-cost-bonus [:credit (* -3 (count (get-in corp [:servers :rd :ices])))])
                                           (runner-install target)
                                           (tag-runner 1) )}} card))}

   "Corporate Scandal"
   {:msg "give the Corp 1 additional bad publicity"
    :effect (req (swap! state update-in [:corp :has-bad-pub] inc))
    :leave-play (req (swap! state update-in [:corp :has-bad-pub] dec))}

   "Cyber Threat"
   {:prompt "Choose a server" :choices (req runnable-servers)
    :delayed-completion true
    :effect (req (let [serv target]
                   (continue-ability
                     state :corp
                     {:optional
                      {:prompt (msg "Rez a piece of ICE protecting " serv "?")
                       :yes-ability {:prompt (msg "Choose a piece of " serv " ICE to rez") :player :corp
                                     :choices {:req #(and (not (:rezzed %))
                                                          (= (last (:zone %)) :ices))}
                                     :effect (req (rez state :corp target nil))}
                       :no-ability {:effect (effect (game.core/run eid serv nil card))
                                    :msg (msg "make a run on " serv " during which no ICE can be rezzed")}}}
                    card nil)))}

   "Data Breach"
   {:delayed-completion true
    :effect (req (register-events state side (:events (card-def card))
                                  (assoc card :zone '(:discard)))
                 (when-completed (game.core/run state side :rd nil card)
                                 (let [card (get-card state (assoc card :zone '(:discard)))]
                                   (unregister-events state side card)
                                   (if (:run-again card)
                                     (game.core/run state side eid :rd nil card)
                                     (effect-completed state side eid))
                                   (update! state side (dissoc card :run-again)))))
    :events {:successful-run-ends {:optional {:req (req (= [:rd] (:server target)))
                                              :prompt "Make another run on R&D?"
                                              :yes-ability {:effect (effect (update! (assoc card :run-again true)))}}}}}

   "Day Job"
   {:additional-cost [:click 3]
    :msg "gain 10 [Credits]" :effect (effect (gain :credit 10))}

   "Déjà Vu"
   {:prompt "Choose a card to add to Grip" :choices (req (cancellable (:discard runner) :sorted))
    :msg (msg "add " (:title target) " to their Grip")
    :effect (req (move state side target :hand)
                 (when (has-subtype? target "Virus")
                   (resolve-ability state side
                                    {:prompt "Choose a virus to add to Grip"
                                     :msg (msg "add " (:title target) " to their Grip")
                                     :choices (req (cancellable
                                                     (filter #(has-subtype? % "Virus") (:discard runner)) :sorted))
                                     :effect (effect (move target :hand))} card nil)))}

   "Demolition Run"
   {:prompt "Choose a server" :choices ["HQ" "R&D"]
    :abilities [{:msg (msg "trash " (:title (:card (first (get-in @state [side :prompt])))) " at no cost")
                 :effect (effect (trash-no-cost))}]
    :effect (effect (run target nil card)
                    (prompt! card (str "Click Demolition Run in the Temporary Zone to trash a card being accessed at no cost") ["OK"] {})
                    (resolve-ability
                      {:effect (req (let [c (move state side (last (:discard runner)) :play-area)]
                                      (card-init state side c false)
                                      (register-events state side
                                                       {:run-ends {:effect (effect (trash c))}} c)))}
                     card nil))
    :events {:run-ends nil}}

   "Deuces Wild"
   (let [all [{:effect (effect (gain :credit 3))
               :msg "gain 3 [Credits]"}
              {:effect (effect (draw 2))
               :msg "draw 2 cards"}
              {:effect (effect (lose :tag 1))
               :msg "remove 1 tag"}
              {:prompt "Select 1 piece of ice to expose"
               :msg "expose 1 ice and make a run"
               :choices {:req #(and (installed? %) (ice? %))}
               :delayed-completion true
               :effect (req (when-completed (expose state side target)
                                            (continue-ability
                                              state side
                                              {:prompt "Choose a server"
                                               :choices (req runnable-servers)
                                               :delayed-completion true
                                               :effect (effect (game.core/run eid target))}
                                              card nil)))}]
         choice (fn choice [abis]
                  {:prompt "Choose an ability to resolve"
                   :choices (map #(capitalize (:msg %)) abis)
                   :delayed-completion true
                   :effect (req (let [chosen (some #(when (= target (capitalize (:msg %))) %) abis)]
                                  (when-completed
                                    (resolve-ability state side chosen card nil)
                                    (if (= (count abis) 4)
                                      (continue-ability state side (choice (remove-once #(not= % chosen) abis)) card nil)
                                      (effect-completed state side eid)))))})]
     {:delayed-completion true
      :effect (effect (continue-ability (choice all) card nil))})

   "Diesel"
   {:msg "draw 3 cards" :effect (effect (draw 3))}

   "Dirty Laundry"
   {:prompt "Choose a server" :choices (req runnable-servers)
    :effect (effect (run target {:end-run {:req (req (:successful run)) :msg " gain 5 [Credits]"
                                           :effect (effect (gain :runner :credit 5))}} card))}

   "Drive By"
   {:choices {:req #(let [topmost (get-nested-host %)]
                     (and (is-remote? (second (:zone topmost)))
                          (= (last (:zone topmost)) :content)
                          (not (:rezzed %))))}
    :delayed-completion true
    :effect (req (when-completed (expose state side target) ;; would be nice if this could return a value on completion
                                 (if async-result ;; expose was successful
                                   (if (#{"Asset" "Upgrade"} (:type target))
                                     (do (system-msg state :runner (str "uses Drive By to trash " (:title target)))
                                         (trash state side (assoc target :seen true))
                                         (effect-completed state side eid))
                                     (effect-completed state side eid))
                                   (effect-completed state side eid))))}

   "Early Bird"
   {:prompt "Choose a server"
    :choices (req runnable-servers)
    :msg (msg "make a run on " target " and gain [Click]")
    :effect (effect (gain :click 1) (run target nil card))}

   "Easy Mark"
   {:msg "gain 3 [Credits]" :effect (effect (gain :credit 3))}

   "Emergency Shutdown"
   {:req (req (some #{:hq} (:successful-run runner-reg)))
    :msg (msg "derez " (:title target))
    :choices {:req #(and (ice? %)
                         (rezzed? %))}
    :effect (effect (derez target))}

   "Employee Strike"
   {:msg "disable the Corp's identity"
    :effect (effect (disable-identity :corp))
    :leave-play (effect (enable-identity :corp))}

   "Escher"
   (letfn [(es [] {:prompt "Select two pieces of ICE to swap positions"
                   :choices {:req #(and (installed? %) (ice? %)) :max 2}
                   :effect (req (if (= (count targets) 2)
                                  (do (swap-ice state side (first targets) (second targets))
                                      (resolve-ability state side (es) card nil))
                                  (system-msg state side "has finished rearranging ICE")))})]
     {:effect (effect (run :hq {:replace-access
                                {:msg "rearrange installed ICE"
                                 :effect (effect (resolve-ability (es) card nil))}} card))})

   "Eureka!"
   {:effect
    (req (let [topcard (first (:deck runner))
               caninst (some #(= % (:type topcard)) '("Hardware" "Resource" "Program"))
               cost (min 10 (:cost topcard))]
           (when caninst
             (do (gain state side :credit cost)
                 (runner-install state side topcard)))
           (when (get-card state topcard) ; only true if card was not installed
             (do (system-msg state side (str "reveals and trashes " (:title topcard)))
                 (trash state side topcard)
                 (when caninst (lose state side :credit cost))))))}

   "Exclusive Party"
   {:msg (msg "draw 1 card and gain "
              (count (filter #(= (:title %) "Exclusive Party") (:discard runner)))
              " [Credits]")
    :effect (effect (draw) (gain :credit (count (filter #(= (:title %) "Exclusive Party") (:discard runner)))))}

   "Executive Wiretaps"
   {:msg (msg "reveal cards in HQ: " (join ", " (map :title (:hand corp))))}

   "Exploratory Romp"
   {:prompt "Choose a server" :choices (req runnable-servers)
    :effect (effect (run target
                       {:replace-access
                        {:prompt "Advancements to remove from a card in or protecting this server?"
                         :choices ["0", "1", "2", "3"]
                         :effect (req (let [c (Integer/parseInt target)]
                                        (resolve-ability
                                          state side
                                          {:choices {:req #(and (contains? % :advance-counter)
                                                                (= (:server run) (vec (rest (butlast (:zone %))))))}
                                          :msg (msg "remove " c " advancements from "
                                                (card-str state target))
                                          :effect (req (add-prop state :corp target :advance-counter (- c))
                                                       (swap! state update-in [:runner :prompt] rest)
                                                       (handle-end-run state side))}
                                         card nil)))}} card))}

   "Express Delivery"
   {:prompt "Choose a card to add to your Grip" :choices (req (take 4 (:deck runner)))
    :msg "look at the top 4 cards of their Stack and add 1 of them to their Grip"
    :effect (effect (move target :hand) (shuffle! :deck))}

   "Fear the Masses"
   {:effect (effect (run :hq {:req (req (= target :hq))
                              :replace-access
                              {:delayed-completion true
                               :mandatory true
                               :msg "force the Corp to trash the top card of R&D"
                               :effect (req (mill state :corp)
                                            (let [n (count (filter #(= (:title card) (:title %)) (:hand runner)))]
                                              (if (> n 0)
                                                (continue-ability state side
                                                  {:prompt "Reveal how many copies of Fear the Masses?"
                                                   :choices {:number (req n)}
                                                   :effect (req (when (> target 0)
                                                                  (mill state :corp target)
                                                                  (system-msg state side
                                                                              (str "reveals " target " copies of Fear the Masses,"
                                                                                   " forcing the Corp to trash " target " cards"
                                                                                   " from the top of R&D"))))}
                                                 card nil)
                                                (effect-completed state side eid card))))}} card))}

   "Feint"
   {:effect (effect (run :hq nil card) (register-events (:events (card-def card))
                                                        (assoc card :zone '(:discard))))
    :events {:successful-run {:msg "access 0 cards"
                              :effect (effect (max-access 0))}
             :run-ends {:effect (effect (unregister-events card))}}}

   "Fisk Investment Seminar"
   {:msg "make each player draw 3 cards"
    :effect (effect (draw 3) (draw :corp 3))}

   "Forged Activation Orders"
   {:choices {:req #(and (ice? %)
                         (not (rezzed? %)))}
    :effect (req (let [ice target
                       serv (zone->name (second (:zone ice)))
                       icepos (ice-index state ice)]
                   (resolve-ability
                     state :corp
                     {:prompt (msg "Rez " (:title ice) " at position " icepos
                                   " of " serv " or trash it?") :choices ["Rez" "Trash"]
                      :effect (effect (resolve-ability
                                        (if (and (= target "Rez") (<= (rez-cost state :corp ice) (:credit corp)))
                                          {:msg (msg "force the rez of " (:title ice))
                                           :effect (effect (rez :corp ice))}
                                          {:msg (msg "trash the ICE at position " icepos " of " serv)
                                           :effect (effect (trash :corp ice))})
                                        card nil))}
                     card nil)))}

   "Forked"
   {:prompt "Choose a server" :choices (req runnable-servers) :effect (effect (run target nil card))}

   "Frame Job"
   {:prompt "Choose an agenda to forfeit"
    :choices (req (:scored runner))
    :effect (effect (forfeit target) (gain :corp :bad-publicity 1))
    :msg (msg "forfeit " (:title target) " and give the Corp 1 bad publicity")}

   "\"Freedom Through Equality\""
   {:events {:agenda-stolen {:msg "add it to their score area as an agenda worth 1 agenda point"
                             :effect (effect (as-agenda :runner card 1))}}}

   "Freelance Coding Contract"
   {:choices {:max 5
              :req #(and (is-type? % "Program")
                         (in-hand? %))}
    :msg (msg "trash " (join ", " (map :title targets)) " and gain "
              (* 2 (count targets)) " [Credits]")
    :effect (req (doseq [c targets]
                   (trash state side c {:unpreventable true}))
                 (gain state side :credit (* 2 (count targets))))}

   "Game Day"
   {:msg (msg "draw " (- (hand-size state :runner) (count (:hand runner))) " cards")
    :effect (effect (draw (- (hand-size state :runner) (count (:hand runner)))))}

   "Hacktivist Meeting"
   {:events {:rez {:req (req (and (not (ice? target)) (< 0 (count (:hand corp)))))
                   ;; FIXME the above condition is just a bandaid, proper fix would be preventing the rez altogether
                   :msg "force the Corp to trash 1 card from HQ at random"
                   :effect (effect (trash (first (shuffle (:hand corp)))))}}}

   "High-Stakes Job"
   {:prompt "Choose a server"
    :choices (req (let [unrezzed-ice #(seq (filter (complement rezzed?) (:ices (second %))))
                        bad-zones (keys (filter (complement unrezzed-ice) (get-in @state [:corp :servers])))]
                    (zones->sorted-names (remove (set bad-zones) (get-runnable-zones @state)))))
    :effect (effect (run target {:end-run {:req (req (:successful run)) :msg " gain 12 [Credits]"
                                           :effect (effect (gain :runner :credit 12))}} card))}

   "Hostage"
   {:prompt "Choose a Connection"
    :choices (req (cancellable (filter #(has-subtype? % "Connection") (:deck runner)) :sorted))
    :msg (msg "add " (:title target) " to their Grip and shuffle their Stack")
    :effect (req (let [connection target]
                   (trigger-event state side :searched-stack nil)
                   (resolve-ability
                     state side
                     {:prompt (str "Install " (:title connection) "?")
                      :choices ["Yes" "No"]
                      :effect (req (let [d target]
                                     (resolve-ability state side
                                       {:effect (req (shuffle! state side :deck)
                                                     (if (= "Yes" d)
                                                       (runner-install state side connection)
                                                       (move state side connection :hand)))} card nil)))}
                     card nil)))}

   "Ive Had Worse"
   {:effect (effect (draw 3))
    :trash-effect {:req (req (#{:meat :net} target))
                   :effect (effect (draw :runner 3)) :msg "draw 3 cards"}}

   "Immolation Script"
   {:effect (effect (run :archives nil card) (register-events (:events (card-def card))
                                                              (assoc card :zone '(:discard))))
    :events {:successful-run
             {:silent (req true)
              :req (req (= target :archives))
              :effect (req (when-completed
                             (resolve-ability state side
                               {:delayed-completion true
                                :prompt "Choose a piece of ICE in Archives"
                                :choices (req (filter ice? (:discard corp)))
                                :effect (req (let [icename (:title target)]
                                               (continue-ability
                                                 state side
                                                 {:delayed-completion true
                                                  :prompt (msg "Choose a rezzed copy of " icename " to trash")
                                                  :choices {:req #(and (ice? %)
                                                                       (rezzed? %)
                                                                       (= (:title %) icename))}
                                                  :msg (msg "trash " (card-str state target))
                                                  :effect (req (trash state :corp target)
                                                               (unregister-events state side card)
                                                               (effect-completed state side eid card))} card nil)))}
                              card nil)
                             (do-access state side eid (:server run))))}}}

   "Independent Thinking"
   (let [cards-to-draw (fn [ts] (* (count ts) (if (some #(and (not (facedown? %)) (has-subtype? % "Directive")) ts) 2 1)))]
     {:choices {:max 5 :req #(and (:installed %) (= (:side %) "Runner"))}
      :effect (effect (trash-cards targets) (draw :runner (cards-to-draw targets)))
      :msg (msg "trash " (count targets) " card" (when (not= 1(count targets)) "s") " and draw " (cards-to-draw targets) " cards")})

   "Indexing"
   {:delayed-completion true
    :effect (effect (run :rd
                         {:replace-access
                          {:msg "rearrange the top 5 cards of R&D"
                           :delayed-completion true
                           :effect (req (show-wait-prompt state :corp "Runner to rearrange the top cards of R&D")
                                        (let [from (take 5 (:deck corp))]
                                          (if (pos? (count from))
                                            (continue-ability state side (reorder-choice :corp :corp from '()
                                                                                         (count from) from) card nil)
                                            (do (clear-wait-prompt state :corp)
                                                (effect-completed state side eid card)))))}} card))}

   "Infiltration"
   {:prompt "Gain 2 [Credits] or expose a card?" :choices ["Gain 2 [Credits]" "Expose a card"]
    :effect (effect (continue-ability (if (= target "Expose a card")
                                        {:choices {:req installed?}
                                         :delayed-completion true
                                         :effect (effect (expose eid target))}
                                         {:msg "gain 2 [Credits]" :effect (effect (gain :credit 2))})
                                      card nil))}

   "Information Sifting"
   (letfn [(access-pile [cards pile]
             {:prompt "Select a card to access. You must access all cards."
              :choices [(str "Card from pile " pile)]
              :effect (req (system-msg state side (str "accesses " (:title (first cards))))
                           (when-completed
                             (handle-access state side [(first cards)])
                             (do (if (< 1 (count cards))
                                   (continue-ability state side (access-pile (next cards) pile) card nil)
                                   (effect-completed state side eid card)))))})
           (which-pile [p1 p2]
             {:prompt "Choose a pile to access"
              :choices [(str "Pile 1 (" (count p1) " cards)") (str "Pile 2 (" (count p2) " cards)")]
              :effect (req (let [choice (if (.startsWith target "Pile 1") 1 2)]
                             (clear-wait-prompt state :corp)
                             (continue-ability state side
                                (access-pile (if (= 1 choice) p1 p2) choice)
                                card nil)))})]
     (let [access-effect
           {:delayed-completion true
            :mandatory true
            :effect (req (if (< 1 (count (:hand corp)))
                           (do (show-wait-prompt state :runner "Corp to create two piles")
                               (continue-ability
                                 state :corp
                                 {:prompt (msg "Select up to " (dec (count (:hand corp))) " cards for the first pile")
                                  :choices {:req #(and (in-hand? %) (card-is? % :side :corp))
                                            :max (req (dec (count (:hand corp))))}
                                  :effect (effect (clear-wait-prompt :runner)
                                                  (show-wait-prompt :corp "Runner to choose a pile")
                                                  (continue-ability
                                                    :runner
                                                    (which-pile (shuffle targets)
                                                                (shuffle (vec (clojure.set/difference
                                                                                (set (:hand corp)) (set targets)))))
                                                    card nil))
                                  } card nil))
                           (effect-completed state side eid card)))}]
       {:effect (effect (run :hq {:req (req (= target :hq))
                                  :replace-access access-effect}
                             card))}))

   "Inject"
   {:effect (req (doseq [c (take 4 (get-in @state [:runner :deck]))]
                   (if (is-type? c "Program")
                     (do (trash state side c) (gain state side :credit 1)
                         (system-msg state side (str "trashes " (:title c) " and gains 1 [Credits]")))
                     (do (move state side c :hand)
                         (system-msg state side (str "adds " (:title c) " to Grip"))))))}

   "Injection Attack"
   {:prompt "Choose a server"
    :choices (req runnable-servers)
    :delayed-completion true
    :effect (effect (run target nil card)
                    (continue-ability
                      {:prompt "Choose an icebreaker"
                       :choices {:req #(and (installed? %) (has-subtype? % "Icebreaker"))}
                       :effect (effect (pump target 2 :all-run))}
                      card nil))}

   "Inside Job"
   {:prompt "Choose a server" :choices (req runnable-servers) :effect (effect (run target nil card))}

   "Itinerant Protesters"
   {:msg "reduce the Corp's maximum hand size by 1 for each bad publicity"
    :effect (req (lose state :corp :hand-size-modification (:bad-publicity corp))
                 (add-watch state :itin
                   (fn [k ref old new]
                     (let [bpnew (get-in new [:corp :bad-publicity])
                           bpold (get-in old [:corp :bad-publicity])]
                       (when (> bpnew bpold)
                         (lose state :corp :hand-size-modification (- bpnew bpold)))
                       (when (< bpnew bpold)
                         (gain state :corp :hand-size-modification (- bpold bpnew)))))))
    :leave-play (req (remove-watch state :itin)
                     (gain state :corp :hand-size-modification (:bad-publicity corp)))}

   "Knifed"
   {:prompt "Choose a server" :choices (req runnable-servers) :effect (effect (run target nil card))}

   "Kraken"
   {:req (req (:stole-agenda runner-reg)) :prompt "Choose a server" :choices (req servers)
    :msg (msg "force the Corp to trash an ICE protecting " target)
    :effect (req (let [serv (next (server->zone state target))
                       servname target]
                   (resolve-ability
                     state :corp
                     {:prompt (msg "Choose a piece of ICE in " target " to trash")
                      :choices {:req #(and (= (last (:zone %)) :ices)
                                           (= serv (rest (butlast (:zone %)))))}
                      :effect (req (trash state :corp target)
                                   (system-msg state side (str "trashes "
                                    (card-str state target))))}
                    card nil)))}

   "Lawyer Up"
   {:msg "remove 2 tags and draw 3 cards"
    :effect (effect (draw 3) (lose :tag 2))}

   "Legwork"
   {:effect (effect (run :hq nil card) (register-events (:events (card-def card))
                                                        (assoc card :zone '(:discard))))
    :events {:successful-run {:silent (req true)
                              :effect (effect (access-bonus 2))}
             :run-ends {:effect (effect (unregister-events card))}}}

   "Leverage"
   {:req (req (some #{:hq} (:successful-run runner-reg)))
    :player :corp
    :prompt "Take 2 bad publicity?"
    :choices ["Yes" "No"]
    :effect (req (if (= target "Yes")
                   (do (gain state :corp :bad-publicity 2) (system-msg state :corp "takes 2 bad publicity"))
                   (do (register-events state side
                                        {:pre-damage {:effect (effect (damage-prevent :net Integer/MAX_VALUE)
                                                                      (damage-prevent :meat Integer/MAX_VALUE)
                                                                      (damage-prevent :brain Integer/MAX_VALUE))}
                                         :runner-turn-begins {:effect (effect (unregister-events card))}}
                                        (assoc card :zone '(:discard)))
                       (system-msg state :runner "is immune to damage until the beginning of the Runner's next turn"))))
    ; This :events is a hack so that the unregister-events above will fire.
    :events {:runner-turn-begins nil :pre-damage nil}}

   "Levy AR Lab Access"
   {:msg "shuffle their Grip and Heap into their Stack and draw 5 cards"
    :effect (effect (shuffle-into-deck :hand :discard) (draw 5)
                    (move (first (:play-area runner)) :rfg))}

   "Lucky Find"
   {:msg "gain 9 [Credits]"
    :effect (effect (gain :credit 9))}

   "Making an Entrance"
   (letfn [(entrance-trash [cards]
             {:prompt "Choose a card to trash"
              :choices (cons "None" cards)
              :delayed-completion true
              :msg (req (when (not= target "None") (str "trash " (:title target))))
              :effect (req (if (= target "None")
                             (if (not-empty cards)
                               (continue-ability state side (reorder-choice :runner :corp cards '()
                                                                            (count cards) cards) card nil)
                               (do (clear-wait-prompt state :corp)
                                   (effect-completed state side eid card)))
                             (do (trash state side target {:unpreventable true})
                                 (continue-ability state side (entrance-trash (remove-once #(not= % target) cards))
                                                   card nil))))})]
     {:msg "look at and trash or rearrange the top 6 cards of their Stack"
      :delayed-completion true
      :effect (req (show-wait-prompt state :corp "Runner to rearrange the top cards of their stack")
                   (let [from (take 6 (:deck runner))]
                     (continue-ability state side (entrance-trash from) card nil)))})

   "Mass Install"
   (let [mhelper (fn mi [n] {:prompt "Select a program to install"
                             :choices {:req #(and (is-type? % "Program")
                                                  (in-hand? %))}
                             :effect (req (runner-install state side target)
                                            (when (< n 3)
                                              (resolve-ability state side (mi (inc n)) card nil)))})]
     {:effect (effect (resolve-ability (mhelper 1) card nil))})

   "Modded"
   {:prompt "Choose a program or piece of hardware to install from your Grip"
    :choices {:req #(and (or (is-type? % "Hardware")
                             (is-type? % "Program"))
                         (in-hand? %))}
    :effect (effect (install-cost-bonus [:credit -3]) (runner-install target))}

   "Net Celebrity"
   {:recurring 1}

   "Networking"
   {:msg "remove 1 tag"
    :effect (effect (lose :tag 1))
    :optional {:prompt "Pay 1 [Credits] to add Networking to Grip?"
               :yes-ability {:cost [:credit 1]
                             :msg "add it to their Grip"
                             :effect (effect (move (last (:discard runner)) :hand))}}}

   "Notoriety"
   {:req (req (and (some #{:hq} (:successful-run runner-reg))
                   (some #{:rd} (:successful-run runner-reg))
                   (some #{:archives} (:successful-run runner-reg))))
    :effect (effect (as-agenda :runner (first (:play-area runner)) 1))
    :msg "add it to their score area as an agenda worth 1 agenda point"}

   "Out of the Ashes"
   (letfn [(ashes-flag []
             {:runner-phase-12 {:priority -1
                                :once :per-turn
                                :once-key :out-of-ashes
                                :effect (effect (continue-ability
                                                  (ashes-recur (count (filter #(= "Out of the Ashes" (:title %))
                                                                              (:discard runner))))
                                                  card nil))}})
           (ashes-run []
             {:prompt "Choose a server"
              :choices (req runnable-servers)
              :delayed-completion true
              :effect (effect (run eid target nil card))})
           (ashes-recur [n]
             {:prompt "Remove Out of the Ashes from the game to make a run?"
              :choices ["Yes" "No"]
              :effect (req (if (= target "Yes")
                             (let [card (some #(when (= "Out of the Ashes" (:title %)) %) (:discard runner))]
                               (system-msg state side "removes Out of the Ashes from the game to make a run")
                               (move state side card :rfg)
                               (unregister-events state side card)
                               (when-completed (resolve-ability state side (ashes-run) card nil)
                                               (if (< 1 n)
                                                 (continue-ability state side (ashes-recur (dec n)) card nil)
                                                 (effect-completed state side eid card))))))})]
   {:prompt "Choose a server"
    :choices (req runnable-servers)
    :effect (effect (run eid target nil card))
    :move-zone (req (if (= [:discard] (:zone card))
                      (register-events state side (ashes-flag) (assoc card :zone [:discard]))
                      (unregister-events state side card)))
    :events {:runner-phase-12 nil}})

   "Paper Tripping"
   {:msg "remove all tags" :effect (effect (lose :tag :all))}

   "Planned Assault"
   {:msg (msg "play " (:title target))
    :choices (req (cancellable (filter #(and (has-subtype? % "Run")
                                             (<= (:cost %) (:credit runner))) (:deck runner)) :sorted))
    :prompt "Choose a Run event" :effect (effect (trigger-event :searched-stack nil)
                                                 (shuffle! :deck)
                                                 (play-instant target {:no-additional-cost true}))}

   "Political Graffiti"
   (let [update-agendapoints (fn [state side target amount]
                               (set-prop state side (get-card state target) :agendapoints (+ amount (:agendapoints (get-card state target))))
                               (gain-agenda-point state side amount))]
     {:events {:purge {:effect (effect (trash card))}}
      :trash-effect {:effect (req (let [current-side (get-scoring-owner state {:cid (:agenda-cid card)})]
                                    (update-agendapoints state current-side (find-cid (:agenda-cid card) (get-in @state [current-side :scored])) 1)))}
      :effect (effect (run :archives
                        {:req (req (= target :archives))
                         :replace-access
                         {:prompt "Choose an agenda to host Political Graffiti"
                          :choices {:req #(in-corp-scored? state side %)}
                          :msg (msg "host Political Graffiti on " (:title target) " as a hosted condition counter")
                          :effect (req (host state :runner (get-card state target)
                                         ; keep host cid in :agenda-cid because `trash` will clear :host
                                         (assoc card :zone [:discard] :installed true :agenda-cid (:cid (get-card state target))))
                                       (update-agendapoints state :corp target -1))}} card))})

   "Populist Rally"
   {:req (req (seq (filter #(has-subtype? % "Seedy") (all-installed state :runner))))
    :msg "give the Corp 1 fewer [Click] to spend on their next turn"
    :effect (effect (lose :corp :click-per-turn 1)
                    (register-events (:events (card-def card))
                                     (assoc card :zone '(:discard))))
    :events {:corp-turn-ends {:effect (effect (gain :corp :click-per-turn 1)
                                              (unregister-events card))}}}

   "Power Nap"
   {:effect (effect (gain :credit (+ 2 (count (filter #(has-subtype? % "Double")
                                                      (:discard runner))))))
    :msg (msg "gain " (+ 2 (count (filter #(has-subtype? % "Double") (:discard runner)))) " [Credits]")}

   "Power to the People"
   {:effect (effect (register-events {:pre-steal-cost
                                      {:once :per-turn :effect (effect (gain :credit 7))
                                                       :msg "gain 7 [Credits]"}
                                      :runner-turn-ends
                                      {:effect (effect (unregister-events card))}}
                    (assoc card :zone '(:discard))))
    :events {:pre-steal-cost nil :runner-turn-ends nil}}

   "Prey"
   {:prompt "Choose a server" :choices (req runnable-servers) :effect (effect (run target nil card))}

   "Push Your Luck"
   {:effect (effect (show-wait-prompt :runner "Corp to guess Odd or Even")
                    (resolve-ability
                      {:player :corp :prompt "Guess whether the Runner will spend an Odd or Even number of credits with Push Your Luck"
                       :choices ["Even" "Odd"] :msg "make the Corp choose a guess"
                       :effect (req (let [guess target]
                                      (clear-wait-prompt state :runner)
                                      (resolve-ability
                                        state :runner
                                        {:choices :credit :prompt "How many credits?"
                                         :msg (msg "spend " target " [Credits]. The Corp guessed " guess)
                                         :effect (req (when (or (and (= guess "Even") (odd? target))
                                                                (and (= guess "Odd") (even? target)))
                                                        (system-msg state :runner (str "gains " (* 2 target) " [Credits]"))
                                                        (gain state :runner :credit (* 2 target))))} card nil)))}
                      card nil))}

   "Quality Time"
   {:msg "draw 5 cards" :effect (effect (draw 5))}

   "Queens Gambit"
   {:choices ["0", "1", "2", "3"] :prompt "How many advancement tokens?"
    :effect (req (let [c (Integer/parseInt target)]
                   (resolve-ability
                     state side
                     {:choices {:req #(and (is-remote? (second (:zone %)))
                                           (= (last (:zone %)) :content)
                                           (not (:rezzed %)))}
                      :msg (msg "add " c " advancement tokens on a card and gain " (* 2 c) " [Credits]")
                      :effect (effect (gain :credit (* 2 c))
                                      (add-prop :corp target :advance-counter c {:placed true})
                                      (register-turn-flag! card :can-access
                                                           ;; prevent access of advanced card
                                                           (fn [_ _ card] (not (same-card? target card)))))}
                     card nil)))}

   "Quest Completed"
   {:req (req (and (some #{:hq} (:successful-run runner-reg))
                   (some #{:rd} (:successful-run runner-reg))
                   (some #{:archives} (:successful-run runner-reg))))
    :choices {:req installed?} :msg (msg "access " (:title target))
    :effect (effect (handle-access targets))}

   "Rebirth"
   {:msg "change identities"
    :prompt "Choose an identity to become"
    :choices (req (let [is-swappable (fn [c] (and (= "Identity" (:type c))
                                             (= (-> @state :runner :identity :faction) (:faction c))
                                             (not (.startsWith (:code c) "00")) ; only draft identities have this
                                             (not (= (:title c) (-> @state :runner :identity :title)))))
                        swappable-ids (filter is-swappable (vals @all-cards))]
                    (cancellable swappable-ids :sorted)))

     :effect (req
               (move state side (last (:discard runner)) :rfg)
               (disable-identity state side)

               ;; Manually reduce the runner's link by old link
               (lose state :runner :link (get-in @state [:runner :identity :baselink]))

               ;; Move the selected ID to [:runner :identity] and set the zone
               (swap! state update-in [side :identity]
                  (fn [x] (assoc (server-card (:title target) (get-in @state [:runner :user]))
                            :zone [:identity])))

               ;; enable-identity does not do everything that identity-init does
               (identity-init state side (get-in @state [:runner :identity]))
               (system-msg state side "NOTE: passive abilities (Kate, Gabe, etc) will incorrectly fire
                if their once per turn condition was met this turn before Rebirth was played.
                Please adjust your game state manually for the rest of this turn if necessary"))}

   "Recon"
   {:prompt "Choose a server" :choices (req runnable-servers) :effect (effect (run target nil card))}

   "Retrieval Run"
   {:effect (effect (run :archives
                      {:req (req (= target :archives))
                       :replace-access
                       {:prompt "Choose a program to install"
                        :msg (msg "install " (:title target))
                        :choices (req (filter #(is-type? % "Program") (:discard runner)))
                        :effect (effect (runner-install target {:no-cost true}))}} card))}

   "Rigged Results"
   (letfn [(choose-ice []
             {:prompt "Choose a piece of ICE to bypass"
              :choices {:req #(ice? %)}
              :effect (final-effect (system-msg :runner (str "chooses to bypass " (card-str state target)))
                                    (run (second (:zone target))))})
           (corp-choice [spent]
             {:prompt "Guess how many credits were spent"
              :choices ["0" "1" "2"]
              :delayed-completion true
              :effect (req (system-msg state :runner (str "spends " spent "[Credit]. "
                                       (-> corp :user :username) " guesses " target "[Credit]"))
                           (clear-wait-prompt state :runner)
                           (if (not= (str spent) target)
                             (continue-ability state :runner (choose-ice) card nil)
                             (effect-completed state side eid)))})
           (runner-choice [cr]
             {:prompt "Spend how many credits?"
              :choices (take cr ["0" "1" "2"])
              :delayed-completion true
              :effect (effect (show-wait-prompt :runner "Corp to guess")
                              (clear-wait-prompt :corp)
                              (continue-ability :corp (corp-choice (Integer/parseInt target)) card nil))})]
   {:effect (effect (show-wait-prompt :corp "Runner to spend credits")
                    (continue-ability (runner-choice (inc (min 2 (:credit runner)))) card nil))})

   "Rumor Mill"
   (letfn [(eligible? [card] (and (:uniqueness card)
                                  (or (card-is? card :type "Asset")
                                      (card-is? card :type "Upgrade"))
                                  (not (has-subtype? card "Region"))))
           (rumor [state] (filter eligible? (all-installed state :corp)))]
   {:leave-play (req (doseq [c (rumor state)]
                       (enable-card state :corp c)))
    :effect (req (doseq [c (rumor state)]
                   (disable-card state :corp c)))
    :events {:corp-install {:req (req (eligible? target))
                            :effect (effect (disable-card :corp target))}}})

   "Run Amok"
   {:prompt "Choose a server" :choices (req runnable-servers)
    :effect (effect (run target {:end-run {:msg " trash 1 piece of ICE that was rezzed during the run"}} card))}

   "Running Interference"
   {:prompt "Choose a server" :choices (req runnable-servers)
    :effect (effect (run target nil card)
                    (register-events {:pre-rez
                                      {:req (req (ice? target))
                                       :effect (effect (rez-cost-bonus (:cost target)))}
                                      :run-ends
                                      {:effect (effect (unregister-events card))}}
                                     (assoc card :zone '(:discard))))
    :events {:pre-rez nil :run-ends nil}}

   "Satellite Uplink"
   {:choices {:max 2 :req installed?}
    :delayed-completion true
    :effect (req (let [[card1 card2] targets]
                   (when-completed (expose state side card1)
                                   (expose state side eid card2))))}

   "Scavenge"
   {:req (req (and (some #(is-type? % "Program") (all-installed state :runner))
                   (some #(is-type? % "Program") (concat (:hand runner) (:discard runner)))))
    :prompt "Choose an installed program to trash"
    :choices {:req #(and (is-type? % "Program")
                         (installed? %))}
    :effect (req (let [trashed target tcost (- (:cost trashed)) st state si side]
                   (trash state side trashed)
                   (resolve-ability
                     state side
                     {:prompt "Choose a program to install from your Grip or Heap"
                      :show-discard true
                      :choices {:req #(and (is-type? % "Program")
                                           (#{[:hand] [:discard]} (:zone %))
                                           (can-pay? st si nil (modified-install-cost st si % [:credit tcost])))}
                      :effect (effect (install-cost-bonus [:credit (- (:cost trashed))])
                                      (runner-install target))
                      :msg (msg "trash " (:title trashed) " and install " (:title target))} card nil)))}


   "Scrubbed"
   {:events (let [sc {:effect (req (update! state side (dissoc card :scrubbed-target)))}]
                 {:encounter-ice {:once :per-turn
                                  :effect (effect (update! (assoc card :scrubbed-target target))
                                                  (update-ice-strength current-ice))}
                  :pre-ice-strength {:req (req (= (:cid target) (get-in card [:scrubbed-target :cid])))
                                     :effect (effect (ice-strength-bonus -2 target))}
                  :run-ends sc})}

   "Showing Off"
   {:effect (effect (run :rd
                      {:replace-access
                       {:msg "access cards from the bottom of R&D"
                        :delayed-completion true
                        :effect (req (swap! state assoc-in [:corp :deck]
                                            (rseq (into [] (get-in @state [:corp :deck]))))
                                     (do-access state side eid (:server run))
                                     (swap! state assoc-in [:corp :deck]
                                            (rseq (into [] (get-in @state [:corp :deck])))))}} card))}

   "Singularity"
   {:prompt "Choose a server" :choices (req (filter #(can-run-server? state %) remotes))
    :effect (effect (run target
                      {:req (req (is-remote? target))
                       :replace-access
                       {:msg "trash all cards in the server at no cost"
                        :mandatory true
                        :effect (req (let [allcorp (get-in (:servers corp) (conj (:server run) :content))]
                                       (doseq [c allcorp]
                                         (trash state side c))))}} card))}

   "Social Engineering"
   {:prompt "Choose an unrezzed piece of ICE"
    :choices {:req #(and (= (last (:zone %)) :ices) (not (rezzed? %)) (ice? %))}
    :effect (req (let [ice target
                       serv (zone->name (second (:zone ice)))]
              (resolve-ability
                 state :runner
                 {:msg (msg "choose the ICE at position " (ice-index state ice) " of " serv)
                  :effect (effect (register-events {:pre-rez-cost
                                                    {:req (req (= target ice))
                                                     :effect (req (let [cost (rez-cost state side (get-card state target))]
                                                                    (gain state :runner :credit cost)))
                                                     :msg (msg "gain " (rez-cost state side (get-card state target)) " [Credits]")}}
                                  (assoc card :zone '(:discard))))}
               card nil)))
    :events {:pre-rez-cost nil}
    :end-turn {:effect (effect (unregister-events card))}}

   "Special Order"
   {:prompt "Choose an Icebreaker"
    :effect (effect (trigger-event :searched-stack nil)
                    (shuffle! :deck)
                    (system-msg (str "adds " (:title target) " to their Grip and shuffles their Stack"))
                    (move target :hand))
    :choices (req (cancellable (filter #(has-subtype? % "Icebreaker") (:deck runner)) :sorted))}

   "Spooned"
   {:prompt "Choose a server" :choices (req runnable-servers) :effect (effect (run target nil card))}

   "Stimhack"
   {:prompt "Choose a server" :choices (req runnable-servers)
    :effect (effect (gain-run-credits 9)
                    (run target {:end-run
                                 {:msg " take 1 brain damage"
                                  :effect (effect (damage eid :brain 1 {:unpreventable true :card card}))}}
                      card))}

   "Sure Gamble"
   {:msg "gain 9 [Credits]" :effect (effect (gain :credit 9))}

   "Surge"
   {:msg (msg "place 2 virus tokens on " (:title target))
    :choices {:req #(and (has-subtype? % "Virus") (:added-virus-counter %))}
    :effect (req (add-counter state :runner target :virus 2))}

   "System Outage"
   {:events {:corp-draw {:req (req (not (first-event state side :corp-draw)))
                         :msg "force the Corp to lose 1 [Credits]"
                         :effect (effect (lose :corp :credit 1))}}}

   "Test Run"
   {:prompt "Install a program from your Stack or Heap?"
    :choices (cancellable ["Stack" "Heap"])
    :msg (msg "install a program from their " target)
    :effect (effect (resolve-ability
                      {:prompt "Choose a program to install"
                       :choices (req (cancellable
                                       (filter #(is-type? % "Program")
                                               ((if (= target "Heap") :discard :deck) runner))))
                       :effect (effect (trigger-event :searched-stack nil)
                                       (shuffle! :deck)
                                       (runner-install (assoc-in target [:special :test-run] true) {:no-cost true}))
                       :end-turn
                       {:req (req (get-in (find-cid (:cid target) (all-installed state :runner)) [:special :test-run]))
                        :msg (msg "move " (:title target) " to the top of their Stack")
                        :effect (req (move state side (find-cid (:cid target) (all-installed state :runner))
                                           :deck {:front true}))}}
                      card targets))}

   "The Makers Eye"
   {:effect (effect (run :rd nil card) (register-events (:events (card-def card))
                                                        (assoc card :zone '(:discard))))
    :events {:successful-run {:silent (req true)
                              :effect (effect (access-bonus 2))}
             :run-ends {:effect (effect (unregister-events card))}}}

   "The Noble Path"
   {:effect (req (doseq [c (:hand runner)]
                   (trash state side c))
                 (register-events state side
                                  {:pre-damage {:effect (effect (damage-prevent :net Integer/MAX_VALUE)
                                                                (damage-prevent :meat Integer/MAX_VALUE)
                                                                (damage-prevent :brain Integer/MAX_VALUE))}
                                   :run-ends {:effect (effect (unregister-events card))}}
                                  (assoc card :zone '(:discard)))
                 (resolve-ability state side
                   {:prompt "Choose a server"
                    :choices (req runnable-servers)
                    :msg (msg "trash their Grip and make a run on " target ", preventing all damage")
                    :effect (req (let [runtgt [(last (server->zone state target))]
                                       ices (get-in @state (concat [:corp :servers] runtgt [:ices]))]
                                   (swap! state assoc :per-run nil
                                                      :run {:server runtgt :position (count ices)
                                                            :access-bonus 0 :run-effect nil})
                                   (gain-run-credits state :runner (:bad-publicity corp))
                                   (swap! state update-in [:runner :register :made-run] #(conj % (first runtgt)))
                                   (trigger-event state :runner :run runtgt)))} card nil))
    :events {:pre-damage nil :run-ends nil}}

   "The Price of Freedom"
   {:req (req (some #(has-subtype? % "Connection") (all-installed state :runner)))
    :prompt "Choose an installed connection to trash"
    :choices {:req #(and (has-subtype? % "Connection") (installed? %))}
    :msg (msg "trash " (:title target) " to prevent the corp from advancing cards during their next turn")
    :effect (effect (move (find-cid (:cid card) (:discard runner)) :rfg)
                    (trash target)
                    (register-events (:events (card-def card)) (assoc card :zone '(:rfg)))
                    (register-persistent-flag!
                             card :can-advance
                             (fn [state side card]
                                 ((constantly false) (toast state :corp "Cannot advance cards this turn due to The Price of Freedom." "warning")))))
    :events {:corp-turn-ends {:effect (effect (clear-persistent-flag! card :can-advance)
                                              (unregister-events card))}}}

   "Three Steps Ahead"
   {:end-turn {:effect (effect (gain :credit (* 2 (count (:successful-run runner-reg)))))
               :msg (msg "gain " (* 2 (count (:successful-run runner-reg))) " [Credits]")}}

   "Tinkering"
   {:prompt "Choose a piece of ICE"
    :choices {:req #(and (= (last (:zone %)) :ices) (ice? %))}
    :effect (req (let [ice target
                       serv (zone->name (second (:zone ice)))
                       stypes (:subtype ice)]
              (resolve-ability
                 state :runner
                 {:msg (msg "make " (card-str state ice) " gain sentry, code gate, and barrier until the end of the turn")
                  :effect (effect (update! (assoc ice :subtype (combine-subtypes true (:subtype ice) "Sentry" "Code Gate" "Barrier")))
                                  (update-ice-strength (get-card state ice))
                                  (register-events {:runner-turn-ends
                                                    {:effect (effect (update! (assoc (get-card state ice) :subtype stypes)))}}
                                  (assoc card :zone '(:discard))))}
               card nil)))
    :events {:runner-turn-ends nil}}

   "Trade-In"
   {:prompt "Choose a hardware to trash"
    :choices {:req #(and (installed? %)
                         (is-type? % "Hardware"))}
    :msg (msg "trash " (:title target) " and gain " (quot (:cost target) 2) " [Credits]")
    :effect (effect (trash target) (gain [:credit (quot (:cost target) 2)])
                    (resolve-ability {:prompt "Choose a Hardware to add to your Grip from your Stack"
                                      :choices (req (filter #(is-type? % "Hardware")
                                                            (:deck runner)))
                                      :msg (msg "add " (:title target) " to their Grip")
                                      :effect (effect (trigger-event :searched-stack nil)
                                                      (shuffle! :deck)
                                                      (move target :hand))} card nil))}

   "Traffic Jam"
   {:effect (effect (update-all-advancement-costs))
    :leave-play (effect (update-all-advancement-costs))
    :events {:pre-advancement-cost
             {:effect (req (advancement-cost-bonus
                             state side (count (filter #(= (:title %) (:title target)) (:scored corp)))))}}}

   "Uninstall"
   {:choices {:req #(and (installed? %)
                         (#{"Program" "Hardware"} (:type %)))}
    :msg (msg "move " (:title target) " to their Grip")
    :effect (effect (move target :hand))}

   "Vamp"
   {:effect (effect (run :hq {:req (req (= target :hq))
                              :replace-access
                              {:prompt "How many [Credits]?" :choices :credit
                               :msg (msg "take 1 tag and make the Corp lose " target " [Credits]")
                               :effect (effect (lose :corp :credit target) (tag-runner 1))}} card))}

   "Wanton Destruction"
   {:effect (effect (run :hq {:req (req (= target :hq))
                              :replace-access
                              {:msg (msg "force the Corp to discard " target " cards from HQ at random")
                               :prompt "How many [Click] do you want to spend?"
                               :choices (req (map str (range 1 (inc (:click runner)))))
                               :effect (req (let [n (Integer/parseInt target)]
                                              (when (pay state :runner card :click n)
                                                (trash-cards state :corp (take n (shuffle (:hand corp)))))))}} card))}

   "Windfall"
   {:effect (effect (shuffle! :deck)
                    (resolve-ability
                      {:effect (req (let [topcard (first (:deck runner))
                                          cost (:cost topcard)]
                                      (trash state side topcard)
                                      (when-not (is-type? topcard "Event")
                                        (gain state side :credit cost))
                                      (system-msg state side
                                                  (str "shuffles their Stack and trashes " (:title topcard)
                                                       (when-not (is-type? topcard "Event")
                                                         (str " to gain " cost " [Credits]"))))))}
                     card nil))}})
