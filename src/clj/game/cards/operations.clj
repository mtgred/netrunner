(in-ns 'game.core)

(def cards-operations
  {"24/7 News Cycle"
   {:req (req (pos? (count (:scored corp))))
    :delayed-completion true
    :additional-cost [:forfeit]
    :effect (req (continue-ability
                   state side
                   {:prompt "Select an agenda in your score area to trigger its \"when scored\" ability"
                    :choices {:req #(and (is-type? % "Agenda")
                                         (when-scored? %)
                                         (is-scored? state :corp %))}
                    :msg (msg "trigger the \"when scored\" ability of " (:title target))
                    :delayed-completion true
                    ;dissoc :end-turn for Breaking News
                    :effect (effect (continue-ability (dissoc (card-def target) :end-turn) target nil))}
                   card nil))}

   "Accelerated Diagnostics"
   (letfn [(ad [i n adcard]
             {:prompt "Select an operation to play"
              :choices {:req #(and (= (:side %) "Corp")
                                   (is-type? % "Operation")
                                   (= (:zone %) [:play-area]))}
              :msg (msg "play " (:title target))
              :delayed-completion true
              :effect (req (when-completed (play-instant state side target {:no-additional-cost true})
                                           (if (and (not (get-in @state [:corp :register :terminal])) (< i n))
                                             (continue-ability state side (ad (inc i) n adcard) adcard nil)
                                             (effect-completed state side eid))))})]
     {:delayed-completion true
      :implementation "Corp has to manually cards back to R&D to correctly play a draw operation"
      :effect (req (let [n (count (filter #(is-type? % "Operation")
                                          (take 3 (:deck corp))))]
                     (continue-ability state side
                                       {:msg "look at the top 3 cards of R&D"
                                        :delayed-completion true
                                        :effect (req (doseq [c (take 3 (:deck corp))]
                                                       (move state side c :play-area))
                                                     (continue-ability state side (ad 1 n card) card nil))}
                                       card nil)))})

   "Ad Blitz"
   (let [abhelp (fn ab [n total]
                  {:prompt "Select an Advertisement to install and rez" :show-discard true
                   :delayed-completion true
                   :choices {:req #(and (= (:side %) "Corp")
                                        (has-subtype? % "Advertisement")
                                        (or (in-hand? %)
                                            (= (:zone %) [:discard])))}
                   :effect (req (when-completed
                                  (corp-install state side target nil {:install-state :rezzed})
                                  (if (< n total)
                                    (continue-ability state side (ab (inc n) total) card nil)
                                    (effect-completed state side eid))))})]
     {:prompt "How many Advertisements?"
      :delayed-completion true
      :choices :credit
      :msg (msg "install and rez " target " Advertisements")
      :effect (effect (continue-ability (abhelp 1 target) card nil))})

   "Aggressive Negotiation"
   {:req (req (:scored-agenda corp-reg)) :prompt "Choose a card"
    :choices (req (cancellable (:deck corp) :sorted))
    :effect (final-effect (move target :hand) (shuffle! :deck))
    :msg "search R&D for a card and add it to HQ"}

   "An Offer You Cant Refuse"
   {:delayed-completion false
    :prompt "Choose a server" :choices ["Archives" "R&D" "HQ"]
    :effect (req (let [serv target]
                   (show-wait-prompt state :corp (str "Runner to decide on running " target))
                   (continue-ability
                     state side
                     {:optional
                      {:prompt (msg "Make a run on " serv "?") :player :runner
                       :yes-ability {:msg (msg "let the Runner make a run on " serv)
                                     :effect (effect (clear-wait-prompt :corp)
                                                     (game.core/run eid serv nil card))}
                       :no-ability {:effect (effect (clear-wait-prompt :corp)
                                                    (as-agenda :corp (some #(when (= (:cid card) (:cid %)) %) (:discard corp)) 1))
                                    :msg "add it to their score area as an agenda worth 1 agenda point"}}}
                    card nil)))}

   "Anonymous Tip"
   {:msg "draw 3 cards"
    :delayed-completion true
    :effect (effect (draw eid 3 nil))}

   "Archived Memories"
   {:effect (req (let [cid (:cid card)]
                   (resolve-ability state side
                     {:prompt "Select a card from Archives to add to HQ"
                      :show-discard true
                      :choices {:req #(and (not= (:cid %) cid)
                                           (= (:side %) "Corp")
                                           (= (:zone %) [:discard]))}
                      :effect (final-effect (move target :hand)
                                            (system-msg (str "adds " (if (:seen target) (:title target) "an unseen card") " to HQ")))}
                    card nil)))}

   "Ark Lockdown"
   {:delayed-completion true
    :req (req (not-empty (:discard runner)))
    :prompt "Name a card to remove all copies in the Heap from the game"
    :choices (req (cancellable (:discard runner) :sorted))
    :msg (msg "remove all copies of " (:title target) " in the Heap from the game")
    :effect (req (doseq [c (filter #(= (:title target) (:title %)) (:discard runner))]
                   (move state :runner c :rfg))
                 (effect-completed state side eid card))}

   "Audacity"
   (let [audacity (fn au [n] {:prompt "Choose a card on which to place an advancement"
                              :delayed-completion true
                              :choices {:req can-be-advanced?}
                              :cancel-effect (req (effect-completed state side eid))
                              :msg (msg "place an advancement token on " (card-str state target))
                              :effect (req (add-prop state :corp target :advance-counter 1 {:placed true})
                                           (if (< n 2)
                                             (continue-ability state side (au (inc n)) card nil)
                                             (effect-completed state side eid card)))})]
   {:delayed-completion true
    :req (req (let [h (:hand corp)
                    p (:play-area corp)]
                ;; this is needed to pass the req check for can-play? and again when card is actually played
                (if (some #(= (:cid %) (:cid card)) p)
                  (>= (count h) 2)
                  (>= (count h) 3))))
    :effect (req (system-msg state side "trashes all cards in HQ due to Audacity")
                 (doseq [c (:hand corp)]
                   (trash state side c {:unpreventable true}))
                 (continue-ability state side (audacity 1) card nil))})

   "Back Channels"
   {:prompt "Select an installed card in a server to trash"
    :choices {:req #(and (= (last (:zone %)) :content)
                         (is-remote? (second (:zone %))))}
    :effect (final-effect (gain :credit (* 3 (get target :advance-counter 0))) (trash target))
    :msg (msg "trash " (card-str state target) " and gain "
              (* 3 (get target :advance-counter 0)) " [Credits]")}

   "Bad Times"
   {:implementation "Any required program trashing is manual"
    :req (req tagged)
    :msg "force the Runner to lose 2[mu] until the end of the turn"
    :effect (req (lose state :runner :memory 2)
                 (when (< (:memory runner) 0)
                  (system-msg state :runner "must trash programs to free up [mu]")))
    :end-turn {:effect (req (gain state :runner :memory 2)
                            (system-msg state :runner "regains 2[mu]"))}}

   "Beanstalk Royalties"
   {:msg "gain 3 [Credits]"
    :effect (effect (gain :credit 3))}

   "Best Defense"
   {:delayed-completion true
    :req (req (not-empty (all-installed state :runner)))
    :effect (req (let [t (:tag runner)]
                   (continue-ability state side
                     {:prompt (msg "Choose a Runner card with an install cost of " t " or less to trash")
                      :choices {:req #(and (installed? %)
                                           (<= (:cost %) t))}
                      :msg (msg "trash " (:title target))
                      :effect (effect (trash target))}
                    card nil)))}

   "Biased Reporting"
   {:delayed-completion true
    :req (req (not-empty (all-installed state :runner)))
    :prompt "Choose a card type"
    :choices ["Resource" "Hardware" "Program"]
    :effect (req (let [t target
                       num (count (filter #(is-type? % t) (all-installed state :runner)))]
                   (show-wait-prompt state :corp "Runner to choose cards to trash")
                   (when-completed
                     (resolve-ability state :runner
                       {:prompt (msg "Choose any number of cards of type " t " to trash")
                        :choices {:max num :req #(and (installed? %) (is-type? % t))}
                        :cancel-effect (final-effect (clear-wait-prompt :corp))
                        :effect (req (doseq [c targets]
                                       (trash state :runner c {:unpreventable true}))
                                     (gain state :runner :credit (count targets))
                                     (system-msg state :runner (str "trashes " (join ", " (map :title (sort-by :title targets)))
                                                                    " and gains " (count targets) " [Credits]"))
                                     (clear-wait-prompt state :corp))}
                      card nil)
                     (do (let [n (* 2 (count (filter #(is-type? % t) (all-installed state :runner))))]
                           (when (pos? n)
                             (gain state :corp :credit n)
                             (system-msg state :corp (str "uses Biased Reporting to gain " n " [Credits]")))
                           (effect-completed state side eid))))))}

   "Big Brother"
   {:req (req tagged)
    :msg "give the Runner 2 tags"
    :delayed-completion true
    :effect (effect (tag-runner :runner eid 2))}

   "Bioroid Efficiency Research"
   {:implementation "Derez is manual"
    :choices {:req #(and (ice? %)
                         (has-subtype? % "Bioroid")
                         (installed? %)
                         (not (rezzed? %)))}
    :msg (msg "rez " (card-str state target {:visible true}) " at no cost")
    :effect (final-effect (rez target {:ignore-cost :all-costs})
                          (host (get-card state target) (assoc card :zone [:discard] :seen true :condition true)))}

   "Biotic Labor"
   {:msg "gain [Click][Click]"
    :effect (effect (gain :click 2))}

   "Blue Level Clearance"
   {:msg "gain 5 [Credits] and draw 2 cards"
    :delayed-completion true
    :effect (effect (gain :credit 5)
                    (draw eid 2 nil))}

   "BOOM!"
   {:req (req (> (:tag runner) 1))
    :delayed-completion true
    :msg "do 7 meat damage"
    :effect (effect (damage eid :meat 7 {:card card}))}

   "Casting Call"
   {:choices {:req #(and (is-type? % "Agenda")
                         (in-hand? %))}
    :delayed-completion true
    :effect (req (let [agenda target]
                   (continue-ability
                     state side {:prompt (str "Choose a server to install " (:title agenda))
                                 :choices (server-list state agenda)
                                 :effect (req (corp-install state side agenda target {:install-state :face-up})
                                              ; find where the agenda ended up and host on it
                                              (let [agenda (some #(when (= (:cid %) (:cid agenda)) %)
                                                                 (all-installed state :corp))]
                                                ; the operation ends up in :discard when it is played; to host it,
                                                ; we need (host) to look for it in discard.
                                                (host state side agenda (assoc card :zone [:discard]
                                                                                    :seen true :installed true))
                                                (system-msg state side (str "hosts Casting Call on " (:title agenda)))))}
                     card nil)))
    :events {:access {:req (req (= (:cid target) (:cid (:host card))))
                      :delayed-completion true
                      :effect (effect (tag-runner :runner eid 2)) :msg "give the Runner 2 tags"}}}

   "Celebrity Gift"
   {:choices {:max 5
              :req #(and (= (:side %) "Corp")
                         (in-hand? %))}
    :msg (msg "reveal " (join ", " (map :title (sort-by :title targets))) " and gain " (* 2 (count targets)) " [Credits]")
    :effect (final-effect (gain :credit (* 2 (count targets))))}

   "Cerebral Cast"
   {:req (req (last-turn? state :runner :successful-run))
    :psi {:not-equal {:player :runner :prompt "Take 1 tag or 1 brain damage?"
                      :choices ["1 tag" "1 brain damage"] :msg (msg "give the Runner " target)
                      :delayed-completion true
                      :effect (req (if (= target "1 tag")
                                     (tag-runner state side eid 1)
                                     (damage state side eid :brain 1 {:card card})))}}}

   "Cerebral Static"
   {:msg "disable the Runner's identity"
    :effect (effect (disable-identity :runner))
    :leave-play (effect (enable-identity :runner))}

   "\"Clones are not People\""
   {:events {:agenda-scored {:msg "add it to their score area as an agenda worth 1 agenda point"
                             :effect (effect (as-agenda :corp card 1))}}}

   "Closed Accounts"
   {:req (req tagged)
    :msg (msg "force the Runner to lose all " (:credit runner) " [Credits]")
    :effect (effect (lose :runner :credit :all))}

   "Commercialization"
   {:msg (msg "gain " (or (:advance-counter target) 0) " [Credits]")
    :choices {:req ice?}
    :effect (final-effect (gain :credit (or (:advance-counter target) 0)))}

   "Consulting Visit"
   {:prompt  "Choose an Operation from R&D to play"
    :choices (req (cancellable
             (filter #(and (is-type? % "Operation")
                           (<= (:cost %) (:credit corp)))
                      (:deck corp))
             :sorted))
    :effect  (effect (shuffle! :deck)
                     (system-msg "shuffles their deck")
                     (play-instant target))
    :msg (msg "search R&D for " (:title target) " and play it")}

   "Corporate Shuffle"
   {:msg "shuffle all cards in HQ into R&D and draw 5 cards"
    :delayed-completion true
    :effect (effect (shuffle-into-deck :hand)
                    (draw eid 5 nil))}

   "Cyberdex Trial"
   {:msg "purge virus counters"
    :effect (effect (purge))}

   "Death and Taxes"
   (let [gain-cred-effect {:msg "gain 1 [Credits]"
                           :effect (effect (gain :corp :credit 1))}]
     {:implementation "Credit gain mandatory to save on wait-prompts, adjust credits manually if credit not wanted."
      :events {:runner-install gain-cred-effect
               :runner-trash (assoc gain-cred-effect :req (req (installed? target)))}})

   "Dedication Ceremony"
   {:prompt "Select a faceup card"
    :choices {:req #(or (and (card-is? % :side :corp)
                             (:rezzed %))
                        (and (card-is? % :side :runner)
                             (or (installed? %)
                                 (:host %))
                             (not (facedown? %))))}
    :msg (msg "place 3 advancement tokens on " (card-str state target))
    :effect (req (add-prop state :corp target :advance-counter 3 {:placed true})
                 (effect-completed state side eid card)
                 (let [tgtcid (:cid target)]
                   (register-turn-flag! state side
                     target :can-score
                     (fn [state side card]
                       (if (and (= (:cid card) tgtcid)
                                (>= (:advance-counter card) (or (:current-cost card) (:advancementcost card))))
                         ((constantly false) (toast state :corp "Cannot score due to Dedication Ceremony." "warning"))
                         true)))))}

   "Defective Brainchips"
   {:events {:pre-damage {:req (req (= target :brain)) :msg "do 1 additional brain damage"
                          :once :per-turn :effect (effect (damage-bonus :brain 1))}}}

   "Distract the Masses"
   (let [shuffle-two {:delayed-completion true
                      :effect (effect (rfg-and-shuffle-rd-effect (find-cid (:cid card) (:discard corp)) 2))}
         trash-from-hq {:delayed-completion true
                        :prompt "Select up to 2 cards in HQ to trash"
                        :choices {:max 2
                                  :req #(and (= (:side %) "Corp")
                                             (in-hand? %))}
                        :msg (msg "trash " (quantify (count targets) "card") " from HQ")
                        :effect (req (when-completed
                                       (trash-cards state side targets nil)
                                       (continue-ability state side shuffle-two card nil)))
                        :cancel-effect (req (continue-ability state side shuffle-two card nil))}]
     {:delayed-completion true
      :msg "give The Runner 2 [Credits]"
      :effect (effect (gain :runner :credit 2)
                      (continue-ability trash-from-hq card nil))})

   "Diversified Portfolio"
   {:msg (msg "gain " (count (filter #(not (empty? %)) (map #(:content (second %)) (get-remotes @state))))
              " [Credits]")
    :effect (effect (gain :credit (count (filter #(not (empty? %)) (map #(:content (second %)) (get-remotes @state))))))}

   "Door to Door"
   {:events {:runner-turn-begins
             {:trace {:base 1 :msg (msg (if tagged "do 1 meat damage" "give the Runner 1 tag"))
                      :label "Do 1 meat damage if Runner is tagged, or give the Runner 1 tag"
                      :delayed-completion true
                      :effect (req (if tagged
                                     (damage state side eid :meat 1 {:card card})
                                     (tag-runner state :runner eid 1)))}}}}

   "Economic Warfare"
   {:req (req (and (last-turn? state :runner :successful-run)
                   (can-pay? state :runner nil :credit 4)))
    :msg "make the runner lose 4 [Credits]"
    :effect (effect (lose :runner :credit 4))}

   "Election Day"
   {:req (req (->> (get-in @state [:corp :hand])
                   (filter #(not (= (:cid %) (:cid card))))
                   (count)
                   (pos?)))
    :delayed-completion true
    :msg (msg "trash all cards in HQ and draw 5 cards")
    :effect (effect (trash-cards (get-in @state [:corp :hand]))
                    (draw eid 5 nil))}

   "Enforced Curfew"
   {:msg "reduce the Runner's maximum hand size by 1"
    :effect (effect (lose :runner :hand-size-modification 1))
    :leave-play (effect (gain :runner :hand-size-modification 1))}

   "Enforcing Loyalty"
   {:trace {:base 3
            :label "Trash a card not matching the faction of the Runner's identity"
            :delayed-completion true
            :effect (req (let [f (:faction (:identity runner))]
                           (continue-ability
                             state side
                             {:prompt "Select an installed card not matching the faction of the Runner's identity"
                              :choices {:req #(and (installed? %) (not= f (:faction %)) (card-is? % :side :runner))}
                              :msg (msg "trash " (:title target))
                              :effect (effect (trash target))}
                            card nil)))}}

   "Enhanced Login Protocol"
   (letfn [(elp-activated [state]
             (get-in @state [:corp :register :elp-activated] false))
           (add-effect [state side]
             (swap! state assoc-in [:corp :register :elp-activated] true)
             (click-run-cost-bonus state side [:click 1]))
           (remove-effect [state side]
             (click-run-cost-bonus state side [:click -1])
             (swap! state update-in [:corp :register] dissoc :elp-activated))]
     {:effect (req (when (and (= :runner (:active-player @state))
                              (not (:made-click-run runner-reg)))
                     (add-effect state side)
                     (system-msg state side (str "uses Enhanced Login Protocol to add an additional cost of [Click]"
                                                 " to make the first run not through a card ability this turn"))))
      :events {:runner-turn-begins {:msg "add an additional cost of [Click] to make the first run not through a card ability this turn"
                                    :effect (effect (add-effect))}
               :runner-turn-ends {:req (req (elp-activated state))
                                  :effect (effect (remove-effect))}
               :run-ends {:req (req (and (elp-activated state)
                                         (:made-click-run runner-reg)))
                          :effect (effect (remove-effect))}}
      :leave-play (req (when (elp-activated state)
                         (remove-effect state side)))})

   "Exchange of Information"
   {:req (req (and tagged
                   (seq (:scored runner))
                   (seq (:scored corp))))
    :delayed-completion true
    :effect (req
              (continue-ability
                state side
                {:prompt "Select a stolen agenda in the Runner's score area to swap"
                 :choices {:req #(in-runner-scored? state side %)}
                 :delayed-completion true
                 :effect (req
                           (let [stolen target]
                             (continue-ability
                               state side
                               {:prompt (msg "Select a scored agenda to swap for " (:title stolen))
                                :choices {:req #(in-corp-scored? state side %)}
                                :effect (req (let [scored target]
                                               (swap-agendas state side scored stolen)
                                               (system-msg state side (str "uses Exchange of Information to swap "
                                                                           (:title scored) " for " (:title stolen)))
                                               (effect-completed state side eid card)))}
                               card nil)))}
                card nil))}

   "Fast Track"
   {:prompt "Choose an Agenda"
    :choices (req (cancellable (filter #(is-type? % "Agenda") (:deck corp)) :sorted))
    :effect (effect (system-msg (str "adds " (:title target) " to HQ and shuffle R&D"))
                    (shuffle! :deck)
                    (move target :hand) )}

   "Financial Collapse"
   {:delayed-completion true
    :req (req (and (>= (:credit runner) 6) (seq (filter #(is-type? % "Resource") (all-installed state :runner)))))
    :effect (req (let [rcount (count (filter #(is-type? % "Resource") (all-installed state :runner)))]
                   (if (pos? rcount)
                     (do (show-wait-prompt state :corp "Runner to trash a resource to prevent Financial Collapse")
                         (continue-ability
                           state side
                           {:prompt (msg "Trash a resource to prevent Financial Collapse?")
                            :choices ["Yes" "No"] :player :runner
                            :delayed-completion true
                            :effect (final-effect (continue-ability
                                                    (if (= target "Yes")
                                                      {:player :runner
                                                       :prompt "Select a resource to trash"
                                                       :choices {:req #(and (is-type? % "Resource") (installed? %))}
                                                       :effect (req (trash state side target {:unpreventable true})
                                                                    (system-msg state :runner
                                                                                (str "trashes " (:title target)
                                                                                     " to prevent Financial Collapse"))
                                                                    (clear-wait-prompt state :corp))}
                                                      {:effect (effect (lose :runner :credit (* rcount 2))
                                                                       (clear-wait-prompt :corp))
                                                       :msg (msg "make the Runner lose " (* rcount 2) " [Credits]")})
                                                   card nil))} card nil))
                     (continue-ability
                       state side
                       {:effect (effect (lose :runner :credit (* rcount 2)))
                        :msg (msg "make the Runner lose " (* rcount 2) " [Credits]")} card nil))))}

   "Foxfire"
   {:trace {:base 7
            :prompt "Select 1 card to trash"
            :not-distinct true
            :choices {:req #(and (installed? %)
                                 (or (has-subtype? % "Virtual")
                                     (has-subtype? % "Link")))}
            :msg "trash 1 virtual resource or link"
            :effect (effect (trash target) (system-msg (str "trashes " (:title target))))}}

   "Freelancer"
   {:req (req tagged)
    :msg (msg "trash " (join ", " (map :title (sort-by :title targets))))
    :choices {:max 2
              :req #(and (installed? %)
                         (is-type? % "Resource"))}
    :effect (final-effect (trash-cards :runner targets))}

   "Friends in High Places"
   (let [fhelper (fn fhp [n] {:prompt "Select a card in Archives to install with Friends in High Places"
                              :priority -1
                              :delayed-completion true
                              :show-discard true
                              :choices {:req #(and (= (:side %) "Corp")
                                                   (not (is-type? % "Operation"))
                                                   (in-discard? %))}
                              :effect (req (when-completed
                                             (corp-install state side target nil nil)
                                             (do (system-msg state side (str "uses Friends in High Places to "
                                                                             (corp-install-msg target)))
                                                 (if (< n 2)
                                                   (continue-ability state side (fhp (inc n)) card nil)
                                                   (effect-completed state side eid card)))))})]
     {:delayed-completion true
      :effect (effect (continue-ability (fhelper 1) card nil))})

   "Genotyping"
   {:delayed-completion true
    :effect (effect (mill 2)
                    (system-msg "trashes the top 2 cards of R&D")
                    (rfg-and-shuffle-rd-effect eid (first (:play-area corp)) 4))}

   "Green Level Clearance"
   {:msg "gain 3 [Credits] and draw 1 card"
    :delayed-completion true
    :effect (effect (gain :credit 3)
                    (draw eid 1 nil))}

   "Hard-Hitting News"
   {:req (req (last-turn? state :runner :made-run))
    :trace {:base 4
            :delayed-completion true
            :msg "give the Runner 4 tags"
            :label "Give the Runner 4 tags"
            :effect (effect (tag-runner :runner eid 4))}}

   "Hasty Relocation"
   (letfn [(hr-final [chosen original]
             {:prompt (str "The top cards of R&D will be " (clojure.string/join  ", " (map :title chosen)) ".")
              :choices ["Done" "Start over"]
              :delayed-completion true
              :effect (req (if (= target "Done")
                             (do (doseq [c (reverse chosen)] (move state :corp c :deck {:front true}))
                                 (clear-wait-prompt state :runner)
                                 (effect-completed state side eid card))
                             (continue-ability state side (hr-choice original '() 3 original)
                                               card nil)))})
           (hr-choice [remaining chosen n original]
             {:prompt "Choose a card to move next onto R&D"
              :choices remaining
              :delayed-completion true
              :effect (req (let [chosen (cons target chosen)]
                             (if (< (count chosen) n)
                               (continue-ability state side (hr-choice (remove-once #(not= target %) remaining)
                                                                        chosen n original) card nil)
                               (continue-ability state side (hr-final chosen original) card nil))))})]
     {:additional-cost [:mill 1]
      :delayed-completion true
      :msg "trash the top card of R&D, draw 3 cards, and add 3 cards in HQ to the top of R&D"
      :effect (req (when-completed (draw state side 3 nil)
                                   (do (show-wait-prompt state :runner "Corp to add 3 cards in HQ to the top of R&D")
                                       (let [from (get-in @state [:corp :hand])]
                                         (continue-ability state :corp (hr-choice from '() 3 from) card nil)))))})

   "Hatchet Job"
   {:trace {:base 5
            :choices {:req #(and (installed? %)
                                 (card-is? % :side :runner)
                                 (not (has-subtype? % "Virtual")))}
            :msg "add an installed non-virtual card to the Runner's grip"
            :effect (effect (move :runner target :hand true))}}

   "Hedge Fund"
   {:msg "gain 9 [Credits]" :effect (effect (gain :credit 9))}

   "Hellion Alpha Test"
   {:req (req (last-turn? state :runner :installed-resource))
    :trace {:base 2
            :choices {:req #(and (installed? %)
                                 (is-type? % "Resource"))}
            :msg "add a Resource to the top of the Stack"
            :effect (effect (move :runner target :deck {:front true})
                            (system-msg (str "adds " (:title target) " to the top of the Stack")))
            :unsuccessful {:msg "take 1 bad publicity"
                           :effect (effect (gain :corp :bad-publicity 1))}}}

   "Hellion Beta Test"
   {:req (req (last-turn? state :runner :trashed-card))
    :trace {:base 2
            :label "Trash 2 installed non-program cards"
            :choices {:max (req (min 2 (count (filter #(not (is-type? % "Program")) (concat (all-installed state :corp)
                                                                                            (all-installed state :runner))))))
                      :all true
                      :req #(and (installed? %)
                                 (not (is-type? % "Program")))}
            :msg (msg "trash " (join ", " (map :title (sort-by :title targets))))
            :effect (req (doseq [c targets]
                           (trash state side c)))
            :unsuccessful {:msg "take 1 bad publicity"
                           :effect (effect (gain :corp :bad-publicity 1))}}}

   "Heritage Committee"
   {:delayed-completion true
    :effect (req (when-completed (draw state side 3 nil)
                                 (continue-ability state side
                                   {:prompt "Select a card in HQ to put on top of R&D"
                                    :choices {:req #(and (= (:side %) "Corp")
                                                         (in-hand? %))}
                                    :msg "draw 3 cards and add 1 card from HQ to the top of R&D"
                                    :effect (effect (move target :deck {:front true}))}
                                   card nil)))}

   "Housekeeping"
   {:events {:runner-install {:player :runner
                              :prompt "Select a card from your Grip to trash for Housekeeping" :once :per-turn
                              :choices {:req #(and (= (:side %) "Runner")
                                                   (in-hand? %))}
                              :msg (msg "force the Runner to trash " (:title target) " from their Grip")
                              :effect (effect (trash target {:unpreventable true}))}}}

   "Hunter Seeker"
   {:req (req (last-turn? state :runner :stole-agenda))
    :delayed-completion true
    :prompt "Choose a card to trash"
    :choices {:req installed?}
    :msg (msg "trash " (card-str state target))
    :effect (effect (trash target))}

   "Interns"
   {:prompt "Select a card to install from Archives or HQ"
    :show-discard true
    :not-distinct true
    :choices {:req #(and (not (is-type? % "Operation"))
                         (= (:side %) "Corp")
                         (#{[:hand] [:discard]} (:zone %)))}
    :effect (final-effect (corp-install target nil {:no-install-cost true}))
    :msg (msg (corp-install-msg target))}

   "Invasion of Privacy"
   (letfn [(iop [x]
             {:delayed-completion true
              :req (req (pos? (count (filter #(or (is-type? % "Resource")
                                                  (is-type? % "Event")) (:hand runner)))))
              :prompt "Choose a resource or event to trash"
              :msg (msg "trash " (:title target))
              :choices (req (cancellable
                              (filter #(or (is-type? % "Resource")
                                           (is-type? % "Event")) (:hand runner)) :sorted))
              :effect (req (trash state side target)
                           (if (pos? x)
                             (continue-ability state side (iop (dec x)) card nil)
                             (effect-completed state side eid card)))})]
     {:trace {:base 2 :msg "reveal the Runner's Grip and trash up to X resources or events"
              :effect (req (let [x (- target (second targets))]
                             (system-msg state :corp
                                         (str "reveals the Runner's Grip ( "
                                              (join ", " (map :title (sort-by :title (:hand runner))))
                                              " ) and can trash up to " x " resources or events"))
                             (continue-ability state side (iop (dec x)) card nil)))
              :unsuccessful {:msg "take 1 bad publicity" :effect (effect (gain :corp :bad-publicity 1))}}})

   "IPO"
   {:msg "gain 13 [Credits]" :effect (effect (gain :credit 13))}

   "Lag Time"
   {:effect (effect (update-all-ice))
    :events {:pre-ice-strength {:effect (effect (ice-strength-bonus 1 target))}}
    :leave-play (effect (update-all-ice))}

   "Lateral Growth"
   {:delayed-completion true
    :msg "gain 4 [Credits]"
    :effect (effect (gain :credit 4)
                    (continue-ability {:player :corp
                                       :prompt "Select a card to install"
                                       :choices {:req #(and (= (:side %) "Corp")
                                                            (not (is-type? % "Operation"))
                                                            (in-hand? %))}
                                       :delayed-completion true
                                       :msg (msg (corp-install-msg target))
                                       :effect (effect (corp-install eid target nil nil))}
                                      card nil))}

   "Liquidation"
   {:delayed-completion true
    :effect (req (let [n (count (filter #(and (rezzed? %)
                                              (not (is-type? % "Agenda"))) (all-installed state :corp)))]
                   (continue-ability state side
                     {:prompt "Select any number of rezzed cards to trash"
                      :choices {:max n
                                :req #(and (rezzed? %)
                                           (not (is-type? % "Agenda")))}
                      :msg (msg "trash " (join ", " (map :title (sort-by :title targets))) " and gain " (* (count targets) 3) " [Credits]")
                      :effect (req (doseq [c targets]
                                     (trash state side c))
                                   (gain state side :credit (* (count targets) 3)))}
                    card nil)))}

   "Load Testing"
   {:msg "make the Runner lose [Click] when their next turn begins"
    :effect (effect (register-events (:events (card-def card))
                                     (assoc card :zone '(:discard))))
    :events {:runner-turn-begins {:msg "make the Runner lose [Click]"
                                  :effect (effect (lose :runner :click 1)
                                                  (unregister-events card))}}}

   "Localized Product Line"
   {:prompt "Choose a card"
    :choices (req (cancellable (:deck corp) :sorted))
    :delayed-completion true
    :effect (req (let [c (:title target)
                       cs (filter #(= (:title %) c) (:deck corp))]
                   (continue-ability
                    state side
                    {:prompt "How many copies?"
                     :choices {:number (req (count cs))}
                     :msg (msg "add " (quantify target "cop" "y" "ies") " of " c " to HQ")
                     :effect (req (shuffle! state :corp :deck)
                                  (doseq [c (take target cs)]
                                    (move state side c :hand)))}
                    card nil)))}

   "Manhunt"
   {:events {:successful-run {:interactive (req true)
                              :req (req (first-event? state side :successful-run))
                              :trace {:base 2 :msg "give the Runner 1 tag"
                                      :delayed-completion true
                                      :effect (effect (tag-runner :runner eid 1))}}}}

   "Mass Commercialization"
   {:msg (msg "gain " (* 2 (count (filter #(pos? (+ (:advance-counter % 0) (:extra-advance-counter % 0)))
                                          (concat (all-installed state :runner) (all-installed state :corp))))) " [Credits]")
    :effect (effect (gain :credit (* 2 (count (filter #(pos? (+ (:advance-counter % 0) (:extra-advance-counter % 0)))
                                                      (concat (all-installed state :runner) (all-installed state :corp)))))))}

   "MCA Informant"
   {:implementation "Runner must deduct 1 click and 2 credits, then trash host manually"
    :req (req (not-empty (filter #(has-subtype? % "Connection") (all-installed state :runner))))
    :prompt "Choose a connection to host MCA Informant on it"
    :choices {:req #(and (= (:side %) "Runner") (has-subtype? % "Connection") (installed? %))}
    :msg (msg "host it on " (card-str state target) ". The Runner has an additional tag")
    :effect (req (host state side (get-card state target) (assoc card :zone [:discard] :seen true))
                 (swap! state update-in [:runner :tag] inc))
    :leave-play (req (swap! state update-in [:runner :tag] dec)
                     (system-msg state :corp "trashes MCA Informant"))}

   "Medical Research Fundraiser"
   {:msg "gain 8 [Credits]. The Runner gains 3 [Credits]"
    :effect (effect (gain :credit 8) (gain :runner :credit 3))}

   "Midseason Replacements"
   {:req (req (last-turn? state :runner :stole-agenda))
    :trace {:base 6
            :msg "give the Runner X tags"
            :label "Give the Runner X tags"
            :delayed-completion true
            :effect (effect (system-msg (str "gives the Runner " (- target (second targets)) " tags"))
                            (tag-runner :runner eid (- target (second targets))))}}

   "Mushin No Shin"
   {:prompt "Select a card to install from HQ"
    :choices {:req #(and (#{"Asset" "Agenda" "Upgrade"} (:type %))
                         (= (:side %) "Corp")
                         (in-hand? %))}
    :effect (req (corp-install state side (assoc target :advance-counter 3) "New remote")
                 (effect-completed state side eid card)
                 (let [tgtcid (:cid target)]
                   (register-turn-flag! state side
                     card :can-rez
                     (fn [state side card]
                       (if (= (:cid card) tgtcid)
                         ((constantly false) (toast state :corp "Cannot rez due to Mushin No Shin." "warning"))
                         true)))
                   (register-turn-flag! state side
                     card :can-score
                     (fn [state side card]
                       (if (and (= (:cid card) tgtcid)
                                (>= (:advance-counter card) (or (:current-cost card) (:advancementcost card))))
                         ((constantly false) (toast state :corp "Cannot score due to Mushin No Shin." "warning"))
                         true)))))}

   "Mutate"
   {:additional-cost [:ice 1]
    :effect (effect (register-events (:events (card-def card)) (assoc card :zone '(:discard))))

    :events {:corp-trash {:effect (req (let [i (ice-index state target)
                       [reveal r] (split-with (complement ice?) (get-in @state [:corp :deck]))
                       titles (->> (conj (vec reveal) (first r)) (filter identity) (map :title))]
                                           (system-msg state side (str "uses Mutate to trash " (:title target)))
                                           (when (seq titles)
                                             (system-msg state side (str "reveals " (clojure.string/join ", " titles) " from R&D")))
                                           (if-let [ice (first r)]
                                             (let [newice (assoc ice :zone (:zone target) :rezzed true)
                                                   ices (get-in @state (cons :corp (:zone target)))
                                                   newices (apply conj (subvec ices 0 i) newice (subvec ices i))]
                                               (swap! state assoc-in (cons :corp (:zone target)) newices)
                                               (swap! state update-in [:corp :deck] (fn [coll] (remove-once #(not= (:cid %) (:cid newice)) coll)))
                                               (trigger-event state side :corp-install newice)
                                               (card-init state side newice {:resolve-effect false})
                                               (system-msg state side (str "uses Mutate to install and rez " (:title newice) " from R&D at no cost"))
                                               (trigger-event state side :rez newice))
                                             (system-msg state side (str "does not find any ICE to install from R&D")))
                                           (shuffle! state :corp :deck)
                                           (effect-completed state side eid card)
                                           (unregister-events state side card)))}}}

   "Neural EMP"
   {:req (req (last-turn? state :runner :made-run))
    :msg "do 1 net damage"
    :effect (effect (damage eid :net 1 {:card card}))}

   "O₂ Shortage"
   {:delayed-completion true
    :effect (req (if (empty? (:hand runner))
                   (do (gain state :corp :click 2)
                       (system-msg state side (str "uses O₂ Shortage to gain [Click][Click]"))
                       (effect-completed state side eid))
                   (do (show-wait-prompt state :corp "Runner to decide whether or not to trash a card from their Grip")
                       (continue-ability state side
                         {:optional
                          {:prompt "Trash 1 random card from your Grip?"
                           :player :runner
                           :yes-ability {:effect (effect (trash-cards :runner (take 1 (shuffle (:hand runner))))
                                                         (clear-wait-prompt :corp))}
                           :no-ability {:msg "gain [Click][Click]"
                                        :effect (effect (gain :corp :click 2)
                                                        (clear-wait-prompt :corp))}}}
                        card nil))))}

   "Observe and Destroy"
   {:additional-cost [:tag 1]
    :req (req (and (pos? (:tag runner))
                   (< (:credit runner) 6)))
    :delayed-completion true
    :effect (effect (continue-ability
                      {:prompt "Select an installed card to trash"
                       :choices {:req installed?}
                       :msg (msg "remove 1 Runner tag and trash " (:title target))
                       :effect (effect (trash target))}
                     card nil))}

   "Oversight AI"
   {:implementation "Trashing ICE is manual"
    :choices {:req #(and (ice? %) (not (rezzed? %)) (= (last (:zone %)) :ices))}
    :msg (msg "rez " (:title target) " at no cost")
    :effect (final-effect (rez target {:ignore-cost :all-costs})
                          (host (get-card state target) (assoc card :zone [:discard] :seen true :condition true)))}

   "Patch"
   {:choices {:req #(and (ice? %) (rezzed? %))}
    :msg (msg "give +2 strength to " (card-str state target))
    :effect (final-effect (host target (assoc card :zone [:discard] :seen true :condition true))
                          (update-ice-strength (get-card state target)))
    :events {:pre-ice-strength {:req (req (= (:cid target) (:cid (:host card))))
                                :effect (effect (ice-strength-bonus 2 target))}}}

   "Paywall Implementation"
   {:events {:successful-run {:msg "gain 1 [Credits]" :effect (effect (gain :corp :credit 1))}}}

   "Peak Efficiency"
   {:msg (msg "gain " (reduce (fn [c server]
                                (+ c (count (filter (fn [ice] (:rezzed ice)) (:ices server)))))
                              0 (flatten (seq (:servers corp))))
              " [Credits]")
    :effect (effect (gain :credit
                          (reduce (fn [c server]
                                    (+ c (count (filter (fn [ice] (:rezzed ice)) (:ices server)))))
                                  0 (flatten (seq (:servers corp))))))}

   "Power Grid Overload"
   {:req (req (last-turn? state :runner :made-run))
    :trace {:base 2
            :msg "trash 1 piece of hardware"
            :delayed-completion true
            :effect (req (let [max-cost (- target (second targets))]
                           (continue-ability state side
                                             {:choices {:req #(and (is-type? % "Hardware")
                                                                   (<= (:cost %) max-cost))}
                                              :msg (msg "trash " (:title target))
                                              :effect (effect (trash target))}
                                             card nil))
                         (system-msg state :corp (str "trashes 1 piece of hardware with install cost less than or equal to " (- target (second targets)))))}}

   "Power Shutdown"
   {:req (req (last-turn? state :runner :made-run))
    :prompt "Trash how many cards from the top R&D?"
    :choices {:number (req (apply max (map :cost (filter #(or (= "Program" (:type %)) (= "Hardware" (:type %))) (all-installed state :runner)))))}
    :msg (msg "trash " target " cards from the top of R&D")
    :delayed-completion true
    :effect (req (mill state :corp target)
                 (let [n target]
                   (continue-ability state :runner
                                     {:prompt "Select a Program or piece of Hardware to trash"
                                      :choices {:req #(and (#{"Hardware" "Program"} (:type %))
                                                           (<= (:cost %) n))}
                                      :msg (msg "trash " (:title target))
                                      :effect (effect (trash target))}
                                    card nil)))}

   "Precognition"
   {:delayed-completion true
    :msg "rearrange the top 5 cards of R&D"
    :effect (req (show-wait-prompt state :runner "Corp to rearrange the top cards of R&D")
                 (let [from (take 5 (:deck corp))]
                   (if (pos? (count from))
                     (continue-ability state side (reorder-choice :corp :runner from '()
                                                                  (count from) from) card nil)
                     (do (clear-wait-prompt state :runner)
                         (effect-completed state side eid card)))))}

   "Predictive Algorithm"
   {:events {:pre-steal-cost {:effect (effect (steal-cost-bonus [:credit 2]))}}}

   "Preemptive Action"
   {:effect (effect (rfg-and-shuffle-rd-effect (first (:play-area corp)) 3))}

   "Priority Construction"
   (letfn [(install-card [chosen]
            {:prompt "Select a remote server"
             :choices (req (conj (vec (get-remote-names @state)) "New remote"))
             :delayed-completion true
             :effect (effect (corp-install (assoc chosen :advance-counter 3) target {:no-install-cost true}))})]
     {:delayed-completion true
      :prompt "Choose a piece of ICE in HQ to install"
      :choices {:req #(and (in-hand? %) (= (:side %) "Corp") (ice? %))}
      :msg "install an ICE from HQ and place 3 advancements on it"
      :cancel-effect (req (effect-completed state side eid))
      :effect (effect (continue-ability (install-card target) card nil))})

   "Product Recall"
   {:prompt "Select a rezzed asset or upgrade to trash"
    :choices {:req #(and (rezzed? %)
                         (or (is-type? % "Asset") (is-type? % "Upgrade")))}
    :effect (req (let [c target]
                   (trigger-event state side :pre-trash c)
                   (let [tcost (trash-cost state side c)]
                     (trash state side c)
                     (gain state :corp :credit tcost)
                     (resolve-ability state side
                       {:msg (msg "trash " (card-str state c) " and gain " tcost " [Credits]")}
                      card nil)
                     (swap! state update-in [:bonus] dissoc :trash)
                     (effect-completed state side eid card))))}

   "Psychographics"
   {:req (req tagged)
    :choices :credit
    :prompt "How many credits?"
    :delayed-completion true
    :effect (req (let [c (min target (:tag runner))]
                   (continue-ability state side
                                     {:msg (msg "place " c " advancement tokens on "
                                                (card-str state target))
                                      :choices {:req can-be-advanced?}
                                      :effect (effect (add-prop target :advance-counter c {:placed true}))}
                                     card nil)))}

    "Psychokinesis"
    (letfn [(choose-card [cards]
             {:prompt "Select an agenda, asset, or upgrade to install"
              :choices (cons "None" cards)
              :delayed-completion true
              :effect (req (if-not (or (= target "None") (ice? target) (is-type? target "Operation"))
                             (continue-ability state side (install-card target) card nil)
                             (system-msg state side "does not install an asset, agenda, or upgrade"))
                           (effect-completed state side eid card)
                           (clear-wait-prompt state :runner))})
            (install-card [chosen]
             {:prompt "Select a remote server"
              :choices (req (conj (vec (get-remote-names @state)) "New remote"))
              :delayed-completion true
              :effect (effect (clear-wait-prompt :runner)
                              (corp-install (move state side chosen :play-area) target))})]
     {:msg "look at the top 5 cards of R&D"
      :delayed-completion true
      :effect (req (show-wait-prompt state :runner "Corp to look at the top cards of R&D")
                   (let [from (take 5 (:deck corp))]
                     (continue-ability state side (choose-card from) card nil)))})

   "Punitive Counterstrike"
   {:trace {:base 5
            :delayed-completion true
            :msg (msg "do " (:stole-agenda runner-reg-last 0) " meat damage")
            :effect (effect (damage eid :meat (:stole-agenda runner-reg-last 0) {:card card}))}}

   "Reclamation Order"
   {:prompt "Select a card from Archives"
    :show-discard true
    :choices {:req #(and (= (:side %) "Corp")
                         (not= (:title %) "Reclamation Order")
                         (= (:zone %) [:discard]))}
    :msg (msg "name " (:title target))
    :effect (req (let [title (:title target)
                       cards (filter #(= title (:title %)) (:discard corp))
                       n (count cards)]
                   (continue-ability state side
                                     {:prompt (str "Choose how many copies of "
                                                   title " to reveal")
                                      :choices {:number (req n)}
                                      :msg (msg "reveal "
                                                (quantify target "cop" "y" "ies")
                                                " of " title
                                                " from Archives"
                                                (when (pos? target)
                                                  (str " and add "
                                                       (if (= 1 target) "it" "them")
                                                       " to HQ")))
                                      :effect (req (doseq [c (->> cards
                                                                  (sort-by :seen)
                                                                  reverse
                                                                  (take target))]
                                                     (move state side c :hand)))}
                                     card nil)))}

   "Recruiting Trip"
   (let [rthelp (fn rt [total left selected]
                  (if (pos? left)
                    {:prompt (str "Choose a Sysop (" (inc (- total left)) "/" total ")")
                     :choices (req (cancellable (filter #(and (has-subtype? % "Sysop")
                                                              (not (some #{(:title %)} selected))) (:deck corp)) :sorted))
                     :msg (msg "put " (:title target) " into HQ")
                     :delayed-completion true
                     :effect (req (move state side target :hand)
                                  (continue-ability
                                    state side
                                    (rt total (dec left) (cons (:title target) selected))
                                    card nil))}
                    {:effect (effect (shuffle! :corp :deck))
                     :msg (msg "shuffle R&D")}))]
   {:prompt "How many Sysops?"
    :delayed-completion true
    :choices :credit
    :msg (msg "search for " target " Sysops")
    :effect (effect (continue-ability (rthelp target target []) card nil))})

   "Red Planet Couriers"
   {:delayed-completion true
    :req (req (some #(can-be-advanced? %) (all-installed state :corp)))
    :prompt "Select an installed card that can be advanced"
    :choices {:req can-be-advanced?}
    :effect (req (let [installed (get-all-installed state)
                       total-adv (reduce + (map :advance-counter
                                                (filter #(:advance-counter %) installed)))]
                   (doseq [c installed]
                     (update! state side (dissoc c :advance-counter)))
                   (set-prop state side target :advance-counter total-adv)
                   (update-all-ice state side)
                   (system-msg state side (str "uses Red Planet Couriers to move " total-adv
                                               " advancement tokens to " (card-str state target)))
                   (effect-completed state side eid)))}

   "Replanting"
   (letfn [(replant [n]
             {:prompt "Select a card to install with Replanting"
              :delayed-completion true
              :choices {:req #(and (= (:side %) "Corp")
                                   (not (is-type? % "Operation"))
                                   (in-hand? %))}
              :effect (req (when-completed (corp-install state side target nil {:no-install-cost true})
                                           (if (< n 2)
                                             (continue-ability state side (replant (inc n)) card nil)
                                             (effect-completed state side eid card))))})]
     {:delayed-completion true
      :prompt "Select an installed card to add to HQ"
      :choices {:req #(and (= (:side %) "Corp")
                           (installed? %))}
      :msg (msg "add " (card-str state target) " to HQ, then install 2 cards ignoring all costs")
      :effect (req (move state side target :hand)
                   (resolve-ability state side (replant 1) card nil))})

   "Restore"
   {:delayed-completion true
    :effect (effect (continue-ability {:prompt "Select a card in Archives to install & rez with Restore"
                                       :priority -1
                                       :delayed-completion true
                                       :show-discard true
                                       :choices {:req #(and (= (:side %) "Corp")
                                                            (not (is-type? % "Operation"))
                                                            (in-discard? %))}
                                       :effect (req (when-completed
                                                      (corp-install state side target nil {:install-state :rezzed})
                                                      (do (system-msg state side (str "uses Restore to "
                                                                                      (corp-install-msg target)))
                                                          (let [leftover (filter #(= (:title target) (:title %)) (-> @state :corp :discard))]
                                                            (when (seq leftover)
                                                              (doseq [c leftover]
                                                                (move state side c :rfg))
                                                              (system-msg state side (str "removes " (count leftover) " copies of " (:title target) " from the game"))))
                                                          (effect-completed state side eid card))))} card nil))}

   "Restoring Face"
   {:prompt "Select a Sysop, Executive or Clone to trash"
    :msg (msg "trash " (card-str state target) " to remove 2 bad publicity")
    :choices {:req #(and (rezzed? %)
                         (or (has-subtype? % "Clone")
                             (has-subtype? % "Executive")
                             (has-subtype? % "Sysop")))}
    :effect (final-effect (lose :bad-publicity 2) (trash target))}

   "Restructure"
   {:msg "gain 15 [Credits]"
    :effect (effect (gain :credit 15))}

   "Reuse"
   {:delayed-completion true
    :effect (req (let [n (count (:hand corp))]
                   (continue-ability state side
                     {:prompt (msg "Select up to " n " cards in HQ to trash with Reuse")
                      :choices {:max n
                                :req #(and (= (:side %) "Corp")
                                           (in-hand? %))}
                      :msg (msg (let [m (count targets)]
                                  (str "trash " (quantify m "card")
                                       " and gain " (* 2 m) " [Credits]")))
                      :effect (effect (trash-cards targets)
                                      (gain :credit (* 2 (count targets))))} card nil)))}

   "Reverse Infection"
   {:prompt "Choose One:"
    :choices ["Purge virus counters."
              "Gain 2 [Credits]"]
    :effect (req (if (= target "Gain 2 [Credits]")
                   (do (gain state side :credit 2)
                       (system-msg state side "uses Reverse Infection to gain 2 [Credits]"))
                   (let [pre-purge-virus (number-of-virus-counters state)]
                     (purge state side)
                     (let [post-purge-virus (number-of-virus-counters state)
                           num-virus-purged (- pre-purge-virus post-purge-virus)
                           num-to-trash (quot num-virus-purged 3)]
                       (mill state :runner num-to-trash)
                       (system-msg state side (str "uses Reverse Infection to purge "
                                                   num-virus-purged (pluralize " virus counter" num-virus-purged)
                                                   " and trash "
                                                   num-to-trash (pluralize " card" num-to-trash)
                                                   " from the top of the stack"))))))}

   "Rework"
   {:prompt "Select a card from HQ to shuffle into R&D"
    :choices {:req #(and (= (:side %) "Corp")
                         (in-hand? %))}
    :msg "shuffle a card from HQ into R&D"
    :effect (final-effect (move target :deck) (shuffle! :deck))}

   "Rolling Brownout"
   {:msg "increase the play cost of operations and events by 1 [Credits]"
    :events {:play-event {:once :per-turn
                          :msg "gain 1 [Credits]"
                          :effect (effect (gain :corp :credit 1))}
             :pre-play-instant {:effect (effect (play-cost-bonus [:credit 1]))}}}

   "Rover Algorithm"
   {:choices {:req #(and (ice? %) (rezzed? %))}
    :msg (msg "host it as a condition counter on " (card-str state target))
    :effect (final-effect (host target (assoc card :zone [:discard] :seen true :condition true))
                          (update-ice-strength (get-card state target)))
    :events {:pass-ice {:req (req (= (:cid target) (:cid (:host card))))
                                :effect (effect (add-counter card :power 1))}
             :pre-ice-strength {:req (req (= (:cid target) (:cid (:host card))))
                                :effect (effect (ice-strength-bonus (get-in card [:counter :power] 0) target))}}}

   "Sacrifice"
   {:req (req (pos? (:bad-publicity corp)))
    :delayed-completion true
    :additional-cost [:forfeit]
    :effect (effect (register-events (:events (card-def card))
                                     (assoc card :zone '(:discard))))
    :events {:corp-forfeit-agenda {:effect (req (let [bplost (min (:agendapoints (last (:rfg corp))) (:bad-publicity corp))]
                                                  (if (not (neg? bplost)) (do (lose state side :bad-publicity bplost)
                                                                              (gain state side :credit bplost)
                                                                              (system-msg state side (str "uses Sacrifice to lose " bplost " bad publicity and gain " bplost " [Credits]")))
                                                                          (system-msg state side "uses Sacrifice but gains no credits and loses no Bad Publicity"))
                                                  (effect-completed state side eid)
                                                  (unregister-events state side card)))}}}
   "Salems Hospitality"
   {:prompt "Name a Runner card"
    :choices {:card-title (req (and (card-is? target :side "Runner")
                                    (not (card-is? target :type "Identity"))))}
    :effect (req (system-msg state side
                             (str "uses Salem's Hospitality to reveal the Runner's Grip ( "
                                  (join ", " (map :title (sort-by :title (:hand runner))))
                                  " ) and trash any copies of " target))
                 (doseq [c (filter #(= target (:title %)) (:hand runner))]
                   (trash state side c {:unpreventable true})))}

   "Scarcity of Resources"
   {:msg "increase the install cost of resources by 2"
    :events {:pre-install {:req (req (and (is-type? target "Resource")
                                          (not (second targets)))) ; not facedown
                           :effect (effect (install-cost-bonus [:credit 2]))}}}

   "Scorched Earth"
   {:req (req tagged)
    :delayed-completion true
    :msg "do 4 meat damage"
    :effect (effect (damage eid :meat 4 {:card card}))}

   "SEA Source"
   {:req (req (last-turn? state :runner :successful-run))
    :trace {:base 3
            :msg "give the Runner 1 tag"
            :label "Give the Runner 1 tag"
            :delayed-completion true
            :effect (effect (tag-runner :runner eid 1))}}

   "Self-Growth Program"
   {:req (req tagged)
    :prompt "Select two installed Runner cards"
    :choices {:req #(and (installed? %)
                         (= "Runner" (:side %)))
              :max 2}
    :msg (msg (str "move " (join ", " (map :title targets)) " to the Runner's grip"))
    :effect (req (doseq [c targets]
                   (move state :runner c :hand)))}

   "Service Outage"
   (letfn [(so-activated [state]
             (get-in @state [:corp :register :so-activated] false))
           (add-effect [state side]
             (swap! state assoc-in [:corp :register :so-activated] true)
             (run-cost-bonus state side [:credit 1]))
           (remove-effect [state side]
             (run-cost-bonus state side [:credit -1])
             (swap! state update-in [:corp :register] dissoc :so-activated))]
     {:msg "add a cost of 1 [Credit] for the Runner to make the first run each turn"
      :effect (req (when (and (= :runner (:active-player @state))
                              (empty? (:made-run runner-reg)))
                     (add-effect state side)))
      :events {:runner-turn-begins {:msg "add an additional cost of 1 [Credit] to make the first run this turn"
                                    :effect (effect (add-effect))}
               :runner-turn-ends {:req (req (so-activated state))
                                  :effect (effect (remove-effect))}
               :run-ends {:req (req (so-activated state))
                          :effect (effect (remove-effect))}}
      :leave-play (req (when (so-activated state)
                         (remove-effect state side)))})

   "Shipment from Kaguya"
   {:choices {:max 2 :req can-be-advanced?}
    :msg (msg "place 1 advancement token on " (count targets) " cards")
    :effect (req (doseq [t targets] (add-prop state :corp t :advance-counter 1 {:placed true}))
                 (effect-completed state side eid card))}

   "Shipment from MirrorMorph"
   (let [shelper (fn sh [n] {:prompt "Select a card to install with Shipment from MirrorMorph"
                             :priority -1
                             :delayed-completion true
                             :choices {:req #(and (= (:side %) "Corp")
                                                  (not (is-type? % "Operation"))
                                                  (in-hand? %))}
                             :effect (req (when-completed
                                            (corp-install state side target nil nil)
                                            (if (< n 3)
                                              (continue-ability state side (sh (inc n)) card nil)
                                              (effect-completed state side eid card))))})]
     {:delayed-completion true
      :effect (effect (continue-ability (shelper 1) card nil))})

   "Shipment from SanSan"
   {:choices ["0", "1", "2"]
    :prompt "How many advancement tokens?"
    :delayed-completion true
    :effect (req (let [c (str->int target)]
                   (continue-ability
                     state side
                     {:choices {:req can-be-advanced?}
                      :msg (msg "place " c " advancement tokens on " (card-str state target))
                      :effect (effect (add-prop :corp target :advance-counter c {:placed true}))}
                     card nil)))}

   "Shipment from Tennin"
   {:delayed-completion true
    :req (req (not-last-turn? state :runner :successful-run))
    :choices {:req #(and (installed? %) (= (:side %) "Corp"))}
    :msg (msg "place 2 advancement tokens on " (card-str state target))
    :effect (effect (add-prop target :advance-counter 2 {:placed true}))}

   "Shoot the Moon"
   {:choices {:req #(and (ice? %) (not (rezzed? %)))
              :max (req (min (:tag runner)
                             (reduce (fn [c server]
                                       (+ c (count (filter #(not (:rezzed %)) (:ices server)))))
                                     0 (flatten (seq (:servers corp))))))}
    :req (req tagged)
    :effect (req (doseq [t targets] (rez state side t {:ignore-cost :all-costs}))
                 (effect-completed state side eid card))}

   "Snatch and Grab"
   {:trace {:msg "trash a connection"
            :base 3
            :choices {:req #(has-subtype? % "Connection")}
            :delayed-completion true
            :effect (req (let [c target]
                           (continue-ability
                             state side
                             {:prompt (msg "Take 1 tag to prevent " (:title c) " from being trashed?")
                              :choices ["Yes" "No"] :player :runner
                              :delayed-completion true
                              :effect (effect (continue-ability
                                                (if (= target "Yes")
                                                  {:msg (msg "take 1 tag to prevent " (:title c)
                                                             " from being trashed")
                                                   :delayed-completion true
                                                   :effect (effect (tag-runner eid 1 {:unpreventable true}))}
                                                  {:delayed-completion true
                                                   :effect (effect (trash eid c nil))
                                                   :msg (msg "trash " (:title c))})
                                                card nil))}
                             card nil)))}}

   "Special Report"
   {:prompt "Select any number of cards in HQ to shuffle into R&D"
    :choices {:max (req (count (:hand corp)))
              :req #(and (= (:side %) "Corp")
                         (in-hand? %))}
    :msg (msg "shuffle " (count targets) " cards in HQ into R&D and draw " (count targets) " cards")
    :delayed-completion true
    :effect (req (doseq [c targets]
                   (move state side c :deck))
                 (shuffle! state side :deck)
                 (draw state side eid (count targets) nil))}

   "Stock Buy-Back"
   {:msg (msg "gain " (* 3 (count (:scored runner))) " [Credits]")
    :effect (effect (gain :credit (* 3 (count (:scored runner)))))}

   "Sub Boost"
   {:choices {:req #(and (ice? %) (rezzed? %))}
    :msg (msg "make " (card-str state target) " gain Barrier and \"[Subroutine] End the run\"")
    :effect (effect (update! (assoc target :subtype (combine-subtypes true (:subtype target) "Barrier")))
                    (update-ice-strength target)
                    (host (get-card state target) (assoc card :zone [:discard] :seen true :condition true)))}

   "Subcontract"
   (letfn [(sc [i sccard]
             {:prompt "Select an operation in HQ to play"
              :choices {:req #(and (= (:side %) "Corp")
                                   (is-type? % "Operation")
                                   (in-hand? %))}
              :delayed-completion true
              :msg (msg "play " (:title target))
              :effect (req (when-completed (play-instant state side target)
                                           (if (and (not (get-in @state [:corp :register :terminal])) (< i 2))
                                               (continue-ability state side (sc (inc i) sccard) sccard nil)
                                               (effect-completed state side eid))))})]
     {:req (req tagged)
      :delayed-completion true
      :effect (effect (continue-ability (sc 1 card) card nil))})

   "Subliminal Messaging"
   (letfn [(subliminal []
             {:corp-phase-12
              {:effect
               (req (if (not-last-turn? state :runner :made-run)
                      (do (resolve-ability state side
                                           {:optional
                                            {:prompt "Add Subliminal Messaging to HQ?"
                                             :yes-ability {:effect (req (move state side card :hand)
                                                                        (system-msg state side "adds Subliminal Messaging to HQ"))}
                                             :no-ability {:effect (effect (register-events (subliminal) (assoc card :zone '(:discard))))}}}
                                           card nil)
                          (unregister-events state side card))
                      (do (unregister-events state side card)
                          (resolve-ability state side
                                           {:effect (effect (register-events (subliminal) (assoc card :zone '(:discard))))}
                                           card nil))))}})]
     {:msg "gain 1 [Credits]"
      :effect (effect (gain :credit 1)
                      (resolve-ability {:once :per-turn :once-key :subliminal-messaging
                                        :msg "gain [Click]"
                                        :effect (effect (gain :corp :click 1))} card nil))
      :move-zone (req (if (= [:discard] (:zone card))
                        (register-events state side (subliminal) (assoc card :zone '(:discard)))
                        (unregister-events state side card)))
      :events {:corp-phase-12 nil}})

   "Success"
   {:additional-cost [:forfeit]
    :effect (req (resolve-ability state side
                                  {:choices {:req can-be-advanced?}
                                   :msg (msg "advance " (card-str state target) " "
                                             (advancement-cost state side (last (:rfg corp))) " times")
                                   :effect (req (dotimes [_ (advancement-cost state side (last (:rfg corp)))]
                                                  (advance state :corp target :no-cost)))} card nil))}

   "Successful Demonstration"
   {:req (req (last-turn? state :runner :unsuccessful-run))
    :msg "gain 7 [Credits]"
    :effect (effect (gain :credit 7))}

   "Sunset"
   (letfn [(sun [serv]
             {:prompt "Select two pieces of ICE to swap positions"
              :choices {:req #(and (= serv (rest (butlast (:zone %)))) (ice? %))
                        :max 2}
              :delayed-completion true
              :effect (req (if (= (count targets) 2)
                             (do (swap-ice state side (first targets) (second targets))
                                 (continue-ability state side (sun serv) card nil))
                             (do (system-msg state side "has finished rearranging ICE")
                                 (effect-completed state side eid card))))})]
     {:prompt "Choose a server"
      :choices (req servers)
      :delayed-completion true
      :msg (msg "rearrange ICE protecting " target)
      :effect (req (let [serv (next (server->zone state target))]
                     (continue-ability state side (sun serv) card nil)))})

   "Sweeps Week"
   {:effect (effect (gain :credit (count (:hand runner))))
    :msg (msg "gain " (count (:hand runner)) " [Credits]")}

   "Targeted Marketing"
   (let [gaincr {:req (req (= (:title target) (:marketing-target card)))
                 :effect (effect (gain :corp :credit 10))
                 :msg (msg "gain 10 [Credits] from " (:marketing-target card))}]
     {:prompt "Name a Runner card"
      :choices {:card-title (req (and (card-is? target :side "Runner")
                                      (not (card-is? target :type "Identity"))))}
      :effect (effect (update! (assoc card :marketing-target target))
                      (system-msg (str "uses Targeted Marketing to name " target)))
      :events {:runner-install gaincr
               :play-event gaincr}})

   "The All-Seeing I"
   (let [trash-all-resources {:player :runner
                              :effect (req (doseq [resource (get-in runner [:rig :resource])]
                                             (trash state side resource)))
                              :msg (msg "trash all resources")}]
       {:req (req tagged)
        :delayed-completion true
        :effect (effect
                 (continue-ability
                   (if-not (zero? (:bad-publicity corp)) ;; If corp's bad-pub is 0
                     {:optional {:player :runner
                                 :prompt "Remove 1 bad publicity from the corp to prevent all resources from being trashed?"
                                 :yes-ability {:effect (effect (lose :corp :bad-publicity 1))
                                               :player :corp
                                               :msg (msg "lose 1 bad publicity, preventing all resources from being trashed")}
                                 :no-ability trash-all-resources}}
                    trash-all-resources)
                  card targets))})

   "Threat Assessment"
   {:req (req (last-turn? state :runner :trashed-card))
    :prompt "Select an installed Runner card"
    :choices {:req #(and (= (:side %) "Runner") (installed? %))}
    :delayed-completion true
    :effect (req (let [chosen target]
                   (show-wait-prompt state side "Runner to resolve Threat Assessment")
                   (continue-ability state :runner
                                     {:prompt (str "Add " (:title chosen) " to the top of the Stack or take 2 tags?")
                                      :choices [(str "Move " (:title chosen))
                                                "2 tags"]
                                      :delayed-completion true
                                      :effect (req (clear-wait-prompt state :corp)
                                                   (move state :corp (last (:discard corp)) :rfg)
                                                   (if (.startsWith target "Move")
                                                     (do (system-msg state side (str "chooses to move " (:title chosen) " to the Stack"))
                                                       (move state :runner chosen :deck {:front true})
                                                       (effect-completed state side eid))
                                                     (do (system-msg state side "chooses to take 2 tags")
                                                       (tag-runner state :runner eid 2))))}
                                     card nil)))}

   "Threat Level Alpha"
   {:trace {:base 1
            :label "Give the Runner X tags"
            :delayed-completion true
            :effect (req (let [tags (-> @state :runner :tag)]
                           (if (pos? tags)
                             (do (tag-runner state :runner eid tags)
                                 (system-msg state side (str "uses Threat Level Alpha to give the Runner " tags " tags")))
                             (do (tag-runner state :runner eid 1)
                                 (system-msg state side "uses Threat Level Alpha to give the Runner a tag")))))}}

   "Traffic Accident"
   {:req (req (>= (:tag runner) 2))
    :msg "do 2 meat damage"
    :delayed-completion true
    :effect (effect (damage eid :meat 2 {:card card}))}

   "Transparency Initiative"
   {:choices {:req #(and (is-type? % "Agenda")
                         (installed? %)
                         (not (faceup? %)))}
    :effect (effect (update! (assoc target :seen true :rezzed true
                                           :subtype (combine-subtypes false (:subtype target) "Public")))
                    (host (get-card state target) (assoc card :zone [:discard] :seen true))
                    (register-events
                      {:advance {:req (req (= (:hosted card) (:hosted target)))
                                 :effect (effect (gain :credit 1)
                                                 (system-msg
                                                   (str "uses Transparency Initiative to gain 1 [Credit]")))}}
                      target))}

   "Trick of Light"
   {:choices {:req #(and (contains? % :advance-counter) (> (:advance-counter %) 0))}
    :delayed-completion true
    :effect (req (let [fr target tol card]
                   (continue-ability
                     state side
                     {:prompt "Move how many advancement tokens?"
                      :choices (take (inc (:advance-counter fr)) ["0" "1" "2"])
                      :delayed-completion true
                      :effect (req (let [c (str->int target)]
                                     (continue-ability
                                       state side
                                       {:prompt  "Move to where?"
                                        :choices {:req #(and (not= (:cid fr) (:cid %))
                                                             (can-be-advanced? %))}
                                        :effect  (effect (add-prop :corp target :advance-counter c {:placed true})
                                                         (add-prop :corp fr :advance-counter (- c) {:placed true})
                                                         (system-msg (str "moves " c " advancement tokens from "
                                                                          (card-str state fr) " to " (card-str state target))))}
                                       tol nil)))}
                     card nil)))}

   "Trojan Horse"
   {:req (req (:accessed-cards runner-reg))
    :trace {:base 4
            :label "Trace 4 - Trash a program"
            :delayed-completion true
            :effect (req (let [exceed (- target (second targets))]
                           (continue-ability
                             state side
                             {:delayed-completion true
                              :prompt (str "Select a program with an install cost of no more than " exceed "[Credits]")
                              :choices {:req #(and (is-type? % "Program")
                                                   (installed? %)
                                                   (>= exceed (:cost %)))}
                              :msg (msg "trash " (card-str state target))
                              :effect (effect (trash eid target nil))}
                             card nil)))}}

   "Ultraviolet Clearance"
   {:delayed-completion true
    :effect (req (gain state side :credit 10)
                 (when-completed (draw state side 4 nil)
                                 (continue-ability state side
                                                   {:prompt "Choose a card in HQ to install"
                                                    :choices {:req #(and (in-hand? %) (= (:side %) "Corp") (not (is-type? % "Operation")))}
                                                    :msg "gain 10 [Credits], draw 4 cards, and install 1 card from HQ"
                                                    :cancel-effect (req (effect-completed state side eid))
                                                    :effect (effect (corp-install target nil))}
                                                   card nil)))}

   "Violet Level Clearance"
   {:msg "gain 8 [Credits] and draw 4 cards"
    :delayed-completion true
    :effect (effect (gain :credit 8)
                    (draw eid 4 nil))}

   "Voter Intimidation"
   {:req (req (seq (:scored runner)))
    :psi {:not-equal {:player :corp
                      :prompt "Select a resource to trash"
                      :choices {:req #(and (installed? %)
                                           (is-type? % "Resource"))}
                      :msg (msg "trash " (:title target))
                      :effect (effect (trash target))}}}

   "Wake Up Call"
   {:req (req (last-turn? state :runner :trashed-card))
    :prompt "Select a piece of hardware or non-virtual resource"
    :choices {:req #(or (hardware? %)
                        (and (resource? %) (not (has-subtype? % "Virtual"))))}
    :delayed-completion true
    :effect (req (let [chosen target
                       wake card]
                   (show-wait-prompt state side "Runner to resolve Wake Up Call")
                   (continue-ability state :runner
                                     {:prompt (str "Trash " (:title chosen) " or suffer 4 meat damage?")
                                      :choices [(str "Trash " (:title chosen))
                                                "4 meat damage"]
                                      :delayed-completion true
                                      :effect (req (clear-wait-prompt state :corp)
                                                   (move state side (last (:discard corp)) :rfg)
                                                   (if (.startsWith target "Trash")
                                                     (do (system-msg state side (str "chooses to trash " (:title chosen)))
                                                         (trash state side eid chosen nil))
                                                     (do (system-msg state side "chooses to suffer meat damage")
                                                         (damage state side eid :meat 4 {:card wake
                                                                                         :unboostable true}))))}
                                     card nil)))}

   "Wetwork Refit"
   {:choices {:req #(and (ice? %)
                         (has-subtype? % "Bioroid")
                         (rezzed? %))}
    :msg (msg "give " (card-str state target) "\"[Subroutine] Do 1 brain damage\" before all its other subroutines")
    :effect (effect (update! (assoc target :subroutines (cons (do-brain-damage 1) (:subroutines target))))
                    (host (get-card state target) (assoc card :zone [:discard] :seen true :condition true)))
    :leave-play (effect (update! (assoc (:host card) :subroutines (rest (:subroutines (:host card))))))}

   "Witness Tampering"
   {:msg "remove 2 bad publicity"
    :effect (effect (lose :bad-publicity 2))}})
