(in-ns 'game.core)

(def cards-operations
  {"24/7 News Cycle"
   {:req (req (> (count (:scored corp)) 1))
    :delayed-completion true
    :additional-cost [:forfeit]
    :effect (req (continue-ability
                   state side
                   {:prompt "Choose an agenda in your score area to trigger its \"when scored\" ability"
                    :choices {:req #(and (is-type? % "Agenda")
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
                  {:prompt "Select an advertisement to install and rez" :show-discard true
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
     {:prompt "How many advertisements?"
      :delayed-completion true
      :choices :credit
      :msg (msg "install and rez " target " advertisements")
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
                   (continue-ability
                     state side
                     {:optional
                      {:prompt (msg "Make a run on " serv "?") :player :runner
                       :yes-ability {:msg (msg "let the Runner make a run on " serv)
                                     :effect (req (let [s (cond
                                                            (= serv "HQ") [:hq]
                                                            (= serv "R&D") [:rd]
                                                            (= serv "Archives") [:archives])
                                                        ices (get-in @state (concat [:corp :servers] s [:ices]))]
                                                    (swap! state assoc :per-run nil
                                                           :run {:server s :position (count ices)
                                                                 :access-bonus 0 :run-effect nil :cannot-jack-out true})
                                                    (gain-run-credits state :runner (:bad-publicity corp))
                                                    (swap! state update-in [:runner :register :made-run] #(conj % (first s)))
                                                    (trigger-event state :runner :run s)))}
                       :no-ability {:effect (effect (as-agenda :corp (last (:discard corp)) 1))
                                    :msg "add it to their score area and gain 1 agenda point"}}}
                    card nil)))}

   "Anonymous Tip"
   {:msg "draw 3 cards"
    :effect (effect (draw 3))}

   "Archived Memories"
   {:effect (req (let [cid (:cid card)]
                   (resolve-ability state side
                     {:prompt "Choose a card from Archives to add to HQ" :show-discard true
                      :choices {:req #(and (not= (:cid %) cid)
                                           (= (:side %) "Corp")
                                           (= (:zone %) [:discard]))}
                      :effect (final-effect (move target :hand)
                                            (system-msg (str "adds " (if (:seen target) (:title target) "an unseen card") " to HQ")))}
                    card nil)))}

   "Back Channels"
   {:prompt "Choose an installed card in a server to trash"
    :choices {:req #(and (= (last (:zone %)) :content)
                         (is-remote? (second (:zone %))))}
    :effect (final-effect (gain :credit (* 3 (get target :advance-counter 0))) (trash target))
    :msg (msg "trash " (card-str state target) " and gain "
              (* 3 (get target :advance-counter 0)) " [Credits]")}

   "Bad Times"
   {:req (req tagged)
    :msg "force the Runner to lose 2[mu] until the end of the turn"
    :effect (req (lose state :runner :memory 2)
                 (when (< (:memory runner) 0)
                  (system-msg state :runner "must trash programs to free up [mu]")))
    :end-turn {:effect (req (gain state :runner :memory 2)
                            (system-msg state :runner "regains 2[mu]"))}}

   "Beanstalk Royalties"
   {:msg "gain 3 [Credits]"
    :effect (effect (gain :credit 3))}

   "Big Brother"
   {:req (req tagged)
    :msg "give the Runner 2 tags"
    :effect (effect (tag-runner :runner 2))}

   "Bioroid Efficiency Research"
   {:choices {:req #(and (ice? %)
                         (has-subtype? % "Bioroid")
                         (not (rezzed? %)))}
    :msg (msg "rez " (card-str state target {:visible true}) " at no cost")
    :effect (final-effect (rez target {:ignore-cost :all-costs})
                          (host (get-card state target) (assoc card :zone [:discard] :seen true)))}

   "Biotic Labor"
   {:msg "gain [Click][Click]"
    :effect (effect (gain :click 2))}

   "Blue Level Clearance"
   {:msg "gain 5 [Credits] and draw 2 cards"
    :effect (effect (gain :credit 5) (draw 2))}

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
                      :effect (effect (tag-runner :runner 2)) :msg "give the Runner 2 tags"}}}

   "Celebrity Gift"
   {:choices {:max 5 :req #(and (:side % "Corp")
                                (in-hand? %))}
    :msg (msg "reveal " (join ", " (map :title targets)) " and gain " (* 2 (count targets)) " [Credits]")
    :effect (final-effect (gain :credit (* 2 (count targets))))}

   "Cerebral Cast"
   {:req (req (:successful-run runner-reg))
    :psi {:not-equal {:player :runner :prompt "Take 1 tag or 1 brain damage?"
                      :choices ["1 tag" "1 brain damage"] :msg (msg "give the Runner " target)
                      :effect (req (if (= target "1 tag")
                                     (tag-runner state side 1)
                                     (damage state side eid :brain 1 {:card card})))}}}

   "Cerebral Static"
   {:msg "disable the Runner's identity"
    :effect (effect (disable-identity :runner))
    :leave-play (effect (enable-identity :runner))}

   "\"Clones are not People\""
   {:events {:agenda-scored {:msg "add it to their score area and gain 1 agenda point"
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
    :effect (effect (shuffle-into-deck :hand) (draw 5))}

   "Cyberdex Trial"
   {:msg "purge virus counters"
    :effect (effect (purge))}

   "Dedication Ceremony"
   {:prompt "Choose a faceup card"
    :choices {:req rezzed?}
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

   "Diversified Portfolio"
   {:msg (msg "gain " (count (filter #(not (empty? %)) (map #(:content (second %)) (get-remotes @state))))
              " [Credits]")
    :effect (effect (gain :credit (count (filter #(not (empty? %)) (map #(:content (second %)) (get-remotes @state))))))}

   "Election Day"
   {:req (req (->> (get-in @state [:corp :hand])
                   (filter #(not (= (:cid %) (:cid card))))
                   (count)
                   (pos?)))
    :msg (msg "trash all cards in HQ and draw 5 cards")
    :effect (effect (trash-cards (get-in @state [:corp :hand]))
                    (draw 5))}

   "Enforcing Loyalty"
   {:trace {:base 3
            :label "Trash a card not matching the faction of the Runner's identity"
            :delayed-completion true
            :effect (req (let [f (:faction (:identity runner))]
                           (continue-ability
                             state side
                             {:prompt "Choose an installed card not matching the faction of the Runner's identity"
                              :choices {:req #(and (installed? %) (not= f (:faction %)) (card-is? % :side :runner))}
                              :msg (msg "trash " (:title target))
                              :effect (effect (trash target))}
                            card nil)))}}

   "Exchange of Information"
   {:req (req (and tagged
                   (seq (:scored runner))
                   (seq (:scored corp))))
    :delayed-completion true
    :effect (req
              (continue-ability
                state side
                {:prompt "Choose a stolen agenda in the Runner's score area to swap"
                 :choices {:req #(in-runner-scored? state side %)}
                 :delayed-completion true
                 :effect (req
                           (let [stolen target]
                             (continue-ability
                               state side
                               {:prompt (msg "Choose a scored agenda to swap for " (:title stolen))
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
   {:req (req (>= (:credit runner) 6))
    :effect (req (let [rcount (count (filter #(is-type? % "Resource") (all-installed state :runner)))]
                   (if (pos? rcount)
                     (do (show-wait-prompt state :corp "Runner to trash a resource to prevent Financial Collapse")
                         (resolve-ability
                           state side
                           {:prompt (msg "Trash a resource to prevent Financial Collapse?")
                            :choices ["Yes" "No"] :player :runner
                            :effect (final-effect (resolve-ability
                                                    (if (= target "Yes")
                                                      {:prompt "Choose a resource to trash" :player :runner
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
                     (resolve-ability
                       state side
                       {:effect (effect (lose :runner :credit (* rcount 2)))
                        :msg (msg "make the Runner lose " (* rcount 2) " [Credits]")} card nil))))}

   "Foxfire"
   {:trace {:base 7
            :prompt "Choose 1 card to trash"
            :not-distinct true
            :choices {:req #(and (installed? %)
                                 (or (has-subtype? % "Virtual")
                                     (has-subtype? % "Link")))}
            :msg "trash 1 virtual resource or link"
            :effect (effect (trash target) (system-msg (str "trashes " (:title target))))}}

   "Freelancer"
   {:req (req tagged)
    :msg (msg "trash " (join ", " (map :title targets)))
    :choices {:max 2
              :req #(and (installed? %)
                         (is-type? % "Resource"))}
    :effect (final-effect (trash-cards :runner targets))}

   "Green Level Clearance"
   {:msg "gain 3 [Credits] and draw 1 card"
    :effect (effect (gain :credit 3) (draw))}

   "Hard-Hitting News"
   {:req (req (:made-run runner-reg))
    :trace {:base 4
            :msg "give the Runner 4 tags"
            :label "Give the Runner 4 tags"
            :effect (effect (tag-runner :runner 4))}}

   "Hatchet Job"
   {:trace {:base 5
            :choices {:req #(and (installed? %)
                                 (not (has-subtype? % "Virtual")))}
            :msg "add an installed non-virtual card to the Runner's grip"
            :effect (effect (move :runner target :hand true))}}

   "Hedge Fund"
   {:msg "gain 9 [Credits]" :effect (effect (gain :credit 9))}

   "Hellion Alpha Test"
   {:req (req (:installed-resource runner-reg))
    :trace {:base 2
            :choices {:req #(and (installed? %)
                                 (is-type? % "Resource"))}
            :msg "add a Resource to the top of the Stack"
            :effect (effect (move :runner target :deck {:front true})
                            (system-msg (str "adds " (:title target) " to the top of the Stack")))
            :unsuccessful {:msg "take 1 bad publicity"
                           :effect (effect (gain :corp :bad-publicity 1))}}}

   "Heritage Committee"
   {:delayed-completion true
    :effect (effect (draw 3)
                    (continue-ability
                      {:prompt "Choose a card in HQ to put on top of R&D"
                       :choices {:req #(and (in-hand? %)
                                            (= (:side %) "Corp"))}
                       :msg "draw 3 cards and add 1 card from HQ to the top of R&D"
                       :effect (effect (move target :deck {:front true}))}
                      card nil))}

   "Housekeeping"
   {:events {:runner-install {:player :runner
                              :choices {:req #(and (in-hand? %)
                                                   (= (:side %) "Runner"))}
                              :prompt "Choose a card from your Grip to trash for Housekeeping" :once :per-turn
                              :msg (msg "force the Runner to trash " (:title target) " from their Grip")
                              :effect (effect (trash target {:unpreventable true}))}}}

   "Interns"
   {:prompt "Choose a card to install from Archives or HQ"
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
                                              (join ", " (map :title (:hand runner)))
                                              " ) and can trash up to " x " resources or events"))
                             (continue-ability state side (iop (dec x)) card nil)))
              :unsuccessful {:msg "take 1 bad publicity" :effect (effect (gain :corp :bad-publicity 1))}}})

   "Lag Time"
   {:effect (effect (update-all-ice))
    :events {:pre-ice-strength {:effect (effect (ice-strength-bonus 1 target))}}
    :leave-play (effect (update-all-ice))}

   "Lateral Growth"
   {:delayed-completion true
    :msg "gain 4 [Credits]"
    :effect (effect (gain :credit 4)
                    (continue-ability {:player :corp
                                       :prompt "Choose a card to install"
                                       :delayed-completion true
                                       :choices {:req #(and (not (is-type? % "Operation"))
                                                            (:side % "Corp")
                                                            (in-hand? %))}
                                       :effect (effect (corp-install eid target nil nil))
                                       :msg (msg (corp-install-msg target))}
                                      card nil))}

   "Liquidation"
   {:delayed-completion true
    :effect (req (let [n (count (filter #(and (rezzed? %)
                                              (not (is-type? % "Agenda"))) (all-installed state :corp)))]
                   (continue-ability state side
                     {:prompt "Choose any number of rezzed cards to trash"
                      :choices {:max n :req #(and (rezzed? %) (not (is-type? % "Agenda")))}
                      :msg (msg "trash " (join ", " (map :title targets)) " and gain " (* n 3) " [Credits]")
                      :effect (req (doseq [c targets]
                                     (trash state side c))
                                   (gain state side :credit (* n 3)))}
                    card nil)))}

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
                     :msg (msg "add " target " cop" (if (= target 1) "y" "ies") " of " c " to HQ")
                     :effect (req (shuffle! state :corp :deck)
                                  (doseq [c (take target cs)]
                                    (move state side c :hand)))}
                    card nil)))}

   "Manhunt"
   {:events {:successful-run {:req (req (first-event state side :successful-run))
                              :trace {:base 2 :msg "give the Runner 1 tag"
                                      :effect (effect (tag-runner :runner 1))}}}}

   "Medical Research Fundraiser"
   {:msg "gain 8 [Credits]. The Runner gains 3 [Credits]"
    :effect (effect (gain :credit 8) (gain :runner :credit 3))}

   "Midseason Replacements"
   {:req (req (:stole-agenda runner-reg))
    :trace {:base 6
            :msg "give the Runner X tags"
            :label "Give the Runner X tags"
            :effect (effect (tag-runner :runner (- target (second targets)))
                            (system-msg (str "gives the Runner " (- target (second targets)) " tags")))}}

   "Mushin No Shin"
   {:prompt "Choose a card to install from HQ"
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
   {:req (req (seq (filter (every-pred rezzed? ice?) (all-installed state :corp))))
    :prompt "Choose a rezzed piece of ICE to trash"
    :choices {:req (every-pred rezzed? ice?)}
    :msg (msg "to trash " (:title target))
    :effect (req (let [i (ice-index state target)
                       [reveal r] (split-with (complement ice?) (get-in @state [:corp :deck]))
                       titles (->> (conj (vec reveal) (first r)) (filter identity) (map :title))]
                   (trash state side target {:cause :ability-cost :keep-server-alive true})
                   (when (seq titles)
                     (system-msg state side (str "reveals " (clojure.string/join ", " titles) " from R&D")))
                   (if-let [ice (first r)]
                     (let [newice (assoc ice :zone (:zone target) :rezzed true)
                           ices (get-in @state (cons :corp (:zone target)))
                           newices (apply conj (subvec ices 0 i) newice (subvec ices i))]
                       (swap! state assoc-in (cons :corp (:zone target)) newices)
                       (swap! state update-in [:corp :deck] (fn [coll] (remove-once #(not= (:cid %) (:cid newice)) coll)))
                       (trigger-event state side :corp-install newice)
                       (card-init state side newice false)
                       (system-msg state side (str "uses Mutate to install and rez " (:title newice) " from R&D at no cost"))
                       (trigger-event state side :rez newice))
                     (system-msg state side (str "does not find any ICE to install from R&D")))
                   (shuffle! state :corp :deck)
                   (effect-completed state side eid card)))}

   "Neural EMP"
   {:req (req (:made-run runner-reg))
    :msg "do 1 net damage"
    :effect (effect (damage eid :net 1 {:card card}))}

   "Oversight AI"
   {:choices {:req #(and (ice? %) (not (rezzed? %)) (= (last (:zone %)) :ices))}
    :msg (msg "rez " (:title target) " at no cost")
    :effect (final-effect (rez target {:ignore-cost :all-costs})
                          (host (get-card state target) (assoc card :zone [:discard] :seen true)))}

   "Patch"
   {:choices {:req #(and (ice? %) (rezzed? %))}
    :msg (msg "give +2 strength to " (card-str state target))
    :effect (final-effect (host target (assoc card :zone [:discard] :seen true))
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
   {:trace {:base 2
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
   {:req (req (:made-run runner-reg))
    :prompt "Trash how many cards from the top R&D?"
    :choices {:number (req (count (:deck corp)))}
    :msg (msg "trash " target " cards from the top of R&D")
    :delayed-completion true
    :effect (req (mill state :corp target)
                 (let [n target]
                   (continue-ability state :runner
                                     {:prompt "Choose a Program or piece of Hardware to trash"
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

   "Product Recall"
   {:prompt "Choose a rezzed asset or upgrade to trash"
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

   "Punitive Counterstrike"
   {:trace {:base 5 :msg "do meat damage equal to agenda points stolen last turn"
            :effect (effect (damage eid :meat (or (get-in runner [:register :stole-agenda]) 0) {:card card})
                            (system-msg (str "does " (or (:stole-agenda runner-reg) 0) " meat damage")))}}

   "Reclamation Order"
   {:prompt "Choose a card from Archives" :msg (msg "add copies of " (:title target) " to HQ")
    :show-discard true
    :choices {:req #(and (= (:side %) "Corp")
                         (not= (:title %) "Reclamation Order")
                         (= (:zone %) [:discard]))}
    :effect (req (doseq [c (filter #(= (:title target) (:title %)) (:discard corp))]
                   (move state side c :hand))
                 (effect-completed state side eid card))}

   "Recruiting Trip"
   (let [rthelp (fn rt [total left selected]
                  (if (pos? left)
                    {:prompt (str "Select a sysop (" (inc (- total left)) "/" total ")")
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
   {:prompt "How many sysops?"
    :delayed-completion true
    :choices :credit
    :msg (msg "search for " target " sysops")
    :effect (effect (continue-ability (rthelp target target []) card nil))})

   "Restoring Face"
   {:prompt "Choose a Sysop, Executive or Clone to trash"
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
                     {:prompt (msg "Choose up to " n " cards in HQ to trash with Reuse")
                      :choices {:max n :req #(and (:side % "Corp")
                                                  (in-hand? %))}
                      :msg (msg "trash " (count targets) " card" (if (not= 1 (count targets)) "s")
                                " and gain " (* 2 (count targets)) " [Credits]")
                      :effect (effect (trash-cards targets)
                                      (gain :credit (* 2 (count targets))))} card nil)))}

   "Rework"
   {:prompt "Choose a card from HQ to shuffle into R&D"
    :choices {:req #(and (in-hand? %)
                         (= (:side %) "Corp"))}
    :msg "shuffle a card from HQ into R&D"
    :effect (final-effect (move target :deck) (shuffle! :deck))}

   "Salems Hospitality"
   {:prompt "Name a Runner card"
    :choices {:card-title (req (and (card-is? target :side "Runner")
                                    (not (card-is? target :type "Identity"))))}
    :effect (req (system-msg state side
                             (str "uses Salem's Hospitality to reveal the Runner's Grip ( "
                                  (join ", " (map :title (:hand runner)))
                                  " ) and trash any copies of " target))
                 (doseq [c (filter #(= target (:title %)) (:hand runner))]
                   (trash state side c)))}

   "Scarcity of Resources"
   {:msg "increase the install cost of resources by 2"
    :events {:pre-install {:req (req (is-type? target "Resource"))
                           :effect (effect (install-cost-bonus [:credit 2]))}}}

   "Scorched Earth"
   {:req (req tagged)
    :delayed-completion true
    :msg "do 4 meat damage"
    :effect (effect (damage eid :meat 4 {:card card}))}

   "SEA Source"
   {:req (req (:successful-run runner-reg))
    :trace {:base 3
            :msg "give the Runner 1 tag"
            :label "Give the Runner 1 tag"
            :effect (effect (tag-runner :runner 1))}}

   "Shipment from Kaguya"
   {:choices {:max 2 :req can-be-advanced?}
    :msg (msg "place 1 advancement token on " (count targets) " cards")
    :effect (req (doseq [t targets] (add-prop state :corp t :advance-counter 1 {:placed true}))
                 (effect-completed state side eid card))}

   "Shipment from MirrorMorph"
   (let [shelper (fn sh [n] {:prompt "Select a card to install with Shipment from MirrorMorph"
                             :priority -1
                             :delayed-completion true
                             :choices {:req #(and (:side % "Corp")
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
    :effect (req (let [c (Integer/parseInt target)]
                   (continue-ability
                     state side
                     {:choices {:req can-be-advanced?}
                      :msg (msg "place " c " advancement tokens on " (card-str state target))
                      :effect (effect (add-prop :corp target :advance-counter c {:placed true}))}
                     card nil)))}

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
                                                   :effect (final-effect (tag-runner 1 {:unpreventable true}))}
                                                  {:effect (final-effect (trash c)) :msg (msg "trash " (:title c))})
                                                card nil))}
                             card nil)))}}

   "Special Report"
   {:prompt "Choose any number of cards in HQ to shuffle into R&D"
    :choices {:max (req (count (:hand corp))) :req #(and (:side % "Corp")
                                                         (in-hand? %))}
    :msg (msg "shuffle " (count targets) " cards in HQ into R&D and draw " (count targets) " cards")
    :effect (req (doseq [c targets]
                   (move state side c :deck))
                 (shuffle! state side :deck)
                 (draw state side (count targets))
                 (effect-completed state side eid card))}

   "Stock Buy-Back"
   {:msg (msg "gain " (* 3 (count (:scored runner))) " [Credits]")
    :effect (effect (gain :credit (* 3 (count (:scored runner)))))}

   "Sub Boost"
   {:choices {:req #(and (ice? %) (rezzed? %))}
    :msg (msg "make " (card-str state target) " gain Barrier and \"[Subroutine] End the run\"")
    :effect (effect (update! (assoc target :subtype
                                           (->> (vec (.split (:subtype target) " - "))
                                                (concat ["Barrier"])
                                                distinct
                                                (join " - "))))
                    (update-ice-strength target)
                    (host (get-card state target) (assoc card :zone [:discard] :seen true)))}

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
               (req (if (not (:made-run runner-reg))
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
      :mill-effect {:effect (effect (register-events (subliminal) (assoc card :zone '(:discard))))}
      :move-zone (req (if (= [:discard] (:zone card))
                        (register-events state side (subliminal) (assoc card :zone '(:discard)))
                        (unregister-events state side card)))
      :events {:corp-phase-12 nil}})

   "Successful Demonstration"
   {:req (req (:unsuccessful-run runner-reg))
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

   "Traffic Accident"
   {:req (req (>= (:tag runner) 2))
    :msg "do 2 meat damage"
    :delayed-completion true
    :effect (effect (damage eid :meat 2 {:card card}))}

   "Trick of Light"
   {:choices {:req #(and (contains? % :advance-counter) (> (:advance-counter %) 0))}
    :delayed-completion true
    :effect (req (let [fr target tol card]
                   (continue-ability
                     state side
                     {:prompt "Move how many advancement tokens?"
                      :choices (take (inc (:advance-counter fr)) ["0" "1" "2"])
                      :delayed-completion true
                      :effect (req (let [c (Integer/parseInt target)]
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

   "Voter Intimidation"
   {:req (req (seq (:scored runner)))
    :psi {:not-equal {:player :corp :prompt "Choose a resource to trash"
                      :choices {:req #(and (installed? %)
                                           (is-type? % "Resource"))}
                      :msg (msg "trash " (:title target))
                      :effect (effect (trash target))}}}

   "Witness Tampering"
   {:msg "remove 2 bad publicity"
    :effect (effect (lose :bad-publicity 2))}})
