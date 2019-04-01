(ns game.cards.operations
  (:require [game.core :refer :all]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability when-let*]]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [clojure.stacktrace :refer [print-stack-trace]]
            [jinteki.utils :refer [str->int other-side is-tagged? count-tags has-subtype?]]))

;; Card definitions
(def card-definitions
  {"24/7 News Cycle"
   {:req (req (pos? (count (:scored corp))))
    :async true
    :additional-cost [:forfeit]
    :effect (req (continue-ability
                   state side
                   {:prompt "Select an agenda in your score area to trigger its \"when scored\" ability"
                    :choices {:req #(and (is-type? % "Agenda")
                                         (when-scored? %)
                                         (is-scored? state :corp %))}
                    :msg (msg "trigger the \"when scored\" ability of " (:title target))
                    :async true
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
              :async true
              :effect (req (wait-for (play-instant state side target {:no-additional-cost true})
                                     (if (and (not (get-in @state [:corp :register :terminal])) (< i n))
                                       (continue-ability state side (ad (inc i) n adcard) adcard nil)
                                       (effect-completed state side eid))))})]
     {:async true
      :implementation "Corp has to manually cards back to R&D to correctly play a draw operation"
      :effect (req (let [n (count (filter #(is-type? % "Operation")
                                          (take 3 (:deck corp))))]
                     (continue-ability state side
                                       {:msg "look at the top 3 cards of R&D"
                                        :async true
                                        :effect (req (doseq [c (take 3 (:deck corp))]
                                                       (move state side c :play-area))
                                                     (continue-ability state side (ad 1 n card) card nil))}
                                       card nil)))})

   "Ad Blitz"
   (let [abhelp (fn ab [n total]
                  {:prompt "Select an Advertisement to install and rez" :show-discard true
                   :async true
                   :choices {:req #(and (= (:side %) "Corp")
                                        (has-subtype? % "Advertisement")
                                        (or (in-hand? %)
                                            (= (:zone %) [:discard])))}
                   :effect (req (wait-for
                                  (corp-install state side target nil {:install-state :rezzed})
                                  (if (< n total)
                                    (continue-ability state side (ab (inc n) total) card nil)
                                    (effect-completed state side eid))))})]
     {:prompt "How many Advertisements?"
      :async true
      :choices :credit
      :msg (msg "install and rez " target " Advertisements")
      :effect (effect (continue-ability (abhelp 1 target) card nil))})

   "Aggressive Negotiation"
   {:req (req (:scored-agenda corp-reg)) :prompt "Choose a card"
    :choices (req (cancellable (:deck corp) :sorted))
    :effect (effect (move target :hand)
                    (shuffle! :deck))
    :msg "search R&D for a card and add it to HQ"}

   "An Offer You Can't Refuse"
   {:async true
    :prompt "Choose a server" :choices ["Archives" "R&D" "HQ"]
    :effect (req (let [serv target]
                   (show-wait-prompt state :corp (str "Runner to decide on running " target))
                   (continue-ability
                     state side
                     {:optional
                      {:prompt (msg "Make a run on " serv "?") :player :runner
                       :yes-ability {:msg (msg "let the Runner make a run on " serv)
                                     :effect (effect (clear-wait-prompt :corp)
                                                     (make-run eid serv nil card))}
                       :no-ability {:async true
                                    :effect (req (clear-wait-prompt state :corp)
                                                 (as-agenda state :corp eid (some #(when (= (:cid card) (:cid %)) %) (:discard corp)) 1))
                                    :msg "add it to their score area as an agenda worth 1 agenda point"}}}
                     card nil)))}

   "Anonymous Tip"
   {:msg "draw 3 cards"
    :async true
    :effect (effect (draw eid 3 nil))}

   "Archived Memories"
   {:async true
    :effect (req (let [cid (:cid card)]
                   (continue-ability
                     state side
                     {:prompt "Select a card from Archives to add to HQ"
                      :show-discard true
                      :choices {:req #(and (not= (:cid %) cid)
                                           (= (:side %) "Corp")
                                           (= (:zone %) [:discard]))}
                      :effect (effect (move target :hand)
                                      (system-msg (str "adds " (if (:seen target) (:title target) "an unseen card") " to HQ")))}
                     card nil)))}

   "Ark Lockdown"
   {:async true
    :req (req (not-empty (:discard runner)))
    :prompt "Name a card to remove all copies in the Heap from the game"
    :choices (req (cancellable (:discard runner) :sorted))
    :msg (msg "remove all copies of " (:title target) " in the Heap from the game")
    :effect (req (doseq [c (filter #(= (:title target) (:title %)) (:discard runner))]
                   (move state :runner c :rfg))
                 (effect-completed state side eid))}

   "Attitude Adjustment"
   {:async true
    :effect (req (wait-for (draw state side 2 nil)
                           (continue-ability
                             state side
                             {:prompt "Choose up to 2 agendas in HQ or Archives"
                              :choices {:max 2
                                        :req #(and (= (:side %) "Corp")
                                                   (is-type? % "Agenda")
                                                   (or (in-hand? %)
                                                       (in-discard? %)))}
                              :effect (req (gain-credits state side (* 2 (count targets)))
                                           (doseq [c targets]
                                             (move state :corp c :deck))
                                           (shuffle! state :corp :deck)
                                           (let [from-hq (map :title (filter #(= [:hand] (:zone %)) targets))
                                                 from-archives (map :title (filter #(= [:discard] (:zone %)) targets))]
                                             (system-msg
                                               state side
                                               (str "uses Attitude Adjustment to shuffle "
                                                    (join " and "
                                                          (filter identity
                                                                  [(when (not-empty from-hq)
                                                                     (str (join " and " from-hq)
                                                                          " from HQ"))
                                                                   (when (not-empty from-archives)
                                                                     (str (join " and " from-archives)
                                                                          " from Archives"))]))
                                                    " into R&D and gain " (* 2 (count targets)) " [Credits]"))))}
                             card nil)))}

   "Audacity"
   (let [audacity (fn au [n] {:prompt "Choose a card on which to place an advancement"
                              :async true
                              :choices {:req can-be-advanced?}
                              :cancel-effect (req (effect-completed state side eid))
                              :msg (msg "place an advancement token on " (card-str state target))
                              :effect (req (add-prop state :corp target :advance-counter 1 {:placed true})
                                           (if (< n 2)
                                             (continue-ability state side (au (inc n)) card nil)
                                             (effect-completed state side eid)))})]
     {:async true
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
   {:async true
    :prompt "Select an installed card in a server to trash"
    :choices {:req #(and (= (last (:zone %)) :content)
                         (is-remote? (second (:zone %))))}
    :effect (effect (gain-credits (* 3 (get-counters target :advancement)))
                    (trash eid target nil))
    :msg (msg "trash " (card-str state target) " and gain "
              (* 3 (get-counters target :advancement)) " [Credits]")}

   "Bad Times"
   {:implementation "Any required program trashing is manual"
    :req (req tagged)
    :msg "force the Runner to lose 2[mu] until the end of the turn"
    :effect (req (lose state :runner :memory 2)
                 (when (neg? (available-mu state))
                   ;; Give runner a toast as well
                   (toast-check-mu state)
                   (system-msg state :runner "must trash programs to free up [mu]")))
    :end-turn {:effect (req (gain state :runner :memory 2)
                            (system-msg state :runner "regains 2[mu]"))}}

   "Beanstalk Royalties"
   {:msg "gain 3 [Credits]"
    :effect (effect (gain-credits 3))}

   "Best Defense"
   {:async true
    :req (req (not-empty (all-installed state :runner)))
    :effect (req (let [t (count-tags state)]
                   (continue-ability
                     state side
                     {:prompt (msg "Choose a Runner card with an install cost of " t " or less to trash")
                      :choices {:req #(and (installed? %)
                                           (<= (:cost %) t))}
                      :msg (msg "trash " (:title target))
                      :effect (effect (trash target))}
                     card nil)))}

   "Biased Reporting"
   (letfn [(num-installed [state t]
             (count (filter #(is-type? % t) (all-active-installed state :runner))))]
     {:async true
      :req (req (not-empty (all-active-installed state :runner)))
      :prompt "Choose a card type"
      :choices ["Hardware" "Program" "Resource"]
      :effect (req (let [t target
                         n (num-installed state t)]
                     (show-wait-prompt state :corp "Runner to choose cards to trash")
                     (wait-for
                       (resolve-ability
                         state :runner
                         {:prompt (msg "Choose any number of cards of type " t " to trash")
                          :choices {:max n
                                    :req #(and (installed? %) (is-type? % t))}
                          :effect (req (doseq [c targets]
                                         (trash state :runner c {:unpreventable true}))
                                       (gain-credits state :runner (count targets))
                                       (system-msg state :runner
                                                   (str "trashes "
                                                        (join ", " (map :title (sort-by :title targets)))
                                                        " and gains " (count targets) " [Credits]"))
                                       (effect-completed state side eid))}
                         card nil)
                       (clear-wait-prompt state :corp)
                       (let [n (* 2 (num-installed state t))]
                         (when (pos? n)
                           (gain-credits state :corp n)
                           (system-msg state :corp (str "uses Biased Reporting to gain " n " [Credits]")))
                         (effect-completed state side eid)))))})

   "Big Brother"
   {:req (req tagged)
    :msg "give the Runner 2 tags"
    :async true
    :effect (effect (gain-tags :corp eid 2))}

   "Bioroid Efficiency Research"
   {:implementation "Derez is manual"
    :choices {:req #(and (ice? %)
                         (has-subtype? % "Bioroid")
                         (installed? %)
                         (not (rezzed? %)))}
    :msg (msg "rez " (card-str state target {:visible true}) " at no cost")
    :effect (effect (rez target {:ignore-cost :all-costs})
                    (host (get-card state target) (assoc card :zone [:discard] :seen true :condition true)))}

   "Biotic Labor"
   {:msg "gain [Click][Click]"
    :effect (effect (gain :click 2))}

   "Blue Level Clearance"
   {:msg "gain 5 [Credits] and draw 2 cards"
    :async true
    :effect (effect (gain-credits 5)
                    (draw eid 2 nil))}

   "BOOM!"
   {:req (req (> (count-tags state) 1))
    :async true
    :msg "do 7 meat damage"
    :effect (effect (damage eid :meat 7 {:card card}))}

   "Building Blocks"
   {:choices {:req #(and (= (:side %) "Corp")
                         (has-subtype? % "Barrier")
                         (in-hand? %))}
    :async true
    :effect (req (corp-install state side eid target nil {:ignore-all-cost true :install-state :rezzed-no-cost}))}

   "Casting Call"
   {:choices {:req #(and (is-type? % "Agenda")
                         (in-hand? %))}
    :async true
    :effect (req (let [agenda target]
                   (continue-ability
                     state side {:prompt (str "Choose a server to install " (:title agenda))
                                 :choices (installable-servers state agenda)
                                 :effect (req (corp-install state side agenda target {:install-state :face-up})
                                              ; find where the agenda ended up and host on it
                                              (let [agenda (some #(when (= (:cid %) (:cid agenda)) %)
                                                                 (all-installed state :corp))]
                                                ; the operation ends up in :discard when it is played; to host it,
                                                ; we need (host) to look for it in discard.
                                                (host state side agenda (assoc card
                                                                               :zone [:discard]
                                                                               :seen true
                                                                               :installed true))
                                                (system-msg state side (str "hosts Casting Call on " (:title agenda)))))}
                     card nil)))
    :events {:access {:req (req (= (:cid target) (:cid (:host card))))
                      :async true
                      :effect (effect (gain-tags :runner eid 2)) :msg "give the Runner 2 tags"}}}

   "Celebrity Gift"
   {:choices {:max 5
              :req #(and (= (:side %) "Corp")
                         (in-hand? %))}
    :msg (msg "reveal " (join ", " (map :title (sort-by :title targets))) " and gain " (* 2 (count targets)) " [Credits]")
    :effect (effect (reveal targets) (gain-credits (* 2 (count targets))))}

   "Cerebral Cast"
   {:req (req (last-turn? state :runner :successful-run))
    :psi {:not-equal {:player :runner
                      :prompt "Take 1 tag or 1 brain damage?"
                      :choices ["1 tag" "1 brain damage"]
                      :msg (msg "give the Runner " target)
                      :effect (req (if (= target "1 tag")
                                     (gain-tags state :runner eid 1)
                                     (damage state side eid :brain 1 {:card card})))}}}

   "Cerebral Static"
   {:msg "disable the Runner's identity"
    :effect (effect (disable-identity :runner))
    :leave-play (effect (enable-identity :runner))}

   "\"Clones are not People\""
   {:events {:agenda-scored {:msg "add it to their score area as an agenda worth 1 agenda point"
                             :async true
                             :effect (req (as-agenda state :corp eid card 1))}}}

   "Closed Accounts"
   {:req (req tagged)
    :msg (msg "force the Runner to lose all " (:credit runner) " [Credits]")
    :effect (effect (lose-credits :runner :all))}

   "Commercialization"
   {:msg (msg "gain " (get-counters target :advancement) " [Credits]")
    :choices {:req ice?}
    :effect (effect (gain-credits (get-counters target :advancement)))}

   "Complete Image"
   (letfn [(name-a-card []
             {:async true
              :prompt "Name a Runner card"
              :choices {:card-title (req (and (card-is? target :side "Runner")
                                              (not (card-is? target :type "Identity"))))}
              :effect (effect (system-msg (str "uses Complete Image to name " target))
                              (continue-ability (damage-ability) card targets))})
           (damage-ability []
             {:async true
              :msg "do 1 net damage"
              :effect (req (wait-for (damage state side :net 1 {:card card})
                                     (when-let* [should-continue (not (:winner @state))
                                                 cards (some #(when (= (:cid (second %)) (:cid card)) (last %))
                                                             (turn-events state :corp :damage))
                                                 dmg (some #(when (= (:title %) target) %) cards)]
                                       (continue-ability state side (name-a-card) card nil))))})]
     {:async true
      :req (req (and (last-turn? state :runner :successful-run)
                     (<= 3 (:agenda-point runner))))
      :effect (effect (continue-ability (name-a-card) card nil))})

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
    :async true
    :effect (effect (shuffle-into-deck :hand)
                    (draw eid 5 nil))}

   "Cyberdex Trial"
   {:msg "purge virus counters"
    :effect (effect (purge))}

   "Death and Taxes"
   (let [gain-cred-effect {:msg "gain 1 [Credits]"
                           :effect (effect (gain-credits :corp 1))}]
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
                 (effect-completed state side eid)
                 (let [tgtcid (:cid target)]
                   (register-turn-flag!
                     state side
                     target :can-score
                     (fn [state side card]
                       (if (and (= (:cid card) tgtcid)
                                (>= (get-counters card :advancement) (or (:current-cost card)
                                                                         (:advancementcost card))))
                         ((constantly false) (toast state :corp "Cannot score due to Dedication Ceremony." "warning"))
                         true)))))}

   "Defective Brainchips"
   {:events {:pre-damage {:req (req (= target :brain))
                          :msg "do 1 additional brain damage"
                          :once :per-turn
                          :effect (effect (damage-bonus :brain 1))}}}

   "Distract the Masses"
   (let [shuffle-two {:async true
                      :effect (effect (rfg-and-shuffle-rd-effect (find-cid (:cid card) (:discard corp)) 2))}
         trash-from-hq {:async true
                        :prompt "Select up to 2 cards in HQ to trash"
                        :choices {:max 2
                                  :req #(and (= (:side %) "Corp")
                                             (in-hand? %))}
                        :msg (msg "trash " (quantify (count targets) "card") " from HQ")
                        :effect (req (wait-for
                                       (trash-cards state side targets nil)
                                       (continue-ability state side shuffle-two card nil)))
                        :cancel-effect (req (continue-ability state side shuffle-two card nil))}]
     {:async true
      :msg "give The Runner 2 [Credits]"
      :effect (effect (gain-credits :runner 2)
                      (continue-ability trash-from-hq card nil))})

   "Diversified Portfolio"
   (letfn [(number-of-non-empty-remotes [state]
             (count (filter #(not (empty? %))
                            (map #(:content (second %))
                                 (get-remotes state)))))]
     {:msg (msg "gain " (number-of-non-empty-remotes state)
             " [Credits]")
      :effect (effect (gain-credits (number-of-non-empty-remotes state)))})

   "Divert Power"
   {:async true
    :prompt "Select any number of cards to derez"
    :choices {:req #(and (installed? %)
                         (rezzed? %))
              :max (req (count (filter rezzed? (all-installed state :corp))))}
    :effect (req (doseq [c targets]
                   (derez state side c))
                 (let [discount (* -3 (count targets))]
                   (continue-ability
                     state side
                     {:async true
                      :prompt "Select a card to rez"
                      :choices {:req #(and (installed? %)
                                           (= (:side %) "Corp")
                                           (not (rezzed? %))
                                           (not (is-type? % "Agenda")))}
                      :effect (effect (rez-cost-bonus discount)
                                      (rez eid target nil))}
                     card nil)))}

   "Door to Door"
   {:events {:runner-turn-begins
             {:trace {:base 1
                      :label "Do 1 meat damage if Runner is tagged, or give the Runner 1 tag"
                      :successful {:msg (msg (if tagged
                                               "do 1 meat damage"
                                               "give the Runner 1 tag"))
                                   :async true
                                   :effect (req (if tagged
                                                  (damage state side eid :meat 1 {:card card})
                                                  (gain-tags state :corp eid 1)))}}}}}

   "Eavesdrop"
   (let [new-sub {:label "[Eavesdrop]: Trace 3 - Give the Runner 1 tag"}]
     {:implementation "On encounter effect is manual"
      :sub-effect {:label "Give the Runner 1 tag"
                   :trace {:base 3
                           :successful {:msg "give the Runner 1 tag"
                                        :async true
                                        :effect (effect (gain-tags :runner eid 1))}}}
      :choices {:req #(and (ice? %)
                           (installed? %))}
      :msg (msg "give " (card-str state target {:visible false}) " additional text")
      :effect (req (add-extra-sub state :corp (:cid card) (get-card state target) -1 new-sub)
                   (update-ice-strength state side target)
                   (host state side (get-card state target) (assoc card :zone [:discard] :seen true :condition true)))
      :leave-play (req (remove-extra-subs state :corp (:cid card) (:host card)))
      :events {:rez {:req (req (= (:cid target) (:cid (:host card))))
                     :effect (req (add-extra-sub state :corp (:cid card) (get-card state target) -1 new-sub))}}})

   "Economic Warfare"
   {:req (req (and (last-turn? state :runner :successful-run)
                   (can-pay? state :runner nil :credit 4)))
    :msg "make the runner lose 4 [Credits]"
    :effect (effect (lose-credits :runner 4))}

   "Election Day"
   {:req (req (->> (get-in @state [:corp :hand])
                   (filter #(not (= (:cid %) (:cid card))))
                   count
                   pos?))
    :async true
    :msg (msg "trash all cards in HQ and draw 5 cards")
    :effect (effect (trash-cards (get-in @state [:corp :hand]))
                    (draw eid 5 nil))}

   "Enforced Curfew"
   {:msg "reduce the Runner's maximum hand size by 1"
    :effect (effect (lose :runner :hand-size 1))
    :leave-play (effect (gain :runner :hand-size 1))}

   "Enforcing Loyalty"
   {:trace {:base 3
            :label "Trash a card not matching the faction of the Runner's identity"
            :successful
            {:async true
             :effect (req (let [f (:faction (:identity runner))]
                            (continue-ability
                              state side
                              {:prompt "Select an installed card not matching the faction of the Runner's identity"
                               :choices {:req #(and (installed? %)
                                                    (not= f (:faction %))
                                                    (card-is? % :side :runner))}
                               :msg (msg "trash " (:title target))
                               :effect (effect (trash target))}
                              card nil)))}}}

   "Enhanced Login Protocol"
   {:msg "uses Enhanced Login Protocol to add an additional cost of [Click] to make the first run not through a card ability this turn"
    :events {:pre-init-run {:req (req (and (first-event? state side :pre-init-run #(= :click-run (second %)))
                                           (= :click-run (second targets))))
                            :effect (effect (run-additional-cost-bonus [:click 1]))}}}

   "Exchange of Information"
   {:req (req (and tagged
                   (seq (:scored runner))
                   (seq (:scored corp))))
    :async true
    :effect (req
              (continue-ability
                state side
                {:prompt "Select a stolen agenda in the Runner's score area to swap"
                 :choices {:req #(in-runner-scored? state side %)}
                 :async true
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
                                               (effect-completed state side eid)))}
                               card nil)))}
                card nil))}

   "Fast Break"
   {:async true
    :req (req (-> runner :scored count pos?))
    :effect
    (req (let [X (-> runner :scored count)
               draw {:async true
                     :prompt "Draw how many cards?"
                     :choices {:number (req X)
                               :max (req X)
                               :default (req 1)}
                     :msg (msg "draw " target " cards")
                     :effect (effect (draw eid target nil))}
               install-cards (fn install-cards
                               [server n]
                               {:prompt "Select a card to install"
                                :choices {:req #(and (= (:side %) "Corp")
                                                     (not (is-type? % "Operation"))
                                                     (in-hand? %)
                                                     (seq (filter (fn [c] (= server c)) (corp-install-list state %))))}
                                :effect (req (wait-for
                                               (corp-install state side target server nil)
                                               (let [server (if (= "New remote" server)
                                                              (-> (turn-events state side :corp-install)
                                                                  ffirst :zone second zone->name)
                                                              server)]
                                                 (if (< n X)
                                                   (continue-ability state side (install-cards server (inc n)) card nil)
                                                   (effect-completed state side eid)))))
                                :cancel-effect (effect (effect-completed eid))})
               select-server {:async true
                              :prompt "Install cards in which server?"
                              :choices (req (conj (vec (get-remote-names state)) "New remote"))
                              :effect (effect (continue-ability (install-cards target 1) card nil))}]
           (gain-credits state :corp X)
           (wait-for (resolve-ability state side draw card nil)
                     (continue-ability state side select-server card nil))))}

   "Fast Track"
   {:prompt "Choose an Agenda"
    :choices (req (cancellable (filter #(is-type? % "Agenda") (:deck corp)) :sorted))
    :effect (effect (system-msg (str "adds " (:title target) " to HQ and shuffle R&D"))
                    (reveal target)
                    (shuffle! :deck)
                    (move target :hand))}

   "Financial Collapse"
   {:async true
    :req (req (and (>= (:credit runner) 6) (seq (filter #(is-type? % "Resource") (all-active-installed state :runner)))))
    :effect (req (let [rcount (count (filter #(is-type? % "Resource") (all-active-installed state :runner)))]
                   (if (pos? rcount)
                     (do (show-wait-prompt state :corp "Runner to trash a resource to prevent Financial Collapse")
                         (continue-ability
                           state side
                           {:prompt (msg "Trash a resource to prevent Financial Collapse?")
                            :choices ["Yes" "No"] :player :runner
                            :async true
                            :effect (effect (continue-ability
                                              (if (= target "Yes")
                                                {:player :runner
                                                 :prompt "Select a resource to trash"
                                                 :choices {:req #(and (is-type? % "Resource") (installed? %))}
                                                 :effect (req (trash state side target {:unpreventable true})
                                                              (system-msg state :runner
                                                                          (str "trashes " (:title target)
                                                                               " to prevent Financial Collapse"))
                                                              (clear-wait-prompt state :corp))}
                                                {:effect (effect (lose-credits :runner (* rcount 2))
                                                                 (clear-wait-prompt :corp))
                                                 :msg (msg "make the Runner lose " (* rcount 2) " [Credits]")})
                                              card nil))} card nil))
                     (continue-ability
                       state side
                       {:effect (effect (lose-credits :runner (* rcount 2)))
                        :msg (msg "make the Runner lose " (* rcount 2) " [Credits]")} card nil))))}

   "Focus Group"
   {:req (req (last-turn? state :runner :successful-run))
    :async true
    :prompt "Choose a card type"
    :choices ["Event" "Hardware" "Program" "Resource"]
    :effect (req (let [type target
                       numtargets (count (filter #(= type (:type %)) (:hand runner)))]
                   (system-msg
                     state :corp
                     (str "uses Focus Group to choose " target " and reveal the Runner's Grip ( "
                          (join ", " (map :title (sort-by :title (:hand runner)))) " )"))
                   (reveal state side (:hand runner))
                   (if (pos? numtargets)
                     (continue-ability
                       state :corp
                       {:async true
                        :prompt "Pay how many credits?"
                        :choices {:number (req numtargets)}
                        :effect (req (let [c target]
                                       (if (can-pay? state side (:title card) :credit c)
                                         (do (pay state :corp card :credit c)
                                             (continue-ability
                                               state :corp
                                               {:msg (msg "place " (quantify c " advancement token") " on "
                                                          (card-str state target))
                                                :choices {:req installed?}
                                                :effect (effect (add-prop target :advance-counter c {:placed true}))}
                                               card nil))
                                         (effect-completed state side eid))))}
                       card nil)
                     (effect-completed state side eid))))}

   "Foxfire"
   {:trace {:base 7
            :successful {:prompt "Select 1 card to trash"
                         :not-distinct true
                         :choices {:req #(and (installed? %)
                                              (or (has-subtype? % "Virtual")
                                                  (has-subtype? % "Link")))}
                         :msg "trash 1 virtual resource or link"
                         :effect (effect (trash target)
                                         (system-msg (str "trashes " (:title target))))}}}

   "Freelancer"
   {:req (req tagged)
    :msg (msg "trash " (join ", " (map :title (sort-by :title targets))))
    :choices {:max 2
              :req #(and (installed? %)
                         (is-type? % "Resource"))}
    :effect (effect (trash-cards :runner targets))}

   "Friends in High Places"
   (let [fhelper (fn fhp [n] {:prompt "Select a card in Archives to install with Friends in High Places"
                              :priority -1
                              :async true
                              :show-discard true
                              :choices {:req #(and (= (:side %) "Corp")
                                                   (not (is-type? % "Operation"))
                                                   (in-discard? %))}
                              :effect (req (wait-for
                                             (corp-install state side target nil nil)
                                             (do (system-msg state side (str "uses Friends in High Places to "
                                                                             (corp-install-msg target)))
                                                 (if (< n 2)
                                                   (continue-ability state side (fhp (inc n)) card nil)
                                                   (effect-completed state side eid)))))})]
     {:async true
      :effect (effect (continue-ability (fhelper 1) card nil))})

   "Fully Operational"
   (letfn [(full-servers [state]
             (filter #(and (not-empty (:content %))
                           (not-empty (:ices %)))
                     (vals (get-remotes state))))
           (repeat-choice [current total]
             ;; if current > total, this ability will auto-resolve and finish the chain of async methods.
             (when (<= current total)
               {:async true
                :prompt (str "Choice " current " of " total ": Gain 2 [Credits] or draw 2 cards? ")
                :choices ["Gain 2 [Credits]" "Draw 2 cards"]
                :msg (msg (lower-case target))
                :effect (req (if (= target "Gain 2 [Credits]")
                               (do (gain state :corp :credit 2)
                                   (continue-ability state side (repeat-choice (inc current) total)
                                                     card nil))
                               (wait-for (draw state :corp 2 nil) ; don't proceed with the next choice until the draw is done
                                         (continue-ability state side (repeat-choice (inc current) total)
                                                           card nil))))}))]
     {:async true
      :msg (msg "uses Fully Operational to make " (quantify (inc (count (full-servers state))) "gain/draw decision"))
      :effect (effect (continue-ability (repeat-choice 1 (inc (count (full-servers state))))
                                        card nil))})

   "Game Changer"
   {:effect (req (gain state side :click (count (:scored runner)))
                 (move state side (first (:play-area corp)) :rfg))}

   "Game Over"
   {:req (req (last-turn? state :runner :stole-agenda))
    :async true
    :prompt "Choose a card type"
    :choices ["Hardware" "Program" "Resource"]
    :effect (req (let [type target
                       trashtargets (filter #(and (is-type? % type)
                                                  (not (has? % :subtype "Icebreaker")))
                                            (all-active-installed state :runner))
                       numtargets (count trashtargets)
                       typemsg (str (when (= type "Program") "non-Icebreaker ") type
                                     (when-not (= type "Hardware") "s"))]
                   (system-msg state :corp (str "chooses to trash all " typemsg))
                   (show-wait-prompt state :corp "Runner to prevent trashes")
                   (wait-for
                     (resolve-ability
                       state :runner
                       {:async true
                        :req (req (<= 3 (:credit runner)))
                        :prompt (msg "Prevent any " typemsg " from being trashed? Pay 3 [Credits] per card.")
                        :choices {:max (req (min numtargets (quot (:credit runner) 3)))
                                  :req #(and (installed? %)
                                             (is-type? % type)
                                             (not (has? % :subtype "Icebreaker")))}
                        :effect (req (pay state :runner card :credit (* 3 (count targets)))
                                     (system-msg
                                       state :runner
                                       (str "pays " (* 3 (count targets)) " [Credits] to prevent the trashing of "
                                            (join ", " (map :title (sort-by :title targets)))))
                                     (system-msg state :corp (str "trashes all other " typemsg))
                                     (effect-completed state side (make-result eid targets)))}
                       card nil)
                     (trash-cards state side (clojure.set/difference (set trashtargets) (set async-result)))
                     (clear-wait-prompt state :corp)
                     (gain-bad-publicity state :corp 1)
                     (system-msg state :corp "take 1 bad publicity")
                     (effect-completed state side eid))))}

   "Genotyping"
   {:async true
    :effect (effect (mill :corp 2)
                    (system-msg "trashes the top 2 cards of R&D")
                    (rfg-and-shuffle-rd-effect eid (first (:play-area corp)) 4 false))}

   "Green Level Clearance"
   {:msg "gain 3 [Credits] and draw 1 card"
    :async true
    :effect (effect (gain-credits 3)
                    (draw eid 1 nil))}

   "Hangeki"
   {:req (req (last-turn? state :runner :trashed-card))
    :async true
    :prompt "Choose an installed Corp card"
    :choices {:req #(and (= (:side %) "Corp")
                         (installed? %))}
    :effect (effect (show-wait-prompt :corp "Runner to resolve Hangeki")
                    (continue-ability
                      {:optional
                       {:player :runner
                        :async true
                        :prompt "Access card? (If not, add Hangeki to your score area worth -1 agenda point)"
                        :yes-ability
                        {:effect (req (clear-wait-prompt state :corp)
                                      (wait-for (access-card state side target)
                                                (move state :corp (find-latest state card) :rfg)
                                                (system-msg state :corp "removes Hangeki from the game")
                                                (effect-completed state side eid)))}
                        :no-ability
                        {:msg "add it to the Runner's score area as an agenda worth -1 agenda point"
                         :effect (effect (clear-wait-prompt :corp)
                                         (as-agenda :runner eid (find-latest state card) -1))}}}
                      card targets))}

   "Hard-Hitting News"
   {:req (req (last-turn? state :runner :made-run))
    :trace {:base 4
            :label "Give the Runner 4 tags"
            :successful {:async true
                         :msg "give the Runner 4 tags"
                         :effect (effect (gain-tags eid 4))}}}

   "Hasty Relocation"
   (letfn [(hr-final [chosen original]
             {:prompt (str "The top cards of R&D will be " (clojure.string/join  ", " (map :title chosen)) ".")
              :choices ["Done" "Start over"]
              :async true
              :effect (req (if (= target "Done")
                             (do (doseq [c (reverse chosen)] (move state :corp c :deck {:front true}))
                                 (clear-wait-prompt state :runner)
                                 (effect-completed state side eid))
                             (continue-ability state side (hr-choice original '() 3 original)
                                               card nil)))})
           (hr-choice [remaining chosen n original]
             {:prompt "Choose a card to move next onto R&D"
              :choices remaining
              :async true
              :effect (req (let [chosen (cons target chosen)]
                             (if (< (count chosen) n)
                               (continue-ability state side (hr-choice (remove-once #(= target %) remaining)
                                                                       chosen n original) card nil)
                               (continue-ability state side (hr-final chosen original) card nil))))})]
     {:additional-cost [:mill 1]
      :async true
      :msg "trash the top card of R&D, draw 3 cards, and add 3 cards in HQ to the top of R&D"
      :effect (req (wait-for (draw state side 3 nil)
                             (do (show-wait-prompt state :runner "Corp to add 3 cards in HQ to the top of R&D")
                                 (let [from (get-in @state [:corp :hand])]
                                   (continue-ability state :corp (hr-choice from '() 3 from) card nil)))))})

   "Hatchet Job"
   {:trace {:base 5
            :successful {:choices {:req #(and (installed? %)
                                              (card-is? % :side :runner)
                                              (not (has-subtype? % "Virtual")))}
                         :msg "add an installed non-virtual card to the Runner's grip"
                         :effect (effect (move :runner target :hand true))}}}

   "Hedge Fund"
   {:msg "gain 9 [Credits]" :effect (effect (gain-credits 9))}

   "Hellion Alpha Test"
   {:req (req (last-turn? state :runner :installed-resource))
    :trace {:base 2
            :successful {:msg "add a Resource to the top of the Stack"
                         :choices {:req #(and (installed? %)
                                              (is-type? % "Resource"))}
                         :effect (effect (move :runner target :deck {:front true})
                                         (system-msg (str "adds " (:title target) " to the top of the Stack")))}
            :unsuccessful {:msg "take 1 bad publicity"
                           :effect (effect (gain-bad-publicity :corp 1))}}}

   "Hellion Beta Test"
   {:req (req (last-turn? state :runner :trashed-card))
    :trace {:base 2
            :label "Trace 2 - Trash 2 installed non-program cards or take 1 bad publicity"
            :successful {:choices {:max (req (min 2 (count (filter #(or (:facedown %)
                                                                        (not (is-type? % "Program")))
                                                                   (concat (all-installed state :corp)
                                                                           (all-installed state :runner))))))
                                   :all true
                                   :req #(and (installed? %)
                                              (not (is-type? % "Program")))}
                         :msg (msg "trash " (join ", " (map :title (sort-by :title targets))))
                         :effect (req (doseq [c targets]
                                        (trash state side c)))}
            :unsuccessful {:msg "take 1 bad publicity"
                           :effect (effect (gain-bad-publicity :corp 1))}}}

   "Heritage Committee"
   {:async true
    :effect (req (wait-for (draw state side 3 nil)
                           (continue-ability state side
                                             {:prompt "Select a card in HQ to put on top of R&D"
                                              :choices {:req #(and (= (:side %) "Corp")
                                                                   (in-hand? %))}
                                              :msg "draw 3 cards and add 1 card from HQ to the top of R&D"
                                              :effect (effect (move target :deck {:front true}))}
                                             card nil)))}

   "High-Profile Target"
   (letfn [(dmg-count [state] (* 2 (count-tags state)))]
     {:req (req tagged)
      :async true
      :msg (msg "do " (dmg-count state) " meat damage")
      :effect (effect (damage eid :meat (dmg-count state) {:card card}))})

   "Housekeeping"
   {:events {:runner-install {:player :runner
                              :prompt "Select a card from your Grip to trash for Housekeeping" :once :per-turn
                              :choices {:req #(and (= (:side %) "Runner")
                                                   (in-hand? %))}
                              :msg (msg "force the Runner to trash " (:title target) " from their Grip")
                              :effect (effect (trash target {:unpreventable true}))}}}

   "Hunter Seeker"
   {:req (req (last-turn? state :runner :stole-agenda))
    :async true
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
    :effect (effect (corp-install target nil {:ignore-install-cost true}))
    :msg (msg (corp-install-msg target))}

   "Invasion of Privacy"
   (letfn [(iop [x]
             {:async true
              :req (req (->> (:hand runner)
                             (filter #(or (is-type? % "Resource")
                                          (is-type? % "Event")))
                             count
                             pos?))
              :prompt "Choose a resource or event to trash"
              :msg (msg "trash " (:title target))
              :choices (req (cancellable
                              (filter #(or (is-type? % "Resource")
                                           (is-type? % "Event"))
                                      (:hand runner))
                              :sorted))
              :effect (req (trash state side target)
                           (if (pos? x)
                             (continue-ability state side (iop (dec x)) card nil)
                             (effect-completed state side eid)))})]
     {:trace {:base 2
              :successful {:msg "reveal the Runner's Grip and trash up to X resources or events"
                           :effect (req (reveal state side (:hand runner))
                                        (let [x (- target (second targets))]
                                          (system-msg
                                            state :corp
                                            (str "reveals the Runner's Grip ( "
                                                 (join ", " (map :title (sort-by :title (:hand runner))))
                                                 " ) and can trash up to " x " resources or events"))
                                          (continue-ability state side (iop (dec x)) card nil)))}
              :unsuccessful {:msg "take 1 bad publicity"
                             :effect (effect (gain-bad-publicity :corp 1))}}})

   "IPO"
   {:msg "gain 13 [Credits]" :effect (effect (gain-credits 13))}

   "Kill Switch"
   (let [trace-for-brain-damage {:msg (msg "reveal that they accessed " (:title target))
                                 :trace {:base 3
                                         :successful {:msg "do 1 brain damage"
                                                      :async true
                                                      :effect (effect (damage :runner eid :brain 1 {:card card}))}}}]
     {:events {:access (assoc trace-for-brain-damage :req (req (is-type? target "Agenda"))
                              :interactive (req (is-type? target "Agenda")))
               :agenda-scored trace-for-brain-damage}})

   "Lag Time"
   {:effect (effect (update-all-ice))
    :events {:pre-ice-strength {:effect (effect (ice-strength-bonus 1 target))}}
    :leave-play (effect (update-all-ice))}

   "Lateral Growth"
   {:async true
    :msg "gain 4 [Credits]"
    :effect (effect (gain-credits 4)
                    (continue-ability {:player :corp
                                       :prompt "Select a card to install"
                                       :choices {:req #(and (= (:side %) "Corp")
                                                            (not (is-type? % "Operation"))
                                                            (in-hand? %))}
                                       :async true
                                       :msg (msg (corp-install-msg target))
                                       :effect (effect (corp-install eid target nil nil))}
                                      card nil))}

   "Liquidation"
   {:async true
    :req (req (some #(and (rezzed? %) (not (is-type? % "Agenda"))) (all-installed state :corp)))
    :effect (req (let [n (count (filter #(not (is-type? % "Agenda")) (all-active-installed state :corp)))]
                   (continue-ability
                     state side
                     {:prompt "Select any number of rezzed cards to trash"
                      :choices {:max n
                                :req #(and (rezzed? %)
                                           (not (is-type? % "Agenda")))}
                      :msg (msg "trash " (join ", " (map :title (sort-by :title targets))) " and gain " (* (count targets) 3) " [Credits]")
                      :effect (req (doseq [c targets]
                                     (trash state side c))
                                   (gain-credits state side (* (count targets) 3)))}
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
    :async true
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
                              :trace {:base 2
                                      :successful {:msg "give the Runner 1 tag"
                                                   :async true
                                                   :effect (effect (gain-tags eid 1))}}}}}

   "Market Forces"
   (letfn [(credit-diff [state]
             (min (* 3 (count-tags state))
                  (get-in @state [:runner :credit])))]
     {:req (req tagged)
      :msg (msg (let [c (credit-diff state)]
                  (str "make the runner lose " c " [Credits], and gain " c " [Credits]")))
      :effect (req (let [c (credit-diff state)]
                     (lose-credits state :runner c)
                     (gain-credits state :corp c)))})

   "Mass Commercialization"
   {:msg (msg "gain " (* 2 (count (filter #(pos? (+ (get-counters % :advancement) (:extra-advance-counter % 0)))
                                          (get-all-installed state)))) " [Credits]")
    :effect (effect (gain-credits (* 2 (count (filter #(pos? (+ (get-counters % :advancement) (:extra-advance-counter % 0)))
                                                      (get-all-installed state))))))}

   "MCA Informant"
   {:implementation "Runner must deduct 1 click and 2 credits, then trash host manually"
    :req (req (not-empty (filter #(has-subtype? % "Connection")
                                 (all-active-installed state :runner))))
    :prompt "Choose a connection to host MCA Informant on it"
    :choices {:req #(and (= (:side %) "Runner")
                         (has-subtype? % "Connection")
                         (installed? %))}
    :msg (msg "host it on " (card-str state target) ". The Runner has an additional tag")
    :effect (req (host state side (get-card state target) (assoc card :zone [:discard] :seen true))
                 (swap! state update-in [:runner :tag :additional] inc)
                 (trigger-event state :corp :runner-additional-tag-change 1))
    :leave-play (req (swap! state update-in [:runner :tag :additional] dec)
                     (trigger-event state :corp :runner-additional-tag-change 1)
                     (system-msg state :corp "trashes MCA Informant"))}

   "Medical Research Fundraiser"
   {:msg "gain 8 [Credits]. The Runner gains 3 [Credits]"
    :effect (effect (gain-credits 8) (gain-credits :runner 3))}

   "Midseason Replacements"
   {:req (req (last-turn? state :runner :stole-agenda))
    :trace {:base 6
            :label "Trace 6 - Give the Runner X tags"
            :successful {:msg "give the Runner X tags"
                         :async true
                         :effect (effect (system-msg
                                           (str "gives the Runner " (- target (second targets)) " tags"))
                                         (gain-tags eid (- target (second targets))))}}}

   "Mushin No Shin"
   {:prompt "Select a card to install from HQ"
    :choices {:req #(and (#{"Asset" "Agenda" "Upgrade"} (:type %))
                         (= (:side %) "Corp")
                         (in-hand? %))}
    :effect (req (corp-install state side (assoc target :advance-counter 3) "New remote")
                 (effect-completed state side eid)
                 (let [tgtcid (:cid target)]
                   (register-turn-flag!
                     state side
                     card :can-rez
                     (fn [state side card]
                       (if (= (:cid card) tgtcid)
                         ((constantly false) (toast state :corp "Cannot rez due to Mushin No Shin." "warning"))
                         true)))
                   (register-turn-flag!
                     state side
                     card :can-score
                     (fn [state side card]
                       (if (and (= (:cid card) tgtcid)
                                (>= (get-counters card :advancement) (or (:current-cost card) (:advancementcost card))))
                         ((constantly false) (toast state :corp "Cannot score due to Mushin No Shin." "warning"))
                         true)))))}

   "Mutate"
   {:req (req (some #(and (ice? %)
                          (rezzed? %))
                    (all-installed state :corp)))
    :prompt "Select a rezzed piece of ice to trash"
    :choices {:req #(and (ice? %)
                         (rezzed? %))}
    :async true
    :effect (req (let [i (ice-index state target)
                       [revealed-cards r] (split-with (complement ice?) (get-in @state [:corp :deck]))
                       titles (->> (conj (vec revealed-cards) (first r))
                                   (filter identity)
                                   (map :title))]
                   (wait-for (trash state :corp target nil)
                             (do
                               (system-msg state side (str "uses Mutate to trash " (:title target)))
                               (when (seq titles)
                                 (reveal state side revealed-cards)
                                 (system-msg state side (str "reveals " (clojure.string/join ", " titles) " from R&D")))
                               (if-let [ice (first r)]
                                 (let [newice (assoc ice :zone (:zone target) :rezzed true)
                                       ices (get-in @state (cons :corp (:zone target)) [])
                                       newices (apply conj (subvec ices 0 i) newice (subvec ices i))]
                                   (swap! state assoc-in (cons :corp (:zone target)) newices)
                                   (swap! state update-in [:corp :deck] (fn [coll] (remove-once #(= (:cid %) (:cid newice)) coll)))
                                   (trigger-event state side :corp-install newice)
                                   (card-init state side newice {:resolve-effect false
                                                                 :init-data true})
                                   (system-msg state side (str "uses Mutate to install and rez " (:title newice) " from R&D at no cost"))
                                   (trigger-event state side :rez newice))
                                 (system-msg state side (str "does not find any ICE to install from R&D")))
                               (shuffle! state :corp :deck)
                               (effect-completed state side eid)))))}

   "Neural EMP"
   {:req (req (last-turn? state :runner :made-run))
    :msg "do 1 net damage"
    :effect (effect (damage eid :net 1 {:card card}))}

   "O₂ Shortage"
   {:async true
    :effect (req (if (empty? (:hand runner))
                   (do (gain state :corp :click 2)
                       (system-msg state side (str "uses O₂ Shortage to gain [Click][Click]"))
                       (effect-completed state side eid))
                   (do (show-wait-prompt state :corp "Runner to decide whether or not to trash a card from their Grip")
                       (continue-ability
                         state side
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
    :req (req (and (pos? (count-tags state))
                   (< (:credit runner) 6)))
    :async true
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
    :effect (effect (rez target {:ignore-cost :all-costs})
                    (host (get-card state target) (assoc card :zone [:discard] :seen true :condition true)))}

   "Patch"
   {:choices {:req #(and (ice? %) (rezzed? %))}
    :msg (msg "give +2 strength to " (card-str state target))
    :effect (effect (host target (assoc card :zone [:discard] :seen true :condition true))
                    (update-ice-strength (get-card state target)))
    :events {:pre-ice-strength {:req (req (= (:cid target) (:cid (:host card))))
                                :effect (effect (ice-strength-bonus 2 target))}}}

   "Paywall Implementation"
   {:events {:successful-run {:msg "gain 1 [Credits]" :effect (effect (gain-credits :corp 1))}}}

   "Peak Efficiency"
   {:msg (msg "gain " (reduce (fn [c server]
                                (+ c (count (filter (fn [ice] (:rezzed ice)) (:ices server)))))
                              0 (flatten (seq (:servers corp))))
              " [Credits]")
    :effect (effect (gain-credits
                      (reduce (fn [c server]
                                (+ c (count (filter (fn [ice] (:rezzed ice)) (:ices server)))))
                              0 (flatten (seq (:servers corp))))))}

   "Power Grid Overload"
   {:req (req (last-turn? state :runner :made-run))
    :trace {:base 2
            :successful {:msg "trash 1 piece of hardware"
                         :async true
                         :effect (req (let [max-cost (- target (second targets))]
                                        (continue-ability
                                          state side
                                          {:choices {:req #(and (is-type? % "Hardware")
                                                                (<= (:cost %) max-cost))}
                                           :msg (msg "trash " (:title target))
                                           :effect (effect (trash target))}
                                          card nil))
                                      (system-msg
                                        state :corp
                                        (str "trashes 1 piece of hardware with install cost less than or equal to "
                                             (- target (second targets)))))}}}

   "Power Shutdown"
   {:req (req (and (last-turn? state :runner :made-run))
              (not-empty (filter #(or (= "Program" (:type %)) (= "Hardware" (:type %)))
                                 (all-active-installed state :runner))))
    :prompt "Trash how many cards from the top R&D?"
    :choices {:number (req (apply max (map :cost (filter #(or (= "Program" (:type %)) (= "Hardware" (:type %))) (all-active-installed state :runner)))))}
    :msg (msg "trash " target " cards from the top of R&D")
    :async true
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
   {:async true
    :msg "rearrange the top 5 cards of R&D"
    :effect (req (show-wait-prompt state :runner "Corp to rearrange the top cards of R&D")
                 (let [from (take 5 (:deck corp))]
                   (if (pos? (count from))
                     (continue-ability state side (reorder-choice :corp :runner from '()
                                                                  (count from) from) card nil)
                     (do (clear-wait-prompt state :runner)
                         (effect-completed state side eid)))))}

   "Predictive Algorithm"
   {:events {:pre-steal-cost {:effect (effect (steal-cost-bonus [:credit 2]))}}}

   "Preemptive Action"
   {:effect (effect (rfg-and-shuffle-rd-effect (first (:play-area corp)) (min (count (:discard corp)) 3) true))}

   "Priority Construction"
   (letfn [(install-card [chosen]
             {:prompt "Select a remote server"
              :choices (req (conj (vec (get-remote-names state)) "New remote"))
              :async true
              :effect (effect (corp-install (assoc chosen :advance-counter 3) target {:ignore-all-cost true}))})]
     {:async true
      :prompt "Choose a piece of ICE in HQ to install"
      :choices {:req #(and (in-hand? %) (= (:side %) "Corp") (ice? %))}
      :msg "install an ICE from HQ and place 3 advancements on it"
      :cancel-effect (req (effect-completed state side eid))
      :effect (effect (continue-ability (install-card target) card nil))})

   "Product Recall"
   {:async true
    :prompt "Select a rezzed asset or upgrade to trash"
    :choices {:req #(and (rezzed? %)
                         (or (is-type? % "Asset")
                             (is-type? % "Upgrade")))}
    :effect (req (let [tcost (modified-trash-cost state side target)]
                   (wait-for (trash state side target {:unpreventable true})
                             (do (gain-credits state :corp tcost)
                                 (system-msg state side (str "uses Product Recall to trash " (card-str state target)
                                                             " and gain " tcost "[Credits]"))
                                 (effect-completed state side eid)))))}

   "Psychographics"
   {:req (req tagged)
    :async true
    :prompt "Pay how many credits?"
    :choices {:number (req (count-tags state))}
    :effect (req (let [c target]
                   (when (can-pay? state side (:title card) :credit c)
                     (pay state :corp card :credit c)
                     (continue-ability
                       state side
                       {:msg (msg "place " (quantify c " advancement token") " on " (card-str state target))
                        :choices {:req can-be-advanced?}
                        :effect (effect (add-prop target :advance-counter c {:placed true}))}
                       card nil))))}

   "Psychokinesis"
   (letfn [(choose-card [state cards]
             (let [allowed-cards (filter #(some #{"New remote"} (installable-servers state %))
                                         cards)]
               {:prompt "Select an agenda, asset, or upgrade to install"
                :choices (cons "None" allowed-cards)
                :async true
                :effect (req (if-not (or (= target "None") (ice? target) (is-type? target "Operation"))
                               (continue-ability state side (install-card target) card nil)
                               (system-msg state side "does not install an asset, agenda, or upgrade"))
                             (effect-completed state side eid)
                             (clear-wait-prompt state :runner))}))
           (install-card [chosen]
             {:prompt "Select a remote server"
              :choices (req (conj (vec (get-remote-names state)) "New remote"))
              :async true
              :effect (effect (clear-wait-prompt :runner)
                              (corp-install (move state side chosen :play-area) target))})]
     {:msg "look at the top 5 cards of R&D"
      :async true
      :effect (req (show-wait-prompt state :runner "Corp to look at the top cards of R&D")
                (let [top-5 (take 5 (:deck corp))]
                  (continue-ability state side (choose-card state top-5) card nil)))})

   "Punitive Counterstrike"
   {:trace {:base 5
            :successful {:async true
                         :msg (msg "do " (:stole-agenda runner-reg-last 0) " meat damage")
                         :effect (effect (damage eid :meat (:stole-agenda runner-reg-last 0) {:card card}))}}}

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
                                      :effect (req (reveal state side cards)
                                                   (doseq [c (->> cards
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
                                                              (not-any? #{(:title %)} selected)) (:deck corp)) :sorted))
                     :msg (msg "put " (:title target) " into HQ")
                     :async true
                     :effect (req (move state side target :hand)
                                  (continue-ability
                                    state side
                                    (rt total (dec left) (cons (:title target) selected))
                                    card nil))}
                    {:effect (effect (shuffle! :corp :deck))
                     :msg (msg "shuffle R&D")}))]
     {:prompt "How many Sysops?"
      :async true
      :choices :credit
      :msg (msg "search for " target " Sysops")
      :effect (effect (continue-ability (rthelp target target []) card nil))})

   "Red Level Clearance"
   (let [all [{:msg "gain 2 [Credits]"
               :effect (effect (gain-credits 2))}
              {:msg "draw 2 cards"
               :effect (effect (draw 2))}
              {:msg "gain [Click]"
               :effect (effect (gain :click 1))}
              {:prompt "Choose a non-agenda to install"
               :msg "install a non-agenda from hand"
               :choices {:req #(and (not (is-type? % "Agenda"))
                                    (not (is-type? % "Operation"))
                                    (in-hand? %))}
               :async true
               :effect (effect (corp-install eid target nil nil))}]
         can-install? (fn [hand]
                        (seq (filter #(and (not (is-type? % "Agenda"))
                                           (not (is-type? % "Operation")))
                                     hand)))
         choice (fn choice [abis chose-once]
                  {:prompt "Choose an ability to resolve"
                   :choices (map #(capitalize (:msg %)) abis)
                   :async true
                   :effect (req (let [chosen (some #(when (= target (capitalize (:msg %))) %) abis)]
                                  (if (or (not= target "Install a non-agenda from hand")
                                          (and (= target "Install a non-agenda from hand")
                                               (can-install? (:hand corp))))
                                    (wait-for (resolve-ability state side chosen card nil)
                                              (if (false? chose-once)
                                                (continue-ability state side (choice abis true) card nil)
                                                (effect-completed state side eid)))
                                    (continue-ability state side (choice abis chose-once) card nil))))})]
     {:async true
      :effect (effect (continue-ability (choice all false) card nil))})



   "Red Planet Couriers"
   {:async true
    :req (req (some #(can-be-advanced? %) (all-installed state :corp)))
    :prompt "Select an installed card that can be advanced"
    :choices {:req can-be-advanced?}
    :effect (req (let [installed (get-all-installed state)
                       total-adv (reduce + (map #(get-counters % :advancement) installed))]
                   (doseq [c installed]
                     (set-prop state side c :advance-counter 0))
                   (set-prop state side target :advance-counter total-adv)
                   (update-all-ice state side)
                   (system-msg state side (str "uses Red Planet Couriers to move " total-adv
                                               " advancement tokens to " (card-str state target)))
                   (effect-completed state side eid)))}

   "Replanting"
   (letfn [(replant [n]
             {:prompt "Select a card to install with Replanting"
              :async true
              :choices {:req #(and (= (:side %) "Corp")
                                   (not (is-type? % "Operation"))
                                   (in-hand? %))}
              :effect (req (wait-for (corp-install state side target nil {:ignore-all-cost true})
                                     (if (< n 2)
                                       (continue-ability state side (replant (inc n)) card nil)
                                       (effect-completed state side eid))))})]
     {:async true
      :prompt "Select an installed card to add to HQ"
      :choices {:req #(and (= (:side %) "Corp")
                           (installed? %))}
      :msg (msg "add " (card-str state target) " to HQ, then install 2 cards ignoring all costs")
      :effect (req (move state side target :hand)
                   (resolve-ability state side (replant 1) card nil))})

   "Restore"
   {:async true
    :effect (effect (continue-ability
                      {:prompt "Select a card in Archives to install & rez with Restore"
                       :priority -1
                       :async true
                       :show-discard true
                       :choices {:req #(and (= (:side %) "Corp")
                                            (not (is-type? % "Operation"))
                                            (in-discard? %))}
                       :effect (req (wait-for
                                      (corp-install state side target nil {:install-state :rezzed})
                                      (do (system-msg state side (str "uses Restore to "
                                                                      (corp-install-msg target)))
                                          (let [leftover (filter #(= (:title target) (:title %)) (-> @state :corp :discard))]
                                            (when (seq leftover)
                                              (doseq [c leftover]
                                                (move state side c :rfg))
                                              (system-msg state side (str "removes " (count leftover) " copies of " (:title target) " from the game"))))
                                          (effect-completed state side eid))))}
                      card nil))}

   "Restoring Face"
   {:async true
    :prompt "Select a Sysop, Executive or Clone to trash"
    :msg (msg "trash " (card-str state target) " to remove 2 bad publicity")
    :choices {:req #(and (rezzed? %)
                         (or (has-subtype? % "Clone")
                             (has-subtype? % "Executive")
                             (has-subtype? % "Sysop")))}
    :effect (effect (lose :bad-publicity 2)
                    (trash eid target nil))}

   "Restructure"
   {:msg "gain 15 [Credits]"
    :effect (effect (gain-credits 15))}

   "Reuse"
   {:async true
    :effect (req (let [n (count (:hand corp))]
                   (continue-ability
                     state side
                     {:prompt (msg "Select up to " n " cards in HQ to trash with Reuse")
                      :choices {:max n
                                :req #(and (= (:side %) "Corp")
                                           (in-hand? %))}
                      :msg (msg (let [m (count targets)]
                                  (str "trash " (quantify m "card")
                                       " and gain " (* 2 m) " [Credits]")))
                      :effect (effect (trash-cards targets)
                                      (gain-credits (* 2 (count targets))))} card nil)))}

   "Reverse Infection"
   {:prompt "Choose One:"
    :choices ["Purge virus counters."
              "Gain 2 [Credits]"]
    :effect (req (if (= target "Gain 2 [Credits]")
                   (do (gain-credits state side 2)
                       (system-msg state side "uses Reverse Infection to gain 2 [Credits]"))
                   (let [pre-purge-virus (number-of-virus-counters state)]
                     (purge state side)
                     (let [post-purge-virus (number-of-virus-counters state)
                           num-virus-purged (- pre-purge-virus post-purge-virus)
                           num-to-trash (quot num-virus-purged 3)]
                       (mill state :corp :runner num-to-trash)
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
    :effect (effect (move target :deck)
                    (shuffle! :deck))}

   "Riot Suppression"
   {:req (req (last-turn? state :runner :trashed-card))
    :async true
    :effect (req (let [c card]
                   (show-wait-prompt state :corp "Runner to decide if they will take 1 brain damage")
                   (continue-ability
                     state :runner
                     {:optional
                      {:prompt "Take 1 brain damage to prevent having 3 fewer clicks next turn?"
                       :player :runner
                       :end-effect (req (clear-wait-prompt state :corp)
                                        (move state :corp (find-latest state c) :rfg))
                       :yes-ability
                       {:async true
                        :effect (req (system-msg
                                       state :runner
                                       "suffers 1 brain damage to prevent losing 3[Click] to Riot Suppression")
                                     (damage state :runner eid :brain 1 {:card card}))}
                       :no-ability
                       {:msg "give the runner 3 fewer [Click] next turn"
                        :effect (req (swap! state update-in [:runner :extra-click-temp] (fnil #(- % 3) 0)))}}}
                     card nil)))}

   "Rolling Brownout"
   {:msg "increase the play cost of operations and events by 1 [Credits]"
    :events {:play-event {:once :per-turn
                          :msg "gain 1 [Credits]"
                          :effect (effect (gain-credits :corp 1))}
             :pre-play-instant {:effect (effect (play-cost-bonus [:credit 1]))}}}

   "Rover Algorithm"
   {:choices {:req #(and (ice? %) (rezzed? %))}
    :msg (msg "host it as a condition counter on " (card-str state target))
    :effect (effect (host target (assoc card :zone [:discard] :seen true :condition true))
                    (update-ice-strength (get-card state target)))
    :events {:pass-ice {:req (req (= (:cid target) (:cid (:host card))))
                        :effect (effect (add-counter card :power 1))}
             :pre-ice-strength {:req (req (= (:cid target) (:cid (:host card))))
                                :effect (effect (ice-strength-bonus (get-counters card :power) target))}}}

   "Sacrifice"
   {:req (req (and (pos? (:bad-publicity corp))
                   (some #(pos? (:agendapoints %)) (:scored corp))))
    :additional-cost [:forfeit]
    :effect (req (let [bp-lost (max 0 (min (:agendapoints (last (:rfg corp)))
                                           (:bad-publicity corp)))]
                   (system-msg state side (str "uses Sacrifice to lose " bp-lost " bad publicity and gain " bp-lost " [Credits]"))
                   (when (pos? bp-lost)
                     (lose state side :bad-publicity bp-lost)
                     (gain-credits state side bp-lost))))}

   "Salem's Hospitality"
   {:prompt "Name a Runner card"
    :choices {:card-title (req (and (card-is? target :side "Runner")
                                    (not (card-is? target :type "Identity"))))}
    :effect (req (system-msg state side
                             (str "uses Salem's Hospitality to reveal the Runner's Grip ( "
                                  (join ", " (map :title (sort-by :title (:hand runner))))
                                  " ) and trash any copies of " target))
                 (reveal state side (filter #(= target (:title %)) (:hand runner)))
                 (doseq [c (filter #(= target (:title %)) (:hand runner))]
                   (trash state side c {:unpreventable true})))}

   "Scarcity of Resources"
   {:msg "increase the install cost of resources by 2"
    :events {:pre-install {:req (req (and (is-type? target "Resource")
                                          (not (second targets)))) ; not facedown
                           :effect (effect (install-cost-bonus [:credit 2]))}}}

   "Scorched Earth"
   {:req (req tagged)
    :async true
    :msg "do 4 meat damage"
    :effect (effect (damage eid :meat 4 {:card card}))}

   "SEA Source"
   {:req (req (last-turn? state :runner :successful-run))
    :trace {:base 3
            :label "Trace 3 - Give the Runner 1 tag"
            :successful {:msg "give the Runner 1 tag"
                         :async true
                         :effect (effect (gain-tags :corp eid 1))}}}

   "Secure and Protect"
   {:interactive (req true)
    :async true
    :effect (req (if (seq (filter ice? (:deck corp)))
                   (do (show-wait-prompt state :runner "Corp to use Secure and Protect")
                       (continue-ability
                         state side
                         {:async true
                          :prompt "Choose a piece of ice"
                          :choices (req (filter ice? (:deck corp)))
                          :effect (req (let [chosen-ice target]
                                         (continue-ability
                                           state side
                                           {:async true
                                            :prompt (str "Select a server to install " (:title chosen-ice) " on")
                                            :choices ["Archives" "R&D" "HQ"]
                                            :msg (msg "reveal " (:title chosen-ice) "and install it, paying 3[credit] less")
                                            :effect (effect (clear-wait-prompt :runner)
                                                            (reveal state side chosen-ice)
                                                            (shuffle! :deck)
                                                            (install-cost-bonus [:credit -3])
                                                            (corp-install eid chosen-ice target nil))}
                                           card nil)))}
                         card nil))
                   (do (shuffle! state side :deck)
                       (effect-completed state side eid))))}

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
   {:msg "add a cost of 1 [Credit] for the Runner to make the first run each turn"
    :events {:pre-init-run {:req (req (first-event? state side :pre-init-run))
                            :effect (effect (run-additional-cost-bonus [:credit 1]))}}}

   "Shipment from Kaguya"
   {:choices {:max 2 :req can-be-advanced?}
    :msg (msg "place 1 advancement token on " (count targets) " cards")
    :effect (req (doseq [t targets]
                   (add-prop state :corp t :advance-counter 1 {:placed true}))
                 (effect-completed state side eid))}

   "Shipment from MirrorMorph"
   (let [shelper (fn sh [n] {:prompt "Select a card to install with Shipment from MirrorMorph"
                             :async true
                             :choices {:req #(and (= (:side %) "Corp")
                                                  (not (is-type? % "Operation"))
                                                  (in-hand? %))}
                             :effect (req (wait-for
                                            (corp-install state side target nil nil)
                                            (if (< n 3)
                                              (continue-ability state side (sh (inc n)) card nil)
                                              (effect-completed state side eid))))})]
     {:async true
      :effect (effect (continue-ability (shelper 1) card nil))})

   "Shipment from SanSan"
   {:choices ["0", "1", "2"]
    :prompt "How many advancement tokens?"
    :async true
    :effect (req (let [c (str->int target)]
                   (continue-ability
                     state side
                     {:choices {:req can-be-advanced?}
                      :msg (msg "place " c " advancement tokens on " (card-str state target))
                      :effect (effect (add-prop :corp target :advance-counter c {:placed true}))}
                     card nil)))}

   "Shipment from Tennin"
   {:async true
    :req (req (not-last-turn? state :runner :successful-run))
    :choices {:req #(and (installed? %) (= (:side %) "Corp"))}
    :msg (msg "place 2 advancement tokens on " (card-str state target))
    :effect (effect (add-prop target :advance-counter 2 {:placed true}))}

   "Shoot the Moon"
   {:choices {:req #(and (ice? %) (not (rezzed? %)))
              :max (req (min (count-tags state)
                             (reduce (fn [c server]
                                       (+ c (count (filter #(not (:rezzed %)) (:ices server)))))
                                     0 (flatten (seq (:servers corp))))))}
    :req (req tagged)
    :effect (req (doseq [t targets] (rez state side t {:ignore-cost :all-costs}))
                 (effect-completed state side eid))}

   "Snatch and Grab"
   {:trace {:base 3
            :successful
            {:msg "trash a connection"
             :choices {:req #(has-subtype? % "Connection")}
             :async true
             :effect (req (let [c target]
                            (show-wait-prompt state :corp "Runner to decide if they will take 1 tag")
                            (continue-ability
                              state side
                              {:player :runner
                               :prompt (msg "Take 1 tag to prevent " (:title c) " from being trashed?")
                               :choices ["Yes" "No"]
                               :async true
                               :effect (effect (clear-wait-prompt :corp)
                                               (continue-ability
                                                 (if (= target "Yes")
                                                   {:msg (msg "take 1 tag to prevent " (:title c)
                                                              " from being trashed")
                                                    :async true
                                                    :effect (effect (gain-tags :runner eid 1 {:unpreventable true}))}
                                                   {:async true
                                                    :effect (effect (trash :corp eid c nil))
                                                    :msg (msg "trash " (:title c))})
                                                 card nil))}
                              card nil)))}}}

   "Special Report"
   {:prompt "Select any number of cards in HQ to shuffle into R&D"
    :choices {:max (req (count (:hand corp)))
              :req #(and (= (:side %) "Corp")
                         (in-hand? %))}
    :msg (msg "shuffle " (count targets) " cards in HQ into R&D and draw " (count targets) " cards")
    :async true
    :effect (req (doseq [c targets]
                   (move state side c :deck))
                 (shuffle! state side :deck)
                 (draw state side eid (count targets) nil))}

   "Standard Procedure"
   {:req (req (last-turn? state :runner :successful-run))
    :prompt "Choose a card type"
    :choices ["Event" "Hardware" "Program" "Resource"]
    :effect (req (let [n (* 2 (count (filter #(is-type? % target) (:hand runner))))]
                   (gain-credits state :corp n)
                   (reveal state side (:hand runner))
                   (system-msg state side (str "uses Standard Procedure to name " target ", reveal "
                                               (join ", " (map :title (:hand runner)))
                                               " in the Runner's Grip, and gain " n " [Credits]"))))}

   "Stock Buy-Back"
   {:msg (msg "gain " (* 3 (count (:scored runner))) " [Credits]")
    :effect (effect (gain-credits (* 3 (count (:scored runner)))))}

   "Sub Boost"
   (let [new-sub {:label "[Sub Boost]: End the run"}]
     {:sub-effect {:label "End the run"
                   :msg "end the run"
                   :effect (effect (end-run eid card))}
      :choices {:req #(and (ice? %) (rezzed? %))}
      :msg (msg "make " (card-str state target) " gain Barrier and \"[Subroutine] End the run\"")
      :effect (req (update! state side (assoc target :subtype (combine-subtypes true (:subtype target) "Barrier")))
                   (add-extra-sub state :corp (:cid card) (get-card state target) -1 new-sub)
                   (update-ice-strength state side target)
                   (host state side (get-card state target) (assoc card :zone [:discard] :seen true :condition true)))
      :leave-play (req (remove-extra-subs state :corp (:cid card) (:host card)))
      :events {:rez {:req (req (= (:cid target) (:cid (:host card))))
                     :effect (req (add-extra-sub state :corp (:cid card) (get-card state target) -1 new-sub))}}})

   "Subcontract"
   (letfn [(sc [i sccard]
             {:prompt "Select an operation in HQ to play"
              :choices {:req #(and (= (:side %) "Corp")
                                   (is-type? % "Operation")
                                   (in-hand? %))}
              :async true
              :msg (msg "play " (:title target))
              :effect (req (wait-for (play-instant state side target)
                                     (if (and (not (get-in @state [:corp :register :terminal])) (< i 2))
                                       (continue-ability state side (sc (inc i) sccard) sccard nil)
                                       (effect-completed state side eid))))})]
     {:req (req tagged)
      :async true
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
      :effect (effect (gain-credits 1)
                      (resolve-ability {:once :per-turn
                                        :once-key :subliminal-messaging
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
    :effect (effect (gain-credits 7))}

   "Sunset"
   (letfn [(sun [serv]
             {:prompt "Select two pieces of ICE to swap positions"
              :choices {:req #(and (= serv (rest (butlast (:zone %)))) (ice? %))
                        :max 2}
              :async true
              :effect (req (if (= (count targets) 2)
                             (do (swap-ice state side (first targets) (second targets))
                                 (continue-ability state side (sun serv) card nil))
                             (do (system-msg state side "has finished rearranging ICE")
                                 (effect-completed state side eid))))})]
     {:prompt "Choose a server"
      :choices (req servers)
      :async true
      :msg (msg "rearrange ICE protecting " target)
      :effect (req (let [serv (next (server->zone state target))]
                     (continue-ability state side (sun serv) card nil)))})

   "Surveillance Sweep"
   {:events {:pre-init-trace {:req (req run)
                              :effect (req (swap! state assoc-in [:trace :player] :runner))}}}

   "Sweeps Week"
   {:effect (effect (gain-credits (count (:hand runner))))
    :msg (msg "gain " (count (:hand runner)) " [Credits]")}

   "Targeted Marketing"
   (let [gaincr {:req (req (= (:title target) (:marketing-target card)))
                 :effect (effect (gain-credits :corp 10))
                 :msg (msg "gain 10 [Credits] from " (:marketing-target card))}]
     {:prompt "Name a Runner card"
      :choices {:card-title (req (and (card-is? target :side "Runner")
                                      (not (card-is? target :type "Identity"))))}
      :effect (effect (update! (assoc card :marketing-target target))
                      (system-msg (str "uses Targeted Marketing to name " target)))
      :events {:runner-install gaincr
               :play-event gaincr}})

   "The All-Seeing I"
   (let [trash-all-resources
         {:msg "trash all resources"
          :effect (effect (trash-cards :corp eid (filter resource? (all-active-installed state :runner)) nil))}]
     {:req (req tagged)
      :async true
      :effect (effect
                (continue-ability
                  (if (pos? (:bad-publicity corp 0))
                    {:optional
                     {:player :runner
                      :prompt "Remove 1 bad publicity to prevent all resources from being trashed?"
                      :yes-ability {:msg "remove 1 bad publicity, preventing all resources from being trashed"
                                    :effect (effect (lose :bad-publicity 1))}
                      :no-ability trash-all-resources}}
                    trash-all-resources)
                  card nil))})

   "Threat Assessment"
   {:req (req (last-turn? state :runner :trashed-card))
    :prompt "Select an installed Runner card"
    :choices {:req #(and (= (:side %) "Runner") (installed? %))}
    :async true
    :effect (req (let [chosen target]
                   (show-wait-prompt state side "Runner to resolve Threat Assessment")
                   (continue-ability
                     state :runner
                     {:prompt (str "Add " (:title chosen) " to the top of the Stack or take 2 tags?")
                      :choices [(str "Move " (:title chosen))
                                "2 tags"]
                      :async true
                      :effect (req (clear-wait-prompt state :corp)
                                   (move state :corp (last (:discard corp)) :rfg)
                                   (if (.startsWith target "Move")
                                     (do (system-msg state side (str "chooses to move " (:title chosen) " to the Stack"))
                                         (move state :runner chosen :deck {:front true})
                                         (effect-completed state side eid))
                                     (do (system-msg state side "chooses to take 2 tags")
                                         (gain-tags state :runner eid 2))))}
                     card nil)))}

   "Threat Level Alpha"
   {:trace {:base 1
            :successful
            {:label "Give the Runner X tags"
             :async true
             :effect (req (let [tags (max 1 (count-tags state))]
                            (do (gain-tags state :corp eid tags)
                                (system-msg
                                  state side
                                  (str "uses Threat Level Alpha to give the Runner " (quantify tags "tag"))))))}}}

   "Too Big to Fail"
   {:req (req (< (:credit corp) 10))
    :msg "gain 7 [Credits] and take 1 bad publicity"
    :effect (effect (gain-credits 7)
                    (gain-bad-publicity :corp 1))}

   "Traffic Accident"
   {:req (req (>= (count-tags state) 2))
    :msg "do 2 meat damage"
    :async true
    :effect (effect (damage eid :meat 2 {:card card}))}

   "Transparency Initiative"
   {:choices {:req #(and (is-type? % "Agenda")
                         (installed? %)
                         (not (faceup? %)))}
    :effect (effect (update! (assoc target
                                    :seen true
                                    :rezzed true
                                    :subtype (combine-subtypes false (:subtype target) "Public")))
                    (host (get-card state target) (assoc card
                                                         :zone [:discard]
                                                         :seen true))
                    (register-events
                      {:advance {:req (req (= (:hosted card) (:hosted target)))
                                 :effect (effect (gain-credits 1)
                                                 (system-msg
                                                   (str "uses Transparency Initiative to gain 1 [Credit]")))}}
                      target))}

   "Trick of Light"
   {:choices {:req #(pos? (get-counters % :advancement))}
    :async true
    :effect (req (let [fr target tol card]
                   (continue-ability
                     state side
                     {:prompt "Move how many advancement tokens?"
                      :choices (take (inc (get-counters fr :advancement)) ["0" "1" "2"])
                      :async true
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
            :successful {:async true
                         :effect (req (let [exceed (- target (second targets))]
                                        (continue-ability
                                          state side
                                          {:async true
                                           :prompt (str "Select a program with an install cost of no more than "
                                                        exceed "[Credits]")
                                           :choices {:req #(and (is-type? % "Program")
                                                                (installed? %)
                                                                (>= exceed (:cost %)))}
                                           :msg (msg "trash " (card-str state target))
                                           :effect (effect (trash eid target nil))}
                                          card nil)))}}}

   "Ultraviolet Clearance"
   {:async true
    :effect (req (gain-credits state side 10)
                 (wait-for (draw state side 4 nil)
                           (continue-ability state side
                                             {:prompt "Choose a card in HQ to install"
                                              :choices {:req #(and (in-hand? %) (= (:side %) "Corp") (not (is-type? % "Operation")))}
                                              :msg "gain 10 [Credits], draw 4 cards, and install 1 card from HQ"
                                              :cancel-effect (req (effect-completed state side eid))
                                              :effect (effect (corp-install target nil))}
                                             card nil)))}

   "Under the Bus"
   {:req (req (and (last-turn? state :runner :accessed-cards)
                   (not-empty (filter
                                #(and (is-type? % "Resource")
                                      (has-subtype? % "Connection"))
                                (all-active-installed state :runner)))))
    :prompt "Choose a connection to trash"
    :choices {:req #(and (= (:side %) "Runner")
                         (is-type? % "Resource")
                         (has-subtype? % "Connection")
                         (installed? %))}
    :msg (msg "trash " (:title target) " and take 1 bad publicity")
    :async true
    :effect (req (wait-for (trash state side target nil)
                           (gain-bad-publicity state :corp eid 1)))}

   "Violet Level Clearance"
   {:msg "gain 8 [Credits] and draw 4 cards"
    :async true
    :effect (effect (gain-credits 8)
                    (draw eid 4 nil))}

   "Voter Intimidation"
   {:req (req (seq (:scored runner)))
    :psi {:not-equal {:player :corp
                      :prompt "Select a resource to trash"
                      :choices {:req #(and (installed? %)
                                           (is-type? % "Resource"))}
                      :msg (msg "trash " (:title target))
                      :effect (effect (trash target)
                                      (effect-completed eid))}}}

   "Wake Up Call"
   {:req (req (last-turn? state :runner :trashed-card))
    :prompt "Select a piece of hardware or non-virtual resource"
    :choices {:req #(or (hardware? %)
                        (and (resource? %) (not (has-subtype? % "Virtual"))))}
    :async true
    :effect (req (let [chosen target
                       wake card]
                   (show-wait-prompt state side "Runner to resolve Wake Up Call")
                   (continue-ability state :runner
                                     {:prompt (str "Trash " (:title chosen) " or suffer 4 meat damage?")
                                      :choices [(str "Trash " (:title chosen))
                                                "4 meat damage"]
                                      :async true
                                      :effect (req (clear-wait-prompt state :corp)
                                                   (move state :corp (last (:discard corp)) :rfg)
                                                   (if (.startsWith target "Trash")
                                                     (do (system-msg state side (str "chooses to trash " (:title chosen)))
                                                         (trash state side eid chosen nil))
                                                     (do (system-msg state side "chooses to suffer meat damage")
                                                         (damage state side eid :meat 4 {:card wake
                                                                                         :unboostable true}))))}
                                     card nil)))}

   "Wetwork Refit"
   (let [new-sub {:label "[Wetwork Refit] Do 1 brain damage"}]
     {:choices {:req #(and (ice? %)
                           (has-subtype? % "Bioroid")
                           (rezzed? %))}
      :msg (msg "give " (card-str state target) " \"[Subroutine] Do 1 brain damage\" before all its other subroutines")
      :sub-effect (do-brain-damage 1)
      :effect (req (add-extra-sub state :corp (:cid card) target 0 new-sub)
                   (host state side (get-card state target) (assoc card :zone [:discard] :seen true :condition true)))
      :leave-play (req (remove-extra-subs state :corp (:cid card) (:host card)))
      :events {:rez {:req (req (= (:cid target) (:cid (:host card))))
                     :effect (req (add-extra-sub state :corp (:cid card) (get-card state target) 0 new-sub))}}})

   "Witness Tampering"
   {:msg "remove 2 bad publicity"
    :effect (effect (lose :bad-publicity 2))}})
