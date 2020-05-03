(ns game.cards.operations
  (:require [game.core :refer :all]
            [game.core.card :refer :all]
            [game.core.card-defs :refer [define-card]]
            [game.core.eid :refer [make-eid make-result effect-completed]]
            [game.core.card-defs :refer [card-def]]
            [game.core.prompts :refer [show-wait-prompt clear-wait-prompt]]
            [game.core.toasts :refer [toast]]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability when-let*]]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [clojure.stacktrace :refer [print-stack-trace]]
            [jinteki.utils :refer :all]))

;; Card definitions

(define-card "24/7 News Cycle"
  {:req (req (pos? (count (:scored corp))))
   :async true
   :additional-cost [:forfeit]
   :effect (req (continue-ability
                  state side
                  {:prompt "Select an agenda in your score area to trigger its \"when scored\" ability"
                   :choices {:card #(and (agenda? %)
                                         (when-scored? %)
                                         (is-scored? state :corp %))}
                   :msg (msg "trigger the \"when scored\" ability of " (:title target))
                   :async true
                   :effect (effect (continue-ability (card-def target) target nil)
                                   (unregister-events target {:events [{:event :corp-turn-ends}
                                                                       {:event :runner-turn-ends}]}))}
                  card nil))})

(define-card "Accelerated Diagnostics"
  (letfn [(ad [st si e c cards]
            (when-let [cards (filterv #(and (operation? %)
                                            (can-pay? st si (assoc e :source c :source-type :play) c nil [:credit (play-cost st si %)]))
                                      cards)]
              {:async true
               :prompt "Select an operation to play"
               :choices (cancellable cards)
               :msg (msg "play " (:title target))
               :effect (req (wait-for (play-instant state side target {:no-additional-cost true})
                                      (let [cards (filterv #(not (same-card? % target)) cards)]
                                        (continue-ability state side (ad state side eid card cards) card nil))))}))]
    {:async true
     :effect
     (effect
       (continue-ability
         (let [top (take 3 (:deck corp))]
           {:prompt (str "The top 3 cards of R&D are " (join ", " (map :title top)) ".")
            :choices ["OK"]
            :effect (effect (continue-ability (ad state side eid card top) card nil))})
         card nil))}))

(define-card "Ad Blitz"
  (letfn [(ab [n total]
            (when (< n total)
              {:async true
               :show-discard true
               :prompt "Select an Advertisement to install and rez"
               :choices {:card #(and (corp? %)
                                     (has-subtype? % "Advertisement")
                                     (or (in-hand? %)
                                         (in-discard? %)))}
               :effect (req (wait-for (corp-install state side target nil {:install-state :rezzed})
                                      (continue-ability state side (ab (inc n) total) card nil)))}))]
    {:async true
     :req (req (some #(has-subtype? % "Advertisement")
                     (concat (:discard corp) (:hand corp))))
     :prompt "How many Advertisements?"
     :choices :credit
     :msg (msg "install and rez " target " Advertisements")
     :effect (effect (continue-ability (ab 0 target) card nil))}))

(define-card "Aggressive Negotiation"
  {:req (req (:scored-agenda corp-reg))
   :prompt "Choose a card"
   :choices (req (cancellable (:deck corp) :sorted))
   :effect (effect (move target :hand)
                   (shuffle! :deck))
   :msg "search R&D for a card and add it to HQ"})

(define-card "An Offer You Can't Refuse"
  {:async true
   :prompt "Choose a server"
   :choices ["Archives" "R&D" "HQ"]
   :effect (req (let [serv target]
                  (show-wait-prompt state :corp (str "Runner to decide on running " target))
                  (continue-ability
                    state side
                    {:optional
                     {:prompt (str "Make a run on " serv "?")
                      :player :runner
                      :yes-ability {:msg (str "let the Runner make a run on " serv)
                                    :async true
                                    :effect (effect (clear-wait-prompt :corp)
                                                    (make-run eid serv nil card)
                                                    (prevent-jack-out))}
                      :no-ability {:async true
                                   :msg "add it to their score area as an agenda worth 1 agenda point"
                                   :effect (req (clear-wait-prompt state :corp)
                                                (as-agenda state :corp eid card 1))}}}
                    card nil)))})

(define-card "Anonymous Tip"
  {:msg "draw 3 cards"
   :async true
   :effect (effect (draw eid 3 nil))})

(define-card "Archived Memories"
  {:prompt "Select a card from Archives to add to HQ"
   :show-discard true
   :choices {:card #(and (corp? %)
                         (in-discard? %))}
   :msg (msg "add " (if (faceup? target) (:title target) "an unseen card") " to HQ")
   :effect (effect (move target :hand))})

(define-card "Argus Crackdown"
  {:trash-after-resolving false
   :events [{:event :successful-run
             :req (req (let [server (first (get-in @state [:run :server]))]
                         (not-empty (get-in @state [:corp :servers server :ices]))))
             :msg (msg "deal 2 meat damage")
             :async true
             :effect (effect (damage eid :meat 2 {:card card}))}
            {:event :corp-turn-begins
             :async true
             :effect (effect (trash eid card nil))}]})

(define-card "Ark Lockdown"
  {:async true
   :req (req (not-empty (:discard runner)))
   :prompt "Name a card to remove all copies in the Heap from the game"
   :choices (req (cancellable (:discard runner) :sorted))
   :msg (msg "remove all copies of " (:title target) " in the Heap from the game")
   :effect (req (doseq [c (filter #(= (:title target) (:title %)) (:discard runner))]
                  (move state :runner c :rfg))
                (effect-completed state side eid))})

(define-card "Attitude Adjustment"
  {:async true
   :effect (req (wait-for (draw state side 2 nil)
                          (continue-ability
                            state side
                            {:prompt "Choose up to 2 agendas in HQ or Archives"
                             :choices {:max 2
                                       :card #(and (corp? %)
                                                   (agenda? %)
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
                            card nil)))})

(define-card "Audacity"
  (letfn [(audacity [n]
            (if (< n 2)
              {:prompt "Choose a card on which to place an advancement"
               :async true
               :choices {:card can-be-advanced?
                         :all true}
               :msg (msg "place an advancement token on " (card-str state target))
               :effect (req (add-prop state :corp target :advance-counter 1 {:placed true})
                            (continue-ability state side (audacity (inc n)) card nil))}))]
    {:async true
     :req (req (and (<= 3 (count (:hand corp)))
                    (some can-be-advanced? (all-installed state :corp))))
     :effect (req (system-msg state side "trashes all cards in HQ due to Audacity")
                  (wait-for (trash-cards state side (:hand corp) {:unpreventable true})
                            (continue-ability state side (audacity 0) card nil)))}))

(define-card "Back Channels"
  {:async true
   :prompt "Select an installed card in a server to trash"
   :choices {:card #(and (= (last (:zone %)) :content)
                         (is-remote? (second (:zone %))))}
   :effect (effect (gain-credits (* 3 (get-counters target :advancement)))
                   (trash eid target nil))
   :msg (msg "trash " (card-str state target) " and gain "
             (* 3 (get-counters target :advancement)) " [Credits]")})

(define-card "Bad Times"
  {:implementation "Any required program trashing is manual"
   :req (req tagged)
   :msg "force the Runner to lose 2[mu] until the end of the turn"
   :effect (req (lose state :runner :memory 2)
                (when (neg? (available-mu state))
                  ;; Give runner a toast as well
                  (toast-check-mu state)
                  (system-msg state :runner "must trash programs to free up [mu]"))
                (register-events
                  state side card
                  [{:event :corp-turn-ends
                    :duration :end-of-turn
                    :effect (effect (gain :runner :memory 2)
                                    (system-msg :runner "regains 2[mu]"))}]))})

(define-card "Beanstalk Royalties"
  {:msg "gain 3 [Credits]"
   :effect (effect (gain-credits 3))})

(define-card "Best Defense"
  {:async true
   :req (req (not-empty (all-installed state :runner)))
   :prompt (msg "Choose a Runner card with an install cost of " (count-tags state) " or less to trash")
   :choices {:req (req (and (runner? target)
                            (installed? target)
                            (not (facedown? target))
                            (<= (:cost target) (count-tags state))))}
   :msg (msg "trash " (:title target))
   :effect (effect (trash eid target nil))})

(define-card "Biased Reporting"
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
                                   :card #(and (installed? %)
                                               (is-type? % t))}
                         :async true
                         :effect (req (wait-for
                                        (trash-cards state :runner targets {:unpreventable true})
                                        (gain-credits state :runner (count targets))
                                        (system-msg state :runner
                                                    (str "trashes " (join ", " (map :title targets))
                                                         " and gains " (count targets) " [Credits]"))
                                        (effect-completed state side eid)))}
                        card nil)
                      (clear-wait-prompt state :corp)
                      (let [n (* 2 (num-installed state t))]
                        (when (pos? n)
                          (gain-credits state :corp n)
                          (system-msg state :corp (str "uses Biased Reporting to gain " n " [Credits]")))
                        (effect-completed state side eid)))))}))

(define-card "Big Brother"
  {:req (req tagged)
   :msg "give the Runner 2 tags"
   :async true
   :effect (effect (gain-tags :corp eid 2))})

(define-card "Bioroid Efficiency Research"
  {:async true
   :req (req (some #(and (ice? %)
                         (has-subtype? % "Bioroid")
                         (not (rezzed? %)))
                   (all-installed state :corp)))
   :choices {:card #(and (ice? %)
                         (has-subtype? % "Bioroid")
                         (installed? %)
                         (not (rezzed? %)))}
   :msg (msg "rez " (card-str state target {:visible true}) " at no cost")
   :effect (req (wait-for (rez state side target {:ignore-cost :all-costs})
                          (host state side (get-card state target) (assoc card :seen true :condition true))
                          (effect-completed state side eid)))
   :events [{:event :encounter-ice-ends
             :condition :hosted
             :async true
             :req (req (and (same-card? target (:host card))
                            (empty? (remove :broken (:subroutines target)))))
             :effect (effect (system-msg :corp
                                         (str "derezzes " (:title target)
                                              " and trashes Bioroid Efficiency Research"))
                             (derez :corp target)
                             (trash :corp eid card {:unpreventable true}))}]})

(define-card "Biotic Labor"
  {:msg "gain [Click][Click]"
   :effect (effect (gain :click 2))})

(define-card "Blue Level Clearance"
  {:msg "gain 5 [Credits] and draw 2 cards"
   :async true
   :effect (effect (gain-credits 5)
                   (draw eid 2 nil))})

(define-card "BOOM!"
  {:req (req (<= 2 (count-tags state)))
   :async true
   :msg "do 7 meat damage"
   :effect (effect (damage eid :meat 7 {:card card}))})

(define-card "Building Blocks"
  {:req (req (pos? (count (filter #(has-subtype? % "Barrier") (:hand corp)))))
   :prompt "Select a Barrier to install and rez"
   :choices {:card #(and (corp? %)
                         (has-subtype? % "Barrier")
                         (in-hand? %))}
   :async true
   :msg (msg "reveal " (:title target))
   :effect (effect (reveal target)
                   (corp-install eid target nil {:ignore-all-cost true
                                                 :install-state :rezzed-no-cost}))})

(define-card "Casting Call"
  {:choices {:card #(and (agenda? %)
                         (in-hand? %))}
   :async true
   :effect (req (wait-for (corp-install state side target nil {:install-state :face-up})
                          (let [agenda async-result]
                            (host state side agenda (assoc card
                                                           :seen true
                                                           :condition true
                                                           :installed true))
                            (system-msg state side (str "hosts Casting Call on " (:title agenda)))
                            (effect-completed state side eid))))
   :events [{:event :access
             :condition :hosted
             :async true
             :req (req (same-card? target (:host card)))
             :msg "give the Runner 2 tags"
             :effect (effect (gain-tags :runner eid 2))}]})

(define-card "Celebrity Gift"
  {:choices {:max 5
             :card #(and (corp? %)
                         (in-hand? %))}
   :msg (msg "reveal " (join ", " (map :title (sort-by :title targets))) " and gain " (* 2 (count targets)) " [Credits]")
   :effect (effect (reveal targets) (gain-credits (* 2 (count targets))))})

(define-card "Cerebral Cast"
  {:req (req (last-turn? state :runner :successful-run))
   :psi {:not-equal {:player :runner
                     :prompt "Take 1 tag or 1 brain damage?"
                     :choices ["1 tag" "1 brain damage"]
                     :msg (msg "give the Runner " target)
                     :effect (req (if (= target "1 tag")
                                    (gain-tags state :runner eid 1)
                                    (damage state side eid :brain 1 {:card card})))}}})

(define-card "Cerebral Static"
  {:msg "disable the Runner's identity"
   :effect (effect (disable-identity :runner))
   :leave-play (effect (enable-identity :runner))})

(define-card "\"Clones are not People\""
  {:events [{:event :agenda-scored
             :msg "add it to their score area as an agenda worth 1 agenda point"
             :async true
             :effect (req (as-agenda state :corp eid card 1))}]})

(define-card "Closed Accounts"
  {:req (req tagged)
   :msg (msg "force the Runner to lose all " (:credit runner) " [Credits]")
   :effect (effect (lose-credits :runner :all))})

(define-card "Commercialization"
  {:msg (msg "gain " (get-counters target :advancement) " [Credits]")
   :choices {:card ice?}
   :effect (effect (gain-credits (get-counters target :advancement)))})

(define-card "Complete Image"
  (letfn [(name-a-card []
            {:async true
             :prompt "Name a Runner card"
             :choices {:card-title (req (and (runner? target)
                                             (not (identity? target))))}
             :msg (msg "name " target)
             :effect (effect (continue-ability (damage-ability) card targets))})
          (damage-ability []
            {:async true
             :msg "do 1 net damage"
             :effect (req (wait-for (damage state side :net 1 {:card card})
                                    (let [should-continue (not (:winner @state))
                                          cards (some #(when (same-card? (second %) card) (last %))
                                                      (turn-events state :corp :damage))
                                          dmg (some #(when (= (:title %) target) %) cards)]
                                      (continue-ability state side (when (and should-continue dmg)
                                                                     (name-a-card))
                                                        card nil))))})]
    {:implementation "Doesn't work with Chronos Protocol: Selective Mind-mapping"
     :async true
     :req (req (and (last-turn? state :runner :successful-run)
                    (<= 3 (:agenda-point runner))))
     :effect (effect (continue-ability (name-a-card) card nil))}))

(define-card "Consulting Visit"
  {:async true
   :prompt  "Choose an Operation from R&D to play"
   :choices (req (cancellable
                   (filter #(and (operation? %)
                                 (<= (:cost %) (:credit corp)))
                           (:deck corp))
                   :sorted))
   :msg (msg "search R&D for " (:title target) " and play it")
   :effect (effect (shuffle! :deck)
                   (system-msg "shuffles their deck")
                   (play-instant eid target nil))})

(define-card "Corporate Shuffle"
  {:msg "shuffle all cards in HQ into R&D and draw 5 cards"
   :async true
   :effect (effect (shuffle-into-deck :hand)
                   (draw eid 5 nil))})

(define-card "Cyberdex Trial"
  {:msg "purge virus counters"
   :effect (effect (purge))})

(define-card "Death and Taxes"
  {:implementation "Credit gain mandatory to save on wait-prompts, adjust credits manually if credit not wanted."
   :events [{:event :runner-install
             :msg "gain 1 [Credits]"
             :effect (effect (gain-credits :corp 1))}
            {:event :runner-trash
             :req (req (some installed? targets))
             :msg "gain 1 [Credits]"
             :effect (effect (gain-credits :corp 1))}]})

(define-card "Dedication Ceremony"
  {:prompt "Select a faceup card"
   :choices {:card #(or (and (corp? %)
                             (rezzed? %))
                        (and (runner? %)
                             (or (installed? %)
                                 (:host %))
                             (not (facedown? %))))}
   :msg (msg "place 3 advancement tokens on " (card-str state target))
   :effect (effect (add-prop target :advance-counter 3 {:placed true})
                   (register-turn-flag!
                     target :can-score
                     (fn [state side card]
                       (if (same-card? card target)
                         ((constantly false) (toast state :corp "Cannot score due to Dedication Ceremony." "warning"))
                         true)))
                   (effect-completed eid))})

(define-card "Defective Brainchips"
  {:events [{:event :pre-damage
             :req (req (= target :brain))
             :msg "do 1 additional brain damage"
             :once :per-turn
             :effect (effect (damage-bonus :brain 1))}]})

(define-card "Digital Rights Management"
  {:req (req (and (< 1 (:turn @state))
                  (not (in-coll? (get-in @state [:runner :register-last-turn :successful-run]) :hq))))
   :prompt "Choose an Agenda"
   :implementation "Does not prevent scoring agendas installed later in the turn"
   ; ToDo: When floating triggers are implemented, this should be an effect that listens to :corp-install as Clot does
   :choices (req (conj (vec (filter agenda? (:deck corp))) "None"))
   :msg (msg (if (not= "None" target)
               (str "add " (:title target) " to HQ and shuffle R&D")
               "shuffle R&D"))
   :effect (let [end-effect (req (system-msg state side "can not score agendas for the remainder of the turn")
                                 (swap! state assoc-in [:corp :register :cannot-score]
                                        (filter agenda? (all-installed state :corp)))
                                 (effect-completed state side eid))]
             (req (when (not= "None" target)
                    (reveal state side target)
                    (move state side target :hand))
                  (shuffle! state side :deck)
                  (continue-ability state side
                                    {:prompt "You may install a card in HQ"
                                     :choices {:card #(and (in-hand? %)
                                                           (corp? %)
                                                           (not (operation? %)))}
                                     :effect (req (wait-for (resolve-ability
                                                              state side
                                                              (let [card-to-install target]
                                                                {:prompt (str "Choose a location to install " (:title target))
                                                                 :choices (remove #{"HQ" "R&D" "Archives"} (corp-install-list state target))
                                                                 :async true
                                                                 :effect (effect (corp-install eid card-to-install target nil))})
                                                              target nil)
                                                            (end-effect state side eid card targets)))
                                     :cancel-effect (effect (system-msg "does not use Digital Rights Management to install a card")
                                                            (end-effect eid card targets))}
                                    card nil)))})

(define-card "Distract the Masses"
  (let [shuffle-two {:async true
                     :effect (effect (shuffle-into-rd-effect eid card 2 false))}
        trash-from-hq {:async true
                       :prompt "Select up to 2 cards in HQ to trash"
                       :choices {:max 2
                                 :card #(and (corp? %)
                                             (in-hand? %))}
                       :msg (msg "trash " (quantify (count targets) "card") " from HQ")
                       :effect (req (wait-for
                                      (trash-cards state side targets nil)
                                      (continue-ability state side shuffle-two card nil)))
                       :cancel-effect (effect (continue-ability shuffle-two card nil))}]
    {:async true
     :rfg-instead-of-trashing true
     :msg "give The Runner 2 [Credits]"
     :effect (effect (gain-credits :runner 2)
                     (continue-ability trash-from-hq card nil))}))

(define-card "Diversified Portfolio"
  (letfn [(number-of-non-empty-remotes [state]
            (count (filter #(not (empty? %))
                           (map #(:content (second %))
                                (get-remotes state)))))]
    {:msg (msg "gain " (number-of-non-empty-remotes state)
            " [Credits]")
     :effect (effect (gain-credits (number-of-non-empty-remotes state)))}))

(define-card "Divert Power"
  {:async true
   :prompt "Select any number of cards to derez"
   :choices {:card #(and (installed? %)
                         (rezzed? %))
             :max (req (count (filter rezzed? (all-installed state :corp))))}
   :effect (req (doseq [c targets]
                  (derez state side c))
                (let [discount (* -3 (count targets))]
                  (continue-ability
                    state side
                    {:async true
                     :prompt "Select a card to rez"
                     :choices {:card #(and (installed? %)
                                           (corp? %)
                                           (not (rezzed? %))
                                           (not (agenda? %)))}
                     :effect (effect (rez eid target {:cost-bonus discount}))}
                    card nil)))})

(define-card "Door to Door"
  {:events [{:event :runner-turn-begins
             :trace {:base 1
                     :label "Do 1 meat damage if Runner is tagged, or give the Runner 1 tag"
                     :successful {:msg (msg (if tagged
                                              "do 1 meat damage"
                                              "give the Runner 1 tag"))
                                  :async true
                                  :effect (req (if tagged
                                                 (damage state side eid :meat 1 {:card card})
                                                 (gain-tags state :corp eid 1)))}}}]})

(define-card "Eavesdrop"
  {:choices {:card #(and (ice? %)
                         (installed? %))}
   :msg (msg "give " (card-str state target {:visible false}) " additional text")
   :effect (effect (host target (assoc card :seen true :condition true)))
   :events [{:event :encounter-ice
             :condition :hosted
             :req (req (same-card? target (:host card)))
             :trace {:base 3
                     :successful {:msg "give the Runner 1 tag"
                                  :async true
                                  :effect (effect (gain-tags :runner eid 1))}}}]})

(define-card "Economic Warfare"
  {:req (req (and (last-turn? state :runner :successful-run)
                  (can-pay? state :runner (assoc eid :source card :source-type :ability) card nil :credit 4)))
   :msg "make the runner lose 4 [Credits]"
   :effect (effect (lose-credits :runner 4))})

(define-card "Election Day"
  {:req (req (->> (get-in @state [:corp :hand])
                  (filter #(not (same-card? % card)))
                  count
                  pos?))
   :async true
   :msg (msg "trash all cards in HQ and draw 5 cards")
   :effect (req (wait-for (trash-cards state side (get-in @state [:corp :hand]))
                          (draw state side eid 5 nil)))})

(define-card "Enforced Curfew"
  {:msg "reduce the Runner's maximum hand size by 1"
   :effect (effect (lose :runner :hand-size 1))
   :leave-play (effect (gain :runner :hand-size 1))})

(define-card "Enforcing Loyalty"
  {:trace {:base 3
           :label "Trash a card not matching the faction of the Runner's identity"
           :successful
           {:async true
            :prompt "Select an installed card not matching the faction of the Runner's identity"
            :choices {:req (req (and (installed? target)
                                     (runner? target)
                                     (not= (:faction (:identity runner)) (:faction target))))}
            :msg (msg "trash " (:title target))
            :effect (effect (trash eid target nil))}}})

(define-card "Enhanced Login Protocol"
  {:msg "uses Enhanced Login Protocol to add an additional cost of [Click] to make the first run not through a card ability this turn"
   :constant-effects [{:type :run-additional-cost
                       :req (req (and (no-event? state side :run #(:click-run (nth % 2)))
                                      (:click-run (second targets))))
                       :value [:click 1]}]})

(define-card "Exchange of Information"
  {:req (req (and tagged
                  (seq (:scored runner))
                  (seq (:scored corp))))
   :async true
   :effect (req
             (continue-ability
               state side
               {:prompt "Select a stolen agenda in the Runner's score area to swap"
                :choices {:card #(in-runner-scored? state side %)}
                :async true
                :effect (req
                          (let [stolen target]
                            (continue-ability
                              state side
                              {:prompt (msg "Select a scored agenda to swap for " (:title stolen))
                               :choices {:card #(in-corp-scored? state side %)}
                               :effect (req (let [scored target]
                                              (swap-agendas state side scored stolen)
                                              (system-msg state side (str "uses Exchange of Information to swap "
                                                                          (:title scored) " for " (:title stolen)))))}
                              card nil)))}
               card nil))})

(define-card "Fast Break"
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
                               :choices {:card #(and (corp? %)
                                                     (not (operation? %))
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
                    (continue-ability state side select-server card nil))))})

(define-card "Fast Track"
  {:prompt "Choose an Agenda"
   :choices (req (cancellable (filter agenda? (:deck corp)) :sorted))
   :effect (effect (system-msg (str "adds " (:title target) " to HQ and shuffle R&D"))
                   (reveal target)
                   (shuffle! :deck)
                   (move target :hand))})

(define-card "Financial Collapse"
  {:async true
   :req (req (and (>= (:credit runner) 6)
                  (seq (filter resource? (all-active-installed state :runner)))))
   :effect (req (let [rcount (count (filter resource? (all-active-installed state :runner)))]
                  (if (pos? rcount)
                    (do (show-wait-prompt state :corp "Runner to trash a resource to prevent Financial Collapse")
                        (continue-ability
                          state :runner
                          {:optional
                           {:prompt "Trash a resource to prevent Financial Collapse?"
                            :yes-ability
                            {:prompt "Select a resource to trash"
                             :choices {:card #(and (resource? %)
                                                   (installed? %))}
                             :async true
                             :effect (req (clear-wait-prompt state :corp)
                                          (system-msg state :runner
                                                      (str "trashes " (:title target)
                                                           " to prevent Financial Collapse"))
                                          (trash state :runner eid target {:unpreventable true}))}
                            :no-ability {:player :corp
                                         :effect (effect (lose-credits :runner (* rcount 2))
                                                         (clear-wait-prompt :corp))
                                         :msg (msg "make the Runner lose " (* rcount 2) " [Credits]")}}}
                          card nil))
                    (continue-ability
                      state side
                      {:effect (effect (lose-credits :runner (* rcount 2)))
                       :msg (msg "make the Runner lose " (* rcount 2) " [Credits]")}
                      card nil))))})

(define-card "Focus Group"
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
                                      (if (can-pay? state side (assoc eid :source card :source-type :ability) card (:title card) :credit c)
                                        (do (pay state :corp card :credit c)
                                            (continue-ability
                                              state :corp
                                              {:msg (msg "place " (quantify c " advancement token") " on "
                                                         (card-str state target))
                                               :choices {:card installed?}
                                               :effect (effect (add-prop target :advance-counter c {:placed true}))}
                                              card nil))
                                        (effect-completed state side eid))))}
                      card nil)
                    (effect-completed state side eid))))})

(define-card "Foxfire"
  {:trace {:base 7
           :successful {:prompt "Select 1 card to trash"
                        :choices {:card #(and (installed? %)
                                              (or (has-subtype? % "Virtual")
                                                  (has-subtype? % "Link")))}
                        :msg "trash 1 virtual resource or link"
                        :async true
                        :effect (effect (system-msg (str "trashes " (:title target)))
                                        (trash eid target nil))}}})

(define-card "Freelancer"
  {:req (req tagged)
   :msg (msg "trash " (join ", " (map :title (sort-by :title targets))))
   :choices {:max 2
             :card #(and (installed? %)
                         (resource? %))}
   :async true
   :effect (effect (trash-cards :runner eid targets))})

(define-card "Friends in High Places"
  (let [fhelper (fn fhp [n] {:prompt "Select a card in Archives to install with Friends in High Places"
                             :async true
                             :show-discard true
                             :choices {:card #(and (corp? %)
                                                   (not (operation? %))
                                                   (in-discard? %))}
                             :effect (req (wait-for
                                            (corp-install state side target nil nil)
                                            (do (system-msg state side (str "uses Friends in High Places to "
                                                                            (corp-install-msg target)))
                                                (if (< n 2)
                                                  (continue-ability state side (fhp (inc n)) card nil)
                                                  (effect-completed state side eid)))))})]
    {:async true
     :effect (effect (continue-ability (fhelper 1) card nil))}))

(define-card "Fully Operational"
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
                              (do (gain-credits state :corp 2)
                                  (continue-ability state side (repeat-choice (inc current) total)
                                                    card nil))
                              (wait-for (draw state :corp 2 nil) ; don't proceed with the next choice until the draw is done
                                        (continue-ability state side (repeat-choice (inc current) total)
                                                          card nil))))}))]
    {:async true
     :msg (msg "uses Fully Operational to make " (quantify (inc (count (full-servers state))) "gain/draw decision"))
     :effect (effect (continue-ability (repeat-choice 1 (inc (count (full-servers state))))
                                       card nil))}))

(define-card "Game Changer"
  {:rfg-instead-of-trashing true
   :effect (effect (gain :click (count (:scored runner))))})

(define-card "Game Over"
  {:req (req (last-turn? state :runner :stole-agenda))
   :async true
   :prompt "Choose a card type"
   :choices ["Hardware" "Program" "Resource"]
   :effect (req (let [card-type target
                      trashtargets (filter #(and (is-type? % card-type)
                                                 (not (has-subtype? % "Icebreaker")))
                                           (all-active-installed state :runner))
                      numtargets (count trashtargets)
                      typemsg (str (when (= card-type "Program") "non-Icebreaker ") card-type
                                   (when-not (= card-type "Hardware") "s"))]
                  (system-msg state :corp (str "chooses to trash all " typemsg))
                  (show-wait-prompt state :corp "Runner to prevent trashes")
                  (wait-for (resolve-ability
                              state :runner
                              {:async true
                               :req (req (<= 3 (:credit runner)))
                               :prompt (msg "Prevent any " typemsg " from being trashed? Pay 3 [Credits] per card.")
                               :choices {:max (req (min numtargets (quot (total-available-credits state :runner eid card) 3)))
                                         :card #(and (installed? %)
                                                     (is-type? % card-type)
                                                     (not (has-subtype? % "Icebreaker")))}
                               :effect (req (wait-for (pay-sync state :runner card :credit (* 3 (count targets)))
                                                      (system-msg
                                                        state :runner
                                                        (str async-result " to prevent the trashing of "
                                                             (join ", " (map :title (sort-by :title targets)))))
                                                      (effect-completed state side (make-result eid targets))))}
                              card nil)
                            (let [prevented async-result]
                              (when (not async-result)
                                (system-msg state :runner (str "chooses to not prevent Corp trashing all " typemsg)))
                              (wait-for (trash-cards state side (clojure.set/difference (set trashtargets) (set async-result)))
                                        (clear-wait-prompt state :corp)
                                        (system-msg state :corp
                                                    (str "trashes all "
                                                         (when (seq prevented) "other ")
                                                         typemsg
                                                         ": " (join ", " (map :title (sort-by :title async-result)))))
                                        (wait-for (gain-bad-publicity state :corp 1)
                                                  (when async-result
                                                    (system-msg state :corp "takes 1 bad publicity from Game Over"))
                                                  (effect-completed state side eid)))))))})

(define-card "Genotyping"
  {:async true
   :msg "trash the top 2 cards of R&D"
   :rfg-instead-of-trashing true
   :effect (req (wait-for (mill state :corp :corp 2)
                          (shuffle-into-rd-effect state side eid card 4 false)))})

(define-card "Green Level Clearance"
  {:msg "gain 3 [Credits] and draw 1 card"
   :async true
   :effect (effect (gain-credits 3)
                   (draw eid 1 nil))})

(define-card "Hangeki"
  {:req (req (last-turn? state :runner :trashed-card))
   :async true
   :prompt "Choose an installed Corp card"
   :choices {:card #(and (corp? %)
                         (installed? %))}
   :effect (effect (show-wait-prompt :corp "Runner to resolve Hangeki")
                   (continue-ability
                     {:optional
                      {:player :runner
                       :async true
                       :prompt "Access card? (If not, add Hangeki to your score area worth -1 agenda point)"
                       :yes-ability
                       {:async true
                        :effect (req (clear-wait-prompt state :corp)
                                     (wait-for (access-card state side target)
                                               (update! state side (assoc card :rfg-instead-of-trashing true))
                                               (effect-completed state side eid)))}
                       :no-ability
                       {:async true
                        :msg "add it to the Runner's score area as an agenda worth -1 agenda point"
                        :effect (effect (clear-wait-prompt :corp)
                                        (as-agenda :runner eid card -1))}}}
                     card targets))})

(define-card "Hard-Hitting News"
  {:req (req (last-turn? state :runner :made-run))
   :trace {:base 4
           :label "Give the Runner 4 tags"
           :successful {:async true
                        :msg "give the Runner 4 tags"
                        :effect (effect (gain-tags eid 4))}}})

(define-card "Hasty Relocation"
  (letfn [(hr-final [chosen original]
            {:prompt (str "The top cards of R&D will be " (join  ", " (map :title chosen)) ".")
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
    {:additional-cost [:trash-from-deck 1]
     :async true
     :msg "trash the top card of R&D, draw 3 cards, and add 3 cards in HQ to the top of R&D"
     :effect (req (wait-for (draw state side 3 nil)
                            (show-wait-prompt state :runner "Corp to add 3 cards in HQ to the top of R&D")
                            (let [from (get-in @state [:corp :hand])]
                              (continue-ability state :corp (hr-choice from '() 3 from) card nil))))}))

(define-card "Hatchet Job"
  {:trace {:base 5
           :successful {:choices {:card #(and (installed? %)
                                              (runner? %)
                                              (not (has-subtype? % "Virtual")))}
                        :msg "add an installed non-virtual card to the Runner's grip"
                        :effect (effect (move :runner target :hand true))}}})

(define-card "Hedge Fund"
  {:msg "gain 9 [Credits]" :effect (effect (gain-credits 9))})

(define-card "Hellion Alpha Test"
  {:req (req (last-turn? state :runner :installed-resource))
   :trace {:base 2
           :successful {:msg "add a Resource to the top of the Stack"
                        :choices {:card #(and (installed? %)
                                              (resource? %))}
                        :effect (effect (move :runner target :deck {:front true})
                                        (system-msg (str "adds " (:title target) " to the top of the Stack")))}
           :unsuccessful {:msg "take 1 bad publicity"
                          :effect (effect (gain-bad-publicity :corp 1))}}})

(define-card "Hellion Beta Test"
  {:req (req (last-turn? state :runner :trashed-card))
   :trace {:base 2
           :label "Trash 2 installed non-program cards or take 1 bad publicity"
           :successful {:choices {:max (req (min 2 (count (filter #(or (facedown? %)
                                                                       (not (program? %)))
                                                                  (concat (all-installed state :corp)
                                                                          (all-installed state :runner))))))
                                  :all true
                                  :card #(and (installed? %)
                                              (not (program? %)))}
                        :msg (msg "trash " (join ", " (map :title (sort-by :title targets))))
                        :async true
                        :effect (effect (trash-cards eid targets))}
           :unsuccessful {:msg "take 1 bad publicity"
                          :async true
                          :effect (effect (gain-bad-publicity :corp eid 1))}}})

(define-card "Heritage Committee"
  {:async true
   :effect (req (wait-for (draw state side 3 nil)
                          (continue-ability state side
                                            {:prompt "Select a card in HQ to put on top of R&D"
                                             :choices {:card #(and (corp? %)
                                                                   (in-hand? %))}
                                             :msg "draw 3 cards and add 1 card from HQ to the top of R&D"
                                             :effect (effect (move target :deck {:front true}))}
                                            card nil)))})

(define-card "High-Profile Target"
  (letfn [(dmg-count [state] (* 2 (count-tags state)))]
    {:req (req tagged)
     :async true
     :msg (msg "do " (dmg-count state) " meat damage")
     :effect (effect (damage eid :meat (dmg-count state) {:card card}))}))

(define-card "Housekeeping"
  {:events [{:event :runner-install
             :req (req (first-event? state side :runner-install))
             :player :runner
             :prompt "Select a card from your Grip to trash for Housekeeping"
             :choices {:card #(and (runner? %)
                                   (in-hand? %))}
             :async true
             :msg (msg "force the Runner to trash" (:title target) " from their grip")
             :effect (effect (trash :runner eid target {:unpreventable true}))}]})

(define-card "Hunter Seeker"
  {:req (req (last-turn? state :runner :stole-agenda))
   :async true
   :prompt "Choose a card to trash"
   :choices {:card installed?}
   :msg (msg "trash " (card-str state target))
   :effect (effect (trash eid target nil))})

(define-card "Hyoubu Precog Manifold"
  {:trash-after-resolving false
   :prompt "Choose a server"
   :choices (req servers)
   :msg (msg "choose " target)
   :effect (effect (update! (assoc-in card [:special :hyoubu-precog-target] target)))
   :events [{:event :successful-run
             :req (req (= (zone->name (get-in @state [:run :server])) (get-in card [:special :hyoubu-precog-target])))
             :psi {:not-equal {:msg "end the run"
                               :async true
                               :effect (effect (end-run eid card))}}}
            {:event :corp-turn-begins
             :async true
             :effect (effect (trash eid card nil))}]})

(define-card "Interns"
  {:async true
   :prompt "Select a card to install from Archives or HQ"
   :show-discard true
   :not-distinct true
   :choices {:card #(and (not (operation? %))
                         (corp? %)
                         (#{[:hand] [:discard]} (:zone %)))}
   :effect (effect (corp-install eid target nil {:ignore-install-cost true}))
   :msg (msg (corp-install-msg target))})

(define-card "Invasion of Privacy"
  (letfn [(iop [x]
            {:async true
             :req (req (->> (:hand runner)
                            (filter #(or (resource? %)
                                         (event? %)))
                            count
                            pos?))
             :prompt "Choose a resource or event to trash"
             :msg (msg "trash " (:title target))
             :choices (req (cancellable
                             (filter #(or (resource? %)
                                          (event? %))
                                     (:hand runner))
                             :sorted))
             :effect (req (wait-for (trash state side target nil)
                                    (if (pos? x)
                                      (continue-ability state side (iop (dec x)) card nil)
                                      (effect-completed state side eid))))})]
    {:trace {:base 2
             :successful {:msg "reveal the Runner's Grip and trash up to X resources or events"
                          :async true
                          :effect (req (reveal state side (:hand runner))
                                    (let [x (- target (second targets))]
                                      (system-msg
                                        state :corp
                                        (str "reveals the Runner's Grip ( "
                                             (join ", " (map :title (sort-by :title (:hand runner))))
                                             " ) and can trash up to " x " resources or events"))
                                      (continue-ability state side (iop (dec x)) card nil)))}
             :unsuccessful {:msg "take 1 bad publicity"
                            :async true
                            :effect (effect (gain-bad-publicity :corp eid 1))}}}))

(define-card "IPO"
  {:msg "gain 13 [Credits]"
   :effect (effect (gain-credits 13))})

(define-card "Kakurenbo"
  (let [install-abi {:async true
                     :prompt "Select an agenda, asset or upgrade to install from Archives and place 2 advancement tokens on"
                     :show-discard true
                     :not-distinct true
                     :choices {:card #(and (or (agenda? %)
                                               (asset? %)
                                               (upgrade? %))
                                           (in-discard? %))}
                     :effect (req (wait-for (corp-install state side (make-eid state {:source card :source-type :corp-install})
                                                          target nil nil)
                                            (system-msg state side "uses Kakurenbo to place 2 advancements counters on it")
                                            (add-prop state side eid async-result :advance-counter 2 {:placed true})))}]
    {:prompt "Select any number of cards in HQ to trash"
     :rfg-instead-of-trashing true
     :choices {:max (req (count (:hand corp)))
               :card #(and (corp? %)
                           (in-hand? %))}
     :msg (msg "trash " (count targets) " cards in HQ")
     :async true
     :effect (req (wait-for (trash-cards state side targets {:unpreventable true})
                            (doseq [c (:discard (:corp @state))]
                              (update! state side (assoc-in c [:seen] false)))
                            (shuffle! state :corp :discard)
                            (continue-ability state side install-abi card nil)))
     :cancel-effect (req (doseq [c (:discard (:corp @state))]
                           (update! state side (assoc-in c [:seen] false)))
                         (shuffle! state :corp :discard)
                         (continue-ability state side install-abi card nil))}))

(define-card "Kill Switch"
  (let [trace-for-brain-damage {:msg (msg "reveal that they accessed " (:title target))
                                :trace {:base 3
                                        :successful {:msg "do 1 brain damage"
                                                     :async true
                                                     :effect (effect (damage :runner eid :brain 1 {:card card}))}}}]
    {:events [(assoc trace-for-brain-damage
                     :event :access
                     :req (req (agenda? target))
                     :interactive (req (agenda? target)))
              (assoc trace-for-brain-damage :event :agenda-scored)]}))

(define-card "Lag Time"
  {:effect (effect (update-all-ice))
   :constant-effects [{:type :ice-strength
                       :value 1}]
   :leave-play (effect (update-all-ice))})

(define-card "Lateral Growth"
  {:async true
   :msg "gain 4 [Credits]"
   :effect (effect (gain-credits 4)
                   (continue-ability {:player :corp
                                      :prompt "Select a card to install"
                                      :choices {:card #(and (corp? %)
                                                            (not (operation? %))
                                                            (in-hand? %))}
                                      :async true
                                      :msg (msg (corp-install-msg target))
                                      :effect (effect (corp-install eid target nil nil))}
                                     card nil))})

(define-card "Liquidation"
  {:async true
   :req (req (some #(and (rezzed? %)
                         (not (agenda? %)))
                   (all-installed state :corp)))
   :effect (req (let [n (count (filter #(not (agenda? %)) (all-active-installed state :corp)))]
                  (continue-ability
                    state side
                    {:async true
                     :prompt "Select any number of rezzed cards to trash"
                     :choices {:max n
                               :card #(and (rezzed? %)
                                           (not (agenda? %)))}
                     :msg (msg "trash " (join ", " (map :title targets))
                               " and gain " (* (count targets) 3) " [Credits]")
                     :effect (req (wait-for (trash-cards state side targets nil)
                                            (gain-credits state side (* (count targets) 3))
                                            (effect-completed state side eid)))}
                    card nil)))})

(define-card "Load Testing"
  {:msg "make the Runner lose [Click] when their next turn begins"
   :events [{:event :runner-turn-begins
             :duration :until-runner-turn-begins
             :msg "make the Runner lose [Click]"
             :effect (effect (lose :runner :click 1))}]})

(define-card "Localized Product Line"
  {:async true
   :prompt "Choose a card"
   :choices (req (cancellable (:deck corp) :sorted))
   :effect (effect
             (continue-ability
               (let [title (:title target)
                     copies (filter #(= (:title %) title) (:deck corp))]
                 {:prompt "How many copies?"
                  :choices {:number (req (count copies))}
                  :msg (msg "add " (quantify target "cop" "y" "ies") " of " title " to HQ")
                  :effect (req (shuffle! state :corp :deck)
                               (doseq [copy (take target copies)]
                                 (move state side copy :hand)))})
               card nil))})

(define-card "Manhunt"
  {:events [{:event :successful-run
             :interactive (req true)
             :req (req (first-event? state side :successful-run))
             :trace {:base 2
                     :successful {:msg "give the Runner 1 tag"
                                  :async true
                                  :effect (effect (gain-tags eid 1))}}}]})

(define-card "Market Forces"
  (letfn [(credit-diff [state]
            (min (* 3 (count-tags state))
                 (get-in @state [:runner :credit])))]
    {:req (req tagged)
     :msg (msg (let [c (credit-diff state)]
                 (str "make the runner lose " c " [Credits], and gain " c " [Credits]")))
     :effect (req (let [c (credit-diff state)]
                    (lose-credits state :runner c)
                    (gain-credits state :corp c)))}))

(define-card "Mass Commercialization"
  {:msg (msg "gain " (* 2 (count (filter #(pos? (+ (get-counters % :advancement) (:extra-advance-counter % 0)))
                                         (get-all-installed state)))) " [Credits]")
   :effect (effect (gain-credits (* 2 (count (filter #(pos? (+ (get-counters % :advancement) (:extra-advance-counter % 0)))
                                                     (get-all-installed state))))))})

(define-card "MCA Informant"
  {:req (req (not-empty (filter #(has-subtype? % "Connection")
                                (all-active-installed state :runner))))
   :prompt "Choose a connection to host MCA Informant on"
   :choices {:card #(and (runner? %)
                         (has-subtype? % "Connection")
                         (installed? %))}
   :msg (msg "host it on " (card-str state target) ". The Runner has an additional tag")
   :effect (req (host state side (get-card state target) (assoc card :seen true :condition true))
                (swap! state update-in [:runner :tag :additional] inc)
                (trigger-event state :corp :runner-additional-tag-change 1))
   :leave-play (req (swap! state update-in [:runner :tag :additional] dec)
                    (trigger-event state :corp :runner-additional-tag-change -1)
                    (system-msg state :corp "trashes MCA Informant"))
   :runner-abilities [{:label "Trash MCA Informant host"
                       :cost [:click 1 :credit 2]
                       :req (req (= :runner side))
                       :async true
                       :effect (effect (system-msg :runner (str "spends [Click] and 2 [Credits] to trash "
                                                                (card-str state (:host card))))
                                       (trash :runner eid (get-card state (:host card)) nil))}]})

(define-card "Media Blitz"
  {:implementation "Media Blitz gains the title of whichever agenda is selected"
   :async true
   :req (req (pos? (count (:scored runner))))
   :effect
   (effect
     (continue-ability
       {:prompt "Select an agenda in the runner's score area"
        :choices {:card #(and (agenda? %)
                              (is-scored? state :runner %))}
        :effect (req (update! state side (assoc card :title (:title target)))
                     ;; TODO: Move reverting the name back to here when we move
                     ;; abilities into the card object and stop relying on `card-def`.
                     (card-init state side (get-card state card) {:resolve-effect false :init-data true}))}
       card nil))
   :events [{:event :trash-current
             :req (req (same-card? card target))
             :effect (effect (update! (assoc card :title "Media Blitz")))}]})

(define-card "Medical Research Fundraiser"
  {:msg "gain 8 [Credits]. The Runner gains 3 [Credits]"
   :effect (effect (gain-credits 8)
                   (gain-credits :runner 3))})

(define-card "Midseason Replacements"
  {:req (req (last-turn? state :runner :stole-agenda))
   :trace {:base 6
           :label "Trace 6 - Give the Runner X tags"
           :successful {:msg "give the Runner X tags"
                        :async true
                        :effect (effect (system-msg
                                          (str "gives the Runner " (- target (second targets)) " tags"))
                                        (gain-tags eid (- target (second targets))))}}})

(define-card "Mushin No Shin"
  {:prompt "Select a card to install from HQ"
   :choices {:card #(and (not (operation? %))
                         (corp? %)
                         (in-hand? %))}
   :async true
   :effect (req (wait-for (corp-install state side target "New remote" nil)
                          (let [installed-card async-result]
                            (add-prop state side installed-card :advance-counter 3 {:placed true})
                            (register-turn-flag!
                              state side
                              card :can-rez
                              (fn [state side card]
                                (if (same-card? card installed-card)
                                  ((constantly false) (toast state :corp "Cannot rez due to Mushin No Shin." "warning"))
                                  true)))
                            (register-turn-flag!
                              state side
                              card :can-score
                              (fn [state side card]
                                (if (same-card? card installed-card)
                                  ((constantly false) (toast state :corp "Cannot score due to Mushin No Shin." "warning"))
                                  true)))
                            (effect-completed state side eid))))})

(define-card "Mutate"
  {:req (req (some #(and (ice? %)
                         (rezzed? %))
                   (all-installed state :corp)))
   :prompt "Select a rezzed piece of ice to trash"
   :choices {:card #(and (ice? %)
                         (rezzed? %))}
   :async true
   :effect (req (let [index (ice-index state target)
                      [revealed-cards r] (split-with (complement ice?) (get-in @state [:corp :deck]))
                      titles (->> (conj (vec revealed-cards) (first r))
                                  (filter identity)
                                  (map :title))]
                  (wait-for (trash state :corp target nil)
                            (shuffle! state :corp :deck)
                            (do
                              (system-msg state side (str "uses Mutate to trash " (:title target)))
                              (when (seq titles)
                                (reveal state side revealed-cards)
                                (system-msg state side (str "reveals " (clojure.string/join ", " titles) " from R&D")))
                              (let [ice (first r)
                                    zone (zone->name (second (:zone target)))]
                                (if ice
                                  (do (system-msg state side (str "uses Mutate to install and rez " (:title ice) " from R&D at no cost"))
                                      (corp-install state side eid ice zone {:ignore-all-cost true
                                                                             :install-state :rezzed-no-cost
                                                                             :display-message false
                                                                             :index index}))
                                  (do (system-msg state side "does not find any ICE to install from R&D")
                                      (effect-completed state side eid))))))))})

(define-card "NAPD Cordon"
  {:trash-after-resolving false
   :events [{:event :pre-steal-cost
             :effect (req (let [counter (get-counters target :advancement)]
                            (steal-cost-bonus state side [:credit (+ 4 (* 2 counter))])))}
            {:event :corp-turn-begins
             :async true
             :effect (effect (trash eid card nil))}]})

(define-card "Neural EMP"
  {:req (req (last-turn? state :runner :made-run))
   :msg "do 1 net damage"
   :effect (effect (damage eid :net 1 {:card card}))})

(define-card "NEXT Activation Command"
  {:trash-after-resolving false
   :constant-effects [{:type :ice-strength
                       :value 2}
                      {:type :prevent-ability
                       :req (req (let [target-card (first targets)
                                       ability (second targets)]
                                   (and (not (has-subtype? target-card "Icebreaker"))
                                        (:break ability))))
                       :value true}]
   :effect (effect (update-all-ice))
   :trash-effect {:effect (effect (update-all-ice))}
   :events [{:event :corp-turn-begins
             :async true
             :effect (effect (trash eid card nil))}]})

(define-card "O₂ Shortage"
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
                          :yes-ability {:async true
                                        :effect (effect (clear-wait-prompt :corp)
                                                        (trash-cards :runner eid (take 1 (shuffle (:hand runner))) nil))}
                          :no-ability {:msg "gain [Click][Click]"
                                       :effect (effect (clear-wait-prompt :corp)
                                                       (gain :corp :click 2))}}}
                        card nil))))})

(define-card "Observe and Destroy"
  {:async true
   :additional-cost [:tag 1]
   :req (req (and (pos? (count-tags state))
                  (< (:credit runner) 6)))
   :prompt "Select an installed card to trash"
   :choices {:card #(and (runner? %)
                         (installed? %))}
   :msg (msg "remove 1 Runner tag and trash " (card-str state target))
   :effect (effect (trash eid target nil))})

(define-card "Oversight AI"
  {:choices {:card #(and (ice? %)
                         (not (rezzed? %))
                         (= (last (:zone %)) :ices))}
   :msg (msg "rez " (card-str state target) " at no cost")
   :effect (effect (rez target {:ignore-cost :all-costs
                                :no-msg true})
                   (host (get-card state target) (assoc card :seen true :condition true)))
   :events [{:event :subroutines-broken
             :condition :hosted
             :async true
             :req (req (and (same-card? target (:host card))
                            (empty? (remove :broken (:subroutines target)))))
             :msg (msg "trash itself and " (card-str state target))
             :effect (effect (trash :corp eid target {:unpreventable true}))}]})

(define-card "Patch"
  {:choices {:card #(and (ice? %)
                         (rezzed? %))}
   :msg (msg "give +2 strength to " (card-str state target))
   :effect (req (let [card (host state side target (assoc card :seen true :condition true))]
                  (update-ice-strength state side (get-card state target))))
   :constant-effects [{:type :ice-strength
                       :req (req (same-card? target (:host card)))
                       :value 2}]})

(define-card "Paywall Implementation"
  {:events [{:event :successful-run
             :msg "gain 1 [Credits]"
             :effect (effect (gain-credits :corp 1))}]})

(define-card "Peak Efficiency"
  {:msg (msg "gain " (reduce (fn [c server]
                               (+ c (count (filter (fn [ice] (:rezzed ice)) (:ices server)))))
                             0 (flatten (seq (:servers corp))))
             " [Credits]")
   :effect (effect (gain-credits
                     (reduce (fn [c server]
                               (+ c (count (filter (fn [ice] (:rezzed ice)) (:ices server)))))
                             0 (flatten (seq (:servers corp))))))})

(define-card "Power Grid Overload"
  {:req (req (last-turn? state :runner :made-run))
   :trace {:base 2
           :successful {:msg "trash 1 piece of hardware"
                        :async true
                        :effect
                        (effect
                          (continue-ability
                            (let [max-cost (- target (second targets))]
                              {:choices {:card #(and (hardware? %)
                                                     (<= (:cost %) max-cost))}
                               :msg (msg "trash " (:title target))
                               :effect (effect (trash eid target nil))})
                            card nil))}}})

(define-card "Power Shutdown"
  {:req (req (and (last-turn? state :runner :made-run)
                  (not-empty (filter #(or (hardware? %)
                                          (program? %))
                                     (all-active-installed state :runner)))))
   :prompt "Trash how many cards from the top R&D?"
   :choices {:number (req (->> (all-active-installed state :runner)
                               (filter #(or (hardware? %)
                                            (program? %)))
                               (map :cost)
                               (apply max)))}
   :msg (msg "trash " target " cards from the top of R&D")
   :async true
   :effect (req (wait-for (mill state :corp :corp target)
                          (continue-ability
                            state :runner
                            (let [n target]
                              {:async true
                               :prompt "Select a Program or piece of Hardware to trash"
                               :choices {:card #(and (or (hardware? %)
                                                         (program? %))
                                                     (<= (:cost %) n))}
                               :msg (msg "trash " (:title target))
                               :effect (effect (trash eid target nil))})
                            card nil)))})

(define-card "Precognition"
  {:async true
   :msg "rearrange the top 5 cards of R&D"
   :effect (req (show-wait-prompt state :runner "Corp to rearrange the top cards of R&D")
                (let [from (take 5 (:deck corp))]
                  (if (pos? (count from))
                    (continue-ability state side (reorder-choice :corp :runner from '()
                                                                 (count from) from) card nil)
                    (do (clear-wait-prompt state :runner)
                        (effect-completed state side eid)))))})

(define-card "Predictive Algorithm"
  {:events [{:event :pre-steal-cost
             :effect (effect (steal-cost-bonus [:credit 2]))}]})

(define-card "Preemptive Action"
  {:async true
   :rfg-instead-of-trashing true
   :effect (effect (shuffle-into-rd-effect eid card 3 true))})

(define-card "Priority Construction"
  (letfn [(install-card [chosen]
            {:prompt "Select a remote server"
             :choices (req (conj (vec (get-remote-names state)) "New remote"))
             :async true
             :effect (effect (corp-install eid (assoc chosen :advance-counter 3) target {:ignore-all-cost true}))})]
    {:async true
     :prompt "Choose a piece of ICE in HQ to install"
     :choices {:card #(and (in-hand? %)
                        (corp? %)
                        (ice? %))}
     :msg "install an ICE from HQ and place 3 advancements on it"
     :cancel-effect (req (effect-completed state side eid))
     :effect (effect (continue-ability (install-card target) card nil))}))

(define-card "Product Recall"
  {:async true
   :prompt "Select a rezzed asset or upgrade to trash"
   :choices {:card #(and (rezzed? %)
                         (or (asset? %)
                             (upgrade? %)))}
   :msg (msg "trash " (card-str state target)
             " and gain " (trash-cost state side target) " [Credits]")
   :effect (req (wait-for (trash state side target {:unpreventable true})
                          (gain-credits state :corp (trash-cost state side target))
                          (effect-completed state side eid)))})

(define-card "Psychographics"
  {:req (req tagged)
   :async true
   :prompt "Pay how many credits?"
   :choices {:number (req (count-tags state))}
   :effect (req (let [c target]
                  (if (can-pay? state side (assoc eid :source card :source-type :ability) card (:title card) :credit c)
                    (do (pay state :corp card :credit c)
                        (continue-ability
                          state side
                          {:msg (msg "place " (quantify c " advancement token") " on " (card-str state target))
                           :choices {:card can-be-advanced?}
                           :effect (effect (add-prop target :advance-counter c {:placed true}))}
                          card nil))
                    (effect-completed state side eid))))})

(define-card "Psychokinesis"
  (letfn [(choose-card [state cards]
            (let [allowed-cards (filter #(some #{"New remote"} (installable-servers state %))
                                        cards)]
              {:prompt "Select an agenda, asset, or upgrade to install"
               :choices (conj (vec allowed-cards) "None")
               :async true
               :effect (req (if (or (= target "None")
                                    (ice? target)
                                    (operation? target))
                              (do (clear-wait-prompt state :runner)
                                  (system-msg state side "does not install an asset, agenda, or upgrade")
                                  (effect-completed state side eid))
                              (continue-ability state side (install-card target) card nil)))}))
          (install-card [chosen]
            {:prompt "Select a remote server"
             :choices (req (conj (vec (get-remote-names state)) "New remote"))
             :async true
             :effect (effect (clear-wait-prompt :runner)
                             (corp-install eid chosen target nil))})]
    {:msg "look at the top 5 cards of R&D"
     :async true
     :effect (req (show-wait-prompt state :runner "Corp to look at the top cards of R&D")
               (let [top-5 (take 5 (:deck corp))]
                 (continue-ability state side (choose-card state top-5) card nil)))}))

(define-card "Punitive Counterstrike"
  {:trace {:base 5
           :successful {:async true
                        :msg (msg "do " (:stole-agenda runner-reg-last 0) " meat damage")
                        :effect (effect (damage eid :meat (:stole-agenda runner-reg-last 0) {:card card}))}}})

(define-card "Reclamation Order"
  {:prompt "Select a card from Archives"
   :show-discard true
   :choices {:card #(and (corp? %)
                         (not= (:title %) "Reclamation Order")
                         (in-discard? %))}
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
                                    card nil)))})

(define-card "Recruiting Trip"
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
     :effect (effect (continue-ability (rthelp target target []) card nil))}))

(define-card "Red Level Clearance"
  (let [all [{:msg "gain 2 [Credits]"
              :effect (effect (gain-credits 2))}
             {:msg "draw 2 cards"
              :effect (effect (draw 2))}
             {:msg "gain [Click]"
              :effect (effect (gain :click 1))}
             {:prompt "Choose a non-agenda to install"
              :msg "install a non-agenda from hand"
              :choices {:card #(and (not (agenda? %))
                                    (not (operation? %))
                                    (in-hand? %))}
              :async true
              :effect (effect (corp-install eid target nil nil))}]
        can-install? (fn [hand]
                       (seq (remove #(or (agenda? %)
                                         (operation? %))
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
                                               (continue-ability state side
                                                                 (choice (remove #(= % chosen) abis) true)
                                                                 card nil)
                                               (do (clear-wait-prompt state :runner)
                                                   (effect-completed state side eid))))
                                   (continue-ability state side (choice abis chose-once) card nil))))})]
    {:async true
     :effect (effect (show-wait-prompt :runner "Corp to use Red Level Clearance")
                     (continue-ability (choice all false) card nil))})

  )

(define-card "Red Planet Couriers"
  {:async true
   :req (req (some #(can-be-advanced? %) (all-installed state :corp)))
   :prompt "Select an installed card that can be advanced"
   :choices {:card can-be-advanced?}
   :effect (req (let [installed (get-all-installed state)
                      total-adv (reduce + (map #(get-counters % :advancement) installed))]
                  (doseq [c installed]
                    (set-prop state side c :advance-counter 0))
                  (set-prop state side target :advance-counter total-adv)
                  (update-all-ice state side)
                  (system-msg state side (str "uses Red Planet Couriers to move " total-adv
                                              " advancement tokens to " (card-str state target)))
                  (effect-completed state side eid)))})

(define-card "Replanting"
  (letfn [(replant [n]
            {:prompt "Select a card to install with Replanting"
             :async true
             :choices {:card #(and (corp? %)
                                   (not (operation? %))
                                   (in-hand? %))}
             :effect (req (wait-for (corp-install state side target nil {:ignore-all-cost true})
                                    (if (< n 2)
                                      (continue-ability state side (replant (inc n)) card nil)
                                      (effect-completed state side eid))))})]
    {:async true
     :prompt "Select an installed card to add to HQ"
     :choices {:card #(and (corp? %)
                        (installed? %))}
     :msg (msg "add " (card-str state target) " to HQ, then install 2 cards ignoring all costs")
     :effect (req (move state side target :hand)
               (resolve-ability state side (replant 1) card nil))}))

(define-card "Restore"
  {:async true
   :effect (effect (continue-ability
                     {:prompt "Select a card in Archives to install & rez with Restore"
                      :async true
                      :show-discard true
                      :choices {:card #(and (corp? %)
                                            (not (operation? %))
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
                     card nil))})

(define-card "Restoring Face"
  {:async true
   :prompt "Select a Sysop, Executive or Clone to trash"
   :msg (msg "trash " (:title target) " to remove 2 bad publicity")
   :choices {:card #(or (has-subtype? % "Clone")
                        (has-subtype? % "Executive")
                        (has-subtype? % "Sysop"))}
   :effect (req (lose-bad-publicity state side 2)
                (when (facedown? target)
                  (reveal state side target))
                (trash state side eid target nil))})

(define-card "Restructure"
  {:msg "gain 15 [Credits]"
   :effect (effect (gain-credits 15))})

(define-card "Reuse"
  {:async true
   :prompt (msg "Select up to " (quantify (count (:hand corp)) "card") " in HQ to trash")
   :choices {:max (req (count (:hand corp)))
             :card #(and (corp? %)
                         (in-hand? %))}
   :msg (msg (let [m (count targets)]
               (str "trash " (quantify m "card")
                    " and gain " (* 2 m) " [Credits]")))
   :effect (req (wait-for (trash-cards state side targets {:unpreventable true})
                          (gain-credits state side eid (* 2 (count async-result)) nil)))})

(define-card "Reverse Infection"
  {:async true
   :prompt "Choose one:"
   :choices ["Purge virus counters"
             "Gain 2 [Credits]"]
   :effect (req (if (= target "Gain 2 [Credits]")
                  (do (gain-credits state side 2)
                      (system-msg state side "uses Reverse Infection to gain 2 [Credits]")
                      (effect-completed state side eid))
                  (let [pre-purge-virus (number-of-virus-counters state)]
                    (purge state side)
                    (let [post-purge-virus (number-of-virus-counters state)
                          num-virus-purged (- pre-purge-virus post-purge-virus)
                          num-to-trash (quot num-virus-purged 3)]
                      (wait-for (mill state :corp :runner num-to-trash)
                                (system-msg state side
                                            (str "uses Reverse Infection to purge "
                                                 (quantify num-virus-purged "virus counter")
                                                 " and trash "
                                                 (quantify num-to-trash "card")
                                                 " from the top of the stack"))
                                (effect-completed state side eid))))))})

(define-card "Rework"
  {:prompt "Select a card from HQ to shuffle into R&D"
   :choices {:card #(and (corp? %)
                         (in-hand? %))}
   :msg "shuffle a card from HQ into R&D"
   :effect (effect (move target :deck)
                   (shuffle! :deck))})

(define-card "Riot Suppression"
  {:req (req (last-turn? state :runner :trashed-card))
   :async true
   :rfg-instead-of-trashing true
   :effect (effect (show-wait-prompt :corp "Runner to decide if they will take 1 brain damage")
                   (continue-ability
                     (let [c card]
                       {:optional
                        {:prompt "Take 1 brain damage to prevent having 3 fewer clicks next turn?"
                         :player :runner
                         :end-effect (effect (clear-wait-prompt :corp))
                         :yes-ability
                         {:async true
                          :effect (req (system-msg
                                         state :runner
                                         "suffers 1 brain damage to prevent losing 3[Click] to Riot Suppression")
                                       (damage state :runner eid :brain 1 {:card card}))}
                         :no-ability
                         {:msg "give the runner 3 fewer [Click] next turn"
                          :effect (req (swap! state update-in [:runner :extra-click-temp] (fnil #(- % 3) 0)))}}})
                     card nil))})

(define-card "Rolling Brownout"
  {:msg "increase the play cost of operations and events by 1 [Credits]"
   :constant-effects [{:type :play-cost
                       :value 1}]
   :events [{:event :play-event
             :once :per-turn
             :msg "gain 1 [Credits]"
             :effect (effect (gain-credits :corp 1))}]})

(define-card "Rover Algorithm"
  {:choices {:card #(and (ice? %)
                         (rezzed? %))}
   :msg (msg "host it as a condition counter on " (card-str state target))
   :effect (effect (host target (assoc card :installed true :seen true :condition true))
                   (update-ice-strength (get-card state target)))
   :constant-effects [{:type :ice-strength
                       :req (req (same-card? target (:host card)))
                       :value (req (get-counters card :power))}]
   :events [{:event :pass-ice
             :condition :hosted
             :req (req (same-card? target (:host card)))
             :effect (effect (add-counter card :power 1))}]})

(define-card "Sacrifice"
  {:req (req (and (pos? (count-bad-pub state))
                  (some #(pos? (:agendapoints %)) (:scored corp))))
   :additional-cost [:forfeit]
   :effect (req (let [bp-lost (max 0 (min (:agendapoints (last (:rfg corp)))
                                          (count-bad-pub state)))]
                  (system-msg state side (str "uses Sacrifice to lose " bp-lost " bad publicity and gain " bp-lost " [Credits]"))
                  (when (pos? bp-lost)
                    (lose-bad-publicity state side bp-lost)
                    (gain-credits state side bp-lost))))})

(define-card "Salem's Hospitality"
  {:prompt "Name a Runner card"
   :choices {:card-title (req (and (runner? target)
                                   (not (identity? target))))}
   :async true
   :effect (req (system-msg state side
                            (str "uses Salem's Hospitality to reveal the Runner's Grip ( "
                                 (join ", " (map :title (sort-by :title (:hand runner))))
                                 " ) and trash any copies of " target))
                (reveal state side (filter #(= target (:title %)) (:hand runner)))
                (let [cards (filter #(= target (:title %)) (:hand runner))]
                  (trash-cards state side eid cards {:unpreventable true})))})

(define-card "Scapenet"
  {:req (req (last-turn? state :runner :successful-run))
   :trace {:base 7
           :successful
           {:async true
            :prompt "Choose an installed virtual or chip card to remove from game"
            :choices {:card #(and (installed? %)
                                  (or (has-subtype? % "Virtual")
                                      (has-subtype? % "Chip")))}
            :msg (msg "remove " (card-str state target) " from game")
            :effect (effect (move :runner target :rfg)
                            (effect-completed eid))}}})

(define-card "Scarcity of Resources"
  {:msg "increase the install cost of resources by 2"
   :constant-effects [{:type :install-cost
                       :req (req (resource? target))
                       :value 2}]})

(define-card "Scorched Earth"
  {:req (req tagged)
   :async true
   :msg "do 4 meat damage"
   :effect (effect (damage eid :meat 4 {:card card}))})

(define-card "SEA Source"
  {:req (req (last-turn? state :runner :successful-run))
   :trace {:base 3
           :label "Trace 3 - Give the Runner 1 tag"
           :successful {:msg "give the Runner 1 tag"
                        :async true
                        :effect (effect (gain-tags :corp eid 1))}}})

(define-card "Secure and Protect"
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
                                           :prompt (str "Select where to install " (:title chosen-ice))
                                           :choices ["Archives" "R&D" "HQ"]
                                           :msg (msg "reveal " (:title chosen-ice) " and install it, paying 3 [Credit] less")
                                           :effect (effect (clear-wait-prompt :runner)
                                                           (reveal state side chosen-ice)
                                                           (shuffle! :deck)
                                                           (corp-install eid chosen-ice target {:cost-bonus -3}))}
                                          card nil)))}
                        card nil))
                  (do (shuffle! state side :deck)
                      (effect-completed state side eid))))})

(define-card "Self-Growth Program"
  {:req (req tagged)
   :prompt "Select two installed Runner cards"
   :choices {:card #(and (installed? %)
                         (runner? %))
             :max 2}
   :msg (msg (str "move " (join ", " (map :title targets)) " to the Runner's grip"))
   :effect (req (doseq [c targets]
                  (move state :runner c :hand)))})

(define-card "Service Outage"
  {:msg "add a cost of 1 [Credit] for the Runner to make the first run each turn"
   :constant-effects [{:type :run-additional-cost
                       :req (req (no-event? state side :run))
                       :value [:credit 1]}]})

(define-card "Shipment from Kaguya"
  {:choices {:max 2
             :card #(and (corp? %)
                         (installed? %)
                         (can-be-advanced? %))}
   :msg (msg "place 1 advancement token on " (count targets) " cards")
   :effect (req (doseq [t targets]
                  (add-prop state :corp t :advance-counter 1 {:placed true})))})

(define-card "Shipment from MirrorMorph"
  (letfn [(shelper [n]
            (when (< n 3)
              {:async true
               :prompt "Select a card to install with Shipment from MirrorMorph"
               :choices {:card #(and (corp? %)
                                     (not (operation? %))
                                     (in-hand? %))}
               :effect (req (wait-for (corp-install state side target nil nil)
                                      (continue-ability state side (shelper (inc n)) card nil)))}))]
    {:async true
     :effect (effect (continue-ability (shelper 0) card nil))}))

(define-card "Shipment from SanSan"
  {:choices ["0" "1" "2"]
   :prompt "How many advancement tokens?"
   :async true
   :effect (req (let [c (str->int target)]
                  (continue-ability
                    state side
                    {:choices {:card can-be-advanced?}
                     :msg (msg "place " c " advancement tokens on " (card-str state target))
                     :effect (effect (add-prop :corp target :advance-counter c {:placed true}))}
                    card nil)))})

(define-card "Shipment from Tennin"
  {:req (req (not-last-turn? state :runner :successful-run))
   :choices {:card #(and (corp? %)
                         (installed? %))}
   :msg (msg "place 2 advancement tokens on " (card-str state target))
   :effect (effect (add-prop target :advance-counter 2 {:placed true}))})

(define-card "Shoot the Moon"
  (letfn [(rez-helper [ice]
            (when (seq ice)
              {:async true
               :effect (req (wait-for (rez state side (first ice) {:ignore-cost :all-costs})
                                      (continue-ability state side (rez-helper (rest ice)) card nil)))}))]
    {:req (req tagged)
     :choices {:card #(and (ice? %)
                        (not (rezzed? %)))
               :max (req (min (count-tags state)
                              (reduce (fn [c server]
                                        (+ c (count (filter #(not (:rezzed %)) (:ices server)))))
                                      0 (flatten (seq (:servers corp))))))}
     :effect (effect (continue-ability (rez-helper targets) card nil))}))

(define-card "Snatch and Grab"
  {:trace {:base 3
           :successful
           {:msg "trash a connection"
            :choices {:card #(has-subtype? % "Connection")}
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
                             card nil)))}}})

(define-card "Special Report"
  {:prompt "Select any number of cards in HQ to shuffle into R&D"
   :choices {:max (req (count (:hand corp)))
             :card #(and (corp? %)
                         (in-hand? %))}
   :msg (msg "shuffle " (count targets) " cards in HQ into R&D and draw " (count targets) " cards")
   :async true
   :effect (req (doseq [c targets]
                  (move state side c :deck))
                (shuffle! state side :deck)
                (draw state side eid (count targets) nil))})

(define-card "Standard Procedure"
  {:req (req (last-turn? state :runner :successful-run))
   :prompt "Choose a card type"
   :choices ["Event" "Hardware" "Program" "Resource"]
   :msg (msg "name " target
             ", revealing " (join ", " (map :title (:hand runner)))
             " in the Runner's Grip, and gains "
             (* 2 (count (filter #(is-type? % target) (:hand runner)))) " [Credits]")
   :effect (effect (reveal (:hand runner))
                   (gain-credits :corp (* 2 (count (filter #(is-type? % target) (:hand runner))))))})

(define-card "Stock Buy-Back"
  {:msg (msg "gain " (* 3 (count (:scored runner))) " [Credits]")
   :effect (effect (gain-credits (* 3 (count (:scored runner)))))})

(define-card "Sub Boost"
  (let [new-sub {:label "[Sub Boost]: End the run"}]
    {:sub-effect {:label "End the run"
                  :msg "end the run"
                  :effect (effect (end-run eid card))}
     :choices {:card #(and (ice? %)
                           (rezzed? %))}
     :msg (msg "make " (card-str state target) " gain Barrier and \"[Subroutine] End the run\"")
     :effect (req (update! state side (assoc target :subtype (combine-subtypes true (:subtype target) "Barrier")))
                  (add-extra-sub! state :corp (get-card state target) new-sub (:cid card))
                  (update-ice-strength state side target)
                  (host state side (get-card state target) (assoc card :seen true :condition true)))
     :leave-play (req (remove-extra-subs! state :corp (:host card) (:cid card)))
     :events [{:event :rez
               :req (req (same-card? target (:host card)))
               :effect (req (add-extra-sub! state :corp (get-card state target) new-sub (:cid card)))}]}))

(define-card "Subcontract"
  (letfn [(sc [i sccard]
            {:prompt "Select an operation in HQ to play"
             :choices {:card #(and (corp? %)
                                   (operation? %)
                                   (in-hand? %))}
             :async true
             :msg (msg "play " (:title target))
             :effect (req (wait-for (play-instant state side target nil)
                                    (if (and (not (get-in @state [:corp :register :terminal])) (< i 2))
                                      (continue-ability state side (sc (inc i) sccard) sccard nil)
                                      (effect-completed state side eid))))})]
    {:req (req tagged)
     :async true
     :effect (effect (continue-ability (sc 1 card) card nil))}))

(define-card "Subliminal Messaging"
  {:msg "gain 1 [Credits]"
   :effect (effect (gain-credits 1)
                   (continue-ability
                     {:once :per-turn
                      :once-key :subliminal-messaging
                      :msg "gain [Click]"
                      :effect (effect (gain :corp :click 1))}
                     card nil))
   :events [{:event :corp-phase-12
             :location :discard
             :optional
             {:req (req (not-last-turn? state :runner :made-run))
              :prompt "Add Subliminal Messaging to HQ?"
              :yes-ability
              {:msg "add Subliminal Messaging to HQ"
               :effect (effect (move card :hand))}}}]})

(define-card "Success"
  {:additional-cost [:forfeit]
   :effect (req (resolve-ability state side
                                 {:choices {:card can-be-advanced?}
                                  :msg (msg "advance " (card-str state target) " "
                                            (advancement-cost state side (last (:rfg corp))) " times")
                                  :effect (req (dotimes [_ (advancement-cost state side (last (:rfg corp)))]
                                                 (advance state :corp (make-eid state {:source card :source-type :advance}) target :no-cost)))} card nil))})

(define-card "Successful Demonstration"
  {:req (req (last-turn? state :runner :unsuccessful-run))
   :msg "gain 7 [Credits]"
   :effect (effect (gain-credits 7))})

(define-card "Sunset"
  (letfn [(sun [serv]
            {:prompt "Select two pieces of ICE to swap positions"
             :choices {:card #(and (= serv (rest (butlast (:zone %))))
                                   (ice? %))
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
     :effect (req (let [serv (rest (server->zone state target))]
                    (continue-ability state side (sun serv) card nil)))}))

(define-card "Surveillance Sweep"
  {:events [{:event :pre-init-trace
             :req (req run)
             :effect (req (swap! state assoc-in [:trace :player] :runner))}]})

(define-card "Sweeps Week"
  {:effect (effect (gain-credits (count (:hand runner))))
   :msg (msg "gain " (count (:hand runner)) " [Credits]")})

(define-card "SYNC Rerouting"
  {:trash-after-resolving false
   :events [{:event :run
             :async true
             :msg (msg "force the Runner to " (decapitalize target))
             :player :runner
             :prompt "Pay 4 [Credits] or take 1 tag?"
             :choices ["Pay 4 [Credits]" "Take 1 tag"]
             :effect (req (if (= target "Pay 4 [Credits]")
                            (pay-sync state :runner eid card :credit 4)
                            (gain-tags state :corp eid 1 nil)))}
            {:event :corp-turn-begins
             :async true
             :effect (effect (trash eid card nil))}]})

(define-card "Targeted Marketing"
  (let [gaincr {:req (req (= (:title target) (get-in card [:special :marketing-target])))
                :effect (effect (gain-credits :corp 10))
                :msg (msg "gain 10 [Credits] from " (:marketing-target card))}]
    {:prompt "Name a Runner card"
     :choices {:card-title (req (and (runner? target)
                                     (not (identity? target))))}
     :effect (effect (update! (assoc-in card [:special :marketing-target] target))
                     (system-msg (str "uses Targeted Marketing to name " target)))
     :events [(assoc gaincr :event :runner-install)
              (assoc gaincr :event :play-event)]}))

(define-card "The All-Seeing I"
  (let [trash-all-resources
        {:msg "trash all resources"
         :async true
         :effect (effect (trash-cards :corp eid (filter resource? (all-active-installed state :runner))))}]
    {:req (req tagged)
     :async true
     :effect (effect
               (continue-ability
                 (if (pos? (count-bad-pub state))
                   {:optional
                    {:player :runner
                     :prompt "Remove 1 bad publicity to prevent all resources from being trashed?"
                     :yes-ability
                     {:msg "remove 1 bad publicity, preventing all resources from being trashed"
                      :async true
                      :effect (effect (lose-bad-publicity eid 1))}
                     :no-ability trash-all-resources}}
                   trash-all-resources)
                 card nil))}))

(define-card "Threat Assessment"
  {:req (req (last-turn? state :runner :trashed-card))
   :prompt "Select an installed Runner card"
   :choices {:card #(and (runner? %)
                         (installed? %))}
   :rfg-instead-of-trashing true
   :async true
   :effect (effect (show-wait-prompt "Runner to resolve Threat Assessment")
                   (continue-ability
                     :runner
                     (let [chosen target]
                       {:prompt (str "Add " (:title chosen) " to the top of the Stack or take 2 tags?")
                        :choices [(str "Move " (:title chosen))
                                  "Take 2 tags"]
                        :async true
                        :effect (req (clear-wait-prompt state :corp)
                                     (if (= target "Take 2 tags")
                                       (do (system-msg state side "chooses to take 2 tags")
                                           (gain-tags state :runner eid 2))
                                       (do (system-msg state side (str "chooses to move " (:title chosen) " to the Stack"))
                                           (move state :runner chosen :deck {:front true})
                                           (effect-completed state side eid))))})
                     card nil))})

(define-card "Threat Level Alpha"
  {:trace {:base 1
           :successful
           {:label "Give the Runner X tags"
            :async true
            :effect (req (let [tags (max 1 (count-tags state))]
                           (gain-tags state :corp eid tags)
                           (system-msg
                             state side
                             (str "uses Threat Level Alpha to give the Runner " (quantify tags "tag")))))}}})

(define-card "Too Big to Fail"
  {:req (req (< (:credit corp) 10))
   :msg "gain 7 [Credits] and take 1 bad publicity"
   :effect (effect (gain-credits 7)
                   (gain-bad-publicity :corp 1))})

(define-card "Traffic Accident"
  {:req (req (>= (count-tags state) 2))
   :msg "do 2 meat damage"
   :async true
   :effect (effect (damage eid :meat 2 {:card card}))})

(define-card "Transparency Initiative"
  {:choices {:card #(and (agenda? %)
                         (installed? %)
                         (not (faceup? %)))}
   :effect (effect (update! (assoc target
                                   :seen true
                                   :rezzed true
                                   :subtype (combine-subtypes false (:subtype target) "Public")))
                   (host (get-card state target) (assoc card :seen true :condition true))
                   (register-events
                     target
                     [{:event :advance
                       :req (req (= (:hosted card) (:hosted target)))
                       :effect (effect (gain-credits 1)
                                       (system-msg
                                         (str "uses Transparency Initiative to gain 1 [Credit]")))}]))})

(define-card "Trick of Light"
  {:async true
   :req (req (let [advanceable (some can-be-advanced? (get-all-installed state))
                   advanced (some #(get-counters % :advancement) (get-all-installed state))]
               (and advanceable advanced)))
   :choices {:card #(and (pos? (get-counters % :advancement))
                         (installed? %))}
   :effect (effect
             (continue-ability
               (let [fr target]
                 {:async true
                  :prompt "Move how many advancement tokens?"
                  :choices (take (inc (get-counters fr :advancement)) ["0" "1" "2"])
                  :effect (effect
                            (continue-ability
                              (let [c (str->int target)]
                                {:prompt "Move to where?"
                                 :choices {:card #(and (not (same-card? fr %))
                                                       (can-be-advanced? %))}
                                 :msg (msg "move " c " advancement tokens from "
                                           (card-str state fr) " to " (card-str state target))
                                 :effect (effect (add-prop :corp target :advance-counter c {:placed true})
                                                 (add-prop :corp fr :advance-counter (- c) {:placed true}))})
                              card nil))})
               card nil))})

(define-card "Trojan Horse"
  {:req (req (:accessed-cards runner-reg-last))
   :trace {:base 4
           :label "Trace 4 - Trash a program"
           :successful {:async true
                        :effect (req (let [exceed (- target (second targets))]
                                       (continue-ability
                                         state side
                                         {:async true
                                          :prompt (str "Select a program with an install cost of no more than "
                                                       exceed "[Credits]")
                                          :choices {:card #(and (program? %)
                                                                (installed? %)
                                                                (>= exceed (:cost %)))}
                                          :msg (msg "trash " (card-str state target))
                                          :effect (effect (trash eid target nil))}
                                         card nil)))}}})

(define-card "Ultraviolet Clearance"
  {:async true
   :effect (req (gain-credits state side 10)
                (wait-for (draw state side 4 nil)
                          (continue-ability
                            state side
                            {:async true
                             :prompt "Choose a card in HQ to install"
                             :choices {:card #(and (in-hand? %)
                                                   (corp? %)
                                                   (not (operation? %)))}
                             :msg "gain 10 [Credits], draw 4 cards, and install 1 card from HQ"
                             :cancel-effect (req (effect-completed state side eid))
                             :effect (effect (corp-install eid target nil nil))}
                            card nil)))})

(define-card "Under the Bus"
  {:req (req (and (last-turn? state :runner :accessed-cards)
                  (not-empty (filter
                               #(and (resource? %)
                                     (has-subtype? % "Connection"))
                               (all-active-installed state :runner)))))
   :prompt "Choose a connection to trash"
   :choices {:card #(and (runner? %)
                         (resource? %)
                         (has-subtype? % "Connection")
                         (installed? %))}
   :msg (msg "trash " (:title target) " and take 1 bad publicity")
   :async true
   :effect (req (wait-for (trash state side target nil)
                          (gain-bad-publicity state :corp eid 1)))})

(define-card "Violet Level Clearance"
  {:msg "gain 8 [Credits] and draw 4 cards"
   :async true
   :effect (effect (gain-credits 8)
                   (draw eid 4 nil))})

(define-card "Voter Intimidation"
  {:req (req (seq (:scored runner)))
   :psi {:not-equal {:player :corp
                     :async true
                     :prompt "Select a resource to trash"
                     :choices {:card #(and (installed? %)
                                           (resource? %))}
                     :msg (msg "trash " (:title target))
                     :effect (effect (trash eid target nil))}}})

(define-card "Wake Up Call"
  {:async true
   :rfg-instead-of-trashing true
   :req (req (last-turn? state :runner :trashed-card))
   :prompt "Select a piece of hardware or non-virtual resource"
   :choices {:card #(or (hardware? %)
                        (and (resource? %)
                             (not (has-subtype? % "Virtual"))))}
   :effect (effect (show-wait-prompt "Runner to resolve Wake Up Call")
                   (continue-ability
                     :runner
                     (let [chosen target
                           wake card]
                       {:prompt (str "Trash " (:title chosen) " or suffer 4 meat damage?")
                        :choices [(str "Trash " (:title chosen))
                                  "4 meat damage"]
                        :async true
                        :effect (req (clear-wait-prompt state :corp)
                                     (if (= target "4 meat damage")
                                       (do (system-msg state side "chooses to suffer meat damage")
                                           (damage state side eid :meat 4 {:card wake
                                                                           :unboostable true}))
                                       (do (system-msg state side (str "chooses to trash " (:title chosen)))
                                           (trash state side eid chosen nil))))})
                     card nil))})

(define-card "Wetwork Refit"
  (let [new-sub {:label "[Wetwork Refit] Do 1 brain damage"}]
    {:choices {:card #(and (ice? %)
                           (has-subtype? % "Bioroid")
                           (rezzed? %))}
     :msg (msg "give " (card-str state target) " \"[Subroutine] Do 1 brain damage\" before all its other subroutines")
     :sub-effect (do-brain-damage 1)
     :effect (req (add-extra-sub! state :corp target new-sub (:cid card) {:front true})
                  (host state side (get-card state target) (assoc card :seen true :condition true)))
     :leave-play (req (remove-extra-subs! state :corp (:host card) (:cid card)))
     :events [{:event :rez
               :req (req (same-card? target (:host card)))
               :effect (req (add-extra-sub! state :corp (get-card state target) new-sub (:cid card) {:front true}))}]}))

(define-card "Witness Tampering"
  {:msg "remove 2 bad publicity"
   :effect (effect (lose-bad-publicity 2))})
