(ns game.cards.operations
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [game.core.access :refer [access-card steal-cost-bonus]]
   [game.core.actions :refer [advance score]]
   [game.core.bad-publicity :refer [gain-bad-publicity lose-bad-publicity]]
   [game.core.board :refer [all-active-installed all-installed
                            get-all-installed get-remote-names get-remotes
                            installable-servers server->zone]]
   [game.core.card :refer [agenda? asset? can-be-advanced? card-index corp? corp-installable-type?
                           event? facedown? faceup? get-advancement-requirement
                           get-card get-counters get-zone hardware? has-subtype? ice? identity? in-discard?
                           in-hand? installed? is-type? operation? program? resource? rezzed? runner?
                           upgrade?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.cost-fns :refer [play-cost trash-cost]]
   [game.core.costs :refer [total-available-credits]]
   [game.core.damage :refer [damage damage-bonus]]
   [game.core.def-helpers :refer [corp-recur defcard do-brain-damage
                                  reorder-choice]]
   [game.core.drawing :refer [draw]]
   [game.core.effects :refer [register-floating-effect]]
   [game.core.eid :refer [effect-completed make-eid make-result]]
   [game.core.engine :refer [pay register-events resolve-ability]]
   [game.core.events :refer [first-event? last-turn? no-event? not-last-turn?
                             turn-events]]
   [game.core.flags :refer [can-score? clear-persistent-flag! in-corp-scored?
                            in-runner-scored? is-scored? prevent-jack-out
                            register-persistent-flag! register-turn-flag! when-scored? zone-locked?]]
   [game.core.gaining :refer [gain-clicks gain-credits lose-clicks
                              lose-credits]]
   [game.core.hand-size :refer [runner-hand-size+]]
   [game.core.ice :refer [add-extra-sub! remove-extra-subs! update-all-ice]]
   [game.core.identities :refer [disable-identity enable-identity]]
   [game.core.initializing :refer [ability-init card-init]]
   [game.core.installing :refer [corp-install corp-install-list
                                 corp-install-msg install-as-condition-counter]]
   [game.core.memory :refer [mu+ update-mu]]
   [game.core.moving :refer [as-agenda mill move swap-agendas swap-ice trash
                             trash-cards]]
   [game.core.optional :refer [get-autoresolve set-autoresolve]]
   [game.core.payment :refer [can-pay? cost-target]]
   [game.core.play-instants :refer [play-instant]]
   [game.core.prompts :refer [cancellable clear-wait-prompt show-wait-prompt]]
   [game.core.props :refer [add-counter add-prop]]
   [game.core.purging :refer [purge]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [derez rez]]
   [game.core.runs :refer [end-run make-run]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [is-remote? remote->name zone->name]]
   [game.core.shuffling :refer [shuffle! shuffle-into-deck
                                shuffle-into-rd-effect]]
   [game.core.tags :refer [gain-tags]]
   [game.core.to-string :refer [card-str]]
   [game.core.toasts :refer [toast]]
   [game.core.update :refer [update!]]
   [game.core.virus :refer [number-of-virus-counters]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]))


(defn- lockdown
  ;; Helper function for lockdowns. Enforces the "cannot play if there's another active lockdown"
  ;; restriction, and handles the card staying in the play area/trashing at the start of the corp
  ;; turn.
  [cardfn]
  (let [untrashed (assoc cardfn :on-play (conj {:trash-after-resolving false
                                                :req (req (not (some #(has-subtype? % "Lockdown")
                                                                     (:play-area corp))))}
                                               (:on-play cardfn)))]
    (update untrashed :events conj {:event :corp-turn-begins
                                    :async true
                                    :effect (effect (trash eid card nil))})))

;; Card definitions
(defcard "24/7 News Cycle"
  {:on-play
   {:req (req (pos? (count (:scored corp))))
    :additional-cost [:forfeit]
    :async true
    :effect (effect
              (continue-ability
                {:prompt "Choose an agenda in your score area"
                 :choices {:card #(and (agenda? %)
                                       (when-scored? %)
                                       (is-scored? state :corp %))}
                 :msg (msg "trigger the \"when scored\" ability of " (:title target))
                 :async true
                 :effect (effect (continue-ability (:on-score (card-def target)) target nil))}
                card nil))}})

(defcard "Accelerated Diagnostics"
  (letfn [(ad [st si e c cards]
            (when-let [cards (filterv #(and (operation? %)
                                            (can-pay? st si (assoc e :source c :source-type :play)
                                                      c nil [:credit (play-cost st si %)]))
                                      cards)]
              {:async true
               :prompt "Choose an operation to play"
               :choices (cancellable cards)
               :msg (msg "play " (:title target))
               :effect (req (wait-for (play-instant state side target {:no-additional-cost true})
                                      (let [cards (filterv #(not (same-card? % target)) cards)]
                                        (continue-ability state side (ad state side eid card cards) card nil))))}))]
    {:on-play
     {:prompt (msg "The top cards of R&D are " (str/join ", " (map :title (take 3 (:deck corp)))))
      :choices ["OK"]
      :async true
      :effect (effect (continue-ability (ad state side eid card (take 3 (:deck corp))) card nil))}}))

(defcard "Ad Blitz"
  (letfn [(ab [n total]
            (when (< n total)
              {:async true
               :show-discard true
               :prompt "Choose an Advertisement to install and rez"
               :choices {:card #(and (corp? %)
                                     (has-subtype? % "Advertisement")
                                     (or (in-hand? %)
                                         (in-discard? %)))}
               :effect (req (wait-for (corp-install state side target nil {:install-state :rezzed})
                                      (continue-ability state side (ab (inc n) total) card nil)))}))]
    {:on-play
     {:req (req (some #(has-subtype? % "Advertisement")
                      (concat (:discard corp) (:hand corp))))
      :prompt "How many Advertisements do you want to install and rez?"
      :choices :credit
      :msg (msg "install and rez " target " Advertisements")
      :async true
      :effect (effect (continue-ability (ab 0 target) card nil))}}))

(defcard "Aggressive Negotiation"
  {:on-play
   {:req (req (:scored-agenda corp-reg))
    :prompt "Choose a card"
    :choices (req (cancellable (:deck corp) :sorted))
    :msg "search R&D for a card and add it to HQ"
    :effect (effect (move target :hand)
                    (shuffle! :deck))}})

(defcard "An Offer You Can't Refuse"
  {:on-play
   {:async true
    :prompt "Choose a server"
    :choices ["Archives" "R&D" "HQ"]
    :effect
    (effect (show-wait-prompt (str "Runner to decide on running " target))
            (continue-ability
              (let [serv target]
                {:optional
                 {:prompt (str "Make a run on " serv "?")
                  :player :runner
                  :yes-ability {:msg (str "let the Runner make a run on " serv)
                                :async true
                                :effect (effect (clear-wait-prompt :corp)
                                                (make-run eid serv card)
                                                (prevent-jack-out))}
                  :no-ability {:msg "add itself to their score area as an agenda worth 1 agenda point"
                               :effect (effect (clear-wait-prompt :corp)
                                               (as-agenda :corp card 1))}}})
              card nil))}})

(defcard "Anonymous Tip"
  {:on-play
   {:msg "draw 3 cards"
    :async true
    :effect (effect (draw eid 3))}})

(defcard "Archived Memories"
  {:on-play (corp-recur)})

(defcard "Argus Crackdown"
  (lockdown
  {:events [{:event :successful-run
             :req (req (not-empty run-ices))
             :msg "deal 2 meat damage"
             :async true
             :effect (effect (damage eid :meat 2 {:card card}))}]}))

(defcard "Ark Lockdown"
  {:on-play
   {:req (req (and (not-empty (:discard runner))
                   (not (zone-locked? state :runner :discard))))
    :prompt "Name a card to remove all copies in the Heap from the game"
    :choices (req (cancellable (:discard runner) :sorted))
    :msg (msg "remove all copies of " (:title target) " in the Heap from the game")
    :async true
    :effect (req (doseq [c (filter #(same-card? :title target %) (:discard runner))]
                   (move state :runner c :rfg))
                 (effect-completed state side eid))}})

(defcard "Attitude Adjustment"
  {:on-play
   {:async true
    :effect
    (req (wait-for
           (draw state side 2)
           (continue-ability
             state side
             {:prompt "Choose up to 2 agendas in HQ or Archives"
              :choices {:max 2
                        :card #(and (corp? %)
                                    (agenda? %)
                                    (or (in-hand? %)
                                        (in-discard? %)))}
              :async true
              :show-discard true
              :effect
              (req (wait-for
                     (reveal state side targets)
                     (wait-for
                       (gain-credits state side (* 2 (count targets)))
                       (doseq [c targets]
                         (move state :corp c :deck))
                       (shuffle! state :corp :deck)
                       (let [from-hq (map :title (filter in-hand? targets))
                             from-archives (map :title (filter in-discard? targets))]
                         (system-msg
                           state side
                           (str "uses Attitude Adjustment to shuffle "
                                (str/join
                                  " and "
                                  (filter identity
                                          [(when (not-empty from-hq)
                                             (str (str/join " and " from-hq)
                                                  " from HQ"))
                                           (when (not-empty from-archives)
                                             (str (str/join " and " from-archives)
                                                  " from Archives"))]))
                                " into R&D and gain "
                                (* 2 (count targets)) " [Credits]")))
                       (effect-completed state side eid))))}
             card nil)))}})

(defcard "Audacity"
  (letfn [(audacity [x]
            {:prompt (msg "Choose a card that can be advanced to place advancement counters on (" x " remaining)")
             :async true
             :choices {:card can-be-advanced?
                       :all true}
             :msg (msg "place 1 advancement counter on " (card-str state target))
             :effect (req (wait-for (add-prop state side target :advance-counter 1 {:placed true})
                                    (if (> x 1)
                                      (continue-ability state side (audacity (dec x)) card nil)
                                      (effect-completed state side eid))))})]
    {:on-play
     {:req (req (and (<= 3 (count (:hand corp)))
                     (some can-be-advanced? (all-installed state :corp))))
      :async true
      :msg "trash all cards in HQ"
      :effect (req (wait-for (trash-cards state side (:hand corp) {:unpreventable true :cause-card card})
                             (continue-ability state side (audacity 2) card nil)))}}))

(defcard "Back Channels"
  {:on-play
   {:prompt "Choose an installed card in a server to trash"
    :choices {:card #(and (= (last (get-zone %)) :content)
                          (is-remote? (second (get-zone %))))}
    :msg (msg "trash " (card-str state target) " and gain "
              (* 3 (get-counters target :advancement)) " [Credits]")
    :async true
    :effect (req (wait-for (gain-credits state side (* 3 (get-counters target :advancement)))
                           (trash state side eid target {:cause-card card})))}})

(defcard "Backroom Machinations"
  {:on-play
   {:additional-cost [:tag 1]
    :msg "add it to their score area as an agenda worth 1 agenda point"
    :effect (req (as-agenda state :corp card 1))}})

(defcard "Bad Times"
  {:implementation "Any required program trashing is manual"
   :on-play {:req (req tagged)
             :msg "force the Runner to lose 2[mu] until the end of the turn"
             :effect (req (register-floating-effect
                            state :corp card
                            (assoc (mu+ -2) :duration :end-of-turn))
                          (update-mu state))}})

(defcard "Beanstalk Royalties"
  {:on-play
   {:msg "gain 3 [Credits]"
    :async true
    :effect (effect (gain-credits eid 3))}})

(defcard "Best Defense"
  {:on-play
   {:req (req (not-empty (all-installed state :runner)))
    :prompt (msg "Choose a Runner card with an install cost of " (count-tags state) " or less to trash")
    :choices {:req (req (and (runner? target)
                             (installed? target)
                             (not (facedown? target))
                             (<= (:cost target) (count-tags state))))}
    :msg (msg "trash " (:title target))
    :async true
    :effect (effect (trash eid target {:cause-card card}))}})

(defcard "Biased Reporting"
  (letfn [(num-installed [state t]
            (count (filter #(is-type? % t) (all-active-installed state :runner))))]
    {:on-play
     {:req (req (not-empty (all-active-installed state :runner)))
      :prompt "Choose one"
      :choices ["Hardware" "Program" "Resource"]
      :async true
      :msg (msg "choose " target)
      :effect (req (let [t target
                         n (num-installed state t)]
                     (wait-for
                       (resolve-ability
                         state :runner
                         {:waiting-prompt true
                          :prompt (msg "Choose any number of cards of type " t " to trash")
                          :choices {:max n
                                    :card #(and (installed? %)
                                                (is-type? % t))}
                          :async true
                          :effect
                          (req (wait-for
                                 (trash-cards state :runner targets {:unpreventable true :cause-card card :cause :forced-to-trash})
                                 (let [trashed-cards async-result]
                                   (wait-for
                                     (gain-credits state :runner (count trashed-cards))
                                     (system-msg state :runner
                                                 (str "trashes " (str/join ", " (map :title trashed-cards))
                                                      " and gains " (count trashed-cards)
                                                      " [Credits]"))
                                     (effect-completed state side eid)))))}
                         card nil)
                       (let [n (* 2 (num-installed state t))]
                         (if (pos? n)
                           (do (system-msg state :corp (str "uses Biased Reporting to gain " n " [Credits]"))
                               (gain-credits state :corp eid n))
                           (effect-completed state side eid))))))}}))

(defcard "Big Brother"
  {:on-play
   {:req (req tagged)
    :msg "give the Runner 2 tags"
    :async true
    :effect (effect (gain-tags :corp eid 2))}})

(defcard "Big Deal"
  {:on-play
   {:req (req (pos? (count (all-installed state :corp))))
    :prompt "Choose a card on which to place 4 advancement counters"
    :rfg-instead-of-trashing true
    :async true
    :choices {:card #(and (corp? %)
                          (installed? %))}
    :msg (msg "place 4 advancement counters on " (card-str state target))
    :effect (req (wait-for (add-prop state :corp target :advance-counter 4 {:placed true})
                           (let [card-to-score target]
                             (continue-ability
                               state side
                               {:optional
                                {:req (req (can-score? state side (get-card state card-to-score)))
                                 :prompt (str "Score " (:title card-to-score) "?")
                                 :yes-ability {:async true
                                               :effect (effect (score eid (get-card state card-to-score)))}
                                 :no-ability {:effect (effect (system-msg (str "declines to use Big Deal to score " (card-str state card-to-score))))}}}
                               card nil))))}})

(defcard "Bioroid Efficiency Research"
  {:on-play {:req (req (some #(and (ice? %)
                                   (has-subtype? % "Bioroid")
                                   (not (rezzed? %)))
                             (all-installed state :corp)))
             :choices {:card #(and (ice? %)
                                   (has-subtype? % "Bioroid")
                                   (installed? %)
                                   (not (rezzed? %)))}
             :msg (msg "rez " (card-str state target {:visible true}) " at no cost")
             :async true
             :effect (req (wait-for (rez state side target {:ignore-cost :all-costs})
                                    (install-as-condition-counter state side eid card (:card async-result))))}
   :events [{:event :end-of-encounter
             :condition :hosted
             :async true
             :req (req (and (same-card? (:ice context) (:host card))
                            (empty? (remove :broken (:subroutines (:ice context))))))
             :effect (effect (system-msg :corp
                                         (str "derezzes " (:title (:ice context))
                                              " and trashes Bioroid Efficiency Research"))
                             (derez :corp (:ice context))
                             (trash :corp eid card {:unpreventable true :cause-card card}))}]})

(defcard "Biotic Labor"
  {:on-play
   {:msg "gain [Click][Click]"
    :effect (effect (gain-clicks 2))}})

(defcard "Blue Level Clearance"
  {:on-play
   {:msg "gain 5 [Credits] and draw 2 cards"
    :async true
    :effect (req (wait-for (gain-credits state side 5)
                           (draw state side eid 2)))}})

(defcard "BOOM!"
  {:on-play
   {:req (req (<= 2 (count-tags state)))
    :msg "do 7 meat damage"
    :async true
    :effect (effect (damage eid :meat 7 {:card card}))}})

(defcard "Building Blocks"
  {:on-play
   {:req (req (pos? (count (filter #(has-subtype? % "Barrier") (:hand corp)))))
    :prompt "Choose a Barrier to install and rez"
    :choices {:card #(and (corp? %)
                          (has-subtype? % "Barrier")
                          (in-hand? %))}
    :msg (msg "reveal " (:title target))
    :async true
    :effect (req (wait-for
                   (reveal state side target)
                   (corp-install state side eid target nil {:ignore-all-cost true
                                                            :install-state :rezzed-no-cost})))}})

(defcard "Casting Call"
  {:on-play {:choices {:card #(and (agenda? %)
                                   (in-hand? %))}
             :async true
             :effect (req (wait-for
                            (corp-install state side target nil {:install-state :face-up})
                            (let [agenda async-result]
                              (system-msg state side (str "hosts Casting Call on " (:title agenda)))
                              (install-as-condition-counter state side eid card agenda))))}
   :events [{:event :access
             :condition :hosted
             :async true
             :req (req (same-card? target (:host card)))
             :msg "give the Runner 2 tags"
             :effect (effect (gain-tags :runner eid 2))}]})

(defcard "Celebrity Gift"
  {:on-play
   {:choices {:max 5
              :card #(and (corp? %)
                          (in-hand? %))}
    :msg (msg "reveal " (str/join ", " (map :title (sort-by :title targets))) " and gain " (* 2 (count targets)) " [Credits]")
    :async true
    :effect (req (wait-for
                   (reveal state side targets)
                   (gain-credits state side eid (* 2 (count targets)))))}})

(defcard "Cerebral Cast"
  {:on-play
   {:psi {:req (req (last-turn? state :runner :successful-run))
          :not-equal {:player :runner
                      :prompt "Choose one"
                      :waiting-prompt true
                      :choices ["Take 1 tag" "Suffer 1 brain damage"]
                      :msg (msg "force the Runner to " (decapitalize target))
                      :effect (req (if (= target "Take 1 tag")
                                     (gain-tags state :runner eid 1)
                                     (damage state side eid :brain 1 {:card card})))}}}})

(defcard "Cerebral Static"
  {:on-play {:msg "disable the Runner's identity"
             :effect (effect (disable-identity :runner))}
   :leave-play (effect (enable-identity :runner))})

(defcard "\"Clones are not People\""
  {:events [{:event :agenda-scored
             :msg "add it to their score area as an agenda worth 1 agenda point"
             :effect (req (as-agenda state :corp card 1))}]})

(defcard "Closed Accounts"
  {:on-play
   {:req (req tagged)
    :msg (msg "force the Runner to lose all " (:credit runner) " [Credits]")
    :async true
    :effect (effect (lose-credits :runner eid :all))}})

(defcard "Commercialization"
  {:on-play
   {:msg (msg "gain " (get-counters target :advancement) " [Credits]")
    :choices {:card #(and (ice? %)
                          (installed? %))}
    :async true
    :effect (effect (gain-credits eid (get-counters target :advancement)))}})

(defcard "Complete Image"
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
                                          cards (some #(when (same-card? (:card (first %)) card) (:cards-trashed (first %)))
                                                      (turn-events state :corp :damage))
                                          dmg (some #(when (= (:title %) target) %) cards)]
                                      (continue-ability state side (when (and should-continue dmg)
                                                                     (name-a-card))
                                                        card nil))))})]
    {:implementation "Doesn't work with Chronos Protocol: Selective Mind-mapping"
     :on-play {:async true
               :req (req (and (last-turn? state :runner :successful-run)
                              (<= 3 (:agenda-point runner))))
               :effect (effect (continue-ability (name-a-card) card nil))}}))

(defcard "Consulting Visit"
  {:on-play
   {:prompt  "Choose an Operation from R&D to play"
    :choices (req (cancellable
                    (filter #(and (operation? %)
                                  (<= (:cost %) (:credit corp)))
                            (:deck corp))
                    :sorted))
    :msg (msg "search R&D for " (:title target) " and play it")
    :async true
    :effect (effect (shuffle! :deck)
                    (system-msg "shuffles their deck")
                    (play-instant eid target nil))}})

(defcard "Corporate Shuffle"
  {:on-play
   {:msg "shuffle all cards in HQ into R&D and draw 5 cards"
    :async true
    :effect (effect (shuffle-into-deck :hand)
                    (draw eid 5))}})

(defcard "Cyberdex Trial"
  {:on-play
   {:msg "purge virus counters"
    :effect (effect (purge))}})

(defcard "Death and Taxes"
  {:implementation "Credit gain mandatory to save on wait-prompts, adjust credits manually if credit not wanted."
   :events [{:event :runner-install
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits :corp eid 1))}
            {:event :runner-trash
             :req (req (installed? (:card target)))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits :corp eid 1))}]})

(defcard "Dedication Ceremony"
  {:on-play
   {:prompt "Choose a faceup card"
    :choices {:card #(or (and (corp? %)
                              (installed? %)
                              (faceup? %))
                         (and (runner? %)
                              (or (installed? %)
                                  (:host %))
                              (not (facedown? %))))}
    :msg (msg "place 3 advancement tokens on " (card-str state target))
    :effect (effect (add-counter target :advancement 3 {:placed true})
                    (register-turn-flag!
                      target :can-score
                      (fn [state _ card]
                        (if (same-card? card target)
                          ((constantly false) (toast state :corp "Cannot score due to Dedication Ceremony." "warning"))
                          true))))}})

(defcard "Defective Brainchips"
  {:events [{:event :pre-damage
             :req (req (= target :brain))
             :msg "do 1 additional brain damage"
             :once :per-turn
             :effect (effect (damage-bonus :brain 1))}]})

(defcard "Digital Rights Management"
  {:on-play
   {:req (req (and (< 1 (:turn @state))
                   (not (some #{:hq} (:successful-run runner-reg-last)))))
    :prompt "Choose an Agenda"
    :choices (req (conj (vec (filter agenda? (:deck corp))) "None"))
    :msg (msg (if (= "None" target)
                "shuffle R&D"
                (str "add " (:title target) " to HQ and shuffle R&D")))
    :effect (let [end-effect (req (system-msg state side "can not score agendas for the remainder of the turn")
                                  (swap! state assoc-in [:corp :register :cannot-score]
                                         (filter agenda? (all-installed state :corp)))
                                  (register-events
                                    state side card
                                    [{:event :corp-install
                                      :duration :until-corp-turn-begins
                                      :async true
                                      :req (req (agenda? (:card context)))
                                      :effect (req
                                                (register-turn-flag!
                                                  state side
                                                  (:card context) :can-score
                                                  (fn [state _ card]
                                                    (if (same-card? card (:card context))
                                                      ((constantly false) (toast state :corp "Cannot score due to Digital Rights Management." "warning"))
                                                      true)))
                                                (effect-completed state side eid))}])
                                  (effect-completed state side eid))]
              (req (wait-for
                     (resolve-ability
                       state side
                       (when-not (= "None" target)
                         {:async true
                          :effect (req (wait-for (reveal state side target)
                                                 (move state side target :hand)
                                                 (effect-completed state side eid)))})
                       card targets)
                     (shuffle! state side :deck)
                     (continue-ability
                       state side
                       {:prompt "Choose a card in HQ to install"
                        :choices {:card #(and (in-hand? %)
                                              (corp? %)
                                              (not (operation? %)))}
                        :effect (req (wait-for (resolve-ability
                                                 state side
                                                 (let [card-to-install target]
                                                   {:prompt "Choose a server"
                                                    :choices (remove #{"HQ" "R&D" "Archives"} (corp-install-list state card-to-install))
                                                    :async true
                                                    :effect (effect (corp-install eid card-to-install target nil))})
                                                 target nil)
                                               (end-effect state side eid card targets)))
                        :cancel-effect (effect (system-msg "declines to use Digital Rights Management to install a card")
                                               (end-effect eid card targets))}
                       card nil))))}})

(defcard "Distract the Masses"
  (let [shuffle-two {:async true
                     :effect (effect (shuffle-into-rd-effect eid card 2))}
        trash-from-hq {:async true
                       :prompt "Choose up to 2 cards in HQ to trash"
                       :choices {:max 2
                                 :card #(and (corp? %)
                                             (in-hand? %))}
                       :msg (msg "trash " (quantify (count targets) "card") " from HQ")
                       :effect (req (wait-for
                                      (trash-cards state side targets {:cause-card card})
                                      (continue-ability state side shuffle-two card nil)))
                       :cancel-effect (effect (continue-ability shuffle-two card nil))}]
    {:on-play
     {:rfg-instead-of-trashing true
      :msg "give The Runner 2 [Credits]"
      :async true
      :effect (req (wait-for (gain-credits state :runner 2)
                             (continue-ability state side trash-from-hq card nil)))}}))

(defcard "Diversified Portfolio"
  (letfn [(number-of-non-empty-remotes [state]
            (count (filter seq (map #(:content (second %)) (get-remotes state)))))]
    {:on-play
     {:msg (msg "gain " (number-of-non-empty-remotes state)
             " [Credits]")
      :async true
      :effect (effect (gain-credits eid (number-of-non-empty-remotes state)))}}))

(defcard "Divert Power"
  {:on-play
   {:prompt "Choose any number of cards to derez"
    :choices {:card #(and (installed? %)
                          (rezzed? %))
              :max (req (count (filter rezzed? (all-installed state :corp))))}
    :async true
    :effect (req (doseq [c targets]
                   (derez state side c))
                 (let [discount (* -3 (count targets))]
                   (continue-ability
                     state side
                     {:async true
                      :prompt "Choose a card to rez"
                      :choices {:card #(and (installed? %)
                                            (corp? %)
                                            (not (rezzed? %))
                                            (not (agenda? %)))}
                      :effect (effect (rez eid target {:cost-bonus discount}))}
                     card nil)))}})

(defcard "Door to Door"
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

(defcard "Eavesdrop"
  {:on-play {:choices {:card #(and (ice? %)
                                   (installed? %))}
             :msg (msg "give " (card-str state target {:visible false}) " additional text")
             :async true
             :effect (effect (install-as-condition-counter eid card target))}
   :events [{:event :encounter-ice
             :condition :hosted
             :trace {:base 3
                     :req (req (same-card? current-ice (:host card)))
                     :successful {:msg "give the Runner 1 tag"
                                  :async true
                                  :effect (effect (gain-tags :runner eid 1))}}}]})

(defcard "Economic Warfare"
  {:on-play
   {:req (req (and (last-turn? state :runner :successful-run)
                   (>= (:credit runner) 4)))
    :msg "make the runner lose 4 [Credits]"
    :async true
    :effect (effect (lose-credits :runner eid 4))}})

(defcard "Election Day"
  {:on-play
   {:req (req (->> (get-in @state [:corp :hand])
                   (filter #(not (same-card? % card)))
                   count
                   pos?))
    :msg "trash all cards in HQ and draw 5 cards"
    :async true
    :effect (req (wait-for (trash-cards state side (get-in @state [:corp :hand]) {:cause-card card})
                           (draw state side eid 5)))}})

(defcard "Enforced Curfew"
  {:on-play {:msg "reduce the Runner's maximum hand size by 1"}
   :constant-effects [(runner-hand-size+ -1)]})

(defcard "Enforcing Loyalty"
  {:on-play
   {:trace
    {:base 3
     :label "Trash a card not matching the faction of the Runner's identity"
     :successful
     {:async true
      :prompt "Choose an installed card not matching the faction of the Runner's identity"
      :choices {:req (req (and (installed? target)
                               (runner? target)
                               (not= (:faction (:identity runner)) (:faction target))))}
      :msg (msg "trash " (:title target))
      :effect (effect (trash eid target {:cause-card card}))}}}})

(defcard "Enhanced Login Protocol"
  {:on-play {:msg (str "add an additional cost of [Click]"
                       " to make the first run not through a card ability this turn")}
   :constant-effects [{:type :run-additional-cost
                       :req (req (and (no-event? state side :run #(:click-run (:cost-args (first %))))
                                      (:click-run (second targets))))
                       :value [:click 1]}]})

(defcard "Exchange of Information"
  {:on-play
   {:req (req (and tagged
                   (seq (:scored runner))
                   (seq (:scored corp))))
    :prompt "Choose an agenda in the Runner's score area to swap"
    :choices {:req (req (in-runner-scored? state side target))}
    :async true
    :effect (effect
              (continue-ability
                (let [stolen target]
                  {:prompt (msg "Choose a scored agenda to swap for " (:title stolen))
                   :choices {:req (req (in-corp-scored? state side target))}
                   :msg (msg "swap " (:title target)
                             " for " (:title stolen))
                   :effect (effect (swap-agendas target stolen))})
                card nil))}})

(defcard "Extract"
  ;; doesn't check to see the card is actually trashed (it's not a cost, so may be prevented?)
  {:on-play
   {:async true
    :msg "gain 6 [Credit]"
    :effect (req (wait-for (gain-credits state side 6)
                           (continue-ability
                             state side
                             {:prompt "Choose an installed card to trash"
                              :req (req (not-empty (all-installed state :corp)))
                              :choices {:card #(and (installed? %)
                                                    (corp? %))}
                              :async true
                              :waiting-prompt true
                              :msg (msg "trash " (card-str state target) " and gain 3 [Credits]")
                              :cancel-effect (effect (system-msg "declines to use Extract to trash an installed card")
                                                     (effect-completed eid))
                              :effect (req (wait-for (trash state side target {:cause-card card})
                                                     (gain-credits state side eid 3)))}
                             card nil)))}})

(defcard "Fast Break"
  {:on-play
   {:req (req (-> runner :scored count pos?))
    :async true
    :effect
    (req (let [X (-> runner :scored count)
               draw {:async true
                     :prompt "How many cards do you want to draw?"
                     :choices {:number (req X)
                               :max (req X)
                               :default (req 1)}
                     :msg (msg "draw " (quantify target "card"))
                     :effect (effect (draw eid target))}
               install-cards (fn install-cards
                               [server n]
                               {:prompt "Choose a card to install"
                                :choices {:card #(and (corp? %)
                                                      (not (operation? %))
                                                      (in-hand? %)
                                                      (seq (filter (fn [c] (= server c)) (corp-install-list state %))))}
                                :effect (req (wait-for
                                               (corp-install state side target server nil)
                                               (let [server (remote->name (second (:zone async-result)))]
                                                 (if (< n X)
                                                   (continue-ability state side (install-cards server (inc n)) card nil)
                                                   (effect-completed state side eid)))))})
               select-server {:async true
                              :prompt "Choose a server"
                              :choices (req (conj (vec (get-remote-names state)) "New remote"))
                              :effect (effect (continue-ability (install-cards target 1) card nil))}]
           (wait-for (gain-credits state :corp X)
                     (wait-for (resolve-ability state side draw card nil)
                               (continue-ability state side select-server card nil)))))}})

(defcard "Fast Track"
  {:on-play
   {:prompt "Choose an Agenda"
    :choices (req (cancellable (filter agenda? (:deck corp)) :sorted))
    :async true
    :effect (req (system-msg state side (str "adds " (:title target) " to HQ and shuffle R&D"))
                 (wait-for (reveal state side target)
                           (shuffle! state side :deck)
                           (move state side target :hand)
                           (effect-completed state side eid)))}})

(defcard "Financial Collapse"
  (letfn [(count-resources [state]
            (* 2 (count (filter resource? (all-active-installed state :runner)))))]
    {:on-play
     {:optional
      {:req (req (and (<= 6 (:credit runner))
                      (pos? (count-resources state))))
       :player :runner
       :waiting-prompt true
       :prompt "Trash a resource?"
       :yes-ability
       {:prompt "Choose a resource to trash"
        :choices {:card #(and (resource? %)
                           (installed? %))}
        :async true
        :effect (effect (system-msg :runner
                                    (str "trashes " (:title target)
                                         " to prevent Financial Collapse"))
                  (trash :runner eid target {:unpreventable true :cause-card card :cause :forced-to-trash}))}
       :no-ability
       {:player :corp
        :async true
        :msg (msg "make the Runner lose " (count-resources state) " [Credits]")
        :effect (effect (lose-credits :runner eid (count-resources state)))}}}}))

(defcard "Focus Group"
  {:on-play
   {:req (req (last-turn? state :runner :successful-run))
    :prompt "Choose one"
    :choices ["Event" "Hardware" "Program" "Resource"]
    :async true
    :effect (req (let [type target
                       numtargets (count (filter #(= type (:type %)) (:hand runner)))]
                   (system-msg
                     state :corp
                     (str "uses Focus Group to choose " target
                          " and reveal the Runner's Grip ("
                          (str/join ", " (map :title (sort-by :title (:hand runner))))
                          ")"))
                   (wait-for
                     (reveal state side (:hand runner))
                     (continue-ability
                       state :corp
                       (when (pos? numtargets)
                         {:async true
                          :prompt "How many credits do you want to pay?"
                          :choices {:number (req numtargets)}
                          :effect (req (let [c target]
                                         (if (can-pay? state side (assoc eid :source card :source-type :ability) card (:title card) :credit c)
                                           (let [new-eid (make-eid state {:source card :source-type :ability})]
                                             (wait-for (pay state :corp new-eid card :credit c)
                                                       (when-let [payment-str (:msg async-result)]
                                                         (system-msg state :corp payment-str))
                                                       (continue-ability
                                                         state :corp
                                                         {:msg (msg "place " (quantify c " advancement token") " on "
                                                                    (card-str state target))
                                                          :choices {:card installed?}
                                                          :effect (effect (add-prop target :advance-counter c {:placed true}))}
                                                         card nil)))
                                           (effect-completed state side eid))))})
                       card nil))))}})

(defcard "Foxfire"
  {:on-play
   {:trace {:base 7
            :successful
            {:prompt "Choose 1 card to trash"
             :choices {:card #(and (installed? %)
                                   (or (has-subtype? % "Virtual")
                                       (has-subtype? % "Link")))}
             :msg (msg "trash " (card-str state target))
             :async true
             :effect (effect (trash eid target {:cause-card card}))}}}})

(defcard "Freelancer"
  {:on-play
   {:req (req tagged)
    :msg (msg "trash " (str/join ", " (map :title (sort-by :title targets))))
    :choices {:max 2
              :card #(and (installed? %)
                          (resource? %))}
    :async true
    :effect (effect (trash-cards :runner eid targets {:cause-card card}))}})

(defcard "Friends in High Places"
  (let [fhelper (fn fhp [n] {:prompt "Choose a card in Archives to install"
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
    {:on-play
     {:async true
      :effect (effect (continue-ability (fhelper 1) card nil))} }))

(defcard "Fully Operational"
  (letfn [(full-servers [state]
            (filter #(and (not-empty (:content %))
                          (not-empty (:ices %)))
                    (vals (get-remotes state))))
          (repeat-choice [current total]
            ;; if current > total, this ability will auto-resolve and finish the chain of async methods.
            (when (<= current total)
              {:async true
               :prompt (str "Choose one. Choice " current " of " total)
               :choices ["Gain 2 [Credits]" "Draw 2 cards"]
               :msg (msg (decapitalize target))
               :effect (req (if (= target "Gain 2 [Credits]")
                              (wait-for (gain-credits state :corp 2)
                                        (continue-ability state side (repeat-choice (inc current) total)
                                                          card nil))
                              (wait-for (draw state :corp 2) ; don't proceed with the next choice until the draw is done
                                        (continue-ability state side (repeat-choice (inc current) total)
                                                          card nil))))}))]
    {:on-play
     {:msg (msg "make " (quantify (inc (count (full-servers state))) "gain/draw decision"))
      :async true
      :effect (effect (continue-ability (repeat-choice 1 (inc (count (full-servers state))))
                                        card nil))}}))

(defcard "Game Changer"
  {:on-play
   {:rfg-instead-of-trashing true
    :effect (effect (gain-clicks (count (:scored runner))))}})

(defcard "Game Over"
  {:on-play
   {:req (req (last-turn? state :runner :stole-agenda))
    :prompt "Choose one"
    :choices ["Hardware" "Program" "Resource"]
    :async true
    :effect (req (let [card-type target
                       trashtargets (filter #(and (is-type? % card-type)
                                                  (not (has-subtype? % "Icebreaker")))
                                            (all-active-installed state :runner))
                       numtargets (count trashtargets)
                       typemsg (str (when (= card-type "Program") "non-Icebreaker ") card-type
                                    (when-not (= card-type "Hardware") "s"))]
                   (system-msg state :corp (str "chooses to trash all " typemsg))
                   (wait-for (resolve-ability
                               state :runner
                               {:async true
                                :req (req (<= 3 (:credit runner)))
                                :waiting-prompt true
                                :prompt (msg "Prevent any " typemsg " from being trashed? Pay 3 [Credits] per card")
                                :choices {:max (req (min numtargets (quot (total-available-credits state :runner eid card) 3)))
                                          :card #(and (installed? %)
                                                      (is-type? % card-type)
                                                      (not (has-subtype? % "Icebreaker")))}
                                :effect (req (wait-for (pay state :runner (make-eid state eid) card :credit (* 3 (count targets)))
                                                       (system-msg
                                                         state :runner
                                                         (str (:msg async-result) " to prevent the trashing of "
                                                              (str/join ", " (map :title (sort-by :title targets)))))
                                                       (effect-completed state side (make-result eid targets))))}
                               card nil)
                             (let [prevented async-result
                                   cids-to-trash (set/difference (set (map :cid trashtargets)) (set (map :cid prevented)))
                                   cards-to-trash (filter #(cids-to-trash (:cid %)) trashtargets)]
                               (when (not async-result)
                                 (system-msg state :runner (str "chooses to not prevent Corp trashing all " typemsg)))
                               (wait-for (trash-cards state side cards-to-trash {:cause-card card})
                                         (system-msg state :corp
                                                     (str "trashes all "
                                                          (when (seq prevented) "other ")
                                                          typemsg
                                                          ": " (str/join ", " (map :title (sort-by :title async-result)))))
                                         (wait-for (gain-bad-publicity state :corp 1)
                                                   (when async-result
                                                     (system-msg state :corp "takes 1 bad publicity from Game Over"))
                                                   (effect-completed state side eid)))))))}})

(defcard "Genotyping"
  {:on-play
   {:msg "trash the top 2 cards of R&D"
    :rfg-instead-of-trashing true
    :async true
    :effect (req (wait-for (mill state :corp :corp 2)
                           (shuffle-into-rd-effect state side eid card 4)))}})

(defcard "Government Subsidy"
  {:on-play
   {:msg "gain 15 [Credits]"
    :async true
    :effect (effect (gain-credits eid 15))}})

(defcard "Green Level Clearance"
  {:on-play
   {:msg "gain 3 [Credits] and draw 1 card"
    :async true
    :effect (req (wait-for (gain-credits state side 3)
                           (draw state side eid 1)))}})

(defcard "Hangeki"
  {:on-play
   {:req (req (last-turn? state :runner :trashed-card))
    :prompt "Choose an installed Corp card"
    :choices {:card #(and (corp? %)
                          (installed? %))}
    :async true
    :msg (msg "choose " (card-str state target))
    :effect
    (effect (continue-ability
              {:optional
               {:player :runner
                :async true
                :waiting-prompt true
                :prompt "Access the installed card?"
                :yes-ability
                {:async true
                 :effect (req (wait-for (access-card state side target)
                                        (update! state side (assoc card :rfg-instead-of-trashing true))
                                        (effect-completed state side eid)))}
                :no-ability
                {:msg "add itself to the Runner's score area as an agenda worth -1 agenda point"
                 :effect (effect (as-agenda :runner card -1))}}}
              card targets))}})

(defcard "Hansei Review"
  {:on-play
   {:async true
    :effect (req (if (pos? (count (:hand corp)))
                   (continue-ability
                     state :corp
                     {:prompt "Choose a card in HQ to trash"
                      :choices {:max 1
                                :all true
                                :card #(and (corp? %)
                                            (in-hand? %))}
                      :msg "trash a card from HQ and gain 10 [Credits]"
                      :async true
                      :effect (req (wait-for (trash-cards state side targets {:cause-card card})
                                             (gain-credits state side eid 10)))} card nil)
                   (do
                     (system-msg state side "uses Hansei Review to gain 10 [Credits]")
                     (gain-credits state side eid 10))))}})

(defcard "Hard-Hitting News"
  {:on-play
   {:trace {:base 4
            :req (req (last-turn? state :runner :made-run))
            :label "Give the Runner 4 tags"
            :successful
            {:async true
             :msg "give the Runner 4 tags"
             :effect (effect (gain-tags eid 4))}}}})

(defcard "Hasty Relocation"
  (letfn [(hr-final [chosen original]
            {:prompt (str "The top cards of R&D will be " (str/join  ", " (map :title chosen)))
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
    {:on-play
     {:additional-cost [:trash-from-deck 1]
      :msg "trash the top card of R&D, draw 3 cards, and add 3 cards in HQ to the top of R&D"
      :waiting-prompt true
      :async true
      :effect (req (wait-for (draw state side 3)
                             (let [from (get-in @state [:corp :hand])]
                               (continue-ability state :corp (hr-choice from '() 3 from) card nil))))}}))

(defcard "Hatchet Job"
  {:on-play
   {:trace {:base 5
            :successful
            {:choices {:card #(and (installed? %)
                                   (runner? %)
                                   (not (has-subtype? % "Virtual")))}
             :msg "add an installed non-virtual card to the Runner's Grip"
             :effect (effect (move :runner target :hand true))}}}})

(defcard "Hedge Fund"
  {:on-play
   {:msg "gain 9 [Credits]"
    :async true
    :effect (effect (gain-credits eid 9))}})

(defcard "Hellion Alpha Test"
  {:on-play
   {:trace
    {:base 2
     :req (req (last-turn? state :runner :installed-resource))
     :successful {:msg "add a Resource to the top of the Stack"
                  :choices {:card #(and (installed? %)
                                        (resource? %))}
                  :effect (effect (move :runner target :deck {:front true})
                                  (system-msg (str "adds " (:title target) " to the top of the Stack")))}
     :unsuccessful {:msg "take 1 bad publicity"
                    :effect (effect (gain-bad-publicity :corp 1))}}}})

(defcard "Hellion Beta Test"
  {:on-play
   {:trace
    {:base 2
     :req (req (last-turn? state :runner :trashed-card))
     :label "Trash 2 installed non-program cards or take 1 bad publicity"
     :successful {:choices {:max (req (min 2 (count (filter #(or (facedown? %)
                                                                 (not (program? %)))
                                                            (concat (all-installed state :corp)
                                                                    (all-installed state :runner))))))
                            :all true
                            :card #(and (installed? %)
                                        (not (program? %)))}
                  :msg (msg "trash " (str/join ", " (map :title (sort-by :title targets))))
                  :async true
                  :effect (effect (trash-cards eid targets {:cause-card card}))}
     :unsuccessful {:msg "take 1 bad publicity"
                    :async true
                    :effect (effect (gain-bad-publicity :corp eid 1))}}}})

(defcard "Heritage Committee"
  {:on-play
   {:async true
    :effect (req (wait-for (draw state side 3)
                           (continue-ability
                             state side
                             {:prompt "Choose a card in HQ to add to the top of R&D"
                              :choices {:card #(and (corp? %)
                                                    (in-hand? %))}
                              :msg "draw 3 cards and add 1 card from HQ to the top of R&D"
                              :effect (effect (move target :deck {:front true}))}
                             card nil)))}})

(defcard "High-Profile Target"
  (letfn [(dmg-count [state] (* 2 (count-tags state)))]
    {:on-play
     {:req (req tagged)
      :msg (msg "do " (dmg-count state) " meat damage")
      :async true
      :effect (effect (damage eid :meat (dmg-count state) {:card card}))}}))

(defcard "Housekeeping"
  {:events [{:event :runner-install
             :req (req (first-event? state side :runner-install))
             :player :runner
             :prompt "Choose a card from your grip to trash"
             :choices {:card #(and (runner? %)
                                   (in-hand? %))}
             :async true
             :msg (msg "force the Runner to trash"
                       (:title target) " from their grip")
             :effect (effect (trash :runner eid target {:unpreventable true :cause-card card :cause :forced-to-trash}))}]})

(defcard "Hunter Seeker"
  {:on-play
   {:req (req (last-turn? state :runner :stole-agenda))
    :prompt "Choose a card to trash"
    :choices {:card installed?}
    :msg (msg "trash " (card-str state target))
    :async true
    :effect (effect (trash eid target {:cause-card card}))}})

(defcard "Hyoubu Precog Manifold"
  (lockdown
   {:on-play {:prompt "Choose a server"
              :choices (req servers)
              :msg (msg "choose " target)
              :effect (effect (update! (assoc-in card [:special :hyoubu-precog-target] target)))}
    :events [{:event :successful-run
              :psi {:req (req (= (zone->name (get-in @state [:run :server]))
                                 (get-in card [:special :hyoubu-precog-target])))
                    :not-equal {:msg "end the run"
                                :async true
                                :effect (effect (end-run eid card))}}}]}))

(defcard "Interns"
  {:on-play
   {:prompt "Choose a card to install from Archives or HQ"
    :show-discard true
    :not-distinct true
    :choices {:card #(and (not (operation? %))
                          (corp? %)
                          (or (in-hand? %)
                              (in-discard? %)))}
    :msg (msg (corp-install-msg target))
    :async true
    :effect (effect (corp-install eid target nil {:ignore-install-cost true}))}})

(defcard "Invasion of Privacy"
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
             :effect (req (wait-for (trash state side target {:cause-card card})
                                    (if (pos? x)
                                      (continue-ability state side (iop (dec x)) card nil)
                                      (effect-completed state side eid))))})]
    {:on-play
     {:trace
      {:base 2
       :successful {:async true
                    :effect (req (wait-for
                                   (reveal state side (:hand runner))
                                   (let [x (- target (second targets))]
                                     (system-msg
                                       state :corp
                                       (str "uses Invasion of Privacy to reveal the Runner's Grip ( "
                                            (str/join ", " (map :title (sort-by :title (:hand runner))))
                                            " ) and trash up to " x " resources or events"))
                                     (continue-ability state side (iop (dec x)) card nil))))}
       :unsuccessful {:msg "take 1 bad publicity"
                      :async true
                      :effect (effect (gain-bad-publicity :corp eid 1))}}}}))

(defcard "IPO"
  {:on-play
   {:msg "gain 13 [Credits]"
    :async true
    :effect (effect (gain-credits eid 13))}})

(defcard "Kakurenbo"
  (let [install-abi {:async true
                     :prompt "Choose an agenda, asset or upgrade to install from Archives and place 2 advancement tokens on"
                     :show-discard true
                     :not-distinct true
                     :choices {:card #(and (or (agenda? %)
                                               (asset? %)
                                               (upgrade? %))
                                           (in-discard? %))}
                     :effect (req (wait-for (corp-install state side (make-eid state {:source card :source-type :corp-install})
                                                          target nil nil)
                                            (system-msg state side "uses Kakurenbo to place 2 advancements counters on the installed card")
                                            (add-prop state side eid async-result :advance-counter 2 {:placed true})))}]
    {:on-play
     {:prompt "Choose any number of cards in HQ to trash"
      :rfg-instead-of-trashing true
      :choices {:max (req (count (:hand corp)))
                :card #(and (corp? %)
                            (in-hand? %))}
      :msg (msg "trash " (quantify (count targets) "card") " in HQ")
      :async true
      :effect (req (wait-for (trash-cards state side targets {:unpreventable true :cause-card card})
                             (doseq [c (:discard (:corp @state))]
                               (update! state side (assoc-in c [:seen] false)))
                             (shuffle! state :corp :discard)
                             (continue-ability state side install-abi card nil)))
      :cancel-effect (req (system-msg state :corp "declines to use Kakurenbo to trash any cards from HQ")
                          (doseq [c (:discard (:corp @state))]
                            (update! state side (assoc-in c [:seen] false)))
                          (shuffle! state :corp :discard)
                          (continue-ability state side install-abi card nil))}}))

(defcard "Kill Switch"
  (let [trace-for-brain-damage {:msg (msg "reveal that they accessed " (:title (or (:card context) target)))
                                :trace {:base 3
                                        :req (req (or (agenda? (:card context))
                                                      (agenda? target)))
                                        :successful {:msg "do 1 brain damage"
                                                     :async true
                                                     :effect (effect (damage :runner eid :brain 1 {:card card}))}}}]
    {:events [(assoc trace-for-brain-damage
                     :event :access
                     :interactive (req (agenda? target)))
              (assoc trace-for-brain-damage :event :agenda-scored)]}))

(defcard "Lag Time"
  {:on-play {:effect (effect (update-all-ice))}
   :constant-effects [{:type :ice-strength
                       :value 1}]
   :leave-play (effect (update-all-ice))})

(defcard "Lateral Growth"
  {:on-play
   {:msg "gain 4 [Credits]"
    :async true
    :effect (req (wait-for (gain-credits state side 4)
                           (continue-ability
                             state side
                             {:prompt "Choose a card to install"
                              :req (req (not-empty (filter corp-installable-type? (:hand corp))))
                              :choices {:card #(and (corp? %)
                                                    (corp-installable-type? %)
                                                    (in-hand? %))}
                              :async true
                              :msg (msg (corp-install-msg target))
                              :cancel-effect (effect (system-msg "declines to use Lateral Growth to install a card")
                                                     (effect-completed eid))
                              :effect (effect (corp-install eid target nil nil))}
                             card nil)))}})

(defcard "Liquidation"
  {:on-play
   {:req (req (some #(and (rezzed? %)
                          (not (agenda? %)))
                    (all-installed state :corp)))
    :prompt "Choose any number of rezzed cards to trash"
    :choices {:max (req (count (filter #(not (agenda? %)) (all-active-installed state :corp))))
              :card #(and (rezzed? %)
                          (not (agenda? %)))}
    :msg (msg "trash " (str/join ", " (map :title targets))
              " and gain " (* (count targets) 3) " [Credits]")
    :async true
    :effect (req (wait-for (trash-cards state side targets {:cause-card card})
                           (gain-credits state side eid (* (count targets) 3))))}})

(defcard "Load Testing"
  {:on-play {:msg "make the Runner lose [Click] when their next turn begins"}
   :events [{:event :runner-turn-begins
             :duration :until-runner-turn-begins
             :msg "make the Runner lose [Click]"
             :effect (effect (lose-clicks :runner 1))}]})

(defcard "Localized Product Line"
  {:on-play
   {:prompt "Choose a card"
    :choices (req (cancellable (:deck corp) :sorted))
    :async true
    :effect (effect
              (continue-ability
                (let [title (:title target)
                      copies (filter #(= (:title %) title) (:deck corp))]
                  {:prompt "How many copies do you want to find?"
                   :choices {:number (req (count copies))}
                   :msg (msg "add " (quantify target "cop" "y" "ies") " of " title " to HQ")
                   :effect (req (shuffle! state :corp :deck)
                                (doseq [copy (take target copies)]
                                  (move state side copy :hand)))})
                card nil))}})

(defcard "Manhunt"
  {:events [{:event :successful-run
             :interactive (req true)
             :trace {:req (req (first-event? state side :successful-run))
                     :base 2
                     :successful {:msg "give the Runner 1 tag"
                                  :async true
                                  :effect (effect (gain-tags eid 1))}}}]})

(defcard "Market Forces"
  (letfn [(credit-diff [state]
            (min (* 3 (count-tags state))
                 (get-in @state [:runner :credit])))]
    {:on-play
     {:req (req tagged)
      :msg (msg (let [c (credit-diff state)]
                  (str "make the runner lose " c " [Credits], and gain " c " [Credits]")))
      :async true
      :effect (req (let [c (credit-diff state)]
                     (wait-for (lose-credits state :runner (make-eid state eid) c)
                               (gain-credits state :corp eid c))))}}))

(defcard "Mass Commercialization"
  {:on-play
   {:msg (msg "gain " (* 2 (count (filter #(pos? (get-counters % :advancement))
                                          (get-all-installed state)))) " [Credits]")
    :async true
    :effect (effect (gain-credits eid (* 2 (count (filter #(pos? (get-counters % :advancement))
                                                          (get-all-installed state))))))}})

(defcard "MCA Informant"
  {:on-play {:req (req (not-empty (filter #(has-subtype? % "Connection")
                                          (all-active-installed state :runner))))
             :prompt "Choose a connection to host MCA Informant on"
             :choices {:card #(and (runner? %)
                                   (has-subtype? % "Connection")
                                   (installed? %))}
             :msg (msg "host itself on " (card-str state target) ". The Runner has an additional tag")
             :effect (effect (install-as-condition-counter eid card target))}
   :constant-effects [{:type :tags
                       :value 1}]
   :leave-play (req (system-msg state :corp "trashes MCA Informant"))
   :runner-abilities [{:label "Trash MCA Informant host"
                       :cost [:click 1 :credit 2]
                       :async true
                       :effect (effect (system-msg :runner (str "spends [Click] and 2 [Credits] to trash "
                                                                (card-str state (:host card))))
                                       (trash :runner eid (get-card state (:host card)) {:cause-card (:host card)}))}]})

(defcard "Media Blitz"
  {:on-play
   {:async true
    :req (req (pos? (count (:scored runner))))
    :effect
    (effect
      (continue-ability
        {:prompt "Choose an agenda in the runner's score area"
         :choices {:card #(and (agenda? %)
                               (is-scored? state :runner %))}
         :effect (req (update! state side (assoc card :title (:title target) :abilities (ability-init (card-def target))))
                      (card-init state side (get-card state card) {:resolve-effect false :init-data true})
                      (update! state side (assoc (get-card state card) :title "Media Blitz")))}
        card nil))}})

(defcard "Medical Research Fundraiser"
  {:on-play
   {:msg "gain 8 [Credits]. The Runner gains 3 [Credits]"
    :async true
    :effect (req (wait-for (gain-credits state side 8)
                           (gain-credits state :runner eid 3)))}})

(defcard "Midseason Replacements"
  {:on-play
   {:trace
    {:req (req (last-turn? state :runner :stole-agenda))
     :base 6
     :label "Trace 6 - Give the Runner X tags"
     :successful {:msg "give the Runner X tags"
                  :async true
                  :effect (effect (system-msg
                                    (str "gives the Runner " (quantify (- target (second targets)) "tag")))
                                  (gain-tags eid (- target (second targets))))}}}})

(defcard "Mitosis"
  (letfn [(mitosis-ability [state side card eid target-cards]
            (wait-for (corp-install state side (first target-cards) "New remote" nil)
                      (let [installed-card async-result]
                        (add-prop state side installed-card :advance-counter 2 {:placed true})
                        (register-turn-flag!
                          state side
                          card :can-rez
                          (fn [state _ card]
                            (if (same-card? card installed-card)
                              ((constantly false) (toast state :corp "Cannot rez due to Mitosis." "Warning"))
                              true)))
                        (register-turn-flag!
                          state side
                          card :can-score
                          (fn [state _ card]
                            (if (same-card? card installed-card)
                              ((constantly false) (toast state :corp "Cannot score due to Mitosis." "Warning"))
                              true)))
                        (if (seq (rest target-cards))
                          (mitosis-ability state side card eid (rest target-cards))
                          (effect-completed state side eid)))))]
    {:on-play
     {:prompt "Choose 2 cards to install in new remote servers"
      :choices {:card #(and (not (operation? %))
                            (corp? %)
                            (in-hand? %))
                :max 2}
      :msg (msg (if (= 2 (count targets))
                  "install 2 cards from HQ in new remote servers, and place two advancements on each of them"
                  "install a card from HQ in a new remote server, and place two advancements on it"))
      :async true
      :effect (req (mitosis-ability state side card eid targets))}}))

(defcard "Mushin No Shin"
  {:on-play
   {:prompt "Choose a card to install from HQ"
    :choices {:card #(and (not (operation? %))
                          (corp? %)
                          (in-hand? %))}
    :async true
    :effect (req (wait-for (corp-install state side target "New remote" nil)
                           (let [installed-card async-result]
                             (add-prop state side installed-card :advance-counter 3 {:placed true})
                             (register-persistent-flag!
                               state side
                               installed-card :can-rez
                               (fn [state _ card]
                                 (if (same-card? card installed-card)
                                   ((constantly false) (toast state :corp "Cannot rez due to Mushin No Shin." "warning"))
                                   true)))
                             (register-turn-flag!
                               state side
                               installed-card :can-score
                               (fn [state _ card]
                                 (if (same-card? card installed-card)
                                   ((constantly false) (toast state :corp "Cannot score due to Mushin No Shin." "warning"))
                                   true)))
                             (register-events
                              state side installed-card
                              [{:event :corp-turn-begins
                                :duration :until-corp-turn-begins
                                :unregister-once-resolved true
                                :async true
                                :effect (req
                                         (clear-persistent-flag! state :corp installed-card :can-rez)
                                         (effect-completed state side eid))}])
                             (effect-completed state side eid))))}})

(defcard "Mutate"
  {:on-play
   {:req (req (some #(and (ice? %)
                          (rezzed? %))
                    (all-installed state :corp)))
    :prompt "Choose a rezzed piece of ice to trash"
    :choices {:card #(and (ice? %)
                          (rezzed? %))}
    :async true
    :effect (req (let [index (card-index state target)
                       [revealed-cards r] (split-with (complement ice?) (get-in @state [:corp :deck]))
                       titles (->> (conj (vec revealed-cards) (first r))
                                   (filter identity)
                                   (map :title))]
                   (wait-for (trash state :corp target {:cause-card card})
                             (shuffle! state :corp :deck)
                             (system-msg state side (str "uses Mutate to trash " (:title target)))
                             (wait-for
                               (reveal state side revealed-cards)
                               (system-msg state side (str "reveals " (clojure.string/join ", " titles) " from R&D"))
                               (let [ice (first r)
                                     zone (zone->name (second (get-zone target)))]
                                 (if ice
                                   (do (system-msg state side (str "uses Mutate to install and rez " (:title ice) " from R&D at no cost"))
                                       (corp-install state side eid ice zone {:ignore-all-cost true
                                                                              :install-state :rezzed-no-cost
                                                                              :display-message false
                                                                              :index index}))
                                   (do (system-msg state side "does not find any ice to install from R&D")
                                       (effect-completed state side eid))))))))}})

(defcard "Mutually Assured Destruction"
  {:on-play
   {:req (req (some #(and (rezzed? %)
                          (not (agenda? %)))
                    (all-installed state :corp)))
    :prompt "Choose any number of rezzed cards to trash"
    :interactive (req true)
    :choices {:max (req (count (filter #(not (agenda? %)) (all-active-installed state :corp))))
              :card #(and (rezzed? %)
                          (not (agenda? %)))}
    :msg (msg "trash " (str/join ", " (map :title targets))
              " and give the runner " (quantify (count targets) "tag"))
    :async true
    :effect (req (wait-for (trash-cards state side targets {:cause-card card})
                           (gain-tags state :corp eid (count targets))))}})

(defcard "NAPD Cordon"
  (lockdown
   {:events [{:event :pre-steal-cost
              :effect (req (let [counter (get-counters target :advancement)]
                             (steal-cost-bonus state side [:credit (+ 4 (* 2 counter))] {:source card :source-type :ability})))}]}))

(defcard "Neural EMP"
  {:on-play
   {:req (req (last-turn? state :runner :made-run))
    :msg "do 1 net damage"
    :async true
    :effect (effect (damage eid :net 1 {:card card}))}})

(defcard "Neurospike"
  {:on-play
   {:msg (msg "do " (:scored-agenda corp-reg 0) " net damage")
    :async true
    :effect (effect (damage eid :net (:scored-agenda corp-reg 0) {:card card}))}})

(defcard "NEXT Activation Command"
  (lockdown
   {:constant-effects [{:type :ice-strength
                        :value 2}
                       {:type :prevent-paid-ability
                        :req (req (let [target-card (first targets)
                                        ability (second targets)]
                                    (and (not (has-subtype? target-card "Icebreaker"))
                                         (:break ability))))
                        :value true}]}))


(defcard "O₂ Shortage"
  {:on-play
   {:async true
    :player :runner
    :waiting-prompt true
    :prompt "Choose one"
    :choices (req [(when-not (empty? (:hand runner))
                     "Trash 1 random card from the grip")
                   "The Corp gains [Click][Click]"])
    :msg (msg (if (= target "The Corp gains [Click][Click]")
                 "gain [Click][Click]"
                 (str "force the Runner to " (decapitalize target))))
    :effect (req (if (= target "The Corp gains [Click][Click]")
                   (do (gain-clicks state :corp 2)
                       (effect-completed state side eid))
                   (let [c (take 1 (shuffle (:hand runner)))]
                     (trash-cards state :runner eid c {:cause-card card :cause :forced-to-trash}))))}})

(defcard "Observe and Destroy"
  {:on-play
   {:additional-cost [:tag 1]
    :req (req (and (pos? (count-real-tags state))
                   (< (:credit runner) 6)))
    :prompt "Choose an installed card to trash"
    :choices {:card #(and (runner? %)
                          (installed? %))}
    :msg (msg "remove 1 Runner tag and trash " (card-str state target))
    :async true
    :effect (effect (trash eid target {:cause-card card}))}})

(defcard "Oversight AI"
  {:on-play {:choices {:card #(and (ice? %)
                                   (not (rezzed? %))
                                   (= (last (get-zone %)) :ices))}
             :msg (msg "rez " (card-str state target) " at no cost")
             :async true
             :effect (req (wait-for (rez state side target {:ignore-cost :all-costs :no-msg true})
                                    (install-as-condition-counter state side eid card (:card async-result))))}
   :events [{:event :subroutines-broken
             :condition :hosted
             :async true
             :req (req (and (same-card? target (:host card))
                            (empty? (remove :broken (:subroutines target)))))
             :msg (msg "trash itself and " (card-str state target))
             :effect (effect (trash :corp eid target {:unpreventable true :cause-card card}))}]})

(defcard "Patch"
  {:on-play {:choices {:card #(and (ice? %)
                                   (rezzed? %))}
             :msg (msg "give +2 strength to " (card-str state target))
             :async true
             :effect (effect (install-as-condition-counter eid card target))}
   :constant-effects [{:type :ice-strength
                       :req (req (same-card? target (:host card)))
                       :value 2}]})

(defcard "Paywall Implementation"
  {:events [{:event :successful-run
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits :corp eid 1))}]})

(defcard "Peak Efficiency"
  {:on-play
   {:msg (msg "gain " (reduce (fn [c server]
                                (+ c (count (filter (fn [ice] (:rezzed ice)) (:ices server)))))
                              0 (flatten (seq (:servers corp))))
              " [Credits]")
    :async true
    :effect (effect (gain-credits
                      eid
                      (reduce (fn [c server]
                                (+ c (count (filter (fn [ice] (:rezzed ice)) (:ices server)))))
                              0 (flatten (seq (:servers corp))))))}})

(defcard "Power Grid Overload"
  {:on-play
   {:trace
    {:base 2
     :req (req (last-turn? state :runner :made-run))
     :successful
     {:msg "trash 1 piece of hardware"
      :async true
      :effect
      (effect
        (continue-ability
          (let [max-cost (- target (second targets))]
            {:choices {:card #(and (hardware? %)
                                   (<= (:cost %) max-cost))}
             :msg (msg "trash " (:title target))
             :effect (effect (trash eid target {:cause-card card}))})
          card nil))}}}})

(defcard "Power Shutdown"
  {:on-play
   {:req (req (and (last-turn? state :runner :made-run)
                   (not-empty (filter #(or (hardware? %)
                                           (program? %))
                                      (all-active-installed state :runner)))))
    :prompt "How many cards do you want to trash from the top of R&D?"
    :choices {:number (req (count (:deck corp)))}
    :msg (msg "trash " (quantify target "card") " from the top of R&D")
    :async true
    :effect (req (wait-for (mill state :corp :corp target)
                           (continue-ability
                             state :runner
                             (let [n target]
                               {:async true
                                :prompt "Choose a Program or piece of Hardware to trash"
                                :choices {:card #(and (or (hardware? %)
                                                          (program? %))
                                                      (<= (:cost %) n))}
                                :msg (msg "trash " (:title target))
                                :effect (effect (trash eid target {:cause-card card :cause :forced-to-trash}))})
                             card nil)))}})

(defcard "Precognition"
  {:on-play
   {:msg "rearrange the top 5 cards of R&D"
    :waiting-prompt true
    :async true
    :effect (effect (continue-ability
                      (let [from (take 5 (:deck corp))]
                        (when (pos? (count from))
                          (reorder-choice :corp :runner from '()
                                          (count from) from)))
                      card nil))}})

(defcard "Predictive Algorithm"
  {:events [{:event :pre-steal-cost
             :effect (effect (steal-cost-bonus [:credit 2] {:source card :source-type :ability}))}]})

(defcard "Predictive Planogram"
  {:on-play
   {:prompt "Choose one"
    :waiting-prompt true
    :choices (req ["Gain 3 [Credits]"
                   "Draw 3 cards"
                   (when tagged
                     "Gain 3 [Credits] and draw 3 cards")])
    :msg (msg (decapitalize target))
    :async true
    :effect (req (case target
                   "Gain 3 [Credits]"
                   (gain-credits state :corp eid 3)
                   "Draw 3 cards"
                   (draw state :corp eid 3)
                   "Gain 3 [Credits] and draw 3 cards"
                   (wait-for (gain-credits state :corp 3)
                             (draw state :corp eid 3))
                   ; else
                   (effect-completed state side eid)))}})

(defcard "Preemptive Action"
  {:on-play {:rfg-instead-of-trashing true
             :async true
             :effect (effect (shuffle-into-rd-effect eid card 3 true))}})

(defcard "Priority Construction"
  (letfn [(install-card [chosen]
            {:prompt "Choose a remote server"
             :choices (req (conj (vec (get-remote-names state)) "New remote"))
             :async true
             :effect (effect (corp-install eid (assoc chosen :advance-counter 3) target {:ignore-all-cost true}))})]
    {:on-play
     {:prompt "Choose a piece of ice in HQ to install"
      :choices {:card #(and (in-hand? %)
                         (corp? %)
                         (ice? %))}
      :msg "install a piece of ice from HQ and place 3 advancements on it"
      :cancel-effect (req (effect-completed state side eid))
      :async true
      :effect (effect (continue-ability (install-card target) card nil))}}))

(defcard "Product Recall"
  {:on-play
   {:prompt "Choose a rezzed asset or upgrade to trash"
    :choices {:card #(and (rezzed? %)
                          (or (asset? %)
                              (upgrade? %)))}
    :msg (msg "trash " (card-str state target)
              " and gain " (trash-cost state side target) " [Credits]")
    :async true
    :effect (req (wait-for (trash state side target {:unpreventable true :cause-card card})
                           (gain-credits state :corp eid (trash-cost state side target))))}})

(defcard "Psychographics"
  {:on-play
   {:req (req tagged)
    :prompt "How many credits do you want to spend?"
    :choices {:number (req (count-tags state))}
    :async true
    :effect (req (let [c target]
                   (if (can-pay? state side (assoc eid :source card :source-type :ability) card (:title card) :credit c)
                     (let [new-eid (make-eid state {:source card :source-type :ability})]
                       (wait-for (pay state :corp new-eid card :credit c)
                                 (when-let [payment-str (:msg async-result)]
                                   (system-msg state :corp payment-str))
                                 (continue-ability
                                   state side
                                   {:msg (msg "place " (quantify c " advancement token") " on " (card-str state target))
                                    :choices {:card can-be-advanced?}
                                    :effect (effect (add-prop target :advance-counter c {:placed true}))}
                                   card nil)))
                     (effect-completed state side eid))))}})

(defcard "Psychokinesis"
  (letfn [(choose-card [state cards]
            (let [allowed-cards (filter #(some #{"New remote"} (installable-servers state %))
                                        cards)]
              {:prompt "Choose an agenda, asset, or upgrade to install"
               :choices (conj (vec allowed-cards) "None")
               :async true
               :effect (req (if (or (= target "None")
                                    (ice? target)
                                    (operation? target))
                              (do (system-msg state side "declines to use Psychokinesis to install a card")
                                  (effect-completed state side eid))
                              (continue-ability state side (install-card target) card nil)))}))
          (install-card [chosen]
            {:prompt "Choose a remote server"
             :choices (req (conj (vec (get-remote-names state)) "New remote"))
             :async true
             :effect (effect (corp-install eid chosen target nil))})]
    {:on-play
     {:req (req (pos? (count (:deck corp))))
      :msg (msg "look at the top " (quantify (count (take 5 (:deck corp))) "card") " of R&D")
      :waiting-prompt true
      :async true
      :effect (effect (continue-ability
                        (let [top-5 (take 5 (:deck corp))]
                          (choose-card state top-5))
                        card nil))}}))

(defcard "Public Trail"
  {:on-play
   {:req (req (last-turn? state :runner :successful-run))
    :player :runner
    :msg (msg (if (= target "Pay 8 [Credits]")
                (str "force the runner to " (decapitalize target))
                "give the runner 1 tag"))
    :waiting-prompt true
    :prompt "Choose one"
    :choices (req ["Take 1 tag"
                   (when (can-pay? state :runner (assoc eid :source card :source-type :ability) card (:title card) :credit 8)
                     "Pay 8 [Credits]")])
    :async true
    :effect (req (if (= target "Pay 8 [Credits]")
                   (wait-for (pay state :runner (make-eid state eid) card :credit 8)
                             (system-msg state :runner (:msg async-result))
                             (effect-completed state side eid))
                   (gain-tags state :corp eid 1)))}})

(defcard "Punitive Counterstrike"
  {:on-play
   {:trace
    {:base 5
     :successful
     {:async true
      :msg (msg "do " (:stole-agenda runner-reg-last 0) " meat damage")
      :effect (effect (damage eid :meat (:stole-agenda runner-reg-last 0) {:card card}))}}}})

(defcard "Reclamation Order"
  {:on-play
   {:prompt "Choose a card from Archives"
    :show-discard true
    :choices {:card #(and (corp? %)
                          (not= (:title %) "Reclamation Order")
                          (in-discard? %))}
    :msg (msg "name " (:title target))
    :async true
    :effect (req (let [title (:title target)
                       cards (filter #(= title (:title %)) (:discard corp))
                       n (count cards)]
                   (continue-ability
                     state side
                     {:prompt (str "How many copies of "
                                   title " do you want to reveal?")
                      :choices {:number (req n)}
                      :msg (msg "reveal "
                                (quantify target "cop" "y" "ies")
                                " of " title
                                " from Archives"
                                (when (pos? target)
                                  (str " and add "
                                       (if (= 1 target) "it" "them")
                                       " to HQ")))
                      :async true
                      :effect (req (wait-for
                                     (reveal state side cards)
                                     (doseq [c (->> cards
                                                    (sort-by :seen)
                                                    reverse
                                                    (take target))]
                                       (move state side c :hand))
                                     (effect-completed state side eid)))}
                     card nil)))}})

(defcard "Recruiting Trip"
  (let [rthelp (fn rt [total left selected]
                 (if (pos? left)
                   {:prompt (str "Choose a Sysop (" (inc (- total left)) "/" total ")")
                    :choices (req (cancellable (filter #(and (has-subtype? % "Sysop")
                                                             (not-any? #{(:title %)} selected)) (:deck corp)) :sorted))
                    :msg (msg "add " (:title target) " to HQ")
                    :async true
                    :effect (req (move state side target :hand)
                                 (continue-ability
                                   state side
                                   (rt total (dec left) (cons (:title target) selected))
                                   card nil))}
                   {:effect (effect (shuffle! :corp :deck))
                    :msg "shuffle R&D"}))]
    {:on-play
     {:prompt "How many Sysops do you want to search for?"
      :choices :credit
      :msg (msg "search for " target " Sysops")
      :async true
      :effect (effect (continue-ability (rthelp target target []) card nil))}}))

(defcard "Red Level Clearance"
  (let [all [{:msg "gain 2 [Credits]"
              :async true
              :effect (effect (gain-credits eid 2))}
             {:msg "draw 2 cards"
              :async true
              :effect (effect (draw eid 2))}
             {:msg "gain [Click]"
              :effect (effect (gain-clicks 1))}
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
                                               (effect-completed state side eid)))
                                   (continue-ability state side (choice abis chose-once) card nil))))})]
    {:on-play
     {:waiting-prompt true
      :async true
      :effect (effect (continue-ability (choice all false) card nil))}}))

(defcard "Red Planet Couriers"
  {:on-play
   {:req (req (some #(can-be-advanced? %) (all-installed state :corp)))
    :prompt "Choose an installed card that can be advanced"
    :choices {:card can-be-advanced?}
    :async true
    :effect (req (let [installed (get-all-installed state)
                       total-adv (reduce + (map #(get-counters % :advancement) installed))]
                   (doseq [c installed]
                     (add-prop state side c :advance-counter (- (get-counters c :advancement)) {:placed true}))
                   (add-prop state side target :advance-counter total-adv {:placed true})
                   (update-all-ice state side)
                   (system-msg state side (str "uses Red Planet Couriers to move " total-adv
                                               " advancement tokens to " (card-str state target)))
                   (effect-completed state side eid)))}})

(defcard "Replanting"
  (letfn [(replant [n]
            {:prompt "Choose a card to install"
             :async true
             :choices {:card #(and (corp? %)
                                   (not (operation? %))
                                   (in-hand? %))}
             :effect (req (wait-for (corp-install state side target nil {:ignore-all-cost true})
                                    (if (< n 2)
                                      (continue-ability state side (replant (inc n)) card nil)
                                      (effect-completed state side eid))))})]
    {:on-play
     {:prompt "Choose an installed card to add to HQ"
      :choices {:card #(and (corp? %)
                         (installed? %))}
      :msg (msg "add " (card-str state target)
             " to HQ, then install 2 cards ignoring all costs")
      :async true
      :effect (req (move state side target :hand)
                (continue-ability state side (replant 1) card nil))}}))

(defcard "Restore"
  {:on-play
   {:prompt "Choose a card in Archives to install & rez"
    :show-discard true
    :choices {:card #(and (corp? %)
                          (not (operation? %))
                          (in-discard? %))}
    :async true
    :effect (req (wait-for
                   (corp-install state side target nil {:install-state :rezzed})
                   (let [seen (assoc target :seen true)]
                     (system-msg state side (str "uses Restore to "
                                                 (corp-install-msg seen)))
                     (let [leftover (filter #(= (:title target) (:title %)) (-> @state :corp :discard))]
                       (when (seq leftover)
                         (doseq [c leftover]
                           (move state side c :rfg))
                         (system-msg state side (str "removes " (count leftover) " copies of " (:title target) " from the game"))))
                     (effect-completed state side eid))))}})

(defcard "Restoring Face"
  {:on-play
   {:prompt "Choose a Sysop, Executive or Clone to trash"
    :msg (msg "trash " (:title target) " to remove 2 bad publicity")
    :choices {:card #(or (has-subtype? % "Clone")
                         (has-subtype? % "Executive")
                         (has-subtype? % "Sysop"))}
    :async true
    :effect (req (wait-for
                   (lose-bad-publicity state side 2)
                   (wait-for
                     (resolve-ability
                       state side
                       (when (facedown? target)
                         {:async true
                          :effect (effect (reveal eid target))})
                       card targets)
                     (trash state side eid target {:cause-card card}))))}})

(defcard "Restructure"
  {:on-play
   {:msg "gain 15 [Credits]"
    :async true
    :effect (effect (gain-credits eid 15))}})

(defcard "Retribution"
  {:on-play
   {:req (req (and tagged
                   (->> (all-installed state :runner)
                        (filter #(or (hardware? %)
                                     (program? %)))
                        not-empty)))
    :prompt "Choose a program or piece of hardware to trash"
    :choices {:req (req (and (installed? target)
                             (or (program? target)
                                 (hardware? target))))}
    :msg (msg "trash " (card-str state target))
    :async true
    :effect (effect (trash eid target {:cause-card card}))}})

(defcard "Reuse"
  {:on-play
   {:prompt (msg "Choose up to " (quantify (count (:hand corp)) "card") " in HQ to trash")
    :choices {:max (req (count (:hand corp)))
              :card #(and (corp? %)
                          (in-hand? %))}
    :msg (msg (let [m (count targets)]
                (str "trash " (quantify m "card")
                     " and gain " (* 2 m) " [Credits]")))
    :async true
    :effect (req (wait-for (trash-cards state side targets {:unpreventable true :cause-card card})
                           (gain-credits state side eid (* 2 (count async-result)))))}})

(defcard "Reverse Infection"
  {:on-play
   {:prompt "Choose one"
    :waiting-prompt true
    :choices ["Purge virus counters"
              "Gain 2 [Credits]"]
    :async true
    :effect (req (if (= target "Gain 2 [Credits]")
                   (wait-for (gain-credits state side 2)
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
                                 (effect-completed state side eid))))))}})

(defcard "Rework"
  {:on-play
   {:prompt "Choose a card from HQ to shuffle into R&D"
    :choices {:card #(and (corp? %)
                          (in-hand? %))}
    :msg "shuffle a card from HQ into R&D"
    :effect (effect (move target :deck)
                    (shuffle! :deck))}})

(defcard "Riot Suppression"
  {:on-play
   {:rfg-instead-of-trashing true
    :req (req (last-turn? state :runner :trashed-card))
    :player :runner
    :async true
    :waiting-prompt true
    :prompt "Choose one"
    :msg (msg "force the Runner to " (decapitalize target))
    :choices ["Suffer 1 brain damage" "Get 3 fewer [Click] on the next turn"]
    :effect (req (if (= target "Suffer 1 brain damage")
                   (damage state :runner eid :brain 1 {:card card})
                   (do (swap! state update-in [:runner :extra-click-temp] (fnil #(- % 3) 0))
                       (effect-completed state side eid))))}})

(defcard "Rolling Brownout"
  {:on-play {:msg "increase the play cost of operations and events by 1 [Credits]"}
   :constant-effects [{:type :play-cost
                       :value 1}]
   :events [{:event :play-event
             :once :per-turn
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits :corp eid 1))}]})

(defcard "Rover Algorithm"
  {:on-play {:choices {:card #(and (ice? %)
                                   (rezzed? %))}
             :msg (msg "host itself as a condition counter on " (card-str state target))
             :async true
             :effect (effect (install-as-condition-counter eid card target))}
   :constant-effects [{:type :ice-strength
                       :req (req (same-card? target (:host card)))
                       :value (req (get-counters card :power))}]
   :events [{:event :pass-ice
             :condition :hosted
             :req (req (same-card? (:ice context) (:host card)))
             :msg "place 1 power counter on itself"
             :effect (effect (add-counter card :power 1))}]})

(defcard "Sacrifice"
  {:on-play
   {:req (req (and (pos? (count-bad-pub state))
                   (some #(pos? (:agendapoints %)) (:scored corp))))
    :additional-cost [:forfeit]
    :async true
    :effect (req (let [bp-lost (max 0 (min (:agendapoints (last (:rfg corp)))
                                           (count-bad-pub state)))]
                   (system-msg state side (str "uses Sacrifice to lose " bp-lost
                                               " bad publicity and gain " bp-lost " [Credits]"))
                   (if (pos? bp-lost)
                     (wait-for (lose-bad-publicity state side bp-lost)
                               (gain-credits state side eid bp-lost))
                     (effect-completed state side eid))))}})

(defcard "Salem's Hospitality"
  {:on-play
   {:prompt "Name a Runner card"
    :choices {:card-title (req (and (runner? target)
                                    (not (identity? target))))}
    :async true
    :effect (req (system-msg state side
                             (str "uses Salem's Hospitality to reveal the Runner's Grip ( "
                                  (str/join ", " (map :title (sort-by :title (:hand runner))))
                                  " ) and trash any copies of " target))
                 (let [cards (filter #(= target (:title %)) (:hand runner))]
                   (wait-for
                     (reveal state side cards)
                     (trash-cards state side eid cards {:unpreventable true :cause-card card}))))}})

(defcard "Scapenet"
  {:on-play
   {:trace
    {:req (req (last-turn? state :runner :successful-run))
     :base 7
     :successful
     {:prompt "Choose an installed virtual or chip card to remove from game"
      :choices {:card #(and (installed? %)
                            (or (has-subtype? % "Virtual")
                                (has-subtype? % "Chip")))}
      :msg (msg "remove " (card-str state target) " from game")
      :async true
      :effect (effect (move :runner target :rfg)
                      (effect-completed eid))}}}})

(defcard "Scarcity of Resources"
  {:on-play {:msg "increase the install cost of resources by 2"}
   :constant-effects [{:type :install-cost
                       :req (req (resource? target))
                       :value 2}]})

(defcard "Scorched Earth"
  {:on-play
   {:req (req tagged)
    :msg "do 4 meat damage"
    :async true
    :effect (effect (damage eid :meat 4 {:card card}))}})

(defcard "SEA Source"
  {:on-play
   {:trace
    {:base 3
     :req (req (last-turn? state :runner :successful-run))
     :label "Trace 3 - Give the Runner 1 tag"
     :successful
     {:msg "give the Runner 1 tag"
      :async true
      :effect (effect (gain-tags :corp eid 1))}}}})

(defcard "Seamless Launch"
  {:on-play
   {:prompt "Choose an installed card"
    :req (req (some #(and (corp? %)
                          (installed? %)
                          (not (= :this-turn (installed? %))))
                    (all-installed state :corp)))
    :choices {:card #(and (corp? %)
                          (installed? %)
                          (not (= :this-turn (installed? %))))}
    :msg (msg "place 2 advancement tokens on " (card-str state target))
    :async true
    :effect (effect (add-prop eid target :advance-counter 2 {:placed true}))}})

(defcard "Secure and Protect"
  {:on-play
   {:interactive (req true)
    :waiting-prompt true
    :async true
    :effect (req (if (seq (filter ice? (:deck corp)))
                   (continue-ability
                     state side
                     {:async true
                      :prompt "Choose a piece of ice"
                      :choices (req (filter ice? (:deck corp)))
                      :effect
                      (effect
                        (continue-ability
                          (let [chosen-ice target]
                            {:async true
                             :prompt "Choose a server"
                             :choices ["Archives" "R&D" "HQ"]
                             :msg (msg "reveal " (:title chosen-ice) " and install it, paying 3 [Credit] less")
                             :effect (req (wait-for
                                            (reveal state side chosen-ice)
                                            (shuffle! state side :deck)
                                            (corp-install state side eid chosen-ice target {:cost-bonus -3})))})
                          card nil))}
                     card nil)
                   (do (shuffle! state side :deck)
                       (effect-completed state side eid))))}})

(defcard "Self-Growth Program"
  {:on-play
   {:req (req tagged)
    :prompt "Choose 2 installed Runner cards"
    :choices {:card #(and (installed? %)
                          (runner? %))
              :max 2}
    :msg (msg (str "move " (str/join ", " (map :title targets)) " to the Runner's Grip"))
    :effect (req (doseq [c targets]
                   (move state :runner c :hand)))}})

(defcard "Service Outage"
  {:on-play {:msg "add a cost of 1 [Credit] for the Runner to make the first run each turn"}
   :constant-effects [{:type :run-additional-cost
                       :req (req (no-event? state side :run))
                       :value [:credit 1]}]})

(defcard "Shipment from Kaguya"
  {:on-play
   {:choices {:max 2
              :card #(and (corp? %)
                          (installed? %)
                          (can-be-advanced? %))}
    :msg (msg "place 1 advancement token on " (quantify (count targets) "card"))
    :effect (req (doseq [t targets]
                   (add-prop state :corp t :advance-counter 1 {:placed true})))}})

(defcard "Shipment from MirrorMorph"
  (letfn [(shelper [n]
            (when (< n 3)
              {:async true
               :prompt "Choose a card to install"
               :choices {:card #(and (corp? %)
                                     (not (operation? %))
                                     (in-hand? %))}
               :effect (req (wait-for (corp-install state side target nil nil)
                                      (continue-ability state side (shelper (inc n)) card nil)))}))]
    {:on-play
     {:async true
      :effect (effect (continue-ability (shelper 0) card nil))}}))

(defcard "Shipment from SanSan"
  {:on-play
   {:choices ["0" "1" "2"]
    :prompt "How many advancement tokens do you want to place?"
    :async true
    :effect (req (let [c (str->int target)]
                   (continue-ability
                     state side
                     {:choices {:card can-be-advanced?}
                      :msg (msg "place " (quantify c "advancement token") " on " (card-str state target))
                      :effect (effect (add-prop :corp target :advance-counter c {:placed true}))}
                     card nil)))}})

(defcard "Shipment from Tennin"
  {:on-play
   {:req (req (not-last-turn? state :runner :successful-run))
    :choices {:card #(and (corp? %)
                          (installed? %))}
    :msg (msg "place 2 advancement tokens on " (card-str state target))
    :effect (effect (add-prop target :advance-counter 2 {:placed true}))}})

(defcard "Shoot the Moon"
  (letfn [(rez-helper [ice]
            (when (seq ice)
              {:async true
               :effect (req (wait-for (rez state side (first ice) {:ignore-cost :all-costs})
                                      (continue-ability state side (rez-helper (rest ice)) card nil)))}))]
    {:on-play
     {:req (req tagged)
      :choices {:card #(and (ice? %)
                         (not (rezzed? %)))
                :max (req (min (count-tags state)
                               (reduce (fn [c server]
                                         (+ c (count (filter #(not (:rezzed %)) (:ices server)))))
                                       0 (flatten (seq (:servers corp))))))}
      :async true
      :effect (effect (continue-ability (rez-helper targets) card nil))}}))

(defcard "Snatch and Grab"
  {:on-play
   {:trace
    {:base 3
     :successful
     {:waiting-prompt true
      :msg "trash a connection"
      :choices {:card #(has-subtype? % "Connection")}
      :async true
      :effect
      (effect
        (continue-ability
          (let [c target]
            {:optional
             {:player :runner
              :waiting-prompt true
              :prompt (str "Take 1 tag to prevent " (:title c) " from being trashed?")
              :yes-ability
              {:async true
               :msg (msg "take 1 tag to prevent " (:title c)
                         " from being trashed")
               :effect (effect (gain-tags :runner eid 1 {:unpreventable true}))}
              :no-ability
              {:async true
               :msg (msg "trash " (:title c))
               :effect (effect (trash :corp eid c {:cause-card card}))}}})
          card nil))}}}})

(defcard "Special Report"
  {:on-play
   {:prompt "Choose any number of cards in HQ to shuffle into R&D"
    :choices {:max (req (count (:hand corp)))
              :card #(and (corp? %)
                          (in-hand? %))}
    :msg (msg "shuffle " (quantify (count targets) "card") " in HQ into R&D and draw " (quantify (count targets) "card"))
    :async true
    :effect (req (doseq [c targets]
                   (move state side c :deck))
                 (shuffle! state side :deck)
                 (draw state side eid (count targets)))}})

(defcard "Sprint"
  {:on-play
   {:async true
    :effect (req (wait-for
                   (draw state side 3)
                   (system-msg state side (str "uses Sprint to draw "
                                               (quantify (count async-result) "card")))
                   (continue-ability
                     state side
                     {:prompt "Choose 2 cards in HQ to shuffle into R&D"
                      :choices {:max 2
                                :all true
                                :card #(and (corp? %)
                                            (in-hand? %))}
                      :msg (msg "shuffle " (quantify (count targets) "card") " from HQ into R&D")
                      :effect (req (doseq [c targets]
                                     (move state side c :deck))
                                   (shuffle! state side :deck))}
                     card nil)))}})

(defcard "Standard Procedure"
  {:on-play
   {:req (req (last-turn? state :runner :successful-run))
    :prompt "Choose one"
    :choices ["Event" "Hardware" "Program" "Resource"]
    :msg (msg "name " target
              ", revealing " (str/join ", " (map :title (:hand runner)))
              " in the Runner's Grip, and gains "
              (* 2 (count (filter #(is-type? % target) (:hand runner)))) " [Credits]")
    :async true
    :effect (req (wait-for
                   (reveal state side (:hand runner))
                   (gain-credits state :corp eid (* 2 (count (filter #(is-type? % target) (:hand runner)))))))}})

(defcard "Stock Buy-Back"
  {:on-play
   {:msg (msg "gain " (* 3 (count (:scored runner))) " [Credits]")
    :async true
    :effect (effect (gain-credits eid (* 3 (count (:scored runner)))))}})

(defcard "Sub Boost"
  (let [new-sub {:label "[Sub Boost]: End the run"}]
    {:sub-effect {:label "End the run"
                  :msg "end the run"
                  :async true
                  :effect (effect (end-run eid card))}
     :on-play {:choices {:card #(and (ice? %)
                                     (rezzed? %))}
               :msg (msg "make " (card-str state target) " gain Barrier and \"[Subroutine] End the run\"")
               :async true
               :effect (req (add-extra-sub! state :corp (get-card state target) new-sub (:cid card))
                            (install-as-condition-counter state side eid card (get-card state target)))}
     :constant-effects [{:type :gain-subtype
                         :req (req (and (same-card? target (:host card))
                                        (rezzed? target)))
                         :value "Barrier"}]
     :leave-play (req (remove-extra-subs! state :corp (:host card) (:cid card)))
     :events [{:event :rez
               :condition :hosted
               :req (req (same-card? (:card context) (:host card)))
               :effect (req (add-extra-sub! state :corp (get-card state (:card context)) new-sub (:cid card)))}]}))

(defcard "Subcontract"
  (letfn [(sc [i sccard]
            {:prompt "Choose an operation in HQ to play"
             :choices {:card #(and (corp? %)
                                   (operation? %)
                                   (in-hand? %))}
             :async true
             :msg (msg "play " (:title target))
             :effect (req (wait-for (play-instant state side target nil)
                                    (if (and (not (get-in @state [:corp :register :terminal])) (< i 2))
                                      (continue-ability state side (sc (inc i) sccard) sccard nil)
                                      (effect-completed state side eid))))})]
    {:on-play
     {:req (req tagged)
      :async true
      :effect (effect (continue-ability (sc 1 card) card nil))}}))

(defcard "Subliminal Messaging"
  {:on-play {:msg "gain 1 [Credits]"
             :async true
             :effect (req (wait-for (gain-credits state side 1)
                                    (continue-ability
                                      state side
                                      {:once :per-turn
                                       :once-key :subliminal-messaging
                                       :msg "gain [Click]"
                                       :effect (effect (gain-clicks :corp 1))}
                                      card nil)))}
   :events [{:event :corp-phase-12
             :location :discard
             :optional
             {:req (req (not-last-turn? state :runner :made-run))
              :prompt "Add Subliminal Messaging to HQ?"
              :autoresolve (get-autoresolve :auto-fire)
              :yes-ability
              {:msg "add itself to HQ"
               :async true
               :effect (req (wait-for (reveal state side (make-eid state eid) card))
                            (move state side card :hand)
                            (effect-completed state side eid))}}}]
   :abilities [(set-autoresolve :auto-fire "Subliminal Messaging returning to HQ from Archives")]})



(defcard "Success"
  (letfn [(advance-n-times [state side eid card target n]
            (if (pos? n)
              (wait-for (advance state :corp (make-eid state {:source card}) (get-card state target) :no-cost)
                        (advance-n-times state side eid card target (dec n)))
              (effect-completed state side eid)))]
    {:on-play
     {:additional-cost [:forfeit]
      :choices {:card can-be-advanced?}
      :msg (msg "advance " (card-str state target)
             " " (quantify (get-advancement-requirement (cost-target eid :forfeit)) "time"))
      :async true
      :effect (effect (advance-n-times eid card target (get-advancement-requirement (cost-target eid :forfeit))))}}))

(defcard "Successful Demonstration"
  {:on-play
   {:req (req (last-turn? state :runner :unsuccessful-run))
    :msg "gain 7 [Credits]"
    :async true
    :effect (effect (gain-credits eid 7))}})

(defcard "Sunset"
  (letfn [(sun [serv]
            {:prompt "Choose two pieces of ice to swap"
             :choices {:card #(and (= serv (get-zone %))
                                   (ice? %))
                       :max 2}
             :async true
             :effect (req (if (= (count targets) 2)
                            (do (swap-ice state side (first targets) (second targets))
                                (system-msg state side
                                            (str "uses Sunset to swap "
                                                 (card-str state (first targets))
                                                 " with "
                                                 (card-str state (second targets))))
                                (continue-ability state side (sun serv) card nil))
                            (do (system-msg state side "has finished rearranging ice")
                                (effect-completed state side eid))))})]
    {:on-play
     {:prompt "Choose a server"
      :choices (req servers)
      :msg (msg "rearrange ice protecting " target)
      :async true
      :effect (req (let [serv (conj (server->zone state target) :ices)]
                     (continue-ability state side (sun serv) card nil)))}}))

(defcard "Surveillance Sweep"
  {:events [{:event :pre-init-trace
             :req (req run)
             :effect (req (swap! state assoc-in [:trace :player] :runner))}]})

(defcard "Sweeps Week"
  {:on-play
   {:msg (msg "gain " (count (:hand runner)) " [Credits]")
    :async true
    :effect (effect (gain-credits eid (count (:hand runner))))}})

(defcard "SYNC Rerouting"
  (lockdown
   {:events [{:event :run
              :async true
              :msg (msg "force the Runner to " (decapitalize target))
              :player :runner
              :prompt "Choose one"
              :choices (req [(when (can-pay? state :runner eid card nil [:credit 4])
                               "Pay 4 [Credits]")
                             "Take 1 tag"])
              :effect (req (if (= target "Pay 4 [Credits]")
                             (wait-for (pay state :runner (make-eid state eid) card :credit 4)
                                       (system-msg state :runner (:msg async-result))
                                       (effect-completed state side eid))
                             (gain-tags state :corp eid 1 nil)))}]}))

(defcard "Targeted Marketing"
  (let [gaincr {:req (req (= (:title (:card context)) (get-in card [:special :marketing-target])))
                :async true
                :msg (msg "gain 10 [Credits] from " (:marketing-target card))
                :effect (effect (gain-credits :corp eid 10))}]
    {:on-play {:prompt "Name a Runner card"
               :choices {:card-title (req (and (runner? target)
                                               (not (identity? target))))}
               :effect (effect (update! (assoc-in card [:special :marketing-target] target))
                               (system-msg (str "uses Targeted Marketing to name " target)))}
     :events [(assoc gaincr :event :runner-install)
              (assoc gaincr :event :play-event)]}))

(defcard "The All-Seeing I"
  (let [trash-all-resources
        {:msg "trash all resources"
         :async true
         :effect (effect (trash-cards :corp eid (filter resource? (all-active-installed state :runner)) {:cause-card card}))}]
    {:on-play
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
                  card nil))}}))

(defcard "Threat Assessment"
  {:on-play
   {:req (req (last-turn? state :runner :trashed-card))
    :prompt "Choose an installed Runner card"
    :choices {:card #(and (runner? %)
                          (installed? %))}
    :rfg-instead-of-trashing true
    :async true
    :effect (effect
              (continue-ability
                (let [chosen target]
                  {:player :runner
                   :waiting-prompt true
                   :prompt "Choose one"
                   :choices [(str "Add " (:title chosen) " to the top of the Stack")
                             "Take 2 tags"]
                   :async true
                   :msg (msg "force the Runner to" (decapitalize target))
                   :effect (req (if (= target "Take 2 tags")
                                  (gain-tags state :runner eid 2)
                                  (do (move state :runner chosen :deck {:front true})
                                      (effect-completed state side eid))))})
                card nil))}})

(defcard "Threat Level Alpha"
  {:on-play
   {:trace
    {:base 1
     :successful
     {:label "Give the Runner X tags"
      :async true
      :effect (req (let [tags (max 1 (count-tags state))]
                     (gain-tags state :corp eid tags)
                     (system-msg
                       state side
                       (str "uses Threat Level Alpha to give the Runner " (quantify tags "tag")))))}}}})

(defcard "Too Big to Fail"
  {:on-play
   {:req (req (< (:credit corp) 10))
    :msg "gain 7 [Credits] and take 1 bad publicity"
    :async true
    :effect (req (wait-for (gain-credits state side 7)
                           (gain-bad-publicity state :corp eid 1)))}})

(defcard "Traffic Accident"
  {:on-play
   {:req (req (<= 2 (count-tags state)))
    :msg "do 2 meat damage"
    :async true
    :effect (effect (damage eid :meat 2 {:card card}))}})

(defcard "Transparency Initiative"
  {:constant-effects [{:type :gain-subtype
                       :req (req (and (same-card? target (:host card))
                                      (rezzed? target)))
                       :value "Public"}]
   :on-play {:choices {:card #(and (agenda? %)
                                   (installed? %)
                                   (not (faceup? %)))}
             :effect (req (let [target (update! state side (assoc target
                                                                  :seen true
                                                                  :rezzed true))]
                            (wait-for (install-as-condition-counter state side card target)
                                      (let [card async-result]
                                        (register-events
                                          state side card
                                          [{:event :advance
                                            :condition :hosted
                                            :req (req (same-card? (:host card) target))
                                            :async true
                                            :msg "gain 1 [Credit]"
                                            :effect (effect (gain-credits eid 1))}])))))}})

(defcard "Trick of Light"
  {:on-play
   {:prompt "Choose an installed card you can advance"
    :req (req (let [advanceable (some can-be-advanced? (get-all-installed state))
                    num-installed (count (get-all-installed state))]
                 (and advanceable
                      (> num-installed 1))))
    :choices {:card #(and (can-be-advanced? %)
                          (installed? %))}
    :async true
    :effect (effect
              (continue-ability
                (let [card-to-advance target]
                  {:async true
                   :prompt "Choose another installed card"
                   :choices {:card #(and (not (same-card? card-to-advance %))
                                         (installed? %))}
                   :effect (effect
                             (continue-ability
                               (let [source target]
                                 {:prompt "How many advancement counters do you want to move?"
                                  :choices (take (inc (get-counters source :advancement)) ["0" "1" "2"])
                                  :msg (msg "move " target " advancement counters from "
                                            (card-str state source) " to " (card-str state card-to-advance))
                                  :effect (effect (add-prop :corp card-to-advance :advance-counter (str->int target) {:placed true})
                                                  (add-prop :corp source :advance-counter (- (str->int target)) {:placed true}))})
                               card nil))})
                card nil))}})

(defcard "Trojan Horse"
  {:on-play
   {:trace
    {:base 4
     :req (req (:accessed-cards runner-reg-last))
     :label "Trace 4 - Trash a program"
     :successful
     {:async true
      :effect (req (let [exceed (- target (second targets))]
                     (continue-ability
                       state side
                       {:async true
                        :prompt (str "Choose a program with an install cost of no more than "
                                     exceed " [Credits]")
                        :choices {:card #(and (program? %)
                                              (installed? %)
                                              (>= exceed (:cost %)))}
                        :msg (msg "trash " (card-str state target))
                        :effect (effect (trash eid target {:cause-card card}))}
                       card nil)))}}}})

(defcard "Trust Operation"
  (let [ability {:prompt "Choose a card to install from Archives"
                 :show-discard true
                 :choices {:card #(and (corp? %)
                                       (in-discard? %))}
                 :msg (msg "install and rez " (:title target) ", ignoring all costs")
                 :async true
                 :cancel-effect (effect (system-msg "declines to use Trust Operation to install a card")
                                        (effect-completed eid))
                 :effect (effect (corp-install eid target nil {:ignore-all-cost true
                                                               :install-state :rezzed-no-cost}))}]
    {:on-play {:req (req tagged)
               :msg (msg "trash " (:title target))
               :prompt "Choose a resource to trash"
               :choices {:card #(and (installed? %)
                                     (resource? %))}
               :async true
               :cancel-effect (effect
                               (system-msg "declines to use Trust Operation to trash a resource")
                               (continue-ability ability card nil))
               :effect (req (wait-for (trash state side target {:cause-card card})
                                      (continue-ability state side ability card nil)))}}))

(defcard "Ultraviolet Clearance"
  {:on-play
   {:async true
    :effect (req (wait-for
                   (gain-credits state side 10)
                   (wait-for
                     (draw state side 4)
                     (continue-ability
                       state side
                       {:prompt "Choose a card in HQ to install"
                        :choices {:card #(and (in-hand? %)
                                              (corp? %)
                                              (not (operation? %)))}
                        :msg (msg (corp-install-msg target))
                        :cancel-effect (effect (system-msg "declines to use Ultraviolet Clearance to install a card")
                                               (effect-completed eid))
                        :async true
                        :effect (effect (corp-install eid target nil nil))}
                       card nil))))}})

(defcard "Under the Bus"
  {:on-play
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
    :effect (req (wait-for (trash state side target {:cause-card card})
                           (gain-bad-publicity state :corp eid 1)))}})

(defcard "Violet Level Clearance"
  {:on-play
   {:msg "gain 8 [Credits] and draw 4 cards"
    :async true
    :effect (req (wait-for (gain-credits state side 8)
                           (draw state side eid 4)))}})

(defcard "Voter Intimidation"
  {:on-play
   {:psi {:req (req (seq (:scored runner)))
          :not-equal
          {:player :corp
           :async true
           :prompt "Choose a resource to trash"
           :choices {:card #(and (installed? %)
                                 (resource? %))}
           :msg (msg "trash " (:title target))
           :effect (effect (trash eid target {:cause-card card}))}}}})

(defcard "Wake Up Call"
  {:on-play
   {:rfg-instead-of-trashing true
    :req (req (last-turn? state :runner :trashed-card))
    :prompt "Choose a piece of hardware or non-virtual resource"
    :choices {:card #(or (hardware? %)
                         (and (resource? %)
                              (not (has-subtype? % "Virtual"))))}
    :async true
    :effect (effect
              (continue-ability
                (let [chosen target
                      wake card]
                  {:player :runner
                   :waiting-prompt true
                   :prompt "Choose one"
                   :choices [(str "Trash " (card-str state chosen))
                             "Suffer 4 meat damage"]
                   :async true
                   :effect (req (if (= target "Suffer 4 meat damage")
                                  (do (system-msg state side "suffers 4 meat damage")
                                      (damage state side eid :meat 4 {:card wake
                                                                      :unboostable true}))
                                  (do (system-msg state side (str "trashes " (card-str state chosen)))
                                      (trash state side eid chosen {:cause-card card :cause :forced-to-trash}))))})
                card nil))}})

(defcard "Wetwork Refit"
  (let [new-sub {:label "[Wetwork Refit] Do 1 brain damage"}]
    {:on-play {:choices {:card #(and (ice? %)
                                     (has-subtype? % "Bioroid")
                                     (rezzed? %))}
               :msg (msg "give " (card-str state target) " \"[Subroutine] Do 1 brain damage\" before all its other subroutines")
               :async true
               :effect (req (add-extra-sub! state :corp target new-sub (:cid card) {:front true})
                            (install-as-condition-counter state side eid card (get-card state target)))}
     :sub-effect (do-brain-damage 1)
     :leave-play (req (remove-extra-subs! state :corp (:host card) (:cid card)))
     :events [{:event :rez
               :req (req (same-card? (:card context) (:host card)))
               :effect (req (add-extra-sub! state :corp (get-card state (:card context)) new-sub (:cid card) {:front true}))}]}))

(defcard "Witness Tampering"
  {:on-play
   {:msg "remove 2 bad publicity"
    :effect (effect (lose-bad-publicity 2))}})
