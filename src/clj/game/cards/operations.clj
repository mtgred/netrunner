(ns game.cards.operations
  (:require
   [clojure.set :as set]
   [game.core.access :refer [access-card steal-cost-bonus]]
   [game.core.actions :refer [advance score]]
   [game.core.bad-publicity :refer [gain-bad-publicity lose-bad-publicity]]
   [game.core.board :refer [all-active-installed all-installed
                            get-all-installed get-remote-names get-remotes
                            installable-servers server->zone]]
   [game.core.card :refer [active? agenda? asset? can-be-advanced? card-index corp? corp-installable-type?
                           event? facedown? faceup? get-advancement-requirement
                           get-card get-counters get-title get-zone hardware? has-subtype? has-any-subtype? ice? identity?
                           in-discard? in-hand? installed? is-type? operation? program? resource?
                           rezzed? runner? upgrade?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.choose-one :refer [choose-one-helper cost-option]]
   [game.core.cost-fns :refer [play-cost trash-cost]]
   [game.core.costs :refer [total-available-credits]]
   [game.core.damage :refer [damage]]
   [game.core.def-helpers :refer [combine-abilities corp-install-up-to-n-cards corp-recur defcard do-meat-damage draw-abi drain-credits gain-credits-ability give-tags do-brain-damage reorder-choice something-can-be-advanced? get-x-fn with-revealed-hand tutor-abi]]
   [game.core.drawing :refer [draw]]
   [game.core.effects :refer [register-lingering-effect]]
   [game.core.eid :refer [effect-completed make-eid make-result]]
   [game.core.engine :refer [do-nothing pay register-events resolve-ability should-trigger? unregister-events]]
   [game.core.events :refer [event-count first-event? last-turn? no-event? not-last-turn? turn-events ]]
   [game.core.flags :refer [can-score? clear-persistent-flag! in-corp-scored?
                            in-runner-scored? is-scored?
                            register-persistent-flag! register-turn-flag! when-scored? zone-locked?]]
   [game.core.gaining :refer [gain-clicks gain-credits lose-clicks
                              lose-credits]]
   [game.core.hand-size :refer [runner-hand-size+]]
   [game.core.ice :refer [update-all-ice]]
   [game.core.identities :refer [disable-identity enable-identity]]
   [game.core.initializing :refer [ability-init card-init]]
   [game.core.installing :refer [corp-install corp-install-msg install-as-condition-counter]]
   [game.core.memory :refer [mu+ update-mu]]
   [game.core.moving :refer [as-agenda mill move swap-agendas swap-ice trash
                             trash-cards]]
   [game.core.optional :refer [get-autoresolve set-autoresolve]]
   [game.core.payment :refer [can-pay? cost-target x-cost-value cost-value ->c]]
   [game.core.play-instants :refer [play-instant]]
   [game.core.prevention :refer [damage-boost]]
   [game.core.prompts :refer [cancellable clear-wait-prompt show-wait-prompt]]
   [game.core.props :refer [add-counter add-prop]]
   [game.core.purging :refer [purge]]
   [game.core.revealing :refer [reveal reveal-loud]]
   [game.core.rezzing :refer [can-pay-to-rez? derez rez rez-multiple-cards]]
   [game.core.runs :refer [end-run make-run]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [is-remote? remote->name zone->name]]
   [game.core.shuffling :refer [shuffle! shuffle-into-deck
                                shuffle-into-rd-effect]]
   [game.core.tags :refer [gain-tags lose-tags]]
   [game.core.threat :refer [threat threat-level]]
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

;; helper for the faceup-archives-count cards
(defn- faceup-archives-types [corp]
  (count (distinct (map :type (filter faceup? (:discard corp))))))

(defn- clearance
  [creds cards]
  {:msg (str "gain " creds " [Credits] and draw " (quantify cards "card"))
   :async true
   :effect (req (wait-for (gain-credits state side creds {:suppress-checkpoint true})
                          (draw state side eid cards)))})

(defn- gain-n-clicks
  "Ability to gain n clicks"
  [n]
  {:msg (str "gain " (apply str (repeat n "[Click]")))
   :effect (req (gain-clicks state side n))})

(defn- place-n-advancement-counters
  ([n] (place-n-advancement-counters n nil))
  ([n must-be-advanceable?]
   {:msg (msg "place " (quantify n " advancement counter") " on "
              (card-str state target))
    :choices {:req (req (and (installed? target)
                             (or (not must-be-advanceable?)
                                 (can-be-advanced? state target))))}
    :async true
    :effect (effect (add-prop eid target :advance-counter n {:placed true}))}))

(defn- trash-type
  "Trash an installed card of the given type"
  ([type f loud?] (trash-type type f loud? 1 nil))
  ([type f loud? max-targets all?] (trash-type type f loud? max-targets all? nil))
  ([type f loud? max-targets all? ab]
   (letfn [(valid-targets? [state] (filterv f (concat (all-installed state :runner)
                                                      (all-installed state :corp))))]
     (merge
       {:async true
        :change-in-game-state {:silent (not loud?)
                               :req (req (seq (valid-targets? state)))}
        :prompt (cond
                  (= 1 max-targets) (str "Choose a " type " to trash")
                  all? (msg (let [ct (count (valid-targets? state))]
                              (if (= 1 ct)
                                (str "Choose a " type " to trash")
                                (str "Choose " ct " " type "s to trash"))))
                  :else (str "Choose up to " (quantify max-targets type) " to trash"))
        :waiting-prompt true
        :choices {:card #(and (installed? %)
                              (f %))
                  :max (req (min max-targets (count (valid-targets? state))))
                  :all all?}
        :msg (msg "trash " (enumerate-cards targets))
        :effect (req (trash-cards state side eid targets {:cause-card card}))}
       ab))))

;; Card definitions
(defcard "24/7 News Cycle"
  {:on-play
   {:additional-cost [(->c :forfeit)]
    :async true
    :change-in-game-state {:req (req (pos? (count (:scored corp))))}
    :effect (req (continue-ability
                   state side
                   {:prompt "Choose an agenda in your score area"
                    :choices {:card #(and (agenda? %)
                                          (when-scored? %)
                                          (is-scored? state :corp %))}
                    :msg (msg "trigger the \"when scored\" ability of " (:title target))
                    :async true
                    :effect (effect (continue-ability (:on-score (card-def target)) target nil))}
                   card nil))}})

(defcard "Accelerated Diagnostics"
  (letfn [(shuffle-count-fn [state]
            (event-count state :corp :corp-shuffle-deck))
          (is-top-x? [state card X]
            (some #(same-card? % card) (take X (get-in @state [:corp :deck]))))
          (ad [state eid card remaining-cards starting-shuffle-count]
            ;; rules note: if R&D is shuffled, or the cards are no longer in the top 3 cards of R&D,
            ;; (relative to when AD was played) then AD is short-circuited and cannot finish resolving
            (let [playable-cards (filterv #(and (operation? %)
                                                (is-top-x? state % (count remaining-cards))
                                                (can-pay? state :corp (assoc eid :source card
                                                                             :source-type :play)
                                                          card nil [(->c :credit (play-cost state :corp %))]))
                                          remaining-cards)]
              (cond
                (not (seq remaining-cards))
                {}
                (> (shuffle-count-fn state) starting-shuffle-count)
                {:msg (str "is unable to continue resolving " (:title card))}
                (seq playable-cards)
                {:prompt "Choose an operation to play"
                 :choices (cancellable playable-cards)
                 :msg (msg "play " (:title target))
                 :async true
                 :effect (req (wait-for
                                (play-instant state side target {:no-additional-cost true})
                                (let [remaining (filterv #(not (same-card? % target)) remaining-cards)]
                                  (continue-ability
                                    state side
                                    (ad state eid card remaining starting-shuffle-count)
                                    card nil))))
                 :cancel-effect (req (trash-cards state side eid remaining-cards {:unpreventable true :cause-card card}))}
                :else
                {:prompt "There are no playable cards"
                 :choices ["OK"]
                 :async true
                 :effect (req (trash-cards state side eid remaining-cards {:unpreventable true :cause-card card}))})))]
   {:prompt (msg "The top cards of R&D are (top->bottom): " (enumerate-cards (take 3 (:deck corp))))
    :change-in-game-state {:req (req (seq (:deck corp)))}
    :choices ["OK"]
    :async true
    :effect (req (continue-ability
                   state side
                   (ad state eid card (take 3 (:deck corp)) (shuffle-count-fn state))
                   card nil))}))

(defcard "Active Policing"
  (let [lose-click-abi
        {:msg "give the Runner -1 allotted [Click] for [runner-pronoun] next turn"
         :async true
         :effect (req (swap! state update-in [:runner :extra-click-temp] (fnil dec 0))
                      (continue-ability
                        state side
                        (when (threat-level 3 state)
                          {:optional
                           {:prompt "Pay 2 [credits] to give the Runner -1 allotted [Click] for [runner-pronoun] next turn?"
                            :yes-ability
                            {:cost [(->c :credit 2)]
                             :msg "give the Runner -1 allotted [Click] for [runner-pronoun] next turn"
                             :effect (req (swap! state update-in [:runner :extra-click-temp] (fnil dec 0)))}}})
                        card nil))}]
  {:on-play {:req (req (or (last-turn? state :runner :trashed-card)
                           (last-turn? state :runner :stole-agenda)))
             :prompt "Choose a card to install"
             :waiting-prompt true
             :choices {:card #(and (corp-installable-type? %)
                                   (in-hand? %))}
             :async true
             :cancel-effect (effect (system-msg (str "declines to use " (:title card) " to install a card from HQ"))
                                    (continue-ability lose-click-abi card nil))
             :effect (req (wait-for (corp-install state side target nil {:msg-keys {:install-source card
                                                                                    :display-origin true}})
                                    (continue-ability state side lose-click-abi card nil)))}}))

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
               :effect (req (wait-for (corp-install state side target nil {:install-state :rezzed
                                                                           :msg-keys {:install-source card
                                                                                      :display-origin true}})
                                      (continue-ability state side (ab (inc n) total) card nil)))}))]
    {:on-play
     {:base-play-cost [(->c :x-credits)]
      :msg (msg "install and rez " (x-cost-value eid) " Advertisements")
      :async true
      :effect (effect (continue-ability (ab 0 (x-cost-value eid)) card nil))}}))

(defcard "Aggressive Negotiation"
  {:on-play (assoc (tutor-abi nil) :req (req (:scored-agenda corp-reg)))})

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
                                :effect (req (clear-wait-prompt state :corp)
                                             (register-lingering-effect
                                               state side card
                                               {:type :cannot-jack-out
                                                :value true
                                                :duration :end-of-run})
                                             (make-run state :runner eid serv card))}
                  :no-ability {:msg "add itself to [their] score area as an agenda worth 1 agenda point"
                               :effect (effect (clear-wait-prompt :corp)
                                               (as-agenda :corp card 1))}}})
              card nil))}})

(defcard "Anonymous Tip"
  {:on-play (draw-abi 3)})

(defcard "Archived Memories"
  {:on-play (corp-recur)})

(defcard "Argus Crackdown"
  (lockdown
    {:events [{:event :successful-run
               :automatic :corp-damage
               :req (req (not-empty run-ices))
               :msg "deal 2 meat damage"
               :async true
               :effect (effect (damage eid :meat 2 {:card card}))}]}))

(defcard "Ark Lockdown"
  {:on-play
   {:change-in-game-state {:req (req (and (not-empty (:discard runner))
                                          (not (zone-locked? state :runner :discard))))}
    :prompt "Name a card to remove all copies in the Heap from the game"
    :show-discard true
    :choices (req (cancellable (:discard runner) :sorted))
    :msg (msg "remove all copies of " (:title target) " in the Heap from the game")
    :async true
    :effect (req (doseq [c (filter #(same-card? :title target %)
                                   (:discard runner))]
                   (move state :runner c :rfg))
                 (effect-completed state side eid))}})

(defcard "Armed Asset Protection"
  (let [faceup-agendas (fn [corp]
                         (some #(and (faceup? %) (agenda? %)) (:discard corp)))]
    {:on-play
     {:msg (msg "gain 3 [Credits], then gain " (faceup-archives-types corp) " [Credits]")
      :async true
      :effect (req (wait-for (gain-credits state :corp 3)
                             (wait-for (gain-credits state :corp (faceup-archives-types corp))
                                       (continue-ability
                                         state side
                                         (when (faceup-agendas corp)
                                           {:msg "gain 2 [Credits] for having faceup agendas in Archives"
                                            :effect (effect (gain-credits eid 2))
                                            :async true})
                                         card nil))))}}))

(defcard "Attitude Adjustment"
  {:on-play
   {:async true
    :msg (msg "draw 2 cards")
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
              :effect (req (let [to-gain (* 2 (count targets))]
                             (wait-for
                               (reveal-loud state side card
                                                {:and-then (str ", gain " to-gain " [Credits], and shuffle [them] into R&D")}
                                                targets)
                               (wait-for
                                 (gain-credits state side to-gain)
                                 (doseq [c targets]
                                   (move state :corp c :deck))
                                 (shuffle! state :corp :deck)
                                 (effect-completed state side eid)))))}
             card nil)))}})

(defcard "Audacity"
  (letfn [(audacity [x]
            {:prompt (msg "Choose a card that can be advanced to place advancement counters on (" x " remaining)")
             :async true
             :choices {:req (req (can-be-advanced? state target))}
             :msg (msg "place 1 advancement counter on " (card-str state target))
             :effect (req (wait-for (add-prop state side target :advance-counter 1 {:placed true})
                                    (if (> x 1)
                                      (continue-ability state side (audacity (dec x)) card nil)
                                      (effect-completed state side eid))))})]
    {:on-play
     {:req (req (<= 3 (count (:hand corp))))
      :async true
      :msg "trash all cards in HQ"
      :effect (req (wait-for (trash-cards state side (:hand corp) {:unpreventable true :cause-card card})
                             (continue-ability state side (audacity 2) card nil)))}}))

(defcard "Back Channels"
  {:on-play
   {:prompt "Choose an installed card in a server to trash"
    :choices {:card #(and (= (last (get-zone %)) :content)
                          (is-remote? (second (get-zone %))))}
    :change-in-game-state {:req (req (some #(and (= (last (get-zone %)) :content)
                                                 (is-remote? (second (get-zone %))))
                                           (all-installed state :corp)))}
    :msg (msg "trash " (card-str state target) " and gain "
              (* 3 (get-counters target :advancement)) " [Credits]")
    :async true
    :effect (req (wait-for
                   (gain-credits state side (* 3 (get-counters target :advancement)) {:suppress-checkpoint true})
                   (trash state side eid target {:cause-card card})))}})

(defcard "Backroom Machinations"
  {:on-play
   {:additional-cost [(->c :tag 1)]
    :msg "add itself to the score area as an agenda worth 1 agenda point"
    :effect (req (as-agenda state :corp card 1))}})

(defcard "Bad Times"
  {:on-play {:req (req tagged)
             :msg "force the Runner to lose 2[mu] until the end of the turn"
             :effect (req (register-lingering-effect
                            state :corp card
                            (assoc (mu+ -2) :duration :end-of-turn))
                          (update-mu state))}})

(defcard "Beanstalk Royalties"
  {:on-play (gain-credits-ability 3)})

(defcard "Best Defense"
  {:on-play
   {:prompt (msg "Choose a Runner card with an install cost of " (count-tags state) " or less to trash")
    :choices {:req (req (and (runner? target)
                             (installed? target)
                             (not (facedown? target))
                             (<= (:cost target) (count-tags state))))}
    :change-in-game-state {:req (req (some #(and (runner? %)
                                                 (installed? %)
                                                 (not (facedown? %))
                                                 (<= (:cost %) (count-tags state)))
                                           (all-installed state :runner)))}
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
                                 (trash-cards state :runner targets {:unpreventable true :cause-card card :cause :forced-to-trash :suppress-checkpoint true})
                                 (let [trashed-cards async-result]
                                   (wait-for
                                     (gain-credits state :runner (count trashed-cards))
                                     (system-msg state :runner
                                                 (str "trashes " (enumerate-cards trashed-cards)
                                                      " and gains " (count trashed-cards)
                                                      " [Credits]"))
                                     (effect-completed state side eid)))))}
                         card nil)
                       (let [n (* 2 (num-installed state t))]
                         (if (pos? n)
                           (do (system-msg state :corp (str "uses " (:title card) " to gain " n " [Credits]"))
                               (gain-credits state :corp eid n))
                           (effect-completed state side eid))))))}}))

(defcard "Big Brother"
  {:on-play (assoc (give-tags 2) :req (req tagged))})

(defcard "Big Deal"
  {:on-play
   {:prompt "Choose a card on which to place 4 advancement counters"
    :rfg-instead-of-trashing true
    :async true
    :choices {:card #(and (corp? %)
                          (installed? %))}
    :msg (msg "place 4 advancement counters on " (card-str state target))
    :change-in-game-state {:req (req (seq (all-installed state :corp)))}
    :effect (req (wait-for (add-prop state :corp target :advance-counter 4 {:placed true})
                           (let [card-to-score target]
                             (continue-ability
                               state side
                               {:optional
                                {:req (req (can-score? state side (get-card state card-to-score)))
                                 :prompt (str "Score " (:title card-to-score) "?")
                                 :yes-ability {:async true
                                               :effect (effect (score eid (get-card state card-to-score)))}
                                 :no-ability {:effect (effect (system-msg (str "declines to use " (:title card) " to score " (card-str state card-to-score))))}}}
                               card nil))))}})

(defcard "Bigger Picture"
  {:on-play (choose-one-helper
              {:req (req tagged)}
              [{:option "Give the runner 1 tag"
                :ability (give-tags 1)}
               {:option "Remove any number of tags"
                :ability {:req (req tagged)
                          :prompt "Remove how many tags?"
                          :choices {:number (req (count-tags state))}
                          :async true
                          :effect (req (continue-ability
                                         state side
                                         (assoc (drain-credits :corp :runner (* 5 target))
                                                :cost [(->c :tag target)])
                                         card nil))}}])})

(defcard "Bioroid Efficiency Research"
  {:on-play {:choices {:card #(and (ice? %)
                                   (has-subtype? % "Bioroid")
                                   (installed? %)
                                   (not (rezzed? %)))}
             :change-in-game-state {:req (req (some #(and (ice? %)
                                                          (not (rezzed? %)))
                                                    (all-installed state :corp)))}
             :async true
             :cancel-effect (req (do-nothing state side eid nil card))
             :effect (req (wait-for (rez state side target {:ignore-cost :all-costs})
                                    (install-as-condition-counter state side eid card (:card async-result))))}
   :events [{:event :subroutines-broken
             :condition :hosted
             :async true
             :req (req (and (same-card? (:ice context) (:host card))
                            (:all-subs-broken context)))
             :effect (req (wait-for
                            (derez state side (:ice context)
                                   {:msg-keys {:and-then " and trash itself"}
                                    :suppress-checkpoint true})
                            (trash state :corp eid card {:cause-card card})))}]})

(defcard "Biotic Labor"
  {:on-play (gain-n-clicks 2)})

(defcard "Blue Level Clearance"
  {:on-play (clearance 5 2)})

(defcard "BOOM!"
  {:on-play
   {:req (req (<= 2 (count-tags state)))
    :msg "do 7 meat damage"
    :async true
    :effect (effect (damage eid :meat 7 {:card card}))}})

(defcard "Bring Them Home"
  (let [threat-abi
        {:optional
         {:prompt "Shuffle 1 random card from the grip into the stack?"
          :req (req (threat-level 3 state))
          :waiting-prompt true
          :yes-ability
          {:cost [(->c :credit 2)]
           :req (req (seq (:hand runner)))
           :async true
           :effect (req (let [target-card (first (shuffle (:hand runner)))]
                          (wait-for
                            (reveal-loud state side card {:and-then " and shuffle it into the Stack"} target-card)
                            (move state :runner target-card :deck)
                            (shuffle! state :runner :deck)
                            (effect-completed state side eid))))}}}]
    {:on-play
     {:async true
      :req (req (or (last-turn? state :runner :trashed-card)
                    (last-turn? state :runner :stole-agenda)))
      :effect (req
                (let [chosen-cards (take 2 (shuffle (:hand runner)))]
                  (wait-for
                    (reveal-loud state side card {:and-then " and place [them] on the top of the stack (in a random order)"} chosen-cards)
                    (doseq [c (shuffle chosen-cards)]
                      (move state :runner c :deck {:front true}))
                    (continue-ability
                      state side
                      threat-abi
                      card nil))))}}))

(defcard "Building Blocks"
  {:on-play
   {:prompt "Choose a Barrier to install and rez"
    :choices {:card #(and (corp? %)
                          (has-subtype? % "Barrier")
                          (in-hand? %))}
    :async true
    :change-in-game-state {:req (req (seq (:hand corp)))}
    :effect (req (wait-for
                   (reveal-loud state side card nil target)
                   (corp-install state side eid target nil {:ignore-all-cost true
                                                            :msg-keys {:install-source card
                                                                       :display-origin true}
                                                            :install-state :rezzed-no-cost})))}})

(defcard "Business As Usual"
  (let [faux-purge {:choices {:req (req (and (installed? target)
                                             (pos? (get-counters target :virus))))}
                    :async true
                    :effect (effect (add-counter eid target :virus (* -1 (get-counters target :virus)) nil))
                    :msg (msg "remove all virus counters from " (card-str state target))}
        kaguya {:choices {:max 2
                          :req (req (and (corp? target)
                                         (installed? target)
                                         (can-be-advanced? state target)))}
                :msg (msg "place 1 advancement counter on " (quantify (count targets) "card"))
                :async true
                :effect (req (let [[f1 f2] targets]
                               (if f2
                                 (wait-for (add-prop state :corp f1 :advance-counter 1 {:placed true})
                                           (add-prop state :corp eid f2 :advance-counter 1 {:placed true}))
                                 (add-prop state :corp eid f1 :advance-counter 1 {:placed true}))))}]
    {:on-play (choose-one-helper
                {:optional :after-first
                 :change-in-game-state {:req (req (or (something-can-be-advanced? state)
                                                      (some #(pos? (get-counters % :virus)) (all-installed state :runner))))}
                 :count (req (if (threat-level 3 state) 2 1))}
                [{:option "Place 1 advancement counter on up to two cards you can advance"
                  :ability kaguya}
                 {:option "Remove all virus counters from 1 installed card"
                  :ability faux-purge}])}))

(defcard "Casting Call"
  {:on-play {:choices {:card #(and (agenda? %)
                                   (in-hand? %))}
             :change-in-game-state {:req (req (seq (:hand corp)))}
             :async true
             :effect (req (wait-for
                            (corp-install state side target nil {:install-state :face-up
                                                                 :msg-keys {:install-source card
                                                                            :display-origin true}})
                            (let [agenda async-result]
                              (system-msg state side (str "hosts " (:title card) " on " (:title agenda)))
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
    :change-in-game-state {:req (req (seq (:hand corp)))}
    :msg (msg "reveal " (enumerate-cards targets :sorted) " from HQ and gain " (* 2 (count targets)) " [Credits]")
    :async true
    :effect (req (wait-for
                   (reveal state side targets)
                   (gain-credits state side eid (* 2 (count targets)))))}})

(defcard "Cerebral Cast"
  {:on-play
   {:psi {:req (req (last-turn? state :runner :successful-run))
          :not-equal {:player :runner
                      :async true
                      :prompt "Choose one"
                      :waiting-prompt true
                      :choices ["Take 1 tag" "Suffer 1 core damage"]
                      :msg (msg "force the Runner to " (decapitalize target))
                      :effect (req (if (= target "Take 1 tag")
                                     (gain-tags state :runner eid 1)
                                     (damage state side eid :brain 1 {:card card})))}}}})

(defcard "Cerebral Static"
  {:on-play {:msg "disable the Runner's identity"}
   :static-abilities [{:type :disable-card
                       :req (req (same-card? target (:identity runner)))
                       :value true}]})

(defcard "\"Clones are not People\""
  {:events [{:event :agenda-scored
             :msg "add itself to the score area as an agenda worth 1 agenda point"
             :effect (req (as-agenda state :corp card 1))}]})

(defcard "Closed Accounts"
  {:on-play
   {:req (req tagged)
    :change-in-game-state {:req (req (pos? (:credit runner)))}
    :msg (msg "force the Runner to lose all " (:credit runner) " [Credits]")
    :async true
    :effect (effect (lose-credits :runner eid :all))}})

(defcard "Commercialization"
  {:on-play
   {:msg (msg "gain " (get-counters target :advancement) " [Credits]")
    :change-in-game-state {:req (req (some #(and (ice? %) (pos? (get-counters % :advancement)))
                                           (all-installed state :corp)))}
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
    :change-in-game-state {:req (req (seq (:deck corp)))}
    :choices (req (cancellable
                    (filter #(and (operation? %)
                                  (<= (:cost %) (:credit corp)))
                            (:deck corp))
                    :sorted))
    :msg (msg "search R&D for " (:title target) " and play it")
    :async true
    :effect (effect (shuffle! :deck)
                    (system-msg "shuffles [their] deck")
                    (play-instant eid target nil))}})

(defcard "Corporate Hospitality"
  {:on-play (combine-abilities (clearance 6 2) (corp-recur))})

(defcard "Corporate Shuffle"
  {:on-play
   {:msg "shuffle all cards in HQ into R&D and draw 5 cards"
    :async true
    :effect (effect (shuffle-into-deck :hand)
                    (draw eid 5))}})

(defcard "Cyberdex Trial"
  {:play-sound "virus-purge"
   :on-play
   {:msg "purge virus counters"
    :async true
    :effect (effect (purge eid))}})

(defcard "Death and Taxes"
  (let [maybe-gain-credit {:prompt "Gain 1 [Credits]?"
                           :waiting-prompt true
                           :autoresolve (get-autoresolve :auto-fire)
                           :yes-ability {:msg "gain 1 [Credits]"
                                         :async true
                                         :effect (effect (gain-credits :corp eid 1))}}]
    {:special {:auto-fire :always}
     :abilities [(set-autoresolve :auto-fire "Death and Taxes")]
     :events [{:event :runner-install :optional maybe-gain-credit}
              {:event :runner-trash :optional (assoc maybe-gain-credit :req (req (installed? (:card target))))}]}))

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
    :msg (msg "place 3 advancement counters on " (card-str state target))
    :async true
    :effect (req (add-counter state side eid target :advancement 3 {:placed true})
                 (register-turn-flag!
                   state side
                   target :can-score
                   (fn [state _ card]
                     (if (same-card? card target)
                       ((constantly false) (toast state :corp "Cannot score due to Dedication Ceremony." "warning"))
                       true))))}})

(defcard "Defective Brainchips"
  {:prevention [{:prevents :pre-damage
                 :type :event
                 :max-uses 1
                 :mandatory true
                 :ability {:async true
                           :condition :active
                           :req (req
                                  (and (or (= :brain (:type context))
                                           (= :core (:type context)))
                                       (first-event? state side :pre-damage-flag #(= :brain (:type (first %))))
                                       (not= :all (:prevented context))
                                       (pos? (:remaining context))
                                       (not (:unboostable context))))
                           :msg "increase the pending core damage by 1"
                           :effect (req (damage-boost state side eid 1))}}]})

(defcard "Digital Rights Management"
  {:on-play
   {:req (req (and (< 1 (:turn @state))
                   (not (some #{:hq} (:successful-run runner-reg-last)))))
    :prompt "Choose an Agenda"
    :change-in-game-state {:req (req (or (seq (:deck corp))
                                         (seq (:hand corp))))}
    :choices (req (conj (vec (filter agenda? (:deck corp))) "None"))
    :msg (msg (if (= "None" target)
                "shuffle R&D"
                (str "reveal " (:title target) " from R&D and add it to HQ")))
    :async true
    :effect (let [end-effect (req (system-msg state side "can not score agendas for the remainder of the turn")
                                  (register-lingering-effect
                                    state side card
                                    {:type :cannot-score
                                     :duration :end-of-turn
                                     :value true})
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
                        :async true
                        :effect (req (wait-for (resolve-ability
                                                 state side
                                                 (let [card-to-install target]
                                                   {:prompt "Choose a server"
                                                    :choices (remove #{"HQ" "R&D" "Archives"} (installable-servers state card-to-install))
                                                    :async true
                                                    :effect (effect (corp-install eid card-to-install target {:msg-keys {:install-source card
                                                                                                                         :display-origin true}}))})
                                                 card nil)
                                               (end-effect state side eid card targets)))
                        :cancel-effect (effect (system-msg (str "declines to use " (:title card) " to install a card"))
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

(defcard "Distributed Tracing"
  {:on-play (assoc (give-tags 1) :req (req (last-turn? state :runner :stole-agenda)))})

(defcard "Diversified Portfolio"
  (letfn [(number-of-non-empty-remotes [state]
            (count (filter seq (map #(:content (second %)) (get-remotes state)))))]
    {:on-play
     {:msg (msg "gain " (number-of-non-empty-remotes state)
                " [Credits]")
      :change-in-game-state {:req (req (pos? (number-of-non-empty-remotes state)))}
      :async true
      :effect (effect (gain-credits eid (number-of-non-empty-remotes state)))}}))

(defcard "Divert Power"
  {:on-play
   {:prompt "Choose any number of cards to derez"
    :choices {:card #(and (installed? %)
                          (rezzed? %))
              :max (req (count (filter rezzed? (all-installed state :corp))))}
    :change-in-game-state {:req (req (seq (all-installed state :corp)))}
    :async true
    :effect (req (wait-for
                   (derez state side targets)
                   (let [discount (* 3 (count targets))]
                     (continue-ability
                       state side
                       {:async true
                        :prompt (str "Choose a card to rez, paying " discount " [Credits] less")
                        :choices {:req (req (and ((every-pred installed? corp? (complement rezzed?) (complement agenda?)) target)
                                                 (can-pay-to-rez? state side (assoc eid :source card)
                                                                  target {:cost-bonus (- discount)})))}
                        :effect (req (rez state side eid target {:cost-bonus (- discount)}))}
                       card nil))))}})

(defcard "Door to Door"
  {:events [{:event :runner-turn-begins
             :automatic :corp-damage
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
             :change-in-game-state {:req (req (some ice? (all-installed state :corp)))}
             :async true
             :effect (effect (install-as-condition-counter eid card target))}
   :events [{:event :encounter-ice
             :condition :hosted
             :trace {:base 3
                     :req (req (same-card? current-ice (:host card)))
                     :successful (give-tags 1)}}]})

(defcard "Economic Warfare"
  {:on-play
   {:req (req (last-turn? state :runner :successful-run))
    :async true
    :change-in-game-state {:req (req (>= (:credit runner) 4))}
    :msg "make the runner lose 4 [Credits]"
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

(defcard "End of the Line"
  {:play-sound "end-of-the-line"
   :on-play
   {:additional-cost [(->c :tag 1)]
    :msg "do 4 meat damage"
    :async true
    :effect (effect (damage eid :meat 4 {:card card}))}})

(defcard "Enforced Curfew"
  {:on-play {:msg "reduce the Runner's maximum hand size by 1"}
   :static-abilities [(runner-hand-size+ -1)]})

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
                               (or (not= (:faction (:identity runner)) (:faction target))
                                   (= (:faction (:identity runner)) "Neutral"))))}
      :msg (msg "trash " (:title target))
      :effect (effect (trash eid target {:cause-card card}))}}}})

(defcard "Enhanced Login Protocol"
  {:on-play {:msg (str "add an additional cost of [Click]"
                       " to make the first run not through a card ability each turn")}
   :static-abilities
   [{:type :run-additional-cost
     :req (req (and (no-event? state side :run #(:click-run (:cost-args (first %))))
                    (:click-run (second targets))))
     :value [(->c :click 1)]}]})

(defcard "Exchange of Information"
  {:on-play
   {:req (req tagged)
    :change-in-game-state {:req (req (and (seq (:scored runner)) (seq (:scored corp))))}
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
                              :cancel-effect (effect (system-msg (str "declines to use " (:title card) " to trash an installed card"))
                                                     (effect-completed eid))
                              :effect (req (wait-for (trash state side target {:cause-card card})
                                                     (gain-credits state side eid 3)))}
                             card nil)))}})

(defcard "Fast Break"
  {:x-fn (req (-> runner :scored count))
   :on-play
   {:async true
    :change-in-game-state {:req (req (pos? ((get-x-fn) state side eid card targets)))}
    :msg (msg "gain " ((get-x-fn) state side eid card targets) " [Credits]")
    :effect
    (req (let [draw {:async true
                     :prompt "How many cards do you want to draw?"
                     :waiting-prompt true
                     :choices {:number (get-x-fn)
                               :max (get-x-fn)
                               :default (req 1)}
                     :msg (msg "draw " (quantify target "card"))
                     :effect (effect (draw eid target))}
               install-cards (fn install-cards
                               [server n]
                               (when (pos? n)
                                 {:prompt "Choose a card to install"
                                  :choices {:card #(and (corp? %)
                                                        (not (operation? %))
                                                        (in-hand? %)
                                                        (seq (filter (fn [c] (= server c)) (installable-servers state %))))}
                                  :async true
                                  :effect (req (wait-for
                                                 (corp-install state side target server {:msg-keys {:install-source card
                                                                                                    :display-origin true}})
                                                 (let [server (remote->name (second (:zone async-result)))]
                                                   (if (< n ((get-x-fn) state side eid card targets))
                                                     (continue-ability state side (install-cards server (inc n)) card nil)
                                                     (effect-completed state side eid)))))}))
               select-server {:async true
                              :prompt "Choose a server"
                              :choices (req (conj (vec (get-remote-names state)) "New remote"))
                              :effect (effect (continue-ability (install-cards target 1) card nil))}]
           (wait-for (gain-credits state :corp ((get-x-fn) state side eid card targets))
                     (wait-for (resolve-ability state side draw card nil)
                               (continue-ability state side select-server card nil)))))}})

(defcard "Fast Track"
  {:on-play
   {:prompt "Choose an Agenda"
    :change-in-game-state {:req (req (seq (:deck corp)))}
    :choices (req (cancellable (filter agenda? (:deck corp)) :sorted))
    :async true
    :msg (msg "reveal " (:title target) " from R&D and add it to HQ")
    :cancel-effect (req (system-msg state side (str "uses " (:title card) " to shuffle R&D"))
                        (shuffle! state side :deck)
                        (effect-completed state side eid))
    :effect (req (wait-for (reveal state side target)
                           (shuffle! state side :deck)
                           (move state side target :hand)
                           (effect-completed state side eid)))}})

(defcard "Financial Collapse"
  (letfn [(count-resources [state]
            (* 2 (count (filter resource? (all-active-installed state :runner)))))]
    {:on-play
     {:optional
      {:req (req (<= 6 (:credit runner)))
       :change-in-game-state {:req (req (pos? (count-resources state)))}
       :player :runner
       :waiting-prompt true
       :prompt "Trash a resource?"
       :yes-ability
       {:display-side :runner
        :cost [(->c :resource 1)]
        :msg :cost}
       :no-ability
       {:player :corp
        :async true
        :msg (msg "make the Runner lose " (count-resources state) " [Credits]")
        :effect (effect (lose-credits :runner eid (count-resources state)))}}}}))

(defcard "Flood the Market"
  (letfn [(full-servers [state]
            (count (filter #(and (not-empty (:content %))
                                 (not-empty (:ices %)))
                           (vals (get-remotes state)))))]
    {:on-play {:change-in-game-state {:req (req (pos? (full-servers state)))}
               :async true
               :prompt (msg "Choose a card and place " (quantify (full-servers state) "advancement counter") " on it")
               :choices {:req (req (and (installed? target)
                                        (can-be-advanced? state target)))}
               :msg (msg "place " (quantify (full-servers state) "advancement counter") " on " (card-str state target))
               :effect (req (add-counter state state eid target :advancement (full-servers state)))}}))

(defcard "Focus Group"
  {:on-play
   {:req (req (last-turn? state :runner :successful-run))
    :prompt "Choose one"
    :choices ["Event" "Hardware" "Program" "Resource"]
    :async true
    :msg (msg "choose " target)
    :effect (req (let [type target
                       numtargets (count (filter #(= type (:type %)) (:hand runner)))]
                   (continue-ability
                     state side
                     (with-revealed-hand :runner {:event-side :corp}
                       (when (pos? numtargets)
                         {:async true
                          :prompt "How many credits do you want to pay?"
                          :choices {:number (req numtargets)}
                          :effect (req (let [c target]
                                         (if (can-pay? state side eid card (:title card) (->c :credit c))
                                           (wait-for (pay state :corp card (->c :credit c))
                                                     (when-let [payment-str (:msg async-result)]
                                                       (system-msg state :corp payment-str))
                                                     (continue-ability
                                                       state :corp
                                                       (place-n-advancement-counters c)
                                                       card nil))
                                           (effect-completed state side eid))))}))
                       card nil)))}})

(defcard "Foxfire"
  {:on-play
   {:trace {:base 7
            :successful (trash-type
                          "virtual resource or link"
                          (fn [c] (or (and (resource? c)
                                           (has-subtype? c "Virtual"))
                                      (has-subtype? c "Link")))
                          :loud)}}})

(defcard "Freelancer"
  {:on-play (trash-type "resource" resource? :loud 2 nil {:req (req tagged)})})

(defcard "Friends in High Places"
  (let [fhelper (fn fhp [n] {:prompt "Choose a card in Archives to install"
                             :async true
                             :show-discard true
                             :choices {:card #(and (corp? %)
                                                   (not (operation? %))
                                                   (in-discard? %))}
                             :effect (req (wait-for
                                            (corp-install state side target nil {:msg-keys {:install-source card
                                                                                            :display-origin true}})
                                            (if (< n 2)
                                              (continue-ability state side (fhp (inc n)) card nil)
                                              (effect-completed state side eid))))})]
    {:on-play
     {:async true
      :change-in-game-state {:req (req (seq (:discard corp)))}
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
    :change-in-game-state {:req (req (pos? (count (:scored runner))))}
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
                                :effect (req (wait-for (pay state :runner (make-eid state eid) card (->c :credit (* 3 (count targets))))
                                                       (system-msg
                                                         state :runner
                                                         (str (:msg async-result) " to prevent the trashing of "
                                                              (enumerate-cards targets :sorted)))
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
                                                          ": " (enumerate-cards async-result :sorted)))
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
  {:on-play (gain-credits-ability 15)})

(defcard "Greasing the Palm"
  {:on-play
   {:msg "gain 5 [Credits]"
    :async true
    :effect (req (wait-for (gain-credits state side 5)
                           (continue-ability
                             state side
                             {:prompt "Choose a card to install"
                              :waiting-prompt true
                              :req (req (not-empty (filter corp-installable-type? (:hand corp))))
                              :choices {:card #(and (corp? %)
                                                    (corp-installable-type? %)
                                                    (in-hand? %))}
                              :async true
                              :cancel-effect (effect (system-msg (str "declines to use " (:title card) " to install a card from HQ"))
                                                     (effect-completed eid))
                              :effect (req (wait-for (corp-install state :corp (make-eid state eid) target nil {:msg-keys {:install-source card
                                                                                                                           :display-origin true}})
                                                     (let [installed-card async-result]
                                                       (if (not (zero? (count-tags state)))
                                                         (continue-ability
                                                           state side
                                                           {:optional {:prompt "Remove 1 tag to place 1 advancement counter on the installed card?"
                                                                       :waiting-prompt true
                                                                       :yes-ability {:msg (msg "place 1 advancement counter on " (card-str state installed-card))
                                                                                     :cost [(->c :tag 1)]
                                                                                     :async true
                                                                                     :effect (req (add-prop state :corp eid installed-card :advance-counter 1 {:placed true}))}}}
                                                           card nil)
                                                         (effect-completed state side eid)))))}
                             card nil)))}})

(defcard "Green Level Clearance"
  {:on-play (clearance 3 1)})

(defcard "Hangeki"
  {:on-play
   {:req (req (last-turn? state :runner :trashed-card))
    :prompt "Choose an installed Corp card"
    :change-in-game-state {:req (req (seq (all-installed state :corp)))}
    :choices {:card #(and (corp? %)
                          (installed? %))}
    :async true
    :msg (msg "choose " (card-str state target))
    :effect
    (effect (continue-ability
              {:optional
               {:player :runner
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
                     (system-msg state side (str "uses " (:title card) " to gain 10 [Credits]"))
                     (gain-credits state side eid 10))))}})

(defcard "Hard-Hitting News"
  {:on-play
   {:trace {:base 4
            :req (req (last-turn? state :runner :made-run))
            :label "Give the Runner 4 tags"
            :successful (give-tags 4)}}})

(defcard "Hasty Relocation"
  (letfn [(hr-final [chosen original]
            {:prompt (str "The top cards of R&D will be " (enumerate-cards chosen))
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
     {:additional-cost [(->c :trash-from-deck 1)]
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
             :msg "add 1 installed non-virtual card to the grip"
             :effect (effect (move :runner target :hand true))}}}})

(defcard "Hedge Fund"
  {:on-play (gain-credits-ability 9)})

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
     :req (req (:trashed-accessed-card runner-reg-last))
     :label "Trash 2 installed non-program cards or take 1 bad publicity"
     :successful (trash-type "non-program" #(or (facedown? %) (not (program? %))) :loud 2 :all)
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
             :prompt "Choose a card to trash"
             :choices {:card #(and (runner? %)
                                   (in-hand? %))}
             :async true
             :msg (msg "force the Runner to trash"
                       (:title target) " from the grip")
             :effect (effect (trash :runner eid target {:unpreventable true :cause-card card :cause :forced-to-trash}))}]})

(defcard "Hunter Seeker"
  {:on-play (trash-type "card" installed? :loud 1 nil
                        {:req (req (last-turn? state :runner :stole-agenda))})})

(defcard "Hyoubu Precog Manifold"
  (lockdown
   {:on-play {:prompt "Choose a server"
              :choices (req servers)
              :msg (msg "choose " target)
              :effect (effect (update! (assoc card :card-target target)))}
    :events [{:event :successful-run
              :psi {:req (req (= (zone->name (get-in @state [:run :server]))
                                 (:card-target card)))
                    :not-equal {:msg "end the run"
                                :async true
                                :effect (effect (end-run eid card))}}}]}))

(defcard "Hypoxia"
  {:on-play {:req (req tagged)
             :msg "do 1 core damage and give the Runner -1 allotted [Click] for [runner-pronoun] next turn"
             :rfg-instead-of-trashing true
             :async true
             :effect (req (wait-for (damage state :runner :brain 1 {:card card})
                                    (swap! state update-in [:runner :extra-click-temp] (fnil dec 0))
                                    (effect-completed state side eid)))}})

(defcard "Interns"
  {:on-play
   {:prompt "Choose a card to install from Archives or HQ"
    :change-in-game-state {:req (req (or (seq (:hand corp))
                                         (some #(or (not (operation? %)) (not (:seen %))) (:discard corp))))}
    :show-discard true
    :not-distinct true
    :choices {:card #(and (not (operation? %))
                          (corp? %)
                          (or (in-hand? %)
                              (in-discard? %)))}
    :async true
    :effect (effect (corp-install eid target nil {:ignore-install-cost true
                                                  :msg-keys {:install-source card
                                                             :display-origin true}}))}})

(defcard "Invasion of Privacy"
  {:on-play
   {:trace
    {:base 2
     :successful (with-revealed-hand :runner {:event-side :corp}
                   {:prompt (msg "Trash up to " (- target (second targets))
                                 " resources and/or events from the grip")
                    :choices {:req (req (and (in-hand? target)
                                             (runner? target)
                                             (or (resource? target)
                                                 (event? target))))
                              :max (req (min (- target (second targets))
                                             (count (filter #(or (resource? %)
                                                                 (event? %))
                                                            (:hand runner)))))}
                    :async true
                    :msg (msg "trash " (enumerate-cards targets))
                    :effect (req (trash-cards state side eid targets {:cause-card card}))})
     :unsuccessful {:msg "take 1 bad publicity"
                    :async true
                    :effect (effect (gain-bad-publicity :corp eid 1))}}}})

(defcard "IP Enforcement"
  (letfn [(valid-agenda? [state c x]
            (and (agenda? c)
                 (= (:zone c) [:scored])
                 (= (:scored-side c) :runner)
                 ;; Note: fake agendas do not have titles, but will have agendapoints keys
                 ;; with the way our system is set up (they use a printed title only)
                 (:title c)
                 (= (:agendapoints c) x)))
          (resolve-fixed-cost-abi [state side eid card x]
            (continue-ability
              state side
              (if (some #(valid-agenda? state % x) (get-in @state [:runner :scored]))
                {:cost [(->c :tag x) (->c :credit x)]
                 :prompt (str "Choose an agenda with " x " printed agenda points")
                 :choices {:req (req (valid-agenda? state target x))}
                 :async true
                 :effect (req
                           (let [updated-card (update! state side (update target :counter {}))]
                             (corp-install state side eid (get-card state target) nil
                                           {:msg-keys {:install-source card
                                                       :known true
                                                       :include-cost-from-eid eid
                                                       :set-zone "the Runner score area"
                                                       :display-origin true}
                                            :counters {:advance-counter (if tagged 1 0)}})))}
                {:cost [(->c :tag x) (->c :credit x)]
                 :change-in-game-state {:req (req false)}})
              card nil))]
    {:on-play {:prompt "Remove how many tags?"
               :choices {:number (req (min (count-tags state)
                                           (total-available-credits state side
                                                                    (assoc eid :source-type :play)
                                                                    card)))
                         :default (req 0)}
               :async true
               :effect (req (resolve-fixed-cost-abi state side eid card target))
               :cancel-effect (req (resolve-fixed-cost-abi state side eid card 0))}}))


(defcard "IPO"
  {:on-play (gain-credits-ability 13)})

(defcard "Kakurenbo"
  (let [install-abi {:async true
                     :prompt "Choose an agenda, asset or upgrade to install from Archives and place 2 advancement counters on"
                     :show-discard true
                     :not-distinct true
                     :choices {:card #(and (or (agenda? %)
                                               (asset? %)
                                               (upgrade? %))
                                           (in-discard? %))}
                     :effect (req (corp-install state side (assoc eid :source card :source-type :corp-install)
                                                target nil {:counters {:advance-counter 2}
                                                            :msg-keys {:install-source card
                                                                       :display-origin true}}))}]
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
      :cancel-effect (req (system-msg state :corp (str "declines to use " (:title card) " to trash any cards from HQ"))
                          (doseq [c (:discard (:corp @state))]
                            (update! state side (assoc-in c [:seen] false)))
                          (shuffle! state :corp :discard)
                          (continue-ability state side install-abi card nil))}}))

(defcard "Key Performance Indicators"
  {:on-play (choose-one-helper
              {:count 2
               :optional true}
              [{:option "Gain 2 [Credit]"
                :ability (gain-credits-ability 2)}
               {:option "Install 1 piece of ice from HQ, ignoring all costs"
                :req (req (some ice? (:hand corp)))
                :ability {:choices {:card (every-pred ice? corp? in-hand?)}
                          :async true
                          :effect (req (corp-install state side eid target nil {:ignore-all-cost true
                                                                                :install-source card}))}}
               {:option "Place 1 advancement counter"
                :req (req (some can-be-advanced? (all-installed state :corp)))
                :ability {:choices {:req (req (and (corp? target)
                                                   (installed? target)
                                                   (can-be-advanced? state target)))}
                          ;; note - this is sokka's champ card (one of three), so I'm throwing this tiny easter egg in - nbk, 2025
                          :msg (msg "place 1 " (when (= (get-in @state [side :user :username]) "Sokka234") "solid gold ") "advancement counter on " (card-str state target))
                          :async true
                          :effect (req (add-prop state side eid target :advance-counter 1 {:placed true}))}}
               {:option "Draw 1 card. Shuffle 1 card from HQ into R&D"
                :req (req (>= (count (:hand corp)) 1))
                :ability {:msg "draw 1 card"
                          :async true
                          :effect (req (wait-for (draw state side 1)
                                                 (continue-ability
                                                   state side
                                                   {:prompt "Shuffle 1 card into R&D"
                                                    :req (req (seq (:hand corp)))
                                                    :choices {:card (every-pred corp? in-hand?)
                                                              :all true}
                                                    :msg "shuffle 1 card from HQ into R&D"
                                                    :effect (req (move state side target :deck)
                                                                 (shuffle! state :corp :deck))}
                                                   card nil)))}}])})

(defcard "Kill Switch"
  (let [trace-for-brain-damage {:msg (msg "reveal that they accessed " (:title (or (:card context) target)))
                                :trace {:base 3
                                        :req (req (or (agenda? (:card context))
                                                      (agenda? target)))
                                        :successful {:msg "do 1 core damage"
                                                     :async true
                                                     :effect (effect (damage :runner eid :brain 1 {:card card}))}}}]
    {:events [(assoc trace-for-brain-damage
                     :event :access
                     :interactive (req (agenda? target)))
              (assoc trace-for-brain-damage :event :agenda-scored)]}))

(defcard "Lag Time"
  {:on-play {:effect (effect (update-all-ice))}
   :static-abilities [{:type :ice-strength
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
                              :cancel-effect (effect (system-msg (str "declines to use " (:title card) " to install a card"))
                                                     (effect-completed eid))
                              :effect (effect (corp-install eid target nil {:msg-keys {:install-source card
                                                                                       :display-origin true}}))}
                             card nil)))}})

(defcard "Liquidation"
  {:on-play
   {:prompt "Choose any number of rezzed cards to trash"
    :choices {:max (req (count (filter #(not (agenda? %)) (all-active-installed state :corp))))
              :card #(and (rezzed? %)
                          (not (agenda? %)))}
    :change-in-game-state {:req (req (some rezzed? (all-installed state :corp)))}
    :msg (msg "trash " (enumerate-cards targets)
              " and gain " (* (count targets) 3) " [Credits]")
    :async true
    :effect (req (wait-for (trash-cards state side targets {:cause-card card})
                           (gain-credits state side eid (* (count targets) 3))))}})

(defcard "Load Testing"
  {:on-play {:msg (msg "make the Runner lose [Click] when [runner-pronoun] next turn begins")}
   :events [{:event :runner-turn-begins
             :duration :until-runner-turn-begins
             :msg "make the Runner lose [Click]"
             :effect (effect (lose-clicks :runner 1))}]})

(defcard "Localized Product Line"
  {:on-play
   {:prompt "Choose a card"
    :choices (req (cancellable (:deck corp) :sorted))
    :change-in-game-state {:req (req (seq (:deck corp)))}
    :async true
    :effect (effect
              (continue-ability
                (let [title (:title target)
                      copies (filter #(= (:title %) title) (:deck corp))]
                  {:prompt (msg "How many copies of " title " do you want to find?")
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
                     :successful (give-tags 1)}}]})

(defcard "Market Forces"
  {:on-play (assoc (drain-credits :corp :runner (req (* 3 (count-tags state))))
                   :req (req tagged)
                   :change-in-game-state {:req (req (pos? (:credit runner)))})})

(defcard "Mass Commercialization"
  {:on-play
   {:msg (msg "gain " (* 2 (count (filter #(pos? (get-counters % :advancement))
                                          (get-all-installed state)))) " [Credits]")
    :change-in-game-state {:req (req (pos? (count (filter #(pos? (get-counters % :advancement))
                                                          (get-all-installed state)))))}
    :async true
    :effect (effect (gain-credits eid (* 2 (count (filter #(pos? (get-counters % :advancement))
                                                          (get-all-installed state))))))}})

(defcard "MCA Informant"
  {:on-play {:prompt "Choose a connection to host MCA Informant on"
             :change-in-game-state {:req (req (some #(has-subtype? % "Connection") (all-installed state :runner)))}
             :choices {:card #(and (runner? %)
                                   (has-subtype? % "Connection")
                                   (installed? %))}
             :msg (msg "host itself on " (card-str state target) ". The Runner has an additional tag")
             :async true
             :effect (effect (install-as-condition-counter eid card target))}
   :static-abilities [{:type :tags
                       :value 1}]
   :leave-play (req (system-msg state :corp "trashes MCA Informant"))
   :runner-abilities [{:action true
                       :label "Trash MCA Informant host"
                       :cost [(->c :click 1) (->c :credit 2)]
                       :async true
                       :effect (effect (system-msg :runner (str "spends [Click] and 2 [Credits] to trash "
                                                                (card-str state (:host card))))
                                       (trash :runner eid (get-card state (:host card)) {:cause-card (:host card)}))}]})

(defcard "Measured Response"
  {:on-play (choose-one-helper
              {:req (req (and (last-turn? state :runner :successful-run)
                              (threat-level 4 state)))
               :player :runner}
              [(cost-option [(->c :credit 8)] :runner)
               {:option "Corp does 4 meat damage"
                :player :corp
                :ability {:msg "do 4 meat damage"
                          :async true
                          :effect (req (damage state :corp eid :meat 4))}}])})

(defcard "Media Blitz"
  {:on-play
   {:prompt "Choose an agenda in the runner's score area"
    :choices {:req (req (and (agenda? target)
                             (is-scored? state :runner target)))}
    :change-in-game-state {:req (req (seq (:scored runner)))}
    :effect (req (update! state side (assoc card :title (:title target) :abilities (ability-init (card-def target))))
                 (card-init state side (get-card state card) {:resolve-effect false :init-data true})
                 (update! state side (assoc (get-card state card) :title "Media Blitz")))}})

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
     :successful {:msg (msg "give the Runner " (quantify (- target (second targets)) "tag"))
                  :async true
                  :effect (effect (gain-tags eid (- target (second targets))))}}}})

(defcard "Mindscaping"
  {:on-play
   (choose-one-helper
     [{:option "Gain 4 [Credits] and draw 2 cards"
       :ability {:msg "gain 4 [Credits] and draw 2 cards"
                 :async true
                 :effect (req (wait-for (gain-credits state side 4 {:suppress-checkpoint true})
                                        (wait-for
                                          (draw state :corp 2)
                                          (continue-ability
                                            state side
                                            {:req (req (pos? (count (:hand corp))))
                                             :prompt "Choose 1 card to add to the top of R&D"
                                             :waiting-prompt true
                                             :msg "add 1 card from HQ to the top of R&D"
                                             :choices {:card #(and (in-hand? %)
                                                                   (corp? %))
                                                       ;; just incase everything gets jinja'd out of hand
                                                       :all (req (not (zero? (count (:hand corp)))))}
                                             :effect (effect (move target :deck {:front true}))}
                                            card nil))))}}
      {:option "Do 1 net damage per tag (up to 3)"
       :ability {:async true
                 :msg (msg "do " (min 3 (count-tags state)) " net damage")
                 :effect (req (damage state side eid :net (min 3 (count-tags state)) {:card card}))}}])})

(defcard "Mitosis"
  (letfn [(mitosis-ability [state side card eid target-cards]
            (wait-for (corp-install state side (first target-cards) "New remote" {:counters {:advance-counter 2}
                                                                                  :msg-keys {:install-source card
                                                                                             :display-origin true}})
                      (when-let [installed-card async-result]
                        (register-turn-flag!
                          state side
                          card :can-rez
                          (fn [state _ card]
                            (if (same-card? card installed-card)
                              ((constantly false) (toast state :corp "Cannot rez due to Mitosis." "warning"))
                              true)))
                        (register-turn-flag!
                          state side
                          card :can-score
                          (fn [state _ card]
                            (if (same-card? card installed-card)
                              ((constantly false) (toast state :corp "Cannot score due to Mitosis." "warning"))
                              true))))
                      (if (seq (rest target-cards))
                        (mitosis-ability state side card eid (rest target-cards))
                        (effect-completed state side eid))))]
    {:on-play
     {:prompt "Choose 2 cards to install in new remote servers"
      :change-in-game-state {:req (req (seq (:hand corp)))}
      :choices {:card #(and (not (operation? %))
                            (corp? %)
                            (in-hand? %))
                :max 2}
      :async true
      :effect (req (mitosis-ability state side card eid targets))}}))

(defcard "Mushin No Shin"
  {:on-play
   {:prompt "Choose a card to install from HQ"
    :change-in-game-state {:req (req (seq (:hand corp)))}
    :choices {:card #(and (not (operation? %))
                          (corp? %)
                          (in-hand? %))}
    :async true
    :effect (req (wait-for (corp-install state side target "New remote" {:counters {:advance-counter 3}
                                                                         :msg-keys {:install-source card
                                                                                    :display-origin true}})
                           (when-let [installed-card async-result]
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
                                           (effect-completed state side eid))}]))
                           (effect-completed state side eid)))}})

(defcard "Mutate"
  {:on-play
   {:prompt "Choose a rezzed piece of ice to trash"
    :req (req (some (every-pred ice? rezzed?) (all-installed state :corp)))
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
                             (system-msg state side (str "uses " (:title card) " to trash " (:title target)))
                             (wait-for
                               (reveal state side revealed-cards)
                               (system-msg state side (str "reveals " (enumerate-str titles) " from R&D"))
                               (let [ice (first r)
                                     zone (zone->name (second (get-zone target)))]
                                 (if ice
                                   (do (system-msg state side (str "uses " (:title card) 
                                                                   " to install and rez "
                                                                   (:title ice)
                                                                   " from R&D at no cost"))
                                       (corp-install state side eid ice zone {:ignore-all-cost true
                                                                              :install-state :rezzed-no-cost
                                                                              :display-message false
                                                                              :origin-index index}))
                                   (do (system-msg state side (str "uses " (:title card) " to shuffle R&D"))
                                       (effect-completed state side eid))))))))}})

(defcard "Mutually Assured Destruction"
  {:on-play
   {:prompt "Choose any number of rezzed cards to trash"
    :interactive (req true)
    :change-in-game-state {:req (req (some rezzed? (all-installed state :corp)))}
    :choices {:max (req (count (filter #(not (agenda? %)) (all-active-installed state :corp))))
              :card #(and (rezzed? %)
                          (not (agenda? %)))}
    :msg (msg "trash " (enumerate-cards targets :sorted)
              " and give the runner " (quantify (count targets) "tag"))
    :async true
    :effect (req (wait-for (trash-cards state side targets {:cause-card card})
                           (gain-tags state :corp eid (count targets))))}})

(defcard "Nanomanagement"
  {:on-play (gain-n-clicks 2)})

(defcard "NAPD Cordon"
  (lockdown
    {:static-abilities
     [{:type :steal-additional-cost
       :value (req (->c :credit (+ 4 (* 2 (get-counters target :advancement)))))}]}))

(defcard "Net Watchlist"
  {:implementation "Only modifies ability costs, does not adjust non-ability uses"
   :static-abilities [{:type :card-ability-additional-cost
                       :req (req (and (has-subtype? (:card context) "Icebreaker")
                                      (not (get-in context [:ability :break]))))
                       :value (->c :credit 2)}
                      {:type :break-sub-additional-cost
                       :req (req (has-subtype? (:card context) "Icebreaker"))
                       :value (->c :credit 2)}]})

(defcard "Neural EMP"
  {:on-play
   {:req (req (last-turn? state :runner :made-run))
    :msg "do 1 net damage"
    :async true
    :effect (effect (damage eid :net 1 {:card card}))}})

(defcard "Neurospike"
  {:on-play
   {:msg (msg "do " (:scored-agenda corp-reg 0) " net damage")
    :change-in-game-state {:req (req (pos? (:scored-agenda corp-reg 0)))}
    :async true
    :effect (effect (damage eid :net (:scored-agenda corp-reg 0) {:card card}))}})

(defcard "NEXT Activation Command"
  (lockdown
   {:static-abilities [{:type :ice-strength
                        :value 2}
                       {:type :prevent-paid-ability
                        :req (req (let [target-card (first targets)
                                        ability (second targets)]
                                    (and (not (has-subtype? target-card "Icebreaker"))
                                         (:break ability))))
                        :value true}]}))

(defcard "Nonequivalent Exchange"
  {:on-play
   {:optional
    {:prompt "Have each player gain 2 [Credits]?"
     :waiting-prompt true
     :yes-ability
     {:msg "gain 7 [Credits]. The Runner gains 2 [Credits]"
      :async true
      :effect (req (wait-for (gain-credits state side 7)
                             (gain-credits state :runner eid 2)))}
     :no-ability
     {:msg "gain 5 [Credits]"
      :async true
      :effect (effect (gain-credits eid 5))}}}})

(defcard "O₂ Shortage"
  {:on-play
   (choose-one-helper
     {:player :runner}
     [(cost-option [(->c :randomly-trash-from-hand 1)] :runner)
      {:option "The Corp gains [Click][Click]"
       :player :corp
       :ability (gain-n-clicks 2)}])})

(defcard "Observe and Destroy"
  {:on-play (trash-type "installed" installed? :loud 1 :all {:additional-cost [(->c :tag 1)]
                                                             :req (req (< (:credit runner) 6))})})

(defcard "Oppo Research"
  {:on-play {:msg "give the Runner 2 tags"
             :async true
             :req (req (or (last-turn? state :runner :trashed-card)
                           (last-turn? state :runner :stole-agenda)))
             :effect (req
                       (wait-for (gain-tags state :corp (make-eid state eid) 2)
                                 (continue-ability
                                     state side
                                     {:optional
                                      {:prompt "Pay 5 [Credit] to give the Runner 2 tags?"
                                       :req (req (threat-level 3 state))
                                       :waiting-prompt true
                                       :yes-ability {:async true
                                                     :cost [(->c :credit 5)]
                                                     :msg "give the Runner 2 tags"
                                                     :effect (req (gain-tags state :corp eid 2))}}}
                                     card nil)))}})

(defcard "Oversight AI"
  {:on-play {:choices {:card #(and (ice? %)
                                   (not (rezzed? %))
                                   (= (last (get-zone %)) :ices))}
             :change-in-game-state {:req (req (some (every-pred ice? (complement rezzed?))
                                                    (all-installed state :corp)))}
             :async true
             :effect (req (wait-for (rez state side target {:ignore-cost :all-costs})
                                    (install-as-condition-counter state side eid card (:card async-result))))}
   :events [{:event :subroutines-broken
             :condition :hosted
             :async true
             :req (req (and (same-card? (:ice context) (:host card))
                            (:all-subs-broken context)))
             :msg (msg "trash " (card-str state (:ice context)))
             :effect (effect (trash :corp eid (:ice context) {:unpreventable true :cause-card card}))}]})

(defcard "Patch"
  {:on-play {:choices {:card #(and (ice? %)
                                   (rezzed? %))}
             :change-in-game-state {:req (req (some (every-pred ice? rezzed?) (all-installed state :corp)))}
             :msg (msg "give +2 strength to " (card-str state target))
             :async true
             :effect (effect (install-as-condition-counter eid card target))}
   :static-abilities [{:type :ice-strength
                       :req (req (same-card? target (:host card)))
                       :value 2}]})

(defcard "Paywall Implementation"
  {:events [{:event :successful-run
             :automatic :gain-credits
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits :corp eid 1))}]})

(defcard "Peak Efficiency"
  {:on-play
   {:msg (msg "gain " (reduce (fn [c server]
                                (+ c (count (filter (fn [ice] (:rezzed ice)) (:ices server)))))
                              0 (flatten (seq (:servers corp))))
              " [Credits]")
    :change-in-game-state {:req (req (some (every-pred ice? rezzed?) (all-installed state :corp)))}
    :async true
    :effect (effect (gain-credits
                      eid
                      (reduce (fn [c server]
                                (+ c (count (filter (fn [ice] (:rezzed ice)) (:ices server)))))
                              0 (flatten (seq (:servers corp))))))}})

(defcard "Peer Review"
  (let [gain-abi {:msg (msg "gain 7 [credits]")
                  :async true
                  :effect (req (wait-for
                                 (gain-credits state side 7)
                                 (continue-ability
                                   state side
                                   {:prompt "Install a card from HQ in the root of a remote server"
                                    ;; todo - filter out "cental only" cards
                                    :choices {:card #(and (in-hand? %)
                                                          (corp? %)
                                                          (not (ice? %))
                                                          (not (operation? %)))}
                                    :async true
                                    :waiting-prompt true
                                    :effect (req (wait-for (resolve-ability
                                                             state side
                                                             (let [card-to-install target]
                                                               {:prompt "Choose a server"
                                                                :choices (remove #{"HQ" "R&D" "Archives"} (installable-servers state card-to-install))
                                                                :async true
                                                                :effect (effect (corp-install eid card-to-install target nil))})
                                                             target nil)
                                                           (effect-completed state side eid)))}
                                   card nil)))}]
    {:on-play {:async true
               :effect (effect (continue-ability
                                (if (>= (count (:hand corp)) 2)
                                  {:prompt "Choose a card in HQ to keep private"
                                   :choices {:req (req (and (in-hand? target)
                                                            (corp? target)))
                                             :all true}
                                   :async true
                                   :effect (req (wait-for
                                                  (reveal-loud state side card nil (remove #(same-card? % target) (:hand corp)))
                                                  (continue-ability state side gain-abi card nil)))}
                                  gain-abi)
                                card nil))}}))

(defcard "Petty Cash"
  {:flashback [(->c :click 1)]
   :on-play {:msg "gain 5 [credits]"
             :async true
             :req (req (no-event? state side :action-resolved))
             :effect (req (wait-for
                            (gain-credits state side 5)
                            (continue-ability
                              state side
                              (when-not (some #{:hand} (:previous-zone card))
                                (gain-n-clicks 1))
                              card nil)))}})

(defcard "Pivot"
  ;; todo - it might be possible to pre-check the additional costs (consulting never did this)
  {:on-play {:prompt "Choose a card"
             ;; we need the req or the prompt will still show
             :waiting-prompt true
             :msg (msg "reveal " (:title target) " from R&D and add it to HQ")
             :choices (req (sort-by :title (filter #(or (operation? %) (agenda? %)) (:deck corp))))
             :change-in-game-state {:req (req (or (seq (:deck corp))
                                                  (and (threat-level 3 state)
                                                       (seq (:hand corp)))))}
             :async true
             :effect (req (wait-for (reveal state side target)
                                    (shuffle! state :corp :deck)
                                    (move state side target :hand)
                                    (if (threat-level 3 state)
                                      (continue-ability
                                        state side
                                        {:prompt "Choose a card to play or install"
                                         :waiting-prompt true
                                         :choices {:card #(and (corp? %)
                                                               (in-hand? %)
                                                               (if (operation? %)
                                                                 (<= (:cost %) (:credit corp))
                                                                 true))}
                                         :async true
                                         :effect (req (let [target-card target]
                                                        (if (operation? target-card)
                                                          (continue-ability
                                                            state side
                                                            {:async true
                                                             :msg (msg "play " (:title target-card))
                                                             :effect (req (play-instant state side eid target-card nil))}
                                                            card nil)
                                                          (continue-ability
                                                            state side
                                                            {:async true
                                                             :effect (effect (corp-install eid target-card nil {:msg-keys {:install-source card
                                                                                                                           :display-origin true}}))}
                                                            card nil))))}
                                        card nil)
                                      (effect-completed state side eid))))}})

(defcard "Power Grid Overload"
  {:on-play
   {:trace
    {:base 2
     :req (req (last-turn? state :runner :made-run))
     :successful {:async true
                  :effect (req (let [max-cost (- target (second targets))]
                                 (continue-ability
                                   state side
                                   (trash-type (str "piece of hardware that costs " max-cost " or less")
                                               #(and (hardware? %)
                                                     (<= (:cost %) max-cost))
                                               :loud)
                                   card nil)))}}}})

(defcard "Power Shutdown"
  {:on-play
   {:req (req (last-turn? state :runner :made-run))
    :prompt "How many cards do you want to trash from the top of R&D?"
    :waiting-prompt true
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
    :change-in-game-state {:req (req (seq (:deck corp)))}
    :waiting-prompt true
    :async true
    :effect (effect (continue-ability
                      (let [from (take 5 (:deck corp))]
                        (when (pos? (count from))
                          (reorder-choice :corp :runner from '()
                                          (count from) from)))
                      card nil))}})

(defcard "Predictive Algorithm"
  {:static-abilities [{:type :steal-additional-cost
                       :value (req (->c :credit 2))}]})

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
             :change-in-game-state {:req (req (seq (:discard corp)))}
             :async true
             :effect (effect (shuffle-into-rd-effect eid card 3 true))}})

(defcard "Priority Construction"
  (letfn [(install-card [chosen]
            {:prompt "Choose a remote server"
             :choices (req (conj (vec (get-remote-names state)) "New remote"))
             :async true
             :effect (effect (corp-install eid chosen target {:ignore-all-cost true
                                                              :counters {:advance-counter 3}
                                                              :msg-keys {:install-source card
                                                                         :display-origin true}}))})]
    {:on-play
     {:prompt "Choose a piece of ice in HQ to install"
      :change-in-game-state {:req (req (seq (:hand corp)))}
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
    :change-in-game-state {:req (req (some #(and (rezzed? %) (or (asset? %) (upgrade? %)))
                                           (all-installed state :corp)))}
    :choices {:card #(and (rezzed? %)
                          (or (asset? %)
                              (upgrade? %)))}
    :msg (msg "trash " (card-str state target)
              " and gain " (trash-cost state side target) " [Credits]")
    :async true
    :effect (req (wait-for (trash state side target {:unpreventable true :cause-card card})
                           (gain-credits state :corp eid (trash-cost state side target))))}})

(defcard "Psychographics"
  {:on-play {:change-in-game-state {:req (req (and tagged (pos? (x-cost-value eid))))}
             :waiting-prompt true
             :base-play-cost [(->c :x-credits 0 {:maximum (req (count-tags state))})]
             :choices {:req (req (can-be-advanced? state target))}
             :msg (msg "place " (quantify (x-cost-value eid) " advancement counter") " on " (card-str state target))
             :async true
             :effect (req (add-prop state side eid target :advance-counter (x-cost-value eid) {:placed true}))}})

(defcard "Psychokinesis"
  (letfn [(install-card [chosen]
            {:prompt "Choose a remote server"
             :waiting-prompt true
             :choices (req (conj (vec (get-remote-names state)) "New remote"))
             :async true
             :effect (effect (corp-install eid chosen target {:msg-keys {:install-source card
                                                                         :origin-index (first (positions #{chosen} (take 5 (:deck corp))))
                                                                         :display-origin true}}))})]
    {:on-play
     {:async true
      :change-in-game-state {:req (req (seq (:deck corp)))}
      :msg "look at the top 5 cards of R&D"
      :effect
      (effect
        (continue-ability
          (let [top-five (take 5 (:deck corp))]
            {:prompt (str "The top cards of R&D are (top->bottom): " (enumerate-cards top-five))
             :waiting-prompt true
             :choices ["OK"]
             :async true
             :effect
             (effect
               (continue-ability
                 {:prompt "Choose an agenda, asset, or upgrade to install"
                  :waiting-prompt true
                  :async true
                  :choices
                  (cancellable (filter #(and (corp-installable-type? %)
                                             (some #{"New remote"} (installable-servers state %)))
                                       top-five))
                  :effect (effect (continue-ability (install-card target) card nil))
                  :cancel-effect (effect (system-msg (str "declines to use " (get-title card) " to install a card from the top of R&D"))
                                         (effect-completed eid))}
                 card nil))})
          card nil))}}))


(defcard "Public Trail"
  {:on-play (choose-one-helper
              {:req (req (last-turn? state :runner :successful-run))
               :player :runner}
              [{:option "Take 1 tag"
                :ability {:async true
                          :display-side :corp
                          :msg "give the runner 1 tag"
                          :effect (req (gain-tags state :corp eid 1))}}
               (cost-option [(->c :credit 8)] :runner)])})

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
     {:base-play-cost [(->c :x-credits)]
      :msg (msg "search for " (x-cost-value eid) " Sysops")
      :async true
      :effect (effect (continue-ability (rthelp (x-cost-value eid) (x-cost-value eid) []) card nil))}}))

(defcard "Red Level Clearance"
  (let [all [{:msg "gain 2 [Credits]"
              :async true
              :effect (effect (gain-credits eid 2))}
             {:msg "draw 2 cards"
              :async true
              :effect (effect (draw eid 2))}
             (gain-n-clicks 1)
             {:prompt "Choose a non-agenda to install"
              :msg "install a non-agenda from hand"
              :choices {:card #(and (not (agenda? %))
                                    (corp-installable-type? %)
                                    (in-hand? %))}
              :async true
              :effect (effect (corp-install eid target nil {:msg-keys {:install-source card
                                                                       :display-origin true}}))}]
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
  (letfn [(clear-counters [state side eid [c :as installed]]
            (if (seq installed)
              (wait-for (add-prop state side c :advance-counter (- (get-counters c :advancement)) {:placed true :suppress-checkpoint true})
                        (clear-counters state side eid (rest installed)))
              (effect-completed state side eid)))]
    {:on-play
     {:prompt "Choose an installed card that can be advanced"
      :choices {:req (req (can-be-advanced? state target))}
      :change-in-game-state {:req (req (something-can-be-advanced? state))}
      :async true
      :effect (req (let [installed (get-all-installed state)
                         total-adv (reduce + (map #(get-counters % :advancement) installed))]
                     (wait-for
                       (clear-counters state side installed)
                       (wait-for
                         (add-prop state side target :advance-counter total-adv {:placed true})
                         (update-all-ice state side)
                         (system-msg state side (str "uses " (:title card) " to move "
                                                     (quantify total-adv "advancement counter")
                                                     " to " (card-str state target)))
                         (effect-completed state side eid)))))}}))

(defcard "Replanting"
  (letfn [(replant [n]
            {:prompt "Choose a card to install"
             :async true
             :choices {:card #(and (corp? %)
                                   (not (operation? %))
                                   (in-hand? %))}
             :effect (req (wait-for (corp-install state side target nil {:ignore-all-cost true
                                                                         :msg-keys {:install-source card
                                                                                    :display-origin true}})
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
                   (corp-install state side target nil {:install-state :rezzed
                                                        :msg-keys {:install-source card
                                                                   :display-origin true}})
                   (let [leftover (filter #(= (:title target) (:title %)) (-> @state :corp :discard))]
                     (when (seq leftover)
                       (doseq [c leftover]
                         (move state side c :rfg))
                       (system-msg state side (str "removes " (count leftover) " copies of " (:title target) " from the game"))))
                   (effect-completed state side eid)))}})

(defcard "Restoring Face"
  {:on-play
   {:prompt "Choose a Sysop, Executive or Clone to trash"
    :msg (msg "trash " (:title target) " to remove 2 bad publicity")
    :choices {:card #(has-any-subtype? % ["Clone" "Executive" "Sysop"])}
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
  {:on-play (gain-credits-ability 15)})

(defcard "Retribution"
  {:on-play (trash-type "program of piece of hardware" #(or (program? %) (hardware? %))
                        :loud 1 nil {:req (req tagged)})})

(defcard "Reuse"
  {:on-play
   {:prompt (msg "Choose up to " (quantify (count (:hand corp)) "card") " in HQ to trash")
    :change-in-game-state {:req (req (seq (:hand corp)))}
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
                             (system-msg state side (str "uses " (:title card) " to gain 2 [Credits]"))
                             (effect-completed state side eid))
                   (let [pre-purge-virus (number-of-virus-counters state)]
                     (wait-for
                       (purge state side)
                       (let [post-purge-virus (number-of-virus-counters state)
                             num-virus-purged (- pre-purge-virus post-purge-virus)
                             num-to-trash (quot num-virus-purged 3)]
                         (wait-for (mill state :corp :runner num-to-trash)
                                   (system-msg state side
                                               (str "uses " (:title card) " to purge "
                                                    (quantify num-virus-purged "virus counter")
                                                    " and trash "
                                                    (quantify num-to-trash "card")
                                                    " from the top of the stack"))
                                   (effect-completed state side eid)))))))}})

(defcard "Rework"
  {:on-play
   {:prompt "Choose a card from HQ to shuffle into R&D"
    :change-in-game-state {:req (req (seq (:hand corp)))}
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
    :choices ["Suffer 1 core damage" "Get 3 fewer [Click] on the next turn"]
    :effect (req (if (= target "Suffer 1 core damage")
                   (pay state :runner eid card [(->c :brain 1)])
                   (do (swap! state update-in [:runner :extra-click-temp] (fnil #(- % 3) 0))
                       (effect-completed state side eid))))}})

(defcard "Rolling Brownout"
  {:on-play {:msg "increase the play cost of operations and events by 1 [Credits]"}
   :static-abilities [{:type :play-cost
                       :value 1}]
   :events [{:event :play-event
             :req (req (first-event? state side :play-event))
             :msg "gain 1 [Credits]"
             :async true
             :effect (effect (gain-credits :corp eid 1))}]})

(defcard "Rover Algorithm"
  {:on-play {:choices {:card #(and (ice? %)
                                   (rezzed? %))}
             :change-in-game-state {:req (req (some (every-pred ice? rezzed?) (all-installed state :corp)))}
             :msg (msg "host itself as a condition counter on " (card-str state target))
             :async true
             :effect (effect (install-as-condition-counter eid card target))}
   :static-abilities [{:type :ice-strength
                       :req (req (same-card? target (:host card)))
                       :value (req (get-counters card :power))}]
   :events [{:event :pass-ice
             :condition :hosted
             :req (req (same-card? (:ice context) (:host card)))
             :msg "place 1 power counter on itself"
             :async true
             :effect (effect (add-counter eid card :power 1 nil))}]})

(defcard "Sacrifice"
  {:on-play
   {:additional-cost [(->c :forfeit)]
    :async true
    :change-in-game-state {:req (req (pos? (count-bad-pub state)))}
    :effect (req (let [bp-lost (max 0 (min (:agendapoints (last (:rfg corp)))
                                           (count-bad-pub state)))]
                   (system-msg state side (str "uses " (:title card) " to lose " bp-lost
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
    :msg (msg "reveal "(enumerate-cards (:hand runner) :sorted)
              " from the grip and trash any copies of " target)
    :effect (req (let [cards (filter #(= target (:title %)) (:hand runner))]
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
      :effect (effect (move :runner target :rfg))}}}})

(defcard "Scarcity of Resources"
  {:on-play {:msg "increase the install cost of resources by 2"}
   :static-abilities [{:type :install-cost
                       :req (req (resource? target))
                       :value 2}]})

(defcard "Scorched Earth"
  {:on-play (assoc (do-meat-damage 4) :req (req tagged))})

(defcard "SEA Source"
  {:on-play
   {:trace
    {:base 3
     :req (req (last-turn? state :runner :successful-run))
     :label "Trace 3 - Give the Runner 1 tag"
     :successful (give-tags 1)}}})

(defcard "Seamless Launch"
  {:on-play
   {:prompt "Choose an installed card"
    :change-in-game-state {:req (req (some #(and (corp? %)
                                                 (installed? %)
                                                 (not= :this-turn (installed? %)))
                                           (all-installed state :corp)))}
    :choices {:card #(and (corp? %)
                          (installed? %)
                          (not= :this-turn (installed? %)))}
    :msg (msg "place 2 advancement counters on " (card-str state target))
    :async true
    :effect (effect (add-prop eid target :advance-counter 2 {:placed true}))}})

(defcard "Secure and Protect"
  {:on-play
   {:interactive (req true)
    :change-in-game-state {:req (req (seq (:deck corp)))}
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
                             :msg (msg "reveal " (:title chosen-ice) " from R&D and install it, paying 3 [Credit] less")
                             :effect (req (wait-for
                                            (reveal state side chosen-ice)
                                            (shuffle! state side :deck)
                                            (corp-install state side eid chosen-ice target {:cost-bonus -3
                                                                                            :msg-keys {:install-source card
                                                                                                       :display-origin true}})))})
                          card nil))}
                     card nil)
                   (do (shuffle! state side :deck)
                       (effect-completed state side eid))))}})

(defcard "Self-Growth Program"
  {:on-play
   {:req (req tagged)
    :prompt "Choose 2 installed Runner cards"
    :change-in-game-state {:req (req (seq (all-installed state :runner)))}
    :choices {:card #(and (installed? %)
                          (runner? %))
              :max 2}
    :msg (msg "move " (enumerate-cards targets) " to the grip")
    :effect (req (doseq [c targets]
                   (move state :runner c :hand)))}})

(defcard "Service Outage"
  {:on-play {:msg "add a cost of 1 [Credit] for the Runner to make the first run each turn"}
   :static-abilities [{:type :run-additional-cost
                       :req (req (no-event? state side :run))
                       :value [(->c :credit 1)]}]})

(defcard "Shipment from Kaguya"
  {:on-play
   {:choices {:max 2
              :req (req (and (corp? target)
                             (installed? target)
                             (can-be-advanced? state target)))}
    :change-in-game-state {:req (req (something-can-be-advanced? state))}
    :msg (msg "place 1 advancement counters on " (quantify (count targets) "card"))
    :async true
    :effect (req (let [[f1 f2] targets]
                               (if f2
                                 (wait-for (add-prop state :corp f1 :advance-counter 1 {:placed true})
                                           (add-prop state :corp eid f2 :advance-counter 1 {:placed true}))
                                 (add-prop state :corp eid f1 :advance-counter 1 {:placed true}))))}})

(defcard "Shipment from MirrorMorph"
  {:on-play (corp-install-up-to-n-cards 3)})

(defcard "Shipment from SanSan"
  {:on-play
   {:choices ["0" "1" "2"]
    :prompt "How many advancement counters do you want to place?"
    :change-in-game-state {:req (req (something-can-be-advanced? state))}
    :async true
    :effect (req (let [c (str->int target)]
                   (continue-ability
                     state side
                     (place-n-advancement-counters c :can-be-advanced)
                     card nil)))}})

(defcard "Shipment from Tennin"
  {:on-play (assoc (place-n-advancement-counters 2)
                   :req (req (not-last-turn? state :runner :successful-run)))})

(defcard "Shipment from Vladisibirsk"
  (letfn [(ability [x]
            {:prompt (msg "Choose an installed card to place advancement counters on (" x " remaining)")
             :async true
             :waiting-prompt true
             :choices {:card #(and (corp? %)
                                   (installed? %))}
             :msg (msg "place 1 advancement counter on " (card-str state target))
             :effect (req (wait-for (add-prop state side target :advance-counter 1 {:placed true})
                                    (if (> x 1)
                                      (continue-ability state side (ability (dec x)) card nil)
                                      (effect-completed state side eid))))})]
    {:on-play
     {:async true
      :req (req (<= 2 (count-tags state)))
      :change-in-game-state {:req (req (something-can-be-advanced? state))}
      :effect (effect (continue-ability (ability 4) card nil))}}))

(defcard "Shoot the Moon"
  {:on-play
   {:req (req tagged)
    :change-in-game-state {:req (req (some (every-pred ice? (complement rezzed?)) (all-installed state :corp)))}
    :choices {:card #(and (ice? %)
                          (not (rezzed? %)))
              :max (req (min (count-tags state)
                             (reduce (fn [c server]
                                       (+ c (count (filter #(not (:rezzed %)) (:ices server)))))
                                     0 (flatten (seq (:servers corp))))))}
    :async true
    :effect (req (rez-multiple-cards state side eid targets {:ignore-cost :all-costs}))}})

(defcard "Simulation Reset"
  {:on-play
   {:rfg-instead-of-trashing true
    :prompt "Choose up to 5 cards in HQ to trash"
    :change-in-game-state {:req (req (seq (:hand corp)))}
    :waiting-prompt true
    :choices {:max (req 5)
              :card #(and (corp? %)
                          (in-hand? %))}
    :async true
    :msg (msg "trash " (quantify (count targets) "card") " from HQ")
    :effect (req (let [n (count targets)
                       t targets]
                   (wait-for (resolve-ability state side
                                              {:async true
                                               :effect
                                               (req (wait-for (trash-cards state side t
                                                                           {:unpreventable true :cause-card card})
                                                              (shuffle-into-rd-effect state side eid card (count t) true)))}
                                              card nil)
                             (continue-ability state side
                                               {:msg (msg "draw " (quantify n "card"))
                                                :async true
                                                :effect (effect (draw eid n))}
                                               card nil))))}})

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
    :change-in-game-state {:req (req (seq (:hand corp)))}
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
    :msg "draw 3 cards"
    :effect (req (wait-for
                   (draw state side 3)
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
              ", reveal " (enumerate-cards (:hand runner) :sorted)
              " from the grip, and gain "
              (* 2 (count (filter #(is-type? % target) (:hand runner)))) " [Credits]")
    :async true
    :effect (req (wait-for
                   (reveal state side (:hand runner))
                   (gain-credits state :corp eid (* 2 (count (filter #(is-type? % target) (:hand runner)))))))}})

(defcard "Stock Buy-Back"
  {:on-play
   {:msg (msg "gain " (* 3 (count (:scored runner))) " [Credits]")
    :change-in-game-state {:req (req (seq (:scored runner)))}
    :async true
    :effect (effect (gain-credits eid (* 3 (count (:scored runner)))))}})

(defcard "Sudden Commandment"
  (let [play-instant-second {:optional
                             {:prompt "Pay 3 [Credits] to gain [Click]?"
                              :waiting-prompt true
                              :req (req (threat-level 3 state))
                              :yes-ability {:cost [(->c :credit 3)]
                                            :msg "gain [Click]"
                                            :effect (effect (gain-clicks 1))}}}
        play-instant-first {:prompt "Choose a non-terminal operation"
                            :choices (req (conj (filter #(and (operation? %)
                                                              (not (has-subtype? % "Terminal"))
                                                              (should-trigger? state :corp (assoc eid :source % :source-type :play) % nil (or (:on-play (card-def %)) {}))
                                                              (can-pay? state side (assoc eid :source % :source-type :play) % nil [(->c :credit (play-cost state side % nil))]))
                                                        (:hand corp))
                                                "Done"))
                            :async true
                            :effect (req
                                      (let [is-first-mandate? (first-event? state side :play-operation #(has-subtype? (:card (first %)) "Mandate"))]
                                        (if (= target "Done")
                                          (continue-ability state side (when is-first-mandate? play-instant-second) card nil)
                                          (wait-for (play-instant state side (make-eid state eid) target nil)
                                                    (continue-ability state side (when is-first-mandate? play-instant-second) card nil)))))}]
    {:on-play {:msg "draw 2 cards"
               :async true
               :effect (req (wait-for (draw state side (make-eid state eid) 2)
                                      (continue-ability
                                        state side
                                        play-instant-first
                                        card nil)))}}))

(defcard "Sub Boost"
  {:on-play {:choices {:card #(and (ice? %)
                                   (rezzed? %))}
             :change-in-game-state {:req (req (some (every-pred ice? rezzed?) (all-installed state :corp)))}
             :msg (msg "make " (card-str state target) " gain Barrier and \"[Subroutine] End the run\"")
             :async true
             :effect (req (install-as-condition-counter state side eid card (get-card state target)))}
   :static-abilities [{:type :gain-subtype
                       :req (req (and (same-card? target (:host card))
                                      (rezzed? target)))
                       :value "Barrier"}
                      {:type :additional-subroutines
                       :req (req (and (same-card? target (:host card))
                                      (rezzed? target)))
                       :value {:subroutines [{:label "[Sub Boost] End the run"
                                              :msg "end the run"
                                              :async true
                                              :effect (effect (end-run eid card))}]}}]})

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
      :change-in-game-state {:req (req (seq (:hand corp)))}
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
   :highlight-in-discard true
   :events [{:event :corp-phase-12
             :location :discard
             :optional
             {:req (req (not-last-turn? state :runner :made-run))
              :prompt (msg "Add " (:title card) " to HQ?")
              :yes-ability
              {:msg "reveal and add itself to HQ"
               :async true
               :effect (req (wait-for (reveal state side card)
                                      (move state side card :hand)
                                      (effect-completed state side eid)))}}}]})

(defcard "Success"
  (letfn [(advance-n-times [state side eid card target n]
            (if (pos? n)
              (wait-for (advance state :corp (make-eid state {:source card}) (get-card state target) :no-cost)
                        (advance-n-times state side eid card target (dec n)))
              (effect-completed state side eid)))]
    {:on-play
     {:additional-cost [(->c :forfeit)]
      :choices {:req (req (can-be-advanced? state target))}
      :change-in-game-state {:req (req (something-can-be-advanced? state))}
      :msg (msg "advance " (card-str state target)
             " " (quantify (get-advancement-requirement (cost-target eid :forfeit)) "time"))
      :async true
      :effect (effect (advance-n-times eid card target (get-advancement-requirement (cost-target eid :forfeit))))}}))

(defcard "Successful Demonstration"
  {:on-play (assoc (gain-credits-ability 7) :req (req (last-turn? state :runner :unsuccessful-run)))})

(defcard "Sunset"
  (letfn [(sun [serv]
            {:prompt "Choose 2 pieces of ice to swap"
             :choices {:card #(and (= serv (get-zone %))
                                   (ice? %))
                       :max 2}
             :async true
             :effect (req (if (= (count targets) 2)
                            (do (swap-ice state side (first targets) (second targets))
                                (system-msg state side
                                            (str "uses " (:title card) " to swap "
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
  {:static-abilities
   [{:type :trace-runner-spends-first
     :req (req run)
     :value true}]})

(defcard "Sweeps Week"
  {:on-play
   {:msg (msg "gain " (count (:hand runner)) " [Credits]")
    :change-in-game-state {:req (req (seq (:hand runner)))}
    :async true
    :effect (effect (gain-credits eid (count (:hand runner))))}})

(defcard "SYNC Rerouting"
  (lockdown
   {:events [(choose-one-helper
               {:event :run
                :player :runner}
               [{:option "Take 1 tag"
                 :ability {:async true
                           :display-side :corp
                           :msg "give the runner 1 tag"
                           :effect (req (gain-tags state :corp eid 1))}}
                (cost-option [(->c :credit 4)] :runner)])]}))

(defcard "Targeted Marketing"
  (let [gaincr {:req (req (= (:title (:card context)) (:card-target card)))
                :async true
                :msg "gain 10 [Credits]"
                :effect (effect (gain-credits :corp eid 10))}]
    {:on-play {:prompt "Name a Runner card"
               :choices {:card-title (req (and (runner? target)
                                               (not (identity? target))))}
               :effect (effect (update! (assoc card :card-target target))
                               (system-msg (str "uses " (:title card) " to name " target)))}
     :events [(assoc gaincr :event :runner-install)
              (assoc gaincr :event :play-event)]}))

(defcard "The All-Seeing I"
  (let [trash-all-resources
        {:msg "trash all resources"
         :async true
         :effect (effect (trash-cards :corp eid (filter resource? (all-active-installed state :runner)) {:cause-card card}))}]
    {:on-play
     {:req (req tagged)
      :change-in-game-state {:req (req (some resource? (all-active-installed state :runner)))}
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
                     (wait-for (gain-tags state :corp (make-eid state eid) tags)
                               (system-msg
                                 state side
                                 (str "uses " (:title card) " to give the Runner " (quantify tags "tag")))
                               (effect-completed state nil eid))))}}}})

(defcard "Too Big to Fail"
  {:on-play
   {:req (req (< (:credit corp) 10))
    :msg "gain 7 [Credits] and take 1 bad publicity"
    :async true
    :effect (req (wait-for (gain-credits state side 7 {:suppress-checkpoint true})
                           (gain-bad-publicity state :corp eid 1)))}})

(defcard "Top-Down Solutions"
  {:on-play
   {:async true
    :msg "draw 2 cards"
    :effect (req (wait-for
                   (draw state side 2)
                   (continue-ability state side (corp-install-up-to-n-cards 2) card nil)))}})

(defcard "Traffic Accident"
  {:on-play
   {:req (req (<= 2 (count-tags state)))
    :msg "do 2 meat damage"
    :async true
    :effect (effect (damage eid :meat 2 {:card card}))}})

(defcard "Transparency Initiative"
  {:static-abilities [{:type :gain-subtype
                       :req (req (and (same-card? target (:host card))
                                      (rezzed? target)))
                       :value "Public"}]
   :on-play {:choices {:card #(and (agenda? %)
                                   (installed? %)
                                   (not (faceup? %)))}
             :change-in-game-state {:req (req (some (every-pred (complement faceup?) (complement ice?)) (all-installed state :corp)))}
             :async true
             :effect (req (let [target (update! state side (assoc target
                                                                  :seen true
                                                                  :rezzed true))]
                            (wait-for (install-as-condition-counter state side card target)
                                      (let [card async-result]
                                        (register-events
                                          state side card
                                          [{:event :advance
                                            :condition :hosted
                                            :req (req (same-card? (:host card) (:card context)))
                                            :async true
                                            :msg "gain 1 [Credit]"
                                            :effect (effect (gain-credits eid 1))}]))
                                      (effect-completed state side eid))))}})

(defcard "Trick of Light"
  {:on-play
   {:prompt "Choose an installed card you can advance"
    :choices {:req (req (and (can-be-advanced? state target)
                             (installed? target)))}
    :change-in-game-state {:req (req (something-can-be-advanced? state))}
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
                                  :async true
                                  :effect (req (wait-for
                                                 (add-prop state :corp card-to-advance :advance-counter (str->int target) {:placed true :suppress-checkpoint true})
                                                 (add-prop state :corp eid source :advance-counter (- (str->int target)) {:placed true})))})
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
                 :cancel-effect (effect (system-msg (str "declines to use " (:title card) " to install a card"))
                                        (effect-completed eid))
                 :effect (effect (corp-install eid target nil {:ignore-all-cost true
                                                               :msg-keys {:install-source card
                                                                          :display-origin true}
                                                               :install-state :rezzed-no-cost}))}]
    {:on-play {:req (req tagged)
               :msg (msg "trash " (:title target))
               :change-in-game-state {:req (req (or (some resource? (all-active-installed state :runner))
                                                    (some #(or (not (operation? %)) (not (:seen %))) (:discard corp))))}
               :prompt "Choose a resource to trash"
               :choices {:card #(and (installed? %)
                                     (resource? %))}
               :async true
               :cancel-effect (effect
                               (system-msg (str "declines to use " (:title card) " to trash a resource"))
                               (continue-ability ability card nil))
               :effect (req (wait-for (trash state side target {:cause-card card})
                                      (continue-ability state side ability card nil)))}}))

(defcard "Touch-ups"
  (let [name-abi {:prompt "Choose a card type"
                  :waiting-prompt true
                  :choices ["Event" "Hardware" "Program" "Resource"]
                  :async true
                  :msg (msg "choose " target)
                  :effect (req (let [chosen-type target]
                                 (continue-ability
                                   state side
                                   (with-revealed-hand :runner {:event-side :corp}
                                     {:prompt (str "Shuffle up to two " chosen-type " cards into the stack")
                                      :choices {:req (req (and (in-hand? target)
                                                               (runner? target)
                                                               (= (:type target) chosen-type)))
                                                :max 2}
                                      :effect (req (doseq [t targets]
                                                     (move state :runner t :deck))
                                                   (shuffle! state :runner :deck))
                                      :msg (msg "shuffle " (enumerate-cards targets) " into the Stack")})
                                     card nil)))}]
    {:on-play {:async true
               :change-in-game-state {:req (req (some #(or (not (rezzed? %)) (can-be-advanced? state %)) (all-installed state :corp)))}
               :choices {:req (req (can-be-advanced? state target))}
               :msg (msg "place 2 advancement counters on " (card-str state target))
               :effect (req (wait-for (add-prop state side target :advance-counter 2 {:placed true})
                                      (continue-ability state side name-abi card nil)))}}))

(defcard "Ultraviolet Clearance"
  {:on-play
   {:async true
    :effect (req (wait-for
                   (resolve-ability state side (clearance 10 4) card nil)
                   (continue-ability
                     state side
                     {:prompt "Choose a card in HQ to install"
                      :choices {:card #(and (in-hand? %)
                                            (corp? %)
                                            (not (operation? %)))}
                      :cancel-effect (effect (system-msg (str "declines to use " (:title card) " to install a card"))
                                             (effect-completed eid))
                      :async true
                      :effect (effect (corp-install eid target nil {:msg-keys {:install-source card
                                                                               :display-origin true}}))}
                     card nil)))}})

(defcard "Under the Bus"
  {:on-play
   {:req (req (last-turn? state :runner :accessed-cards))
    :prompt "Choose a connection to trash"
    :choices {:card #(and (runner? %)
                          (resource? %)
                          (has-subtype? % "Connection")
                          (installed? %))}
    :msg (msg "trash " (:title target) " and take 1 bad publicity")
    :async true
    :effect (req (wait-for (trash state side target {:cause-card card :suppress-checkpoint true})
                           (gain-bad-publicity state :corp eid 1)))
    :cancel-effect (effect (system-msg (str "uses " (:title card) " to take 1 bad publicity"))
                           (gain-bad-publicity eid 1))}})

(defcard "Violet Level Clearance"
  {:on-play (clearance 8 4)})

(defcard "Voter Intimidation"
  {:on-play
   {:psi {:req (req (seq (:scored runner)))
          :not-equal (trash-type "resource" resource? :loud)}}})

(defcard "Wake Up Call"
  {:on-play
   {:rfg-instead-of-trashing true
    :req (req (last-turn? state :runner :trashed-card))
    :prompt "Choose a piece of hardware or non-virtual resource"
    :change-in-game-state {:req (req (some #(or (hardware? %)
                                                (and (resource? %) (not (has-subtype? % "Virtual"))))
                                           (all-active-installed state :runner)))}
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
  {:on-play {:choices {:card #(and (ice? %)
                                   (has-subtype? % "Bioroid")
                                   (rezzed? %))}
             :msg (msg "give " (card-str state target) " \"[Subroutine] Do 1 core damage\" before all its other subroutines")
             :async true
             :change-in-game-state {:req (req (some #(and (ice? %) (rezzed? %) (has-subtype? % "Bioroid"))
                                                    (all-installed state :corp)))}
             :effect (req (install-as-condition-counter state side eid card (get-card state target)))}
   :static-abilities [{:type :additional-subroutines
                       :duration :end-of-run
                       :req (req (and (same-card? target (:host card))
                                      (rezzed? target)))
                       :value {:position :front
                               :subroutines [(assoc (do-brain-damage 1) :label "[Wetwork Refit] Do 1 core damage")]}}]})

(defcard "Witness Tampering"
  {:on-play
   {:msg "remove 2 bad publicity"
    :change-in-game-state {:req (req (pos? (count-bad-pub state)))}
    :effect (effect (lose-bad-publicity 2))}})

(defcard "Your Digital Life"
  {:on-play {:msg (msg "gain " (count (:hand corp)) " [Credits]")
             :change-in-game-state {:req (req (seq (:hand corp)))}
             :async true
             :effect (effect (gain-credits :corp eid (count (:hand corp))))}})
