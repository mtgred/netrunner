(ns game.cards.events
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [game.core.access :refer [access-card breach-server get-only-card-to-access
                             num-cards-to-access]]
   [game.core.agendas :refer [update-all-agenda-points]]
   [game.core.bad-publicity :refer [gain-bad-publicity]]
   [game.core.board :refer [all-active-installed all-installed server->zone]]
   [game.core.card :refer [agenda? asset? card-index condition-counter? corp?
                           event? facedown? faceup? get-card get-counters
                           get-nested-host get-title get-zone hardware? has-subtype? ice? in-discard? in-hand?
                           installed? is-type? operation? program? resource? rezzed? runner? upgrade?]]
   [game.core.charge :refer [can-charge charge-ability charge-card]]
   [game.core.checkpoint :refer [fake-checkpoint]]
   [game.core.cost-fns :refer [install-cost play-cost rez-cost]]
   [game.core.damage :refer [damage damage-prevent]]
   [game.core.def-helpers :refer [breach-access-bonus defcard offer-jack-out
                                  reorder-choice]]
   [game.core.drawing :refer [draw]]
   [game.core.effects :refer [register-lingering-effect]]
   [game.core.eid :refer [complete-with-result effect-completed make-eid
                          make-result]]
   [game.core.engine :refer [do-nothing not-used-once? pay register-events register-pending-event
                             resolve-ability trigger-event trigger-event-simult
                             unregister-events unregister-floating-events]]
   [game.core.events :refer [first-event? first-run-event? run-events
                             turn-events run-event-count]]
   [game.core.expose :refer [expose]]
   [game.core.finding :refer [find-cid find-latest]]
   [game.core.flags :refer [any-flag-fn? can-rez? can-trash?
                            clear-all-flags-for-card! clear-run-flag! clear-turn-flag!
                            in-corp-scored? register-run-flag! register-turn-flag! zone-locked?]]
   [game.core.gaining :refer [gain gain-clicks gain-credits lose lose-clicks
                              lose-credits]]
   [game.core.hand-size :refer [corp-hand-size+ hand-size]]
   [game.core.hosting :refer [host]]
   [game.core.ice :refer [all-subs-broken? get-strength pump pump-all-icebreakers
                          update-all-ice update-breaker-strength]]
   [game.core.identities :refer [disable-card disable-identity enable-card
                                 enable-identity]]
   [game.core.initializing :refer [card-init make-card]]
   [game.core.installing :refer [install-as-condition-counter install-locked?
                                 runner-can-install? runner-install]]
   [game.core.link :refer [get-link]]
   [game.core.mark :refer [identify-mark-ability mark-changed-event]]
   [game.core.memory :refer [available-mu]]
   [game.core.moving :refer [as-agenda flip-facedown forfeit mill move
                             swap-ice trash trash-cards]]
   [game.core.payment :refer [can-pay? ->c]]
   [game.core.play-instants :refer [play-instant]]
   [game.core.prompts :refer [cancellable clear-wait-prompt]]
   [game.core.props :refer [add-counter add-icon add-prop remove-icon]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [derez get-rez-cost rez]]
   [game.core.runs :refer [bypass-ice can-run-server? gain-next-run-credits get-runnable-zones
                           make-run prevent-access successful-run-replace-breach
                           total-cards-accessed]]
   [game.core.sabotage :refer [sabotage-ability]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [central->name is-central? is-remote? remote->name
                              target-server unknown->kw zone->name
                              zones->sorted-names]]
   [game.core.set-aside :refer [get-set-aside set-aside]]
   [game.core.shuffling :refer [shuffle! shuffle-into-deck]]
   [game.core.tags :refer [gain-tags lose-tags tag-prevent]]
   [game.core.threat :refer [threat threat-level]]
   [game.core.to-string :refer [card-str]]
   [game.core.toasts :refer [toast]]
   [game.core.update :refer [update!]]
   [game.core.virus :refer [get-virus-counters]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]
   [jinteki.validator :refer [legal?]]))

(defn- cutlery
  [subtype]
  {:makes-run true
   :on-play {:async true
             :prompt "Choose a server"
             :change-in-game-state (req (seq runnable-servers))
             :choices (req runnable-servers)
             :effect (effect (make-run eid target card))}
   :events [{:event :subroutines-broken
             :async true
             :req (req (and (has-subtype? target subtype)
                            (all-subs-broken? target)
                            (let [pred #(and (has-subtype? (first %) subtype)
                                             (all-subs-broken? (first %)))]
                              (first-run-event? state side :subroutines-broken pred))))
             :msg (msg "trash " (card-str state target))
             :effect (effect (trash eid target {:cause-card card}))}]})

;; Card definitions

(defcard "Account Siphon"
  {:makes-run true
   :on-play {:async true
             :change-in-game-state (req hq-runnable)
             :effect (req (make-run state side eid :hq card))}
   :events [(successful-run-replace-breach
              {:target-server :hq
               :this-card-run true
               :ability
               {:async true
                :msg (msg "force the Corp to lose " (min 5 (:credit corp))
                          " [Credits], gain " (* 2 (min 5 (:credit corp)))
                          " [Credits] and take 2 tags")
                :effect (req (let [creds-lost (min 5 (:credit corp))]
                               (wait-for
                                (lose-credits state :corp creds-lost)
                                (wait-for (gain-tags state :runner 2)
                                          (gain-credits state :runner eid (* 2 creds-lost))))))}})]})

(defcard "Always Have a Backup Plan"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :change-on-game-state (req (seq runnable-servers))
             :choices (req runnable-servers)
             :async true
             :msg (msg "make a run on " target)
             :effect (req (wait-for (make-run state side target card)
                                    (let [card (get-card state card)
                                          run-again (get-in card [:special :run-again])]
                                      (if run-again
                                        (make-run state side eid run-again card {:ignore-costs true})
                                        (effect-completed state side eid)))))}
   :events [{:event :run-ends
             :optional {:req (req (and (not (get-in card [:special :run-again]))
                                       (:unsuccessful target)))
                        :player :runner
                        :prompt "Make another run on the same server?"
                        :yes-ability
                        {:effect (req (let [last-run (get-in @state [:runner :register :last-run])
                                            attacked-server (first (:server last-run))
                                            ice (:ice (ffirst (run-events last-run :encounter-ice)))]
                                        (update! state side (update card :special
                                                                    assoc
                                                                    :run-again attacked-server
                                                                    :run-again-ice ice))))}}}
            {:event :encounter-ice
             :once :per-run
             :req (req (and (get-in card [:special :run-again])
                            (same-card? (:ice context) (get-in card [:special :run-again-ice]))))
             :msg (msg "bypass " (:title (:ice context)))
             :effect (req (bypass-ice state))}]})

(defcard "Amped Up"
  {:on-play
   {:msg "gain [Click][Click][Click] and suffer 1 core damage"
    :async true
    :effect (effect (gain-clicks 3)
                    (damage eid :brain 1 {:unpreventable true :card card}))}})

(defcard "Another Day, Another Paycheck"
  {:events [{:event :agenda-stolen
             :trace {:base 0
                     :unsuccessful
                     {:async true
                      :effect (effect (gain-credits :runner eid (+ (:agenda-point runner) (:agenda-point corp))))
                      :msg (msg (str "gain " (+ (:agenda-point runner) (:agenda-point corp)) " [Credits]"))}}}]})

(defcard "Apocalypse"
  (let [corp-trash {:async true
                    :effect (req (let [ai (all-installed state :corp)
                                       onhost (filter #(= '(:onhost) (:zone %)) ai)
                                       unhosted (->> ai
                                                     (remove #(= '(:onhost) (:zone %)))
                                                     (sort-by #(vec (:zone %)))
                                                     (reverse))
                                       allcorp (concat onhost unhosted)]
                                   (trash-cards state :runner eid allcorp {:cause-card card})))}
        runner-facedown {:effect (req (let [installedcards (all-active-installed state :runner)
                                            ishosted (fn [c] (= '(:onhost) (get c :zone)))
                                            hostedcards (filter ishosted installedcards)
                                            nonhostedcards (remove ishosted installedcards)]
                                        (doseq [oc hostedcards
                                                :let [c (get-card state oc)]
                                                :when (not (condition-counter? c))]
                                          (flip-facedown state side c))
                                        (doseq [oc nonhostedcards
                                                :let [c (get-card state oc)]]
                                          (flip-facedown state side c))))}]
    {:on-play {:req (req (and (some #{:hq} (:successful-run runner-reg))
                              (some #{:rd} (:successful-run runner-reg))
                              (some #{:archives} (:successful-run runner-reg))))
               :async true
               ;; trash cards from right to left
               ;; otherwise, auto-killing servers would move the cards to the next server
               ;; so they could no longer be trashed in the same loop
               :msg "trash all installed Corp cards and turn all installed Runner cards facedown"
               :effect (req (wait-for
                              (resolve-ability state side corp-trash card nil)
                              (continue-ability state side runner-facedown card nil)))}}))

(defcard "Ashen Epilogue"
  {:on-play
   {:msg (msg (if (not (zone-locked? state :runner :discard))
                "shuffle the grip and heap into the stack"
                "shuffle the grip into the stack"))
    :rfg-instead-of-trashing true
    :async true
    :effect (req (shuffle-into-deck state :runner :hand :discard)
                 (let [top-5 (take 5 (get-in @state [:runner :deck]))]
                   (doseq [c top-5]
                     (move state side c :rfg))
                   (system-msg state side
                               (str "removes "
                                    (enumerate-str (map :title top-5))
                                    " from the game and draws 5 cards"))
                   (draw state :runner eid 5)))}})

(defcard "Bahia Bands"
  (let [all [{:async true
              :effect (effect (draw eid 2))
              :msg "draw 2 cards"}
             {:msg "install a card from the grip, paying 1 [Credits] less"
              :async true
              :req (req (not (install-locked? state side)))
              :effect (effect (continue-ability
                                {:prompt "Choose a card to install"
                                 :waiting-prompt true
                                 :choices {:req (req (and (or (hardware? target)
                                                              (program? target)
                                                              (resource? target))
                                                          (in-hand? target)
                                                          (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                                                    [(->c :credit (install-cost state side target {:cost-bonus -1}))])))}
                                 :async true
                                 :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target {:cost-bonus -1}))}
                                card nil))}
             {:msg "remove 1 tag"
              :async true
              :effect (effect (lose-tags eid 1))}
             {:effect (effect (add-counter (get-card state card) :credit 4)
                              (effect-completed eid))
              :async true
              :msg "place 4 [Credits] for paying trash costs"}]
        choice (fn choice [abis rem]
                 {:prompt (str "Choose an ability to resolve (" rem " remaining)")
                  :waiting-prompt true
                  :choices (map #(capitalize (:msg %)) abis)
                  :async true
                  :effect (req (let [chosen (some #(when (= target (capitalize (:msg %))) %) abis)]
                                 (wait-for
                                   (resolve-ability state side chosen card nil)
                                   (if (< 1 rem)
                                     (continue-ability state side (choice (remove-once #(= % chosen) abis) (dec rem)) card nil)
                                     (effect-completed state side eid)))))})]
    {:makes-run true
     :on-play {:prompt "Choose a server"
               :choices (req runnable-servers)
               :async true
               :effect (effect (make-run eid target card))}
     :interactions {:pay-credits {:req (req (and (= :runner-trash-corp-cards (:source-type eid))
                                               (corp? target)))
                                  :type :credit}}
     :events [{:event :successful-run
               :interactive (req true)
               :async true
               :req (req this-card-run)
               :effect (req (continue-ability
                              state side
                              (choice all 2)
                              card nil))}]}))

(defcard "Because I Can"
  {:makes-run true
   :on-play {:async true
             :prompt "Choose a server"
             :change-in-game-state (req (seq (filter #(can-run-server? state %) remotes)))
             :choices (req (cancellable (filter #(can-run-server? state %) remotes)))
             :effect (effect (make-run eid target card))}
   :events [(successful-run-replace-breach
              {:target-server :remote
               :this-card-run true
               :ability
               {:msg "shuffle all cards in the server into R&D"
                :effect (req (doseq [c (:content run-server)]
                               (move state :corp c :deck))
                             (shuffle! state :corp :deck))}})]})

(defcard "Black Hat"
  {:on-play
   {:trace
    {:base 4
     :unsuccessful
     {:effect (effect (register-events
                        card [(breach-access-bonus :rd 2 {:duration :end-of-turn})
                              (breach-access-bonus :hq 2 {:duration :end-of-turn})]))}}}})

(defcard "Blackmail"
  {:makes-run true
   :on-play {:req (req (has-bad-pub? state))
             :prompt "Choose a server"
             :change-in-game-state (req (seq runnable-servers))
             :choices (req runnable-servers)
             :msg "prevent ice from being rezzed during this run"
             :async true
             :effect (effect (register-run-flag!
                               card
                               :can-rez
                               (fn [state _side card]
                                 (if (ice? card)
                                   ((constantly false)
                                    (toast state :corp "Cannot rez ice on this run due to Blackmail"))
                                   true)))
                             (make-run eid target card))}})

(defcard "Blueberry!™ Diesel"
  {:on-play {:async true
             :change-in-game-state (req (seq (:deck runner)))
             :prompt "Move a card to the bottom of the stack?"
             :not-distinct true
             :choices (req (conj (vec (take 2 (:deck runner))) "No"))
             :effect (req (when-not (string? target)
                            (move state side target :deck))
                          (system-msg state side
                                      (str "looks at the top 2 cards of the stack"
                                           (when-not (string? target)
                                             " and adds one to the bottom of the stack")))
                          (system-msg state side (str "uses " (:title card) " to draw 2 cards"))
                          (draw state :runner eid 2))}})

(defcard "Bravado"
  ; Bravado only counts distinct pieces of ice that were passed.
  ; That means if a piece of ice was reinstalled and then repassed, it needs to be counted twice.
  ; This is handled by tracking :card-moved and counting them in :special :bravado-moved.
  (letfn [(iced-servers [state side eid card]
            (filter #(-> (get-in @state (cons :corp (server->zone state %))) :ices count pos?)
                    (zones->sorted-names (get-runnable-zones state side eid card nil))))]
    {:makes-run true
     :on-play {:async true
               :change-in-game-state (req (seq (iced-servers state side eid card)))
               :prompt "Choose an iced server"
               :choices (req (iced-servers state side eid card))
               :effect (effect (register-events
                                 card
                                 [{:event :pass-ice
                                   :duration :end-of-run
                                   :effect (effect (update! (update-in (get-card state card) [:special :bravado-passed] (fnil conj #{}) (:cid (:ice context)))))}])
                               (make-run eid target (get-card state card)))}
     :events [{:event :run-ends
               :silent (req true)
               :msg (msg "gain "
                      (+ 6 (count (get-in card [:special :bravado-passed]))
                         (get-in card [:special :bravado-moved] 0))
                      " [Credits]")
               :async true
               :effect (req (let [qty (+ 6 (count (get-in card [:special :bravado-passed]))
                                         (get-in card [:special :bravado-moved] 0))]
                              (gain-credits state :runner eid qty)))}
              {:event :card-moved
               :silent (req true)
               :req (req (get (get-in card [:special :bravado-passed])
                              (:cid (:moved-card context))))
               :effect (req (let [card (update! state side (update-in card [:special :bravado-moved] (fnil inc 0)))]
                              (update! state side
                                       (update-in card [:special :bravado-passed]
                                                  disj (:cid (:moved-card context))))))}]}))

(defcard "Bribery"
  {:makes-run true
   :on-play
   {:async true
    :prompt "How many credits do you want to pay?"
    :choices :credit
    :msg (msg "increase the rez cost of the first unrezzed piece of ice approached by " target " [Credits]")
    :effect (effect
              (continue-ability
               (let [bribery-x target
                     eid (assoc eid :x-cost true)]
                  {:prompt "Choose a server"
                   :choices (req runnable-servers)
                   :async true
                   :effect (effect
                             (register-events
                               card
                               [{:event :approach-ice
                                 :duration :end-of-run
                                 :unregister-once-resolved true
                                 :req (req (and (not (rezzed? (:ice context)))
                                                (first-run-event? state side :approach-ice
                                                                  (fn [targets]
                                                                    (let [context (first targets)]
                                                                      (not (rezzed? (:ice context))))))))
                                 :effect (effect
                                           (register-lingering-effect
                                             card
                                             (let [approached-ice (:ice context)]
                                               {:type :rez-additional-cost
                                                :duration :end-of-run
                                                :unregister-once-resolved true
                                                :req (req (same-card? approached-ice target))
                                                :value [(->c :credit bribery-x)]})))}])
                             (make-run eid target card))})
                card nil))}})

(defcard "Brute-Force-Hack"
  {:on-play
   {:async true
    :effect
    (req (let [affordable-ice
               (seq (filter
                      identity
                      (for [ice (all-installed state :corp)
                            :when (and (ice? ice)
                                       (rezzed? ice))
                            :let [cost (rez-cost state side ice)]]
                        (when (can-pay? state side eid card nil [(->c :credit cost)])
                          [(:cid ice) cost]))))
               eid (assoc eid :x-cost true)]
           (continue-ability
             state side
             {:prompt "How many credits do you want to spend?"
              :choices :credit
              :msg (msg "spends " target " [Credit] on Brute-Force-Hack")
              :async true
              :effect (effect (continue-ability
                                {:choices {:card #(and (rezzed? %)
                                                       (some (fn [c] (and (= (first c)
                                                                             (:cid %))
                                                                          (<= (second c) target)))
                                                             affordable-ice))}
                                 :msg (msg "derez " (card-str state target))
                                 :effect (effect (derez target))}
                                card nil))}
             card nil)))}})

(defcard "Build Script"
  {:on-play
   {:msg "gain 1 [Credits] and draw 2 cards"
    :async true
    :effect (req (wait-for (gain-credits state side 1)
                           (draw state side eid 2)))}})

(defcard "Burner"
  (letfn [(move-ab [chosen-cards n]
            {:prompt (str "Choose a card (" n " remaining)")
             :choices chosen-cards
             :async true
             :effect (req (let [target-card target]
                            (continue-ability
                              state side
                              {:prompt (str "Choose where to put " (:title target-card))
                               :choices ["Top of R&D" "Bottom of R&D"]
                               :async true
                               :msg (msg "add " (:title target-card) " to the "
                                         (decapitalize target))
                               :effect (req
                                         (if (= target "Top of R&D")
                                           (move state :corp target-card :deck {:front true})
                                           (move state :corp target-card :deck {:front false}))
                                         (if (= 1 n)
                                           (effect-completed state side eid)
                                           (continue-ability
                                             state side
                                             (move-ab
                                               (remove-once #(= % target-card) chosen-cards)
                                               (dec n))
                                             card nil)))}
                              card nil)))})]
    {:makes-run true
     :on-play {:async true
               :change-in-game-state (req hq-runnable)
               :effect (req (make-run state side eid :hq card))}
     :events [(successful-run-replace-breach
                {:target-server :hq
                 :this-card-run true
                 :mandatory true
                 :ability
                 {:msg "reveal 3 random cards from HQ"
                  :req (req (<= 1 (count (:hand corp))))
                  :async true
                  :effect (req (let [chosen-cards (take 3 (shuffle (:hand corp)))]
                                 (system-msg
                                   state side
                                   (str "reveals " (enumerate-str (map :title chosen-cards))
                                        " from HQ"))
                                 (continue-ability
                                   state side
                                   (move-ab chosen-cards (min 2 (count chosen-cards)))
                                   card nil)))}})]}))

(defcard "By Any Means"
  {:on-play
   {:effect
    (effect
      (register-events
        card
        [{:event :access
          :duration :end-of-turn
          :req (req (and (can-trash? state :runner target)
                         (not (in-discard? target))))
          :interactive (req true)
          :async true
          :msg (msg "trash " (:title target) " at no cost and suffer 1 meat damage")
          :effect (req (wait-for (trash state side (assoc target :seen true) {:cause-card card
                                                                              :accessed true})
                                 (swap! state assoc-in [:runner :register :trashed-card] true)
                                 (damage state :runner eid :meat 1 {:unboostable true})))}]))}})

(defcard "Calling in Favors"
  {:on-play
   {:msg (msg "gain " (count (filter #(and (has-subtype? % "Connection") (resource? %))
                                     (all-active-installed state :runner))) " [Credits]")
    :change-in-game-state (req (some #(and (has-subtype? % "Connection") (resource? %))
                               (all-active-installed state :runner)))
    :async true
    :effect (effect (gain-credits eid (count (filter #(and (has-subtype? % "Connection")
                                                           (resource? %))
                                                     (all-active-installed state :runner)))))}})

(defcard "Career Fair"
  {:on-play
   {:prompt "Choose a resource to install"
    :change-in-game-state (req (seq (:hand runner)))
    :choices {:req (req (and (resource? target)
                             (in-hand? target)
                             (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                       [(->c :credit (install-cost state side target {:cost-bonus -3}))])))}
    :async true
    :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target {:cost-bonus -3}))}})

(defcard "Careful Planning"
  {:on-play
   {:prompt "Choose a card in or protecting a remote server"
    :choices {:card #(is-remote? (second (get-zone %)))}
    :msg (msg "prevent the Corp from rezzing " (card-str state target) " for the rest of the turn")
    :effect (req (add-icon state side card target "CP" (faction-label card))
                 (let [t target]
                   (register-events state side card
                     [{:event :post-runner-turn-ends
                       :duration :end-of-turn
                       :unregister-once-resolved true
                       :effect (effect (remove-icon card t))}]))
                 (register-turn-flag! state side card :can-rez
                                      (fn [state _side card]
                                        (if (same-card? card target)
                                          ((constantly false)
                                           (toast state :corp "Cannot rez the rest of this turn due to Careful Planning"))
                                           true))))}})

(defcard "Carpe Diem"
  {:makes-run true
   :events [mark-changed-event]
   :on-play
   {:async true
    :effect (req (wait-for
                   (resolve-ability state side identify-mark-ability card nil)
                   (let [marked-server (:mark @state)]
                     (update! state :runner (assoc card :card-target (central->name marked-server)))
                     (system-msg state side (str "uses " (:title card) " to gain 4 [Credits]"))
                     (wait-for (gain-credits state :runner 4)
                               (continue-ability
                                 state side
                                 {:optional
                                  {:prompt (str "Run on " (zone->name marked-server) "?")
                                   :no-ability {:effect (effect (system-msg (str "declines to use " (:title card) " to make a run")))}
                                   :yes-ability {:msg (str "make a run on " (zone->name marked-server))
                                                 :async true
                                                 :effect (effect (make-run eid marked-server))}}}
                                 card nil)))))}})

(defcard "CBI Raid"
  (letfn [(cbi-final [chosen original]
            {:player :corp
             :prompt (str "The top cards of R&D will be " (enumerate-str (map :title chosen)))
             :choices ["Done" "Start over"]
             :async true
             :effect (req (if (= target "Done")
                            (do (doseq [c (reverse chosen)]
                                  (move state :corp c :deck {:front true}))
                                (effect-completed state side eid))
                            (continue-ability state side (cbi-choice original '() (count original) original)
                                              card nil)))})
          (cbi-choice [remaining chosen n original]
            {:player :corp
             :prompt "Choose a card to move next onto R&D"
             :choices remaining
             :async true
             :effect (effect
                       (continue-ability
                         (let [chosen (cons target chosen)]
                           (if (< (count chosen) n)
                             (cbi-choice (remove-once #(= target %) remaining) chosen n original)
                             (cbi-final chosen original)))
                         card nil))})]
    {:makes-run true
     :on-play {:async true
               :change-in-game-state (req hq-runnable)
               :effect (req (make-run state side eid :hq card))}
     :events [(successful-run-replace-breach
                {:target-server :hq
                 :mandatory true
                 :this-card-run true
                 :ability
                 {:msg "force the Corp to add all cards in HQ to the top of R&D"
                  :player :corp
                  :waiting-prompt true
                  :async true
                  :effect (effect
                            (continue-ability
                              (let [from (:hand corp)]
                                (when (pos? (count from))
                                  (cbi-choice from '() (count from) from)))
                              card nil))}})]}))

(defcard "Chastushka"
  {:makes-run true
   :on-play {:async true
             :change-in-game-state (req hq-runnable)
             :effect (req (make-run state side eid :hq card))}
   :events [(successful-run-replace-breach
              {:target-server :hq
               :this-card-run true
               :mandatory true
               :ability (sabotage-ability 4)})]})

(defcard "Chrysopoeian Skimming"
  {:on-play {:prompt "Choose an agenda to reveal"
             :player :corp
             :waiting-prompt true
             :choices (req (conj (filter agenda? (:hand corp)) "Done"))
             :async true
             :effect (req (if (= target "Done")
                            (do (system-msg state :corp "declines to reveal an agenda from HQ")
                                (continue-ability
                                  state :runner
                                  {:msg "look at the top 3 cards of R&D"
                                   :prompt (msg "The top cards of R&D are (top->bottom): " (enumerate-str (map :title (take 3 (:deck corp)))))
                                   :waiting-prompt true
                                   :choices ["OK"]}
                                  card nil))
                            (do
                              (wait-for (reveal state side target)
                                        (system-msg state :corp (str "reveals " (:title target) " from HQ"))
                                        (continue-ability
                                          state :runner
                                          {:msg "gain [Click] and draw 1 card"
                                           :async true
                                           :effect (req (gain-clicks state :runner 1)
                                                        (draw state :runner eid 1))}
                                          card nil)))))}})

(defcard "Code Siphon"
  (letfn [(rd-ice [state]
            (* -3 (count (get-in @state [:corp :servers :rd :ices]))))]
    {:makes-run true
     :on-play {:async true
               :change-in-game-state (req rd-runnable)
               :effect (req (make-run state side eid :rd card))}
     :events [(successful-run-replace-breach
                {:target-server :rd
                 :this-card-run true
                 :ability
                 {:async true
                  :prompt "Choose a program to install"
                  :msg (msg "install " (:title target) " and take 1 tag")
                  :choices (req (filter #(and (program? %)
                                              (runner-can-install? state side % false)
                                              (can-pay? state side (assoc eid :source card :source-type :runner-install) % nil
                                                        [(->c :credit (install-cost state side % {:cost-bonus (rd-ice state)}))]))
                                        (:deck runner)))
                  :effect (req (trigger-event state side :searched-stack)
                               (shuffle! state side :deck)
                               (wait-for (runner-install state side target {:cost-bonus (rd-ice state)})
                                         (gain-tags state side eid 1)))}})]}))

(defcard "Cold Read"
  {:implementation "Used programs restriction not enforced"
   :makes-run true
   :data {:counter {:credit 4}}
   :on-play {:async true
             :prompt "Choose a server"
             :change-in-game-state (req (seq runnable-servers))
             :choices (req runnable-servers)
             :effect (effect (make-run eid target card))}
   :interactions {:pay-credits {:req (req run)
                                :type :credit}}
   :events [{:event :run-ends
             :player :runner
             :prompt "Choose a program that was used during the run"
             :choices {:card #(and (program? %)
                                   (installed? %))}
             :msg (msg "trash " (:title target))
             :async true
             :effect (effect (trash eid target {:unpreventable true
                                                :cause-card card}))}]})

(defcard "Compile"
  (letfn [(compile-fn [where]
            {:prompt "Choose a program to install"
             :choices (req (cancellable (filter program? (get runner where))))
             :async true
             :effect (req (when (= :deck where)
                            (trigger-event state side :searched-stack)
                            (shuffle! state side :deck))
                          (runner-install state side (assoc eid :source card :source-type :runner-install)
                                          (assoc-in target [:special :compile-installed] true)
                                          {:ignore-all-cost true}))})]
    {:makes-run true
     :on-play {:prompt "Choose a server"
               :msg "make a run and install a program on encounter with the first piece of ice"
               :choices (req runnable-servers)
               :change-in-game-state (req (seq runnable-servers))
               :async true
               :effect (effect (make-run eid target card))}
     :events [{:event :encounter-ice
               :optional
               {:prompt "Install a program?"
                :once :per-run
                :yes-ability
                {:async true
                 :prompt "Choose where to install the program from"
                 :choices (req (if (not (zone-locked? state :runner :discard)) ["Stack" "Heap"] ["Stack"]))
                 :msg (msg "install a program from [their] " target)
                 :effect (effect (continue-ability
                                   (compile-fn (if (= "Stack" target) :deck :discard))
                                   card nil))}}}
              {:event :run-ends
               :async true
               :interactive (req true)
               :effect (req (let [compile-installed (first (filterv #(get-in % [:special :compile-installed])
                                                                    (all-active-installed state :runner)))]
                              (if (some? compile-installed)
                                (do (system-msg state :runner (str "moved " (:title compile-installed)
                                                                   " to the bottom of the Stack"))
                                    (move state :runner compile-installed :deck)
                                    (effect-completed state side eid))
                                (effect-completed state side eid))))}]}))

(defcard "Concerto"
  (letfn [(reveal-and-load-credits [stack]
            (when-let [topcard (first stack)]
              {:async true
               :msg (msg "reveal " (get-title topcard) " from the top of the stack, "
                         "move it to the grip and place " (:cost topcard) " [Credits] on itself")
               :effect (req (wait-for (reveal state side topcard)
                                      (add-counter state side card :credit (:cost topcard) {:placed true})
                                      (move state :runner topcard :hand)
                                      (effect-completed state side eid)))}))]
    {:makes-run true
     :interactions {:pay-credits {:req (req run)
                                  :type :credit}}
     :on-play {:async true
               :effect (req (wait-for (resolve-ability state side
                                                       (reveal-and-load-credits (:deck runner))
                                                       card nil)
                                      (continue-ability state side
                                        {:prompt "Choose a server"
                                         :choices (req runnable-servers)
                                         :async true
                                         :effect (effect (make-run eid target (get-card state card)))}
                                        (get-card state card) nil)))}}))

(defcard "Contaminate"
  {:on-play
   {:msg (msg "place 3 virus tokens on " (:title target))
    :choices {:req (req (and (installed? target)
                             (runner? target)
                             (zero? (get-virus-counters state target))))}
    :change-in-game-state (req (some #(zero? (get-virus-counters state %))
                                     (all-installed state :runner)))
    :async true
    :effect (effect (add-counter :runner eid target :virus 3 nil))}})

(defcard "Corporate \"Grant\""
  {:events [{:event :runner-install
             :req (req (first-event? state side :runner-install))
             :msg "force the Corp to lose 1 [Credit]"
             :async true
             :effect (effect (lose-credits :corp eid 1))}]})

(defcard "Corporate Scandal"
  {:on-play {:msg "give the Corp 1 additional bad publicity"
             :implementation "No enforcement that this Bad Pub cannot be removed"
             :effect (req (swap! state update-in [:corp :bad-publicity :additional] inc))}
   :leave-play (req (swap! state update-in [:corp :bad-publicity :additional] dec))})

(defcard "Creative Commission"
  {:on-play {:msg (msg "gain 5 [Credits]"
                       (when (pos? (:click runner))
                         " and lose [Click]"))
             :async true
             :effect (req (when (pos? (:click runner))
                            (lose-clicks state :runner 1))
                          (gain-credits state :runner eid 5))}})

(defcard "Credit Crash"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :choices (req runnable-servers)
             :change-in-game-state (req (seq runnable-servers))
             :async true
             :effect (effect (make-run eid target card))}
   :events [{:event :pre-access-card
             :once :per-run
             :async true
             :req (req (not (agenda? target)))
             :effect (req (let [c target
                                cost (or (and (or (asset? c)
                                                  (upgrade? c)
                                                  (ice? c))
                                              (rez-cost state side c))
                                         (and (operation? c)
                                              (play-cost state side c)))
                                title (:title c)]
                            (if (can-pay? state :corp eid card nil [(->c :credit cost)])
                              (continue-ability
                                state :corp
                                {:optional
                                 {:waiting-prompt true
                                  :prompt (msg "Spend " cost " [Credits] to prevent the trash of " title "?")
                                  :player :corp
                                  :yes-ability {:async true
                                                :effect (req (system-msg state :corp (str "spends " cost " [Credits] to prevent "
                                                                                          title " from being trashed at no cost"))
                                                             (lose-credits state :corp eid cost))}
                                  :no-ability {:msg (msg "trash " title " at no cost")
                                               :async true
                                               :effect (effect (trash eid (assoc c :seen true) {:cause-card card}))}}}
                                card nil)
                              (do (system-msg state side (str "uses " (:title card) " to trash " title " at no cost"))
                                  (trash state side eid (assoc c :seen true) nil)))))}]})

(defcard "Credit Kiting"
  {:on-play
   {:req (req (some #{:hq :rd :archives} (:successful-run runner-reg)))
    :prompt "Choose a card to install"
    :choices {:req (req (and (not (event? target))
                             (in-hand? target)
                             (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                       [(->c :credit (install-cost state side target {:cost-bonus -8}))])))}
    :async true
    :cancel-effect (req (gain-tags state :runner eid 1))
    :effect (req (let [new-eid (make-eid state {:source card :source-type :runner-install})]
                   (wait-for (runner-install state :runner new-eid target {:cost-bonus -8})
                             (gain-tags state :runner eid 1))))}})

(defcard "Cyber Threat"
  {:makes-run true
   :on-play
   {:prompt "Choose a server"
    :choices (req runnable-servers)
    :async true
    :effect (req (let [serv target]
                   (continue-ability
                     state :corp
                     (if (seq (filter #(and (installed? %)
                                            (not (rezzed? %))
                                            (ice? %)
                                            (can-pay? state side eid card nil
                                                      [(->c :credit (rez-cost state side %))]))
                                      (all-installed state :corp)))
                       {:optional
                        {:prompt (msg "Rez a piece of ice protecting " serv "?")
                         :yes-ability {:async true
                                       :prompt (msg "Choose a piece of ice protecting " serv " to rez")
                                       :player :corp
                                       :choices {:card #(and (installed? %)
                                                             (not (rezzed? %))
                                                             (ice? %)
                                                             (can-pay? state side eid card nil
                                                                       [(->c :credit (rez-cost state side %))]))}
                                       :effect (effect (rez :corp eid target))
                                       :cancel-effect
                                       (effect (register-run-flag!
                                                 card
                                                 :can-rez
                                                 (fn [state _side card]
                                                   (if (ice? card)
                                                     ((constantly false)
                                                      (toast state :corp "Cannot rez ice on this run due to Cyber Threat"))
                                                     true)))
                                               (make-run eid serv card))}
                         :no-ability {:async true
                                      :effect (effect (register-run-flag!
                                                        card
                                                        :can-rez
                                                        (fn [state _side card]
                                                          (if (ice? card)
                                                            ((constantly false)
                                                             (toast state :corp "Cannot rez ice on this run due to Cyber Threat"))
                                                            true)))
                                                      (make-run eid serv card))
                                      :msg (msg "make a run on " serv " during which no ice can be rezzed")}}}
                       {:async true
                        :effect (effect (register-run-flag!
                                          card
                                          :can-rez
                                          (fn [state _side card]
                                            (if (ice? card)
                                              ((constantly false)
                                               (toast state :corp "Cannot rez ice on this run due to Cyber Threat"))
                                              true)))
                                        (make-run eid serv card))
                        :msg (msg "make a run on " serv " during which no ice can be rezzed")})
                     card nil)))}})

(defcard "Data Breach"
  {:makes-run true
   :on-play {:async true
             :change-in-game-state (req rd-runnable)
             :effect (req (wait-for
                            (make-run state side :rd card)
                            (let [card (get-card state card)]
                              (if (:run-again card)
                                (make-run state side eid :rd card)
                                (effect-completed state side eid)))))}
   :events [{:event :run-ends
             :optional {:req (req (and (:successful target)
                                       (not (:run-again card))
                                       (= [:rd] (:server target))))
                        :prompt "Make another run on R&D?"
                        :yes-ability {:effect (effect (clear-wait-prompt :corp)
                                                      (update! (assoc card :run-again true)))}}}]})

(defcard "Day Job"
  {:on-play
   {:additional-cost [(->c :click 3)]
    :msg "gain 10 [Credits]"
    :async true
    :effect (effect (gain-credits eid 10))}})

(defcard "Deep Data Mining"
  {:makes-run true
   :on-play {:async true
             :change-in-game-state (req rd-runnable)
             :effect (req (make-run state side eid :rd card))}
   :events [{:event :successful-run
             :req (req (and (= :rd (target-server context))
                            this-card-run))
             :silent (req true)
             :effect (effect (register-events
                              card [(breach-access-bonus :rd
                                                         (max 0 (min 4 (available-mu state)))
                                                         {:duration :end-of-run})]))}]})

(defcard "Deep Dive"
  (letfn [(shuffle-back [state cards-to-shuffle]
            (doseq [c cards-to-shuffle]
              (move state :corp c :deck))
            (system-msg state :corp "shuffles R&D")
            (shuffle! state :corp :deck))
          (deep-dive-access [cards]
            ;; Accesses a card from a set of given cards.
            ;; Returns the set of unchosen cards as async-result
            {:prompt "Choose a card to access"
             :waiting-prompt true
             :not-distinct true
             :choices cards
             :async true
             :effect (req (wait-for
                            (access-card state side target)
                            (let [new-cards (remove #(same-card? % target) cards)]
                              (effect-completed state side (make-result eid new-cards)))))})]
    {:on-play
     {:req (req (and (some #{:hq} (:successful-run runner-reg))
                     (some #{:rd} (:successful-run runner-reg))
                     (some #{:archives} (:successful-run runner-reg))))
      :async true
      :change-in-game-state (req (seq (:deck corp)))
      :effect (req (set-aside state :corp eid (take 8 (:deck corp)))
                   (let [top-8 (sort-by :title (get-set-aside state :corp eid))]
                     (system-msg state side (str "uses " (get-title card)
                                                 " to set aside "
                                                 (enumerate-str (map get-title top-8))
                                                 " from the top of R&D"))
                     (wait-for
                       (resolve-ability state side
                                        {:async true
                                         :prompt (str "The set aside cards are: "
                                                      (enumerate-str (map get-title top-8)))
                                         :choices ["OK"]}
                                        card nil)
                       (wait-for
                         (resolve-ability state side (deep-dive-access top-8) card nil)
                         (if (seq async-result)
                           (wait-for
                             (resolve-ability state side
                                              {:optional
                                               {:prompt "Pay [Click] to access another card?"
                                                :req (req (can-pay? state :runner
                                                                    (assoc eid :source card :source-type :ability)
                                                                    card nil [(->c :click 1)]))
                                                :no-ability
                                                {:effect (effect (system-msg (str "declines to use "
                                                                                  (:title card)
                                                                                  " to access another card")))}
                                                :yes-ability
                                                {:async true
                                                 :cost [(->c :click 1)]
                                                 :msg "access another card"
                                                 :effect
                                                 (req (wait-for
                                                        (resolve-ability state side
                                                                         (deep-dive-access async-result)
                                                                         card nil)
                                                        (effect-completed state side eid)))}}}
                                              card nil)
                             (do (shuffle-back state (get-set-aside state :corp eid))
                                 (effect-completed state side eid)))
                           (do (shuffle-back state (get-set-aside state :corp eid))
                               (effect-completed state side eid)))))))}}))

(defcard "Déjà Vu"
  {:on-play
   {:change-in-game-state (req (and (seq (:discard runner))
                              (not (zone-locked? state :runner :discard))))
    :prompt "Choose a card to add to Grip"
    :choices (req (cancellable (:discard runner) :sorted))
    :msg (msg "add " (:title target) " to [their] Grip")
    :async true
    :effect (effect (move target :hand)
                    (continue-ability
                      (when (has-subtype? target "Virus")
                        {:prompt "Choose a virus to add to Grip"
                         :msg (msg "add " (:title target) " to [their] Grip")
                         :choices (req (cancellable (filter #(has-subtype? % "Virus") (:discard runner)) :sorted))
                         :effect (effect (move target :hand))})
                      card nil))}})

(defcard "Demolition Run"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :change-in-game-state (req (or hq-runnable rd-runnable))
             :choices (req [(when hq-runnable "HQ")
                            (when rd-runnable "R&D")])
             :async true
             :effect (req (make-run state side eid target card))}
   :interactions {:access-ability
                  {:label "Trash card"
                   :trash? true
                   :msg (msg "trash " (:title target) " at no cost")
                   :req (req (and (can-trash? state :runner target)
                                  (not (in-discard? target)))) ;;for if the run gets diverted
                   :async true
                   :effect (effect (trash eid (assoc target :seen true) {:cause-card card}))}}})

(defcard "Deuces Wild"
  (let [all [{:effect (effect (gain-credits eid 3))
              :async true
              :msg "gain 3 [Credits]"}
             {:async true
              :effect (effect (draw eid 2))
              :msg "draw 2 cards"}
             {:async true
              :effect (effect (lose-tags eid 1))
              :msg "remove 1 tag"}
             {:prompt "Choose 1 piece of ice to expose"
              :msg "expose 1 ice and make a run"
              :choices {:card #(and (installed? %)
                                    (ice? %))}
              :async true
              :effect (req (wait-for (expose state side target)
                                     (continue-ability
                                       state side
                                       {:prompt "Choose a server"
                                        :choices (req runnable-servers)
                                        :async true
                                        :effect (effect (make-run eid target))}
                                       card nil)))
              :cancel-effect (effect (continue-ability
                                       {:prompt "Choose a server"
                                        :choices (req runnable-servers)
                                        :async true
                                        :effect (effect (make-run eid target))}
                                       card nil))}]
        choice (fn choice [abis]
                 {:prompt "Choose an ability to resolve"
                  :choices (map #(capitalize (:msg %)) abis)
                  :async true
                  :effect (req (let [chosen (some #(when (= target (capitalize (:msg %))) %) abis)]
                                 (wait-for
                                   (resolve-ability state side chosen card nil)
                                   (if (= (count abis) 4)
                                     (continue-ability state side (choice (remove-once #(= % chosen) abis)) card nil)
                                     (effect-completed state side eid)))))})]
    {:on-play
     {:async true
      :effect (effect (continue-ability (choice all) card nil))}}))

(defcard "Diana's Hunt"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :msg "make a run"
             :change-in-game-state (req (seq runnable-servers))
             :choices (req runnable-servers)
             :async true
             :effect (effect (make-run eid target card))}
   :events [{:event :encounter-ice
             :optional
             {:req (req (seq (filter program? (:hand runner))))
              :prompt "Install a program from the grip?"
              :yes-ability
              {:prompt "Choose a program to install"
               :async true
               :choices {:card #(and (in-hand? %)
                                     (program? %))}
               :msg (msg "install " (:title target) ", ignoring all costs")
               :effect (effect (runner-install eid (assoc-in target [:special :diana-installed] true) {:ignore-all-cost true}))}}}
            {:event :run-ends
             :async true
             :effect (req (let [installed-cards (filter #(get-in % [:special :diana-installed]) (all-active-installed state :runner))]
                            (if (seq installed-cards)
                              (do
                                (system-msg state :runner (str "trashes " (quantify (count installed-cards) "card")
                                                               " (" (enumerate-str (map :title installed-cards))
                                                                          ") at the end of the run from Diana's Hunt"))
                                (trash-cards state :runner eid installed-cards {:unpreventable true
                                                                                :cause-card card}))
                              (effect-completed state side eid))))}]})

(defcard "Diesel"
  {:on-play
   {:msg "draw 3 cards"
    :change-in-game-state (req (seq (:deck runner)))
    :async true
    :effect (effect (draw eid 3))}})

(defcard "Direct Access"
  {:makes-run true
   :static-abilities [{:type :disable-card
                       :req (req (or (same-card? target (:identity corp))
                                     (same-card? target (:identity runner))))

                       :value true}]
   :on-play {:async true
             :effect (req
                       ;; note - this fake checkpoint forces abilities like RP to be blank
                       (fake-checkpoint state)
                       (continue-ability
                         state side
                         {:async true
                          :prompt "Choose a server"
                          :choices (req runnable-servers)
                          :effect (effect (make-run eid target card))}
                         card nil))}
   :events [{:event :run-ends
             :unregister-once-resolved true
             :async true
             :effect (req (continue-ability
                            state :runner
                            {:optional
                             {:prompt "Shuffle Direct Access into the Stack?"
                              :yes-ability
                              {:msg "shuffle itself into the Stack"
                               :effect (effect (move (get-card state card) :deck)
                                               (shuffle! :deck))}}}
                            card nil))}]})

(defcard "Dirty Laundry"
  {:makes-run true
   :on-play {:async true
             :prompt "Choose a server"
             :choices (req runnable-servers)
             :change-in-game-state (req (seq runnable-servers))
             :effect (effect (make-run eid target card))}
   :events [{:event :run-ends
             :req (req (and (:successful target)
                            this-card-run))
             :msg "gain 5 [Credits]"
             :async true
             :effect (effect (gain-credits :runner eid 5))}]})

(defcard "Diversion of Funds"
  (letfn [(five-or-all [corp] (min 5 (:credit corp)))]
    {:makes-run true
     :on-play {:async true
               :change-in-game-state (req hq-runnable)
               :effect (req (make-run state side eid :hq card))}
     :events [(successful-run-replace-breach
                {:target-server :hq
                 :this-card-run true
                 :ability
                 {:msg (msg "force the Corp to lose " (five-or-all corp)
                            " [Credits], and gain " (five-or-all corp)
                            " [Credits]")
                  :async true
                  :effect (req (wait-for (gain-credits state :runner (five-or-all corp))
                                         (lose-credits state :corp eid (five-or-all corp))))}})]}))

(defcard "Divide and Conquer"
  {:makes-run true
   :on-play {:async true
             :change-in-game-state (req archives-runnable)
             :effect (req (make-run state side eid :archives card))}
   :events [{:event :end-breach-server
             :async true
             :req (req (and (= :archives (:from-server target))
                            (:successful run)))
             :effect (req (wait-for (breach-server state side [:hq] {:no-root true})
                                    (breach-server state side eid [:rd] {:no-root true})))}]})

(defcard "Drive By"
  {:on-play
   {:choices {:card #(let [topmost (get-nested-host %)]
                       (and (is-remote? (second (get-zone topmost)))
                            (= (last (get-zone topmost)) :content)
                            (not (:rezzed %))))}
    :async true
    :effect (req (wait-for (expose state side target)
                           (if-let [target async-result]
                             (if (or (asset? target)
                                     (upgrade? target))
                               (do (system-msg state :runner (str "uses " (:title card) " to trash " (:title target)))
                                   (trash state :runner eid (assoc target :seen true) {:cause-card card}))
                               (effect-completed state side eid))
                             (effect-completed state side eid))))}})

(defcard "Early Bird"
  {:makes-run true
   :on-play
   {:prompt "Choose a server"
    :choices (req runnable-servers)
    :msg (msg "make a run on " target " and gain [Click]")
    :async true
    :effect (effect (gain-clicks 1)
                    (make-run eid target card))}})

(defcard "Easy Mark"
  {:on-play
   {:msg "gain 3 [Credits]"
    :async true
    :effect (effect (gain-credits eid 3))}})

(defcard "Embezzle"
  {:makes-run true
   :on-play {:async true
             :change-in-game-state (req hq-runnable)
             :effect (req (make-run state side eid :hq card))}
   :events [(successful-run-replace-breach
              {:target-server :hq
               :this-card-run true
               :mandatory true
               :ability
               {:prompt "Choose a card type"
                :choices ["Asset" "Upgrade" "Operation" "ICE"]
                :msg (msg "reveal 2 cards from HQ and trash all "
                          target (when (not= "ICE" target) "s"))
                :async true
                :effect (req (let [cards-to-reveal (take 2 (shuffle (:hand corp)))
                                   cards-to-trash (filter #(is-type? % target) cards-to-reveal)
                                   credits (* 4 (count cards-to-trash))]
                               (system-msg state side
                                           (str "uses " (:title card) " to reveal "
                                                (enumerate-str (map :title cards-to-reveal))
                                                " from HQ"))
                               (wait-for
                                 (reveal state side cards-to-reveal)
                                 (if (pos? credits)
                                   (do (system-msg
                                         state side
                                         (str "uses " (:title card) " to trash "
                                              (enumerate-str (map :title cards-to-trash))
                                              " from HQ and gain "
                                              credits " [Credits]"))
                                       (wait-for (trash-cards state :runner (map #(assoc % :seen true) cards-to-trash) {:cause-card card})
                                                 (gain-credits state :runner eid credits)))
                                   (effect-completed state side eid)))))}})]})

(defcard "Emergency Shutdown"
  {:on-play
   {:req (req (some #{:hq} (:successful-run runner-reg)))
    :change-in-game-state (req (some (every-pred ice? rezzed?) (all-installed state :corp)))
    :msg (msg "derez " (:title target))
    :choices {:card #(and (ice? %)
                          (rezzed? %))}
    :effect (effect (derez target))}})

(defcard "Emergent Creativity"
  (letfn [(ec [trash-cost to-trash]
            {:async true
             :prompt "Choose a piece of hardware or program to install"
             :msg (msg "trash " (if (empty? to-trash) "no cards" (enumerate-str (map :title to-trash)))
                       " and install " (:title target)
                       " lowering the cost by " trash-cost)
             :choices (req (cancellable (filter #(and (or (program? %)
                                                          (hardware? %))
                                                      (can-pay? state side (assoc eid :source card :source-type :runner-install) % nil
                                                                [(->c :credit (install-cost state side % {:cost-bonus (- trash-cost)}))]))
                                                (:deck runner)) :sorted))
             :effect (req (trigger-event state side :searched-stack)
                          (shuffle! state side :deck)
                          (runner-install state side (assoc eid :source card :source-type :runner-install)
                                          target {:cost-bonus (- trash-cost)}))})]
    {:on-play
     {:prompt "Choose pieces of hardware and/or programs to trash"
      :change-in-game-state (req (or (seq (:deck runner))
                               (seq (:hand runner))))
      :choices {:card #(and (or (hardware? %)
                                (program? %))
                         (in-hand? %))
                :max (req (count (:hand runner)))}
      :cancel-effect (effect (continue-ability (ec 0 []) card nil))
      :async true
      :effect (req (let [trash-cost (reduce + (keep :cost targets))
                         to-trash targets]
                     (wait-for (trash-cards state side to-trash {:unpreventable true :cause-card card})
                               (continue-ability state side (ec trash-cost to-trash) card nil))))}}))

(defcard "Employee Strike"
  {:on-play {:msg "disable the Corp's identity"}
   :static-abilities [{:type :disable-card
                       :req (req (same-card? target (:identity corp)))
                       :value true}]})

(defcard "En Passant"
  {:on-play
   {:req (req (:successful-run runner-reg))
    :prompt "Choose an unrezzed piece of ice that you passed on your last run"
    :choices {:req (req (some #(same-card? target %)
                              (->> (:events (:last-run runner-reg))
                                   (filter #(= :pass-ice (first %)))
                                   (map second)
                                   (keep #(get-card state (:ice (first %))))
                                   (filter (complement rezzed?)))))}
    :msg (msg "trash " (card-str state target))
    :async true
    :cancel-effect (req (do-nothing state side eid card))
    :effect (effect (trash eid target {:cause-card card}))}})

(defcard "Encore"
  {:on-play
   {:req (req (and (some #{:hq} (:successful-run runner-reg))
                   (some #{:rd} (:successful-run runner-reg))
                   (some #{:archives} (:successful-run runner-reg))))
    :rfg-instead-of-trashing true
    :msg "take an additional turn after this one"
    :effect (req (swap! state update-in [:runner :extra-turns] (fnil inc 0)))}})

(defcard "Escher"
  (letfn [(es [] {:async true
                  :prompt "Choose 2 pieces of ice to swap positions"
                  :choices {:card #(and (installed? %)
                                        (ice? %))
                            :max 2}
                  :effect (req (if (= (count targets) 2)
                                 (do (swap-ice state side (first targets) (second targets))
                                     (continue-ability state side (es) card nil))
                                 (do (system-msg state side "has finished rearranging ice")
                                     (effect-completed state side eid))))})]
    {:makes-run true
     :on-play {:async true
               :change-in-game-state (req hq-runnable)
               :effect (req (make-run state side eid :hq card))}
     :events [(successful-run-replace-breach
                {:target-server :hq
                 :this-card-run true
                 :mandatory true
                 :ability
                 {:async true
                  :msg "rearrange installed ice"
                  :effect (effect (continue-ability (es) card nil))}})]}))

(defcard "Eureka!"
  {:on-play
   {:async true
    :change-in-game-state (req (seq (:deck runner)))
    :effect (req (let [topcard (first (:deck runner))
                       caninst (and (or (hardware? topcard)
                                        (program? topcard)
                                        (resource? topcard))
                                    (can-pay? state side (assoc eid :source card :source-type :runner-install) topcard nil
                                              [(->c :credit (install-cost state side topcard {:cost-bonus -10}))]))]
                   (if caninst
                     (continue-ability
                       state side
                       {:optional
                        {:prompt (msg "Install " (:title topcard) "?")
                         :yes-ability {:async true
                                       :effect (effect (runner-install eid topcard {:cost-bonus -10}))}
                         :no-ability {:async true
                                      :effect (req (wait-for
                                                     (reveal state side topcard)
                                                     (system-msg (str "reveals "
                                                                      (:title topcard)
                                                                      " from the top of the stack and trashes it"))
                                                     (trash eid topcard {:unpreventable true
                                                                         :cause-card card})))}}}
                       card nil)
                     (wait-for (reveal state side topcard)
                               (system-msg state side (str "reveals " (:title topcard) " from the top of the stack and trashes it"))
                               (trash state side eid topcard {:unpreventable true :cause-card card})))))}})

(defcard "Exclusive Party"
  {:on-play
   {:msg (msg "draw 1 card and gain "
              (count (filter #(= (:title %) (:title card)) (:discard runner)))
              " [Credits]")
    :async true
    :effect (req (wait-for (draw state side 1)
                           (gain-credits state side eid (count (filter #(= (:title %) (:title card)) (:discard runner))))))}})

(defcard "Executive Wiretaps"
  {:on-play
   {:msg (msg "reveal " (enumerate-str (sort (map :title (:hand corp)))) " from HQ")
    :change-in-game-state (req (seq (:deck corp)))
    :async true
    :effect (effect (reveal eid (:hand corp)))}})

(defcard "Exploit"
  {:on-play
   {:req (req (and (some #{:hq} (:successful-run runner-reg))
                   (some #{:rd} (:successful-run runner-reg))
                   (some #{:archives} (:successful-run runner-reg))))
    :prompt "Choose up to 3 pieces of ice to derez"
    :change-in-game-state (req (some (every-pred ice? rezzed?) (all-installed state :corp)))
    :choices {:max 3
              :card #(and (rezzed? %)
                          (ice? %))}
    :msg (msg "derez " (enumerate-str (map :title targets)))
    :effect (req (doseq [c targets]
                   (derez state side c)))}})

(defcard "Exploratory Romp"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :choices (req runnable-servers)
             :change-in-game-state (req (seq runnable-servers))
             :async true
             :effect (effect (make-run eid target card))}
   :events [(successful-run-replace-breach
              {:mandatory true
               :this-card-run true
               :ability
               {:req (req (some #(and (pos? (get-counters % :advancement))
                                      (= (first (:server run)) (second (get-zone %))))
                                (all-installed state :corp)))
                :prompt "How many advancements tokens do you want to remove?"
                :choices ["0" "1" "2" "3"]
                :async true
                :waiting-prompt true
                :effect (req (let [c (str->int target)]
                               (continue-ability
                                 state side
                                 {:choices {:card #(and (pos? (get-counters % :advancement))
                                                        (= (first (:server run)) (second (get-zone %))))}
                                  :msg (msg "remove " (quantify c "advancement token")
                                            " from " (card-str state target))
                                  :effect (req (let [to-remove (min c (get-counters target :advancement))]
                                                 (add-prop state :corp target :advance-counter (- to-remove))))}
                                 card nil)))}})]})

(defcard "Express Delivery"
  {:on-play
   {:prompt "Choose a card to add to the grip"
    :change-in-game-state (req (seq (:deck runner)))
    :choices (req (take 4 (:deck runner)))
    :msg "look at the top 4 cards of the stack and add 1 of them to the grip"
    :effect (effect (move target :hand)
                    (shuffle! :deck))}})

(defcard "Eye for an Eye"
  {:makes-run true
   :on-play {:req (req (not tagged))
             :change-in-game-state (req hq-runnable)
             :async true
             :effect (req (make-run state side eid :hq card))}
   :interactions {:access-ability
                  {:label "Trash card"
                   :trash? true
                   :cost [(->c :trash-from-hand 1)]
                   :msg (msg "trash " (:title target) " from HQ")
                   :async true
                   :effect (effect (trash eid (assoc target :seen true) {:accessed true :cause-card card}))}}
   :events [{:event :successful-run
             :silent (req true)
             :req (req (and (= :hq (target-server context))
                            this-card-run))
             :async true
             :msg "take 1 tag and access 1 additional card from HQ"
             :effect (req
                       (wait-for (gain-tags state :runner 1 {:unpreventable true})
                                 (register-events
                                   state side
                                   card [(breach-access-bonus :hq 1 {:duration :end-of-run})])
                                 (effect-completed state side eid)))}]})

(defcard "Falsified Credentials"
  {:on-play
   {:prompt "Choose one"
    :choices ["Agenda" "Asset" "Upgrade"]
    :msg (msg "guess " target)
    :async true
    :effect (effect
              (continue-ability
                (let [chosen-type target]
                  {:choices {:card #(let [topmost (get-nested-host %)]
                                      (and (is-remote? (second (get-zone topmost)))
                                           (= (last (get-zone topmost)) :content)
                                           (not (rezzed? %))))}
                   :async true
                   :effect (req (wait-for (expose state side target)
                                          (continue-ability
                                            state :runner
                                            (when (and async-result ;; expose was successful
                                                       (= chosen-type (:type target)))
                                              {:msg "gain 5 [Credits]"
                                               :async true
                                               :effect (effect (gain-credits eid 5))})
                                            card nil)))})
                card nil))}})

(defcard "Fear the Masses"
  {:makes-run true
   :on-play {:async true
             :change-in-game-state (req hq-runnable)
             :effect (req (make-run state side eid :hq card))}
   :events [(successful-run-replace-breach
              {:target-server :hq
               :this-card-run true
               :mandatory true
               :ability
               {:async true
                :msg "force the Corp to trash the top card of R&D"
                :effect (req (wait-for
                               (mill state :corp :corp 1)
                               (continue-ability
                                 state side
                                 (let [n (count (filter #(same-card? :title card %) (:hand runner)))]
                                   {:async true
                                    :prompt (msg "How many copies of " (:title card) " do you want to reveal?")
                                    :choices {:card #(and (in-hand? %)
                                                          (same-card? :title card %))
                                              :max n}
                                    :msg (msg "reveal "
                                              (quantify (count targets) "cop" "y" "ies")
                                              " of itself,"
                                              " forcing the Corp to trash " (quantify (count targets) "additional card")
                                              " from the top of R&D")
                                    :effect (req (wait-for
                                                   (reveal state :runner targets)
                                                   (mill state :corp eid :corp (count targets))))})
                                 card nil)))}})]})

(defcard "Feint"
  {:makes-run true
   :on-play {:async true
             :change-in-game-state (req hq-runnable)
             :effect (req (make-run state side eid :hq card))}
   :events [{:event :encounter-ice
             :req (req (< (get-in card [:special :bypass-count] 0) 2))
             :msg (msg "bypass " (:title (:ice context)))
             :effect (req (bypass-ice state)
                          (update! state side (update-in card [:special :bypass-count] (fnil inc 0))))}
            {:event :successful-run
             :effect (effect (prevent-access))}]})

(defcard "Finality"
  {:makes-run true
   :on-play {:async true
             :change-in-game-state (req rd-runnable)
             :additional-cost [(->c :brain 1)]
             :effect (req (make-run state side eid :rd card))}
   :events [{:event :successful-run
             :silent (req true)
             :req (req (and (= :rd (target-server context))
                            this-card-run))
             :effect (effect (register-events
                              card [(breach-access-bonus :rd 3 {:duration :end-of-run})]))}]})

(defcard "Fisk Investment Seminar"
  {:on-play
   {:msg "make each player draw 3 cards"
    :change-in-game-state (req (or (seq (:deck runner))
                             (seq (:deck corp))))
    :async true
    :effect (req (wait-for (draw state :runner 3)
                           (draw state :corp eid 3)))}})

(defcard "Forged Activation Orders"
  {:on-play
   {:choices {:card #(and (ice? %)
                          (not (rezzed? %)))}
    :change-in-game-state (req (some (every-pred ice? (complement rezzed?)) (all-installed state :corp)))
    :async true
    :effect (req (let [ice target
                       serv (zone->name (second (get-zone ice)))
                       icepos (card-index state ice)]
                   (continue-ability
                     state :corp
                     {:prompt "Choose one"
                      :choices [(when (and (can-rez? state :corp ice)
                                           (can-pay? state :corp eid ice nil (get-rez-cost state :corp ice nil)))
                                  (str "Rez " (card-str state ice)))
                                (str "Trash " (card-str state ice))]
                      :async true
                      :msg (msg "force the Corp to " (decapitalize target))
                      :waiting-prompt true
                      :effect (req (if (str/starts-with? target "Rez")
                                     (rez state :corp eid ice)
                                     (trash state :corp eid ice {:cause-card card
                                                                 :cause :forced-to-trash})))}
                     card nil)))}})

(defcard "Forked"
  (cutlery "Sentry"))

(defcard "Frame Job"
  {:on-play
   {:prompt "Choose an agenda to forfeit"
    :change-in-game-state (req (:scored runner))
    :choices (req (:scored runner))
    :msg (msg "forfeit " (get-title card) " and give the Corp 1 bad publicity")
    :async true
    :effect (req (wait-for (forfeit state side (make-eid state eid) target {:msg false})
                           (gain-bad-publicity state :corp 1)
                           (effect-completed state side eid)))}})

(defcard "Frantic Coding"
  {:on-play
   {:async true
    :change-in-game-state (req (seq (:deck runner)))
    :effect
    (effect
      (continue-ability
        (let [top-ten (take 10 (:deck runner))]
          {:prompt (str "The top cards of the stack are (top->bottom): " (enumerate-str (map :title top-ten)))
           :choices ["OK"]
           :async true
           :effect
           (effect
             (continue-ability
               {:prompt "Install a program?"
                :choices (concat
                           (->> top-ten
                                (filter #(and (program? %)
                                              (can-pay? state side (assoc eid :source card :source-type :runner-install) % nil
                                                        [(->c :credit (install-cost state side % {:cost-bonus -5}))])))
                                (sort-by :title)
                                (seq))
                           ["Done"])
                :async true
                :effect
                (req (letfn [(log-and-trash-cards [cards]
                               (system-msg state side
                                           (str "uses " (get-title card)
                                                " to trash "
                                                (enumerate-str (map :title cards))
                                                " from the top of the stack"))
                               (trash-cards state side eid cards {:unpreventable true :cause-card card}))]
                       (if (= target "Done")
                         (log-and-trash-cards top-ten)
                       (let [number-of-shuffles (count (turn-events state :runner :runner-shuffle-deck))]
                       (wait-for (runner-install state side (make-eid state {:source card :source-type :runner-install})
                                                 target {:cost-bonus -5})
                                 (if (= number-of-shuffles (count (turn-events state :runner :runner-shuffle-deck)))
                                   (log-and-trash-cards (remove #(same-card? % target) top-ten))
                                   (do (system-msg state side "does not have to trash cards because the stack was shuffled")
                                       (effect-completed state side eid))))))))}
               card nil))})
        card nil))}})

(defcard "\"Freedom Through Equality\""
  {:events [{:event :agenda-stolen
             :msg (msg "add itself to [their] score area as an agenda worth 1 agenda point")
             :effect (req (as-agenda state :runner card 1))}]})

(defcard "Freelance Coding Contract"
  {:on-play
   {:choices {:max 5
              :card #(and (program? %)
                          (in-hand? %))}
    :change-in-game-state (req (seq (:hand runner)))
    :msg (msg "trash " (enumerate-str (map :title targets)) " and gain "
              (* 2 (count targets)) " [Credits]")
    :async true
    :effect (req (wait-for (trash-cards state side targets {:unpreventable true :cause-card card})
                           (gain-credits state side eid (* 2 (count targets)))))}})

(defcard "Game Day"
  {:on-play
   {:msg (msg "draw " (quantify (- (hand-size state :runner) (count (:hand runner))) "card"))
    :change-in-game-state (req (pos? (- (hand-size state :runner) (count (:hand runner)))))
    :async true
    :effect (effect (draw eid (- (hand-size state :runner) (count (:hand runner)))))}})

(defcard "Glut Cipher"
  {:makes-run true
   :on-play {:async true
             :change-in-game-state (req archives-runnable)
             :effect (req (make-run state side eid :archives card))}
   :events [(successful-run-replace-breach
              {:target-server :archives
               :this-card-run true
               :mandatory true
               :ability
               {:req (req (<= 5 (count (:discard corp))))
                :show-discard true
                :async true
                :player :corp
                :waiting-prompt true
                :prompt "Choose 5 cards from Archives to add to HQ"
                :choices {:max 5
                          :all true
                          :card #(and (corp? %)
                                      (in-discard? %))}
                :msg (msg "move "
                          (let [seen (filter :seen targets)
                                m (count  (remove :seen targets))]
                            (str (enumerate-str (map :title seen))
                                 (when (pos? m)
                                   (str (when-not (empty? seen) " and ")
                                        (quantify m "unseen card")))
                                 " into HQ, then trash 5 cards")))
                :effect (req (doseq [c targets]
                               (move state side c :hand))
                             (trash-cards state :corp eid (take 5 (shuffle (:hand (:corp @state)))) {:cause-card card}))}})]})

(defcard "Government Investigations"
  {:flags {:prevent-secretly-spend (req 2)}})

(defcard "Guinea Pig"
  {:on-play
   {:msg "trash all cards in the grip and gain 10 [Credits]"
    :async true
    :effect (req (wait-for (trash-cards state side (:hand runner) {:unpreventable true
                                                                   :cause-card card})
                           (gain-credits state :runner eid 10)))}})

(defcard "Hacktivist Meeting"
  {:static-abilities [{:type :rez-additional-cost
                       :req (req (not (ice? target)))
                       :value [(->c :randomly-trash-from-hand 1)]}]})

(defcard "Harmony AR Therapy"
  (letfn [(choose-end [to-shuffle]
            (let [to-shuffle (sort to-shuffle)]
              {:msg (msg "shuffle " (quantify (count to-shuffle) "card") " back into the stack: " (enumerate-str to-shuffle))
               :effect (req (doseq [c-title to-shuffle]
                              (let [c (some #(when (= (:title %) c-title) %) (:discard runner))]
                                (move state side c :deck)))
                            (shuffle! state side :deck))}))
          (choose-next [to-shuffle target remaining]
            (let [remaining (if (= "Done" target)
                              remaining
                              (remove #(= % target) remaining))
                  to-shuffle (if (= "Done" target)
                               to-shuffle
                               (if target
                                 (concat to-shuffle [target])
                                 []))
                  remaining-choices (- 5 (count to-shuffle))
                  finished? (or (= "Done" target)
                                (= 0 remaining-choices)
                                (empty? remaining))]
              {:prompt (msg (if finished?
                              (str "Shuffling: " (enumerate-str to-shuffle))
                              (str "Choose up to " remaining-choices
                                   (when (not-empty to-shuffle)
                                     " more")
                                   " cards."
                                   (when (not-empty to-shuffle)
                                     (str "[br]Shuffling: " (enumerate-str to-shuffle))))))
               :async true
               :choices (req (if finished?
                               ["OK" "Start over"]
                               (concat remaining (when (not-empty to-shuffle) ["Done"]))))
               :effect (req (if finished?
                              (if (= "OK" target)
                                (continue-ability state side (choose-end to-shuffle) card nil)
                                (continue-ability state side (choose-next '() nil (distinct (map :title (:discard runner)))) card nil))
                              (continue-ability state side (choose-next to-shuffle target remaining) card nil)))}))]
    {:on-play
     {:rfg-instead-of-trashing true
      :waiting-prompt true
      :async true
      :effect (req (if (and (not (zone-locked? state :runner :discard))
                            (pos? (count (:discard runner))))
                     (continue-ability state side (choose-next '() nil (sort (distinct (map :title (:discard runner))))) card nil)
                     (do (system-msg state :runner (str "uses " (:title card) " to shuffle the stack"))
                         (shuffle! state :runner :deck)
                         (effect-completed state side eid))))}}))

(defcard "High-Stakes Job"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :choices (req (let [unrezzed-ice #(seq (filter (complement rezzed?) (:ices (second %))))
                                 bad-zones (keys (filter (complement unrezzed-ice) (get-in @state [:corp :servers])))]
                             (zones->sorted-names (remove (set bad-zones) (get-runnable-zones state side eid card nil)))))
             :async true
             :effect (effect (make-run eid target card))}
   :events [{:event :run-ends
             :req (req (and (:successful target)
                            this-card-run))
             :msg "gain 12 [Credits]"
             :async true
             :effect (effect (gain-credits :runner eid 12))}]})

(defcard "Hostage"
  {:on-play
   {:prompt "Choose a Connection"
    :change-in-game-state (req (seq (:deck runner)))
    :choices (req (cancellable (filter #(has-subtype? % "Connection") (:deck runner)) :sorted))
    :msg (msg "add " (:title target) " from the stack to the grip and shuffle the stack")
    :async true
    :effect (effect (trigger-event :searched-stack)
                    (continue-ability
                      (let [connection target]
                        (if (can-pay? state side (assoc eid :source card :source-type :runner-install) connection nil
                                      [(->c :credit (install-cost state side connection))])
                          {:optional {:prompt (str "Install " (:title connection) "?")
                                      :yes-ability {:async true
                                                    :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) connection nil)
                                                                    (shuffle! :deck))}
                                      :no-ability {:effect (effect (move connection :hand)
                                                                   (shuffle! :deck))}}}
                          {:effect (effect (move connection :hand)
                                           (shuffle! :deck))}))
                      card nil))}})

(defcard "Hot Pursuit"
  {:makes-run true
   :on-play {:async true
             :change-in-game-state (req hq-runnable)
             :effect (req (make-run state side eid :hq card))}
   :events [{:event :successful-run
             :async true
             :msg "gain 9 [Credits] and take 1 tag"
             :req (req (and (= :hq (target-server context))
                            this-card-run))
             :effect (req (wait-for (gain-tags state :runner 1)
                                    (gain-credits state :runner eid 9)))}]})

(defcard "I've Had Worse"
  {:on-play {:async true
             :change-in-game-state (req (seq (:deck runner)))
             :effect (effect (draw eid 3))}
   :on-trash {:when-inactive true
              :interactive (req true)
              :async true
              :req (req (#{:meat :net} (:cause context)))
              :msg "draw 3 cards"
              :effect (effect (draw :runner eid 3))}})

(defcard "Immolation Script"
  {:makes-run true
   :on-play {:async true
             :change-in-game-state (req archives-runnable)
             :effect (req (make-run state side eid :archives card))}
   :events [{:event :breach-server
             :async true
             :req (req (and (= target :archives)
                            ;; don't prompt unless there's at least 1 rezzed piece of ice matching one in Archives
                            (not-empty (set/intersection
                                         (into #{} (map :title (filter ice? (:discard corp))))
                                         (into #{} (map :title (filter rezzed? (all-installed state :corp))))))))
             :prompt "Choose a piece of ice in Archives"
             :choices (req (filter ice? (:discard corp)))
             :effect (effect (continue-ability
                               (let [ice target]
                                 {:async true
                                  :prompt (msg "Choose a rezzed copy of " (:title ice) " to trash")
                                  :choices {:card #(and (ice? %)
                                                        (rezzed? %)
                                                        (same-card? :title % ice))}
                                  :msg (msg "trash " (card-str state target))
                                  :effect (effect (trash eid target {:cause-card card}))})
                               card nil))}]})

(defcard "In the Groove"
  {:events [{:event :runner-install
             :duration :end-of-turn
             :req (req (and (<= 1 (:cost (:card context)))
                            (not (:facedown context))))
             :interactive (req (or (has-subtype? (:card context) "Cybernetic")
                                   (first-event? state side :runner-install)))
             :async true
             :prompt "Choose one"
             :waiting-prompt true
             :choices ["Draw 1 card" "Gain 1 [Credits]"]
             :msg (msg (decapitalize target))
             :effect (req (if (= target "Draw 1 card")
                            (draw state side eid 1)
                            (gain-credits state side eid 1)))}]})

(defcard "Independent Thinking"
  (letfn [(cards-to-draw [targets]
            (* (count targets)
               (if (some #(and (not (facedown? %)) (has-subtype? % "Directive")) targets) 2 1)))]
    {:on-play
     {:prompt "Choose up to 5 installed cards to trash"
      :change-in-game-state (req (seq (all-installed state :runner)))
      :choices {:max 5
                :card #(and (installed? %)
                         (runner? %))}
      :msg (msg "trash " (enumerate-str (map :title targets))
             " and draw " (quantify (cards-to-draw targets) "card"))
      :async true
      :effect (req (wait-for (trash-cards state side targets {:cause-card card})
                             (draw state :runner eid (cards-to-draw targets))))}}))

(defcard "Indexing"
  {:makes-run true
   :on-play {:async true
             :change-in-game-state (req rd-runnable)
             :effect (req (make-run state side eid :rd card))}
   :events [(successful-run-replace-breach
              {:target-server :rd
               :this-card-run true
               :ability
               {:msg "rearrange the top 5 cards of R&D"
                :waiting-prompt true
                :async true
                :effect (effect
                          (continue-ability
                            (let [from (take 5 (:deck corp))]
                              (when (pos? (count from))
                                (reorder-choice :corp :corp from '() (count from) from)))
                            card nil))}})]})

(defcard "Infiltration"
  {:on-play
   {:prompt "Choose one"
    :waiting-prompt true
    :choices ["Gain 2 [Credits]" "Expose a card"]
    :async true
    :effect (effect (continue-ability
                      (if (= target "Expose a card")
                        {:choices {:card #(and (installed? %)
                                               (not (rezzed? %)))}
                         :async true
                         :effect (effect (expose eid target))}
                        {:msg "gain 2 [Credits]"
                         :async true
                         :effect (effect (gain-credits eid 2))})
                      card nil))}})

(defcard "Information Sifting"
  (letfn [(access-pile [cards pile pile-size]
            {:prompt "Choose a card to access. You must access all cards"
             :choices [(str "Card from pile " pile)]
             :async true
             :req (req (if (:max-access run)
                         (< (total-cards-accessed run) (:max-access run))
                         true))
             :effect (req (wait-for
                            (access-card state side (first cards))
                            (if (< 1 (count cards))
                              (continue-ability state side (access-pile (rest cards) pile pile-size) card nil)
                              (effect-completed state side eid))))})
          (which-pile [p1 p2]
            {:player :runner
             :waiting-prompt true
             :prompt "Choose a pile to access"
             :choices [(str "Pile 1 (" (quantify (count p1) "card") ")")
                       (str "Pile 2 (" (quantify (count p2) "card") ")")]
             :async true
             :effect (req (let [choice (if (str/starts-with? target "Pile 1") 1 2)]
                            (system-msg state side (str "chooses to access " target))
                            (continue-ability
                              state side
                              (access-pile (if (= 1 choice) p1 p2) choice (count (if (= 1 choice) p1 p2)))
                              card nil)))})]
    (let [access-effect
          {:player :corp
           :req (req (<= 1 (count (:hand corp))))
           :async true
           :waiting-prompt true
           :prompt (msg "Choose up to " (quantify (dec (count (:hand corp))) "card") " for the first pile")
           :choices {:card #(and (in-hand? %)
                                 (corp? %))
                     :max (req (dec (count (:hand corp))))}
           :effect (effect (continue-ability
                             (which-pile (shuffle targets)
                                         (shuffle (vec (set/difference
                                                         (set (:hand corp)) (set targets)))))
                             card nil))}]
      {:makes-run true
       :on-play {:async true
                 :change-in-game-state (req hq-runnable)
                 :effect (req (make-run state side eid :hq card))}
       :events [(successful-run-replace-breach
                  {:target-server :hq
                   :this-card-run true
                   :mandatory true
                   :ability access-effect})]})))

(defcard "Inject"
  {:on-play
   {:async true
    :change-in-game-state (req (seq (:deck runner)))
    :effect (req (let [cards (take 4 (:deck runner))
                       programs (filter program? cards)
                       others (remove program? cards)]
                   (wait-for
                     (reveal state side cards)
                     (if (seq programs)
                       (wait-for (trash-cards state side programs {:unpreventable true
                                                                   :cause-card card})
                                 (system-msg state side (str "reveals "
                                                             (enumerate-str (map :title programs))
                                                             " from the top of the stack,"
                                                             " trashes them, and gains "
                                                             (count programs) " [Credits]"))
                                 (wait-for (gain-credits state side (count programs))
                                           (doseq [c others]
                                             (move state side c :hand)
                                             (system-msg state side (str "adds " (:title c) " to the grip")))
                                           (effect-completed state side eid)))
                       (do (doseq [c others]
                             (move state side c :hand)
                             (system-msg state side (str "adds " (:title c) " to the grip")))
                           (effect-completed state side eid))))))}})

(defcard "Injection Attack"
  {:makes-run true
   :on-play
   {:prompt "Choose a server"
    :choices (req runnable-servers)
    :change-in-game-state (req (seq runnable-servers))
    :async true
    :effect (effect (continue-ability
                      (let [server target]
                        {:prompt "Choose an icebreaker"
                         :choices {:card #(and (installed? %)
                                               (has-subtype? % "Icebreaker"))}
                         :async true
                         :effect (effect (pump target 2 :end-of-run)
                                         (make-run eid server card))})
                      card nil))}})

(defcard "Inside Job"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :choices (req runnable-servers)
             :change-in-game-state (req (seq runnable-servers))
             :async true
             :effect (effect (make-run eid target card))}
   :events [{:event :encounter-ice
             :req (req (first-run-event? state side :encounter-ice))
             :once :per-run
             :msg (msg "bypass " (:title (:ice context)))
             :effect (req (bypass-ice state))}]})

(defcard "Insight"
  {:on-play
   {:async true
    :player :corp
    :change-in-game-state (req (seq (:deck corp)))
    :waiting-prompt true
    :effect (req (wait-for
                   (resolve-ability state :corp (reorder-choice :corp (take 4 (:deck corp))) card targets)
                   (let [top-4 (take 4 (get-in @state [:corp :deck]))]
                     (system-msg state :runner (str "reveals "
                                                    (enumerate-str (map :title top-4))
                                                    " from the top of R&D (top->bottom)"))
                     (reveal state :runner eid top-4))))}})

(defcard "Interdiction"
  (let [ab (effect (register-turn-flag!
                     card :can-rez
                     (fn [state _side card]
                       (if (and (= (:active-player @state) :runner) (not (ice? card)))
                         ((constantly false)
                          (toast state :corp "Cannot rez non-ice on the Runner's turn due to Interdiction"))
                         true))))]
    {:on-play {:msg "prevent the Corp from rezzing non-ice cards on the Runner's turn"
               :effect ab}
     :events [{:event :runner-turn-begins
               :effect ab}]
     :leave-play (req (clear-all-flags-for-card! state side card))}))

(defcard "Into the Depths"
  ;; note - Into the Depths specifies "each time you passed an ICE". This means, unlike bravado,
  ;; passing the same ice multiple times (ie thimblerig) counts.
  (let [all [{:msg "gain 4 [Credits]"
              :async true
              :effect (effect (gain-credits eid 4))}
             {:msg "install a program from the stack"
              :async true
              :req (req (not (install-locked? state side)))
              :effect (effect (continue-ability
                                {:prompt "Choose a program to install"
                                 :msg (msg (if (= target "Done")
                                             "shuffle the stack"
                                             (str "install " (:title target) " from the stack")))
                                 :choices (req (concat
                                                 (->> (:deck runner)
                                                      (filter
                                                        #(and (program? %)
                                                              (can-pay? state side
                                                                        (assoc eid :source card :source-type :runner-install)
                                                                        % nil [(->c :credit (install-cost state side %))])))
                                                      (sort-by :title)
                                                      (seq))
                                                 ["Done"]))
                                 :async true
                                 :effect (req (trigger-event state side :searched-stack)
                                              (shuffle! state side :deck)
                                              (if (= target "Done")
                                                (effect-completed state side eid)
                                                (runner-install state side (assoc eid :source card :source-type :runner-install) target nil)))}
                                card nil))}
             {:async true
              :effect (effect (continue-ability (charge-ability state side eid card) card nil))
              :msg "charge a card"}]
        choice (fn choice [abis rem]
                 {:prompt (str "Choose an ability to resolve (" rem " remaining)")
                  :waiting-prompt true
                  :choices (map #(capitalize (:msg %)) abis)
                  :async true
                  :effect (req (let [chosen (some #(when (= target (capitalize (:msg %))) %) abis)]
                                 (wait-for
                                   (resolve-ability state side chosen card nil)
                                   (if (< 1 rem)
                                     (continue-ability state side (choice (remove-once #(= % chosen) abis) (dec rem)) card nil)
                                     (effect-completed state side eid)))))})]
    {:makes-run true
     :on-play {:prompt "Choose a server"
               :change-in-game-state (req (seq runnable-servers))
               :choices (req runnable-servers)
               :async true
               :effect (effect (register-events
                                 card
                                 [{:event :pass-ice
                                   :silent (req true)
                                   :duration :end-of-run
                                   :effect (effect (update! (update-in (get-card state card) [:special :how-deep-are-we] (fnil inc 0))))}])
                               (make-run eid target card))}
     :events [{:event :successful-run
               :interactive (req true)
               :async true
               :req (req this-card-run)
               :effect (req (let [ice-passed (get-in card [:special :how-deep-are-we])
                                  num-choices (if (nil? ice-passed) 0 (min 3 ice-passed))]
                              (if (< 0 num-choices)
                                (continue-ability state side (choice all num-choices) card nil)
                                (effect-completed state side eid))))}]}))

(defcard "Isolation"
  {:on-play
   {:additional-cost [(->c :resource 1)]
    :msg "gain 7 [Credits]"
    :async true
    :effect (effect (gain-credits eid 7))}})

(defcard "Itinerant Protesters"
  {:on-play {:msg "reduce the Corp's maximum hand size by 1 for each bad publicity"}
   :static-abilities [(corp-hand-size+ (req (- (count-bad-pub state))))]})

(defcard "Jailbreak"
  {:makes-run true
   :on-play {:change-in-game-state (req (or rd-runnable hq-runnable))
             :prompt "Choose a server"
             :choices (req [(when hq-runnable "HQ")
                            (when rd-runnable "R&D")])
             :async true
             :effect (req (make-run state side eid target card))}
   :events [{:event :successful-run
             :silent (req true)
             :async true
             :req (req (and (#{:hq :rd} (target-server context))
                            this-card-run))
             :effect (effect (register-events
                              card [(breach-access-bonus (target-server context) 1 {:duration :end-of-run})])
                             (draw eid 1))}]})

(defcard "Joy Ride"
  {:on-play {:async true
             :change-in-game-state (req rd-runnable)
             :effect (req (make-run state side eid :rd card))}
   :events [{:event :successful-run
             :silent (req true)
             :async true
             :req (req (and (= :rd (target-server context))
                            this-card-run))
             :msg "draw 5 cards"
             :effect (effect (draw eid 5))}]})

(defcard "Katorga Breakout"
  {:makes-run true
   :on-play {:async true
             :prompt "Choose a server"
             :change-in-game-state (req (seq runnable-servers))
             :choices (req runnable-servers)
             :effect (effect (make-run eid target card))}
   :events [{:event :successful-run
             :req (req (and this-card-run
                            (not (zone-locked? state :runner :discard))))
             :prompt "Choose 1 card to add to the grip"
             :waiting-prompt true
             :choices (req (cancellable (:discard runner) :sorted))
             :msg (msg "add " (:title target) " to the grip")
             :effect (effect (move target :hand))}]})

(defcard "Khusyuk"
  (let [access-revealed (fn [revealed]
                          {:async true
                           :prompt "Choose a card to access"
                           :waiting-prompt true
                           :not-distinct true
                           :choices revealed
                           :req (req (not= (:max-access run) 0))
                           :effect (effect (access-card eid target))})
        select-install-cost (fn [state]
                              (let [current-values
                                    (->> (all-active-installed state :runner)
                                         (keep :cost)
                                         (remove zero?)
                                         frequencies
                                         (merge {1 0})
                                         (into (sorted-map)))]
                                {:async true
                                 :prompt "Choose an install cost from among your installed cards"
                                 ;; We don't want to generate 99 prompt buttons, so only add 99 at the end
                                 :choices (mapv str (for [x (->> current-values keys last inc (range 1) (#(concat % [99])))]
                                                      (str x " [Credit]: "
                                                           (quantify (get current-values x 0) "card"))))
                                 :effect (effect (complete-with-result
                                                   eid [(str->int (first (str/split target #" ")))
                                                        (min 6 (str->int (nth (str/split target #" ") 2)))]))}))
        access-effect
        {:async true
         :effect (req (wait-for
                        (resolve-ability state side (select-install-cost state) card nil)
                        (let [revealed (seq (take (second async-result) (:deck corp)))]
                          (system-msg state :runner (str "uses " (:title card) " to choose an install cost of "
                                                         (first async-result)
                                                         " [Credit] and reveals "
                                                         (if revealed
                                                           (str (enumerate-str (map :title revealed))
                                                                " from the top of R&D (top->bottom)")
                                                           "no cards")))
                          (wait-for
                            (resolve-ability
                              state side
                              (when revealed
                                {:async true
                                 :effect (effect (reveal eid revealed))})
                              card nil)
                            (wait-for
                              (resolve-ability state side (when (and revealed (not (get-only-card-to-access state)))
                                                            (access-revealed revealed))
                                               card nil)
                              (shuffle! state :corp :deck)
                              (system-msg state :runner "shuffles R&D")
                              (effect-completed state side eid))))))}]
    {:makes-run true
     :on-play {:async true
               :change-in-game-state (req rd-runnable)
               :effect (req (make-run state side eid :rd card))}
     :events [(successful-run-replace-breach
                {:target-server :rd
                 :this-card-run true
                 :mandatory true
                 :ability access-effect})]}))

(defcard "Knifed"
  (cutlery "Barrier"))

(defcard "Kraken"
  {:on-play
   {:req (req (:stole-agenda runner-reg))
    :prompt "Choose a server"
    :change-in-game-state (req (some ice? (all-installed state :corp)))
    :choices (req servers)
    :msg (msg "force the Corp to trash a piece of ice protecting " target)
    :async true
    :effect (effect
              (continue-ability
                (let [serv (second (server->zone state target))]
                  {:player :corp
                   :async true
                   :prompt (msg "Choose a piece of ice in " target " to trash")
                   :choices {:card #(and (ice? %)
                                         (= serv (second (get-zone %))))}
                   :effect (effect (system-msg (str "trashes " (card-str state target)))
                                   (trash :corp eid target {:cause-card card}))})
                card nil))}})

(defcard "Labor Rights"
  {:on-play
   {:rfg-instead-of-trashing true
    :async true
    :change-in-game-state (req (or (seq (:deck runner))
                          (and (seq (:discard runner))
                               (not (zone-locked? state :runner :discard)))))
    :effect (req
              (let [mill-count (min 3 (count (:deck runner)))
                    top-n-msg (seq (take mill-count (:deck runner)))]
                (wait-for (mill state :runner :runner mill-count)
                          (system-msg state :runner (if top-n-msg
                                                      (str "trashes " (enumerate-str (map :title top-n-msg))
                                                           " from the top of the stack")
                                                      "trashes no cards from the top of the stack"))
                          (let [heap-count (min 3 (count (get-in @state [:runner :discard])))]
                            (continue-ability
                              state side
                              (if (not (zone-locked? state :runner :discard))
                                {:prompt (str "Choose " (quantify heap-count "card") " to shuffle into the stack")
                                 :show-discard true
                                 :async true
                                 :choices {:max heap-count
                                           :all true
                                           :not-self true
                                           :card #(and (runner? %)
                                                       (in-discard? %))}
                                 :effect (req (doseq [c targets]
                                                (move state side c :deck))
                                              (system-msg state :runner (str "shuffles " (enumerate-str (map :title targets))
                                                                             " from the heap into the stack, and draws 1 card"))
                                              (shuffle! state :runner :deck)
                                              (draw state :runner eid 1))}
                                {:effect (effect
                                           (do (system-msg state :runner "shuffles the stack and draws 1 card")
                                               (shuffle! state :runner :deck)
                                               (draw state :runner eid 1)))})
                              card nil)))))}})

(defcard "Lawyer Up"
  {:on-play
   {:msg "remove 2 tags and draw 3 cards"
    :change-in-game-state (req (or tagged (seq (:deck runner))))
    :async true
    :effect (req (wait-for (lose-tags state side 2)
                           (draw state side eid 3)))}})

(defcard "Lean and Mean"
  {:makes-run true
   :on-play
   {:prompt "Choose a server"
    :change-in-game-state (req (seq runnable-servers))
    :choices (req runnable-servers)
    :msg (msg "make a run on " target
              (when (<= (count (filter program? (all-active-installed state :runner))) 3)
                ", giving +2 strength to all icebreakers"))
    :async true
    :effect (req (when (<= (count (filter program? (all-active-installed state :runner))) 3)
                   (pump-all-icebreakers state side 2 :end-of-run))
                 (make-run state side eid target card))}})

(defcard "Leave No Trace"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :msg "make a run and derez all ice that is rezzed during this run"
             :choices (req runnable-servers)
             :change-in-game-state (req (seq runnable-servers))
             :async true
             :effect (req (make-run state side eid target (get-card state card)))}
   :events [{:event :run-ends
             :effect (req (let [rezzed-ice (->> (run-events target :rez)
                                                (keep (fn [[{:keys [card]}]]
                                                        (when (ice? card)
                                                          (get-card state card))))
                                                (filter rezzed?))]
                            (doseq [ice rezzed-ice]
                              (derez state :runner ice))
                            (when (seq rezzed-ice)
                              (system-msg state :runner (str "uses " (:title card) " to derez " (enumerate-str (map :title rezzed-ice)))))))}]})

(defcard "Legwork"
  {:makes-run true
   :on-play {:async true
             :change-in-game-state (req hq-runnable)
             :effect (req (make-run state side eid :hq card))}
   :events [{:event :successful-run
             :silent (req true)
             :req (req (and (= :hq (target-server context))
                            this-card-run))
             :effect (effect (register-events
                              card [(breach-access-bonus :hq 2 {:duration :end-of-run})]))}]})

(defcard "Leverage"
  {:on-play
   {:optional
    {:req (req (some #{:hq} (:successful-run runner-reg)))
     :player :corp
     :prompt "Take 2 bad publicity?"
     :yes-ability {:player :corp
                   :msg "takes 2 bad publicity"
                   :effect (effect (gain-bad-publicity :corp 2))}
     :no-ability {:player :runner
                  :msg "is immune to damage until the beginning of the Runner's next turn"
                  :effect (effect
                            (register-events
                              card
                              [{:event :pre-damage
                                :duration :until-runner-turn-begins
                                :effect (effect (damage-prevent :net Integer/MAX_VALUE)
                                                (damage-prevent :meat Integer/MAX_VALUE)
                                                (damage-prevent :brain Integer/MAX_VALUE))}
                               {:event :runner-turn-begins
                                :duration :until-runner-turn-begins
                                :effect (effect (unregister-floating-events :until-runner-turn-begins))}]))}}}})

(defcard "Levy AR Lab Access"
  {:on-play
   {:msg (msg (if (not (zone-locked? state :runner :discard))
                "shuffle the grip and heap into the stack and draw 5 cards"
                "shuffle the grip into the stack and draw 5 cards"))
    :rfg-instead-of-trashing true
    :async true
    :effect (effect (shuffle-into-deck :hand :discard)
                    (draw eid 5))}})

(defcard "Lucky Find"
  {:on-play
   {:msg "gain 9 [Credits]"
    :async true
    :effect (effect (gain-credits eid 9))}})

(defcard "Mad Dash"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :change-in-game-state (req (seq runnable-servers))
             :choices (req runnable-servers)
             :async true
             :effect (effect (make-run eid target card))}
   :events [{:event :run-ends
             :async true
             :req (req this-card-run)
             :effect (req (if (:did-steal target)
                            (do (system-msg state :runner
                                            (str "adds Mad Dash to [their] score area as an agenda worth 1 agenda point"))
                                (as-agenda state :runner (get-card state card) 1)
                                (effect-completed state side eid))
                            (do (system-msg state :runner
                                            (str "suffers 1 meat damage from Mad Dash"))
                                (damage state side eid :meat 1 {:card card}))))}]})

(defcard "Making an Entrance"
  (letfn [(entrance-trash [cards]
            {:prompt "Choose a card to trash"
             :choices (concat cards ["Done"])
             :async true
             :effect (req (if (= target "Done")
                            (continue-ability
                              state side
                              (when (seq cards)
                                (reorder-choice :runner :corp cards '()
                                                (count cards) cards))
                              card nil)
                            (wait-for (trash state side target {:unpreventable true
                                                                :cause-card card})
                                      (system-msg state side (str "trashes " (:title target)))
                                      (continue-ability
                                        state side
                                        (when-let [cards (seq (remove-once #(= % target) cards))]
                                          (entrance-trash cards))
                                        card nil))))})]
    {:on-play
     {:msg "look at and trash or rearrange the top 6 cards of the stack"
      :change-in-game-state (req (seq (:deck runner)))
      :async true
      :waiting-prompt true
      :effect (effect (continue-ability (entrance-trash (take 6 (:deck runner))) card nil))}}))

(defcard "Marathon"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :change-in-game-state (req (seq (filter #(can-run-server? state %) remotes)))
             :choices (req (filter #(can-run-server? state %) remotes))
             :async true
             :effect (effect (make-run eid target card))}
   :events [{:event :run-ends
             :req (req this-card-run)
             :effect (req
                       (let [blocked-server (first (:server target))]
                         (register-lingering-effect
                           state side card
                           {:type :cannot-run-on-server
                            :req (req true)
                            :value [blocked-server]
                            :duration :end-of-turn}))
                       (when (:successful target)
                         (system-msg state :runner (str "gains [Click] and adds Marathon to [their] grip"))
                         (gain-clicks state :runner 1)
                         (move state :runner card :hand)
                         (unregister-events state side card)))}]})

(defcard "Mars for Martians"
  (letfn [(count-clan [state] (count (filter #(and (has-subtype? % "Clan") (resource? %))
                                             (all-active-installed state :runner))))]
    {:on-play
     {:msg (msg "draw " (quantify (count-clan state) "card") " and gain " (count-tags state) " [Credits]")
      :async true
      :effect (req (wait-for (draw state side (count-clan state))
                             (gain-credits state side eid (count-tags state))))}}))

(defcard "Mass Install"
  (letfn [(mhelper [n]
            (when (< n 3)
              {:async true
               :req (req (some #(and (program? %)
                                     (can-pay? state side (assoc eid :source card :source-type :runner-install) % nil
                                               [(->c :credit (install-cost state side %))]))
                               (:hand runner)))
               :prompt "Choose a program to install"
               :choices {:req (req (and (program? target)
                                        (in-hand? target)
                                        (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                                  [(->c :credit (install-cost state side target))])))}
               :effect (req (wait-for (runner-install state side target nil)
                                      (continue-ability state side (mhelper (inc n)) card nil)))}))]
    {:on-play
     {:async true
      :change-in-game-state (req (seq (:hand runner)))
      :effect (effect (continue-ability (mhelper 0) card nil))}}))

(defcard "Meeting of Minds"
  (letfn [(credit-gain-abi [type]
            {:choices {:max (req (count (:hand runner)))
                       :card #(and (runner? %)
                                   (in-hand? %)
                                   (has-subtype? % type))}
             :prompt (msg "Choose any number of " (decapitalize type) " resources to reveal")
             :msg (msg "reveal " (enumerate-str (map :title (sort-by :title targets))) " from the Grip and gain " (count targets) " [Credits]")
             :async true
             :effect (req (wait-for
                             (reveal state side targets)
                             (gain-credits state side eid (* 1 (count targets)))))})
          (tutor-abi [type]
            {:prompt (str "Choose a " (decapitalize type) " resource")
             :choices (req (cancellable (filter #(has-subtype? % type)
                                                (:deck runner)) :sorted))
             :msg (msg "add " (:title target) " from the stack to the grip and shuffle the stack")
             :async true
             :effect (effect (trigger-event :searched-stack)
                             (move target :hand)
                             (shuffle! :deck)
                             (continue-ability (credit-gain-abi type) card nil))})]
    {:on-play {:prompt "Choose one"
               :async true
               :waiting-prompt true
               :choices ["Connection" "Virtual"]
               :effect (req (let [choice target]
                              (continue-ability
                                state side
                                {:optional
                                 {:prompt (str "Search the stack for a " (decapitalize choice) " resource?")
                                  :yes-ability
                                  {:async true
                                   :msg (msg "search the stack for a " (decapitalize choice) " resource")
                                   :effect (effect (continue-ability (tutor-abi choice) card nil))}
                                  :no-ability
                                  {:async true
                                   :effect (effect (continue-ability (credit-gain-abi choice) card nil))}}}
                                card nil)))}}))

(defcard "Mining Accident"
  {:on-play
   {:req (req (some #{:hq :rd :archives} (:successful-run runner-reg)))
    :rfg-instead-of-trashing true
    :msg (msg "force the corp to " (decapitalize target))
    :waiting-prompt true
    :player :corp
    :prompt "Choose one"
    :choices (req [(when (can-pay? state :corp eid card nil (->c :credit 5))
                     "Pay 5 [Credits]")
                   "Take 1 bad publicity"])
    :async true
    :effect (req (if (= target "Pay 5 [Credits]")
                   (wait-for (pay state :corp (make-eid state eid) card (->c :credit 5))
                             (system-msg state :corp (:msg async-result))
                             (effect-completed state side eid))
                   (do (gain-bad-publicity state :corp 1)
                       (effect-completed state side eid))))}})

(defcard "Möbius"
  {:on-play
   {:async true
    :change-in-game-state (req rd-runnable)
    :effect (req (wait-for (make-run state side :rd card)
                           (let [card (get-card state card)]
                             (if (get-in card [:special :run-again])
                               (make-run state side eid :rd card)
                               (effect-completed state side eid)))))}
   :events [{:event :successful-run
             :req (req (and (get-in card [:special :run-again])
                            (= :rd (target-server context))))
             :msg "gain 4 [Credits]"
             :async true
             :effect (effect (gain-credits eid 4))}
            {:event :run-ends
             :interactive (req true)
             :optional {:req (req (and (:successful target)
                                       (not (get-in card [:special :run-again]))
                                       (= [:rd] (:server target))))
                        :prompt "Make another run on R&D?"
                        :yes-ability {:effect (effect (clear-wait-prompt :corp)
                                                      (update! (assoc-in card [:special :run-again] true)))}}}]})

(defcard "Modded"
  {:on-play
   {:prompt "Choose a program or piece of hardware to install"
    :change-in-game-state (req (seq (:hand runner)))
    :choices {:req (req (and (or (hardware? target)
                                 (program? target))
                             (in-hand? target)
                             (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                       [(->c :credit (install-cost state side target {:cost-bonus -3}))])))}
    :async true
    :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target {:cost-bonus -3}))}})

(defcard "Moshing"
  {:on-play
   {:additional-cost [(->c :trash-from-hand 3)]
    :msg "draw 3 cards and gain 3 [Credits]"
    :async true
    :effect (req (wait-for (draw state side 3)
                           (gain-credits state side eid 3)))}})

(defcard "Mutual Favor"
  {:on-play
   {:prompt "Choose an Icebreaker"
    :change-in-game-state (req (seq (:deck runner)))
    :choices (req (cancellable (filter #(has-subtype? % "Icebreaker") (:deck runner)) :sorted))
    :msg (msg "add " (:title target) " from the stack to the grip and shuffle the stack")
    :async true
    :effect (effect (trigger-event :searched-stack)
                    (continue-ability
                      (let [icebreaker target]
                        (if (and (:successful-run runner-reg)
                                 (can-pay? state side (assoc eid :source card :source-type :runner-install) icebreaker nil
                                           [(->c :credit (install-cost state side icebreaker))]))
                          {:optional
                           {:prompt (str "Install " (:title icebreaker) "?")
                            :yes-ability
                            {:async true
                             :msg (msg " install " (:title icebreaker))
                             :effect (req (runner-install state side (assoc eid :source card :source-type :runner-install) icebreaker nil)
                                          (shuffle! state side :deck))}
                            :no-ability
                            {:effect (req (move state side icebreaker :hand)
                                          (shuffle! state side :deck))}}}
                          {:effect (req (move state side icebreaker :hand)
                                        (shuffle! state side :deck))}))
                      card nil))}})

(defcard "Net Celebrity"
  {:recurring 1
   :interactions {:pay-credits {:req (req run)
                                :type :recurring}}})

(defcard "Networking"
  {:on-play
   {:async true
    :msg (msg (if tagged
                "remove 1 tag"
                "do nothing"))
    :effect (req (wait-for (lose-tags state side 1)
                           (continue-ability
                             state side
                             {:optional
                              {:prompt (msg "Pay 1 [Credits] to add " (:title card) " to Grip?")
                               :yes-ability
                               {:cost [(->c :credit 1)]
                                :msg "add itself to the Grip"
                                :effect (effect (move card :hand))}}}
                             card nil)))}})

(defcard "Notoriety"
  {:on-play
   {:req (req (and (some #{:hq} (:successful-run runner-reg))
                   (some #{:rd} (:successful-run runner-reg))
                   (some #{:archives} (:successful-run runner-reg))))
    :msg (msg "add itself to [their] score area as an agenda worth 1 agenda point")
    :effect (req (as-agenda state :runner card 1))}})

(defcard "Office Supplies"
  {:on-play
   {:play-cost-bonus (req (- (get-link state)))
    :prompt "Choose one"
    :waiting-prompt true
    :choices ["Gain 4 [Credits]" "Draw 4 cards"]
    :msg (msg (decapitalize target))
    :async true
    :effect (req (if (= target "Gain 4 [Credits]")
                   (gain-credits state :runner eid 4)
                   (draw state :runner eid 4)))}})

(defcard "On the Lam"
  {:on-play {:prompt "Choose a resource to host On the Lam on"
             :choices {:card #(and (resource? %)
                                   (installed? %))}
             :change-in-game-state (req (some resource? (all-active-installed state :runner)))
             :async true
             :effect (req (system-msg state side (str "hosts On the Lam on " (:title target)))
                          (install-as-condition-counter state side eid card target))}
   :interactions {:prevent [{:type #{:net :brain :meat :tag}
                             :req (req true)}]}
   :abilities [{:label "Avoid 3 tags"
                :msg "avoid up to 3 tags"
                :async true
                :cost [(->c :trash-can)]
                :effect (effect (tag-prevent :runner eid 3))}
               {:label "Prevent up to 3 damage"
                :msg "prevent up to 3 damage"
                :cost [(->c :trash-can)]
                :effect (effect (damage-prevent :net 3)
                                (damage-prevent :meat 3)
                                (damage-prevent :brain 3))}]})

(defcard "Out of the Ashes"
  (let [ashes-run {:prompt "Choose a server"
                   :choices (req runnable-servers)
                   :async true
                   :effect (effect (make-run eid target card))}
        ashes-recur (fn ashes-recur []
                      {:optional
                       {:req (req (not (zone-locked? state :runner :discard)))
                        :prompt (req (str "Remove Out of the Ashes from the game to make a run? ("
                                          (count (filter #(= "Out of the Ashes" (:title %)) (:discard runner)))
                                          " available)"))
                        :yes-ability
                        {:async true
                         :msg "removes Out of the Ashes from the game to make a run"
                         :effect
                         (req (move state side card :rfg)
                              (wait-for (resolve-ability state side (make-eid state eid) ashes-run card nil)
                                        (if-let [next-out-of-ashes (some #(when (and (= "Out of the Ashes" (:title %))
                                                                                     (not (same-card? card %))) %)
                                                                         (:discard runner))]
                                          (continue-ability state side (ashes-recur) (get-card state next-out-of-ashes) nil)
                                          (effect-completed state side eid))))}}})]
    {:makes-run true
     :on-play {:prompt "Choose a server"
               :choices (req runnable-servers)
               :change-in-game-state (req (seq runnable-servers))
               :async true
               :effect (effect (make-run eid target card))}
     :events [{:event :runner-turn-begins
               :async true
               :interactive (req true)
               :silent (req (let [ashes (filter #(= "Out of the Ashes" (:title %))
                                                (:discard runner))]
                              (or (not= card (first ashes))
                                  (not (not-used-once? state {:once :per-turn
                                                              :once-key :out-of-ashes}
                                                       card)))))
               :location :discard
               :once :per-turn
               :once-key :out-of-ashes
               :effect (req (wait-for (resolve-ability state side (make-eid state eid) (ashes-recur) card nil)
                                      (effect-completed state side eid)))}]}))

(defcard "Overclock"
  {:makes-run true
   :data {:counter {:credit 5}}
   :interactions {:pay-credits {:req (req run)
                                :type :credit}}
   :on-play {:prompt "Choose a server"
             :change-in-game-state (req (seq runnable-servers))
             :choices (req runnable-servers)
             :async true
             :effect (effect (make-run eid target card))}})

(defcard "Paper Tripping"
  {:on-play
   {:msg "remove all tags"
    :change-in-game-state (req tagged)
    :async true
    :effect (effect (lose-tags eid :all))}})

(defcard "Peace in Our Time"
  {:on-play
   {:req (req (not (:scored-agenda corp-reg-last)))
    :msg "gain 10 [Credits]. The Corp gains 5 [Credits]"
    :async true
    :effect (req (wait-for (gain-credits state :runner 10)
                           (register-turn-flag! state side card :can-run nil)
                           (gain-credits state :corp eid 5)))}})

(defcard "Pinhole Threading"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :change-in-game-state (req (seq runnable-servers))
             :choices (req runnable-servers)
             :async true
             :effect (effect (make-run eid target card))}
   :events [(successful-run-replace-breach
              {:mandatory true
               :this-card-run true
               :ability
               {:prompt "Choose a card in the root of another server to access"
                :choices {:req (req (and (not= (first (:server (:run @state)))
                                               (second (get-zone (get-nested-host target))))
                                         (= (last (get-zone (get-nested-host target)))
                                            :content)))}
                :async true
                :waiting-prompt true
                :effect (req (if (agenda? target)
                               (let [protected-card target]
                                 ;; Prevent the runner from stealing or trashing agendas
                                 ;; we can't just register a run flag, because there may be another
                                 ;; breach after this (ie ganked into kitsune),
                                 ;; so we have to clear the flags after this access.
                                 (register-run-flag!
                                   state side
                                   card :can-steal
                                   (fn [_ _ c] (not (same-card? c protected-card))))
                                 (register-run-flag!
                                   state side
                                   card :can-trash
                                   (fn [_ _ c] (not (same-card? c protected-card))))
                                 (wait-for (access-card state side protected-card)
                                           (clear-run-flag! state side card :can-steal)
                                           (clear-run-flag! state side card :can-trash)
                                           (effect-completed state side eid)))
                               (access-card state side eid target)))}})]})

(defcard "Planned Assault"
  {:on-play
   {:prompt "Choose a Run event"
    :change-in-game-state (req (seq (:deck runner)))
    :choices (req (sort-by :title
                           (filter #(and (has-subtype? % "Run")
                                         (can-pay? state side (assoc eid :source card :source-type :play) % nil
                                                   [(->c :credit (play-cost state side %))]))
                                   (:deck runner))))
    :msg (msg "play " (:title target))
    :async true
    :effect (effect (trigger-event :searched-stack)
                    (shuffle! :deck)
                    (play-instant eid target {:no-additional-cost true}))}})

(defcard "Political Graffiti"
  {:makes-run true
   :on-play {:async true
             :change-in-game-state (req archives-runnable)
             :effect (req (make-run state side eid :archives card))}
   :static-abilities [{:type :agenda-value
                       :req (req (same-card? (:host card) target))
                       :value -1}]
   :events [{:event :purge
             :condition :hosted
             :async true
             :msg "trash itself"
             :effect (req (wait-for (trash state :runner card {:cause :purge
                                                               :cause-card card})
                                    (update-all-agenda-points state side)
                                    (effect-completed state side eid)))}
            (successful-run-replace-breach
              {:target-server :archives
               :this-card-run true
               :mandatory true
               :ability
               {:prompt (msg "Choose an agenda to host " (:title card) " on")
                :choices {:req (req (in-corp-scored? state side target))}
                :msg (msg "host itself on " (:title target) " as a hosted condition counter")
                :async true
                :effect (req (wait-for (install-as-condition-counter state side (make-eid state eid) card target)
                                       (update-all-agenda-points state side)
                                       (effect-completed state side eid)))}})]})

(defcard "Populist Rally"
  {:on-play {:req (req (seq (filter #(has-subtype? % "Seedy") (all-active-installed state :runner))))
             :msg (msg "give the Corp 1 fewer [Click] to spend on [corp-pronoun] next turn")
             :effect (effect (lose :corp :click-per-turn 1))}
   :events [{:event :corp-turn-ends
             :duration :until-corp-turn-ends
             :effect (effect (gain :corp :click-per-turn 1))}]})

(defcard "Power Nap"
  {:on-play
   {:async true
    :msg (msg "gain " (+ 2 (count (filter #(has-subtype? % "Double") (:discard runner)))) " [Credits]")
    :effect (effect (gain-credits eid (+ 2 (count (filter #(has-subtype? % "Double")
                                                          (:discard runner))))))}})

(defcard "Power to the People"
  {:events [{:event :access
             :req (req (agenda? target))
             :duration :end-of-turn
             :once :per-turn
             :unregister-once-resolved true
             :msg "gain 7 [Credits]"
             :async true
             :effect (effect (gain-credits eid 7))}]})

(defcard "Prey"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :change-in-game-state (req (seq runnable-servers))
             :choices (req runnable-servers)
             :async true
             :effect (effect (make-run eid target card))}
   :events [{:event :pass-ice
             :req (req (and (rezzed? (:ice context))
                            (not-used-once? state {:once :per-run} card)
                            (<= (get-strength (:ice context)) (count (all-installed state :runner)))))
             :async true
             :effect
             (effect
               (continue-ability
                 (let [ice (:ice context)]
                   (if (pos? (get-strength ice))
                     {:optional
                      {:prompt (str "Trash " (quantify (get-strength ice) "installed card")
                                    " to trash " (:title ice) "?")
                       :once :per-run
                       :yes-ability
                       {:async true
                        :cost [(->c :trash-installed (get-strength ice))]
                        :msg (msg "trash " (card-str state ice))
                        :effect (effect (trash eid ice {:cause-card card}))}}}
                     {:optional
                      {:prompt (str "Trash " (:title ice) "?")
                       :once :per-run
                       :yes-ability
                       {:async true
                        :msg (msg "trash " (card-str state ice))
                        :effect (effect (trash eid ice {:cause-card card}))}}}))
                 card nil))}]})

(defcard "Privileged Access"
  (let [install-program-from-heap
          {:prompt "Choose a program to install"
           :waiting-prompt true
           :async true
           :req (req (and
                       (not (get-in card [:special :maybe-a-bonus-tag]))
                       (not (zone-locked? state :runner :discard))
                       (not (install-locked? state side))
                       (threat-level 3 state)))
           :interactive (req true)
           :ability-name "Privileged Access (program)"
           :choices (req (concat
                           (->> (:discard runner)
                                (filter
                                  #(and (program? %)
                                        (can-pay? state side
                                                  (assoc eid :source card :source-type :runner-install)
                                                  % nil [(->c :credit (install-cost state side %))])))
                                (sort-by :title)
                                (seq))
                           ["Done"]))
           :effect (req (if (= target "Done")
                          (effect-completed state side eid)
                          (do (update! state side (assoc-in card [:special :maybe-a-bonus-tag] true))
                              (wait-for
                                (runner-install state side (make-eid state (assoc eid :source card :source-type :runner-install)) target)
                                (update! state side (dissoc-in card [:special :maybe-a-bonus-tag]))
                                (system-msg state side (str "uses " (:title card) " to install " (:title target) " from the heap"))
                                (effect-completed state side eid)))))}
        install-resource-from-heap
          {:prompt "Choose a resource to install, paying 2 [Credits] less"
           :waiting-prompt true
           :req (req (and
                       (not (get-in card [:special :maybe-a-bonus-tag]))
                       (not (zone-locked? state :runner :discard))
                          (not (install-locked? state side))))
           :async true
           :interactive (req true)
           :ability-name "Privileged Access (resource)"
           :choices (req (concat
                           (->> (:discard runner)
                                (filter
                                  #(and (resource? %)
                                        (can-pay? state side
                                                  (assoc eid :source card :source-type :runner-install)
                                                  % nil [(->c :credit (install-cost state side % {:cost-bonus -2}))])))
                                (sort-by :title)
                                (seq))
                           ["Done"]))
           :effect (req (if (= target "Done")
                          (effect-completed state side eid)
                          (do (update! state side (assoc-in card [:special :maybe-a-bonus-tag] true))
                              (wait-for
                                (runner-install state side (make-eid state (assoc eid :source card :source-type :runner-install)) target {:cost-bonus -2})
                                (update! state side (dissoc-in card [:special :maybe-a-bonus-tag]))
                                (system-msg state side (str "uses " (:title card) " to install " (:title target) " from the heap, paying 2 [Credits] less"))
                                (effect-completed state side eid)))))}]
    {:makes-run true
     :on-play {:async true
               :req (req (not tagged))
               :change-in-game-state (req archives-runnable)
               :effect (req (make-run state side eid :archives card))}
     :events [(successful-run-replace-breach
                {:target-server :archives
                 :this-card-run true
                 :mandatory true
                 :ability {:async true
                           :msg "take 1 tag"
                           :effect (req
                                     (register-pending-event state :runner-gain-tag
                                                             card install-resource-from-heap)
                                     (register-pending-event state :runner-gain-tag
                                                             card install-program-from-heap)
                                     (wait-for (gain-tags state :runner 1)
                                               (unregister-events state side card)
                                               (effect-completed state side eid)))}})]}))

(defcard "Process Automation"
  {:on-play
   {:msg "gain 2 [Credits] and draw 1 card"
    :async true
    :effect (req (wait-for (gain-credits state side 2)
                           (draw state side eid 1)))}})

(defcard "Push Your Luck"
  (letfn [(corp-choice [spent]
            {:player :corp
             :waiting-prompt true
             :prompt "Choose one"
             :choices ["Even" "Odd"]
             :async true
             :effect (req (let [correct-guess ((if (= target "Even") even? odd?) spent)]
                            (wait-for
                              (lose-credits state :runner (make-eid state eid) spent)
                              (system-msg state :runner (str "spends " spent " [Credit]"))
                              (system-msg state :corp (str (if correct-guess " " " in")
                                                           "correctly guesses " (decapitalize target)))
                              (wait-for
                                (trigger-event-simult state side :reveal-spent-credits nil nil spent)
                                (if correct-guess
                                  (effect-completed state side eid)
                                  (do (system-msg state :runner (str "gains " (* 2 spent) " [Credits]"))
                                      (gain-credits state :runner eid (* 2 spent))))))))})
          (runner-choice [choices]
            {:player :runner
             :prompt "How many credits do you want to spend?"
             :waiting-prompt true
             :choices choices
             :async true
             :effect (effect (continue-ability :corp (corp-choice (str->int target)) card nil))})]
    {:on-play
     {:async true
      :effect (req (let [all-amounts (range (inc (get-in @state [:runner :credit])))
                         valid-amounts (remove #(or (any-flag-fn? state :corp :prevent-secretly-spend %)
                                                    (any-flag-fn? state :runner :prevent-secretly-spend %))
                                               all-amounts)
                         choices (map str valid-amounts)]
                     (continue-ability state side (runner-choice choices) card nil)))}}))

(defcard "Pushing the Envelope"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :change-in-game-state (req (seq runnable-servers))
             :choices (req runnable-servers)
             :msg (msg (if (<= (count (:hand runner)) 2)
                         "make a run, and give +2 strength to installed icebreakers"
                         "make a run"))
             :async true
             :effect (req (when (<= (count (:hand runner)) 2)
                            (pump-all-icebreakers state side 2 :end-of-run))
                          (make-run state side eid target))}})

(defcard "Quality Time"
  {:on-play
   {:msg "draw 5 cards"
    :change-in-game-state (req (seq (:deck runner)))
    :async true
    :effect (effect (draw eid 5))}})

(defcard "Queen's Gambit"
  {:on-play
   {:choices ["0" "1" "2" "3"]
    :prompt "How many advancement tokens do you want to place?"
    :async true
    :effect (effect
              (continue-ability
                (let [c (str->int target)]
                  {:choices {:card #(and (is-remote? (second (get-zone %)))
                                         (= (last (get-zone %)) :content)
                                         (not (:rezzed %)))}
                   :msg (msg "place " (quantify c "advancement token") " on " (card-str state target) " and gain " (* 2 c) " [Credits]")
                   :async true
                   :effect (req (wait-for (gain-credits state side (* 2 c))
                                          (add-prop state :corp target :advance-counter c {:placed true})
                                          (register-turn-flag!
                                            state side
                                            card :can-access
                                            ;; prevent access of advanced card
                                            (fn [_ _ card] (not (same-card? target card))))
                                          (effect-completed state side eid)))})
                card nil))}})

(defcard "Quest Completed"
  {:on-play
   {:req (req (and (some #{:hq} (:successful-run runner-reg))
                   (some #{:rd} (:successful-run runner-reg))
                   (some #{:archives} (:successful-run runner-reg))))
    :change-in-game-state (req (some (complement ice?) (all-installed state :corp)))
    :choices {:card installed?}
    :msg (msg "access " (:title target))
    :async true
    :effect (effect (access-card eid target))}})

(defcard "Raindrops Cut Stone"
  {:makes-run true
   :on-play {:async true
             :change-in-game-state (req (seq runnable-servers))
             :prompt "Choose a server"
             :choices (req runnable-servers)
             :effect (effect (make-run eid target card))}
   :events [{:event :subroutine-fired
             :req (req (some #(= % :play-area) (:zone card)))
             :effect (effect (add-counter (get-card state card) :power 1))}
            {:event :run-ends
             :async true
             :req (req this-card-run)
             :effect (req (let [cards-to-draw (get-counters (get-card state card) :power)]
                            (continue-ability
                              state side
                              {:msg (msg (if (pos? cards-to-draw)
                                           (str "draw " (quantify cards-to-draw "card") " and gain 3 [Credits]")
                                           "gain 3 [Credits]"))
                               :async true
                               :effect (req (if (pos? cards-to-draw)
                                              (wait-for (draw state side cards-to-draw)
                                                        (gain-credits state side eid 3))
                                              (gain-credits state side eid 3)))}
                              card nil)))}]})

(defcard "Rebirth"
  {:on-play
   {:prompt "Choose an identity"
    :rfg-instead-of-trashing true
    :choices (req (let [is-draft-id? #(.startsWith (:code %) "00")
                        runner-identity (:identity runner)
                        format (:format @state)
                        is-swappable #(and (= "Identity" (:type %))
                                           (= "Runner" (:side %))
                                           (= (:faction runner-identity) (:faction %))
                                           (not (is-draft-id? %))
                                           (not= (:title runner-identity) (:title %))
                                           (or (= :casual format)
                                               (legal? format :legal %)))
                        swappable-ids (filter is-swappable (server-cards))]
                    (sort-by :title swappable-ids)))
    :msg "change identities"
    :effect (req (let [old-runner-identity (:identity runner)]
                   ;; Handle hosted cards (Ayla) - Part 1
                   (doseq [c (:hosted old-runner-identity)]
                     (move state side c :temp-hosted))
                   (disable-identity state side)
                   ;; Move the selected ID to [:runner :identity] and set the zone
                   (let [new-id (-> target :title server-card make-card (assoc :zone [(->c :identity)]))
                         num-old-blanks (:num-disabled old-runner-identity)]
                     (swap! state assoc-in [side :identity] new-id)
                     (card-init state side new-id)
                     (when num-old-blanks
                       (dotimes [_ num-old-blanks]
                         (disable-identity state side)))))
                 ;; Handle hosted cards (Ayla) - Part 2
                 (doseq [c (get-in @state [:runner :temp-hosted])]
                   ;; Currently assumes all hosted cards are hosted facedown (Ayla)
                   (host state side (get-in @state [:runner :identity]) c {:facedown true})))}})

(defcard "Reboot"
  (letfn [(install-cards [state side eid card to-install titles]
            (if-let [f (first to-install)]
              (wait-for (runner-install state :runner f {:facedown true :no-msg true})
                        (install-cards state side eid card (rest to-install) titles))
              (do
                (move state side (find-latest state card) :rfg)
                (system-msg state :runner (str "uses " (:title card) " to install " (enumerate-str titles) " facedown"))
                (effect-completed state side eid))))]
    {:makes-run true
     :on-play {:async true
               :change-in-game-state (req archives-runnable)
               :rfg-instead-of-trashing true
               :effect (req (make-run state side eid :archives card))}
     :events [(successful-run-replace-breach
                {:target-server :archives
                 :this-card-run true
                 :mandatory true
                 :ability
                 {:req (req (not (zone-locked? state :runner :discard)))
                  :async true
                  :prompt "Choose up to 5 cards to install"
                  :show-discard true
                  :choices {:max 5
                            :card #(and (in-discard? %)
                                        (runner? %))}
                  :effect (effect (install-cards eid card targets (map :title targets)))}})]}))

(defcard "Recon"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :change-in-game-state (req (seq runnable-servers))
             :choices (req runnable-servers)
             :async true
             :effect (effect (make-run eid target card))}
   :events [{:event :encounter-ice
             :optional (:optional (offer-jack-out
                                   {:req (req (first-run-event? state side :encounter-ice))}))}]})

(defcard "Rejig"
  (let [valid-target? (fn [card] (and (runner? card)
                                      (or (program? card)
                                          (hardware? card))))
        pick-up {:async true
                 :prompt "Choose a program or piece of hardware to add to the grip"
                 :choices {:card #(and (valid-target? %)
                                       (installed? %))}
                 :effect (req (move state side target :hand)
                              (effect-completed state side (make-result eid (:cost target))))}
        put-down (fn [bonus]
                   {:async true
                    :prompt "Choose a program or piece of hardware to install"
                    :choices
                    {:req (req (and (valid-target? target)
                                    (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                              [(->c :credit (install-cost state side target
                                                                     {:cost-bonus (- bonus)}))])))}
                    :effect (effect (runner-install (assoc eid :source card :source-type :runner-install)
                                                    target {:cost-bonus (- bonus)}))})]
    {:on-play
     {:req (req (some valid-target? (all-installed state :runner)))
      :effect (req (wait-for (resolve-ability state side pick-up card nil)
                             (continue-ability state side
                                               (put-down async-result)
                                               card nil)))}}))

(defcard "Reprise"
  (letfn [(opt-run []
            {:optional
             {:prompt "Run a server?"
              :yes-ability
              {:prompt "Choose a server"
               :choices (req runnable-servers)
               :async true
               :msg (msg "make a run on " target)
               :effect (effect (make-run eid target card))}
               :no-ability {:effect (effect (system-msg (str "declines to use " (:title card) " to make a run")))}}})]
    {:makes-run true
     :on-play
     {:async true
      :req (req (:stole-agenda runner-reg))
      :prompt "Choose an installed Corp card to add to HQ"
      :waiting-prompt true
      :choices {:card #(and (installed? %)
                            (corp? %))}
      :msg (msg "add " (card-str state target) " to HQ")
      :cancel-effect (effect (continue-ability (opt-run) card nil))
      :effect (effect (move :corp target :hand)
                      (continue-ability (opt-run) card nil))}}))

(defcard "Reshape"
  {:on-play
   {:prompt "Choose 2 unrezzed pieces of ice to swap positions"
    :choices {:card #(and (installed? %)
                          (not (rezzed? %))
                          (ice? %))
              :max 2
              :all true}
    :msg (msg "swap the positions of " (card-str state (first targets))
              " and " (card-str state (second targets)))
    :effect (effect (swap-ice (first targets) (second targets)))}})

(defcard "Retrieval Run"
  {:makes-run true
   :on-play {:async true
             :change-in-game-state (req archives-runnable)
             :effect (req (make-run state side eid :archives card))}
   :events [(successful-run-replace-breach
              {:target-server :archives
               :this-card-run true
               :ability
               {:async true
                :req (req (not (zone-locked? state :runner :discard)))
                :prompt "Choose a program to install"
                :msg (msg "install " (:title target))
                :choices (req (filter program? (:discard runner)))
                :effect (effect (runner-install (assoc eid :source card :source-type :runner-install)
                                                target {:ignore-all-cost true}))}})]})

(defcard "Rigged Results"
  (letfn [(choose-ice []
            {:player :runner
             :waiting-prompt true
             :prompt "Choose a piece of ice to bypass"
             :choices {:card ice?}
             :msg (msg "make a run and bypass " (card-str state target))
             :async true
             :effect (effect (register-events
                               card
                               (let [target-ice target]
                                 [{:event :encounter-ice
                                   :req (req (same-card? target-ice (:ice context)))
                                   :msg (msg "bypass " (:title (:ice context)))
                                   :effect (req (bypass-ice state))}]))
                             (make-run eid (second (get-zone target)) card))})
          (corp-choice [choices spent]
            {:player :corp
             :waiting-prompt true
             :prompt "How many credits were spent?"
             :choices choices
             :async true
             :effect (req (wait-for
                            (lose-credits state :runner (make-eid state eid) spent)
                            (system-msg state :runner (str "spends " spent " [Credit]"))
                            (system-msg state :corp (str " guesses " target " [Credit]"))
                            (wait-for (trigger-event-simult state side :reveal-spent-credits nil nil spent)
                                      (if (not= spent (str->int target))
                                        (continue-ability state :runner (choose-ice) card nil)
                                        (effect-completed state side eid)))))})
          (runner-choice [choices]
            {:player :runner
             :waiting-prompt true
             :prompt "How many credits do you want to spend?"
             :choices choices
             :async true
             :effect (effect (continue-ability (corp-choice choices (str->int target)) card nil))})]
    {:on-play
     {:async true
      :effect (req (let [all-amounts (range (min 3 (inc (get-in @state [:runner :credit]))))
                         valid-amounts (remove #(or (any-flag-fn? state :corp :prevent-secretly-spend %)
                                                    (any-flag-fn? state :runner :prevent-secretly-spend %))
                                               all-amounts)
                         choices (map str valid-amounts)]
                     (continue-ability state side (runner-choice choices) card nil)))}}))

(defcard "Rigging Up"
  {:on-play
   {:prompt "Choose a program or piece of hardware to install"
    :choices {:req (req (and (or (hardware? target)
                                 (program? target))
                             (in-hand? target)
                             (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                       [(->c :credit (install-cost state side target {:cost-bonus -3}))])))}
    :change-in-game-state (req (seq (:hand runner)))
    :async true
    :effect (req (wait-for (runner-install state side (make-eid state {:source card :source-type :runner-install}) target {:cost-bonus -3})
                           (let [rig-target async-result]
                             (continue-ability
                               state side
                               {:optional
                                {:prompt (str "Charge " (:title rig-target) "?")
                                 :req (req (can-charge state side rig-target))
                                 :yes-ability
                                 {:async true
                                  :effect (effect (charge-card eid rig-target))
                                  :msg (msg "charge " (:title rig-target))}}}
                               card nil))))}})

(defcard "Rip Deal"
  (let [add-cards-from-heap
        {:optional
         {:prompt "Add cards from heap to grip?"
          :waiting-prompt true
          :req (req (and run
                         (pos? (count (:hand corp)))
                         (pos? (count (:discard runner)))
                         (not (zone-locked? state :runner :discard))))
          :yes-ability
          {:async true
           :effect (req (let [random-access-limit (:random-access-limit (num-cards-to-access state side :hq nil))
                              cards-to-move (min (count (:hand corp))
                                                 random-access-limit
                                                 (count (:discard runner)))]
                          (continue-ability
                           state side
                           {:async true
                            :show-discard true
                            :prompt (str "Choose " (quantify cards-to-move "card")
                                         " to add from the heap to the grip")
                            :msg (msg "add " (enumerate-str (map :title targets))
                                      " from the heap to the grip")
                            :choices {:max cards-to-move
                                      :all true
                                      :card #(and (runner? %)
                                                  (in-discard? %))}
                            :effect (req (doseq [c targets]
                                           (move state side c :hand))
                                         (swap! state assoc-in [:run :prevent-hand-access] true)
                                         (effect-completed state side eid))}
                           card nil)))}}}]
    {:makes-run true
     :on-play {:async true
               :rfg-instead-of-trashing true
               :change-in-game-state (req hq-runnable)
               :effect (req (make-run state side eid :hq card))}
     :events [{:event :successful-run
               :silent (req true)
               :req (req (and (= :hq (target-server context))
                              this-card-run))
               :effect (effect (register-events card
                                [{:event :candidates-determined
                                  :duration :end-of-run
                                  :async true
                                  :req (req (= :hq context))
                                  :effect (effect (continue-ability add-cards-from-heap card nil))}]))}]}))

(defcard "Rumor Mill"
  (letfn [(eligible? [card] (and (:uniqueness card)
                                 (or (asset? card)
                                     (upgrade? card))
                                 (not (has-subtype? card "Region"))))]
    {:static-abilities [{:type :disable-card
                         :req (req (eligible? target))
                         :value true}]}))

(defcard "Run Amok"
  (letfn [(get-rezzed-cids [ice]
            (map :cid (filter #(and (rezzed? %)
                                    (ice? %))
                              ice)))]
    {:makes-run true
     :on-play {:prompt "Choose a server"
               :change-in-game-state (req (seq runnable-servers))
               :choices (req runnable-servers)
               :async true
               :effect (effect (update! (assoc-in card [:special :run-amok] (get-rezzed-cids (all-installed state :corp))))
                         (make-run eid target (get-card state card)))}
     :events [{:event :run-ends
               :req (req this-card-run)
               :async true
               :effect (req (let [new (set (get-rezzed-cids (all-installed state :corp)))
                                  old (set (get-in (get-card state card) [:special :run-amok]))
                                  diff-cid (seq (set/difference new old))
                                  diff (map #(find-cid % (all-installed state :corp)) diff-cid)]
                              (continue-ability
                                state :runner
                                (when (seq diff)
                                  {:async true
                                   :prompt "Choose an ice to trash"
                                   :choices {:card #(some (partial same-card? %) diff)
                                             :all true}
                                   :effect (effect (trash eid target {:cause-card card}))})
                                card nil)))}]}))

(defcard "Running Hot"
  {:on-play
   {:msg "gain [Click][Click][Click]"
    :additional-cost [(->c :brain 1)]
    :async true
    :effect (effect (gain-clicks 3)
                    (effect-completed eid))}})

(defcard "Running Interference"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :change-in-game-state (req (seq runnable-servers))
             :choices (req runnable-servers)
             :async true
             :effect (effect (register-lingering-effect
                               card
                               {:type :rez-additional-cost
                                :duration :end-of-run
                                :req (req (ice? target))
                                :value (req [(->c :credit (:cost target))])})
                             (make-run eid target card))}})

(defcard "S-Dobrado"
  {:makes-run true
   :on-play {:prompt "Choose a central server"
             :choices (req (->> runnable-servers
                                (map unknown->kw)
                                (filter is-central?)
                                (map central->name)))
             :change-in-game-state (req (seq (->> runnable-servers
                                            (map unknown->kw)
                                            (filter is-central?)
                                            (map central->name))))
             :async true
             :effect (effect (make-run eid target card))}
   :events [{:event :encounter-ice
             :req (req (first-run-event? state side :encounter-ice))
             :once :per-run
             :msg (msg "bypass " (card-str state current-ice))
             :effect (req (bypass-ice state))}
            {:event :encounter-ice
             :req (req (and (= 2 (count (run-events state side :encounter-ice)))
                            (threat-level 4 state)))
             :async true
             :effect (effect (continue-ability
                               {:optional {:prompt (msg "Spend [Click] to bypass " 
                                                        (card-str state current-ice)
                                                        "?")
                                           :waiting-prompt true
                                           :yes-ability {:msg (msg "bypass " (card-str state current-ice))
                                                         :cost [(->c :click 1 {:allowed-during-run true})]
                                                         :effect (req (bypass-ice state))}}}
                               card nil))}]})

(defcard "Satellite Uplink"
  {:on-play
   {:choices {:max 2
              :card #(and (corp? %)
                          (installed? %)
                          (not (rezzed? %)))}
    :async true
    :change-in-game-state (req (some (complement faceup?) (all-installed state :corp)))
    :effect (req (if (pos? (count targets))
                   (wait-for (expose state side target)
                             (if (= 2 (count targets))
                               (expose state side eid (second targets))
                               (effect-completed state side eid)))
                   (effect-completed state side eid)))}})

(defcard "Scavenge"
  {:on-play
   {:req (req (some #(and (program? %)
                          (installed? %))
                    (all-active-installed state :runner)))
    :prompt "Choose an installed program to trash"
    :choices {:card #(and (program? %)
                          (installed? %))}
    :async true
    :effect (req (let [trashed target
                       tcost (:cost trashed)]
                   (wait-for
                     (trash state side target {:unpreventable true
                                               :cause-card card})
                     (continue-ability
                       state side
                       {:async true
                        :prompt (if (not (zone-locked? state :runner :discard))
                                  "Choose a program to install from the grip or heap"
                                  "Choose a program to install")
                        :show-discard  (not (zone-locked? state :runner :discard))
                        :choices
                        {:req (req (and (program? target)
                                        (or (in-hand? target)
                                            (and (in-discard? target)
                                                 (not (zone-locked? state :runner :discard))))
                                        (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                                  [(->c :credit (install-cost state side target
                                                                              {:cost-bonus (- tcost)}))])))}
                        :msg (msg "trash " (:title trashed)
                                  " and install " (:title target)
                                  ", lowering the cost by " tcost " [Credits]")
                        :effect (effect (runner-install (assoc eid :source card :source-type :runner-install)
                                                        target {:cost-bonus (- tcost)}))}
                       card nil))))}})

(defcard "Scrubbed"
  {:events [{:event :encounter-ice
             :once :per-turn
             :effect (effect
                       (register-lingering-effect
                         card
                         (let [target-ice (:ice context)]
                           {:type :ice-strength
                            :duration :end-of-run
                            :req (req (same-card? target target-ice))
                            :value -2}))
                       (update-all-ice))}]})

(defcard "Showing Off"
  {:makes-run true
   :on-play {:async true
             :change-in-game-state (req rd-runnable)
             :effect (req (make-run state side eid :rd card))}
   :events [{:event :successful-run
             :req (req (and (= :rd (target-server context))
                            this-card-run))
             :silent (req true)
             :msg "access cards from the bottom of R&D"
             :effect (req (swap! state assoc-in [:runner :rd-access-fn] reverse))}
            {:event :run-ends
             :effect (req (swap! state assoc-in [:runner :rd-access-fn] seq))}]})

(defcard "Singularity"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :choices (req (filter #(can-run-server? state %) remotes))
             :change-in-game-state (req (some #(can-run-server? state %) remotes))
             :async true
             :effect (effect (make-run eid target card))}
   :events [(successful-run-replace-breach
              {:target-server :remote
               :this-card-run true
               :mandatory true
               :ability
               {:async true
                :msg "trash all cards in the server at no cost"
                :effect (effect (trash-cards eid (:content run-server) {:cause-card card}))}})]})

(defcard "Social Engineering"
  {:on-play
   {:prompt "Choose an unrezzed piece of ice"
    :choices {:card #(and (not (rezzed? %))
                          (installed? %)
                          (ice? %))}
    :change-in-game-state (req (some (every-pred ice? (complement rezzed?)) (all-installed state :corp)))
    :msg (msg "select " (card-str state target))
    :effect (effect
              (register-events
                card
                (let [ice target]
                  [{:event :rez
                    :duration :end-of-turn
                    :req (req (same-card? (:card context) ice))
                    :msg (msg "gain " (rez-cost state side (get-card state (:card context))) " [Credits]")
                    :async true
                    :effect (effect (gain-credits :runner eid (rez-cost state side (get-card state (:card context)))))}])))}})

(defcard "Spark of Inspiration"
  (letfn [(install-program [state side eid card revealed-card rev-str]
            (if (can-pay? state side (assoc eid :source card :source-type :runner-install)
                          revealed-card nil
                          [(->c :credit (install-cost state side revealed-card {:cost-bonus -10}))])
              (continue-ability
                state side
                {:optional
                 {:prompt (str "Install " (:title revealed-card) " paying 10 [Credits] less?")
                  :waiting-prompt true
                  :yes-ability
                  {:msg (msg "reveal " rev-str " from the top of the stack and install "
                             (:title revealed-card) ", paying 10 [Credits] less")
                   :async true
                   :effect (req (wait-for (runner-install
                                            state side
                                            (make-eid state {:source card :source-type :runner-install})
                                            revealed-card {:cost-bonus -10})
                                          (shuffle! state side :deck)
                                          (system-msg state side "shuffles the Stack")
                                          (effect-completed state side eid)))}
                  :no-ability
                  {:msg (msg "reveal " rev-str " from the top of the stack")
                   :effect (effect (shuffle! :deck)
                                   (system-msg "shuffles the Stack"))}}}
                card nil)
              (continue-ability ;;can't afford to install it somehow
                state side
                {:msg (msg "reveal " rev-str " from the top of the stack")
                 :effect (effect (shuffle! :deck)
                                 (system-msg "shuffles the Stack"))}
                card nil)))
          (spark-search-fn [state side eid card remainder rev-str]
            (if (not-empty remainder)
              (let [revealed-card (first remainder)
                    rest-of-deck (rest remainder)
                    rev-str (if (= "" rev-str)
                              (:title revealed-card)
                              (str rev-str ", " (:title revealed-card)))]
                (if (program? revealed-card)
                  (install-program state side eid card revealed-card rev-str)
                  (spark-search-fn state side eid card rest-of-deck rev-str)))
              (continue-ability
                state side
                {:msg (msg "reveal " rev-str " from the top of the stack")
                 :effect (effect (shuffle! :deck)
                                 (system-msg "shuffles the Stack"))}
                card nil)))]
    {:on-play {:async true
               :change-in-game-state (req (seq (:deck runner)))
               :effect (effect (spark-search-fn eid card (:deck runner) ""))}}))

(defcard "Spear Phishing"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :change-in-game-state (req (seq runnable-servers))
             :choices (req runnable-servers)
             :async true
             :effect (effect (make-run eid target card))}
   :events [{:event :encounter-ice
             :req (req (= 1 run-position))
             :msg (msg "bypass " (:title (:ice context)))
             :effect (req (bypass-ice state))}]})

(defcard "Spec Work"
  {:on-play
   {:additional-cost [(->c :program 1)]
    :msg "gain 4 [Credits] and draw 2 cards"
    :async true
    :effect (req (wait-for (gain-credits state side 4)
                           (draw state side eid 2)))}})

(defcard "Special Order"
  {:on-play
   {:prompt "Choose an Icebreaker"
    :change-in-game-state (req (seq (:deck runner)))
    :choices (req (cancellable (filter #(has-subtype? % "Icebreaker") (:deck runner)) :sorted))
    :msg (msg "add " (:title target) " from the stack to the grip and shuffle the stack")
    :effect (effect (trigger-event :searched-stack)
                    (shuffle! :deck)
                    (move target :hand))}})

(defcard "Spooned"
  (cutlery "Code Gate"))

(defcard "Spot the Prey"
  {:makes-run true
   :on-play
   {:prompt "Choose 1 non-ice card to expose"
    :msg "expose 1 card and make a run"
    :choices {:card #(and (installed? %)
                          (not (ice? %))
                          (corp? %))}
    :async true
    :effect (req (wait-for (expose state side target)
                           (continue-ability
                             state side
                             {:prompt "Choose a server"
                              :choices (req runnable-servers)
                              :async true
                              :effect (effect (make-run eid target))}
                             card nil)))}})

(defcard "Spree"
  {:data {:counter {:power 3}}
   :makes-run true
   :on-play {:prompt "Choose a server"
             :change-in-game-state (req (seq runnable-servers))
             :choices (req runnable-servers)
             :async true
             :effect (effect (make-run eid target card))}
   :abilities [{:cost [(->c :power 1)]
                :label "Host an installed trojan on a piece of ice protecting this server"
                :prompt "Choose an installed trojan"
                :waiting-prompt true
                :choices {:card #(and (has-subtype? % "Trojan")
                                      (program? %)
                                      (installed? %))}
                :async true
                :effect (req (let [trojan target]
                               (continue-ability
                                 state side
                                 {:prompt "Choose a piece of ice protecting this server"
                                  :choices {:card #(and (ice? %)
                                                        (= (first (:server run)) (second (get-zone %))))}
                                  :msg (msg "host " (:title trojan) " on " (card-str state target))
                                  :effect (req (host state side target trojan)
                                               (update-all-ice state side))}
                                 card nil)))}]})

(defcard "Steelskin Scarring"
  {:on-play {:async true
             :msg "draw 3 cards"
             :change-in-game-state (req (seq (:deck runner)))
             :effect (effect (draw eid 3))}
   :on-trash {:when-inactive true
              :interactive (req true)
              :async true
              :req (req (let [zone (first (:zone (:card context)))]
                          (#{:hand :deck} zone)))
              :effect (effect (continue-ability
                                {:optional {:prompt "Draw 2 cards?"
                                            :waiting-prompt true
                                            :yes-ability {:msg "draw 2 cards"
                                                          :async true
                                                          :effect (effect (draw :runner eid 2))}
                                            :no-ability
                                            {:effect (effect (system-msg (str "declines to use " (:title card))))}}}
                                card nil))}})

(defcard "Stimhack"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :change-in-game-state (req (seq runnable-servers))
             :choices (req runnable-servers)
             :async true
             :effect (effect (gain-next-run-credits 9)
                             (make-run eid target card))}
   :events [{:event :run-ends
             :req (req this-card-run)
             :msg "take 1 core damage"
             :effect (effect (damage eid :brain 1 {:unpreventable true
                                                   :card card}))}]})

(defcard "Strike Fund"
  {:on-play {:async true
             :msg "gain 4 [Credits]"
             :effect (req (gain-credits state :runner nil 4)
                          (effect-completed state side eid))}
   :on-trash {:when-inactive true
              :interactive (req true)
              :async true
              :req (req (let [zone (first (:zone (:card context)))]
                          (#{:hand :deck} zone)))
              :effect (effect (continue-ability
                                {:optional {:prompt "Gain 2 [Credits]?"
                                            :waiting-prompt true
                                            :yes-ability {:msg "gain 2 [Credits]"
                                                          :async true
                                                          :effect (effect (gain-credits :runner eid 2))}
                                            :no-ability
                                            {:effect (effect (system-msg (str "declines to use " (:title card))))}}}
                                card nil))}})

(defcard "Sure Gamble"
  {:on-play
   {:msg "gain 9 [Credits]"
    :async true
    :effect (effect (gain-credits eid 9))}})

(defcard "Surge"
  (letfn [(placed-virus-cards [state]
            (->> (turn-events state :runner :counter-added)
                 (filter #(= :virus (:counter-type (second %))))
                 (map first)
                 (keep #(get-card state %))
                 (seq)))]
    {:on-play
     {:req (req (placed-virus-cards state))
      :choices {:req (req (some #(same-card? % target) (placed-virus-cards state)))}
      :msg (msg "place 2 virus tokens on " (:title target))
      :effect (effect (add-counter :runner target :virus 2))}}))

(defcard "SYN Attack"
  {:on-play
   {:player :corp
    :waiting-prompt true
    :prompt "Choose one"
    :choices (req [(when (<= 2 (count (:hand corp)))
                     "Discard 2 cards from HQ")
                   "Draw 4 cards"])
    :async true
    :msg (msg "force the Corp to " (decapitalize target))
    :effect (req (if (= target "Draw 4 cards")
                   (wait-for (draw state :corp 4)
                             (effect-completed state side eid))
                   (continue-ability
                     state :corp
                     {:prompt "Choose 2 cards to discard"
                      :choices {:max 2
                                :all true
                                :card #(and (in-hand? %)
                                            (corp? %))}
                      :async true
                      :effect (effect (trash-cards :corp eid targets {:unpreventable true
                                                                      :cause-card card
                                                                      :cause :forced-to-trash}))}
                     card nil)))}})

(defcard "System Outage"
  {:events [{:event :corp-draw
             :req (req (not (first-event? state side :corp-draw)))
             :msg "force the Corp to lose 1 [Credits]"
             :async true
             :effect (effect (lose-credits :corp eid 1))}]})

(defcard "System Seizure"
  (let [ability {:req (req (get-in card [:special :ss-target]))
                 :effect (effect (update! (dissoc-in card [:special :ss-target])))}]
    {:events [{:event :pump-breaker
               :req (req (or (not (get-in card [:special :ss-target]))
                             (same-card? (:card context) (get-in card [:special :ss-target]))))
               :effect (req (when-not (get-in card [:special :ss-target])
                              (update! state side (assoc-in card [:special :ss-target] (:card context))))
                            (let [new-pump (assoc (:effect context) :duration :end-of-run)]
                              (swap! state assoc :effects
                                     (->> (:effects @state)
                                          (remove #(= (:uuid %) (:uuid new-pump)))
                                          (#(conj % new-pump))
                                          (into []))))
                            (update-breaker-strength state side (:card context)))}
              (assoc ability :event :corp-turn-ends)
              (assoc ability :event :runner-turn-ends)]}))

(defcard "Test Run"
  {:on-play
   {:prompt (req (if (not (zone-locked? state :runner :discard))
                   "Install a program from the stack or heap?"
                   "Install a program from the stack?"))
    :choices (req ["Stack"
                   (when (not (zone-locked? state :runner :discard)) "Heap")])
    :msg (msg "install a program from the " target)
    :async true
    :effect (effect
              (continue-ability
                (let [where target]
                  {:prompt "Choose a program to install"
                   :choices (req (cancellable
                                   (filter program? ((if (= where "Heap") :discard :deck) runner))))
                   :async true
                   :effect (req (when (= where "Stack")
                                  (trigger-event state side :searched-stack)
                                  (shuffle! state side :deck))
                                (wait-for (runner-install state side (make-eid state {:source card :source-type :runner-install})
                                                          target {:ignore-all-cost true})
                                          (if async-result
                                            (let [installed-card (update! state side (assoc-in async-result [:special :test-run] true))]
                                              (register-events
                                                state side installed-card
                                                [{:event :runner-turn-ends
                                                  :duration :end-of-turn
                                                  :req (req (get-in (find-latest state installed-card) [:special :test-run]))
                                                  :msg (msg "move " (:title installed-card) " to the top of the stack")
                                                  :effect (effect (move (find-latest state installed-card) :deck {:front true}))}])
                                              (effect-completed state side eid))
                                            (effect-completed state side eid))))})
                card nil))}})

(defcard "The Maker's Eye"
  {:makes-run true
   :on-play {:async true
             :change-in-game-state (req rd-runnable)
             :effect (req (make-run state side eid :rd card))}
   :events [{:event :successful-run
             :silent (req true)
             :req (req (and (= :rd (target-server context))
                            this-card-run))
             :effect (effect (register-events
                              card [(breach-access-bonus :rd 2 {:duration :end-of-run})]))}]})

(defcard "The Noble Path"
  {:makes-run true
   :on-play {:async true
             :change-in-game-state (req (or (seq (:hand runner))
                                      (seq runnable-servers)))
             :effect (req (wait-for
                            (trash-cards state side (:hand runner) {:cause-card card})
                            (continue-ability
                              state side
                              {:async true
                               :prompt "Choose a server"
                               :choices (req runnable-servers)
                               :msg (msg "trash [their] grip and make a run on " target
                                         ", preventing all damage")
                               :effect (effect (make-run eid target card))}
                              card nil)))}
   :events [{:event :pre-damage
             :duration :end-of-run
             :effect (effect (damage-prevent :net Integer/MAX_VALUE)
                             (damage-prevent :meat Integer/MAX_VALUE)
                             (damage-prevent :brain Integer/MAX_VALUE))}]})

(defcard "The Price"
  {:on-play {:async true
             :change-in-game-state (req (seq (:deck runner)))
             :effect
             (req
               (wait-for (mill state :runner (make-eid state eid) :runner 4)
                         (let [trashed-cards async-result]
                           (system-msg state side
                                       (str "uses " (:title card) " to trash "
                                            (enumerate-str (map :title trashed-cards))
                                            " from the top of the stack"))
                           (continue-ability
                             state side
                             {:prompt "Choose a card to install"
                              :waiting-prompt true
                              :async true
                              :req (req (not (zone-locked? state :runner :discard)))
                              :choices (req (cancellable (filter #(and (not (event? %))
                                                                       (runner-can-install? state side % nil)
                                                                       (can-pay? state side (assoc eid :source card :source-type :runner-install)
                                                                                 % nil [(->c :credit (install-cost state side % {:cost-bonus -3}))])
                                                                       (in-discard? (get-card state %))) trashed-cards)))
                              :msg (msg  "install " (:title target) ", paying 3 [Credits] less")
                              :cancel-effect (effect (system-msg (str "declines to use " (:title card) " to install a card"))
                                                     (effect-completed eid))
                              :effect (req (let [card-to-install (first (seq (filter #(and (= (:title target) (:title %)) (in-discard? (get-card state %))) trashed-cards)))]
                                             (runner-install state side (assoc eid :source card :source-type :runner-install) card-to-install {:cost-bonus -3})))}
                             card nil))))}})

(defcard "The Price of Freedom"
  {:on-play {:additional-cost [(->c :connection 1)]
             :rfg-instead-of-trashing true
             :msg (msg "prevent the Corp from advancing cards during [their] next turn")}
   :events [{:event :corp-turn-begins
             :duration :until-runner-turn-begins
             :effect (effect (register-turn-flag!
                               card :can-advance
                               (fn [state side card]
                                 ((constantly false)
                                  (toast state :corp "Cannot advance cards this turn due to The Price of Freedom." "warning")))))}]})

(defcard "Three Steps Ahead"
  {:on-play
   {:effect (effect (register-events
                      card
                      [{:event :runner-turn-ends
                        :duration :end-of-turn
                        :unregister-once-resolved true
                        :msg (msg "gain " (* 2 (count (:successful-run runner-reg))) " [Credits]")
                        :async true
                        :effect (effect (gain-credits eid (* 2 (count (:successful-run runner-reg)))))}]))}})

(defcard "Tinkering"
  {:on-play
   {:prompt "Choose a piece of ice"
    :choices {:card #(and (installed? %)
                          (ice? %))}
    :change-in-game-state (req (some ice? (all-installed state :corp)))
    :msg (msg "make " (card-str state target) " gain Sentry, Code Gate, and Barrier until the end of the turn")
    :effect (req (register-lingering-effect state side card
                 (let [ice target]
                   {:type :gain-subtype
                    :duration :end-of-turn
                    :req (req (same-card? ice target))
                    :value ["Sentry" "Code Gate" "Barrier"]}))
                 (add-icon state side card target "T" (faction-label card))
                 (let [t target]
                   (register-events state side card
                     [{:event :runner-turn-ends
                       :duration :end-of-turn
                       :unregister-once-resolved true
                       :effect (effect (remove-icon card t))}])))}})

(defcard "Trade-In"
  ;; TODO: look at me plz
  (letfn [(trashed-hw [state] (last (get-in @state [:runner :discard])))]
    {:on-play
     {:additional-cost [(->c :hardware 1)]
      :msg (msg (let [{:keys [title cost]} (trashed-hw state)]
                  (str "trash " title " and gain " (quot cost 2) " [Credits]")))
      :async true
      :effect (req (let [{:keys [cost]} (trashed-hw state)]
                     (wait-for (gain-credits state :runner (quot cost 2))
                               (continue-ability
                                 state :runner
                                 {:prompt "Choose a piece of hardware to add to the grip"
                                  :choices (req (filter hardware?
                                                        (:deck runner)))
                                  :msg (msg "add " (:title target) " from the stack to the Grip and shuffle the stack")
                                  :effect (effect (trigger-event :searched-stack)
                                                  (shuffle! :deck)
                                                  (move target :hand))}
                                 card nil))))}}))

(defcard "Traffic Jam"
  {:static-abilities [{:type :advancement-requirement
                       :value (req (->> (:scored corp)
                                        (filter #(= (:title %) (:title target)))
                                        (count)))}]})

(defcard "Tread Lightly"
  {:on-play
   {:prompt "Choose a server"
    :choices (req runnable-servers)
    :change-in-game-state (req (seq runnable-servers))
    :makes-run true
    :async true
    :effect (effect (register-lingering-effect
                      card
                      {:type :rez-additional-cost
                       :duration :end-of-run
                       :req (req (ice? target))
                       :value (req [(->c :credit 3)])})
                    (make-run eid target card))}})

(defcard "Trick Shot"
  {:makes-run true
   :data {:counter {:credit 4}}
   :interactions {:pay-credits {:req (req run)
                                :type :credit}}
   :on-play {:async true
             :change-in-game-state (req rd-runnable)
             :effect (req 
                      (update! state side (assoc-in card [:special :run-eid] eid))
                      (make-run state side eid :rd card))}
   :events [{:event :successful-run
             :unregister-once-resolved true
             :silent (req true)
             :req (req (and (= :rd (target-server context))
                            this-card-run
                            (= (get-in card [:special :run-eid :eid]) (get-in @state [:run :eid :eid]))))
             :msg "place 2 [Credits] on itself and access 1 additional card from R&D"
             :effect (effect
                       (add-counter card :credit 2 {:placed true})
                       (register-events
                         card [(breach-access-bonus :rd 1 {:duration :end-of-run})]))}
            {:event :run-ends
             :unregister-once-resolved true
             :req (req this-card-run)
             :prompt "Choose a remote server to run"
             :choices (req (cancellable
                             (->> runnable-servers
                                  (map unknown->kw)
                                  (filter is-remote?)
                                  (map remote->name))))
             :msg (msg "make a run on " target)
             :async true
             :effect (effect (make-run eid target card))}]})

(defcard "Uninstall"
  {:on-play
   {:async true
    :change-in-game-state (req (some #(and (not (facedown? %))
                                     (or (hardware? %) (program? %)))
                               (all-installed state :runner)))
    :choices {:card #(and (installed? %)
                          (not (facedown? %))
                          (or (hardware? %)
                              (program? %)))}
    :msg (msg "move " (:title target) " to [their] Grip")
    :effect (effect (move target :hand))}})

(defcard "Unscheduled Maintenance"
  {:events [{:event :corp-install
             :req (req (ice? (:card context)))
             :effect (effect (register-turn-flag!
                               card :can-install-ice
                               (fn [state side card]
                                 (if (ice? card)
                                   ((constantly false)
                                    (toast state :corp "Cannot install ice the rest of this turn due to Unscheduled Maintenance"))
                                   true))))}]
   :leave-play (effect (clear-turn-flag! card :can-install-ice))})

(defcard "Vamp"
  {:makes-run true
   :on-play {:async true
             :change-in-game-state (req hq-runnable)
             :effect (req (make-run state side eid :hq card))}
   :events [(successful-run-replace-breach
              {:target-server :hq
               :this-card-run true
               :ability
               {:async true
                :prompt "How many [Credits] do you want to spend?"
                :choices :credit
                :msg (msg "take 1 tag and make the Corp lose " target " [Credits]")
                :effect (req (wait-for (lose-credits state :corp (make-eid state eid) target)
                                       (gain-tags state side eid 1)))}})]})

(defcard "VRcation"
  {:on-play
   {:msg (msg "draw 4 cards"
              (when (pos? (:click runner))
                " and lose [Click]"))
    :change-in-game-state (req (or (seq (:deck runner))
                             (pos? (:click runner))))
    :async true
    :effect (req (when (pos? (:click runner))
                   (lose-clicks state :runner 1))
                 (draw state :runner eid 4))}})

(defcard "Wanton Destruction"
  {:makes-run true
   :on-play {:async true
             :change-in-game-state (req hq-runnable)
             :effect (req (make-run state side eid :hq card))}
   :events [(successful-run-replace-breach
              {:target-server :hq
               :this-card-run true
               :ability
               {:msg (msg "force the Corp to discard " (quantify target "card") " from HQ at random")
                :prompt "How many [Click] do you want to spend?"
                :choices (req (map str (range 0 (inc (:click runner)))))
                :async true
                :effect (req (let [n (str->int target)]
                               (wait-for (pay state :runner (make-eid state eid) card (->c :click n {:allowed-during-run true}))
                                         (system-msg state :runner (:msg async-result))
                                         (trash-cards state :corp eid (take n (shuffle (:hand corp))) {:cause-card card}))))}})]})

(defcard "Watch the World Burn"
  (letfn [(rfg-card-event [burned-card]
            [{:event :pre-access-card
              :duration :end-of-game
              :req (req (same-card? :title burned-card target))
              :msg (msg (str "remove " (:title burned-card) " from the game"))
              :effect (effect (move :corp target :rfg))}])]
    {:makes-run true
     :on-play {:prompt "Choose a server"
               :choices (req (filter #(can-run-server? state %) remotes))
               :async true
               :effect (effect (make-run eid target card))}
     :events [{:event :pre-access-card
               :req (req (and (not (agenda? target))
                              (:successful run)))
               :once :per-run
               :msg (msg "remove " (:title target) " from the game, and watch for other copies of " (:title target) " to burn")
               :effect (effect (move :corp target :rfg)
                         (register-events card (rfg-card-event target)))}]}))

(defcard "White Hat"
  (letfn [(finish-choice [choices]
            (let [choices (filter #(not= "Done" %) choices)]
              (when (not-empty choices)
                {:effect (req (doseq [c choices]
                                (move state :corp c :deck))
                              (shuffle! state :corp :deck))
                 :msg (str "shuffle " (enumerate-str (map :title choices)) " into R&D")})))
          (choose-cards [choices chosen]
            {:prompt (str "Choose a card in HQ to shuffle into R&D (" (- 2 (count chosen)) " remaining)")
             :player :runner
             :choices (concat choices ["Done"])
             :not-distinct true
             :async true
             :effect (req (if (and (empty? chosen)
                                   (not= "Done" target))
                            (continue-ability state side (choose-cards (remove-once #(= % target) choices) (conj chosen target)) card nil)
                            (continue-ability state side (finish-choice (conj chosen target)) card nil)))})]
    {:on-play
     {:trace
      {:base 3
       :req (req (some #{:hq :rd :archives} (:successful-run runner-reg)))
       :unsuccessful
       {:async true
        :msg (msg "reveal " (enumerate-str (map :title (:hand corp))) " from HQ")
        :effect (req (wait-for
                       (reveal state side (:hand corp))
                       (continue-ability state :runner (choose-cards (:hand corp) #{}) card nil)))}}}}))

(defcard "Wildcat Strike"
  {:on-play
   {:player :corp
    :waiting-prompt true
    :prompt "Choose one"
    :choices ["Runner gains 6 [Credits]" "Runner draws 4 cards"]
    :msg (msg (if (= target "Runner gains 6 [Credits]")
                "gain 6 [Credits]"
                "draw 4 cards"))
    :async true
    :effect (req (if (= target "Runner gains 6 [Credits]")
                   (gain-credits state :runner eid 6)
                   (draw state :runner eid 4)))}})

(defcard "Windfall"
  {:on-play
   {:async true
    :change-in-game-state (req (seq (:deck runner)))
    :effect (req (shuffle! state side :deck)
                 (let [topcard (first (:deck (:runner @state)))
                       cost (:cost topcard)]
                   (wait-for (trash state side topcard {:cause-card card})
                             (wait-for (gain-credits state side (if (event? topcard) 0 cost))
                                       (system-msg state side
                                                   (str "shuffles the stack and trashes " (:title topcard)
                                                        (when-not (event? topcard)
                                                          (str " to gain " cost " [Credits]"))))
                                       (effect-completed state side eid)))))}})

(defcard "Window of Opportunity"
  (let [install-abi
        {:async true
         :effect
         (req (let [targets-in-the-grip
                    (filter #(or (hardware? %)
                                 (program? %))
                            (:hand runner))]
                (continue-ability
                  state side
                  (if (seq targets-in-the-grip)
                    {:prompt "Choose 1 program or piece of hardware"
                     :waiting-prompt true
                     :choices (req (cancellable targets-in-the-grip))
                     :async true
                     :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target))
                     :msg (msg "install " (:title target))}
                    ;; else show a fake prompt so the corp can't infer that no legal targets exist
                    {:prompt "You have no programs or pieces of hardware to install"
                     :choices ["OK"]
                     :prompt-type :bogus})
                card nil)))}]
    {:makes-run true
     :events [{:event :run
               :async true
               :unregister-once-resolved true
               :effect
               (req (let [rezzed-targets
                          (filter #(and (ice? %)
                                        (= (first (:server target)) (second (get-zone %))))
                                  (all-active-installed state :corp))]
                      (if (seq rezzed-targets)
                        (continue-ability
                          state side
                          {:prompt "Choose a piece of ice protecting this server to derez"
                           :waiting-prompt true
                           :choices {:req (req (some #{target} rezzed-targets))}
                           :msg (msg "derez " (card-str state target))
                           :async true
                           :effect
                           (req (let [chosen-ice target]
                                  (register-events
                                    state side card
                                    [{:event :run-ends
                                      :duration :end-of-run
                                      :optional
                                      {:player :corp
                                       :waiting-prompt true
                                       :req (req (and (installed? (get-card state chosen-ice))
                                                      (not (rezzed? (get-card state chosen-ice)))))
                                       :prompt (str "Rez " (card-str state chosen-ice) ", ignoring all costs?")
                                       :yes-ability {:async true
                                                     :effect
                                                     (req 
                                                       (system-msg state :corp (str "rezzes " (card-str state chosen-ice) ", ignoring all costs"))
                                                       (rez state :corp eid chosen-ice {:ignore-cost :all-costs}))}}}])
                                  (derez state side target)
                                  (effect-completed state side eid)))}
                          card nil)
                        (effect-completed state side eid))))}]
     :on-play {:async true
               :prompt "Choose a server"
               :choices (req runnable-servers)
               :effect (req (wait-for (resolve-ability state side install-abi card nil)
                                      (make-run state side eid target card)))}}))
