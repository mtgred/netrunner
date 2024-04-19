(ns game.cards.upgrades
  (:require
   [clojure.string :as str]
   [cond-plus.core :refer [cond+]]
   [game.core.access :refer [access-bonus set-only-card-to-access
                             installed-access-trigger
                             steal-cost-bonus]]
   [game.core.bad-publicity :refer [lose-bad-publicity]]
   [game.core.board :refer [all-active-installed all-installed all-installed-corp card->server
                            get-remotes server->zone server-list]]
   [game.core.card :refer [agenda? asset? can-be-advanced?
                           corp-installable-type? corp? get-card get-counters get-zone
                           has-subtype? ice? in-discard? in-hand? installed? operation? program? resource? rezzed?
                           runner? upgrade?]]
   [game.core.cost-fns :refer [install-cost rez-cost]]
   [game.core.costs :refer [total-available-credits]]
   [game.core.damage :refer [damage]]
   [game.core.def-helpers :refer [corp-rez-toast defcard offer-jack-out
                                  reorder-choice get-x-fn]]
   [game.core.drawing :refer [draw]]
   [game.core.effects :refer [register-lingering-effect]]
   [game.core.eid :refer [effect-completed get-ability-targets is-basic-advance-action? make-eid]]
   [game.core.engine :refer [dissoc-req pay register-default-events
                             register-events resolve-ability unregister-events]]
   [game.core.events :refer [first-event? first-run-event? no-event? turn-events]]
   [game.core.expose :refer [expose-prevent]]
   [game.core.finding :refer [find-cid find-latest]]
   [game.core.flags :refer [clear-persistent-flag! is-scored? register-persistent-flag!
                            register-run-flag!]]
   [game.core.gaining :refer [gain-credits lose-clicks lose-credits]]
   [game.core.hand-size :refer [corp-hand-size+]]
   [game.core.ice :refer [all-subs-broken? get-run-ices pump-ice resolve-subroutine!
                          unbroken-subroutines-choice update-all-ice update-all-icebreakers]]
   [game.core.installing :refer [corp-install corp-install-list]]
   [game.core.moving :refer [mill move remove-from-currently-drawing
                             swap-cards swap-ice trash trash-cards]]
   [game.core.optional :refer [get-autoresolve set-autoresolve]]
   [game.core.payment :refer [can-pay? cost-value ->c]]
   [game.core.play-instants :refer [play-instant]]
   [game.core.prompts :refer [cancellable clear-wait-prompt]]
   [game.core.props :refer [add-counter add-prop set-prop]]
   [game.core.purging :refer [purge]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [rez derez]]
   [game.core.runs :refer [end-run force-ice-encounter jack-out redirect-run
                           set-next-phase start-next-phase]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [central->zone from-same-server? in-same-server?
                              is-central? protecting-same-server? same-server?
                              target-server unknown->kw zone->name]]
   [game.core.shuffling :refer [shuffle!]]
   [game.core.tags :refer [gain-tags]]
   [game.core.threat :refer [threat-level]]
   [game.core.to-string :refer [card-str]]
   [game.core.toasts :refer [toast]]
   [game.core.update :refer [update!]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]))

;; Helpers
(defn mobile-sysop-event
  ([] (mobile-sysop-event :corp-turn-ends))
  ([ev] (mobile-sysop-event ev nil))
  ([ev callback]
   {:event ev
    :optional
    {:prompt (msg "Move " (:title card) " to another server?")
     :waiting-prompt true
     :yes-ability
     {:prompt "Choose a server"
      :waiting-prompt true
      :choices (req (server-list state))
      :msg (msg "move itself to " target)
      :async true
      :effect (req (let [c (move state side card
                                 (conj (server->zone state target) :content))]
                     (unregister-events state side card)
                     (register-default-events state side c)
                     (wait-for (resolve-ability state side callback c nil)
                               (effect-completed state side eid))))}}}))

;; Card definitions

(defcard "Adrian Seis"
  {:events [(mobile-sysop-event)
            {:event :successful-run
             :interactive (req true)
             :psi {:req (req this-server)
                   :not-equal {:msg (msg "prevent the Runner from accessing cards other than " (:title card))
                               :effect (effect (set-only-card-to-access card)
                                               (effect-completed eid))}
                   :equal {:msg (msg "prevent the Runner from accessing " (:title card))
                           :effect (effect (register-run-flag!
                                             card :can-access
                                             ;; prevent access of advanced card
                                             (fn [_ _ target] (not (same-card? target card))))
                                           (effect-completed eid))}}}]})

(defcard "Akitaro Watanabe"
  {:static-abilities [{:type :rez-cost
                       :req (req (and (ice? target)
                                      (= (card->server state card) (card->server state target))))
                       :value -2}]})

(defcard "AMAZE Amusements"
  (let [ability
        {:event :run-ends
         :req (req (= (second (get-zone card)) (first (:server context))))
         :async true
         :effect (req (if (:did-steal context)
                        (do (gain-tags state :corp eid 2)
                            (system-msg state :corp (str "uses " (:title card) " to give the Runner 2 tags")))
                        (effect-completed state side eid)))}]
  {:events [ability]
   :on-trash
   {:req (req (and run (= :runner side)))
    :effect (effect (register-events
                      card
                      [(assoc ability
                              :req (req (= (second (:previous-zone card)) (first (:server context))))
                              :duration :end-of-run)]))}}))

(defcard "Amazon Industrial Zone"
  {:events [{:event :corp-install
             :optional
             {:req (req (and (ice? (:card context))
                             (protecting-same-server? card (:card context))
                             (can-pay? state side (assoc eid :source card :source-type :rez)
                                       (:card context) nil
                                       [(->c :credit (rez-cost state side (:card context) {:cost-bonus -3}))])))
              :prompt "Rez ice with rez cost lowered by 3?"
              :yes-ability {:msg (msg "lower the rez cost of " (:title (:card context)) " by 3 [Credits]")
                            :async true
                            :effect (effect (rez eid (:card context) {:cost-bonus -3}))}}}]})

(defcard "Angelique Garza Correa"
  {:expend {:req (req (threat-level 3 state))
            :cost [(->c :credit 1)]
            :msg "do 1 meat damage"
            :effect (effect (damage eid :meat 1 {:card card}))}
   :on-access {:optional
               {:req (req (rezzed? card))
                :waiting-prompt true
                :prompt (msg "Pay 2 [Credits] to use " (:title card) " ability?")
                :no-ability {:effect (effect (system-msg (str "declines to use " (:title card))))}
                :yes-ability {:async true
                              :cost [(->c :credit 2)]
                              :msg "do 2 meat damage"
                              :effect (effect (damage eid :meat 2 {:card card}))}}}})

(defcard "Anoetic Void"
  {:events [{:event :approach-server
             :interactive (req true)
             :optional
             {:prompt "Pay 2 [Credits] and trash 2 cards from HQ to end the run?"
              :req (req (and (can-pay? state side eid card nil [(->c :credit 2) (->c :trash-from-hand 2)])
                             this-server))
              :yes-ability
              {:async true
               :msg "end the run"
               :cost [(->c :credit 2) (->c :trash-from-hand 2)]
               :effect (req (end-run state side eid card))}}}]})

(defcard "Arella Salvatore"
  (let [select-ability
        {:prompt "Choose a card in HQ to install"
         :choices {:card #(and (corp-installable-type? %)
                               (in-hand? %)
                               (corp? %))}
         :async true
         :cancel-effect (req (effect-completed state side eid))
         :effect (req (wait-for (corp-install state :corp target nil {:ignore-all-cost true :display-message false})
                                (let [inst-target (find-latest state target)]
                                  (add-prop state :corp inst-target :advance-counter 1 {:placed true})
                                  (system-msg state :corp
                                              (str "uses " (:title card) " to install and place a counter on "
                                                   (card-str state inst-target) ", ignoring all costs"))
                                  (effect-completed state side eid))))}]
    {:events [{:event :agenda-scored
               :req (req (= (:previous-zone (:card context)) (get-zone card)))
               :interactive (req (some corp-installable-type? (:hand corp)))
               :silent (req (not-any? corp-installable-type? (:hand corp)))
               :async true
               :effect (req (if (some corp-installable-type? (:hand corp))
                              (continue-ability state side select-ability card nil)
                              (effect-completed state side eid)))}]}))

(defcard "Ash 2X3ZB9CY"
  {:events [{:event :successful-run
             :interactive (req true)
             :trace {:base 4
                     :req (req this-server)
                     :successful
                     {:msg "prevent the Runner from accessing cards other than Ash 2X3ZB9CY"
                      :effect (effect (set-only-card-to-access card)
                                      (effect-completed eid))}}}]})

(defcard "Awakening Center"
  {:can-host (req (ice? target))
   :abilities [{:label "Host a piece of Bioroid ice"
                :cost [(->c :click 1)]
                :prompt "Choose a piece of Bioroid ice in HQ to host"
                :choices {:card #(and (ice? %)
                                      (has-subtype? % "Bioroid")
                                      (in-hand? %))}
                :msg "host a piece of Bioroid ice"
                :async true
                :effect (req (corp-install state side eid target card {:ignore-all-cost true}))}]
   :events [{:event :pass-all-ice
             :optional
             {:req (req (and this-server
                             (some #(can-pay? state side (assoc eid :source card :source-type :rez) % nil
                                              [(->c :credit (rez-cost state side % {:cost-bonus -7}))])
                                   (:hosted card))))
              :prompt "Rez and force the Runner to encounter a hosted piece of ice?"
              :waiting-prompt true
              :yes-ability
              {:async true
               :prompt "Choose a hosted piece of Bioroid ice to rez"
               :choices (req (:hosted card))
               :msg (msg "lower the rez cost of " (:title target) " by 7 [Credits] and force the Runner to encounter it")
               :effect (req (wait-for (rez state side target {:cost-bonus -7})
                                      (let [ice (:card async-result)]
                                        (register-events
                                          state side card
                                          [{:event :run-ends
                                            :duration :end-of-run
                                            :async true
                                            :req (req (get-card state ice))
                                            :effect (effect (trash eid (get-card state ice) {:cause-card card}))}])
                                        (force-ice-encounter state side eid ice))))}
              :no-ability
              {:effect (effect (system-msg (str "declines to use " (:title card))))}}}]})

(defcard "Bamboo Dome"
  {:install-req (req (filter #{"R&D"} targets))
   :abilities [{:cost [(->c :click 1)]
                :req (req (pos? (count (:deck corp))))
                :async true
                :msg (msg (str "reveal " (enumerate-str (map :title (take 3 (:deck corp)))) " from the top of R&D"))
                :label "Add 1 card from top 3 of R&D to HQ"
                :waiting-prompt true
                :effect (req
                          (wait-for
                            (reveal state side (take 3 (:deck corp)))
                            (continue-ability
                              state side
                              {:prompt "Choose a card to add to HQ"
                               :async true
                               :choices (take 3 (:deck corp))
                               :not-distinct true
                               :msg "add 1 of the revealed cards to HQ"
                               :effect (effect (move target :hand)
                                               (continue-ability
                                                 (let [from (take 2 (get-in @state [:corp :deck]))]
                                                   (when (pos? (count from))
                                                     (reorder-choice :corp :runner from '() (count from) from)))
                                                 card nil))}
                              card nil)))}]})

(defcard "Ben Musashi"
  {:on-trash
   {:req (req (and (= :runner side)
                   (:run @state)))
    :effect (effect (register-lingering-effect
                      card
                      {:type :steal-additional-cost
                       :duration :end-of-run
                       :req (req (or (= (get-zone target) (:previous-zone card))
                                     (= (central->zone (get-zone target))
                                        (butlast (:previous-zone card)))))
                       :value (req (->c :net 2))}))}
   :static-abilities [{:type :steal-additional-cost
                       :req (req (or (in-same-server? card target)
                                     (from-same-server? card target)))
                       :value (req (->c :net 2))}]})

(defcard "Bernice Mai"
  {:events [{:event :successful-run
             :interactive (req true)
             :trace {:base 5
                     :req (req this-server)
                     :successful {:msg "give the Runner 1 tag"
                                  :async true
                                  :effect (effect (gain-tags :corp eid 1))}
                     :unsuccessful
                     {:async true
                      :msg "trash itself"
                      :effect (effect (trash eid card {:cause-card card}))}}}]})

(defcard "Bio Vault"
  {:install-req (req (remove #{"HQ" "R&D" "Archives"} targets))
   :advanceable :always
   :abilities [{:label "End the run"
                :req (req (:run @state))
                :msg "end the run"
                :async true
                :cost [(->c :advancement 2) (->c :trash-can)]
                :effect (effect (end-run eid card))}]})

(defcard "Black Level Clearance"
  {:events [{:event :successful-run
             :async true
             :interactive (req true)
             :player :runner
             :req (req this-server)
             :msg (msg "force the Runner to " (decapitalize target))
             :prompt "Choose one"
             :waiting-prompt true
             :choices ["Take 1 core damage" "Jack out"]
             :effect (req (if (= target "Take 1 core damage")
                            (damage state :runner eid :brain 1 {:card card})
                            (wait-for
                              (jack-out state :runner (make-eid state))
                              (wait-for
                                (gain-credits state :corp 5)
                                (wait-for
                                  (draw state :corp 1)
                                  (system-msg state :corp
                                              (str "gains 5 [Credits] and draws 1 card. "
                                                   "Black Level Clearance is trashed"))
                                  (trash state :corp eid card {:cause-card card}))))))}]})

(defcard "Brasília Government Grid"
  {:events [{:event :rez
             :req (req (and (ice? (:card context))
                            this-server run
                            (some #(and (ice? %)
                                        (not (same-card? % (:card context))))
                                  (all-active-installed state :corp))))
             :effect (req
                       (let [rezzed-card (:card context)]
                         (continue-ability
                           state side
                           {:optional
                            {:prompt (str "Derez another piece of ice to give "
                                          (:title rezzed-card)
                                          " +3 strength for the remainder of the run?")
                             :waiting-prompt true
                             :once :per-turn
                             :yes-ability {:choices {:card #(and (ice? %)
                                                                 (rezzed? %)
                                                                 (not (same-card? % rezzed-card)))}
                                           :async true
                                           :msg (msg "derez " (card-str state target) " to give " (card-str state rezzed-card) " +3 strength for the remainder of the run")
                                           :effect (req (derez state side (get-card state target))
                                                        (pump-ice state side rezzed-card 3 :end-of-run)
                                                        (effect-completed state side eid))}}}
                           card nil)))}]})

(defcard "Breaker Bay Grid"
  {:static-abilities [{:type :rez-cost
                       :req (req (in-same-server? card target))
                       :value -5}]})

(defcard "Bryan Stinson"
  {:abilities [{:cost [(->c :click 1)]
                :keep-menu-open :while-clicks-left
                :req (req (and (< (:credit runner) 6)
                               (pos? (count (filter #(and (operation? %)
                                                          (has-subtype? % "Transaction")) (:discard corp))))))
                :label "Play a transaction operation from Archives, ignoring all costs, and remove it from the game"
                :prompt "Choose a transaction operation to play"
                :msg (msg "play " (:title target) " from Archives, ignoring all costs, and removes it from the game")
                :choices (req (cancellable (filter #(and (operation? %)
                                                         (has-subtype? % "Transaction")) (:discard corp)) :sorted))
                :async true
                :effect (effect (play-instant eid (-> target
                                                      (assoc :rfg-instead-of-trashing true)
                                                      (assoc-in [:special :rfg-when-trashed] true))
                                              {:ignore-cost true}))}]})

(defcard "Calibration Testing"
  {:install-req (req (remove #{"HQ" "R&D" "Archives"} targets))
   :abilities [{:label "Place 1 advancement token on a card in this server"
                :async true
                :trash-icon true
                :effect (effect (continue-ability
                                  {:prompt "Choose a card in this server"
                                   :choices {:card #(in-same-server? % card)}
                                   :async true
                                   :msg (msg "place an advancement token on " (card-str state target))
                                   :cost [(->c :trash-can)]
                                   :effect (effect (add-prop target :advance-counter 1 {:placed true}))}
                                  card nil))}]})

(defcard "Caprice Nisei"
  {:events [{:event :pass-all-ice
             :psi {:req (req this-server)
                   :not-equal {:msg "end the run"
                               :effect (effect (end-run eid card))}}}]})

(defcard "Cayambe Grid"
  (let [ability {:interactive (req (->> (all-installed state :corp)
                                        (filter #(and (ice? %)
                                                      (same-server? card %)))
                                        count
                                        pos?))
                 :label "place 1 advancement counter (start of turn)"
                 :async true
                 :effect
                 (effect
                   (continue-ability
                     (when (->> (all-installed state :corp)
                                (filter #(and (ice? %)
                                              (same-server? card %)))
                                count
                                pos?)
                       {:prompt (str "Place 1 advancement counter on an ice protecting " (zone->name (second (get-zone card))))
                        :choices {:card #(and (ice? %)
                                              (same-server? % card))}
                        :msg (msg "place 1 advancement counter on " (card-str state target))
                        :effect (effect (add-prop target :advance-counter 1 {:placed true}))})
                     card nil))}]
    {:events [(assoc ability :event :corp-turn-begins)
              {:event :approach-server
               :interactive (req true)
               :req (req this-server)
               :async true
               :effect
               (effect
                 (continue-ability
                   (let [cost (->> (get-run-ices state)
                                   (filter #(pos? (get-counters % :advancement)))
                                   count
                                   (* 2))]
                     {:async true
                      :player :runner
                      :waiting-prompt true
                      :prompt "Choose one"
                      :choices [(when (can-pay? state :runner eid card nil [(->c :credit cost)])
                                  (str "Pay " cost " [Credits]"))
                                "End the run"]
                      :msg (msg (if (= target "End the run")
                                  (decapitalize target)
                                  (str "force the Runner to " (decapitalize target))))
                      :effect (req (if (= target "End the run")
                                     (end-run state side eid card)
                                     (wait-for (pay state :runner (make-eid state eid) card (->c :credit cost))
                                               (system-msg state :runner (:msg async-result))
                                               (effect-completed state side eid))))})
                   card nil))}]
     :abilities [ability]}))

(defcard "ChiLo City Grid"
  {:events [{:event :successful-trace
             :req (req this-server)
             :async true
             :effect (effect (gain-tags :corp eid 1))
             :msg "give the Runner 1 tag"}]})

(defcard "Code Replicator"
  {:abilities [{:label "Force the runner to approach the passed piece of ice again"
                :req (req (and this-server
                               (< run-position (count (get-run-ices state)))
                               (rezzed? (get-in (:ices (card->server state card)) [(:position run)]))))
                :async true
                :effect (req (wait-for
                               (trash state :corp (make-eid state eid) card {:cause-card card})
                               (swap! state update-in [:run :position] inc)
                               (set-next-phase state :approach-ice)
                               (update-all-ice state side)
                               (update-all-icebreakers state side)
                               (system-msg state :corp (str "trashes " (:title card) " to make the runner approach "
                                                            (:title (get-in (:ices (card->server state card)) [(:position run)]))
                                                            " again"))
                               (wait-for (resolve-ability state :runner (make-eid state eid) (offer-jack-out) card nil)
                                         (if (not (:ended (:end-run @state)))
                                           (start-next-phase state side eid)
                                           (effect-completed state side eid)))))}]})

(defcard "Cold Site Server"
  {:static-abilities [{:type :run-additional-cost
                       :req (req (= (:server (second targets)) (unknown->kw (get-zone card))))
                       :value (req (repeat (get-counters card :power) [(->c :credit 1) (->c :click 1)]))}]
   :events [{:event :corp-turn-begins
             :req (req (pos? (get-counters card :power)))
             :msg "remove all hosted power counters"
             :effect (effect (add-counter card :power (- (get-counters card :power))))}]
   :abilities [{:cost [(->c :click 1)]
                :keep-menu-open :while-clicks-left
                :msg "place 1 power counter on itself"
                :effect (effect (add-counter card :power 1))}]})

(defcard "Corporate Troubleshooter"
  {:abilities [{:async true
                :label "Add strength to a rezzed piece of ice protecting this server"
                :cost [(->c :trash-can) (->c :x-credits)]
                :choices {:all true
                          :req (req (and (ice? target)
                                         (rezzed? target)
                                         (protecting-same-server? card target)))}
                :msg (msg "add " (cost-value eid :x-credits)
                          " strength to " (:title target))
                :effect (effect (pump-ice target (cost-value eid :x-credits) :end-of-turn))}]})

(defcard "Crisium Grid"
  {:static-abilities [{:type :block-successful-run
                       :req (req this-server)
                       :value true}]})

(defcard "Cyberdex Virus Suite"
  {:flags {:rd-reveal (req true)}
   :on-access {:optional
               {:waiting-prompt true
                :prompt "Purge virus counters?"
                :yes-ability {:msg "purge virus counters"
                              :async true
                              :effect (effect (purge eid))}}}
   :abilities [{:label "Purge virus counters"
                :msg "purge virus counters"
                :cost [(->c :trash-can)]
                :async true
                :effect (effect (purge eid))}]})

(defcard "Daniela Jorge Inácio"
  (let [steal-cost {:type :steal-additional-cost
                    :req (req (or (in-same-server? card target)
                                  (from-same-server? card target)))
                    :value (req (->c :add-random-from-hand-to-bottom-of-deck 2))}]
    {:implementation "trash cost not displayed on dialogue"
     :static-abilities [steal-cost]
     :events [{:event :pre-access-card
               :req (req (and (rezzed? card)
                              (same-card? target card)))
               ;; It would be lovely to instead use :trash-cost-bonus [(->c :add-random-from-hand-to-bottom-of-deck 2)]
               :effect
               (req (register-run-flag!
                      state side
                      card :can-trash
                      (fn [state _ card]
                        (or (not (same-card? target card))
                            (can-pay? state :runner eid
                                      card nil [(->c :add-random-from-hand-to-bottom-of-deck 2)])))))}
              steal-cost]
     :on-trash {:async true
                :interactive (req true)
                :req (req (and run (= :runner side)))
                :msg "force the Runner to add 2 random cards from the grip to the bottom of the stack as additional cost to steal agendas from this server or its root"
                :effect
                (req (wait-for (pay state :runner (make-eid state eid) card [(->c :add-random-from-hand-to-bottom-of-deck 2)])
                               (system-msg state :runner (:msg async-result))
                               (register-lingering-effect
                                 state side card
                                 (assoc steal-cost
                                        :req
                                        (req (or (= (:previous-zone card)
                                                    (:zone target))
                                                 ;; special central-servers case
                                                 (= (central->zone (:zone target))
                                                    (butlast (:previous-zone card)))))
                                        :duration :end-of-run))
                               (effect-completed state side eid)))}}))

(defcard "Daruma"
  (let [choose-swap
        (fn [to-swap]
          {:prompt (str "Choose a card to swap with " (:title to-swap))
           :choices {:not-self true
                     :card #(and (corp? %)
                                 (not (or (operation? %)
                                          (ice? %)))
                                 (or (in-hand? %) ; agenda, asset or upgrade from HQ
                                     (installed? %)))} ; card installed in a server
           :cost [(->c :trash-can)]
           :msg (msg "swap " (card-str state to-swap)
                     " with " (card-str state target))
           :effect (effect (swap-cards to-swap target))})
        ability
        {:optional
         {:waiting-prompt true
          :prompt (msg "Trash " (:title card) " to swap a card in this server?")
          :yes-ability
          {:async true
           :prompt "Choose a card in this server to swap"
           :choices {:req (req (and (installed? target)
                                    (in-same-server? card target)))
                     :not-self true}
           :effect (effect (continue-ability (choose-swap target) card nil))}
          :no-ability {:effect (effect (clear-wait-prompt :runner))}}}]
    {:events [{:event :approach-server
               :interactive (req true)
               :req (req this-server)
               :async true
               :effect (req (wait-for (resolve-ability state :corp (make-eid state eid) ability card nil)
                                      (continue-ability state :runner (offer-jack-out) card nil)))}]}))

(defcard "Dedicated Technician Team"
  {:recurring 2
   :interactions {:pay-credits {:req (req (and (= :corp-install (:source-type eid))
                                               (= (second (get-zone card))
                                                  (unknown->kw (:server (get-ability-targets eid))))))
                                :type :recurring}}})

(defcard "Defense Construct"
  {:advanceable :always
   :abilities [{:label "Add cards from Archives to HQ"
                :req (req (and run
                               (= (:server run) [:archives])
                               (pos? (get-counters card :advancement))))
                :cost [(->c :trash-can)]
                :show-discard true
                :choices {:max (req (get-counters card :advancement))
                          :card #(and (corp? %)
                                      (not (:seen %))
                                      (in-discard? %))}
                :msg (msg "add " (quantify (count targets) "facedown card") " in Archives to HQ")
                :effect (req (doseq [c targets]
                               (move state side c :hand)))}]})

(defcard "Disposable HQ"
  (letfn [(dhq [i n]
            {:player :corp
             :req (req (pos? n))
             :prompt "Choose a card in HQ to add to the bottom of R&D"
             :choices {:card #(and (corp? %)
                                   (in-hand? %))}
             :async true
             :msg "add a card to the bottom of R&D"
             :effect (effect (move target :deck)
                             (continue-ability
                               (when (< i n)
                                 (dhq (inc i) n))
                               card nil))})]
    {:flags {:rd-reveal (req true)}
     :on-access {:optional
                 {:waiting-prompt true
                  :prompt "Add cards from HQ to the bottom of R&D?"
                  :yes-ability
                  {:async true
                   :msg "add cards in HQ to the bottom of R&D"
                   :effect (effect (continue-ability
                                     (dhq 1 (count (:hand corp)))
                                     card nil))}}}}))

(defcard "Djupstad Grid"
  {:events [{:event :agenda-scored
             :req (req (= (:previous-zone (:card context)) (get-zone card)))
             :interactive (req true)
             :async true
             :effect (effect (damage eid :brain 1 {:card card}))}]})

(defcard "Drone Screen"
  {:events [{:event :run
             :async true
             :trace {:base 3
                     :req (req (and this-server tagged))
                     :successful
                     {:msg "do 1 meat damage"
                      :async true
                      :effect (effect (damage eid :meat 1 {:card card
                                                           :unpreventable true}))}}}]})

(defcard "Embolus"
  (let [maybe-gain-counter {:once :per-turn
                            :async true
                            :label "Place 1 power counter (start of turn)"
                            :effect (effect
                                      (continue-ability
                                        {:optional
                                         {:prompt (msg "Pay 1 [Credit] to place 1 power counter on " (:title card) "?")
                                          :yes-ability {:effect (effect (add-counter card :power 1))
                                                        :cost [(->c :credit 1)]
                                                        :msg "place 1 power counter on itself"}}}
                                        card nil))}
        etr {:req (req this-server)
             :cost [(->c :power 1)]
             :msg "end the run"
             :effect (effect (end-run eid card))}]
    {:derezzed-events [(assoc corp-rez-toast :event :runner-turn-ends)]
     :events [(assoc maybe-gain-counter :event :corp-turn-begins)
              {:event :successful-run
               :req (req (pos? (get-counters card :power)))
               :msg "remove 1 power counter from itself"
               :effect (effect (add-counter card :power -1))}]
     :abilities [maybe-gain-counter
                 etr]}))

(defcard "Experiential Data"
  {:static-abilities [{:type :ice-strength
                       :req (req (protecting-same-server? card target))
                       :value 1}]})

(defcard "Expo Grid"
  (let [ability {:req (req (some #(and (asset? %)
                                       (rezzed? %))
                                 (get-in corp (get-zone card))))
                 :msg "gain 1 [Credits]"
                 :once :per-turn
                 :label "Gain 1 [Credits] (start of turn)"
                 :async true
                 :effect (effect (gain-credits eid 1))}]
    {:derezzed-events [(assoc corp-rez-toast :event :runner-turn-ends)]
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Forced Connection"
  {:flags {:rd-reveal (req true)}
   :on-access {:interactive (req true)
               :trace {:req (req (not (in-discard? card)))
                       :base 3
                       :successful {:msg "give the Runner 2 tags"
                                    :async true
                                    :effect (effect (gain-tags :corp eid 2))}}}})

(defcard "Fractal Threat Matrix"
  {:events [{:event :subroutines-broken
             :req (req (and (all-subs-broken? target)
                            (protecting-same-server? card target)))
             :msg (msg (let [deck (:deck runner)]
                         (if (pos? (count deck))
                           (str "trash " (enumerate-str (map :title (take 2 deck))) " from the stack")
                           "trash no cards from the stack (it is empty)")))
             :async true
             :effect (effect (mill :corp eid :runner 2))}]})

(defcard "Ganked!"
  {:flags {:rd-reveal (req true)}
   :on-access
   {:optional
    {:req (req (and (not (in-discard? card))
                    (some #(and (ice? %)
                                (protecting-same-server? card %))
                          (all-active-installed state :corp))))
     :waiting-prompt true
     :prompt (msg "Trash " (:title card) " to force the Runner to encounter a piece of ice?")
     :yes-ability
     {:async true
      :choices {:req (req (and (ice? target)
                               (installed? target)
                               (rezzed? target)
                               (protecting-same-server? card target)))}
      :msg (msg "force the Runner to encounter " (card-str state target))
      :effect (req (wait-for (trash state :corp (assoc card :seen true) {:unpreventable true :cause-card card})
                             (force-ice-encounter state side eid target)))}
     :no-ability {:effect (effect (system-msg (str "declines to use " (:title card))))}}}})

(defcard "Georgia Emelyov"
  {:events [{:event :unsuccessful-run
             :req (req (= (target-server target)
                          (second (get-zone card))))
             :async true
             :msg "do 1 net damage"
             :effect (effect (damage eid :net 1 {:card card}))}]
   :abilities [{:cost [(->c :credit 2)]
                :label "Move to another server"
                :async true
                :effect (effect (continue-ability
                                  {:prompt "Choose a server"
                                   :choices (server-list state)
                                   :msg (msg "move to " target)
                                   :effect (req (let [c (move state side card
                                                              (conj (server->zone state target) :content))]
                                                  (unregister-events state side card)
                                                  (register-default-events state side c)))}
                                  card nil))}]})

(defcard "Giordano Memorial Field"
  {:events [{:event :successful-run
             :interactive (req true)
             :async true
             :req (req this-server)
             :effect (effect 
                       (continue-ability
                         (let [credit-cost (* 2 (count (:scored runner)))]
                            {:player :runner
                             :async true
                             :waiting-prompt true
                             :prompt "Choose one"
                             :choices [(when (can-pay? state :runner eid card nil (->c :credit credit-cost))  
                                         (str "Pay " credit-cost " [Credits]"))
                                       "End the run"]
                             :msg (msg (if (= "End the run" target)
                                         (decapitalize target)
                                         (str "force the runner to " (decapitalize target))))
                             :effect (req (if (= "End the run" target)
                                            (end-run state :corp eid card)
                                            (wait-for (pay state :runner (make-eid state eid) card (->c :credit credit-cost))
                                                      (system-msg state :runner (:msg async-result))
                                                      (effect-completed state side eid))))})
                         card nil))}]})

(defcard "Heinlein Grid"
  {:abilities [{:req (req this-server)
                :label "Force the Runner to lose all [Credits] from spending or losing a [Click]"
                :msg (msg "force the Runner to lose all " (:credit runner) " [Credits]")
                :once :per-run
                :async true
                :effect (effect (lose-credits :runner eid :all))}]})

(defcard "Helheim Servers"
  {:abilities [{:label "All ice protecting this server has +2 strength until the end of the run"
                :msg "increase the strength of all ice protecting this server until the end of the run"
                :req (req (and this-server
                               (pos? (count run-ices))
                               (pos? (count (:hand corp)))))
                :async true
                :cost [(->c :trash-from-hand 1)]
                :effect (effect (register-lingering-effect
                                  card
                                  {:type :ice-strength
                                   :duration :end-of-run
                                   :req (req (protecting-same-server? card target))
                                   :value 2})
                                (update-all-ice))
                :keep-menu-open :while-cards-in-hand}]})

(defcard "Henry Phillips"
  (letfn [(hp-gain-credits [state side eid n]
            (if (pos? n)
              (wait-for (gain-credits state :corp 2)
                        (hp-gain-credits state side eid (dec n)))
              (effect-completed state side eid)))]
    {:events [{:event :subroutines-broken
               :req (req (and this-server tagged))
               :msg (msg "gain " (* 2 (count (second targets))) " [Credits]")
               :async true
               :effect (effect (hp-gain-credits :corp eid (count (second targets))))}]}))

(defcard "Hired Help"
  (let [prompt-to-trash-agenda-or-etr
        {:prompt "Choose one"
         :waiting-prompt true
         :player :runner
         :choices ["Trash 1 scored agenda" "End the run"]
         :async true
         :effect (req (if (= target "End the run")
                        (do (system-msg state :runner (str "declines to pay the additional cost from " (:title card)))
                            (end-run state side eid card))
                        (if (seq (:scored runner))
                          (continue-ability state :runner
                                            {:prompt "Choose an Agenda to trash"
                                             :async true
                                             :choices {:max 1
                                                       :card #(is-scored? state side %)}
                                             :effect (req (wait-for (trash state side target {:unpreventable true :cause-card card :cause :forced-to-trash})
                                                                    (system-msg state :runner (str "trashes " (:title target)
                                                                                                   " as an additional cost to initiate a run"))
                                                                    (effect-completed state side eid)))}
                                            card nil)
                          (do (system-msg state :runner (str "cannot pay the additional cost from " (:title card)))
                              (end-run state side eid card)))))}]
    {:events [{:event :run
               :async true
               :req (req (and this-server
                              (empty? (filter #{:hq} (:successful-run runner-reg)))))
               :effect (req (continue-ability state :runner prompt-to-trash-agenda-or-etr card nil))}]}))

(defcard "Hokusai Grid"
  {:events [{:event :successful-run
             :req (req this-server)
             :msg "do 1 net damage"
             :async true
             :effect (effect (damage eid :net 1 {:card card}))}]})

(defcard "Increased Drop Rates"
  {:flags {:rd-reveal (req true)}
   :on-access {:interactive (req true)
               :player :runner
               :async true
               :waiting-prompt true
               :msg (msg (if (= target "The Corp removes 1 bad publicity")
                           "remove 1 bad publicity"
                           (str "force the Runner to " (decapitalize target))))
               :prompt "Choose one"
               :choices ["Take 1 tag" "The Corp removes 1 bad publicity"]
               :effect (req (if (= target "Take 1 tag")
                              (gain-tags state side eid 1 {:unpreventable true})
                              (do (lose-bad-publicity state :corp 1)
                                  (effect-completed state side eid))))}})

(defcard "Intake"
  {:flags {:rd-reveal (req true)}
   :on-access {:interactive (req true)
               :trace {:req (req (not (in-discard? card)))
                       :base 4
                       :label "add an installed program or virtual resource to the Grip"
                       :successful
                       {:waiting-prompt true
                        :prompt "Choose a program or virtual resource"
                        :player :corp
                        :choices {:card #(and (installed? %)
                                              (or (program? %)
                                                  (and (resource? %)
                                                       (has-subtype? % "Virtual"))))}
                        :msg (msg "move " (:title target) " to the Grip")
                        :async true
                        :effect (req (move state :runner target :hand)
                                     (effect-completed state side eid))}}}})

(defcard "Isaac Liberdade"
  (let [ability {:interactive (req true)
                 :req (req (some #(and (ice? %)
                                       (zero? (get-counters % :advancement))
                                       (same-server? card %))
                                 (all-installed-corp state)))
                 :prompt "Choose a piece of ice protecting this server"
                 :waiting-prompt true
                 :choices {:req (req (and (ice? target)
                                          (zero? (get-counters target :advancement))
                                          (same-server? target card)))}
                 :msg (msg "place 1 advancement counter on " (card-str state target))
                 :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]
    {:static-abilities [{:type :ice-strength
                         :req (req (and (ice? target)
                                        (= (card->server state card) (card->server state target))))
                         :value (req (if (pos? (get-counters target :advancement)) 2 0))}]
     :events [(mobile-sysop-event :corp-turn-ends ability)]}))

(defcard "Jinja City Grid"
  (letfn [(install-ice [ice ices grids server]
            (let [remaining (remove-once #(same-card? % ice) ices)]
              {:async true
               :effect (req (if (= "None" server)
                              (continue-ability state side (choose-ice remaining grids) card nil)
                              (wait-for
                                (reveal state side ice)
                                (system-msg state side (str "reveals that they drew " (:title ice)))
                                (wait-for (corp-install state side ice server {:cost-bonus -4})
                                          (remove-from-currently-drawing state side ice)
                                          (continue-ability
                                            state side
                                            (when-not (= 1 (count ices))
                                              (choose-ice remaining grids))
                                            card nil)))))}))
          (choose-grid [ice ices grids]
            (if (= 1 (count grids))
              (install-ice ice ices grids (-> (first grids) :zone second zone->name))
              {:async true
               :prompt (str "Choose a server to install " (:title ice))
               :choices (conj (mapv #(-> % :zone second zone->name) grids) "None")
               :effect (effect (continue-ability (install-ice ice ices grids target) card nil))}))
          (choose-ice [ices grids]
            (when (seq ices)
              {:async true
               :prompt "Choose an ice to reveal and install"
               :choices (conj (mapv :title ices) "None")
               :effect
               (effect (continue-ability
                         (when-not (= "None" target)
                           (choose-grid (some #(when (= target (:title %)) %) ices) ices grids))
                         card nil))}))]
    {:events [{:event :corp-draw
               ;; This prevents multiple Jinja from showing the "choose a server to install into" sequence
               :once :per-turn
               :once-key :jinja-city-grid-draw
               :async true
               :waiting-prompt true
               :req (req (not (find-cid (:cid card) (flatten (vals (get-in @state [:trash :trash-list]))))))
               :effect (req (cond
                              ;; if ice were drawn, do the full routine
                              (some ice? corp-currently-drawing)
                              (let [ices (filter #(and (ice? %)
                                                       (get-card state %))
                                                 corp-currently-drawing)
                                    grids (filterv #(= (:title card) (:title %))
                                                   (all-active-installed state :corp))]
                                (continue-ability
                                  state side
                                  (when (not-empty ices)
                                    (choose-ice ices grids))
                                  card nil))
                              ;; else show a fake prompt so the runner can't infer that ice weren't drawn
                              :else
                              (continue-ability
                                state :corp
                                {:prompt "You did not draw any ice"
                                 :choices ["Carry on!"]
                                 :prompt-type :bogus}
                                card nil)))}
              {:event :post-corp-draw
               :effect (req (swap! state dissoc-in [:per-turn :jinja-city-grid-draw]))}]}))

(defcard "K. P. Lynn"
  {:events [{:event :pass-all-ice
             :req (req this-server)
             :player :runner
             :waiting-prompt true
             :prompt "Choose one"
             :choices ["Take 1 tag" "End the run"]
             :async true
             :msg (msg (if (= target "End the run")
                         (decapitalize target)
                         (str "force the Runner to " (decapitalize target))))
             :effect (req (if (= target "Take 1 tag")
                            (gain-tags state :runner eid 1)
                            (end-run state side eid card)))}]})

(defcard "Keegan Lane"
  {:abilities [{:req (req (and this-server
                               (some? (first (filter program? (all-active-installed state :runner))))))
                :prompt "Choose a program to trash"
                :label "Trash a program"
                :msg (msg "trash " (:title target))
                :choices {:card #(and (installed? %)
                                      (program? %))}
                :cost [(->c :tag 1) (->c :trash-can)]
                :async true
                :effect (effect (trash eid target {:cause-card card}))}]})

(defcard "Khondi Plaza"
  {:x-fn (req (count (get-remotes state)))
   :recurring (get-x-fn)
   :interactions {:pay-credits {:req (req (and (= :rez (:source-type eid))
                                               (ice? target)
                                               (same-server? card target)))
                                :type :recurring}}})

(defcard "La Costa Grid"
  (let [ability {:prompt (msg "Choose a card in " (zone->name (second (get-zone card))))
                 :label "Place 1 advancement counter (start of turn)"
                 :msg (msg "place 1 advancement counter on " (card-str state target))
                 :choices {:req (req (and (installed? target)
                                          (in-same-server? card target)))}
                 :effect (effect (add-prop target :advance-counter 1 {:placed true}))}]
    {:install-req (req (remove #{"HQ" "R&D" "Archives"} targets))
     :derezzed-events [corp-rez-toast]
     :flags {:corp-phase-12 (req true)}
     :events [(assoc ability :event :corp-turn-begins)]
     :abilities [ability]}))

(defcard "Letheia Nisei"
  {:events [{:event :approach-server
             :interactive (req true)
             :psi {:req (req this-server)
                   :once :per-run
                   :not-equal
                   {:optional
                    {:waiting-prompt true
                     :prompt (msg "Trash " (:title card) " to force the Runner to approach the outermost piece of ice?")
                     :autoresolve (get-autoresolve :auto-fire)
                     :yes-ability
                     {:async true
                      :msg "force the Runner to approach the outermost piece of ice"
                      :effect (req (wait-for (trash state side (make-eid state eid) card {:unpreventable true :cause-card card})
                                             (redirect-run state side (zone->name (second (get-zone card))) :approach-ice)
                                             (continue-ability state :runner (offer-jack-out) card nil)))}}}}}]
   :abilities [(set-autoresolve :auto-fire "Letheia Nisei")]})

(defcard "Malapert Data Vault"
  {:events [{:event :agenda-scored
             :interactive (req true)
             :optional
             {:prompt "Search R&D for non-agenda card?"
              :req (req (= (:previous-zone (:card context)) (get-zone card)))
              :yes-ability
              {:prompt "Choose a card"
               :choices (req (cancellable (filter #(not (agenda? %)) (:deck corp))
                                          :sorted))
               :msg (msg "reveal " (:title target) " from R&D and add it to HQ")
               :async true
               :effect (req (wait-for
                              (reveal state side target)
                              (shuffle! state side :deck)
                              (move state side target :hand)
                              (effect-completed state side eid)))}
              :no-ability
              {:effect (effect (system-msg (str "declines to use " (:title card))))}}}]})

(defcard "Manegarm Skunkworks"
  {:events [{:event :approach-server
             :interactive (req true)
             :player :runner
             :prompt "Choose one"
             :waiting-prompt true
             :req (req this-server)
             :choices (req [(when (can-pay? state :runner eid card nil [(->c :click 2)])
                              "Spend [Click][Click]")
                            (when (can-pay? state :runner eid card nil [(->c :credit 5)])
                              "Pay 5 [Credits]")
                            "End the run"])
             :async true
             :effect (req (cond+
                            [(and (= target "Spend [Click][Click]")
                                  (can-pay? state :runner eid card nil [(->c :click 2)]))
                             (wait-for (pay state side (make-eid state eid) card (->c :click 2))
                                       (system-msg state side (:msg async-result))
                                       (effect-completed state :runner eid))]
                            [(and (= target "Pay 5 [Credits]")
                                  (can-pay? state :runner eid card nil [(->c :credit 5)]))
                             (wait-for (pay state side (make-eid state eid) card (->c :credit 5))
                                       (system-msg state side (:msg async-result))
                                       (effect-completed state :runner eid))]
                            [:else
                             (system-msg state :corp (str "uses " (:title card) " to end the run"))
                             (end-run state :corp eid card)]))}]})

(defcard "Manta Grid"
  {:events [{:event :run-ends
             :msg "gain a [Click] next turn"
             :req (req (and (:successful target)
                            (= (target-server target) (second (get-zone card)))
                            (or (< (:credit runner) 6) (zero? (:click runner)))))
             :effect (req (swap! state update-in [:corp :extra-click-temp] (fnil inc 0)))}]})

(defcard "Marcus Batty"
  {:abilities [{:label "Start a Psi game to resolve a subroutine"
                :cost [(->c :trash-can)]
                :psi {:req (req this-server)
                      :not-equal
                      {:prompt "Choose a piece of ice"
                       :choices {:card #(and (ice? %)
                                             (rezzed? %))
                                 :all true}
                       :async true
                       :effect (effect
                                 (continue-ability
                                   (let [ice target]
                                     {:prompt "Choose a subroutine"
                                      :choices (req (unbroken-subroutines-choice ice))
                                      :msg (msg "resolve the subroutine (\"[subroutine] "
                                                                                        target "\") from " (:title ice))
                                      :async true
                                      :effect (req (let [sub (first (filter #(= target (make-label (:sub-effect %))) (:subroutines ice)))]
                                                     (resolve-subroutine! state side eid ice (assoc sub :external-trigger true))))})
                                   card nil))}}}]})

(defcard "Mason Bellamy"
  {:events [{:event :end-of-encounter
             :req (req (and this-server
                            (seq (filter :broken (:subroutines (:ice context))))))
             :msg "force the Runner to lose [Click]"
             :effect (effect (lose-clicks :runner 1))}]})

(defcard "Mavirus"
  {:flags {:rd-reveal (req true)}
   :on-access {:optional
               {:waiting-prompt true
                :prompt "Purge virus counters?"
                :yes-ability
                {:msg (msg "purge virus counters")
                 :async true
                 :effect (req (wait-for
                                (purge state side)
                                (if (rezzed? card)
                                  (do
                                    (system-msg state side (str "uses " (:title card) " to do 1 net damage"))
                                    (damage state side eid :net 1 {:card card}))
                                  (effect-completed state side eid))))}
                :no-ability {:async true
                             :effect (req (system-msg state :corp (str "declines to use " (:title card)))
                                          (if (rezzed? card)
                                            (do
                                              (system-msg state side (str "uses " (:title card) " to do 1 net damage"))
                                              (damage state side eid :net 1 {:card card}))
                                            (effect-completed state side eid)))}}}
   :abilities [{:label "Purge virus counters"
                :msg "purge virus counters"
                :cost [(->c :trash-can)]
                :async true
                :effect (effect (purge eid))}]})

(defcard "Midori"
  {:events [{:event :approach-ice
             :optional
             {:req (req this-server)
              :once :per-run
              :prompt "Swap the piece of ice being approached with a piece of ice from HQ?"
              :yes-ability
              {:async true
               :prompt "Choose a piece of ice"
               :choices {:card #(and (ice? %)
                                     (in-hand? %))}
               :msg (msg "swap " (card-str state current-ice)
                         " with a piece of ice from HQ")
               :effect (req (swap-cards state :corp current-ice target)
                            (continue-ability state :runner (offer-jack-out) card nil))}}}]})

(defcard "Midway Station Grid"
  {:static-abilities [{:type :break-sub-additional-cost
                       :req (req (and ; The card is an icebreaker
                                      (has-subtype? (:card context) "Icebreaker")
                                      ; and is using a break ability
                                      (contains? (:ability context) :break)
                                      (pos? (count (:broken-subs (:ability context))))
                                      ; during a run on this server
                                      this-server))
                       :value (->c :credit 1)}]})

(defcard "Mr. Hendrik"
  (installed-access-trigger
    2
    {:async true
     ;; Adding a msg to print the ability's cost
     :msg "force the Runner to suffer a core damage or lose all remaining [Click]"
     :effect
     (effect (continue-ability {:player :runner
                                :prompt "Choose one"
                                :waiting-prompt true
                                :choices (req ["Suffer 1 core damage"
                                               (when (pos? (:click runner))
                                                 "Lose all remaining [Click]")])
                                :async true
                                :msg (msg (if (= target "Suffer 1 core damage")
                                            "do 1 core damage"
                                            (str "force the Runner to " (decapitalize target))))
                                :effect (req (if (= target "Suffer 1 core damage")
                                               (damage state :corp eid :brain 1 {:card card})
                                               (do (lose-clicks state :runner (:click runner))
                                                   (effect-completed state side eid))))}
                               card nil))}))

(defcard "Mumbad City Grid"
  {:events [{:event :pass-ice
             :req (req (and this-server (<= 2 (count run-ices))))
             :async true
             :effect
             (effect
               (continue-ability
                 (let [passed-ice (:ice context)]
                   {:prompt (msg "Choose a piece of ice to swap with " (:title target))
                    :choices {:req (req (and (installed? target)
                                             (ice? target)
                                             (= (target-server run) (second (get-zone target)))
                                             (not (same-card? target passed-ice))))}
                    :effect (effect (swap-ice target passed-ice))})
                 card nil))}]})

(defcard "Mumbad Virtual Tour"
  {:flags {:must-trash (req (when installed true))}})

(defcard "Mwanza City Grid"
  ;; note - the 'unboost' and 'gain-creds' fns need to be tied to the access-boost fns,
  ;; otherwise we hit some edge cases where mwanza is trashed during access,
  ;; but another access is forced or credits are paid twice with something like
  ;; ganked into shiro or ganked into kitsune -nbkelly, 2022
  (let [mwanza-gain-creds
        {:silent (req true)
         :async true
         :unregister-once-resolved true
         :effect (req (if-let [accessed-cards (reduce + (vals (:cards-accessed target)))]
                        (do (system-msg state :corp
                                        (str "gains " (* 2 accessed-cards)
                                             " [Credits] from "(:title card)))
                            (gain-credits state :corp eid (* 2 accessed-cards)))
                        (effect-completed state side eid)))}
        unboost-access (fn [bonus-server]
                         {:req (req (= (:from-server target) bonus-server))
                          :unregister-once-resolved true
                          :effect (req (access-bonus state :runner bonus-server -3))})
        boost-access-when-trashed (fn [bonus-server]
                                    {:req (req (= target bonus-server))
                                     :msg "force the runner to access 3 additional cards"
                                     :effect (req (access-bonus state :runner bonus-server 3)
                                                  (register-events
                                                   state side
                                                   card
                                                   [(assoc mwanza-gain-creds
                                                           :event :end-breach-server
                                                           :duration :end-of-run)
                                                    (assoc (unboost-access bonus-server)
                                                           :event :end-breach-server
                                                           :duration :end-of-run)]))})
        boost-access-by-3 {:req (req (= target (second (get-zone card))))
                           :msg "force the Runner to access 3 additional cards"
                           :effect (req (let [bonus-server (-> card :zone second)]

                                          (access-bonus state :runner bonus-server 3)
                                          (register-events
                                           state side
                                           card
                                           [(assoc mwanza-gain-creds
                                                           :event :end-breach-server
                                                           :duration :end-of-run)
                                            (assoc (unboost-access bonus-server)
                                                   :event :end-breach-server
                                                   :duration :end-of-run)])))}]
    {:install-req (req (filter #{"HQ" "R&D"} targets))
     :events [(assoc boost-access-by-3 :event :breach-server)]
     ;; if there is a run, mark mwanza effects to remain active until the run
     :on-trash  {:req (req (and (= :runner side)
                                (:run @state)))
                 :effect (req
                          (let [bonus-server (second (:previous-zone card))]
                            (register-events
                             state side
                             card
                             [(assoc (boost-access-when-trashed bonus-server)
                                     :event :breach-server
                                     :duration :end-of-run)])))}}))

(defcard "Nanisivik Grid"
  {:events [{:event :approach-server
             :interactive (req true)
             :prompt "Choose a facedown piece of ice in Archives"
             :waiting-prompt true
             :req (req (and this-server
                            ;; not filtering ice only so that we don't reveal valuable information
                            (seq (filter #(not (:seen %)) (:discard corp)))))
             :show-discard true
             :choices {:card #(and (ice? %)
                                   (in-discard? %)
                                   (not (:seen %)))}
             :async true
             :msg (msg "reveal " (:title target) " from Archives")
             :effect
             (req (wait-for (reveal state side target)
                            (update! state side (assoc target :seen true))
                            (continue-ability
                              state side
                              (let [ice (get-card state target)]
                                {:async true
                                 :prompt "Choose a subroutine to resolve"
                                 :choices (req (unbroken-subroutines-choice ice))
                                 :msg (msg "resolve the subroutine (\"[subroutine] " target "\") from " (card-str state ice))
                                 :effect (req (let [sub (first (filter #(= target (make-label (:sub-effect %))) (:subroutines ice)))]
                                                (continue-ability state side (:sub-effect sub) ice nil)))})
                              card nil)))}]})

(defcard "Navi Mumbai City Grid"
  {:static-abilities [{:type :prevent-paid-ability
                       :req (req (let [target-card (first targets)]
                                   (and run
                                        (= (:side target-card) "Runner")
                                        (= (target-server run) (second (get-zone card)))
                                        (not (has-subtype? target-card "Icebreaker")))))
                       :value true}]})

(defcard "NeoTokyo Grid"
  (let [ng {:req (req (in-same-server? card (:card context)))
            :once :per-turn
            :msg "gain 1 [Credits]"
            :async true
            :effect (effect (gain-credits eid 1))}]
    {:events [(assoc ng :event :advance)
              (assoc ng :event :advancement-placed)]}))

(defcard "Nihongai Grid"
  {:events [{:event :successful-run
             :interactive (req true)
             :optional
             {:req (req (and this-server
                             (or (< (total-available-credits state :runner eid card) 6)
                                 (< (count (:hand runner)) 2))
                             (not-empty (:hand corp))
                             (pos? (count (take 5 (:deck corp))))))
              :prompt "Look at the top 5 cards of R&D?"
              :yes-ability
              {:async true
               :msg "look at the top 5 cards of R&D"
               :effect
               (effect
                 (continue-ability
                   {:async true
                    :prompt "Choose a card in R&D"
                    :choices (take 5 (:deck corp))
                    :effect (effect
                              (continue-ability
                                (when-let [rdc target]
                                  {:prompt "Choose a card in HQ"
                                   :choices {:card in-hand?}
                                   :msg "swap a card from the top 5 of R&D with a card in HQ"
                                   :effect (req (move state side rdc :hand)
                                                (move state side target :deck {:index (:index rdc)}))})
                                card nil))}
                   card nil))}}}]})

(defcard "Oaktown Grid"
  {:static-abilities [{:type :trash-cost
                       :req (req (in-same-server? card target))
                       :value 3}]})

(defcard "Oberth Protocol"
  {:additional-cost [(->c :forfeit)]
   :events [{:event :advance
             :req (req (and (same-server? card (:card context))
                            (= 1 (count (filter #(= (second (get-zone (:card %))) (second (get-zone card)))
                                                (map first (turn-events state side :advance)))))))
             :msg (msg "place 1 additional advancement token on " (card-str state (:card context)))
             :effect (effect (add-prop :corp (:card context) :advance-counter 1 {:placed true}))}]})

(defcard "Off the Grid"
  {:install-req (req (remove #{"HQ" "R&D" "Archives"} targets))
   :static-abilities [{:type :cannot-run-on-server
                       :req (req (rezzed? card))
                       :value (req (second (get-zone card)))}]
   :events [{:event :successful-run
             :req (req (= :hq (target-server context)))
             :async true
             :msg "trash itself"
             :effect (req (trash state :corp eid card {:cause-card card}))}]})

(defcard "Old Hollywood Grid"
  {:on-trash
   {:req (req (and (= :runner side)
                   (:run @state)))
    :effect (effect (register-lingering-effect
                      card
                      {:type :cannot-steal
                       :duration :end-of-run
                       :req (req (and (not-any? #(= (:title target) (:title %))
                                                (:scored runner))
                                      (or (= (get-zone target)
                                             (:previous-zone card))
                                          (= (central->zone (get-zone target))
                                             (butlast (:previous-zone card))))))
                       :value true}))}
   :static-abilities [{:type :cannot-steal
                       :duration :end-of-run
                       :req (req (and (not-any? #(= (:title target) (:title %))
                                                (:scored runner))
                                      (or (in-same-server? card target)
                                          (from-same-server? card target))))
                       :value true}]})

(defcard "Overseer Matrix"
  (let [ability {:event :runner-trash
                 :once-per-instance true
                 :interactive (req true)
                 :req (req (some #(and (corp? (:card %))
                                       (or (in-same-server? card (:card %))
                                           (from-same-server? card (:card %))))
                                 targets))
                 :waiting-prompt true
                 :prompt "How many credits do you want to pay?"
                 :player :corp
                 :choices {:number (req (min (->> targets
                                                  (filter #(or (in-same-server? card (:card %))
                                                               (from-same-server? card (:card %))
                                                               (in-same-server? (assoc card :zone (:previous-zone card)) (:card %))))
                                                  count)
                                             (total-available-credits state :corp eid card)))}
                 :async true
                 :effect
                 (effect
                   (continue-ability
                     (let [n target]
                       {:async true
                        :cost [(->c :credit n)]
                        :msg (str "give the Runner " (quantify n "tag"))
                        :effect (effect (gain-tags :corp eid n))})
                     card nil))}]
    {:on-trash {:silent (req true)
                :req (req (= :runner side))
                :effect (req (when run
                               (register-events
                                 state side card
                                 [(assoc ability :duration :end-of-run)])))}
     :events [ability]}))

(defcard "Panic Button"
  {:install-req (req (filter #{"HQ"} targets))
   :abilities [{:cost [(->c :credit 1)]
                :keep-menu-open :while-credits-left
                :msg "draw 1 card"
                :req (req (and run (= (target-server run) :hq)))
                :async true
                :effect (effect (draw eid 1))}]})

(defcard "Port Anson Grid"
  {:on-rez {:msg "prevent the Runner from jacking out unless they trash an installed program"}
   :static-abilities [{:type :jack-out-additional-cost
                       :duration :end-of-run
                       :req (req this-server)
                       :value [(->c :program 1)]}]
   :events [{:event :run
             :req (req this-server)
             :msg "prevent the Runner from jacking out unless they trash an installed program"}]})

(defcard "Prisec"
  (installed-access-trigger
    2
    {:waiting-prompt true
     :msg "do 1 meat damage and give the Runner 1 tag"
     :async true
     :effect (req (wait-for (damage state side :meat 1 {:card card})
                            (gain-tags state :corp eid 1)))}))

(defcard "Product Placement"
  {:flags {:rd-reveal (req true)}
   :on-access {:req (req (not (in-discard? card)))
               :msg "gain 2 [Credits]"
               :async true
               :effect (effect (gain-credits :corp eid 2))}})

(defcard "Red Herrings"
  {:on-trash
   {:req (req (and (= :runner side)
                   (:run @state)))
    :effect (effect (register-lingering-effect
                      card
                      {:type :steal-additional-cost
                       :duration :end-of-run
                       :req (req (or (= (get-zone target) (:previous-zone card))
                                     (= (central->zone (get-zone target))
                                        (butlast (:previous-zone card)))))
                       :value (req (->c :credit 5))}))}
   :static-abilities [{:type :steal-additional-cost
                       :req (req (or (in-same-server? card target)
                                     (from-same-server? card target)))
                       :value (req (->c :credit 5))}]})

(defcard "Reduced Service"
  {:static-abilities [{:type :run-additional-cost
                       :req (req (= (:server (second targets)) (unknown->kw (get-zone card))))
                       :value (req (repeat (get-counters card :power) [(->c :credit 2)]))}]
   :events [{:event :successful-run
             :req (req (and (pos? (get-counters card :power))
                            (is-central? (:server context))))
             :msg "remove 1 hosted power counter"
             :effect (effect (add-counter card :power -1))}]
   :on-rez {:waiting-prompt true
            :prompt "How many credits do you want to pay?"
            :choices (req (map str (range (inc (min 4 (get-in @state [:corp :credit]))))))
            :async true
            :effect (req (let [spent (str->int target)]
                           (add-counter state :corp card :power spent)
                           (system-msg state :corp (str "uses " (:title card) " to place "
                                                        (quantify spent "power counter")
                                                        " on itself"))
                           (lose-credits state :corp eid spent)))}})

(defcard "Research Station"
  {:install-req (req (filter #{"HQ"} targets))
   :static-abilities [(corp-hand-size+ 2)]})

(defcard "Ruhr Valley"
  {:static-abilities [{:type :run-additional-cost
                       :req (req (= (:server (second targets)) (unknown->kw (get-zone card))))
                       :value [(->c :click 1)]}]})

(defcard "Rutherford Grid"
  {:static-abilities [{:type :trace-base-strength
                       :req (req this-server)
                       :value 2}]})

(defcard "Ryon Knight"
  {:abilities [{:label "Do 1 core damage"
                :req (req (and this-server (zero? (:click runner))))
                :cost [(->c :trash-can)]
                :msg "do 1 core damage"
                :async true
                :effect (effect (damage eid :brain 1 {:card card}))}]})

(defcard "SanSan City Grid"
  {:static-abilities [{:type :advancement-requirement
                       :req (req (in-same-server? card target))
                       :value -1}]})

(defcard "Satellite Grid"
  {:on-rez {:effect (req (doseq [c (:ices (card->server state card))]
                           (set-prop state side c :extra-advance-counter 1))
                         (update-all-ice state side))}
   :events [{:event :corp-install
             :req (req (and (ice? (:card context))
                            (protecting-same-server? card (:card context))))
             :effect (effect (set-prop (:card context) :extra-advance-counter 1))}]
   :leave-play (req (doseq [c (:ices (card->server state card))]
                      (update! state side (dissoc c :extra-advance-counter)))
                    (update-all-ice state side))})

(defcard "Self-destruct"
  {:install-req (req (remove #{"HQ" "R&D" "Archives"} targets))
   :abilities [{:async true
                :req (req this-server)
                :cost [(->c :trash-can)]
                :label "Trace X - Do 3 net damage"
                :effect (req (let [serv (card->server state card)
                                   cards (concat (:ices serv) (:content serv))]
                               (wait-for (trash-cards state side cards {:cause-card card})
                                         (continue-ability
                                           state side
                                           {:trace
                                            {:base (count cards)
                                             :successful
                                             {:async true
                                              :msg "do 3 net damage"
                                              :effect (effect (damage eid :net 3 {:card card}))}}}
                                           card nil))))}]})

(defcard "Shell Corporation"
  {:abilities
   [{:cost [(->c :click 1)]
     :msg "store 3 [Credits]"
     :once :per-turn
     :effect (effect (add-counter card :credit 3))}
    {:cost [(->c :click 1)]
     :msg (msg "gain " (get-counters card :credit) " [Credits]")
     :once :per-turn
     :label "Take all credits"
     :async true
     :effect (effect (add-counter card :credit (- (get-counters card :credit)))
                     (gain-credits eid (get-counters card :credit)))}]})

(defcard "Signal Jamming"
  {:abilities [{:label "Cards cannot be installed until the end of the run"
                :msg "prevent cards being installed until the end of the run"
                :req (req this-server)
                :cost [(->c :trash-can)]
                :effect (effect (register-run-flag! card :corp-lock-install (constantly true))
                                (register-run-flag! card :runner-lock-install (constantly true))
                                (toast :runner "Cannot install until the end of the run")
                                (toast :corp "Cannot install until the end of the run"))}]})

(defcard "Simone Diego"
  {:recurring 2
   :interactions {:pay-credits {:req (req (let [ab-target (:card (get-ability-targets eid))]
                                            (and (same-server? card ab-target)
                                                 (or (= :advance (:source-type eid))
                                                     (is-basic-advance-action? eid)))))
                                :type :recurring}}})

(defcard "Strongbox"
  {:on-trash
   {:req (req (and (= :runner side)
                   (:run @state)))
    :effect (effect (register-lingering-effect
                      card
                      {:type :steal-additional-cost
                       :duration :end-of-run
                       :req (req (or (= (get-zone target) (:previous-zone card))
                                     (= (central->zone (get-zone target))
                                        (butlast (:previous-zone card)))))
                       :value (req (->c :click 1))}))}
   :static-abilities [{:type :steal-additional-cost
                       :req (req (or (in-same-server? card target)
                                     (from-same-server? card target)))
                       :value (req (->c :click 1))}]})

(defcard "Surat City Grid"
  {:events [{:event :rez
             :interactive (req true)
             :optional
             {:req (req (let [target (:card context)]
                          (and (same-server? card target)
                               (not (same-card? target card))
                               (some #(and (not (rezzed? %))
                                           (not (agenda? %))
                                           (can-pay? state side (assoc eid :source card :source-type :rez) % nil
                                                     [(->c :credit (install-cost state side % {:cost-bonus -2}))]))
                                     (all-installed state :corp)))))
              :prompt "Rez another card paying 2 [Credits] less?"
              :yes-ability {:prompt "Choose a card to rez"
                            :choices {:req (req (and (not (rezzed? target))
                                                     (not (agenda? target))
                                                     (can-pay? state side (assoc eid :source card :source-type :rez)
                                                               target nil
                                                               [(->c :credit (rez-cost state side target {:cost-bonus -2}))])))}
                            :msg (msg "rez " (:title target) ", lowering the rez cost by 2 [Credits]")
                            :async true
                            :effect (effect (rez eid target {:cost-bonus -2}))}}}]})

(defcard "Tempus"
  {:flags {:rd-reveal (req true)}
   :on-access {:interactive (req true)
               :trace {:req (req (not (in-discard? card)))
                       :base 3
                       :successful
                       {:waiting-prompt true
                        :prompt "Choose one"
                        :player :runner
                        :choices (req [(when (<= 2 (:click runner))
                                         "Lose [Click][Click]")
                                       "Suffer 1 core damage"])
                        :async true
                        :msg (msg "force the Runner to " (decapitalize target))
                        :effect (req (if (and (= target "Lose [Click][Click]")
                                              (<= 2 (:click runner)))
                                       (do (lose-clicks state :runner 2)
                                           (effect-completed state side eid))
                                       (damage state side eid :brain 1 {:card card})))}}}})

(defcard "The Holo Man"
  (let [abi
        {:cost [(->c :click) (->c :credit 4)]
         :label "Place advancement counters on a card in or protecting this server"
         :once :per-turn
         :choices {:req (req (same-server? card target))}
         :effect
         (req (let [n (if (no-event? state side :corp-install #(= [:hand] (:previous-zone (:card (first %))))) 3 2)]
                (system-msg state side (str "uses " (card-str state card) " to place " (quantify n "advancement counter") " on " (card-str state target)))
                (add-prop state side eid target :advance-counter n {:placed true})))}]
    {:abilities [abi]
     :events [(mobile-sysop-event :corp-turn-begins)]}))

(defcard "The Twins"
  {:events [{:event :pass-ice
             :optional
             {:req (req (and this-server
                             (rezzed? (:ice context))
                             (seq (filter #(same-card? :title % (:ice context)) (:hand corp)))))
              :prompt (msg "Force the runner to encounter "
                           (:title current-ice) " again?")
              :yes-ability
              {:async true
               :prompt (msg "Choose a copy of " (:title current-ice) " in HQ")
               :choices {:req (req (and (in-hand? target)
                                        (ice? target)
                                        (same-card? :title current-ice target)))}
               :msg (msg "reveal a copy of " (:title target) " from HQ, trash it and force the Runner to encounter it again")
               :effect (req (wait-for
                              (reveal state side target)
                              (wait-for (trash state side (make-eid state eid) (assoc target :seen true) {:cause-card card})
                                        (force-ice-encounter state side eid current-ice))))}}}]})

(defcard "Tori Hanzō"
  {:events [{:event :pre-resolve-damage
             :optional
             {:req (req (and this-server
                             (= target :net)
                             (= :corp (second targets))
                             (pos? (last targets))
                             (first-run-event? state side :pre-resolve-damage
                                               (fn [[t s]]
                                                 (and (= :net t)
                                                      (= :corp s))))
                             (can-pay? state :corp (assoc eid :source card :source-type :ability) card nil [(->c :credit 2)])))
              :waiting-prompt true
              :prompt "Pay 2 [Credits] to do 1 core damage?"
              :player :corp
              :yes-ability
              {:async true
               :msg "do 1 core damage instead of net damage"
               :effect (req (swap! state update :damage dissoc :damage-replace :defer-damage)
                            (wait-for (pay state :corp (make-eid state eid) card (->c :credit 2))
                                      (system-msg state side (:msg async-result))
                                      (wait-for (damage state side :brain 1 {:card card})
                                                (swap! state assoc-in [:damage :damage-replace] true)
                                                (effect-completed state side eid))))}
              :no-ability
              {:effect (req (swap! state update :damage dissoc :damage-replace))}}}]})

(defcard "Traffic Analyzer"
  {:events [{:event :rez
             :interactive (req true)
             :trace {:base 2
                     :req (req (and (protecting-same-server? card (:card context))
                                    (ice? (:card context))))
                     :successful {:msg "gain 1 [Credits]"
                                  :async true
                                  :effect (effect (gain-credits eid 1))}}}]})

(defcard "Tranquility Home Grid"
  {:install-req (req (remove #{"HQ" "R&D" "Archives"} targets))
   :events [{:event :corp-install
             :interactive (req true)
             :req (req (and (or (asset? (:card context))
                                (agenda? (:card context))
                                (upgrade? (:card context)))
                            (in-same-server? card (:card context))
                            (first-event? state :corp :corp-install #(in-same-server? card (:card (first %))))))
             :prompt "Choose one"
             :waiting-prompt true
             :choices ["Gain 2 [Credits]" "Draw 1 card"]
             :msg (msg (decapitalize target))
             :async true
             :effect (req (if (= target "Gain 2 [Credits]")
                            (gain-credits state side eid 2)
                            (draw state side eid 1)))}]})

(defcard "Tucana"
  (let [ability {:async true
                 :prompt "Choose a piece of ice to install and rez"
                 :waiting-prompt true
                 :interactive (req true)
                 :choices (req (cancellable (filter ice? (:deck corp)) true))
                 :msg (msg "install and rez " (card-str state target) ", paying a total of 3 [Credits] less")
                 :effect (req (wait-for (corp-install state side (make-eid state eid) target nil {:install-state :rezzed :combined-credit-discount 3})
                                        (shuffle! state :corp :deck)
                                        (system-msg state side (str "shuffles R&D"))
                                        (effect-completed state side eid)))
                 :cancel-effect (effect (system-msg (str "declines to use " (:title card)))
                                        (effect-completed eid))}]
    {:install-req (req (remove #{"HQ" "R&D" "Archives"} targets))
     :events [(assoc ability
                     :event :agenda-stolen
                     :req (req (= (:previous-zone (:card context)) (get-zone card))))
              (assoc ability
                     :event :agenda-scored
                     :req (req (= (:previous-zone (:card context)) (get-zone card))))]
     :on-trash
     {:req (req (and run (= :runner side)))
      :effect (effect (register-events
                        card
                        [(assoc ability
                                :event :agenda-stolen
                                :req (req (= (:previous-zone card) (:previous-zone (:card context))))
                                :duration :end-of-run)]))}}))

(defcard "Tyr's Hand"
  {:abilities [{:label "Prevent a subroutine on a piece of Bioroid ice from being broken"
                :req (req (and (= (butlast (get-zone current-ice)) (butlast (get-zone card)))
                               (has-subtype? current-ice "Bioroid")))
                :cost [(->c :trash-can)]
                :msg (msg "prevent a subroutine on " (:title current-ice) " from being broken")}]})

(defcard "Underway Grid"
  {:events [{:event :pre-expose
             :req (req (same-server? card target))
             :msg "prevent 1 card from being exposed"
             :effect (effect (expose-prevent 1))}]
   :static-abilities [{:type :bypass-ice
                       :req (req (same-server? card target))
                       :value false}]})

(defcard "Valley Grid"
  {:events [{:event :subroutines-broken
             :req (req (and this-server (all-subs-broken? target)))
             :msg "reduce the Runner's maximum hand size by 1 until the start of the next Corp turn"
             :effect (effect (register-lingering-effect
                               card
                               {:type :hand-size
                                :duration :until-corp-turn-begins
                                :req (req (= :runner side))
                                :value -1}))}]})

(defcard "Vladisibirsk City Grid"
  {:advanceable :always
   :abilities [{:cost [(->c :advancement 2)]
                :once :per-turn
                :prompt (msg "Choose an advanceable card in " (zone->name (second (get-zone card))))
                :label "Place 2 advancement counters (once per turn)"
                :msg (msg "place 2 advancement counters on " (card-str state target))
                :choices {:not-self true
                          :req (req (and (installed? target)
                                         (can-be-advanced? state target)
                                         (in-same-server? card target)))}
                :effect (effect (add-prop target :advance-counter 2 {:placed true}))}]})

(defcard "Vovô Ozetti"
   {:static-abilities [{:type :rez-cost
                       :req (req (and (or (ice? target)
                                          (threat-level 4 state))
                                      (= (card->server state card) (card->server state target))))
                        :value -2}]
    :events [(mobile-sysop-event)]})

(defcard "Warroid Tracker"
  (letfn [(wt [n]
            {:waiting-prompt true
             :prompt "Choose an installed card to trash"
             :async true
             :interactive (req true)
             :player :runner
             :choices {:all true
                       :max n
                       :card #(and (runner? %)
                                   (installed? %))}
             :msg (msg "force the Runner to trash " (enumerate-str (map :title targets)))
             :effect (req (trash-cards state :runner eid targets {:unpreventable true
                                                                  :cause-card card
                                                                  :cause :forced-to-trash}))})
          (ability []
            {:trace {:base 4
                     :successful
                     {:async true
                      :msg (msg (let [n (min 2 (count (all-installed state :runner)))]
                                  (str "force the runner to trash "
                                       (quantify n "installed card")
                                       (when (not (pos? n))
                                         "but there are no installed cards to trash"))))
                      :effect (effect (continue-ability
                                        (let [n (min 2 (count (all-installed state :runner)))]
                                          (when (pos? n)
                                            (wt n)))
                                        card nil))}}})]
    {:events [{:event :runner-trash
               :async true
               :once-per-instance true
               :req (req (some (fn [target]
                                 (and (corp? (:card target))
                                      (let [target-zone (get-zone (:card target))
                                            target-zone (or (central->zone target-zone) target-zone)
                                            warroid-zone (get-zone card)]
                                        (= (second warroid-zone)
                                           (second target-zone)))))
                               targets))
               :effect (effect (continue-ability (ability) card nil))}]}))

(defcard "Will-o'-the-Wisp"
  {:implementation "Doesn't restrict icebreaker selection"
   :events [{:event :successful-run
             :interactive (req true)
             :optional
             {:req (req (and this-server
                             (some #(has-subtype? % "Icebreaker") (all-active-installed state :runner))))
              :waiting-prompt true
              :prompt (msg "Trash " (:title card) " to choose an icebreaker?")
              :yes-ability {:async true
                            :prompt "Choose an icebreaker used to break at least 1 subroutine during this run"
                            :choices {:card #(has-subtype? % "Icebreaker")}
                            :msg (msg "add " (:title target) " to the bottom of the stack")
                            :effect (req (wait-for (trash state side card {:cause-card card})
                                                   (move state :runner target :deck)
                                                   (effect-completed state side eid)))}}}]})

(defcard "Yakov Erikovich Avdakov"
  (letfn [(valid-target-fn [context card]
            (and (same-server? card (:card context))
                 (corp? (:card context))
                 (installed? (:card context))))]
    {:events [{:event :runner-trash
               :async true
               :once-per-instance false
               :interactive (req true)
               :req (req (valid-target-fn context card))
               :msg "gain 2 [Credits]"
               :effect (effect (gain-credits eid 2))}
              {:event :corp-trash
               :interactive (req true)
               :once-per-instance false
               :req (req (let [cause (:cause context)
                               cause-card (:cause-card context)]
                           (and (not= :corp-install (:source-type eid))
                                (or (corp? (:source eid))
                                    (= :ability-cost cause)
                                    (= :subroutine cause)
                                    (and (corp? cause-card)
                                         (not= cause :opponent-trashes))
                                    (and (runner? cause-card)
                                         (= cause :forced-to-trash)))
                                (valid-target-fn context card))))
               :async true
               :msg "gain 2 [Credits]"
               :effect (effect (gain-credits eid 2))}]}))

(defcard "ZATO City Grid"
  {:install-req (req (remove #{"HQ" "R&D" "Archives"} targets))
   :static-abilities [{:type :gain-encounter-ability
                       :req (req (and (protecting-same-server? card target)
                                      (some #(:printed %) (:subroutines target))
                                      (not (:disabled target))))
                       :value (req {:async true
                                    :ability-name "ZATO Ability"
                                    :interactive (req true)
                                    :optional
                                    {:waiting-prompt true
                                     :prompt "Trash ice to fire a (printed) subroutine?"
                                     :yes-ability {:msg (msg "trash " (card-str state (:ice context)))
                                                   :async true
                                                   :effect (req (let [target-ice (:ice context)]
                                                                  (wait-for (trash state side target-ice {:cause-card target-ice})
                                                                            (continue-ability
                                                                              state side
                                                                              {:prompt "Choose a subroutine to resolve"
                                                                               :choices (req (unbroken-subroutines-choice target-ice))
                                                                               :msg (msg "resolves the subroutine (\"[Subroutine] "
                                                                                         target "\") from " (:title target-ice))
                                                                               :async true
                                                                               :effect (req (let [sub (first (filter #(= target (make-label (:sub-effect %))) (:subroutines target-ice)))]
                                                                                              (resolve-subroutine! state side eid target-ice (assoc sub :external-trigger true))))}
                                                                              card nil))))}}})}]})
