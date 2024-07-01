(ns game.cards.basic
  (:require
   [game.core.agendas :refer [update-advancement-requirement]]
   [game.core.board :refer [all-active-installed installable-servers]]
   [game.core.card :refer [agenda? asset? event? get-card hardware? ice?
                           in-hand? operation? program? resource? upgrade?]]
   [game.core.def-helpers :refer [defcard]]
   [game.core.drawing :refer [draw use-bonus-click-draws!]]
   [game.core.eid :refer [complete-with-result effect-completed make-eid]]
   [game.core.effects :refer [get-effects]]
   [game.core.engine :refer [pay resolve-ability trigger-event]]
   [game.core.flags :refer [can-advance? untrashable-while-resources?]]
   [game.core.gaining :refer [gain-credits]]
   [game.core.installing :refer [corp-can-pay-and-install? corp-install
                                 runner-can-pay-and-install? runner-install]]
   [game.core.moving :refer [trash]]
   [game.core.payment :refer [build-cost-string can-pay? merge-costs ->c]]
   [game.core.play-instants :refer [can-play-instant? play-instant]]
   [game.core.props :refer [add-prop]]
   [game.core.purging :refer [purge]]
   [game.core.runs :refer [make-run]]
   [game.core.say :refer [play-sfx system-msg]]
   [game.core.tags :refer [lose-tags]]
   [game.core.to-string :refer [card-str]]
   [game.macros :refer [effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]))
;; Card definitions

(defcard "Corp Basic Action Card"
  {:abilities [{:action true
                :label "Gain 1 [Credits]"
                :cost [(->c :click)]
                :msg "gain 1 [Credits]"
                :async true
                :effect (req (wait-for (gain-credits state side 1 :corp-click-credit)
                                       (swap! state update-in [:stats side :click :credit] (fnil inc 0))
                                       (play-sfx state side "click-credit")
                                       (effect-completed state side eid)))}
               {:action true
                :label "Draw 1 card"
                :req (req (not-empty (:deck corp)))
                :cost [(->c :click)]
                :msg "draw 1 card"
                :async true
                :effect (req (trigger-event state side :corp-click-draw {:card (-> @state side :deck (nth 0))})
                             (swap! state update-in [:stats side :click :draw] (fnil inc 0))
                             (play-sfx state side "click-card")
                             (draw state side eid 1))}
               {:action true
                :label "Install 1 agenda, asset, upgrade, or piece of ice from HQ"
                :async true
                :req (req (let [{target-card :card server :server} context]
                            (and (not-empty (:hand corp))
                                 (in-hand? target-card)
                                 (or (agenda? target-card)
                                     (asset? target-card)
                                     (ice? target-card)
                                     (upgrade? target-card))
                                 (if server
                                   (corp-can-pay-and-install?
                                     state side eid
                                     target-card server {:base-cost [(->c :click 1)]
                                                    :action :corp-click-install
                                                    :no-toast true})
                                   (some
                                     (fn [server]
                                       (corp-can-pay-and-install?
                                         state side eid
                                         target-card server {:base-cost [(->c :click 1)]
                                                        :action :corp-click-install
                                                        :no-toast true}))
                                     (installable-servers state target-card))))))
                :effect (req (let [{target-card :card server :server} context]
                               (corp-install
                                 state side eid
                                 target-card server {:base-cost [(->c :click 1)]
                                                     :action :corp-click-install})))}
               {:action true
                :label "Play 1 operation"
                :async true
                :req (req (let [target-card (:card context)]
                            (and (not-empty (:hand corp))
                                 (in-hand? target-card)
                                 (operation? target-card)
                                 (can-play-instant? state :corp eid
                                                    target-card {:base-cost [(->c :click 1)]}))))
                :effect (req (play-instant state :corp eid
                                           (:card context) {:base-cost [(->c :click 1)]}))}
               {:action true
                :label "Advance 1 installed card"
                :cost [(->c :click 1) (->c :credit 1)]
                :async true
                :msg (msg "advance " (card-str state (:card context)))
                :req (req (can-advance? state side (:card context)))
                :effect (effect (update-advancement-requirement (:card context))
                                (add-prop (get-card state (:card context)) :advance-counter 1)
                                (play-sfx "click-advance")
                                (effect-completed eid))}
               {:action true
                :label "Trash 1 resource if the Runner is tagged"
                :cost [(->c :click 1) (->c :credit 2)]
                :async true
                :req (req tagged)
                :prompt "Choose a resource to trash"
                :msg (msg "trash " (:title (:card context)))
                ;; I hate that we need to modify the basic action card like this, but I don't think there's any way around it -nbkelly, '24
                :choices {:req (req (and (if (and (->> (all-active-installed state :runner)
                                                       (filter (fn [c] (untrashable-while-resources? c)))
                                                       (seq))
                                                  (< 1 (->> (all-active-installed state :runner)
                                                            (filter resource?)
                                                            count)))
                                           true
                                           (not (untrashable-while-resources? target)))
                                         (resource? target)
                                         (let [additional-costs
                                               (->> (get-effects state side :basic-ability-additional-trash-cost target)
                                                    (concat (get-effects state side :additional-trash-cost target))
                                                    (into [])
                                                    (merge-costs))
                                               can-pay (can-pay? state side (make-eid state (assoc eid :additional-costs additional-costs)) target (:title target) additional-costs)]
                                           (or (empty? additional-costs) can-pay))))}
                :effect (req
                          (let [additional-costs (merge-costs (get-effects state side :basic-ability-additional-trash-cost target))
                                cost-strs (build-cost-string additional-costs)
                                can-pay (can-pay? state side (make-eid state (assoc eid :additional-costs additional-costs)) target (:title target) additional-costs)]
                            (if (empty? additional-costs)
                              (trash state side eid target nil)
                              (wait-for (resolve-ability
                                          state side
                                          (make-eid state eid)
                                          {:prompt (str "Pay the additional cost to trash " (:title target) "?")
                                           :choices [(when can-pay cost-strs) "No"]
                                           :async true
                                           :effect (req (if (= target "No")
                                                          (do (system-msg state side (str "declines to pay the additional cost to trash " (:title target)))
                                                              (effect-completed state side eid))
                                                          (wait-for (pay state side (make-eid state
                                                                                              (assoc eid
                                                                                                     :additional-costs additional-costs
                                                                                                     :source-type :trash-card))
                                                                         nil additional-costs)
                                                                    (system-msg state side (str (:msg async-result) " as an additional cost to trash " (:title target)))
                                                                    (complete-with-result state side eid target))))}
                                          card nil)
                                        (if async-result
                                          (trash state side eid target nil)
                                          (effect-completed state side eid))))))}
               {:action true
                :label "Purge virus counters"
                :cost [(->c :click 3)]
                :msg "purge all virus counters"
                :async true
                :effect (req (play-sfx state side "virus-purge")
                             (purge state side eid))}]})

(defcard "Runner Basic Action Card"
  {:abilities [{:action true
                :label "Gain 1 [Credits]"
                :cost [(->c :click)]
                :msg "gain 1 [Credits]"
                :async true
                :effect (req (wait-for (gain-credits state side 1 :runner-click-credit)
                                       (swap! state update-in [:stats side :click :credit] (fnil inc 0))
                                       (play-sfx state side "click-credit")
                                       (effect-completed state side eid)))}
               {:action true
                :label "Draw 1 card"
                :req (req (not-empty (:deck runner)))
                :cost [(->c :click)]
                :msg "draw 1 card"
                :effect (req (trigger-event state side :runner-click-draw {:card (-> @state side :deck (nth 0))})
                             (swap! state update-in [:stats side :click :draw] (fnil inc 0))
                             (play-sfx state side "click-card")
                             (draw state side eid (+ 1 (use-bonus-click-draws! state))))}
               {:action true
                :label "Install 1 program, resource, or piece of hardware from the grip"
                :async true
                :req (req (let [target-card (:card context)]
                            (and (not-empty (:hand runner))
                                 (in-hand? target-card)
                                 (or (hardware? target-card)
                                     (program? target-card)
                                     (resource? target-card))
                                 (runner-can-pay-and-install?
                                   state :runner (assoc eid :source-type :runner-install)
                                   target-card {:base-cost [(->c :click 1)]}))))
                :effect (req (runner-install
                               state :runner (dissoc eid :source-type)
                               (:card context) {:base-cost [(->c :click 1)]
                                                :no-toast true}))}
               {:action true
                :label "Play 1 event"
                :async true
                :req (req (let [target-card (:card context)]
                            (and (not-empty (:hand runner))
                                 (in-hand? target-card)
                                 (event? target-card)
                                 (can-play-instant? state :runner (assoc eid :source-type :play)
                                                    target-card {:base-cost [(->c :click 1)]}))))
                :effect (req (play-instant state :runner (assoc eid :source-type :play)
                                           (:card context) {:base-cost [(->c :click 1)]}))}
               {:action true
                :label "Run any server"
                :async true
                :effect (effect (make-run eid (:server context) nil {:click-run true}))}
               {:action true
                :label "Remove 1 tag"
                :cost [(->c :click 1) (->c :credit 2)]
                :msg "remove 1 tag"
                :req (req tagged)
                :async true
                :effect (effect (play-sfx "click-remove-tag")
                                (lose-tags eid 1))}]})
