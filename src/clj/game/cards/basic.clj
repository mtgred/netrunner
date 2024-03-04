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
   [game.core.payment :refer [build-cost-string can-pay? merge-costs]]
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
  {:abilities [{:label "Gain 1 [Credits]"
                :cost [:click]
                :msg "gain 1 [Credits]"
                :async true
                :effect (req (wait-for (gain-credits state side 1 :corp-click-credit)
                                       (swap! state update-in [:stats side :click :credit] (fnil inc 0))
                                       (trigger-event state side :corp-click-credit)
                                       (play-sfx state side "click-credit")
                                       (effect-completed state side eid)))}
               {:label "Draw 1 card"
                :req (req (not-empty (:deck corp)))
                :cost [:click]
                :msg "draw 1 card"
                :async true
                :effect (req (trigger-event state side :corp-click-draw (-> @state side :deck (nth 0)))
                             (swap! state update-in [:stats side :click :draw] (fnil inc 0))
                             (play-sfx state side "click-card")
                             (draw state side eid 1))}
               {:label "Install 1 agenda, asset, upgrade, or piece of ice from HQ"
                :async true
                :req (req (and (not-empty (:hand corp))
                               (in-hand? target)
                               (or (agenda? target)
                                   (asset? target)
                                   (ice? target)
                                   (upgrade? target))
                               (if-let [server (second targets)]
                                 (corp-can-pay-and-install?
                                   state side (assoc eid :source server :source-type :corp-install)
                                   target server {:base-cost [:click 1]
                                                  :action :corp-click-install
                                                  :no-toast true})
                                 (some
                                   (fn [server]
                                     (corp-can-pay-and-install?
                                       state side (assoc eid :source server :source-type :corp-install)
                                       target server {:base-cost [:click 1]
                                                      :action :corp-click-install
                                                      :no-toast true}))
                                   (installable-servers state card)))))
                :effect (req (let [server (second targets)]
                               (corp-install
                                 state side (assoc eid :source server :source-type :corp-install)
                                 target server {:base-cost [:click 1]
                                                :action :corp-click-install})))}
               {:label "Play 1 operation"
                :async true
                :req (req (and (not-empty (:hand corp))
                               (in-hand? target)
                               (operation? target)
                               (can-play-instant? state :corp (assoc eid :source :action :source-type :play)
                                                  target {:base-cost [:click 1]})))
                :effect (req (play-instant state :corp (assoc eid :source :action :source-type :play)
                                           target {:base-cost [:click 1]}))}
               {:label "Advance 1 installed card"
                :cost [:click 1 :credit 1]
                :async true
                :msg (msg "advance " (card-str state target))
                :req (req (can-advance? state side target))
                :effect (effect (update-advancement-requirement target)
                                (add-prop (get-card state target) :advance-counter 1)
                                (play-sfx "click-advance")
                                (effect-completed eid))}
               {:label "Trash 1 resource if the Runner is tagged"
                :cost [:click 1 :credit 2]
                :async true
                :req (req tagged)
                :prompt "Choose a resource to trash"
                :msg (msg "trash " (:title target))
                ;; I hate that we need to modify the basic action card like this, but I don't think there's any way around it -nbkelly, '24
                :choices {:req (req (and (if (and (seq (filter (fn [c] (untrashable-while-resources? c)) (all-active-installed state :runner)))
                                                  (> (count (filter resource? (all-active-installed state :runner))) 1))
                                           (and (resource? target) (not (untrashable-while-resources? target)))
                                           (resource? target))
                                         (let [additional-costs (merge-costs (into [] (concat (get-effects state side :additional-trash-cost target) (get-effects state side :basic-ability-additional-trash-cost target))))
                                               can-pay (can-pay? state side (make-eid state (assoc eid :additional-costs additional-costs)) target (:title target) additional-costs)]
                                           (or (empty? additional-costs) can-pay))))}
                :effect (req
                          (let [additional-costs (merge-costs (into [] (get-effects state side :basic-ability-additional-trash-cost target)))
                                cost-strs (build-cost-string additional-costs)
                                can-pay (can-pay? state side (make-eid state (assoc eid :additional-costs additional-costs)) target (:title target) additional-costs)]
                            (if (empty? additional-costs)
                              (trash state side eid target nil)
                              (let [target-card target]
                                (wait-for (resolve-ability
                                            state side
                                            {:prompt (str "Pay the additional cost to trash " (:title target-card) "?")
                                             :choices [(when can-pay cost-strs) "No"]
                                             :async true
                                             :effect (req (if (= target "No")
                                                            (do (system-msg state side (str "declines to pay the additional cost to trash " (:title target-card)))
                                                                (effect-completed state side eid))
                                                            (wait-for (pay state side (make-eid state
                                                                                                (assoc eid :additional-costs additional-costs :source card :source-type :trash-card))
                                                                           nil additional-costs 0)
                                                                      (system-msg state side (str (:msg async-result) " as an additional cost to trash " (:title target-card)))
                                                                      (complete-with-result state side eid target-card))))}
                                            card nil)
                                          (if async-result
                                            (trash state side eid target nil)
                                            (effect-completed state side eid)))))))}
               {:label "Purge virus counters"
                :cost [:click 3]
                :msg "purge all virus counters"
                :async true
                :effect (req (play-sfx state side "virus-purge")
                             (purge state side eid))}]})

(defcard "Runner Basic Action Card"
  {:abilities [{:label "Gain 1 [Credits]"
                :cost [:click]
                :msg "gain 1 [Credits]"
                :async true
                :effect (req (wait-for (gain-credits state side 1 :runner-click-credit)
                                       (swap! state update-in [:stats side :click :credit] (fnil inc 0))
                                       (trigger-event state side :runner-click-credit)
                                       (play-sfx state side "click-credit")
                                       (effect-completed state side eid)))}
               {:label "Draw 1 card"
                :req (req (not-empty (:deck runner)))
                :cost [:click]
                :msg "draw 1 card"
                :effect (req (trigger-event state side :runner-click-draw (-> @state side :deck (nth 0)))
                             (swap! state update-in [:stats side :click :draw] (fnil inc 0))
                             (play-sfx state side "click-card")
                             (draw state side eid (+ 1 (use-bonus-click-draws! state))))}
               {:label "Install 1 program, resource, or piece of hardware from the grip"
                :async true
                :req (req (and (not-empty (:hand runner))
                               (in-hand? target)
                               (or (hardware? target)
                                   (program? target)
                                   (resource? target))
                               (runner-can-pay-and-install?
                                 state :runner (assoc eid :source :action :source-type :runner-install)
                                 target {:base-cost [:click 1]})))
                :effect (req (runner-install
                               state :runner (assoc eid :source :action :source-type :runner-install)
                               target {:base-cost [:click 1]
                                       :no-toast true}))}
               {:label "Play 1 event"
                :async true
                :req (req (and (not-empty (:hand runner))
                               (in-hand? target)
                               (event? target)
                               (can-play-instant? state :runner (assoc eid :source :action :source-type :play)
                                                  target {:base-cost [:click 1]})))
                :effect (req (play-instant state :runner (assoc eid :source :action :source-type :play)
                                           target {:base-cost [:click 1]}))}
               {:label "Run any server"
                :async true
                :effect (effect (make-run eid target nil {:click-run true}))}
               {:label "Remove 1 tag"
                :cost [:click 1 :credit 2]
                :msg "remove 1 tag"
                :req (req tagged)
                :async true
                :effect (effect (play-sfx "click-remove-tag")
                                (lose-tags eid 1))}]})
