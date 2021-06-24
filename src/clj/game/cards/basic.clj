(ns game.cards.basic
  (:require [game.core :refer :all]
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
                :effect (req (wait-for (trigger-event-simult state side (make-eid state eid) :pre-corp-click-draw nil nil)
                                       (trigger-event state side :corp-click-draw (->> @state side :deck (take 1)))
                                       (swap! state update-in [:stats side :click :draw] (fnil inc 0))
                                       (play-sfx state side "click-card")
                                       (draw state side eid 1 nil)))}
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
                :choices {:req (req (if (and (seq (filter (fn [c] (untrashable-while-resources? c)) (all-active-installed state :runner)))
                                             (> (count (filter resource? (all-active-installed state :runner))) 1))
                                      (and (resource? target) (not (untrashable-while-resources? target)))
                                      (resource? target)))}
                :effect (effect (trash eid target nil))}
               {:label "Purge virus counters"
                :cost [:click 3]
                :msg "purge all virus counters"
                :effect (effect (purge)
                                (play-sfx "virus-purge"))}]})

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
                :effect (req (wait-for (trigger-event-simult state side (make-eid state eid) :pre-runner-click-draw nil nil)
                                       (trigger-event state side :runner-click-draw (->> @state side :deck (take 1)))
                                       (swap! state update-in [:stats side :click :draw] (fnil inc 0))
                                       (play-sfx state side "click-card")
                                       (draw state side eid 1 nil)))}
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
