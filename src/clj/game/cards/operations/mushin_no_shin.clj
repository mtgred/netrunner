(in-ns 'game.cards.operations)

(def card-definition-mushin-no-shin
  {"Mushin No Shin"
   {:prompt "Select a card to install from HQ"
    :choices {:req #(and (#{"Asset" "Agenda" "Upgrade"} (:type %))
                         (= (:side %) "Corp")
                         (in-hand? %))}
    :effect (req (corp-install state side (assoc target :advance-counter 3) "New remote")
                 (effect-completed state side eid)
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
                                (>= (get-counters card :advancement) (or (:current-cost card) (:advancementcost card))))
                         ((constantly false) (toast state :corp "Cannot score due to Mushin No Shin." "warning"))
                         true)))))}})
