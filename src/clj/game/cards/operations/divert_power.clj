(in-ns 'game.cards.operations)

(def card-definition-divert-power
  {"Divert Power"
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
                     card nil)))}})
