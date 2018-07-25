(in-ns 'game.cards.assets)

(def card-definition-drudge-work
  {"Drudge Work"
   {:effect (effect (add-counter card :power 3))
    :abilities [{:cost [:click 1]
                 :counter-cost [:power 1]
                 :async true
                 :choices {:req #(and (is-type? % "Agenda")
                                      (or (in-hand? %)
                                          (in-discard? %)))}
                 :msg (msg "reveal " (:title target)
                           (let [target-agenda-points (get-agenda-points state :corp target)]
                             (when (pos? target-agenda-points)
                               (str ", gain " target-agenda-points " [Credits], ")))
                           " and shuffle it into R&D")
                 :effect (req (gain-credits state :corp (get-agenda-points state :corp target))
                              (move state :corp target :deck)
                              (shuffle! state :corp :deck)
                              (if (zero? (get-counters card :power))
                                (trash state side eid card nil)
                                (effect-completed state side eid)))}]}})
