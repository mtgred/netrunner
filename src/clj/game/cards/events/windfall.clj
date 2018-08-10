(in-ns 'game.cards.events)

(def card-definition-windfall
  {"Windfall"
   {:effect (effect (shuffle! :deck)
                    (resolve-ability
                      {:effect (req (let [topcard (first (:deck runner))
                                          cost (:cost topcard)]
                                      (trash state side topcard)
                                      (when-not (is-type? topcard "Event")
                                        (gain-credits state side cost))
                                      (system-msg state side
                                                  (str "shuffles their Stack and trashes " (:title topcard)
                                                       (when-not (is-type? topcard "Event")
                                                         (str " to gain " cost " [Credits]"))))))}
                     card nil))}})
