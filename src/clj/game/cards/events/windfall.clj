(in-ns 'game.core)

(declare run-event)

(def card-events-windfall
  {"Windfall"
   {:effect (effect (shuffle! :deck)
                    (resolve-ability
                      {:effect (req (let [topcard (first (:deck runner))
                                          cost (:cost topcard)]
                                      (trash state side topcard)
                                      (when-not (is-type? topcard "Event")
                                        (gain state side :credit cost))
                                      (system-msg state side
                                                  (str "shuffles their Stack and trashes " (:title topcard)
                                                       (when-not (is-type? topcard "Event")
                                                         (str " to gain " cost " [Credits]"))))))}
                     card nil))}})
