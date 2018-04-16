(in-ns 'game.core)

(declare run-event)

(def card-events-eureka!
  {"Eureka!"
   {:effect (req (let [topcard (first (:deck runner))
                       caninst (or (is-type? topcard "Hardware")
                                   (is-type? topcard "Program")
                                   (is-type? topcard "Resource"))]
                   (if caninst
                     (resolve-ability
                       state side
                       {:optional {:prompt (msg "Install " (:title topcard) "?")
                                   :yes-ability {:effect (effect (install-cost-bonus [:credit -10])
                                                                 (runner-install topcard))}
                                   :no-ability {:effect (effect (trash topcard {:unpreventable true})
                                                                (system-msg (str "reveals and trashes "
                                                                                 (:title topcard))))}}} card nil)
                     (do (trash state side topcard {:unpreventable true})
                         (system-msg state side (str "reveals and trashes " (:title topcard)))))))}})
