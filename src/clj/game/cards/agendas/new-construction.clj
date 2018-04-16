(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-new-construction
  {"New Construction"
   {:install-state :face-up
    :events {:advance
             {:optional
              {:req (req (= (:cid card) (:cid target)))
               :prompt "Install a card from HQ in a new remote?"
               :yes-ability {:prompt "Select a card to install"
                             :choices {:req #(and (not (is-type? % "Operation"))
                                                  (not (is-type? % "ICE"))
                                                  (= (:side %) "Corp")
                                                  (in-hand? %))}
                             :msg (msg "install a card from HQ" (when (>= (:advance-counter (get-card state card)) 5)
                                       " and rez it, ignoring all costs"))
                             :effect (req (if (>= (:advance-counter (get-card state card)) 5)
                                            (do (corp-install state side target "New remote"
                                                              {:install-state :rezzed-no-cost})
                                                (trigger-event state side :rez target))
                                            (corp-install state side target "New remote")))}}}}}})
