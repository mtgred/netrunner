(in-ns 'game.cards.agendas)

(def card-definition-remote-enforcement
  {"Remote Enforcement"
   {:interactive (req true)
    :optional {:prompt "Search R&D for a piece of ice to install protecting a remote server?"
               :yes-ability {:async true
                             :prompt "Choose a piece of ice"
                             :choices (req (filter ice? (:deck corp)))
                             :effect (req (let [chosen-ice target]
                                            (continue-ability state side
                                              {:async true
                                               :prompt (str "Select a server to install " (:title chosen-ice) " on")
                                               :choices (filter #(not (#{"HQ" "Archives" "R&D"} %))
                                                                (corp-install-list state chosen-ice))
                                               :effect (effect (shuffle! :deck)
                                                               (corp-install eid chosen-ice target {:install-state :rezzed-no-rez-cost}))}
                                              card nil)))}}}})
