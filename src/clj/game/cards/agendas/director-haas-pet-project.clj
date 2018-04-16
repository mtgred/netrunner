(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-director-haas-pet-project
  {"Director Haas Pet Project"
   (letfn [(install-ability [server-name n]
             {:prompt "Select a card to install"
              :show-discard true
              :choices {:req #(and (= (:side %) "Corp")
                                   (not (is-type? % "Operation"))
                                   (#{[:hand] [:discard]} (:zone %)))}
              :effect (req (corp-install state side target server-name {:no-install-cost true})
                           (if (< n 2)
                             (continue-ability state side
                                               (install-ability (last (get-remote-names @state)) (inc n))
                                               card nil)
                             (effect-completed state side eid card)))
              :msg (msg (if (pos? n)
                          (corp-install-msg target)
                          "create a new remote server, installing cards from HQ or Archives, ignoring all install costs"))})]
     {:optional {:prompt "Install cards in a new remote server?"
                 :yes-ability (install-ability "New remote" 0)}})})
