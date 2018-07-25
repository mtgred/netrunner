(in-ns 'game.cards.ice)

(def card-definition-hydra
  {"Hydra"
   (letfn [(otherwise-tag [message ability]
             {:msg (msg (if (is-tagged? @state) message "give the Runner 1 tag"))
              :label (str (capitalize message) " if the Runner is tagged; otherwise, give the Runner 1 tag")
              :async true
              :effect (req (if (is-tagged? @state)
                             (ability state :runner eid card nil)
                             (gain-tags state :runner eid 1)))})]
     {:subroutines [(otherwise-tag "do 3 net damage"
                                   (req (damage state :runner :net 3 {:card card})))
                    (otherwise-tag "gain 5 [Credits]"
                                   (req (gain-credits state :corp 5)
                                        (effect-completed state side eid)))
                    (otherwise-tag "end the run"
                                   (req (end-run state side eid)))]})})
