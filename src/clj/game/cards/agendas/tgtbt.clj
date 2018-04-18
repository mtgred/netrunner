(in-ns 'game.core)

(def card-definitions-agendas-tgtbt
  {"TGTBT"
   {:flags {:rd-reveal (req true)}
    :access {:msg "give the Runner 1 tag"
             :delayed-completion true
             :effect (effect (tag-runner :runner eid 1))}}})
