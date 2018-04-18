(in-ns 'game.core)

(def card-definitions-icebreakers-cyber-cypher
  {"Cyber-Cypher"
   (auto-icebreaker ["Code Gate"]
                    {:prompt "Choose a server where this copy of Cyber-Cypher can be used:"
                     :msg (msg "target " target)
                     :choices (req servers)
                     :effect (effect (update! (assoc card :server-target target)))
                     :leave-play (effect (update! (dissoc card :server-target)))
                     :abilities [(break-sub 1 1 "Code Gate")
                                 (strength-pump 1 1)]})})
