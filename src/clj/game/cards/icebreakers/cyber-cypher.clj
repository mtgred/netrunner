(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-cyber-cypher
  {"Cyber-Cypher"
   (auto-icebreaker ["Code Gate"]
                    {:prompt "Choose a server where this copy of Cyber-Cypher can be used:"
                     :msg (msg "target " target)
                     :choices (req servers)
                     :effect (effect (update! (assoc card :server-target target)))
                     :leave-play (effect (update! (dissoc card :server-target)))
                     :abilities [(break-sub 1 1 "Code Gate")
                                 (strength-pump 1 1)]})})
