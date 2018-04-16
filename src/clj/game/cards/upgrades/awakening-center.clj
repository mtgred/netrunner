(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-awakening-center
  {"Awakening Center"
   {:can-host (req (is-type? target "ICE"))
    :abilities [{:label "Host a piece of Bioroid ICE"
                 :cost [:click 1]
                 :prompt "Select a piece of Bioroid ICE to host on Awakening Center"
                 :choices {:req #(and (ice? %)
                                      (has-subtype? % "Bioroid")
                                      (in-hand? %))}
                 :msg "host a piece of Bioroid ICE"
                 :effect (req (corp-install state side target card {:no-install-cost true}))}
                {:req (req (and this-server (= (get-in @state [:run :position]) 0)))
                 :label "Rez a hosted piece of Bioroid ICE"
                 :prompt "Choose a piece of Bioroid ICE to rez" :choices (req (:hosted card))
                 :msg (msg "lower the rez cost of " (:title target) " by 7 [Credits] and force the Runner to encounter it")
                 :effect (effect (rez-cost-bonus -7) (rez target)
                                 (update! (dissoc (get-card state target) :facedown))
                                 (register-events {:run-ends
                                                    {:effect (req (doseq [c (:hosted card)]
                                                                    (when (:rezzed c)
                                                                      (trash state side c)))
                                                                  (unregister-events state side card))}} card))}]
    :events {:run-ends nil}}})