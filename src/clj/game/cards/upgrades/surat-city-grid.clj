(in-ns 'game.core)

(def card-definitions-upgrades-surat-city-grid
  {"Surat City Grid"
   {:events
    {:rez {:req (req (and (same-server? card target)
                          (not (and (is-type? target "Upgrade")
                                    (is-central? (second (:zone target)))))
                          (not= (:cid target) (:cid card))
                          (seq (filter #(and (not (rezzed? %))
                                             (not (is-type? % "Agenda"))) (all-installed state :corp)))))
           :effect (effect (resolve-ability
                             {:optional
                              {:prompt (msg "Rez another card with Surat City Grid?")
                               :yes-ability {:prompt "Select a card to rez"
                                             :choices {:req #(and (not (rezzed? %))
                                                                  (not (is-type? % "Agenda")))}
                                             :msg (msg "rez " (:title target) ", lowering the rez cost by 2 [Credits]")
                                             :effect (effect (rez-cost-bonus -2)
                                                             (rez target))}}}
                            card nil))}}}})
