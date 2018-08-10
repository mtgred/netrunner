(in-ns 'game.cards.upgrades)

(def card-definition-mwanza-city-grid
  {"Mwanza City Grid"
   (let [gain-creds-and-clear {:req (req (= (:from-server target) (second (:zone card))))
                               :silent (req true)
                               :effect (req (let [cnt (total-cards-accessed run)
                                                  total (* 2 cnt)]
                                              (access-bonus state :runner -3)
                                              (when cnt
                                                (gain-credits state :corp total)
                                                (system-msg state :corp
                                                            (str "gains " total " [Credits] from Mwanza City Grid")))))}
         boost-access-by-3 {:req (req (= target (second (:zone card))))
                            :msg "force the Runner to access 3 additional cards"
                            :effect (req (access-bonus state :runner 3))}]
     {:install-req (req (filter #{"HQ" "R&D"} targets))
      :events {:pre-access boost-access-by-3
               :end-access-phase gain-creds-and-clear}
      ;; TODO: as written, this may fail if mwanza is trashed outside of a run on its server
      ;; (e.g. mwanza on R&D, run HQ, use polop to trash mwanza mid-run, shiro fires to cause RD
      :trash-effect                     ; if there is a run, mark mwanza effects to remain active until the end of the run
      {:req (req (:run @state))
       :effect (effect (register-events {:pre-access (assoc boost-access-by-3 :req (req (= target (second (:previous-zone card)))))
                                         :end-access-phase (assoc gain-creds-and-clear :req (req (= (:from-server target) (second (:previous-zone card)))))
                                         :unsuccessful-run-ends {:effect (effect (unregister-events card))}
                                         :successful-run-ends {:effect (effect (unregister-events card))}}
                                        (assoc card :zone '(:discard))))}})})
