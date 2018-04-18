(in-ns 'game.core)

(def card-definitions-resources-councilman
  {"Councilman"
   {:implementation "Does not restrict Runner to Asset / Upgrade just rezzed"
    :events {:rez {:req (req (and (#{"Asset" "Upgrade"} (:type target))
                                  (can-pay? state :runner nil [:credit (rez-cost state :corp target)])))
                   :effect (req (toast state :runner (str "Click Councilman to derez " (card-str state target {:visible true})
                                                          " that was just rezzed") "info")
                                (toast state :corp (str "Runner has the opportunity to derez with Councilman.") "error"))}}
    :abilities [{:prompt "Select an asset or upgrade that was just rezzed"
                 :choices {:req #(and (rezzed? %)
                                      (or (is-type? % "Asset") (is-type? % "Upgrade")))}
                 :effect (req (let [c target
                                    creds (rez-cost state :corp c)]
                                (when (can-pay? state side nil [:credit creds])
                                  (resolve-ability
                                    state :runner
                                    {:msg (msg "pay " creds " [Credit] and derez " (:title c) ". Councilman is trashed")
                                     :effect (req (lose state :runner :credit creds)
                                                  (derez state :corp c)
                                                  (register-turn-flag!
                                                    state side card :can-rez
                                                    (fn [state side card]
                                                      (if (= (:cid card) (:cid c))
                                                        ((constantly false)
                                                         (toast state :corp "Cannot rez the rest of this turn due to Councilman"))
                                                        true)))
                                                  (trash state side card {:unpreventable true}))}
                                   card nil))))}]}})
