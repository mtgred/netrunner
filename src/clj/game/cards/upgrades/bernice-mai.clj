(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-bernice-mai
  {"Bernice Mai"
   {:events {:successful-run {:interactive (req true)
                              :req (req this-server)
                              :trace {:base 5 :msg "give the Runner 1 tag"
                                      :delayed-completion true
                                      :effect (effect (tag-runner :runner eid 1))
                                      :unsuccessful {:effect (effect (system-msg "trashes Bernice Mai from the unsuccessful trace")
                                                                     (trash card))}}}}}

  "Bio Vault"
  {:implementation "Installation restriction not enforced"
   :advanceable :always
   :abilities [{:label "[Trash]: End the run"
                :advance-counter-cost 2
                :req (req (:run @state))
                :msg "end the run. Bio Vault is trashed"
                :delayed-completion true
                :effect (effect
                          (end-run)
                          (trash eid card {:cause :ability-cost}))}]}})