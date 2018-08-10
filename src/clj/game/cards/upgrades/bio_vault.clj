(in-ns 'game.cards.upgrades)

(def card-definition-bio-vault
  {"Bio Vault"
   {:install-req (req (remove #{"HQ" "R&D" "Archives"} targets))
    :advanceable :always
    :abilities [{:label "[Trash]: End the run"
                 :advance-counter-cost 2
                 :req (req (:run @state))
                 :msg "end the run. Bio Vault is trashed"
                 :async true
                 :effect (effect
                           (end-run)
                           (trash eid card {:cause :ability-cost}))}]}})
