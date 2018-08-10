(in-ns 'game.cards.assets)

(def card-definition-sealed-vault
  {"Sealed Vault"
   {:abilities [{:label "Store any number of [Credits] on Sealed Vault"
                 :cost [:credit 1]
                 :prompt "How many [Credits]?"
                 :choices {:number (req (- (:credit corp) 1))}
                 :msg (msg "store " target " [Credits]")
                 :effect (effect (lose-credits target)
                                 (add-counter card :credit target))}
                {:label "Move any number of [Credits] to your credit pool"
                 :cost [:click 1]
                 :prompt "How many [Credits]?"
                 :choices {:counter :credit}
                 :msg (msg "gain " target " [Credits]")
                 :effect (effect (gain-credits target))}
                {:label "[Trash]: Move any number of [Credits] to your credit pool"
                 :prompt "How many [Credits]?"
                 :choices {:counter :credit}
                 :msg (msg "trash it and gain " target " [Credits]")
                 :effect (effect (trash card {:cause :ability-cost})
                                 (gain-credits target))}]}})
