(in-ns 'game.cards.resources)

(def card-definition-clan-vengeance
  {"Clan Vengeance"
   {:events {:pre-resolve-damage {:req (req (pos? (last targets)))
                                  :effect (effect (add-counter card :power 1)
                                                  (system-msg :runner (str "places 1 power counter on Clan Vengeance")))}}
    :abilities [{:label "[Trash]: Trash 1 random card from HQ for each power counter"
                 :req (req (pos? (get-counters card :power)))
                 :msg (msg "trash " (min (get-counters card :power) (count (:hand corp))) " cards from HQ")
                 :effect (effect (trash-cards (take (min (get-counters card :power) (count (:hand corp)))
                                              (shuffle (:hand corp))))
                                 (trash card {:cause :ability-cost}))}]}})
