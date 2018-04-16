(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-clan-vengeance
  {"Clan Vengeance"
   {:events {:pre-resolve-damage {:req (req (pos? (last targets)))
                                  :effect (effect (add-counter card :power 1)
                                                  (system-msg :runner (str "places 1 power counter on Clan Vengeance")))}}
    :abilities [{:label "[Trash]: Trash 1 random card from HQ for each power counter"
                 :req (req (pos? (get-in card [:counter :power] 0)))
                 :msg (msg "trash " (min (get-in card [:counter :power] 0) (count (:hand corp))) " cards from HQ")
                 :effect (effect (trash-cards (take (min (get-in card [:counter :power] 0) (count (:hand corp)))
                                              (shuffle (:hand corp))))
                                 (trash card {:cause :ability-cost}))}]}})