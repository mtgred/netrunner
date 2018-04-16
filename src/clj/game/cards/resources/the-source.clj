(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-the-source
  {"The Source"
   {:effect (effect (update-all-advancement-costs))
    :leave-play (effect (update-all-advancement-costs))
    :events {:agenda-scored {:effect (effect (trash card))}
             :agenda-stolen {:effect (effect (trash card))}
             :pre-advancement-cost {:effect (effect (advancement-cost-bonus 1))}
             :pre-steal-cost {:effect (effect (steal-cost-bonus [:credit 3]))}}}})