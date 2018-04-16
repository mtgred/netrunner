(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-officer-frank
  {"Officer Frank"
   {:abilities [{:cost [:credit 1]
                 :req (req (some #(= :meat %) (map first (turn-events state :runner :damage))))
                 :msg "force the Corp to trash 2 random cards from HQ"
                 :effect (effect (trash-cards :corp (take 2 (shuffle (:hand corp))))
                                 (trash card {:cause :ability-cost}))}]}})
