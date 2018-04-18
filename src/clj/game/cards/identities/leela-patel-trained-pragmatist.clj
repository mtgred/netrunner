(in-ns 'game.core)

(def card-definitions-identities-leela-patel-trained-pragmatist
  {"Leela Patel: Trained Pragmatist"
   (let [leela {:interactive (req true)
                :prompt "Select an unrezzed card to return to HQ"
                :choices {:req #(and (not (rezzed? %)) (installed? %) (card-is? % :side :corp))}
                :msg (msg "add " (card-str state target) " to HQ")
                :effect (final-effect (move :corp target :hand))}]
     {:flags {:slow-hq-access (req true)}
      :events {:agenda-scored leela
               :agenda-stolen leela}})})
