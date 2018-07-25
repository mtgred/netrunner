(in-ns 'game.cards.ice)

(def card-definition-himitsu-bako
  {"Himitsu-Bako"
   {:abilities [{:msg "add it to HQ"
                :cost [:credit 1]
                :effect (effect (move card :hand))}]
    :subroutines [end-the-run]}})
