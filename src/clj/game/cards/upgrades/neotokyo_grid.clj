(in-ns 'game.cards.upgrades)

(def card-definition-neotokyo-grid
  {"NeoTokyo Grid"
   (let [ng {:req (req (in-same-server? card target))
             :once :per-turn
             :msg "gain 1 [Credits]"
             :effect (effect (gain-credits 1))}]
     {:events {:advance ng
               :advancement-placed ng}})})
