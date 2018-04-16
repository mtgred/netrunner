(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-neotokyo-grid
  {"NeoTokyo Grid"
   (let [ng {:req (req (in-same-server? card target))
             :once :per-turn
             :msg "gain 1 [Credits]" :effect (effect (gain :credit 1))}]
     {:events {:advance ng :advancement-placed ng}})})