(in-ns 'game.core)

(def card-definitions-ice-next-gold
  {"NEXT Gold"
   {:subroutines [{:label "Do 1 net damage for each rezzed NEXT ice"
                   :msg (msg "do " (next-ice-count corp) " net damage")
                   :effect (effect (damage eid :net (next-ice-count corp) {:card card}))}
                  trash-program]}})
