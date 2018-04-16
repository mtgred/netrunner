(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-tyrs-hand
  {"Tyrs Hand"
   {:abilities [{:label "[Trash]: Prevent a subroutine on a piece of Bioroid ICE from being broken"
                 :req (req (and (= (butlast (:zone current-ice)) (butlast (:zone card)))
                                (has-subtype? current-ice "Bioroid")))
                 :effect (effect (trash card))
                 :msg (msg "prevent a subroutine on " (:title current-ice) " from being broken")}]}})
