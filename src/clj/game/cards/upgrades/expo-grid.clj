(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-expo-grid
  {"Expo Grid"
   (let [ability {:req (req (some #(and (is-type? % "Asset")
                                        (rezzed? %))
                                  (get-in corp (:zone card))))
                  :msg "gain 1 [Credits]"
                  :once :per-turn
                  :label "Gain 1 [Credits] (start of turn)"
                  :effect (effect (gain :credit 1))}]
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :events {:corp-turn-begins ability}
    :abilities [ability]})})