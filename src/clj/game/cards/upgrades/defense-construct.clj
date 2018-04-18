(in-ns 'game.core)

(def card-definitions-upgrades-defense-construct
  {"Defense Construct"
   {:advanceable :always
    :abilities [{:label "[Trash]: Add 1 facedown card from Archives to HQ for each advancement token"
                 :req (req (and run (= (:server run) [:archives])
                                (pos? (get-in card [:advance-counter] 0))))
                 :effect (effect (resolve-ability
                                   {:show-discard true
                                    :choices {:max (get-in card [:advance-counter] 0)
                                              :req #(and (= (:side %) "Corp")
                                                         (not (:seen %))
                                                         (= (:zone %) [:discard]))}
                                              :msg (msg "add " (count targets) " facedown cards in Archives to HQ")
                                    :effect (req (doseq [c targets]
                                                   (move state side c :hand)))}
                                  card nil)
                                 (trash card))}]}})
