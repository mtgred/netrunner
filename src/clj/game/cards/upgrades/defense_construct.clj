(in-ns 'game.cards.upgrades)

(def card-definition-defense-construct
  {"Defense Construct"
   {:advanceable :always
    :abilities [{:label "[Trash]: Add 1 facedown card from Archives to HQ for each advancement token"
                 :req (req (and run (= (:server run) [:archives])
                                (pos? (get-counters card :advancement))))
                 :effect (effect (resolve-ability
                                   {:show-discard true
                                    :choices {:max (get-counters card :advancement)
                                              :req #(and (= (:side %) "Corp")
                                                         (not (:seen %))
                                                         (= (:zone %) [:discard]))}
                                              :msg (msg "add " (count targets) " facedown cards in Archives to HQ")
                                    :effect (req (doseq [c targets]
                                                   (move state side c :hand)))}
                                  card nil)
                                 (trash card))}]}})
