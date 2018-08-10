(in-ns 'game.cards.icebreakers)

(def card-definition-peregrine
  {"Peregrine"
   (auto-icebreaker ["Code Gate"]
                    {:abilities [(break-sub 1 1 "Code Gate")
                                 (strength-pump 3 3)
                                 {:label "Derez a Code Gate and return Peregrine to your Grip"
                                  :cost [:credit 2]
                                  :req (req (and (rezzed? current-ice) (has-subtype? current-ice "Code Gate")))
                                  :msg (msg "derez " (:title current-ice) " and return Peregrine to their Grip")
                                  :effect (effect (derez current-ice)
                                                  (move card :hand))}]})})
