(in-ns 'game.cards.upgrades)

(def card-definition-bernice-mai
  {"Bernice Mai"
   {:events {:successful-run {:interactive (req true)
                              :req (req this-server)
                              :trace {:base 5
                                      :successful {:msg "give the Runner 1 tag"
                                                   :async true
                                                   :effect (effect (gain-tags :corp eid 1))}
                                      :unsuccessful
                                      {:effect (effect (system-msg "trashes Bernice Mai from the unsuccessful trace")
                                                       (trash card))}}}}}})
