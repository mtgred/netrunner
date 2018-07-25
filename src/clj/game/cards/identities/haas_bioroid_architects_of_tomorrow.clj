(in-ns 'game.cards.identities)

(def card-definition-haas-bioroid-architects-of-tomorrow
  {"Haas-Bioroid: Architects of Tomorrow"
   {:events {:pass-ice
             {:async true
              :once :per-turn
              :req (req (and (rezzed? target)
                             (has-subtype? target "Bioroid")
                             (empty? (filter #(and (rezzed? %) (has-subtype? % "Bioroid"))
                                             (turn-events state side :pass-ice)))))
              :effect (effect (show-wait-prompt :runner "Corp to use Haas-Bioroid: Architects of Tomorrow")
                              (continue-ability
                                {:prompt "Select a Bioroid to rez" :player :corp
                                 :choices {:req #(and (has-subtype? % "Bioroid") (not (rezzed? %)))}
                                 :msg (msg "rez " (:title target))
                                 :cancel-effect (effect (clear-wait-prompt :runner))
                                 :effect (effect (rez-cost-bonus -4)
                                                 (rez target)
                                                 (clear-wait-prompt :runner))}
                               card nil))}}}})
