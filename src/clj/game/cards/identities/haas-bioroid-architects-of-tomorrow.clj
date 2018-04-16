(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-haas-bioroid-architects-of-tomorrow
  {"Haas-Bioroid: Architects of Tomorrow"
   {:events {:pass-ice
             {:delayed-completion true
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
                                 :cancel-effect (final-effect (clear-wait-prompt :runner))
                                 :effect (effect (rez-cost-bonus -4)
                                                 (rez target)
                                                 (clear-wait-prompt :runner))}
                               card nil))}}}})