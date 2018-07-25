(in-ns 'game.cards.identities)

(def card-definition-ayla-bios-rahim-simulant-specialist
  {"Ayla \"Bios\" Rahim: Simulant Specialist"
   {:abilities [{:label "[:click] Add 1 card from NVRAM to your grip"
                 :cost [:click 1]
                 :async true
                 :prompt "Choose a card from NVRAM"
                 :choices (req (cancellable (:hosted card)))
                 :msg "move a card from NVRAM to their Grip"
                 :effect (effect (move target :hand)
                                 (effect-completed eid))}]
    :events {:pre-start-game
             {:req (req (= side :runner))
              :async true
              :effect (req (show-wait-prompt state :corp "the Runner to choose cards for NVRAM")
                           (doseq [c (take 6 (:deck runner))]
                             (move state side c :play-area))
                             (continue-ability state side
                                               {:prompt (str "Select 4 cards for NVRAM")
                                                :async true
                                                :choices {:max 4
                                                          :all true
                                                          :req #(and (= (:side %) "Runner")
                                                                     (= (:zone %) [:play-area]))}
                                                :effect (req (doseq [c targets]
                                                               (host state side (get-card state card) c {:facedown true}))
                                                             (doseq [c (get-in @state [:runner :play-area])]
                                                               (move state side c :deck))
                                                             (shuffle! state side :deck)
                                                             (clear-wait-prompt state :corp)
                                                             (effect-completed state side eid))}
                                               card nil))}}}})
