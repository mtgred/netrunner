(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-apex-invasive-predator
  {"Apex: Invasive Predator"
   (let [ability {:prompt "Select a card to install facedown"
                  :label "Install a card facedown (start of turn)"
                  :once :per-turn
                  :choices {:max 1
                            :req #(and (= (:side %) "Runner")
                                       (in-hand? %))}
                  :req (req (and (pos? (count (:hand runner)))
                                 (:runner-phase-12 @state)))
                  :effect (effect (runner-install target {:facedown true}))}]
     {:events {:runner-turn-begins ability}
      :flags {:runner-phase-12 (req (pos? (count (:hand runner))))}
      :abilities [ability]})})
