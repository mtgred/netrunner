(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-jamie-bzzz-micken-techno-savant
  {"Jamie \"Bzzz\" Micken: Techno Savant"
   {:events {:pre-start-game {:effect draft-points-target}
             :pre-install {:req (req (and (has-most-faction? state :runner "Shaper")
                                          (pos? (count (:deck runner)))
                                          (first-event? state side :pre-install)))
                           :msg "draw 1 card"
                           :once :per-turn
                           :effect (effect (draw 1))}}}})
