(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-azmari-edtech-shaping-the-future
  {"Azmari EdTech: Shaping the Future"
   (let [choose-type {:prompt "Name a Runner card type"
                      :choices ["Event" "Resource" "Program" "Hardware"]
                      :effect (effect (update! (assoc card :az-target target))
                                      (system-msg (str "uses Azmari EdTech: Shaping the Future to name " target)))}
         check-type {:req (req (is-type? target (:az-target card)))
                     :effect (effect (gain :corp :credit 2))
                     :once :per-turn
                     :msg (msg "gain 2 [Credits] from " (:az-target card))}]
     {:events {:corp-turn-ends choose-type
               :runner-install check-type
               :play-event check-type}})})