(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-jesminder-sareen-girl-behind-the-curtain
  {"Jesminder Sareen: Girl Behind the Curtain"
   {:events {:pre-tag {:once :per-run
                       :req (req (:run @state))
                       :msg "avoid the first tag during this run"
                       :effect (effect (tag-prevent 1))}}}})