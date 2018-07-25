(in-ns 'game.cards.identities)

(def card-definition-jesminder-sareen-girl-behind-the-curtain
  {"Jesminder Sareen: Girl Behind the Curtain"
   {:events {:pre-tag {:once :per-run
                       :req (req (:run @state))
                       :msg "avoid the first tag during this run"
                       :effect (effect (tag-prevent :runner 1))}}}})
