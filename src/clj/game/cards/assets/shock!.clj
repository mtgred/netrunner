(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-shock!
  {"Shock!"
   {:flags {:rd-reveal (req true)}
    :access {:msg "do 1 net damage"
             :delayed-completion true
             :effect (effect (damage eid :net 1 {:card card}))}}})