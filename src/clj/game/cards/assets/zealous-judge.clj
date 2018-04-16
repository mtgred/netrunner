(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-zealous-judge
  {"Zealous Judge"
   {:rez-req (req tagged)
    :abilities [{:delayed-completion true
                 :label "Give the Runner 1 tag"
                 :cost [:click 1 :credit 1]
                 :msg (msg "give the Runner 1 tag")
                 :effect (effect (tag-runner eid 1))}]}})