(in-ns 'game.core)

(def card-definitions-ice-searchlight
  {"Searchlight"
   {:advanceable :always
    ;; Could replace this with (tag-trace advance-counters).
    :subroutines [{:label "Trace X - Give the Runner 1 tag"
                   :trace {:base advance-counters
                           :delayed-completion true
                           :effect (effect (tag-runner :runner eid 1))
                           :msg "give the Runner 1 tag"}}]}})
