(in-ns 'game.core)

(def card-definitions-ice-ip-block
  {"IP Block"
   {:abilities [(assoc give-tag :req (req (not-empty (filter #(has-subtype? % "AI") (all-active-installed state :runner))))
                                :label "Give the Runner 1 tag if there is an installed AI")]
    :subroutines [(tag-trace 3)
                  end-the-run-if-tagged]}})
