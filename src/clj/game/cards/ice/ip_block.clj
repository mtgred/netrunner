(in-ns 'game.cards.ice)

(def card-definition-ip-block
  {"IP Block"
   {:abilities [(assoc (give-tags 1)
                  :req (req (seq (filter #(has-subtype? % "AI") (all-active-installed state :runner))))
                  :label "Give the Runner 1 tag if there is an installed AI")]
    :subroutines [(tag-trace 3)
                  end-the-run-if-tagged]}})
