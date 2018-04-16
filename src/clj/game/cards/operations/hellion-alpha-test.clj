(in-ns 'game.core)

(def card-operations-hellion-alpha-test
  {"Hellion Alpha Test"
   {:req (req (last-turn? state :runner :installed-resource))
    :trace {:base 2
            :choices {:req #(and (installed? %)
                                 (is-type? % "Resource"))}
            :msg "add a Resource to the top of the Stack"
            :effect (effect (move :runner target :deck {:front true})
                            (system-msg (str "adds " (:title target) " to the top of the Stack")))
            :unsuccessful {:msg "take 1 bad publicity"
                           :effect (effect (gain-bad-publicity :corp 1))}}}})