(in-ns 'game.core)

(def card-operations-hellion-beta-test
  {"Hellion Beta Test"
   {:req (req (last-turn? state :runner :trashed-card))
    :trace {:base 2
            :label "Trash 2 installed non-program cards"
            :choices {:max (req (min 2 (count (filter #(or (:facedown %) (not (is-type? % "Program"))) (concat (all-installed state :corp)
                                                                                            (all-installed state :runner))))))
                      :all true
                      :req #(and (installed? %)
                                 (not (is-type? % "Program")))}
            :msg (msg "trash " (join ", " (map :title (sort-by :title targets))))
            :effect (req (doseq [c targets]
                           (trash state side c)))
            :unsuccessful {:msg "take 1 bad publicity"
                           :effect (effect (gain-bad-publicity :corp 1))}}}})