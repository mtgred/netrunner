(in-ns 'game.cards.programs)

(def card-definition-origami
  {"Origami"
   {:effect (effect (gain :hand-size
                          {:mod (dec (* 2 (count (filter #(= (:title %) "Origami")
                                                         (all-active-installed state :runner)))))}))
    :leave-play (effect (lose :hand-size
                              {:mod (dec (* 2 (count (filter #(= (:title %) "Origami")
                                                             (all-active-installed state :runner)))))}))}})
