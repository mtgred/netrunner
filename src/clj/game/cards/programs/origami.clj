(in-ns 'game.core)

(declare can-host?)

(def card-programs-origami
  {"Origami"
   {:effect (effect (gain :hand-size-modification
                          (dec (* 2 (count (filter #(= (:title %) "Origami")
                                                   (all-active-installed state :runner)))))))
    :leave-play (effect (lose :hand-size-modification
                              (dec (* 2 (count (filter #(= (:title %) "Origami")
                                                       (all-active-installed state :runner)))))))}})
