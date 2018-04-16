(in-ns 'game.core)

(def card-operations-best-defense
  {"Best Defense"
   {:delayed-completion true
    :req (req (not-empty (all-installed state :runner)))
    :effect (req (let [t (:tag runner)]
                   (continue-ability state side
                     {:prompt (msg "Choose a Runner card with an install cost of " t " or less to trash")
                      :choices {:req #(and (installed? %)
                                           (<= (:cost %) t))}
                      :msg (msg "trash " (:title target))
                      :effect (effect (trash target))}
                    card nil)))}})
