(in-ns 'game.core)

(def card-operations-sacrifice
  {"Sacrifice"
   {:req (req (pos? (:bad-publicity corp)))
    :delayed-completion true
    :additional-cost [:forfeit]
    :effect (effect (register-events (:events (card-def card))
                                     (assoc card :zone '(:discard))))
    :events {:corp-forfeit-agenda {:effect (req (let [bplost (min (:agendapoints (last (:rfg corp))) (:bad-publicity corp))]
                                                  (if (not (neg? bplost)) (do (lose state side :bad-publicity bplost)
                                                                              (gain state side :credit bplost)
                                                                              (system-msg state side (str "uses Sacrifice to lose " bplost " bad publicity and gain " bplost " [Credits]")))
                                                                          (system-msg state side "uses Sacrifice but gains no credits and loses no bad publicity"))
                                                  (effect-completed state side eid)
                                                  (unregister-events state side card)))}}}
   "Salems Hospitality"
   {:prompt "Name a Runner card"
    :choices {:card-title (req (and (card-is? target :side "Runner")
                                    (not (card-is? target :type "Identity"))))}
    :effect (req (system-msg state side
                             (str "uses Salem's Hospitality to reveal the Runner's Grip ( "
                                  (join ", " (map :title (sort-by :title (:hand runner))))
                                  " ) and trash any copies of " target))
                 (doseq [c (filter #(= target (:title %)) (:hand runner))]
                   (trash state side c {:unpreventable true})))}})