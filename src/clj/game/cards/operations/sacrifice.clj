(in-ns 'game.cards.operations)

(def card-definition-sacrifice
  {"Sacrifice"
   {:req (req (pos? (:bad-publicity corp)))
    :async true
    :additional-cost [:forfeit]
    :effect (effect (register-events (:events (card-def card))
                                     (assoc card :zone '(:discard))))
    :events {:corp-forfeit-agenda {:effect (req (let [bplost (min (:agendapoints (last (:rfg corp))) (:bad-publicity corp))]
                                                  (if (not (neg? bplost)) (do (lose state side :bad-publicity bplost)
                                                                              (gain-credits state side bplost)
                                                                              (system-msg state side (str "uses Sacrifice to lose " bplost " bad publicity and gain " bplost " [Credits]")))
                                                                          (system-msg state side "uses Sacrifice but gains no credits and loses no bad publicity"))
                                                  (effect-completed state side eid)
                                                  (unregister-events state side card)))}}}})
