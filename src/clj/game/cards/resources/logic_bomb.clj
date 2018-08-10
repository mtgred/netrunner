(in-ns 'game.cards.resources)

(def card-definition-logic-bomb
  {"Logic Bomb"
   {:implementation "Bypass effect is manual"
    :abilities [{:label "Bypass the encountered ice"
                 :req (req (and (:run @state)
                                (rezzed? current-ice)))
                 :msg (msg "bypass "
                           (:title current-ice)
                           (when (pos? (:click runner))
                             (str " and loses "
                                  (apply str (repeat (:click runner) "[Click]")))))
                 :effect (effect (trash card {:cause :ability-cost})
                                 (lose :click (:click runner)))}]}})
