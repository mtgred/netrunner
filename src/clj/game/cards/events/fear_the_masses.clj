(in-ns 'game.cards.events)

(def card-definition-fear-the-masses
  {"Fear the Masses"
   {:req (req hq-runnable)
    :effect (effect (run :hq {:req (req (= target :hq))
                              :replace-access
                              {:async true
                               :mandatory true
                               :msg "force the Corp to trash the top card of R&D"
                               :effect (req (mill state :corp)
                                            (let [n (count (filter #(= (:title card) (:title %)) (:hand runner)))]
                                              (if (pos? n)
                                                (continue-ability state side
                                                  {:prompt "Reveal how many copies of Fear the Masses?"
                                                   :choices {:number (req n)}
                                                   :effect (req (when (pos? target)
                                                                  (mill state :corp target)
                                                                  (system-msg state side
                                                                              (str "reveals " target " copies of Fear the Masses,"
                                                                                   " forcing the Corp to trash " target " cards"
                                                                                   " from the top of R&D"))))}
                                                 card nil)
                                                (effect-completed state side eid))))}}
                         card))}})
