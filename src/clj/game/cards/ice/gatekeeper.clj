(in-ns 'game.cards.ice)

(def card-definition-gatekeeper
  {"Gatekeeper"
   (let [draw {:async true
               :prompt "Draw how many cards?"
               :choices {:number (req 3)
                         :max (req 3)
                         :default (req 1)}
               :msg (msg "draw " target "cards")
               :effect (effect (draw eid target nil))}
         reveal-and-shuffle {:prompt "Reveal and shuffle up to 3 agendas"
                             :show-discard true
                             :choices {:req #(and (= "Corp" (:side %))
                                                  (or (= [:discard] (:zone %))
                                                      (= [:hand] (:zone %)))
                                                  (is-type? % "Agenda"))
                                       :max (req 3)}
                             :effect (req (doseq [c targets]
                                            (move state :corp c :deck))
                                          (shuffle! state :corp :deck))
                             :cancel-effect (effect (shuffle! :deck))
                             :msg (msg "add "
                                       (str (join ", " (map :title targets)))
                                       " to R&D")}
         draw-reveal-shuffle {:async true
                              :label "Draw cards, reveal and shuffle agendas"
                              :effect (req (wait-for (resolve-ability state side draw card nil)
                                                     (continue-ability state side reveal-and-shuffle card nil)))}]
    {:strength-bonus (req (if (= :this-turn (:rezzed card)) 6 0))
     :subroutines [draw-reveal-shuffle
                   end-the-run]})})
