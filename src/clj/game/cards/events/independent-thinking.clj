(in-ns 'game.core)

(declare run-event)

(def card-events-independent-thinking
  {"Independent Thinking"
   (letfn [(cards-to-draw [targets]
             (* (count targets)
                (if (some #(and (not (facedown? %)) (has-subtype? % "Directive")) targets) 2 1)))]
     {:delayed-completion true
      :prompt "Choose up to 5 installed cards to trash with Independent Thinking"
      :choices {:max 5
                :req #(and (installed? %)
                           (= (:side %) "Runner"))}
      :effect (req (when-completed (trash-cards state side targets nil)
                                   (draw state :runner (cards-to-draw targets))))
      :msg (msg "trash " (join ", " (map :title targets)) " and draw " (quantify (cards-to-draw targets) "card"))})})