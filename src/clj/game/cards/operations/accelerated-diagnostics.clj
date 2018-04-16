(in-ns 'game.core)

(def card-operations-accelerated-diagnostics
  {"Accelerated Diagnostics"
   (letfn [(ad [i n adcard]
             {:prompt "Select an operation to play"
              :choices {:req #(and (= (:side %) "Corp")
                                   (is-type? % "Operation")
                                   (= (:zone %) [:play-area]))}
              :msg (msg "play " (:title target))
              :delayed-completion true
              :effect (req (when-completed (play-instant state side target {:no-additional-cost true})
                                           (if (and (not (get-in @state [:corp :register :terminal])) (< i n))
                                             (continue-ability state side (ad (inc i) n adcard) adcard nil)
                                             (effect-completed state side eid))))})]
     {:delayed-completion true
      :implementation "Corp has to manually cards back to R&D to correctly play a draw operation"
      :effect (req (let [n (count (filter #(is-type? % "Operation")
                                          (take 3 (:deck corp))))]
                     (continue-ability state side
                                       {:msg "look at the top 3 cards of R&D"
                                        :delayed-completion true
                                        :effect (req (doseq [c (take 3 (:deck corp))]
                                                       (move state side c :play-area))
                                                     (continue-ability state side (ad 1 n card) card nil))}
                                       card nil)))})})