(in-ns 'game.core)

(def card-definitions-operations-subcontract
  {"Subcontract"
   (letfn [(sc [i sccard]
             {:prompt "Select an operation in HQ to play"
              :choices {:req #(and (= (:side %) "Corp")
                                   (is-type? % "Operation")
                                   (in-hand? %))}
              :delayed-completion true
              :msg (msg "play " (:title target))
              :effect (req (when-completed (play-instant state side target)
                                           (if (and (not (get-in @state [:corp :register :terminal])) (< i 2))
                                               (continue-ability state side (sc (inc i) sccard) sccard nil)
                                               (effect-completed state side eid))))})]
     {:req (req tagged)
      :delayed-completion true
      :effect (effect (continue-ability (sc 1 card) card nil))})})
