(in-ns 'game.cards.resources)

(def card-definition-bazaar
  {"Bazaar"
   (letfn [(hardware-and-in-hand? [target runner]
             (and (is-type? target "Hardware")
                  (some #(= (:title %) (:title target)) (:hand runner))))]
     {:events
      {:runner-install
       {:interactive (req (hardware-and-in-hand? target runner))
        :silent (req (not (hardware-and-in-hand? target runner)))
        :async true
        :req (req (and (is-type? target "Hardware") (= [:hand] (:previous-zone target))))
        :effect (req (let [hw (:title target)]
                       (continue-ability state side
                                         {:optional {:req (req (some #(when (= (:title %) hw) %) (:hand runner)))
                                                     :prompt (msg "Install another copy of " hw "?")
                                                     :msg (msg "install another copy of " hw)
                                                     :yes-ability {:async true
                                                                   :effect (req (if-let [c (some #(when (= (:title %) hw) %)
                                                                                                 (:hand runner))]
                                                                                  (runner-install state side eid c nil)))}}} card nil)))}}})})
