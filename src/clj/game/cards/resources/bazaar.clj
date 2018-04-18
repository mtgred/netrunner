(in-ns 'game.core)

(def card-definitions-resources-bazaar
  {"Bazaar"
   (letfn [(hardware-and-in-hand? [target runner]
             (and (is-type? target "Hardware")
                  (some #(= (:title %) (:title target)) (:hand runner))))]
     {:events
      {:runner-install
       {:interactive (req (hardware-and-in-hand? target runner))
        :silent (req (not (hardware-and-in-hand? target runner)))
        :delayed-completion true
        :req (req (and (is-type? target "Hardware") (= [:hand] (:previous-zone target))))
        :effect (req (let [hw (:title target)]
                       (continue-ability state side
                                         {:optional {:req (req (some #(when (= (:title %) hw) %) (:hand runner)))
                                                     :prompt (msg "Install another copy of " hw "?")
                                                     :msg (msg "install another copy of " hw)
                                                     :yes-ability {:delayed-completion true
                                                                   :effect (req (if-let [c (some #(when (= (:title %) hw) %)
                                                                                                 (:hand runner))]
                                                                                  (runner-install state side eid c nil)))}}} card nil)))}}})})
