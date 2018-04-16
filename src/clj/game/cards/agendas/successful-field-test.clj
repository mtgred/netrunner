(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-successful-field-test
  {"Successful Field Test"
   (letfn [(sft [n max] {:prompt "Select a card in HQ to install with Successful Field Test"
                         :priority -1
                         :delayed-completion true
                         :choices {:req #(and (= (:side %) "Corp")
                                              (not (is-type? % "Operation"))
                                              (in-hand? %))}
                         :effect (req (when-completed
                                        (corp-install state side target nil {:no-install-cost true})
                                        (if (< n max)
                                          (continue-ability state side (sft (inc n) max) card nil)
                                          (effect-completed state side eid card))))})]
     {:delayed-completion true
      :msg "install cards from HQ, ignoring all costs"
      :effect (req (let [max (count (filter #(not (is-type? % "Operation")) (:hand corp)))]
                     (continue-ability state side (sft 1 max) card nil)))})})