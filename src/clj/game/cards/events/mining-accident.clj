(in-ns 'game.core)

(declare run-event)

(def card-events-mining-accident
  {"Mining Accident"
   (letfn [(mining [] {:player :corp
                       :delayed-completion true
                       :prompt "Pay 5 [Credits] or take 1 Bad Publicity?"
                       :choices ["Pay 5 [Credits]" "Take 1 Bad Publicity"]
                       :effect (req (cond

                                      (and (= target "Pay 5 [Credits]") (can-pay? state :corp nil :credit 5))
                                      (do (lose state :corp :credit 5)
                                          (system-msg state side "pays 5 [Credits] from Mining Accident")
                                          (clear-wait-prompt state :runner)
                                          (effect-completed state side eid))

                                      (= target "Pay 5 [Credits]")
                                      (do (can-pay? state :corp "Mining Accident" :credit 5)
                                          (continue-ability state side (mining) card nil))

                                      (= target "Take 1 Bad Publicity")
                                      (do (gain-bad-publicity state :corp 1)
                                          (system-msg state side "takes 1 bad publicity from Mining Accident")
                                          (clear-wait-prompt state :runner)
                                          (effect-completed state side eid))))})]
   {:req (req (some #{:hq :rd :archives} (:successful-run runner-reg)))
    :delayed-completion true
    :effect (req (move state side (first (:play-area runner)) :rfg)
                 (show-wait-prompt state :runner "Corp to choose to pay or take bad publicity")
                 (continue-ability state side (mining) card nil))
    :msg "make the Corp pay 5 [Credits] or take 1 bad publicity"})})