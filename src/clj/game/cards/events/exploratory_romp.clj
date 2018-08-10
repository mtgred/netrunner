(in-ns 'game.cards.events)

(def card-definition-exploratory-romp
  {"Exploratory Romp"
   (run-event
     {:replace-access {:prompt "Advancements to remove from a card in or protecting this server?"
                       :choices ["0", "1", "2", "3"]
                       :async true
                       :effect (req (let [c (str->int target)]
                                      (show-wait-prompt state :corp "Runner to remove advancements")
                                      (continue-ability state side
                                        {:choices {:req #(and (contains? % :advance-counter)
                                                              (= (first (:server run)) (second (:zone %))))}
                                         :msg (msg "remove " (quantify c "advancement token")
                                                   " from " (card-str state target))
                                         :effect (req (let [to-remove (min c (get-counters target :advancement))]
                                                        (add-prop state :corp target :advance-counter (- to-remove))
                                                        (clear-wait-prompt state :corp)
                                                        (effect-completed state side eid)))}
                                        card nil)))}})})
