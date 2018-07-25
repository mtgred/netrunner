(in-ns 'game.cards.programs)

(def card-definition-false-echo
  {"False Echo"
   {:abilities [{:req (req (and run
                                (< (:position run) (count run-ices))
                                (not (rezzed? current-ice))))
                 :msg "make the Corp rez the passed ICE or add it to HQ"
                 :effect (req (let [s (:server run)
                                    ice (nth (get-in @state (vec (concat [:corp :servers] s [:ices]))) (:position run))
                                    icename (:title ice)
                                    icecost (rez-cost state side ice)]
                                (continue-ability
                                  state side
                                  {:prompt (msg "Rez " icename " or add it to HQ?") :player :corp
                                   :choices (req (if (< (:credit corp) icecost)
                                                     ["Add to HQ"]
                                                     ["Rez" "Add to HQ"]))
                                   :effect (req (if (= target "Rez")
                                                  (rez state side ice)
                                                  (do (move state :corp ice :hand nil)
                                                      (system-msg state :corp (str "chooses to add the passed ICE to HQ"))))
                                                (trash state side card))}
                                 card nil)))}]}})
