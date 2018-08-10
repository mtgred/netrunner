(in-ns 'game.cards.assets)

(def card-definition-constellation-protocol
  {"Constellation Protocol"
   {:derezzed-events {:runner-turn-ends corp-rez-toast}
    :flags {:corp-phase-12
            (req (let [a-token (->> (all-installed state :corp)
                                    (filter ice?)
                                    (filter #(pos? (get-counters % :advancement)))
                                    (remove empty?)
                                    first
                                    :title)]
                   (as-> (all-installed state :corp) it
                     (filter ice? it)
                     (filter can-be-advanced? it)
                     (remove empty? it)
                     (map :title it)
                     (split-with (partial not= a-token) it)
                     (concat (first it) (-> it rest first rest))
                     (count it)
                     (pos? it))))}
    :abilities [{:label "Move an advancement counter between ICE"
                 :once :per-turn
                 :effect (req (show-wait-prompt state :runner "Corp to use Constellation Protocol")
                              (continue-ability
                                state side
                                {:choices {:req #(and (ice? %)
                                                      (get-counters % :advancement))}
                                 :effect (req (let [from-ice target]
                                                (continue-ability
                                                  state side
                                                  {:prompt "Move to where?"
                                                   :choices {:req #(and (ice? %)
                                                                        (not= (:cid from-ice) (:cid %))
                                                                        (can-be-advanced? %))}
                                                   :effect (effect (add-prop :corp target :advance-counter 1)
                                                                   (add-prop :corp from-ice :advance-counter -1)
                                                                   (system-msg
                                                                     (str "uses Constellation Protocol to move an advancement token from "
                                                                          (card-str state from-ice)
                                                                          " to "
                                                                          (card-str state target))))}
                                                  card nil)))
                                 :end-effect (effect (clear-wait-prompt :runner))}
                                card nil))}]}})
