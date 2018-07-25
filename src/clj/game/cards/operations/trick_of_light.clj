(in-ns 'game.cards.operations)

(def card-definition-trick-of-light
  {"Trick of Light"
   {:choices {:req #(pos? (get-counters % :advancement))}
    :async true
    :effect (req (let [fr target tol card]
                   (continue-ability
                     state side
                     {:prompt "Move how many advancement tokens?"
                      :choices (take (inc (get-counters fr :advancement)) ["0" "1" "2"])
                      :async true
                      :effect (req (let [c (str->int target)]
                                     (continue-ability
                                       state side
                                       {:prompt  "Move to where?"
                                        :choices {:req #(and (not= (:cid fr) (:cid %))
                                                             (can-be-advanced? %))}
                                        :effect  (effect (add-prop :corp target :advance-counter c {:placed true})
                                                         (add-prop :corp fr :advance-counter (- c) {:placed true})
                                                         (system-msg (str "moves " c " advancement tokens from "
                                                                          (card-str state fr) " to " (card-str state target))))}
                                       tol nil)))}
                     card nil)))}})
