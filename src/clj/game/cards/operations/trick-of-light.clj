(in-ns 'game.core)

(def card-operations-trick-of-light
  {"Trick of Light"
   {:choices {:req #(and (contains? % :advance-counter) (> (:advance-counter %) 0))}
    :delayed-completion true
    :effect (req (let [fr target tol card]
                   (continue-ability
                     state side
                     {:prompt "Move how many advancement tokens?"
                      :choices (take (inc (:advance-counter fr)) ["0" "1" "2"])
                      :delayed-completion true
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