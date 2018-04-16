(in-ns 'game.core)

(def card-operations-success
  {"Success"
   {:additional-cost [:forfeit]
    :effect (req (resolve-ability state side
                                  {:choices {:req can-be-advanced?}
                                   :msg (msg "advance " (card-str state target) " "
                                             (advancement-cost state side (last (:rfg corp))) " times")
                                   :effect (req (dotimes [_ (advancement-cost state side (last (:rfg corp)))]
                                                  (advance state :corp target :no-cost)))} card nil))}})
