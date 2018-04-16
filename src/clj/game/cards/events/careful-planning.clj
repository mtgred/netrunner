(in-ns 'game.core)

(declare run-event)

(def card-events-careful-planning
  {"Careful Planning"
   {:prompt  "Choose a card in or protecting a remote server"
    :choices {:req #(is-remote? (second (:zone %)))}
    :end-turn {:effect (effect (remove-icon card target))}
    :effect (effect (add-icon card target "CP" "red")
                    (system-msg (str "prevents the rezzing of " (card-str state target)
                                     " for the rest of this turn via Careful Planning"))
                    (register-turn-flag! card :can-rez
                                         (fn [state side card]
                                           (if (= (:cid card) (:cid target))
                                             ((constantly false)
                                               (toast state :corp "Cannot rez the rest of this turn due to Careful Planning"))
                                             true))))}})