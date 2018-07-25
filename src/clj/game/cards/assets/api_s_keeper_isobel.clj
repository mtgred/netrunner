(in-ns 'game.cards.assets)

(def card-definition-api-s-keeper-isobel
  {"API-S Keeper Isobel"
    (letfn [(counters-available? [state] (some #(pos? (get-counters % :advancement)) (all-installed state :corp)))]
      {:flags {:corp-phase-12 (req (counters-available? state))}
       :abilities [{:req (req (and (:corp-phase-12 @state)
                                   (counters-available? state)))
                    :once :per-turn
                    :label "Remove an advancement token (start of turn)"
                    :prompt "Select a card to remove an advancement token from"
                    :choices {:req #(and (pos? (get-counters % :advancement))
                                         (installed? %))}
                    :effect (req (let [cnt (get-counters target :advancement)]
                                   (set-prop state side target :advance-counter (dec cnt))
                                   (gain-credits state :corp 3)
                                   (system-msg state :corp (str "uses API-S Keeper Isobel to remove an advancement token from "
                                                                (card-str state target) " and gains 3 [Credits]"))))}]})})
