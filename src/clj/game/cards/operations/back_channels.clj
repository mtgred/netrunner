(in-ns 'game.cards.operations)

(def card-definition-back-channels
  {"Back Channels"
   {:async true
    :prompt "Select an installed card in a server to trash"
    :choices {:req #(and (= (last (:zone %)) :content)
                         (is-remote? (second (:zone %))))}
    :effect (effect (gain-credits (* 3 (get-counters target :advancement)))
                    (trash eid target nil))
    :msg (msg "trash " (card-str state target) " and gain "
              (* 3 (get-counters target :advancement)) " [Credits]")}})
