(in-ns 'game.core)

(def card-operations-back-channels
  {"Back Channels"
   {:prompt "Select an installed card in a server to trash"
    :choices {:req #(and (= (last (:zone %)) :content)
                         (is-remote? (second (:zone %))))}
    :effect (final-effect (gain :credit (* 3 (get target :advance-counter 0))) (trash target))
    :msg (msg "trash " (card-str state target) " and gain "
              (* 3 (get target :advance-counter 0)) " [Credits]")}})