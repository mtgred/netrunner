(in-ns 'game.cards.agendas)

(def card-definition-illicit-sales
  {"Illicit Sales"
   {:async true
    :effect (req (wait-for (resolve-ability
                             state side
                             {:optional
                              {:prompt "Take 1 bad publicity from Illicit Sales?"
                               :yes-ability {:msg "take 1 bad publicity"
                                             :effect (effect (gain-bad-publicity :corp 1))}}}
                             card nil)
                           (do (let [n (* 3 (+ (get-in @state [:corp :bad-publicity]) (:has-bad-pub corp)))]
                                 (gain-credits state side n)
                                 (system-msg state side (str "gains " n " [Credits] from Illicit Sales"))
                                 (effect-completed state side eid)))))}})
