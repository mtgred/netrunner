(in-ns 'game.cards.events)

(def card-definition-queen-s-gambit
  {"Queen's Gambit"
   {:choices ["0", "1", "2", "3"] :prompt "How many advancement tokens?"
    :effect (req (let [c (str->int target)]
                   (resolve-ability
                     state side
                     {:choices {:req #(and (is-remote? (second (:zone %)))
                                           (= (last (:zone %)) :content)
                                           (not (:rezzed %)))}
                      :msg (msg "add " c " advancement tokens on a card and gain " (* 2 c) " [Credits]")
                      :effect (effect (gain-credits (* 2 c))
                                      (add-prop :corp target :advance-counter c {:placed true})
                                      (register-turn-flag! card :can-access
                                                           ;; prevent access of advanced card
                                                           (fn [_ _ card] (not (same-card? target card)))))}
                     card nil)))}})
