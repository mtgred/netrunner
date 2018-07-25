(in-ns 'game.cards.agendas)

(def card-definition-corporate-war
  {"Corporate War"
   {:msg (msg (if (> (:credit corp) 6) "gain 7 [Credits]" "lose all credits"))
    :interactive (req true)
    :effect (req (if (> (:credit corp) 6)
                   (gain-credits state :corp 7) (lose-credits state :corp :all)))}})
