(in-ns 'game.core)

(def card-definitions-agendas-corporate-war
  {"Corporate War"
   {:msg (msg (if (> (:credit corp) 6) "gain 7 [Credits]" "lose all credits"))
    :interactive (req true)
    :effect (req (if (> (:credit corp) 6)
                   (gain state :corp :credit 7) (lose state :corp :credit :all)))}})
