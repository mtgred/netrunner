(in-ns 'game.core)

(def card-definitions-assets-indian-union-stock-exchange
  {"Indian Union Stock Exchange"
   (let [iuse {:req (req (not= (:faction target) (:faction (:identity corp))))
               :msg "gain 1 [Credits]"
               :effect (effect (gain :credit 1))}]
     {:events {:play-operation iuse :rez iuse}})})
