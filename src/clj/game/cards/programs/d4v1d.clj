(in-ns 'game.cards.programs)

(def card-definition-d4v1d
  {"D4v1d"
   {:implementation "Does not check that ICE strength is 5 or greater"
    :data {:counter {:power 3}}
    :abilities [{:counter-cost [:power 1]
                 :msg "break 1 subroutine"}]}})
