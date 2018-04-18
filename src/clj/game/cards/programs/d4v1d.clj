(in-ns 'game.core)

(def card-definitions-programs-d4v1d
  {"D4v1d"
   {:implementation "Does not check that ICE strength is 5 or greater"
    :data {:counter {:power 3}}
    :abilities [{:counter-cost [:power 1]
                 :msg "break 1 subroutine"}]}})
