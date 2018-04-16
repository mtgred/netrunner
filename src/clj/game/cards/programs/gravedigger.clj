(in-ns 'game.core)

(declare can-host?)

(def card-programs-gravedigger
  {"Gravedigger"
   {:events (let [e {:req (req (and (installed? target) (= (:side target) "Corp")))
                     :effect (effect (add-counter :runner card :virus 1))}]
              {:runner-trash e :corp-trash e})
    :abilities [{:counter-cost [:virus 1]
                 :cost [:click 1]
                 :msg "force the Corp to trash the top card of R&D"
                 :effect (effect (mill :corp))}]}})
