(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-thomas-haas
  {"Thomas Haas"
   {:advanceable :always
    :abilities [{:label "Gain credits" :msg (msg "gain " (* 2 (get card :advance-counter 0)) " [Credits]")
                 :effect (effect (gain :credit (* 2 (get card :advance-counter 0))) (trash card))}]}})