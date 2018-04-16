(in-ns 'game.core)

(declare can-host?)

(def card-programs-bug
  {"Bug"
   {:implementation "Can only pay to see last card drawn after multiple draws"
    :req (req (some #{:hq} (:successful-run runner-reg)))
    :events {:corp-draw {:optional
                         {:prompt (msg "Pay 2 [Credits] to reveal card just drawn?") :player :runner
                          :yes-ability {:msg (msg "reveal the card just drawn: " (:title (last (:hand corp))))
                                        :cost [:credit 2]}}}}}})
