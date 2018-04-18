(in-ns 'game.core)

(def card-definitions-events-retrieval-run
  {"Retrieval Run"
   {:req (req archives-runnable)
    :effect (effect (run :archives
                      {:req (req (= target :archives))
                       :replace-access
                       {:prompt "Choose a program to install"
                        :msg (msg "install " (:title target))
                        :choices (req (filter #(is-type? % "Program") (:discard runner)))
                        :effect (effect (runner-install target {:no-cost true}))}} card))}})
