(in-ns 'game.core)

(declare run-event)

(def card-events-code-siphon
  {"Code Siphon"
   {:req (req rd-runnable)
    :effect (effect (run :rd
                         {:replace-access
                          {:delayed-completion true
                           :prompt "Choose a program to install"
                           :msg (msg "install " (:title target) " and take 1 tag")
                           :choices (req (filter #(is-type? % "Program") (:deck runner)))
                           :effect (effect (trigger-event :searched-stack nil)
                                           (shuffle! :deck)
                                           (install-cost-bonus [:credit (* -3 (count (get-in corp [:servers :rd :ices])))])
                                           (runner-install target)
                                           (tag-runner eid 1) )}} card))}})