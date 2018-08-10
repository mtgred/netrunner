(in-ns 'game.cards.events)

(def card-definition-code-siphon
  {"Code Siphon"
   {:req (req rd-runnable)
    :effect (effect (run :rd
                         {:replace-access
                          {:async true
                           :prompt "Choose a program to install"
                           :msg (msg "install " (:title target) " and take 1 tag")
                           :choices (req (filter #(is-type? % "Program") (:deck runner)))
                           :effect (effect (trigger-event :searched-stack nil)
                                           (shuffle! :deck)
                                           (install-cost-bonus [:credit (* -3 (count (get-in corp [:servers :rd :ices])))])
                                           (runner-install target)
                                           (gain-tags eid 1))}}
                         card))}})
