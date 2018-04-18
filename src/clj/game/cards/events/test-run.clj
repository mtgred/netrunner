(in-ns 'game.core)

(def card-definitions-events-test-run
  {"Test Run"
   {:prompt "Install a program from your Stack or Heap?"
    :choices (cancellable ["Stack" "Heap"])
    :msg (msg "install a program from their " target)
    :effect (effect (resolve-ability
                      {:prompt "Choose a program to install"
                       :choices (req (cancellable
                                       (filter #(is-type? % "Program")
                                               ((if (= target "Heap") :discard :deck) runner))))
                       :effect (effect (trigger-event :searched-stack nil)
                                       (shuffle! :deck)
                                       (runner-install (assoc-in target [:special :test-run] true) {:no-cost true}))
                       :end-turn
                       {:req (req (get-in (find-cid (:cid target) (all-installed state :runner)) [:special :test-run]))
                        :msg (msg "move " (:title target) " to the top of their Stack")
                        :effect (req (move state side (find-cid (:cid target) (all-installed state :runner))
                                           :deck {:front true}))}}
                      card targets))}})
