(in-ns 'game.cards.events)

(def card-definition-compile
  {"Compile"
   {:implementation "Trigger only on first encounter not enforced"
    :prompt "Choose a server"
    :msg "make a run and install a program on encounter with the first piece of ICE"
    :choices (req runnable-servers)
    :async true
    :abilities [{:label "Install a program using Compile"
                 :async true
                 :effect (effect (resolve-ability
                                   {:prompt "Install a program from Stack or Heap?"
                                    :choices ["Stack" "Heap"]
                                    :msg (msg "install " (:title target))
                                    :effect (effect (resolve-ability
                                                      (let [chosen-source target]
                                                        {:prompt (str "Choose a program in your " chosen-source " to install")
                                                         :choices (req (cancellable (filter #(is-type? % "Program")
                                                                                            ((if (= chosen-source "Heap") :discard :deck) runner))))
                                                         :effect (req (runner-install state side (assoc-in target [:special :compile-installed] true) {:no-cost true})
                                                                      (when (= chosen-source "Stack")
                                                                        (shuffle! state :runner :deck)))})
                                                      card nil))}
                                   card nil))}]
    :effect (effect (run target nil card)
                    (prompt! card (str "Click Compile in the Temporary Zone to install a Program") ["OK"] {})
                    (resolve-ability
                      {:effect (req (let [c (move state side (last (:discard runner)) :play-area)]
                                         (card-init state side c {:resolve-effect false})))}
                      card nil))
    :events {:run-ends {:effect (req
                                 (let [compile-installed (first (filter #(get-in % [:special :compile-installed]) (game.core/all-installed state :runner)))]
                                   (when (not (empty? compile-installed))
                                     (system-msg state side (str "moved " (:title compile-installed) " to the bottom of the Stack at the end of the run from Compile"))
                                     (move state :runner compile-installed :deck)))
                                 (unregister-events state side card)
                                 (trash state side card))}}}})
