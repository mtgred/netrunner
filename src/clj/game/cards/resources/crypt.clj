(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-crypt
  {"Crypt"
   {:events {:successful-run
             {:silent (req true)
              :req (req (= :archives target))
              :optional {:prompt "Place a virus counter on Crypt?"
                         :yes-ability {:effect (effect (add-counter card :virus 1)
                                                       (system-msg "places a virus counter on Crypt"))}}}}
    :abilities [{:label "[Click][Trash]: install a virus program from the stack"
                 :prompt "Choose a virus"
                 :msg (msg "install " (:title target) " from the stack")
                 :choices (req (cancellable (filter #(and (is-type? % "Program")
                                                          (has-subtype? % "Virus"))
                                                    (:deck runner)) :sorted))
                 :cost [:click 1]
                 :counter-cost [:virus 3]
                 :effect (effect (trigger-event :searched-stack nil)
                                 (shuffle! :deck)
                                 (runner-install target)
                                 (trash card {:cause :ability-cost}))}]}})
