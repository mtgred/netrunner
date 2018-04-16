(in-ns 'game.core)

(declare close-access-prompt genetics-trigger? shard-constructor)

(def card-resources-counter-surveillance
  {"Counter Surveillance"
   {:implementation "Does not prevent access of cards installed in the root of a server"
    :abilities [{:cost [:click 1]
                 :makes-run true
                 :prompt "Choose a server to run with Counter Surveillance"
                 :msg (msg "run " target " and trashes Counter Surveillance")
                 :choices (req (cancellable runnable-servers))
                 :effect (req (trash state side card {:cause :ability-cost})
                              (game.core/run state side target nil card)
                              (register-events state side
                                               {:successful-run
                                                {:silent (req true)
                                                 :effect (req (if (>= (:credit runner) (:tag runner))
                                                                ;; Can pay, do access
                                                                (do (system-msg state side (str "uses Counter Surveillance to access up to "
                                                                                                (:tag runner) " cards by paying "
                                                                                                (:tag runner) " [Credit]"))
                                                                    (pay state side card :credit (:tag runner))
                                                                    (access-bonus state side (- (:tag runner) 1)))
                                                                ;; Can't pay, don't access cards
                                                                (do (system-msg state side "could not afford to use Counter Surveillance")
                                                                    ;; Cannot access any cards
                                                                    (max-access state side 0))))}
                                                :run-ends {:effect (effect (unregister-events card))}}
                                               (assoc card :zone '(:discard))))}]
    :events {:successful-run nil :run-ends nil}}})