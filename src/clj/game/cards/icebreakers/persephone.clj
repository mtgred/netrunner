(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-persephone
  {"Persephone"
   (auto-icebreaker ["Sentry"]
                    {:implementation "Requires runner to input the number of subroutines allowed to resolve"
                     :abilities [(break-sub 2 1 "Sentry")
                                 (strength-pump 1 1)]
                     :events {:pass-ice {:req (req (and (has-subtype? target "Sentry") (rezzed? target)) (pos? (count (:deck runner))))
                                         :delayed-completion true
                                         :optional {:prompt (msg "Use Persephone's ability??")
                                                    :yes-ability {:prompt "How many subroutines resolved on the passed ICE?"
                                                                  :delayed-completion true
                                                                  :choices {:number (req 10)}
                                                                  :msg (msg (if (pos? target)
                                                                              (str "trash " (:title (first (:deck runner))) " from their Stack and trash " target " cards from R&D")
                                                                              (str "trash " (:title (first (:deck runner))) " from their Stack and nothing from R&D")))
                                                                  :effect (effect (mill :runner)
                                                                                  (mill :runner :corp target))}}}}})})
