(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-lustig
  {"Lustig"
   (auto-icebreaker ["Sentry"]
                    {:abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 3 5)
                                 {:label "Bypass Sentry being encountered"
                                  :req (req (has-subtype? current-ice "Sentry"))
                                  :msg (msg "trash it and bypass " (:title current-ice))
                                  :effect (effect (trash card {:cause :ability-cost}))}]})})