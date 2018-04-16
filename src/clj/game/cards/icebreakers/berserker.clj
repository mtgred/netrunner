(in-ns 'game.core)

(declare add-icon remove-icon can-host? breaker-auto-pump auto-icebreaker cloud-icebreaker strength-pump break-sub cerberus break-and-enter global-sec-breaker deva conspiracy central-breaker)

(def card-icebreakers-berserker
  {"Berserker"
   {:abilities [(break-sub 2 2 "Barrier")]
    :implementation "Number of subroutines on encountered ICE has to be entered by runner when Corp chooses 'No More Action'"
    :events {:encounter-ice {:req (req (and (= (:cid target) (:cid current-ice))
                                            (has-subtype? target "Barrier")
                                            (rezzed? target)))
                             :delayed-completion true
                             :effect (effect (continue-ability :runner
                                               {:prompt "How many subroutines are on the encountered Barrier?"
                                                :choices {:number (req 10)}
                                                :delayed-completion true
                                                :effect (effect (system-msg (str "pumps Berserker by " target " on encounter with the current ICE"))
                                                                (pump card target))} card nil))}}}})