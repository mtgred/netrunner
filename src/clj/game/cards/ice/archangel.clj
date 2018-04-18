(in-ns 'game.core)

(def card-definitions-ice-archangel
  {"Archangel"
   {:flags {:rd-reveal (req true)}
    :access
    {:delayed-completion true
     :req (req (not= (first (:zone card)) :discard))
     :effect (effect (show-wait-prompt :runner "Corp to decide to trigger Archangel")
                     (continue-ability
                       {:optional
                        {:prompt "Pay 3 [Credits] to force Runner to encounter Archangel?"
                         :yes-ability {:cost [:credit 3]
                                       :delayed-completion true
                                       :effect (effect (system-msg :corp "pays 3 [Credits] to force the Runner to encounter Archangel")
                                                       (clear-wait-prompt :runner)
                                                       (continue-ability
                                                         :runner {:optional
                                                                  {:player :runner
                                                                   :prompt "You are encountering Archangel. Allow its subroutine to fire?"
                                                                   :priority 1
                                                                   :yes-ability {:delayed-completion true
                                                                                 :effect (effect (play-subroutine eid {:card card :subroutine 0}))}
                                                                   :no-ability {:effect (effect (effect-completed eid))}}}
                                                         card nil))}
                         :no-ability {:effect (effect (system-msg :corp "declines to force the Runner to encounter Archangel")
                                                      (clear-wait-prompt :runner))}}}
                       card nil))}
   :subroutines [(trace-ability 6 {:delayed-completion true
                                   :effect (effect (show-wait-prompt :runner "Corp to select Archangel target")
                                                   (continue-ability {:choices {:req #(and (installed? %)
                                                                                           (card-is? % :side :runner))}
                                                                      :label "Add 1 installed card to the Runner's Grip"
                                                                      :msg "add 1 installed card to the Runner's Grip"
                                                                      :effect (effect (clear-wait-prompt :runner)
                                                                                      (move :runner target :hand true)
                                                                                      (system-msg (str "adds " (:title target)
                                                                                                       " to the Runner's Grip")))
                                                                      :cancel-effect (effect (clear-wait-prompt :runner)
                                                                                             (effect-completed eid))}
                                                                     card nil))})]}})
