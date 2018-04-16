(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-shikyu
  {"Shi.Kyū"
   {:access
    {:delayed-completion true
     :req (req (not= (first (:zone card)) :deck))
     :effect (effect (show-wait-prompt :runner "Corp to use Shi.Kyū")
                     (continue-ability
                       {:optional
                        {:prompt "Pay [Credits] to use Shi.Kyū?"
                         :yes-ability {:prompt "How many [Credits] for Shi.Kyū?" :choices :credit
                                       :msg (msg "attempt to do " target " net damage")
                                       :delayed-completion true
                                       :effect (effect (clear-wait-prompt :runner)
                                                       (continue-ability
                                                         {:player :runner
                                                          :prompt (str "Take " target " net damage or add Shi.Kyū to your score area as an agenda worth -1 agenda point?")
                                                          :choices [(str "Take " target " net damage") "Add Shi.Kyū to score area"]
                                                          :delayed-completion true
                                                          :effect (let [dmg target]
                                                                    (req (if (= target "Add Shi.Kyū to score area")
                                                                           (do (as-trashed-agenda state :runner card -1)
                                                                               (system-msg state :runner (str "adds Shi.Kyū to their score area as as an agenda worth -1 agenda point"))
                                                                               (trigger-event state side :no-trash card)
                                                                               (effect-completed state side eid))
                                                                           (do (damage state :corp eid :net dmg {:card card})
                                                                               (system-msg state :runner (str "takes " dmg " net damage from Shi.Kyū"))
                                                                               (trigger-event state side :no-trash card)))))}
                                                        card targets))}
                         :no-ability {:effect (effect (clear-wait-prompt :runner))}}}
                      card targets))}}})