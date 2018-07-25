(in-ns 'game.cards.assets)

(def card-definition-shi-kyu
  {"Shi.Kyū"
   {:access
    {:async true
     :req (req (not= (first (:zone card)) :deck))
     :effect (effect (show-wait-prompt :runner "Corp to use Shi.Kyū")
                     (continue-ability
                       {:optional
                        {:prompt "Pay [Credits] to use Shi.Kyū?"
                         :yes-ability
                         {:prompt "How many [Credits] for Shi.Kyū?"
                          :choices :credit
                          :msg (msg "attempt to do " target " net damage")
                          :async true
                          :effect (req (let [dmg target]
                                         (clear-wait-prompt state :runner)
                                         (continue-ability
                                           state :corp
                                           {:player :runner
                                            :prompt (str "Take " dmg " net damage or add Shi.Kyū to your score area as an agenda worth -1 agenda point?")
                                            :choices [(str "Take " dmg " net damage") "Add Shi.Kyū to score area"]
                                            :async true
                                            :effect (req (if (= target "Add Shi.Kyū to score area")
                                                           (do (system-msg state :runner (str "adds Shi.Kyū to their score area as as an agenda worth -1 agenda point"))
                                                               (trigger-event state side :no-trash card)
                                                               (as-trashed-agenda state :runner eid card -1 {:force true}))
                                                           (do (damage state :corp eid :net dmg {:card card})
                                                               (system-msg state :runner (str "takes " dmg " net damage from Shi.Kyū")))))}
                                           card targets)))}
                         :no-ability {:effect (effect (clear-wait-prompt :runner))}}}
                       card targets))}}})
