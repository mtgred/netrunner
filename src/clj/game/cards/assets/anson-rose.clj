(in-ns 'game.core)

(def card-definitions-assets-anson-rose
  {"Anson Rose"
   (let [ability {:label "Place 1 advancement token on Anson Rose (start of turn)"
                  :once :per-turn
                  :effect (effect (add-prop card :advance-counter 1 {:placed true}))}]
     {:derezzed-events {:runner-turn-ends corp-rez-toast}
      :flags {:corp-phase-12 (req true)}
      :events {:corp-turn-begins ability
               :rez {:req (req (and (ice? target) (pos? (:advance-counter card 0))))
                     :delayed-completion true
                     :effect (req (let [ice (get-card state target)
                                        icename (:title ice)]
                                    (show-wait-prompt state :runner "Corp to use Anson Rose")
                                    (continue-ability state side
                                      {:optional
                                       {:prompt (msg "Move advancement tokens from Anson Rose to " icename "?")
                                        :yes-ability {:prompt "Choose how many advancement tokens to remove from Anson Rose"
                                                      :choices {:number (req (:advance-counter card))}
                                                      :effect (effect (clear-wait-prompt :runner)
                                                                      (add-prop :corp ice :advance-counter target {:placed true})
                                                                      (add-prop :corp card :advance-counter (- target) {:placed true})
                                                                      (system-msg (str "uses Anson Rose to move " target " advancement tokens to "
                                                                                       (card-str state ice))))}
                                        :no-ability {:effect (effect (clear-wait-prompt :runner))}}}
                                     card nil)))}}
      :abilities [ability]})})
