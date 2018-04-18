(in-ns 'game.core)

(def card-definitions-events-push-your-luck
  {"Push Your Luck"
   {:effect (effect (show-wait-prompt :runner "Corp to guess Odd or Even")
                    (resolve-ability
                      {:player :corp :prompt "Guess whether the Runner will spend an Odd or Even number of credits with Push Your Luck"
                       :choices ["Even" "Odd"]
                       :msg "force the Corp to make a guess"
                       :effect (req (let [guess target]
                                      (clear-wait-prompt state :runner)
                                      (resolve-ability
                                        state :runner
                                        {:choices :credit :prompt "How many credits?"
                                         :msg (msg "spend " target " [Credits]. The Corp guessed " guess)
                                         :effect (req (when (or (and (= guess "Even") (odd? target))
                                                                (and (= guess "Odd") (even? target)))
                                                        (system-msg state :runner (str "gains " (* 2 target) " [Credits]"))
                                                        (gain state :runner :credit (* 2 target))))} card nil)))}
                      card nil))}})
