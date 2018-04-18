(in-ns 'game.core)

(def card-definitions-events-rigged-results
  {"Rigged Results"
   (letfn [(choose-ice []
             {:prompt "Select a piece of ICE to bypass"
              :choices {:req #(ice? %)}
              :msg (msg "bypass " (card-str state target))
              :effect (final-effect (run (second (:zone target))))})
           (corp-choice [spent]
             {:prompt "Guess how many credits were spent"
              :choices ["0" "1" "2"]
              :delayed-completion true
              :effect (req (system-msg state :runner (str "spends " spent "[Credit]. "
                                       (-> corp :user :username) " guesses " target "[Credit]"))
                           (clear-wait-prompt state :runner)
                           (lose state :runner :credit spent)
                           (if (not= (str spent) target)
                             (continue-ability state :runner (choose-ice) card nil)
                             (effect-completed state side eid)))})
           (runner-choice [cr]
             {:prompt "Spend how many credits?"
              :choices (take cr ["0" "1" "2"])
              :delayed-completion true
              :effect (effect (show-wait-prompt :runner "Corp to guess")
                              (clear-wait-prompt :corp)
                              (continue-ability :corp (corp-choice (str->int target)) card nil))})]
   {:effect (effect (show-wait-prompt :corp "Runner to spend credits")
                    (continue-ability (runner-choice (inc (min 2 (:credit runner)))) card nil))})})
