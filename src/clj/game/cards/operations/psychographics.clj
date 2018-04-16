(in-ns 'game.core)

(def card-operations-psychographics
  {"Psychographics"
   {:req (req tagged)
    :choices :credit
    :prompt "How many credits?"
    :delayed-completion true
    :effect (req (let [c (min target (:tag runner))]
                   (continue-ability state side
                                     {:msg (msg "place " c " advancement tokens on "
                                                (card-str state target))
                                      :choices {:req can-be-advanced?}
                                      :effect (effect (add-prop target :advance-counter c {:placed true}))}
                                     card nil)))}

    "Psychokinesis"
    (letfn [(choose-card [cards]
             {:prompt "Select an agenda, asset, or upgrade to install"
              :choices (cons "None" cards)
              :delayed-completion true
              :effect (req (if-not (or (= target "None") (ice? target) (is-type? target "Operation"))
                             (continue-ability state side (install-card target) card nil)
                             (system-msg state side "does not install an asset, agenda, or upgrade"))
                           (effect-completed state side eid card)
                           (clear-wait-prompt state :runner))})
            (install-card [chosen]
             {:prompt "Select a remote server"
              :choices (req (conj (vec (get-remote-names @state)) "New remote"))
              :delayed-completion true
              :effect (effect (clear-wait-prompt :runner)
                              (corp-install (move state side chosen :play-area) target))})]
     {:msg "look at the top 5 cards of R&D"
      :delayed-completion true
      :effect (req (show-wait-prompt state :runner "Corp to look at the top cards of R&D")
                   (let [from (take 5 (:deck corp))]
                     (continue-ability state side (choose-card from) card nil)))})})