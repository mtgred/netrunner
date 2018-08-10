(in-ns 'game.cards.operations)

(def card-definition-psychokinesis
  {"Psychokinesis"
   (letfn [(choose-card [state cards]
             (let [allowed-cards (filter #(some #{"New remote"} (installable-servers state %))
                                         cards)]
               {:prompt "Select an agenda, asset, or upgrade to install"
                :choices (cons "None" allowed-cards)
                :async true
                :effect (req (if-not (or (= target "None") (ice? target) (is-type? target "Operation"))
                               (continue-ability state side (install-card target) card nil)
                               (system-msg state side "does not install an asset, agenda, or upgrade"))
                             (effect-completed state side eid)
                             (clear-wait-prompt state :runner))}))
           (install-card [chosen]
            {:prompt "Select a remote server"
             :choices (req (conj (vec (get-remote-names state)) "New remote"))
             :async true
             :effect (effect (clear-wait-prompt :runner)
                             (corp-install (move state side chosen :play-area) target))})]
    {:msg "look at the top 5 cards of R&D"
     :async true
     :effect (req (show-wait-prompt state :runner "Corp to look at the top cards of R&D")
                  (let [top-5 (take 5 (:deck corp))]
                    (continue-ability state side (choose-card state top-5) card nil)))})})
