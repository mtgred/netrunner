(in-ns 'game.cards.identities)

(def card-definition-adam-compulsive-hacker
  {"Adam: Compulsive Hacker"
   {:events {:pre-start-game
             {:req (req (= side :runner))
              :async true
              :effect (req (show-wait-prompt state :corp "Runner to choose starting directives")
                           (let [is-directive? #(has-subtype? % "Directive")
                                 directives (filter is-directive? (vals @all-cards))
                                 directives (map make-card directives)
                                 directives (zone :play-area directives)]
                             ;; Add directives to :play-area - assumed to be empty
                             (swap! state assoc-in [:runner :play-area] directives)
                             (continue-ability state side
                                               {:prompt (str "Choose 3 starting directives")
                                                :choices {:max 3
                                                          :all true
                                                          :req #(and (= (:side %) "Runner")
                                                                     (= (:zone %) [:play-area]))}
                                                :effect (req (doseq [c targets]
                                                               (runner-install state side c {:no-cost true
                                                                                             :custom-message (str "starts with " (:title c) " in play")}))
                                                             (swap! state assoc-in [:runner :play-area] [])
                                                             (clear-wait-prompt state :corp))}
                                               card nil)))}}}})
