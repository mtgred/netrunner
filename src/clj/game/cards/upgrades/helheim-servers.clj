(in-ns 'game.core)

(def card-definitions-upgrades-helheim-servers
  {"Helheim Servers"
   {:abilities [{:label "Trash 1 card from HQ: All ice protecting this server has +2 strength until the end of the run"
                 :req (req (and this-server (pos? (count run-ices)) (pos? (count (:hand corp)))))
                 :delayed-completion true
                 :effect (req (show-wait-prompt state :runner "Corp to use Helheim Servers")
                              (when-completed
                                (resolve-ability
                                  state side
                                  {:prompt "Choose a card in HQ to trash"
                                   :choices {:req #(and (in-hand? %) (= (:side %) "Corp"))}
                                   :effect (effect (trash target) (clear-wait-prompt :runner))} card nil)
                                (do (register-events
                                      state side
                                      {:pre-ice-strength {:req (req (= (card->server state card)
                                                                       (card->server state target)))
                                                          :effect (effect (ice-strength-bonus 2 target))}
                                       :run-ends {:effect (effect (unregister-events card))}} card)
                                    (continue-ability
                                      state side
                                      {:effect (req (update-ice-in-server
                                                      state side (card->server state card)))} card nil))))}]
    :events {:pre-ice-strength nil}}})
