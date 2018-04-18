(in-ns 'game.core)

(def card-definitions-operations-an-offer-you-cant-refuse
  {"An Offer You Cant Refuse"
   {:delayed-completion false
    :prompt "Choose a server" :choices ["Archives" "R&D" "HQ"]
    :effect (req (let [serv target]
                   (show-wait-prompt state :corp (str "Runner to decide on running " target))
                   (continue-ability
                     state side
                     {:optional
                      {:prompt (msg "Make a run on " serv "?") :player :runner
                       :yes-ability {:msg (msg "let the Runner make a run on " serv)
                                     :effect (effect (clear-wait-prompt :corp)
                                                     (game.core/run eid serv nil card))}
                       :no-ability {:effect (effect (clear-wait-prompt :corp)
                                                    (as-agenda :corp (some #(when (= (:cid card) (:cid %)) %) (:discard corp)) 1))
                                    :msg "add it to their score area as an agenda worth 1 agenda point"}}}
                    card nil)))}})
