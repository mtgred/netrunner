(in-ns 'game.cards.assets)

(def card-definition-reconstruction-contract
  {"Reconstruction Contract"
   {:events {:damage {:req (req (and (pos? (nth targets 2)) (= :meat target)))
                      :effect (effect (add-counter card :advancement 1)
                                      (system-msg "adds 1 advancement token to Reconstruction Contract"))}}
    :abilities [{:label "[Trash]: Move advancement tokens to another card"
                 :prompt "Select a card that can be advanced"
                 :choices {:req can-be-advanced?}
                 :effect (req (let [move-to target]
                                (resolve-ability
                                  state side
                                  {:prompt "Move how many tokens?"
                                   :choices {:number (req (get-counters card :advancement))
                                             :default (req (get-counters card :advancement))}
                                   :effect (effect (trash card {:cause :ability-cost})
                                                   (add-counter move-to :advancement target {:placed true})
                                                   (system-msg (str "trashes Reconstruction Contract to move " target
                                                                    (pluralize " advancement token" target) " to "
                                                                    (card-str state move-to))))}
                                  card nil)))}]}})
