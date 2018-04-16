(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-reconstruction-contract
  {"Reconstruction Contract"
   {:events {:damage {:req (req (and (pos? (nth targets 2)) (= :meat target)))
                      :effect (effect (add-counter card :advancement 1)
                                      (system-msg "adds 1 advancement token to Reconstruction Contract"))}}
    :abilities [{:label "[Trash]: Move advancement tokens to another card"
                 :prompt "Select a card that can be advanced"
                 :choices {:req can-be-advanced?}
                 :effect (req (let [move-to target
                                    recon card]
                                (resolve-ability
                                  state side
                                  {:prompt "Move how many tokens?"
                                   :choices {:number (req (:advance-counter recon 0))
                                             :default (req (:advance-counter recon 0))}
                                   :effect (effect (add-counter move-to :advancement target)
                                                   (system-msg (str "trashes Reconstruction Contract to move " target
                                                                    (pluralize " advancement token" target) " to "
                                                                    (card-str state move-to)))
                                                   (trash recon {:cause :ability-cost}))}

                                  card nil)
                                ))}]}})
