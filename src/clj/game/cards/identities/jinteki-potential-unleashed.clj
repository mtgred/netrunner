(in-ns 'game.core)

(def card-definitions-identities-jinteki-potential-unleashed
  {"Jinteki: Potential Unleashed"
   {:events {:pre-resolve-damage
             {:req (req (and (-> @state :corp :disable-id not) (= target :net) (pos? (last targets))))
              :effect (req (let [c (first (get-in @state [:runner :deck]))]
                             (system-msg state :corp (str "uses Jinteki: Potential Unleashed to trash " (:title c)
                                                          " from the top of the Runner's Stack"))
                             (mill state :corp :runner 1)))}}}})
