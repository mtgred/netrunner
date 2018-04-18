(in-ns 'game.core)

(def card-definitions-icebreakers-femme-fatale
  {"Femme Fatale"
   (auto-icebreaker ["Sentry"]
                    {:prompt "Select a piece of ICE to target for bypassing"
                     :choices {:req ice?}
                     :leave-play (req (remove-icon state side card))
                     :effect (req (let [ice target
                                        serv (zone->name (second (:zone ice)))]
                                    (add-icon state side card ice "F" "blue")
                                    (system-msg state side
                                                (str "selects " (card-str state ice)
                                                     " for Femme Fatale's bypass ability"))))
                     :abilities [(break-sub 1 1 "Sentry")
                                 (strength-pump 2 1)]})})
