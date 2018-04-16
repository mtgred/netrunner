(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-sso-industries-fueling-innovation
  {"SSO Industries: Fueling Innovation"
   (letfn [(installed-faceup-agendas [state]
             (->> (all-installed state :corp)
               (filter #(is-type? % "Agenda"))
               (filter faceup?)))
           (selectable-ice? [card]
             (and
               (is-type? card "ICE")
               (installed? card)
               (zero? (+ (:advance-counter card 0)
                         (:extra-advance-counter card 0)))))
           (ice-with-no-advancement-tokens [state]
             (->> (all-installed state :corp)
               (filter selectable-ice?)))]
     {:events {:corp-turn-ends
               {:optional
                {:prompt "Place advancement tokens?"
                 :req (req (and
                             (not-empty (installed-faceup-agendas state))
                             (not-empty (ice-with-no-advancement-tokens state))))
                 :yes-ability
                 {:delayed-completion true
                  :effect (req (show-wait-prompt state :runner "Corp to use SSO Industries' ability")
                            (let [agendas (installed-faceup-agendas state)
                                  agenda-points (->> agendas
                                                  (map :agendapoints)
                                                  (reduce +))
                                  ice (ice-with-no-advancement-tokens state)]
                              (continue-ability
                                state side
                                {:prompt (str "Select ICE with no advancement tokens to place "
                                             (quantify agenda-points "advancement token") " on")
                                 :choices {:req #(selectable-ice? %)}
                                 :msg (msg "places " (quantify agenda-points "advancement token")
                                           " on ICE with no advancement tokens")
                                 :effect (req (add-prop state :corp target :advance-counter agenda-points {:placed true})
                                              (clear-wait-prompt state :runner))
                                 :cancel-effect (req (clear-wait-prompt state :runner))}
                                card nil)))}}}}})})