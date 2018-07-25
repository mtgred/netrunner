(in-ns 'game.cards.upgrades)

(def card-definition-daruma
  {"Daruma"
   (letfn [(choose-swap [to-swap]
             {:prompt (str "Select a card to swap with " (:title to-swap))
              :choices {:not-self true
                        :req #(and (= "Corp" (:side %))
                                   (#{"Asset" "Agenda" "Upgrade"} (:type %))
                                   (or (in-hand? %) ; agenda, asset or upgrade from HQ
                                       (and (installed? %) ; card installed in a server
                                            ;; central upgrades are not in a server
                                            (not (#{:hq :rd :archives} (first (:zone %)))))))}
              :effect (req (wait-for (trash state :corp card nil)
                                     (move state :corp to-swap (:zone target) {:keep-server-alive true})
                                     (move state :corp target (:zone to-swap) {:keep-server-alive true})
                                     (system-msg state :corp
                                                 (str "uses Daruma to swap " (card-str state to-swap)
                                                      " with " (card-str state target)))
                                     (clear-wait-prompt state :runner)))
              :cancel-effect (effect (clear-wait-prompt :runner))})
           (ability [card]
             {:optional {:prompt "Trash Daruma to swap a card in this server?"
                         :yes-ability {:async true
                                       :prompt "Select a card in this server to swap"
                                       :choices {:req #(and (installed? %)
                                                            (in-same-server? card %))
                                                 :not-self true}
                                       :effect (effect (continue-ability (choose-swap target) card nil))}
                         :no-ability {:effect (effect (clear-wait-prompt :runner))}}})]
   {:events {:approach-server {:async true
                               :effect (effect (show-wait-prompt :runner "Corp to use Daruma")
                                               (continue-ability :corp (ability card) card nil))}}})})
