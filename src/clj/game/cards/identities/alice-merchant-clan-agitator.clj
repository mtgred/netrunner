(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-alice-merchant-clan-agitator
  {"Alice Merchant: Clan Agitator"
   {:events {:successful-run
             {:delayed-completion true
              :interactive (req true)
              :req (req (and (= target :archives)
                             (first-successful-run-on-server? state :archives)
                             (not-empty (:hand corp))))
              :effect (effect (show-wait-prompt :runner "Corp to trash 1 card from HQ")
                              (continue-ability
                                {:prompt "Choose a card in HQ to discard"
                                 :player :corp
                                 :choices (req (:hand corp))
                                 :msg "force the Corp to trash 1 card from HQ"
                                 :effect (effect (trash :corp target)
                                                 (clear-wait-prompt :runner))}
                               card nil))}}}})
