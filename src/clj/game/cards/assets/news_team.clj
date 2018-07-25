(in-ns 'game.cards.assets)

(def card-definition-news-team
  {"News Team"
   {:flags {:rd-reveal (req true)}
    :access {:msg (msg "force the Runner take 2 tags or add it to their score area as an agenda worth -1 agenda point")
             :async true
             :effect (effect (continue-ability
                               {:player :runner
                                :async true
                                :prompt "Take 2 tags or add News Team to your score area as an agenda worth -1 agenda point?"
                                :choices ["Take 2 tags" "Add News Team to score area"]
                                :effect (req (if (= target "Add News Team to score area")
                                               (do (system-msg state :runner (str "adds News Team to their score area as an agenda worth -1 agenda point"))
                                                   (trigger-event state side :no-trash card)
                                                   (as-trashed-agenda state :runner eid card -1 {:force true}))
                                               (do (system-msg state :runner (str "takes 2 tags from News Team"))
                                                   (gain-tags state :runner eid 2))))}
                               card targets))}}})
