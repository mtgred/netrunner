(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-news-team
  {"News Team"
   {:flags {:rd-reveal (req true)}
    :access {:msg (msg "force the Runner take 2 tags or add it to their score area as an agenda worth -1 agenda point")
             :delayed-completion true
             :effect (effect (continue-ability
                               {:player :runner
                                :delayed-completion true
                                :prompt "Take 2 tags or add News Team to your score area as an agenda worth -1 agenda point?"
                                :choices ["Take 2 tags" "Add News Team to score area"]
                                :effect (req (if (= target "Add News Team to score area")
                                               (do (system-msg state :runner (str "adds News Team to their score area as an agenda worth -1 agenda point"))
                                                   (as-trashed-agenda state :runner card -1 {:force true})
                                                   (trigger-event state side :no-trash card)
                                                   (effect-completed state side eid))
                                               (do (system-msg state :runner (str "takes 2 tags from News Team"))
                                                   (tag-runner state :runner eid 2)
                                                   (trigger-event state side :no-trash card))))}
                               card targets))}}})