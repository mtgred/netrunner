(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-ghost-branch
  {"Ghost Branch"
   (advance-ambush 0 {:delayed-completion true
                      :req (req (< 0 (:advance-counter (get-card state card) 0)))
                      :msg (msg "give the Runner " (quantify (:advance-counter (get-card state card) 0) "tag"))
                      :effect (effect (tag-runner :runner eid (:advance-counter (get-card state card) 0)))})})
