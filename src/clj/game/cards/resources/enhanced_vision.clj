(in-ns 'game.cards.resources)

(def card-definition-enhanced-vision
  {"Enhanced Vision"
   {:events {:successful-run {:silent (req true)
                              :msg (msg "force the Corp to reveal " (:title (first (shuffle (:hand corp)))))
                              :req (req (genetics-trigger? state side :successful-run))}}}})
