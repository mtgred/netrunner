(in-ns 'game.core)

(def card-definitions-resources-enhanced-vision
  {"Enhanced Vision"
   {:events {:successful-run {:silent (req true)
                              :msg (msg "force the Corp to reveal " (:title (first (shuffle (:hand corp)))))
                              :req (req (genetics-trigger? state side :successful-run))}}}})
