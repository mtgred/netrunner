(in-ns 'game.core)

(def card-definitions-resources-corporate-defector
  {"Corporate Defector"
   {:events {:corp-click-draw {:msg (msg "reveal " (-> target first :title))}}}})
