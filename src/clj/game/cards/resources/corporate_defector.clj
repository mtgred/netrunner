(in-ns 'game.cards.resources)

(def card-definition-corporate-defector
  {"Corporate Defector"
   {:events {:corp-click-draw {:msg (msg "reveal " (-> target first :title))}}}})
