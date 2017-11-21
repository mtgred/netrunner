(ns jinteki.nav)
(def navbar-links
  [["Chat" "/" 0 nil]
   ["Cards" "/cards" 1 nil]
   ["Deck Builder" "/deckbuilder" 2 nil]
   ["Play" "/play" 3 nil]
   ["Help" "/help" 4 nil]
   ["Settings" "/account" 5 #(:user %)]
   ["Stats" "/stats" 6 #(:user %)]
   ["About" "/about" 7 nil]])