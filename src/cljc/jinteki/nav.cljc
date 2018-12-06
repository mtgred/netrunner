(ns jinteki.nav)
(def navbar-links
  [["Chat" "/" 0 nil]
   ["Cards" "/cards" 1 nil]
   ["Deck Builder" "/deckbuilder" 2 nil]
   ["Play" "/play" 3 nil]
   ["Help" "/help" 4 nil]
   ["Settings" "/account" 5 #(:user %)]
   ["Integration" "/integration" 6 #(:user %)]
   ["Stats" "/stats" 7 #(:user %)]
   ["About" "/about" 8 nil]])
