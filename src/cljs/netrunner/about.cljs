(ns netrunner.about
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]))

(def app-state (atom {}))

(defn about [cursor owner]
  (om/component
   (sab/html
    [:div.about.panel.blue-shade
     [:h3 "About"]
     [:p "This a website created by an avid Netrunner player from Belgium. The goal is to provide a great way to create and test of Netrunner decks online."]

     [:h3 "Contact"]
     [:p "Twitter: " [:a {:href "http://twitter.com/mtgred"} "@mtgred"]]
     [:p "Email: " [:a {:href "mailto:mtgred@gmail.com"} "mtgred@gmail.com"]]

     [:h3 "Development"]

     [:p "The card browser and deck builder are ready for use. The gameplay functionnalities are currently under development."]

     [:p "Bug reports and feature suggestions can be filled on "
      [:a {:href "https://github.com/mtgred/netrunner/issues"} "Github"] "."]

     [:p "Manabase is built using Clojurescript, Om, Socket.io, Node.js"
      "If you are proficient with these technologies and would like to contribute to the development, feel free to get in touch."]

     [:h3 "Credits"]
     [:p "The Card data and images are provided by "
      [:a {:href "http://netrunnerdb.com"} "netrunnerdb.com"]
      ". Big thanks to "
      [:a {:href "http://twitter.com/alsciende"} "@alsciende"]
      " for creating and maintaining such a great website."]

     [:h3 "Disclaimer"]
     [:p "Netrunner and Android are trademarks of Fantasy Flight Publishing, Inc. and/or Wizards of the Coast LLC. (c) 2013 Wizards."]
     [:p "This is website is not affiliated with Fantasy Flight Games or Wizards of the Coast."]])))

(om/root about app-state {:target (. js/document (getElementById "about"))})
