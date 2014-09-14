(ns netrunner.about
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]))

(def app-state (atom {}))

(defn about [cursor owner]
  (om/component
   (sab/html
    [:div.about.panel.blue-shade
     [:h3 "About"]
     [:p "This website is created by an avid Netrunner player from Belgium. The goal is to provide a great way to create and test Netrunner decks online."]

     [:h3 "Contact"]
     [:p "Twitter: " [:a {:href "http://twitter.com/mtgred"} "@mtgred"]]
     [:p "Email: " [:a {:href "mailto:mtgred@gmail.com"} "mtgred@gmail.com"]]

     [:h3 "Development"]

     [:p "The card browser and deck builder are ready for use. The gameplay functionnalities are currently under development. " [:a {:href "https://www.dropbox.com/s/rpkhxafgile5spp/Cards%20status.xlsx"} "Card implementation status"]]

     [:p "The code is open source and available on "
      [:a {:href "https://github.com/mtgred/netrunner"} "Github"] "."]

     [:p "Bug reports and feature suggestions can be submitted "
      [:a {:href "https://github.com/mtgred/netrunner/issues"} "here"] "."]

     [:h3 "Disclaimer"]
     [:p "Netrunner and Android are trademarks of Fantasy Flight Publishing, Inc. and/or Wizards of the Coast LLC."]
     [:p "This is website is not affiliated with Fantasy Flight Games or Wizards of the Coast."]])))

(om/root about app-state {:target (. js/document (getElementById "about"))})
