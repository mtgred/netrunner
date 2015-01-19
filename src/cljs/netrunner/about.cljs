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
     [:p "Twitter: " [:a {:href "http://twitter.com/mtgred" :target "_blank"} "@mtgred"]]
     [:p "Email: " [:a {:href "mailto:mtgred@gmail.com"} "mtgred@gmail.com"]]

     [:h3 "Development"]

     [:p "The card browser and deck builder are ready for use. The gameplay functionnalities are currently under development. Card automation "
      [:a {:href "https://www.dropbox.com/s/rpkhxafgile5spp/Cards%20status.xlsx"
           :target "_blank"} "status"] "."]

     [:p "The code is open source and available on "
      [:a {:href "https://github.com/mtgred/netrunner" :target "_blank"} "Github"] "."]

     [:p "Bug reports and feature suggestions can be submitted "
      [:a {:href "https://github.com/mtgred/netrunner/issues" :target "_blank"} "here"] "."]

     [:h3 "Frequently Asked Questions"]
     [:ul.list
      [:li
       [:h4 "How can I change my avatar?"]
       [:p "Go to " [:a {:href "http://gravatar.com" :target "_blank"} "gravatar.com"]
        " and create an account with the same email used to register on Jinteki.net."]]
      [:li
       [:h4 "How do I quit a game when it's finished?"]
       [:p "A quit button will be added in the future. In the meanwhile, simply refresh your browser."]]
      [:li
       [:h4 "How can I make donation?"]
       [:p "If you would like to contribute to running costs of Jinteki.net, my PayPal account is mtgred@gmail.com"]]
      [:li
       [:h4 "What is the best supported browser?"]
       [:p "Google Chrome on a desktop/laptop is recommended. Safari and Firefox are supported. Drag and dropping cards during gameplay sometime causes Firefox to navigate to the card image and disconnect from the game. Internet Explorer is not supported."]]]

     [:h3 "Disclaimer"]
     [:p "Netrunner and Android are trademarks of Fantasy Flight Publishing, Inc. and/or Wizards of the Coast LLC."]
     [:p "This is website is not affiliated with Fantasy Flight Games or Wizards of the Coast."]])))

(om/root about app-state {:target (. js/document (getElementById "about"))})
