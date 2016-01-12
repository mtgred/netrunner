(ns netrunner.help
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]))

(def app-state (atom {}))

(def help-contents
  ;; every header tag MUST have an ID here, defined with # in the keyword
  (list [:h3#faq "Frequently Asked Questions"]
        [:h4#avatar "How can I change my avatar?"]
        [:p "Go to " [:a {:href "http://gravatar.com" :target "_blank"} "gravatar.com"]
         " and create an account with the same email as the one used to register on Jinteki.net."]

        [:h4#donation "How can I make a donation?"]
        [:p "Donation info can be found on the " [:a {:href "/about"} "About"] " page."]

        [:h4#toomanycards "There are too many cards or menu options to fit the screen."]
        [:p "Decrease the zoom level of your browser and you should be able to see everything."]

        [:h3#lessfaq "Less Frequently Asked Questions"]
        [:h4#bestbrowser "What is the best supported browser?"]
        [:p "Google Chrome on a desktop or laptop is recommended. Firefox and Safari should work fine too."]

        [:h4#documentation "Is there documentation on how to use jinteki.net?"]
        [:p "Read the "
         [:a {:href "https://github.com/mtgred/netrunner/wiki/Jinteki.net-Guide" :target "_blank"}
          "Jinteki.net Guide"] " on the Github wiki." ]))

(def help-toc
  [:nav {:role "navigation" :class "table-of-contents"} [:ul
   (for [tag help-contents
         :let [tagkey (name (first tag))
               tagcontents (second tag)
               tagname (first (clojure.string/split tagkey #"#"))
               tagid (second (clojure.string/split tagkey #"#"))]
         :when (or (= "h3" tagname) (= "h4" tagname))]
     [:li [:a {:href (str "#" tagid)} tagcontents]])]])

(defn help [cursor owner]
  (om/component
    (sab/html
      [:div.help.panel.blue-shade
       help-toc
       help-contents])))

(om/root help app-state {:target (. js/document (getElementById "help"))})
