(ns netrunner.help
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [clojure.string :refer [split]]))

(def app-state (atom {}))

(def help-contents
  "Sablono tags with FAQ about jinteki.net. Every header tag MUST have an ID here, defined with # in the keyword."
  (list [:h3#general "General"]

        [:h4#documentation "Is there more documentation on how to use jinteki.net?"]
        [:p "Read the "
         [:a {:href "https://github.com/mtgred/netrunner/wiki/Jinteki.net-Guide" :target "_blank"}
          "Jinteki.net Guide"] " on the GitHub wiki."]

        [:h3#site "Website"]

        [:h4#avatar "How can I change my avatar?"]
        [:p "Go to " [:a {:href "http://gravatar.com" :target "_blank"} "gravatar.com"]
         " and create an account with the same email as the one used to register on Jinteki.net."]

        [:h4#bestbrowser "What is the best supported browser?"]
        [:p "Google Chrome or Firefox on a desktop or laptop is recommended. Safari should work fine too. "
         "Touchscreen devices (smartphones, tablets etc.) are currently not supported."]

        [:h3#cards "Cards and Rules"]

        [:h3#troubleshooting "Troubleshooting"]

        [:h4#touchproblems "The website doesn't work on my touchscreen device."]
        [:p "Touchscreen devices are currently not supported. See answer to " [:a {:href "#bestbrowser"} "this question"]
         " for best browsers to use with Jinteki.net."]

        [:h4#toomanycards "There are too many cards or menu options to fit the screen."]
        [:p "Decrease the zoom level of your browser and you should be able to see everything. If you are using "
         "Chrome, you can do it by pressing CTRL and - (minus). If you are using Firefox, you may need to install "
         [:a {:href "https://addons.mozilla.org/pl/firefox/addon/zoom-page/"} "Zoom Page addon"] " before the zoom works correctly."]

        [:h4#zerogames "Whenever I connect to the site, I see there are 0 games in the lobby."]
        [:p "This is most likely a websocket issue. Check if your network filters let through traffic from ws.jinteki.net. "
         "Whitelisting *.jinteki.net should solve the problem."]

        [:h3#getinv "Getting Involved"]

        [:h4#bugs "How can I report a bug?"]
        [:p "The best place to report bugs is the " [:a {:href "https://github.com/mtgred/netrunner/issues"} "GitHub issue tracker"]
         ". Before reporting, it is best to make a quick search to see if it's already been reported. "
         "If the bug concerns a card, look it up in "
         [:a {:href "https://docs.google.com/spreadsheets/d/1ICv19cNjSaW9C-DoEEGH3iFt09PBTob4CAutGex0gnE/pubhtml"} "Card implementation status"]
         " - the card in question may be unimplemented yet."]

        [:h4#features "How can I suggest a feature?"]
        [:p "Same as bugs - feature requests should go on the " [:a {:href "https://github.com/mtgred/netrunner/issues"} "GitHub issue tracker"]
         ". Again, it's best to make a quick search first to avoid duplicating existing issues."]

        [:h4#donations "How can I make a donation?"]
        [:p "Donation info can be found on the " [:a {:href "/about"} "About"] " page."]

        [:h4#devs "How can I help with the coding/webdesign?"]
        [:p "Visit the project page on " [:a {:href "https://github.com/mtgred/netrunner/"} "GitHub"] " and fork "
         "the repository. Implement the changes you were planning on doing and create a PR (Pull Request). If you are in "
         "need of some ideas, check out " [:a {:href "https://github.com/mtgred/netrunner/labels/easy"} "issues marked 'easy' on GitHub"] "."]
        [:p "After two of your PRs have been merged into the master branch, send an e-mail to "
         [:a {:href "mailto:mtgred@gmail.com"} "mtgred@gmail.com"] " stating who you are on GitHub and ask for access "
         "to Jinteki.net Slack, so you can get in better contact with the dev team."]

        [:h4#awesome "Why is this site so awesome?"]
        [:p "Because We Built It."]))

(def help-toc
  "Generates list serving as help's table of contents. Parses help-contents and looks for :h3 and :h4 tags."
  [:nav {:role "navigation" :class "table-of-contents"}
   [:ul
    (for [tag help-contents
          :let [tagkey (name (first tag))
                tagcontents (second tag)
                tagname (first (split tagkey #"#"))
                tagid (second (split tagkey #"#"))]
          :when (= "h4" tagname)]
      [:li [:a (when tagid {:href (str "#" tagid)}) tagcontents]])]])

(defn help [cursor owner]
  (om/component
    (sab/html
      [:div.help.panel.blue-shade
       [:h3 "Table of contents"]
       help-toc
       help-contents])))

(om/root help app-state {:target (. js/document (getElementById "help"))})
