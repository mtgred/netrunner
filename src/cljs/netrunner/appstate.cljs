(ns netrunner.appstate)

(def app-state
  (atom {:active-page "/"
         :user (js->clj js/user :keywordize-keys true)
         :options (merge {:background "lobby-bg" :enablesounds (js->clj (.getItem js/localStorage "enablesounds"))}
                         (:options (js->clj js/user :keywordize-keys true)))
         :cards [] :sets [] :mwl []
         :decks [] :decks-loaded false
         :games [] :gameid nil :messages []}))
