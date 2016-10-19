(ns netrunner.appstate)

(def app-state
  (atom {:active-page "/"
         :user (js->clj js/user :keywordize-keys true)
         :options (merge {:background "lobby-bg"
                          :opponent-alt-art true
                          :enablesounds (let [sounds (js->clj (.getItem js/localStorage "sounds"))]
                                          (if (nil? sounds) true sounds))}
                         (:options (js->clj js/user :keywordize-keys true)))
         :cards [] :sets [] :mwl []
         :decks [] :decks-loaded false
         :games [] :gameid nil :messages []}))
