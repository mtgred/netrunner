(ns netrunner.appstate)

(def app-state
  (atom {:active-page "/"
         :user (js->clj js/user :keywordize-keys true)
         :options (merge {:background "lobby-bg"
                          :show-alt-art true
                          :deckstats "always"
                          :gamestats "always"
                          :sounds (let [sounds (js->clj (.getItem js/localStorage "sounds"))]
                                    (if (nil? sounds) true (= sounds "true")))
                          :sounds-volume (let [volume (js->clj (.getItem js/localStorage "sounds_volume"))]
                                           (if (nil? volume) 100 (js/parseInt volume)))}
                         (:options (js->clj js/user :keywordize-keys true)))

         :cards [] :cards-loaded false
         :sets [] :mwl [] :cycles []
         :decks [] :decks-loaded false
         :stats (:stats (js->clj js/user :keywordize-keys true))
         :games [] :gameid nil :messages []
         :channels {:general [] :america [] :europe [] :asia-pacific [] :united-kingdom [] :français []
                    :español [] :italia [] :português [] :sverige [] :stimhack-league []}
         }))

