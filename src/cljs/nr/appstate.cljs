(ns nr.appstate
  (:require [jinteki.utils :refer [str->int]]
            [reagent.core :as r]))

(defn- get-local-value
  "Read the value of the given key from localStorage. Return the default-value if no matching key found"
  [k default-value]
  (let [rtn (js->clj (.getItem js/localStorage k))]
    (if (nil? rtn) default-value rtn)))

(defn- load-visible-formats
  "Loading visible formats from localStorage. Accounting for the fact that js->clj doesn't handle sets"
  []
  (let [default-visible-formats #{"standard"
                                  "system-gateway"
                                  "startup"
                                  "eternal"
                                  "snapshot"
                                  "snapshot-plus"
                                  "classic"
                                  "casual"}
        serialized (get-local-value "visible-formats" "")]
    (if (empty? serialized) default-visible-formats (set (.parse js/JSON serialized)))))


(def app-state
  (r/atom {:active-page "/"
           :user (js->clj js/user :keywordize-keys true)
           :options (merge {:background "lobby-bg"
                            :card-back (get-local-value "card-back" "nisei")
                            :card-zoom (get-local-value "card-zoom" "image")
                            :pin-zoom (get-local-value "pin-zoom" false)
                            :pronouns "none"
                            :language "en"
                            :show-alt-art true
                            :card-resolution "default"
                            :player-stats-icons (= (get-local-value "player-stats-icons" "true") "true")
                            :stacked-servers (= (get-local-value "stacked-servers" "true") "true")
                            :sides-overlap (= (get-local-value "sides-overlap" "true") "true")
                            :runner-board-order (let [value (get-local-value "runner-board-order" "irl")]
                                                  (case value
                                                    "true" "jnet"
                                                    "false" "irl"
                                                    value))
                            :deckstats "always"
                            :gamestats "always"
                            :log-width (str->int (get-local-value "log-width" "300"))
                            :log-top (str->int (get-local-value "log-top" "419"))
                            :sounds (= (get-local-value "sounds" "true") "true")
                            :lobby-sounds (= (get-local-value "lobby_sounds" "true") "true")
                            :sounds-volume (str->int (get-local-value "sounds_volume" "100"))}
                           (:options (js->clj js/user :keywordize-keys true)))

           :cards-loaded false
           :previous-cards {}
           :sets [] :mwl [] :cycles []
           :decks [] :decks-loaded false
           :stats (:stats (js->clj js/user :keywordize-keys true))
           :visible-formats (load-visible-formats)
           :games [] :gameid nil :messages []
           :channels {:general [] :america [] :europe [] :asia-pacific [] :united-kingdom [] :français []
                      :español [] :italia [] :polska [] :português [] :sverige [] :stimhack-league [] :русский []}
           }))
