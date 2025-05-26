(ns nr.appstate
  (:require
   [cljs.core.async :refer [<!] :refer-macros [go]]
   [jinteki.i18n :as i18n]
   [jinteki.utils :refer [str->int]]
   [nr.ajax :refer [GET]]
   [nr.local-storage :as ls]
   [clojure.string :as str]
   [reagent.core :as r]))

(defn- migrate-legacy-localStorage-keys!
  "Migrates legacy localStorage keys to new consistent kebab-case format"
  []
  (ls/migrate-keys! {"custom_bg_url" "custom-bg-url"
                     "sounds_volume" "sounds-volume"
                     "lobby_sounds" "lobby-sounds"}))

(defn- load-visible-formats
  "Loading visible formats from localStorage"
  []
  (let [default-visible-formats #{"standard"
                                  "system-gateway"
                                  "core"
                                  "throwback"
                                  "startup"
                                  "eternal"
                                  "preconstructed"
                                  "casual"}]
    (ls/load "visible-formats" default-visible-formats)))

(def valid-background-slugs
  #{"apex-bg" "custom-bg"
    "find-the-truth-bg" "freelancer-bg"
    "monochrome-bg" "mushin-no-shin-bg"
    "push-your-luck-bg" "rumor-mill-bg"
    "the-root-bg" "traffic-jam-bg"
    "worlds2020"})

(defn validate-options
  [opts]
  (-> opts
      (update :background #(or (valid-background-slugs %) "worlds2020"))
      (update :runner-board-order #(case %
                                     "true" "jnet"
                                     "false" "irl"
                                     %))))

;; we only support the following languages
;; if trs get added for new languages, I guess we need to update this
(def supported-languages
  #{"en" "fr" "ja" "ko" "pl" "pt" "ru" "zh-simp"})

(def nav-lang
  "en-us, en-uk, etc should just be en, fr-CA -> fr, en->en"
  (let [lang (some-> js/navigator.language (str/split #"-") first)]
    (cond
      ;; if we ever implement proper zh, fix this
      (= lang "zh") "zh-simp"
      (contains? supported-languages lang) lang
      :else "en")))

;; Run migration before creating app-state
(migrate-legacy-localStorage-keys!)

(def app-state
  (let [js-user (js->clj js/user :keywordize-keys true)]
    (r/atom {:active-page "/"
             :user js-user
             :options (-> {:background (ls/load "background" "worlds2020")
                           :custom-bg-url (ls/load "custom-bg-url" "https://nullsignal.games/wp-content/uploads/2022/07/Mechanics-of-Midnight-Sun-Header.png")
                           :corp-card-sleeve (ls/load "corp-card-sleeve" "nsg-card-back")
                           :runner-card-sleeve (ls/load "runner-card-sleeve" "nsg-card-back")
                           :card-zoom (ls/load "card-zoom" "image")
                           :pin-zoom (ls/load "pin-zoom" false)
                           :pronouns (ls/load "pronouns" "none")
                           :language (ls/load "language" nav-lang)
                           :default-format (ls/load "default-format" "standard")
                           :show-alt-art (ls/load "show-alt-art" true)
                           :card-resolution (ls/load "card-resolution" "default")
                           :player-stats-icons (ls/load "player-stats-icons" true)
                           :stacked-cards (ls/load "stacked-cards" true)
                           :sides-overlap (ls/load "sides-overlap" true)
                           :log-timestamps (ls/load "log-timestamps" true)
                           :runner-board-order (ls/load "runner-board-order" "irl")
                           :deckstats (ls/load "deckstats" "always")
                           :gamestats (ls/load "gamestats" "always")
                           :log-width (ls/load "log-width" 300)
                           :log-top (ls/load "log-top" 419)
                           :log-player-highlight (ls/load "log-player-highlight" "blue-red")
                           :sounds (ls/load "sounds" true)
                           :lobby-sounds (ls/load "lobby-sounds" true)
                           :sounds-volume (ls/load "sounds-volume" 100)
                           :disable-websockets (ls/load "disable-websockets" false)
                           :pass-on-rez (ls/load "pass-on-rez" false)
                           :ghost-trojans (ls/load "ghost-trojans" true)
                           :display-encounter-info (ls/load "display-encounter-info" false)
                           :alt-arts (ls/load "alt-arts" {})
                           :bespoke-sounds (ls/load "bespoke-sounds" {})
                           :blocked-users (ls/load "blocked-users" [])}
                          (merge (:options js-user))
                          (validate-options))
             :cards-loaded false
             :connected false
             :previous-cards {}
             :sets [] :mwl [] :cycles []
             :decks [] :decks-loaded false
             :stats (:stats js-user)
             :visible-formats (load-visible-formats)
             :channels {:general [] :america [] :europe [] :asia-pacific [] :united-kingdom [] :français []
                        :español [] :italia [] :polska [] :português [] :sverige [] :stimhack-league [] :русский []}
             :games [] :current-game nil})))

(go (let [lang (get-in @app-state [:options :language] "en")
          response (<! (GET (str "/data/language/" lang)))]
      (when (= 200 (:status response))
        (i18n/insert-lang! lang (:json response)))))

(defn current-gameid [app-state]
  (get-in @app-state [:current-game :gameid]))
