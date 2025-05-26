(ns nr.appstate
  (:require
   [cljs.core.async :refer [<!] :refer-macros [go]]
   [jinteki.i18n :as i18n]
   [jinteki.settings :as settings]
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
             :options (-> (reduce (fn [opts {:keys [key default storage-key]}]
                                    (let [storage-name (or storage-key (name key))
                                          ;; Special handling for language default
                                          default-val (if (= key :language) nav-lang default)]
                                      (assoc opts key (ls/load storage-name default-val))))
                                  {}
                                  settings/all-settings)
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
