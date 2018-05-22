(ns nr.gameboard
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put! <!] :as async]
            [clojure.string :refer [capitalize includes? join lower-case split]]
            [differ.core :as differ]
            [jinteki.utils :refer [str->int]]
            [jinteki.cards :refer [all-cards]]
            [nr.appstate :refer [app-state]]
            [nr.auth :refer [avatar] :as auth]
            [nr.cardbrowser :refer [add-symbols] :as cb]
            [nr.utils :refer [toastr-options influence-dot]]
            [nr.ws :as ws]))

(defonce game-state (atom {}))
(defonce last-state (atom {}))
(defonce lock (atom false))

(defn image-url [{:keys [side code] :as card}]
  (let [art (or (:art card) ; use the art set on the card itself, or fall back to the user's preferences.
                (get-in @game-state [(keyword (lower-case side)) :user :options :alt-arts (keyword code)]))
        art-options (:alt_art (get (:alt-arts @app-state) code))
        special-user (get-in @game-state [(keyword (lower-case side)) :user :special])
        special-wants-art (get-in @game-state [(keyword (lower-case side)) :user :options :show-alt-art])
        viewer-wants-art (get-in @app-state [:options :show-alt-art])
        show-art (and special-user special-wants-art viewer-wants-art)
        art-available (and art-options (not-empty art-options))
        has-art (and art-options
                     art
                     (contains? art-options (keyword art)))
        version-path (if (and has-art show-art)
                       (get art-options (keyword art) (:code card))
                       (:code card))]
    (str "/img/cards/" version-path ".png")))

(def anr-icons {"[Credits]" "credit"
                "[$]" "credit"
                "[c]" "credit"
                "[Credit]" "credit"
                "[Click]" "click"
                "[Subroutine]" "subroutine"
                "[Recurring Credits]" "recurring-credit"
                "1[Memory Unit]" "mu1"
                "1[mu]" "mu1"
                "2[Memory Unit]" "mu2"
                "2[mu]" "mu2"
                "3[Memory Unit]" "mu3"
                "3[mu]" "mu3"
                "[Link]" "link"
                "[l]" "link"
                "[Memory Unit]" "mu"
                "[mu]" "mu"
                "[Trash]" "trash"
                "[t]" "trash"})

(def ci-open "\u2664")
(def ci-seperator "\u2665")
(def ci-close "\u2666")


(defn find-card-regex-impl [title]
  (str "(^|[^" ci-open "\\S])" title "(?![" ci-seperator "\\w]|([^" ci-open "]+" ci-close "))"))

(def find-card-regex (memoize find-card-regex-impl))
(defn is-card-item [item]
  (and (> (.indexOf item ci-seperator) -1)
       (= 0 (.indexOf item ci-open))))


(defn card-image-token-impl [title code]
  (str "$1" ci-open title ci-seperator code ci-close))

(def card-image-token (memoize card-image-token-impl))

(defn card-image-reducer [text card]
  (.replace text (js/RegExp. (find-card-regex (:title card)) "g") (card-image-token (:title card) (:code card))))

(defn get-non-alt-art [[title cards]]
  {:title title :code (:code (first cards))})

(defn prepare-cards []
  (->> @all-cards
       (filter #(not (:replaced_by %)))
       (group-by :title)
       (map get-non-alt-art)
       (sort-by #(count (:title %1)))
       (reverse)))

(def prepared-cards (memoize prepare-cards))

(defn add-image-codes-impl [text]
  (reduce card-image-reducer text (prepared-cards)))

(def add-image-codes (memoize add-image-codes-impl))

(defn extract-card-info [item]
  (if (is-card-item item)
    [(.substring item 1 (.indexOf item ci-seperator))
     (.substring item (inc (.indexOf item ci-seperator)) (dec (count item)))]))

(defn mute-spectators [mute-state]
  (ws/ws-send! [:netrunner/mute-spectators {:gameid-str (:gameid @game-state) :mute-state mute-state}]))

(defn get-card-code [e]
  (let [code (str (.. e -target -id))]
    (when (pos? (count code))
      code)))

(defn card-preview-mouse-over [e channel]
  (.preventDefault e)
  (when-let [code (get-card-code e)]
    (when-let [card (some #(when (= (:code %) code) %) @all-cards)]
      (put! channel (assoc card :implementation :full))))
  nil)

(defn card-preview-mouse-out [e channel]
  (.preventDefault e)
  (when-let [code (get-card-code e)]
    (put! channel false))
  nil)

(defn get-message-parts-impl [text]
  (let [with-image-codes (add-image-codes (if (nil? text) "" text))
        splitted (.split with-image-codes (js/RegExp. (str "(" ci-open "[^" ci-close "]*" ci-close ")") "g"))
        oldstyle (for [i splitted]
                   (seq (.split i (js/RegExp. (str "([1-3]\\[mu\\]|\\[[^\\]]*\\])") "g"))))]
    (flatten oldstyle)))

(def get-message-parts (memoize get-message-parts-impl))




(defn create-span-impl [item]
  (if (= "[hr]" item)
    [:hr]
    (if (= "[!]" item)
      [:div.smallwarning "!"]
      (if-let [class (anr-icons item)]
        [:span {:class (str "anr-icon " class)}]
        (if-let [[title code] (extract-card-info item)]
          [:span {:class "fake-link" :id code} title]
          [:span item])))))

(def create-span (memoize create-span-impl))

(defn parse-state [state]
  (js->clj (.parse js/JSON state) :keywordize-keys true))

(defn card-zoom [card]
  [:div.card-preview.blue-shade
   [:h4 (:title card)]
   (when-let [memory (:memoryunits card)]
     (if (< memory 3)
       [:div.anr-icon {:class (str "mu" memory)} ""]
       [:div.heading (str "Memory: " memory) [:span.anr-icon.mu]]))
   (when-let [cost (:cost card)]
     [:div.heading (str "Cost: " cost)])
   (when-let [trash-cost (:trash card)]
     [:div.heading (str "Trash cost: " trash-cost)])
   (when-let [strength (:strength card)]
     [:div.heading (str "Strength: " strength)])
   (when-let [requirement (:advancementcost card)]
     [:div.heading (str "Advancement requirement: " requirement)])
   (when-let [agenda-point (:agendatpoints card)]
     [:div.heading (str "Agenda points: " agenda-point)])
   (when-let [min-deck-size (:minimumdecksize card)]
     [:div.heading (str "Minimum deck size: " min-deck-size)])
   (when-let [influence-limit (:influencelimit card)]
     [:div.heading (str "Influence limit: " influence-limit)])
   (when-let [influence (:factioncost card)]
     (when-let [faction (:faction card)]
       [:div.heading "Influence "
        [:span.influence
         {:dangerouslySetInnerHTML #js {:__html (apply str (for [i (range influence)] "&#8226;"))}
          :class                   (-> faction .toLowerCase (.replace " " "-"))}]]))
   [:div.text
    [:p [:span.type (str (:type card))] (if (empty? (:subtype card))
                                          "" (str ": " (:subtype card)))]
    [:pre {:dangerouslySetInnerHTML #js {:__html (add-symbols (:text card))}}]]
   (when-let [url (image-url card)]
     [:img {:src url :alt (:title card) :onLoad #(-> % .-target js/$ .show)}])])

(defn get-side [state]
  (let [user-id (:_id (:user @app-state))]
    (cond
      (= (get-in state [:runner :user :_id]) user-id) :runner
      (= (get-in state [:corp :user :_id]) user-id) :corp
      :else :spectator)))

(defn init-game [state]
  (let [side (get-side state)]
    (.setItem js/localStorage "gameid" (:gameid @app-state))
    (reset! game-state state)
    (swap! game-state assoc :side side)
    (reset! last-state @game-state)))

(defn launch-game [{:keys [state]}]
  (init-game state)
  (set! (.-onbeforeunload js/window) #(clj->js "Leaving this page will disconnect you from the game."))
  (-> "#gamelobby" js/$ .fadeOut)
  (-> "#gameboard" js/$ .fadeIn))

(defn send-command
  ([command] (send-command command nil))
  ([command {:keys [no-lock] :as args}]
   (when (or (not @lock) no-lock)
     (try (js/ga "send" "event" "game" command) (catch js/Error e))
     (when-not no-lock (reset! lock true))
     (ws/ws-send! [:netrunner/action {:gameid-str (:gameid @game-state) :command command :args args}]))))


(defn build-exception-msg [msg error]
  (letfn [(build-report-url [error]
            (js/escape (str "Please describe the circumstances of your error here.\n\n\nStack Trace:\n```clojure\n"
                            error
                            "\n```")))]
    (str "<div>"
         msg
         "<br/>"
         "<button type=\"button\" class=\"reportbtn\" style=\"margin-top: 5px\" "
         "onclick=\"window.open('https://github.com/mtgred/netrunner/issues/new?body="
         (build-report-url error)
         "');\">Report on GitHub</button></div>")))

(defn toast
  "Display a toast warning with the specified message.
  Sends a command to clear any server side toasts."
  [msg type options]
  (set! (.-options js/toastr) (toastr-options options))
  (let [f (aget js/toastr (if (= "exception" type) "error" type))]
    (f (if (= "exception" type) (build-exception-msg msg (:last-error @game-state)) msg))
    (send-command "toast")))


(defn concede []
  (ws/ws-send! [:netrunner/concede {:gameid-str (:gameid @game-state)}]))
