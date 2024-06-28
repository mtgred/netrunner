(ns nr.utils
  (:require
   ["@js-joda/locale_en-us" :as js-joda-locale]
   [cljc.java-time.format.date-time-formatter :as formatter]
   [cljc.java-time.zoned-date-time :as zdt]
   [cljc.java-time.zone-id :as zone]
   [cljc.java-time.instant :as inst]
   [clojure.string :refer [join] :as s]
   [goog.object :as gobject]
   [goog.string :as gstring]
   [goog.string.format]
   [nr.appstate :refer [app-state]]
   [nr.translations :refer [tr-data]]
   [reagent.dom :as rd]))

;; Dot definitions
(def zws "\u200B")                  ; zero-width space for wrapping dots
(def influence-dot (str "●" zws))   ; normal influence dot
(def banned-dot (str "✘" zws))      ; on the banned list
(def restricted-dot (str "🦄" zws)) ; on the restricted list
(def alliance-dot (str "○" zws))    ; alliance free-inf dot
(def rotated-dot (str "↻" zws))     ; on the rotation list
(def deck-points-dot (str "❖" zws)) ; costs deck points

(def banned-span
  [:span.invalid {:title "Removed"} " " banned-dot])

(def restricted-span
  [:span {:title "Restricted"} " " restricted-dot])

(def rotated-span
  [:span.casual {:title "Rotated"} " " rotated-dot])

(defn deck-points-card-span [points]
  [:span.legal {:title (when points
                         (str "Deck points: " points))}
   " " deck-points-dot])

(defn- make-dots
  "Returns string of specified dots and number. Uses number for n > 20"
  [dot n]
  (if (<= 20 n)
    (str n dot)
    (join (conj (repeat n dot) ""))))

(defn influence-dots
  "Returns a string with UTF-8 full circles representing influence."
  [num]
  (make-dots influence-dot num))

(defn alliance-dots
  [num]
  (make-dots alliance-dot num))

(defn dots-html
  "Make a hiccup-ready vector for the specified dot and cost-map (influence or mwl)"
  [dot cost-map]
  (for [factionkey (sort (keys cost-map))]
    ^{:key factionkey}
    [:span.influence {:class (name factionkey)} (make-dots dot (factionkey cost-map))]))

(defn faction-icon
  [faction id]
  (letfn [(icon-span [css-faction]
            [:span.faction-icon {:class css-faction
                                 :title (str faction " - " id)}])]
    (case faction
      "Adam" (icon-span "adam")
      "Anarch" (icon-span "anarch")
      "Apex" (icon-span "apex")
      "Criminal" (icon-span "criminal")
      "Haas-Bioroid" (icon-span "hb")
      "Jinteki" (icon-span "jinteki")
      "NBN" (icon-span "nbn")
      "Shaper" (icon-span "shaper")
      "Sunny Lebeau" (icon-span "sunny")
      "Weyland Consortium" (icon-span "weyland")
      "Neutral" [:span.side ""]
      [:span.side "(Unknown)"])))

;; Shared function options
(defn toastr-options
  "Function that generates the correct toastr options for specified settings"
  [options]
  (js-obj "closeButton" (:close-button options false)
          "debug" false
          "newestOnTop" false
          "progressBar" false
          "positionClass" "toast-card"
          ;; preventDuplicates - identical toasts don't stack when the property is set to true.
          ;; Duplicates are matched to the previous toast based on their message content.
          "preventDuplicates" (:prevent-duplicates options true)
          "onclick" nil
          "showDuration" 300
          "hideDuration" 1000
          ;; timeOut - how long the toast will display without user interaction
          "timeOut" (:time-out options 3000)
          ;; extendedTimeOut - how long the toast will display after a user hovers over it
          "extendedTimeOut" (:time-out options 1000)
          "showEasing" "swing"
          "hideEasing" "linear"
          "showMethod" "fadeIn"
          "hideMethod" "fadeOut"
          "tapToDismiss" (:tap-to-dismiss options true)))

(defn non-game-toast
  "Display a toast warning with the specified message."
  ([msg toast-type] (non-game-toast msg toast-type nil))
  ([msg toast-type options]
   (set! (.-options js/toastr) (toastr-options options))
   (let [f (aget js/toastr toast-type)]
     (f msg))))

(defn map-longest
  [f default & colls]
  (lazy-seq
    (when (some seq colls)
      (cons
        (apply f (map #(if (seq %) (first %) default) colls))
        (apply map-longest f default (map rest colls))))))

(def slug->format
  {"standard" "Standard"
   "system-gateway" "System Gateway"
   "startup" "Startup"
   "sunset" "Sunset"
   "eternal" "Eternal"
   "snapshot" "Snapshot"
   "snapshot-plus" "Snapshot Plus"
   "neo" "Neo"
   "casual" "Casual"})

(def format->slug
  {"Standard" "standard"
   "System Gateway" "system-gateway"
   "Startup" "startup"
   "Sunset" "sunset"
   "Eternal" "eternal"
   "Snapshot" "snapshot"
   "Snapshot Plus" "snapshot-plus"
   "Neo" "neo"
   "Casual" "casual"})

(defn regex-escape
  "Escape characters in a string which have special meanings in regexes"
  [string]
  (let [special-chars ".*+?[](){}^$"
        escaped-chars (map #(str "\\" %) special-chars)
        regex-escape-smap (zipmap special-chars escaped-chars)]
    (->> string
         (replace regex-escape-smap)
         (reduce str))))

(def icon-patterns
  "A sequence of icon pattern pairs consisting of an regex, used to match icon
  codes, and the span fragment that should replace it"
  (letfn [(span-of [icon] [:span {:class (str "anr-icon " icon) :title (str " " icon) :aria-label (str icon) :role "img" }])
          (regex-of [icon-code] (re-pattern (str "(?i)" (regex-escape icon-code))))]
    (->> {"[credit]" "credit"
          "[credits]" "credit"
          "[c]" "credit"
          "[recurring credit]" "recurring-credit"
          "[recurring credits]" "recurring-credit"
          "[recurring-credit]" "recurring-credit"
          "[recurring-credits]" "recurring-credit"
          "[click]" "click"
          "[clicks]" "click"
          "1[memory unit]" "mu1"
          "1[mu]" "mu1"
          "2[memory unit]" "mu2"
          "2[mu]" "mu2"
          "3[memory unit]" "mu3"
          "3[mu]" "mu3"
          "[memory unit]" "mu"
          "[mu]" "mu"
          "[link]" "link"
          "[l]" "link"
          "[subroutine]" "subroutine"
          "[trash]" "trash"
          "[t]" "trash"
          "[adam]" "adam"
          "[anarch]" "anarch"
          "[apex]" "apex"
          "[criminal]" "criminal"
          "[hb]" "haas-bioroid"
          "[haas-bioroid]" "haas-bioroid"
          "[jinteki]" "jinteki"
          "[nbn]" "nbn"
          "[shaper]" "shaper"
          "[sunny]" "sunny"
          "[weyland]" "weyland-consortium"
          "[weyland-consortium]" "weyland-consortium"}
      (map (fn [[k v]] [(regex-of k) (span-of v)]))
      (sort-by (comp count str first) >))))

(defn card-patterns-impl
  "A sequence of card pattern pairs consisting of a regex, used to match a card
  name in text, and the span fragment that should replace it"
  []
  (letfn [(span-of [title tr-title] [:span {:class "fake-link" :data-card-title title} tr-title])]
    (distinct (concat
     (->> (:all-cards-and-flips @app-state)
          (vals)
          (remove :replaced_by)
          (map (fn [c] [(:title c) (span-of (:title c) (:title c))]))
          (sort-by (comp count str first) >))
     (->> (:all-cards-and-flips @app-state)
          (vals)
          (remove :replaced_by)
          (map (fn [c] [(tr-data :title c) (span-of (:title c) (tr-data :title c))]))
          (sort-by (comp count str first) >))))))

(def card-patterns-memo (memoize card-patterns-impl))
(defn card-patterns [] (card-patterns-memo (:cards-loaded @app-state)))

(defn contains-card-pattern-impl
  "A card pattern regex, used to match a card name in text to check if the rest
  of the text should be tested as one pass is far faster than 1500 passes"
  []
  (re-pattern
    (->> (:all-cards-and-flips @app-state)
         (vals)
         (filter #(not (:replaced_by %)))
         (map (fn [k] (join "|" (map regex-escape (distinct [(:title k) (tr-data :title k)])))))
         (join "|"))))

(def contains-card-pattern-memo (memoize contains-card-pattern-impl))
(defn contains-card-pattern [] (contains-card-pattern-memo (:cards-loaded @app-state)))

(def special-patterns
  (letfn [(regex-of [icon-code] (re-pattern (str "(?i)" (regex-escape icon-code))))]
    (->> {"[hr]" [:hr]
          "[br]" [:br]
          "[!]" [:div.smallwarning "!"]}
      (map (fn [[k v]] [(regex-of k) v]))
      (sort-by (comp count str first) >))))

(defn replace-in-element
  "Given a string element, split that string on `regex`, then replace the
  matches removed by split with `replacement`. The replacement is performed by
  interleaving `replacement`s into the context and dropping the last one as
  interleave always weaves in excess"
  [element [regex replacement]]
  (if (string? element)
    (let [context (.split element regex)
          replacements (repeat replacement)]
      (->> (interleave context replacements)
           (drop-last)
           (filter not-empty)))
    [element]))

(defn replace-in-fragment
  "Map `replace-in-element` over each element of a fragment, and concatenate
  each returned fragment to flatten the sequence"
  [fragment substitution]
  (reduce concat (map #(replace-in-element % substitution) fragment)))

(defn set-react-key
  "Given a reagent-style HTML element, set the :key attribute of the element"
  [n elem]
  (let [head (first elem)
        attr (if (map? (second elem)) (second elem) {})
        tail (if (map? (second elem)) (drop 2 elem) (drop 1 elem))]
  (into [] (concat [head (merge attr {:key n})] tail))))

(defn render-fragment-impl
  "Run replacements for each [regex replacement] pair in patterns over a
  fragment, and index each HTML element in the return fragment with the :key
  attribute as required by React"
  [fragment patterns]
  (let [counter (atom 0)
        set-next-key (fn [elem] (set-react-key (do (swap! counter inc) @counter) elem))]
    (->> (reduce replace-in-fragment fragment patterns)
         (map #(if (vector? %)
                 (set-next-key %)
                 %))
         (into []))))

(def render-fragment (memoize render-fragment-impl))

(defn render-input
  "Sanitize inputs into fragments before processing them with render-fragment"
  [input patterns]
  (if (not (or (string? input) (vector? input)))
    [:<>]
    (let [fragment (if (string? input) [:<> input] input)]
      (render-fragment fragment patterns))))

(defn render-icons
  "Render all icons in a given text or HTML fragment input"
  [input]
  (render-input input icon-patterns))

(defn render-cards
  "Render all cards in a given text or HTML fragment input"
  [input]
  (render-input input (card-patterns)))

(defn render-specials
  "Render all special codes in a given text or HTML fragment input"
  [input]
  (render-input input special-patterns))

(defn render-message
  "Render icons, cards and special codes in a message"
  [input]
  (render-specials (render-icons (render-cards input))))

(defn- player-highlight-patterns-impl [corp runner]
  (letfn [(regex-of [player-name] (re-pattern (str "^" (regex-escape player-name))))]
    (->> {corp [:span.corp-username corp]
          runner [:span.runner-username runner]}
         (filter (fn [[k _]] (not-empty k)))
         (mapcat (fn [[k v]] [[(regex-of k) v]
                              [(regex-of (str "[!]" k)) [:<> [:div.smallwarning "!"] v]]]))
         (sort-by (comp count str first) >))))
(def player-highlight-patterns (memoize player-highlight-patterns-impl))

(defn render-player-highlight [message corp runner]
  (render-input message (player-highlight-patterns corp runner)))

(defn player-highlight-option-class []
  (case (get-in @app-state [:options :log-player-highlight])
    "blue-red" "log-player-highlight-red-blue"
               nil))

(defn cond-button
  [text cond f]
  (if cond
    [:button {:on-click f :key text} text]
    [:button.disabled {:key text} text]))

(defn checkbox-button [on-text off-text on-cond f]
  (if on-cond
    [:button.on {:on-click f :key on-text} on-text]
    [:button.off {:on-click f :key off-text} off-text]))

(defn tristate-button [on-text off-text on-cond disable-cond f]
  (let [text (if on-cond on-text off-text)]
    (if disable-cond
      [:button.disabled {:key text} text]
      (if on-cond
        [:button.on {:on-click f :key text} text]
        [:button.off {:on-click f :key text} text]))))

(defn notnum->zero
  "Converts a non-positive-number value to zero.  Returns the value if already a number"
  [input]
  (if (pos? (int input)) input 0))

(defn num->percent
  "Converts an input number to a percent of the second input number for display"
  [num1 num2]
  (if (zero? num2)
    "0"
    (gstring/format "%.0f" (* 100 (float (/ num1 num2))))))

(defn set-scroll-top
  "Set the scrollTop parameter of a reagent component"
  [this scroll-top]
  (let [node (rd/dom-node this)]
    (set! (.-scrollTop node) scroll-top)))

(defn store-scroll-top
  "Store the scrollTop parameter of a reagent component in an atom"
  [this scroll-top-atom]
  (let [h (.-scrollTop (rd/dom-node this))]
    (reset! scroll-top-atom h)))

(defn get-image-path
  "Image search priority: Language > Art > Resolution"
  [images lang res art]
  (or (get-in images [lang res art])
      (and (not= res :default)
           (get-in images [lang :default art])) ;; check for default res version of art
      (and (not= art :stock)
           (or (get-in images [lang res :stock]) ;; check for high res version of stock image
               (get-in images [lang :default :stock]))) ;; check for default res version of stock image
      (and (not= lang :en)
           (get-image-path images :en res art)) ;; repeat search for eng version of the art and resolution
      "/img/missing.png"))

(defn image-or-face [card]
  (cond
    (:images card) (:images card)
    (:face card) (get-in card [:faces (keyword (str (:face card))) :images])
    :else (get-in card [:faces :front :images])))

(defn time-span-string [delta]
  (let [days (Math/floor (/ delta (* 60 60 24)))
        delta (mod delta (* 60 60 24))
        hours (Math/floor (/ delta (* 60 60)))
        delta (mod delta (* 60 60))
        minutes (Math/floor (/ delta (* 60)))
        delta (mod delta 60)
        seconds (Math/floor delta)]
    (cond
      (pos? days) (str days " days, " hours " hours")
      (pos? hours) (str hours " hours, " minutes " minutes")
      (pos? minutes) (str minutes " minutes, " seconds " seconds")
      :else (str seconds " seconds"))))

(def mdy-formatter
  (-> (formatter/of-pattern "MMM d YYYY")
      (formatter/with-locale
        (some-> (gobject/get js-joda-locale "Locale")
                (gobject/get "US")))))

(def day-word-with-time-formatter
  (-> (formatter/of-pattern "EEEE, MMM d, YYYY - HH:mm")
      (formatter/with-locale
        (some-> (gobject/get js-joda-locale "Locale")
                (gobject/get "US")))))

(def ISO-ish-formatter
  (-> (formatter/of-pattern "YYYY-MM-dd, HH:mm")
      (formatter/with-locale
        (some-> (gobject/get js-joda-locale "Locale")
                (gobject/get "US")))))

(defn format-zoned-date-time
  "Formats a date string with trailing Z"
  [formatter date]
    (formatter/format formatter (zdt/parse date)))

(defn format-date-time
  "Formats a date time string into a local time string"
  [formatter date]
  (try
    (let [parsed (zdt/parse date)
          default-zone (zone/system-default)
          local-time (zdt/with-zone-same-instant parsed default-zone)]
      (formatter/format formatter local-time))
    (catch js/Object e "dunno")))
