(ns nr.utils
  (:require [clojure.string :refer [join lower-case split] :as s]
            [reagent.dom :as rd]
            [goog.string :as gstring]
            [goog.string.format]
            [medley.core :refer [find-first]]
            [nr.appstate :refer [app-state]]))

;; Dot definitions
(def zws "\u200B")                  ; zero-width space for wrapping dots
(def influence-dot (str "●" zws))   ; normal influence dot
(def banned-dot (str "✘" zws))      ; on the banned list
(def restricted-dot (str "🦄" zws)) ; on the restricted list
(def alliance-dot (str "○" zws))    ; alliance free-inf dot
(def rotated-dot (str "↻" zws))     ; on the rotation list

(def banned-span
  [:span.invalid {:title "Removed"} " " banned-dot])

(def restricted-span
  [:span {:title "Restricted"} " " restricted-dot])

(def rotated-span
  [:span.casual {:title "Rotated"} " " rotated-dot])

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
                                 :title id}])]
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
   "eternal" "Eternal"
   "snapshot" "Snapshot"
   "snapshot-plus" "Snapshot Plus"
   "classic" "Classic"
   "casual" "Casual"})

(def format->slug
  {"Standard" "standard"
   "System Gateway" "system-gateway"
   "Startup" "startup"
   "Eternal" "eternal"
   "Snapshot" "snapshot"
   "Snapshot Plus" "snapshot-plus"
   "Classic" "classic"
   "Casual" "casual"})

(defn regex-escape [string]
  "Escape characters in a string which have special meanings in regexes"
  (let [special-chars ".*+?[](){}^$"
        escaped-chars (map #(str "\\" %) special-chars)
        regex-escape-smap (zipmap special-chars escaped-chars)]
    (->> string
         (replace regex-escape-smap)
         (reduce str))))

(def icon-patterns
  "A sequence of icon pattern pairs consisting of an regex, used to match icon
  codes, and the span fragment that should replace it"
  (letfn [(span-of [icon] [:span {:class (str "anr-icon " icon)}])
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
  (letfn [(span-of [title code] [:span {:class "fake-link" :data-card-title title} title])]
    (->> (:all-cards-and-flips @app-state)
      (vals)
      (filter #(not (:replaced_by %)))
      (map (juxt :title :code))
      (map (fn [[k v]] [k (span-of k v)]))
      (sort-by (comp count str first) >))))

(def card-patterns (memoize card-patterns-impl))

(defn contains-card-pattern-impl
  "A card pattern regex, used to match a card name in text to check if the rest
  of the text should be tested as one pass is far faster than 1500 passes"
  []
  (re-pattern
    (->> (:all-cards-and-flips @app-state)
         (vals)
         (filter #(not (:replaced_by %)))
         (map (fn [k] (regex-escape (:title k))))
         (join "|"))))

(def contains-card-pattern (memoize contains-card-pattern-impl))

(def special-patterns
  (letfn [(regex-of [icon-code] (re-pattern (str "(?i)" (regex-escape icon-code))))]
    (->> {"[hr]" [:hr]
          "[br]" [:br]
          "[!]" [:div.smallwarning "!"]}
      (map (fn [[k v]] [(regex-of k) v]))
      (sort-by (comp count str first) >))))

(defn replace-in-element [element [regex replacement]]
  "Given a string element, split that string on `regex`, then replace the
  matches removed by split with `replacement`. The replacement is performed by
  interleaving `replacement`s into the context and dropping the last one as
  interleave always weaves in excess"
  (if (string? element)
    (let [context (.split element regex)
          replacements (repeat replacement)]
      (->> (interleave context replacements)
           (drop-last)
           (filter not-empty)))
    [element]))

(defn replace-in-fragment [fragment substitution]
  "Map `replace-in-element` over each element of a fragment, and concatenate
  each returned fragment to flatten the sequence"
  (reduce concat (map #(replace-in-element % substitution) fragment)))

(defn set-react-key [n elem]
  "Given a reagent-style HTML element, set the :key attribute of the element"
  (let [head (first elem)
        attr (if (map? (second elem)) (second elem) {})
        tail (if (map? (second elem)) (drop 2 elem) (drop 1 elem))]
  (into [] (concat [head (merge attr {:key n})] tail))))

(defn render-fragment-impl [fragment patterns]
  "Run replacements for each [regex replacement] pair in patterns over a
  fragment, and index each HTML element in the return fragment with the :key
  attribute as required by React"
  (let [counter (atom 0)
        set-next-key (fn [elem] (set-react-key (do (swap! counter inc) @counter) elem))]
    (->> (reduce replace-in-fragment fragment patterns)
         (map #(if (vector? %)
                 (set-next-key %)
                 %))
         (into []))))

(def render-fragment (memoize render-fragment-impl))

(defn render-input [input patterns]
  "Sanitize inputs into fragments before processing them with render-fragment"
  (if (not (or (string? input) (vector? input)))
    [:<>]
    (let [fragment (if (string? input) [:<> input] input)]
      (render-fragment fragment patterns))))

(defn render-icons [input]
  "Render all icons in a given text or HTML fragment input"
  (render-input input icon-patterns))

(defn render-cards [input]
  "Render all cards in a given text or HTML fragment input"
  (if (re-find (contains-card-pattern) (or input ""))
    (render-input input (card-patterns))
    (if (not (or (string? input) (vector? input)))
      [:<>]
      (if (string? input) [:<> input] input))))

(defn render-specials [input]
  "Render all special codes in a given text or HTML fragment input"
  (render-input input special-patterns))

(defn render-message [input]
  "Render icons, cards and special codes in a message"
  (if (string? input)
    (render-specials (render-icons (render-cards input)))
    input))

(defn cond-button [text cond f]
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

(defn non-game-toast
  "Display a toast warning with the specified message."
  [msg type options]
  (set! (.-options js/toastr) (toastr-options options))
  (let [f (aget js/toastr type)]
    (f msg)))

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
  [images lang res art]
  (let [path (get-in images [lang res art])]
    (cond
      path path
      (not= art :stock) (get-image-path images lang res :stock)
      (not= res :default) (get-image-path images lang :default art)
      (not= lang :en) (get-image-path images :en res art)
      :else "/img/missing.png")))

(defn image-or-face [card]
  (cond
    (:images card) (:images card)
    (:face card) (get-in card [:faces (keyword (str (:face card))) :images])
    :else (get-in card [:faces :front :images])))
