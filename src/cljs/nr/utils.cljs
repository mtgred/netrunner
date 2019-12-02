(ns nr.utils
  (:require [clojure.string :refer [join lower-case split] :as s]
            [jinteki.cards :refer [all-cards]]))

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
   "eternal" "Eternal"
   "core-experience" "Core Experience"
   "snapshot" "Snapshot"
   "snapshot-plus" "Snapshot Plus"
   "socr" "SOCR"
   "classic" "Classic"
   "casual" "Casual"})

(def format->slug
  {"Standard" "standard"
   "Eternal" "eternal"
   "Core Experience" "core-experience"
   "Snapshot" "snapshot"
   "Snapshot Plus" "snapshot-plus"
   "SOCR" "socr"
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

(defn card-patterns-impl []
  "A sequence of card pattern pairs consisting of a regex, used to match a card
  name in text, and the span fragment that should replace it"
  (letfn [(span-of [title code] [:span {:class "fake-link" :id code} title])
          (regex-of [card-title] (re-pattern (regex-escape card-title)))]
    (->> @all-cards
         (filter #(not (:replaced_by %)))
         (map (juxt :title :code))
         (map (fn [[k v]] [(regex-of k) (span-of k v)]))
         (sort-by (comp count str first) >))))

(def card-patterns (memoize card-patterns-impl))

(def special-patterns
  (letfn [(regex-of [icon-code] (re-pattern (str "(?i)" (regex-escape icon-code))))]
    (->> {"[hr]" [:hr]
          "[br]" [:br]
          "[!]" [:div.smallwarning "!"]}
      (map (fn [[k v]] [(regex-of k) v]))
      (sort-by (comp count str first) >))))

(defn padded-interleave [pad & seqs]
  "Interleave sequences of uneven lengths by padding out the shorter ones"
  (let [lazy-padded-seqs (map #(concat % (repeat pad)) seqs)
        num-seqs (count lazy-padded-seqs)
        max-len (reduce max (map count seqs))]
    (take (* max-len num-seqs) (apply interleave lazy-padded-seqs))))

(defn replace-in-element [element [regex replacement]]
  "Given a string element, split that string on `regex`, then replace the
  matches removed by split with `replacement`. The replacement is performed by
  first counting the number of matches, then interleaving that many
  `replacement`s into the context. `padded-interleave` allows us to interleave
  sequences of varying lengths by padding out the shorter of the two sequences,
  in this case with empty strings."
  (if (string? element)
    (let [context (split element regex)
          match-count (count (re-seq regex element))
          replacements (repeat match-count replacement)]
      (->> (padded-interleave "" context replacements)
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
  (render-input input (card-patterns)))

(defn render-specials [input]
  "Render all special codes in a given text or HTML fragment input"
  (render-input input special-patterns))

(defn render-message [input]
  "Render icons, cards and special codes in a message"
  (render-specials (render-icons (render-cards input))))
