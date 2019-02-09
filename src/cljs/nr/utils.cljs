(ns nr.utils
  (:require [clojure.string :refer [join, lower-case, split] :as s]
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

(defn- dots-html
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
   "socr8" "SOCR8"
   "casual" "Casual"})

(def format->slug
  {"Standard" "standard"
   "Eternal" "eternal"
   "Core Experience" "core-experience"
   "Snapshot" "snapshot"
   "Snapshot Plus" "snapshot-plus"
   "SOCR8" "socr8"
   "Casual" "casual"})

(defn map-vals [function smap]
  (into {} (map (fn [[k v]] [k (function v)]) smap)))

(defn map-if [condition f s]
  (map #(if (condition %) (f %) %) s))

(def icon-smap
  (letfn [(span-of [icon] [:span {:class (str "anr-icon " icon)}])]
  (map-vals span-of {"[credit]" "credit"
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
                     "[t]" "trash"})))

(def card-smap
  (letfn [(unpack-non-alt-code [[title cards]] [title (:code (first cards))])
          (span-of [title code] [:span {:class "fake-link" :id code} title])]
    (->> @all-cards
         (filter #(not (:replaced_by %)))
         (group-by :title)
         (map unpack-non-alt-code)
         (map (fn [[k v]] [(lower-case k) (span-of k v)]))
         (into {}))))

(def icon-and-card-smap (merge icon-smap card-smap))

(defn ordered-keys-impl [smap]
  (sort-by (comp count first) > smap))

(def ordered-keys (memoize ordered-keys-impl))

(defn into-fragment [text]
  [:<> text])

(def regex-escape-smap
  (let [esc-chars ".*+?[](){}^$"]
    (zipmap esc-chars
            (map #(str "\\" %) esc-chars))))

(defn regex-escape [string]
  (->> string
       (replace regex-escape-smap)
       (reduce str)))

(defn padded-zip [pad s1 & sn]
  "Zip together any number of sequences of uneven lengths by padding out the
  shorter ones"
  (let [seqs (concat [s1] sn)
        lazy-padded-seqs (map #(concat % (repeat pad)) seqs)
        max-len (reduce max (map count seqs))]
    (apply map vector (map (partial take max-len) lazy-padded-seqs))))

(defn replace-in-element [element [pattern replacement]]
  "If element is a string, split that string on pattern boundaries and replace
  all patterns with the replacement"
  (if (string? element)
    (let [pattern-regex (re-pattern (str "(?i)" (regex-escape pattern)))
          pop-if-nil #(if (nil? (last %)) (pop %) %)
          context (split element pattern-regex)
          match-count (count (re-seq pattern-regex element))
          replacements (repeat match-count replacement)]
      (->> (if (empty? context)
             [replacements]
             (padded-zip "" context replacements))
           (reduce concat)
           (filter not-empty)))
    [element]))

(defn replace-in-fragment [fragment substitution]
  "Split all string elements in a fragment and replace the splitting element
  according to substitution"
  (reduce concat (map #(replace-in-element % substitution) fragment)))

(defn set-key [n elem]
  (let [head (first elem)
        attr (if (map? (second elem)) (second elem) {})
        tail (if (map? (second elem)) (drop 2 elem) (drop 1 elem))]
  (into [] (concat [head (merge attr {:key n})] tail))))

(defn render-fragment [fragment replacement-smap]
  "Given a fragment, shallowly replaces text in the fragment with icon and,
  optionally, card preview HTML"
  (let [counter (atom 0)
        set-next-key (fn [elem] (set-key (do (swap! counter inc) @counter) elem))]
    (->> (reduce replace-in-fragment fragment (ordered-keys replacement-smap))
         (replace replacement-smap)
         (map-if vector? set-next-key)
         (into []))))

(defn render-icons [input]
  (if (string? input)
    (render-fragment [:<> input] icon-smap)
    (render-fragment input icon-smap)))

(defn render-icons-and-cards [input]
  (if (string? input)
    (render-fragment [:<> input] icon-and-card-smap)
    (render-fragment input icon-and-card-smap)))
