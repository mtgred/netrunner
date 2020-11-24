(ns nr.deck-status
  (:require
            [jinteki.validator :refer [calculate-deck-status trusted-deck-status]]
            [nr.utils :refer [slug->format]]
            ))

(defn- build-deck-status-label [deck-status violation-details?]
  [:div.status-tooltip.blue-shade
   (doall (for [[status-key {:keys [legal reason description]}] deck-status
                :when description]
            ^{:key status-key}
            [:div {:class (if legal "legal" "invalid")
                   :title (when violation-details? reason)}
             [:span.tick (if legal "✔" "✘")]
             description]))])

(defn- deck-status-details
  [deck use-trusted-info]
  (if use-trusted-info
    (trusted-deck-status deck)
    (calculate-deck-status deck)))

(defn- check-deck-status
  [deck-status]
  (let [fmt (:format deck-status)]
    (if (get-in deck-status [(keyword fmt) :legal])
      fmt "invalid")))

(defn- format-deck-status-span
  [{:keys [format] :as deck-status} tooltip? violation-details?]
  (let [status (check-deck-status deck-status)
        message (str (get slug->format (:format deck-status) "Standard")
                     " "
                     (if-not (= "invalid" status) "legal" "illegal"))]
    [:span.deck-status.shift-tooltip {:class status} message
     (when tooltip?
       (build-deck-status-label deck-status violation-details?))]))

(defn- deck-status-span-impl [deck tooltip? violation-details? use-trusted-info]
  (format-deck-status-span (deck-status-details deck use-trusted-info) tooltip? violation-details?))

(def deck-status-span-memoize (memoize deck-status-span-impl))

(defn deck-status-span
  "Returns a [:span] with standardized message and colors depending on the deck validity."
  ([deck] (deck-status-span deck false false true))
  ([deck tooltip? violation-details? use-trusted-info]
   (deck-status-span-memoize deck tooltip? violation-details? use-trusted-info)))

(defn deck-format-status-span
  "Returns a [:span] with standardized message and colors depending on the deck validity for a single format."
  [deck fmt use-trusted-info?]
  (format-deck-status-span
    (assoc (deck-status-details (assoc deck :format (:keyword fmt)) use-trusted-info?) :format fmt)
    false false))
