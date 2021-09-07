(ns nr.deck-status
  (:require
    [jinteki.validator :refer [calculate-deck-status trusted-deck-status]]
    [nr.translations :refer [tr tr-format]]
    [nr.utils :refer [slug->format]]))

(defn- build-deck-status-label [deck-status violation-details?]
  [:div.status-tooltip.blue-shade
   (doall (for [format (keys slug->format)]
            (let [{{:keys [legal reason description]} (keyword format)} deck-status]
              ^{:key format}
              [:div {:class (if legal "legal" "invalid")
                     :title (when (and violation-details?
                                       (not legal))
                              (or reason "Unknown"))}
               [:span.tick (if legal "✔" "✘")]
               description])))])

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
  (let [format (or format :standard)
        status (check-deck-status deck-status)
        message (str (tr-format (get slug->format (:format deck-status) "Standard"))
                     " "
                     (if-not (= "invalid" status) (tr [:deck-builder.legal "legal"]) (tr [:deck-builder.illegal "illegal"])))]
    [:<>
     [:span.deck-status.shift-tooltip {:class status} message
      (when tooltip?
        (build-deck-status-label deck-status violation-details?))]
     (when-let [reason (:reason ((keyword format) deck-status))]
       (when (and tooltip? (= "invalid" status))
         [:span.deck-status.shift-tooltip.invalid-explanation {:class status} (tr [:deck-builder.why "Why?"])
          [:div.status-tooltip.blue-shade [:div.invalid reason]]]))]))

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
