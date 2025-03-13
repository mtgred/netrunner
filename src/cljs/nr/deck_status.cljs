(ns nr.deck-status
  (:require
    [jinteki.validator :refer [calculate-deck-status trusted-deck-status]]
    [nr.translations :refer [tr tr-format]]
    [nr.utils :refer [slug->format]]
    [clojure.string :as str]))

(defn- build-deck-status-label [deck-status violation-details?]
  [:div.status-tooltip.blue-shade
   (doall (for [fmt (keys slug->format)]
            (let [{:keys [legal reason description]} (get deck-status (keyword fmt))]
              ^{:key fmt}
              [:div {:class (if legal "legal" "invalid")
                     :title (when (and violation-details?
                                       (not legal))
                              (or reason "Unknown"))}
               [:span.tick (if legal "✔" "✘")]
               (tr-format (get slug->format fmt)) description])))])

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
        message [:<> (tr-format (get slug->format (:format deck-status) "Standard"))
                     " "
                     (if (= "invalid" status)
                       [tr [:deck-builder_illegal "illegal"]]
                       [tr [:deck-builder_legal "legal"]])]]
    [:<>
     [:span.deck-status.shift-tooltip {:class status} message
      (when tooltip?
        (build-deck-status-label deck-status violation-details?))]
     (when-let [reason (:reason ((keyword format) deck-status))]
       (when (and tooltip? (= "invalid" status))
         [:span.deck-status.shift-tooltip.invalid-explanation
          {:class status}
          [tr [:deck-builder_why "Why?"]]
          [:span.status-tooltip.blue-shade
           [:span.invalid reason]]]))]))

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
