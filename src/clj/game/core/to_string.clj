(ns game.core.to-string
  (:require
   [clojure.string :as str]
   [game.core.card :refer [card-index corp? get-card get-title ice? installed? rezzed?]]
   [game.core.servers :refer [is-root? zone->name]]))

(defn card-str
  "Gets a string description of an installed card, reflecting whether it is rezzed,
  in/protecting a server, facedown, or hosted."
  ([state card] (card-str state card nil))
  ([state {:keys [zone host facedown] :as card} {:keys [visible maybe-visible]}]
  (str (if (corp? card)
         (let [installed-ice (and (ice? card) (installed? card))]
           ;; Corp card messages
           (str (cond
                  (or (rezzed? card) (:seen card) visible) (get-title card)
                  maybe-visible (str "facedown " (get-title card))
                  :else (if installed-ice "ice" "a card"))
                ;; Hosted cards do not need "in server 1" messages, host has them
                (when-not host
                  (str (cond
                         installed-ice " protecting "
                         (is-root? zone) " in the root of "
                         :else " in ")
                       ;; TODO add naming of scoring area of corp/runner
                       (zone->name (or (second zone) zone)) ;; handles [:hand] as well as [:servers :hq]
                       (when installed-ice
                         (str " at position " (card-index state card)))))))
         ;; Runner card messages
         (if (or facedown visible)
           "a facedown card"
           (get-title card)))
       (when host (str " hosted on " (card-str state (get-card state host)))))))

(defn remote-num
  "Converts a remote zone to a string"
  [zone]
  (let [kw (if (keyword? zone) zone (last zone))
        s (str kw)]
    (when (str/starts-with? s ":remote")
      (subs s 7))))

(defn card-str-edn
  "Gets a string description of an installed card, reflecting whether it is rezzed,
  in/protecting a server, facedown, or hosted."
  [state {:keys [zone facedown host visible maybe-visible] :as card}]
  (if (corp? card)
    ;; Corp card messages
    (let [location (cond
                     host :host
                     (and (ice? card) (installed? card)) :ice
                     (is-root? zone) :central
                     :else :remote)
          visibility (cond
                       (or (rezzed? card) (:seen card) visible) :seen
                       maybe-visible :known
                       :else :unknown)
          title (get-title card)
          zone' (if host
                  (let [h (first (remove #(= [:onhost] (or (second (:zone %)) (:zone %))) (iterate :host card)))]
                    (or (second (:zone h)) (:zone h)))
                  (or (second zone) zone))
          server-name (str/lower-case (zone->name zone'))
          server-n (remote-num zone')
          position (card-index state card)]
      (case #{location visibility}
        #{:host :seen} {:card/str :card-str-corp-hosted-seen
                        :title title
                        :server server-name
                        :server-n server-n}
        #{:host :known} {:card/str :card-str-corp-hosted-known
                         :title title
                         :server server-name
                         :server-n server-n}
        #{:host :unknown} {:card/str :card-str-corp-hosted-unknown
                           :server server-name
                           :server-n server-n}
        ;; "NGO Front in Server 5"
        #{:remote :seen} {:card/str :card-str-corp-installed-remote-seen
                          :title title
                          :server-n server-n}
        ;; "facedown Obokata Protocol in Server 2"
        #{:remote :known} {:card/str :card-str-corp-installed-remote-known
                           :title title
                           :server-n server-n}
        ;; "a card in Server 1"
        #{:remote :unknown} {:card/str :card-str-corp-installed-remote-unknown
                             :server-n server-n}
        ;; "Prisec in root of HQ"
        #{:central :seen} {:card/str :card-str-corp-installed-central-seen
                           :title title
                           :server server-name}
        ;; "facedown Ryon Knight in root of R&D"
        #{:central :known} {:card/str :card-str-corp-installed-central-known
                            :title title
                            :server server-name}
        ;; "a card in root of Archives"
        #{:central :unknown} {:card/str :card-str-corp-installed-central-unknown
                              :server server-name}
        ;; "Ice Wall protecting HQ at position 1"
        #{:ice :seen} {:card/str :card-str-corp-installed-ice-seen
                       :title title
                       :server server-name
                       :server-n server-n
                       :position position}
        #{:ice :known} {:card/str :card-str-corp-installed-ice-known
                        :title title
                        :server server-name
                        :server-n server-n
                        :position position}
        #{:ice :unknown} {:card/str :card-str-corp-installed-ice-unknown
                          :server server-name
                          :server-n server-n
                          :position position}))
    ;; Runner card messages
    (let [seen (or facedown visible)]
      (cond
        (and host seen) {:card/str :card-str-runner-hosted-seen
                         :title (get-title card)}
        seen {:card/str :card-str-runner-seen
              :title (get-title card)}
        host {:card/str :card-str-runner-hosted-unknown}
        :else {:card/str :card-str-runner-unknown}))))
