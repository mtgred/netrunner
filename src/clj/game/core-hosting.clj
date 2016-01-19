(in-ns 'game.core)

(defn get-nested-host
  "Recursively searches upward to find the 'root' card of a hosting chain."
  [state card]
  (if (:host card) (recur state (:host card)) card))

(defn update-hosted!
  "Updates a card that is hosted on another, by recursively updating the host card's
  :hosted vector."
  [state side {:keys [cid] :as card}]
  (if-let [h (get-card state (:host card))]
    (recur state side (let [[head tail] (split-with #(not= (:cid %) cid) (:hosted h))]
                        (assoc h :hosted (vec (concat head [card] (rest tail))))))
    (when-not (:host card)
      (update! state side card))))

(defn remove-from-host
  "Removes a card from its host."
  [state side {:keys [cid] :as card}]
  (let [host-card (get-card state (:host card))]
    (update-hosted! state side (update-in host-card [:hosted] (fn [coll] (remove-once #(not= (:cid %) cid) coll))))))

(defn get-card-hosted
  "Finds the current version of the given card by finding its host."
  [state {:keys [cid zone side host] :as card}]
  (let [root-host (get-card state (get-nested-host state card))
        helper (fn search [card target]
                 (when-not (nil? card)
                   (if-let [c (some #(when (= (:cid %) (:cid target)) %) (:hosted card))]
                     c
                     (some #(when-let [s (search % target)] s) (:hosted card)))))]
    (helper root-host card)))

(defn assoc-host-zones
  "Associates a new zone onto a card and its host(s)."
  [card]
  (let [card (update-in card [:zone] #(map to-keyword %))]
    (if (:host card)
      (update-in card [:host] assoc-host-zones)
      card)))

(defn host
  "Host the target onto the card."
  ([state side card target] (host state side card target nil))
  ([state side card {:keys [zone cid host installed] :as target} {:keys [facedown] :as options}]
   (when (not= cid (:cid card))
     (when installed
       (unregister-events state side target))
     (doseq [s [:runner :corp]]
       (if host
         (when-let [host-card (some #(when (= (:cid host) (:cid %)) %)
                                    (get-in @state (cons s (vec (map to-keyword (:zone host))))))]
           (update! state side (update-in host-card [:hosted]
                                          (fn [coll] (remove-once #(not= (:cid %) cid) coll)))))
         (swap! state update-in (cons s (vec zone))
                (fn [coll] (remove-once #(not= (:cid %) cid) coll)))))
     (swap! state update-in (cons side (vec zone)) (fn [coll] (remove-once #(not= (:cid %) cid) coll)))
     (let [card (assoc-host-zones card)
           c (assoc target :host (dissoc card :hosted)
                           :facedown facedown
                           :zone '(:onhost) ;; hosted cards should not be in :discard or :hand etc
                           :previous-zone (:zone target))]
       (update! state side (update-in card [:hosted] #(conj % c)))
       (when-let [events (:events (card-def target))]
         (when installed
           (register-events state side events c)))
       (when (and installed (:recurring (card-def c)))
         (card-init state side c false))
       (when-let [events (:events (card-def target))]
         (when (and installed (:recurring (card-def c)))
           (unregister-events state side target)
           (register-events state side events c)))
       c))))
