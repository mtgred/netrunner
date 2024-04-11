(ns game.core.servers
  "Utility functions for working with servers and zones"
  (:require
    [game.core.card :refer [get-zone]]
    [game.utils :refer [safe-split string->num]]
    [clojure.string :as string]))

(defn target-server
  "Returns the server keyword corresponding to the target of a run."
  [run]
  (first (:server run)))

(defn remote-num->name
  [num]
  (str "Server " num))

(defn remote->name
  "Converts a remote zone to a string"
  [zone]
  (let [kw (if (keyword? zone) zone (last zone))
        s (str kw)]
    (when (string/starts-with? s ":remote")
      (let [num (last (string/split s #":remote"))]
        (remote-num->name num)))))

(defn central->name
  "Converts a central zone keyword to a string."
  [zone]
  (case (if (keyword? zone) zone (last zone))
    (:hand :hq) "HQ"
    (:deck :rd) "R&D"
    (:discard :archives) "Archives"
    nil))

(defn zone->name
  "Converts a zone to a string."
  [zone]
  (or (central->name zone)
      (remote->name zone)))

(defn name-zone
  "Gets a string representation for the given zone."
  [side zone]
  (let [side (cond (= :corp side) "Corp"
                   (= :runner side) "Runner"
                   :else side)
        zone (if (keyword? zone) [zone] (vec zone))]
  (cond
    (= zone [:hand]) (if (= side "Runner") "Grip" "HQ")
    (= zone [:discard]) (if (= side "Runner") "Heap" "Archives")
    (= zone [:deck]) (if (= side "Runner") "Stack" "R&D")
    (= zone [:set-aside]) "set-aside cards"
    (= (take 1 zone) [:rig]) "Rig"
    (= (take 2 zone) [:servers :hq]) "the root of HQ"
    (= (take 2 zone) [:servers :rd]) "the root of R&D"
    (= (take 2 zone) [:servers :archives]) "the root of Archives"
    :else (zone->name (second zone)))))

(defn zone->sort-key
  [zone]
  (case (if (keyword? zone) zone (last zone))
    :archives -3
    :rd -2
    :hq -1
    (string->num
      (last (safe-split (str zone) #":remote")))))

(defn zones->sorted-names
  [zones]
  (->> zones (sort-by zone->sort-key) (map zone->name)))

(defn is-remote?
  "Returns true if the zone is for a remote server"
  [zone]
  (some? (remote->name zone)))

(defn is-central?
  "Returns true if the zone is for a central server"
  [zone]
  (not (is-remote? zone)))

(defn is-root?
  "Returns true if the zone is root a central server"
  [zone]
  (and (is-central? (second zone))
       (= :content (last zone))))

(defn central->zone
  "Converts a central server keyword like :discard into a corresponding zone vector"
  [zone]
  (case (if (keyword? zone) zone (last zone))
    :discard [:servers :archives]
    :hand [:servers :hq]
    :deck [:servers :rd]
    nil))

(defn type->rig-zone
  "Converts a runner's card type to a vector zone, e.g. 'Program' -> [:rig :program]"
  [type]
  (vec [:rig (-> type string/lower-case keyword)]))

(defn get-server-type
  [zone]
  (or (#{:hq :rd :archives} zone) :remote))

(defn same-server?
  "True if the two cards are IN or PROTECTING the same server."
  [card1 card2]
  (and card1
       card2
       (let [zone1 (get-zone card1)
             zone2 (get-zone card2)]
         (= (second zone1) (second zone2)))))

(defn protecting-same-server?
  "True if an ice is protecting the server that the card is in or protecting."
  [card ice]
  (and card
       ice
       (let [zone1 (get-zone card)
             zone2 (get-zone ice)]
         (and (= (second (or (central->zone zone1) zone1))
                 (second zone2))
              (= :ices (last zone2))))))

(defn in-same-server?
  "True if the two cards are installed IN the same server, or hosted on cards IN the same server."
  [card1 card2]
  (let [zone1 (get-zone card1)
        zone2 (get-zone card2)]
    (and card1
         card2
         (= zone1 zone2)
         (= :content (last zone1)))))

(defn from-same-server?
  "True if the upgrade is in the root of the server that the target is in."
  [upgrade target]
  (and (:cid upgrade)
       (:cid target)
       (= (central->zone (:zone target))
          (butlast (get-zone upgrade)))))

(defn unknown->kw
  "Given a string ('Archives'), a keyword corresponding to a server (:archives)
  or a zone ([:servers :archives]), return the keyword.
  NOTE: return keyword even if server does not exist."
  [name-or-kw-or-zone]
  (cond
    (keyword? name-or-kw-or-zone)
    name-or-kw-or-zone

    (string? name-or-kw-or-zone)
    (case name-or-kw-or-zone
      "HQ" :hq
      "R&D" :rd
      "Archives" :archives
      ;; assume "Server N"
      (->> (string/split name-or-kw-or-zone #" ") last (str "remote") keyword))

    :else
    (second name-or-kw-or-zone)))
