(ns game.core.board
  (:require [clojure.string :as string]
            [game.core.card :refer [agenda? asset? corp? facedown? get-counters installed? is-type? rezzed? runner?]]
            [game.core.card-defs :refer [card-def]]
            [game.core.eid :refer [make-eid]]
            [game.core.servers :refer [is-remote? zones->sorted-names]]
            [game.core.state :refer [make-rid]]
            [game.utils :refer [to-keyword]]))

(defn all-installed
  "Returns a vector of all installed cards for the given side, including those hosted on other cards,
  but not including 'inactive hosting' like Personal Workshop."
  [state side]
  (if (= side :runner)
    (let [top-level-cards (flatten (for [t [:program :hardware :resource :facedown]] (get-in @state [:runner :rig t])))
          hosted-on-ice (->> (:corp @state) :servers seq flatten (mapcat :ices) (mapcat :hosted))]
      (loop [unchecked (concat top-level-cards (filter runner? hosted-on-ice)) installed ()]
        (if (empty? unchecked)
          (filter installed? installed)
          (let [[card & remaining] unchecked]
            (recur (filter identity (into remaining (:hosted card))) (into installed [card]))))))
    (let [servers (->> (:corp @state) :servers seq flatten)
          content (mapcat :content servers)
          ice (mapcat :ices servers)
          top-level-cards (concat ice content)]
      (loop [unchecked top-level-cards installed ()]
        (if (empty? unchecked)
          (filter corp? installed)
          (let [[card & remaining] unchecked]
            (recur (filter identity (into remaining (:hosted card))) (into installed [card]))))))))

(defn get-all-installed
  "Returns a list of all installed cards"
  [state]
  (concat (all-installed state :corp) (all-installed state :runner)))

(defn all-installed-runner-type
  "Returns a list of all installed, non-facedown runner cards of the requested type."
  [state card-type]
  (filter (every-pred #(is-type? % card-type) (complement facedown?)) (all-installed state :runner)))

(defn all-active-installed
  "Returns a vector of active AND installed cards for the given side. This is all face-up installed cards."
  [state side]
  (let [installed (all-installed state side)]
   (if (= side :runner)
     (remove facedown? installed)
     (filter rezzed? installed))))

(defn all-active
  "Returns a vector of all active cards for the given side. Active cards are either installed, the identity,
  currents, or the corp's scored area."
  [state side]
  (if (= side :runner)
    (cons (get-in @state [:runner :identity]) (concat (get-in @state [:runner :current])
                                                      (all-active-installed state side)
                                                      (get-in @state [:runner :play-area])))
    (cons (get-in @state [:corp :identity]) (remove :disabled
                                                    (concat (all-active-installed state side)
                                                            (get-in @state [:corp :current])
                                                            (get-in @state [:corp :scored])
                                                            (get-in @state [:corp :play-area]))))))

(defn installed-byname
  "Returns a truthy card map if a card matching title is installed"
  [state side title]
  (some #(when (= (:title %) title) %) (all-active-installed state side)))

(defn in-play?
  "Returns a truthy card map if the given card is in play (installed)."
  [state card]
  (installed-byname state (to-keyword (:side card)) (:title card)))

;; zone stuff
(defn get-zones [state]
  (keys (get-in @state [:corp :servers])))

(defn get-remote-zones [state]
  (filter is-remote? (get-zones state)))

(defn get-remotes [state]
  (select-keys (get-in @state [:corp :servers]) (get-remote-zones state)))

(defn get-remote-names [state]
  (zones->sorted-names (get-remote-zones state)))

(defn server-list
  "Get a list of all servers (including centrals)"
  [state]
  (zones->sorted-names (get-zones state)))

(defn installable-servers
  "Get list of servers the specified card can be installed in"
  [state card]
  (let [base-list (concat (server-list state) ["New remote"])]
    (if-let [install-req (-> card card-def :install-req)]
      ;; Install req function overrides normal list of install locations
      (install-req state :corp card (make-eid state) base-list)
      ;; Standard list
      (if (or (agenda? card)
              (asset? card))
        (remove #{"HQ" "R&D" "Archives"} base-list)
        base-list))))

(defn server->zone [state server]
  (if (sequential? server)
    (vec (cons :servers server))
    (case server
      "HQ" [:servers :hq]
      "R&D" [:servers :rd]
      "Archives" [:servers :archives]
      "New remote" [:servers (keyword (str "remote" (make-rid state)))]
      [:servers (->> (string/split server #" ") last (str "remote") keyword)])))

(defn card->server
  "Returns the server map that this card is installed in or protecting."
  [state card]
  (let [z (:zone card)]
    (get-in @state [:corp :servers (second z)])))
