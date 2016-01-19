(in-ns 'game.core)

; Stuff that doesn't go in other files.

; No idea what these are for. @justinliew?
(defn copy-events [state side dest source]
  (let [source-def (card-def source)
        source-events (or (:events source-def) {})
        dest-card (merge dest {:events source-events})]
    (update! state side dest-card)
    (register-events state side (:events dest-card) dest-card)))

(defn copy-abilities [state side dest source]
  (let [source-def (card-def source)
        source-abilities (or (:abilities source-def) ())
        ; i think this just copies some bare minimum of info, and relies on the card-def to supply the actual data. We may have to copy the entire thing and conj {:dynamic :something} into it so we handle it as a full dynamic ability
        source-abilities (for [ab source-abilities]
                           (assoc (select-keys ab [:cost :pump :breaks])
                             :label (or (:label ab) (and (string? (:msg ab)) (capitalize (:msg ab))) "")
                             :dynamic :copy))
        dest-card (merge dest {:abilities source-abilities :source source})]
    (update! state side dest-card)))

; I'm making the assumption that we only copy effects if there is a :leave-play effect, as these are persistent effects as opposed to a one-time scored effect.
; think Mandatory Upgrades vs. Improved Tracers
(defn copy-leave-play-effects [state side dest source]
  (let [source-def (card-def source)]
    (when-let [source-leave-play (:leave-play source-def)]
      (let [source-effect (:effect source-def)
            dest-card (merge dest {:source-leave-play source})]
        (when-not (nil? source-effect) (source-effect state side source nil))
        (update! state side dest-card)))))

(defn fire-leave-play-effects [state side card]
  (when-let [source-leave-play (:source-leave-play card)]
    (let [source-def (card-def source-leave-play)
          leave-effect (:leave-play source-def)]
      (when-not (nil? leave-effect) (leave-effect state side card nil)))))


(defn get-remotes [state]
  (filter #(-> % first is-remote?) (get-in state [:corp :servers])))

(defn get-remote-names [state]
  (->> state get-remotes (map (comp zone->name first)) sort))

(defn server-list [state card]
  (let [remotes (cons "New remote" (get-remote-names @state))]
    (if (#{"Asset" "Agenda"} (:type card))
      remotes
      (concat ["HQ" "R&D" "Archives"] remotes))))

(defn server->zone [state server]
  (if (sequential? server)
    (vec (cons :servers server))
    (case server
      "HQ" [:servers :hq]
      "R&D" [:servers :rd]
      "Archives" [:servers :archives]
      "New remote" [:servers (keyword (str "remote" (make-rid state)))]
      [:servers (->> (split server #" ") last (str "remote") keyword)])))

(defn all-installed
  "Returns a vector of all installed cards for the given side, including those hosted on other cards,
  but not including 'inactive hosting' like Personal Workshop."
  [state side]
  (if (= side :runner)
    (let [installed (flatten (for [t [:program :hardware :resource]] (get-in @state [:runner :rig t])))]
      (concat installed (filter :installed (mapcat :hosted installed))))
    (let [servers (->> (:corp @state) :servers seq flatten)]
      (concat (mapcat :content servers) (mapcat :ices servers)))))

(defn installed-byname
  "Returns a truthy card map if a card matching title is installed"
  [state side title]
  (some #(when (= (:title %) title) %) (all-installed state side)))

(defn in-play?
  "Returns a truthy card map if the given card is in play (installed)."
  [state card]
  (installed-byname state (to-keyword (:side card)) (:title card)))
