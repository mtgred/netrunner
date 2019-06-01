(in-ns 'game.core)

(def trash-program {:prompt "Select a program to trash"
                    :label "Trash a program"
                    :msg (msg "trash " (:title target))
                    :choices {:req #(and (installed? %)
                                         (is-type? % "Program"))}
                    :effect (effect (trash target {:cause :subroutine})
                                    (clear-wait-prompt :runner))})

(def trash-hardware {:prompt "Select a piece of hardware to trash"
                     :label "Trash a piece of hardware"
                     :msg (msg "trash " (:title target))
                     :choices {:req #(and (installed? %)
                                          (is-type? % "Hardware"))}
                     :effect (effect (trash target {:cause :subroutine}))})

(def trash-resource-sub {:prompt "Select a resource to trash"
                         :label "Trash a resource"
                         :msg (msg "trash " (:title target))
                         :choices {:req #(and (installed? %)
                                              (is-type? % "Resource"))}
                         :effect (effect (trash target {:cause :subroutine}))})

(def trash-installed {:prompt "Select an installed card to trash"
                      :player :runner
                      :label "Force the Runner to trash an installed card"
                      :msg (msg "force the Runner to trash " (:title target))
                      :choices {:req #(and (installed? %)
                                           (= (:side %) "Runner"))}
                      :effect (effect (trash target {:cause :subroutine}))})

(def corp-rez-toast
  "Effect to be placed with `:runner-turn-ends` to remind players of 'when turn begins'
  triggers"
  {:effect (req (toast state :corp "Reminder: You have unrezzed cards with \"when turn begins\" abilities." "info"))})

(declare reorder-final) ; forward reference since reorder-choice and reorder-final are mutually recursive

(defn reorder-choice
  "Generates a recursive prompt structure for cards that do reordering (Indexing, Making an Entrance, etc.)

  reorder-side is the side to be reordered, i.e. :corp for Indexing and Precognition.
  wait-side is the side that has a wait prompt while ordering is in progress, i.e. :corp for Indexing and Spy Camera.

  This is part 1 - the player keeps choosing cards until there are no more available choices. A wait prompt should
  exist before calling this function. See Indexing and Making an Entrance for examples on how to call this function."

  ([reorder-side cards] (reorder-choice reorder-side (other-side reorder-side) cards `() (count cards) cards nil))
  ([reorder-side wait-side remaining chosen n original] (reorder-choice reorder-side wait-side remaining chosen n original nil))
  ([reorder-side wait-side remaining chosen n original dest]
  {:prompt (str "Select a card to move next "
                (if (= dest "bottom") "under " "onto ")
                (if (= reorder-side :corp) "R&D" "your Stack"))
   :choices remaining
   :async true
   :effect (req (let [chosen (cons target chosen)]
                  (if (< (count chosen) n)
                    (continue-ability
                      state side
                      (reorder-choice reorder-side wait-side (remove-once #(= target %) remaining) chosen n original dest)
                      card nil)
                    (continue-ability
                      state side
                      (reorder-final reorder-side wait-side chosen original dest)
                      card nil))))}))

(defn- reorder-final
  "Generates a recursive prompt structure for cards that do reordering (Indexing, Making an Entrance, etc.)
  This is part 2 - the player is asked for final confirmation of the reorder and is provided an opportunity to start over."

  ([reorder-side wait-side chosen original] (reorder-final reorder-side wait-side chosen original nil))
  ([reorder-side wait-side chosen original dest]
   {:prompt (if (= dest "bottom")
              (str "The bottom cards of " (if (= reorder-side :corp) "R&D" "your Stack")
                   " will be " (join  ", " (map :title (reverse chosen))) ".")
              (str "The top cards of " (if (= reorder-side :corp) "R&D" "your Stack")
                   " will be " (join  ", " (map :title chosen)) "."))
   :choices ["Done" "Start over"]
   :async true
   :effect (req
             (cond
               (and (= dest "bottom") (= target "Done"))
               (do (swap! state update-in [reorder-side :deck]
                          #(vec (concat (drop (count chosen) %) (reverse chosen))))
                   (clear-wait-prompt state wait-side)
                   (effect-completed state side eid))

               (= target "Done")
               (do (swap! state update-in [reorder-side :deck]
                          #(vec (concat chosen (drop (count chosen) %))))
                   (clear-wait-prompt state wait-side)
                   (effect-completed state side eid))

               :else
               (continue-ability state side (reorder-choice reorder-side wait-side original '() (count original) original dest) card nil)))}))

(defn swap-ice
  "Swaps two pieces of ICE."
  [state side a b]
  (let [a-index (ice-index state a)
        b-index (ice-index state b)
        a-new (assoc a :zone (:zone b))
        b-new (assoc b :zone (:zone a))]
    (swap! state update-in (cons :corp (:zone a)) #(assoc % a-index b-new))
    (swap! state update-in (cons :corp (:zone b)) #(assoc % b-index a-new))
    (doseq [newcard [a-new b-new]]
      (unregister-events state side newcard)
      (when (rezzed? newcard)
        (register-events state side (:events (card-def newcard)) newcard))
      (doseq [h (:hosted newcard)]
        (let [newh (-> h
                       (assoc-in [:zone] '(:onhost))
                       (assoc-in [:host :zone] (:zone newcard)))]
          (update! state side newh)
          (unregister-events state side h)
          (when (rezzed? h)
            (register-events state side (:events (card-def newh)) newh)))))
    (update-ice-strength state side a-new)
    (update-ice-strength state side b-new)))

(defn card-index
  "Get the zero-based index of the given card in its server's list of content. Same as ice-index"
  [state card]
  (first (keep-indexed #(when (= (:cid %2) (:cid card)) %1) (get-in @state (cons :corp (:zone card))))))

(defn swap-installed
  "Swaps two installed corp cards - like swap ICE except no strength update"
  [state side a b]
  (let [a-index (card-index state a)
        b-index (card-index state b)
        a-new (assoc a :zone (:zone b))
        b-new (assoc b :zone (:zone a))]
    (swap! state update-in (cons :corp (:zone a)) #(assoc % a-index b-new))
    (swap! state update-in (cons :corp (:zone b)) #(assoc % b-index a-new))
    (doseq [newcard [a-new b-new]]
      (doseq [h (:hosted newcard)]
        (let [newh (-> h
                       (assoc-in [:zone] '(:onhost))
                       (assoc-in [:host :zone] (:zone newcard)))]
          (update! state side newh)
          (unregister-events state side h)
          (register-events state side (:events (card-def newh)) newh))))))

(defn do-net-damage
  "Do specified amount of net-damage."
  [dmg]
  {:label (str "Do " dmg " net damage")
   :async true
   :msg (str "do " dmg " net damage")
   :effect (effect (damage eid :net dmg {:card card}))})

(defn do-meat-damage
  "Do specified amount of meat damage."
  [dmg]
  {:label (str "Do " dmg " meat damage")
   :async true
   :msg (str "do " dmg " meat damage")
   :effect (effect (damage eid :meat dmg {:card card}))})

(defn do-brain-damage
  "Do specified amount of brain damage."
  [dmg]
  {:label (str "Do " dmg " brain damage")
   :async true
   :msg (str "do " dmg " brain damage")
   :effect (effect (damage eid :brain dmg {:card card}))})

(defn pick-virus-counters-to-spend
  "Pick virus counters to spend. For use with Freedom Khumalo and virus breakers, and any other relevant cards.
  This function returns a map for use with resolve-ability or continue-ability.
  The ability triggered returns either {:number n :msg msg} on completed effect, or :cancel on a cancel.
  n is the number of virus counters selected, msg is the msg string of all the cards and the virus counters taken from each.
  If called with no arguments, allows user to select as many counters as they like until 'Cancel' is pressed."
  ([] (pick-virus-counters-to-spend nil (hash-map) 0))
  ([target-count] (pick-virus-counters-to-spend target-count (hash-map) 0 ))
  ([target-count selected-cards counter-count]
   {:async true
    :prompt (str "Select a card with virus counters ("
                 counter-count (when (and target-count (pos? target-count))
                                 (str " of " target-count))
                 " virus counters)")
    :choices {:req #(and (installed? %)
                         (pos? (get-counters % :virus)))}
    :effect (req (add-counter state :runner target :virus -1)
                 (let [selected-cards (update selected-cards (:cid target)
                                              ;; Store card reference and number of counters picked
                                              ;; Overwrite card reference each time
                                              #(assoc % :card target :number (inc (:number % 0))))
                       counter-count (inc counter-count)]
                   (if (or (not target-count) (< counter-count target-count))
                     (continue-ability state side
                                       (pick-virus-counters-to-spend target-count selected-cards counter-count)
                                       card nil)
                     (let [msg (join ", " (map #(let [{:keys [card number]} %
                                                      title (:title card)]
                                                  (str (quantify number "virus counter") " from " title))
                                               (vals selected-cards)))]
                       (effect-completed state side (make-result eid {:number counter-count :msg msg}))))))
    :cancel-effect (if target-count
                     (req (doseq [{:keys [card number]} (vals selected-cards)]
                            (add-counter state :runner (get-card state card) :virus number))
                          (effect-completed state side (make-result eid :cancel)))
                     (req (let [msg (join ", " (map #(let [{:keys [card number]} %
                                                      title (:title card)]
                                                  (str (quantify number "virus counter") " from " title))
                                               (vals selected-cards)))]
                           (effect-completed state side (make-result eid {:number counter-count :msg msg})))))}))

(defn pick-credit-providing-cards
  "Similar to pick-virus-counters-to-spend. Works on :recurring and normal credits."
  ([provider-func] (pick-credit-providing-cards provider-func (hash-map) 0 nil))
  ([provider-func target-count] (pick-credit-providing-cards provider-func (hash-map) 0 target-count))
  ([provider-func selected-cards counter-count target-count]
   (let [providers-cid (map :cid (provider-func))]
         {:async true
          :prompt (str "Select a credit providing card ("
                       counter-count (when (and target-count (pos? target-count))
                                       (str " of " target-count))
                       " credits)")
          :choices {:req #(and (in-coll? providers-cid (:cid %))
                               (case (-> % card-def :interactions :pay-credits :type)
                                 :recurring
                                 (pos? (get-counters % :recurring))

                                 :credit
                                 (pos? (get-counters % :credit))

                                 :custom
                                 true))}
          :effect (req (let [pay-credits-type (-> target card-def :interactions :pay-credits :type)
                             gained-credits (case pay-credits-type
                                              :recurring
                                              (or (add-prop state side target :rec-counter -1) 1)

                                              :credit
                                              (or (add-counter state side target :credit -1) 1)

                                              :custom ;custom functions should return the number of credits provided
                                              (or ((-> target card-def :interactions :pay-credits :custom) state side eid card nil) 1))
                             selected-cards (update selected-cards (:cid target)
                                                    ;; Store card reference and number of counters picked
                                                    ;; Overwrite card reference each time
                                                    #(assoc % :card target :number (inc (:number % 0))))
                             counter-count (+ counter-count gained-credits)
                             next-iteration (fn []
                                              (if (or (not target-count) (< counter-count target-count))
                                                (continue-ability state side
                                                                  (pick-credit-providing-cards provider-func selected-cards counter-count target-count)
                                                                  card nil)
                                                (let [msg (join ", " (map #(let [{:keys [card number]} %
                                                                                 title (:title card)]
                                                                             (str (quantify number "credit") " from " title))
                                                                          (vals selected-cards)))]
                                                  (effect-completed state side (make-result eid {:number counter-count :msg msg})))))]
                         (if (has-subtype? target "Stealth")
                           (wait-for (trigger-event-sync state side (make-eid state eid) :spent-stealth-credit target)
                                     (next-iteration))
                           (next-iteration))))
          :cancel-effect (req (effect-completed state side 0))}))) ; ToDo

(defn never?
  "Returns true if is argument is :never."
  [x]
  (= :never x))

(defn set-autoresolve
  "Makes a card ability which lets the user toggle auto-resolve on an ability. Setting is stored under [:special toggle-kw]."
  [toggle-kw ability-name]
  {:label (str "Toggle auto-resolve on " ability-name)
   :prompt (str "Set auto-resolve on " ability-name " to:")
   :choices ["Always" "Never" "Ask"]
   :effect (effect (update! (update-in card [:special toggle-kw] (fn [x] (keyword (lower-case target)))))
                   (toast (str "From now on, " ability-name " will "
                               ({:always "always" :never "never" :ask "ask whether it should"}
                                (get-in (get-card state card) [:special toggle-kw]))
                               " resolve.") "info"))})

(defn get-autoresolve
  "Returns a 5-fn intended for use in the :autoresolve of an optional ability. Function returns 'Yes', 'No' or nil
  depending on whether card has [:special toggle-kw] set to :always, :never or something else.
  If a function is passed in, instead call that on [:special toggle-kw] and return the result."
  ([toggle-kw] (get-autoresolve toggle-kw {:always "Yes" :never "No"}))
  ([toggle-kw pred] (req (pred (get-in (get-card state card) [:special toggle-kw])))))

(defn get-strength
  [card]
  (or (:current-strength card)
      (:strength card)))

(defn count-num-subroutines
  "Count the number of subroutines for Hernando Cortez. Note that this only
  counts subroutines that exist as part of the card text. It does not count
  subroutines that the card will gain once rezzed."
  [card]
  (->> (-> card :title server-card :text split-lines)
       (filter #(starts-with? % "[subroutine]"))
       count))

;; Load all card definitions into the current namespace
(defn load-all-cards
  "Load all card definitions into their own namespaces"
  ([] (load-all-cards nil))
  ([path]
   (doall (pmap load-file
                (->> (io/file (str "src/clj/game/cards" (when path (str "/" path ".clj"))))
                     (file-seq)
                     (filter #(and (.isFile %)
                                   (string/ends-with? % ".clj")))
                     (map str))))))

(defn get-card-defs
  ([] (get-card-defs nil))
  ([path]
   (->> (all-ns)
        (filter #(starts-with? % (str "game.cards" (when path (str "." path)))))
        (map #(ns-resolve % 'card-definitions))
        (map var-get)
        (apply merge))))

(def cards {})

(defn reset-card-defs
  "Performs any once only initialization that should be performed on startup"
  ([] (reset-card-defs nil))
  ([path]
   (let [cards-var #'game.core/cards]
     (alter-var-root cards-var
                     (constantly
                       (merge cards
                              (do (load-all-cards path)
                                  (get-card-defs path))))))
   'loaded))
