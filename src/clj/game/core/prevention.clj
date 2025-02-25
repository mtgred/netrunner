(ns game.core.prevention
  (:require
   [game.core.board :refer [all-active]]
   [game.core.card :refer [get-card rezzed? same-card?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.choose-one :refer [choose-one-helper]]
   [game.core.cost-fns :refer [card-ability-cost]]
   [game.core.eid :refer [complete-with-result effect-completed]]
   [game.core.effects :refer [any-effects]]
   [game.core.engine :refer [resolve-ability trigger-event-simult trigger-event-sync]]
   [game.core.payment :refer [can-pay?]]
   [game.core.to-string :refer [card-str]]
   [game.utils :refer [dissoc-in enumerate-str quantify]]
   [game.macros :refer [msg req wait-for]]
   [jinteki.utils :refer [other-side]]))

;; so how is this going to work?
;; each player, starting with the active player, gets a chance to prevent effects
;; we get a list of all cards that have prevention effects, and create a prompt with all the specific prevention abilities, along with:
;;  * are they repeatable?
;;  * the source card
;;  * is it an ability, an interrupt, or a triggered event?

(defn- relevant-prevention-abilities
  "selects all prevention abilities which are:
   1) relevant to the context
   2) playable (navi mumbai + req)
   3) the player can afford to pay for
   4) haven't been used too many times (ie net shield, prana condenser)"
  [state side eid key card]
  (let [abs (filter #(= (:prevents %) key) (:prevention (card-def card)))
        with-card (map #(assoc % :card card) abs)
        playable? (filter #(let [cannot-play? (and (= (:type %) :ability)
                                                   (any-effects state side :prevent-paid-ability true? card [(:ability %) 0]))
                                 payable? (can-pay? state side eid card nil (seq (card-ability-cost state side (:ability %) card [])))
                                 ;; todo - account for card being disabled
                                 not-used-too-many-times? (or (not (:max-uses %))
                                                          (not (get-in @state [:prevent key :uses (:cid card)]))
                                                          (< (get-in @state [:prevent key :uses (:cid card)]) (:max-uses %)))
                                 ability-req? (or (not (get-in % [:ability :req]))
                                                  ((get-in % [:ability :req]) state side eid card nil))]
                             (and (not cannot-play?) payable? not-used-too-many-times? ability-req?))
                          abs)]
    (seq (map #(assoc % :card card) playable?))))

(defn- gather-prevention-abilities
  [state side eid key]
  (mapcat #(relevant-prevention-abilities state side eid key %) (all-active state side)))

(defn prevent-numeric
  [state side eid key n]
  (if (get-in @state [:prevent key])
    (do (if (= n :all)
          (swap! state update-in [:prevent key] merge {:prevented :all :remaining 0})
          (do (swap! state update-in [:prevent key :prevented] + n)
              (swap! state update-in [:prevent key :remaining] #(max 0 (- % n)))))
        (trigger-event-sync state side eid (if (= side :corp) :corp-prevent :runner-prevent) {:type key
                                                                                              :amount n}))
    (do (println "tried to prevent " (name key) " outside of a " (name key) "  prevention window")
        (effect-completed state side eid))))

(defn- fetch-and-clear!
  "get the prevent map for a key and also dissoc it from the state"
  [state key]
  (let [res (get-in @state [:prevent key])]
    (swap! state dissoc-in [:prevent key])
    res))

(defn- trigger-prevention
  "Triggers an ability as having prevented something"
  [state side eid key prevention]
  (swap! state update-in [:prevent key :uses (->> prevention :card :cid)] (fnil inc 0))
  (resolve-ability
    state side (assoc eid :source (:card prevention) :source-type :ability)
    (if (:prompt prevention)
      {:optional {:prompt (:prompt prevention)
                  :yes-ability (:ability prevention)}}
      (:ability prevention))
    (:card prevention) nil))

(defn- build-prevention-option
  "Builds a menu item for firing a prevention ability"
  [prevention key]
  {:option (or (:label prevention) (->> prevention :card :printed-title))
   :ability {:async true
             :req (:req (:ability prevention))
             :effect (req (trigger-prevention state side eid key prevention))}})

(defn- resolve-keyed-prevention-for-side
  [state side eid key {:keys [prompt waiting option data-type] :as args}]
  (let [remainder (get-in @state [:prevent key :remaining])
        prompt  (if (string? prompt)  prompt  (prompt state remainder))
        waiting (if (string? waiting) waiting (waiting state remainder))
        option  (if (string? option)  option  (option state remainder))]
    (if (or (if (= data-type :sequential)
              (not (seq remainder))
              (not (pos? remainder)))
            (get-in @state [:prevent key :passed]))
      (do (swap! state dissoc-in [:prevent key :passed])
          (effect-completed state side eid))
      (let [preventions (gather-prevention-abilities state side eid key)]
        (if (empty? preventions)
          (effect-completed state side eid)
          ;; TODO - if there's exactly ONE choice, and it's also mandatory, just rip that choice
          (if (and (= 1 (count preventions))
                   (:mandatory (first preventions)))
            (wait-for (trigger-prevention state side key (first preventions))
                      (resolve-keyed-prevention-for-side state side eid key args))
            (wait-for (resolve-ability
                        state side
                        (choose-one-helper
                          {:prompt prompt
                           :waiting-prompt waiting}
                          (concat (mapv #(build-prevention-option % key) preventions)
                                  [(when-not (some :mandatory preventions)
                                     {:option option
                                      :ability {:effect (req (swap! state assoc-in [:prevent key :passed] true))}})]))
                        nil nil)
                      (resolve-keyed-prevention-for-side state side eid key args))))))))

;; ENCOUNTER PREVENTION
(def prevent-encounter
  (fn [state side eid] (prevent-numeric state side eid :encounter 1)))

;; END RUN PREVENTION
(def prevent-end-run
  (fn [state side eid] (prevent-numeric state side eid :end-run 1)))

(defn- resolve-end-run-prevention-for-side
  [state side eid]
  (resolve-keyed-prevention-for-side
    state side eid :end-run
    {:prompt "Prevent the run from ending"
     :waiting "your opponent to prevent the run from ending"
     :option "Allow the run to end"}))

(defn resolve-end-run-prevention
  [state side eid {:keys [unpreventable card] :as args}]
  (swap! state assoc-in [:prevent :end-run]
         {:count 1 :remaining 1 :prevented 0 :source-player side :source-card card :uses {}})
  (wait-for
    (trigger-event-simult state side :end-run-interrupt nil {:card card :source-eid eid})
    (if unpreventable
      (complete-with-result state side eid (fetch-and-clear! state :end-run))
      (wait-for
        (resolve-end-run-prevention-for-side state :runner)
        (complete-with-result state side eid (fetch-and-clear! state :end-run))))))

;; JACK OUT PREVENTION

(def prevent-jack-out
  (fn [state side eid] (prevent-numeric state side eid :jack-out 1)))

(defn- resolve-jack-out-prevention-for-side
  [state side eid]
  (resolve-keyed-prevention-for-side
    state side eid :jack-out
    {:prompt "Prevent the runner from jacking out"
     :waiting "your opponent to prevent you from jacking out"
     :option "Allow the Runner to jack out"}))

(defn resolve-jack-out-prevention
  [state side eid {:keys [unpreventable card] :as args}]
  (swap! state assoc-in [:prevent :jack-out]
         {:count 1 :remaining 1 :prevented 0 :source-player side :source-card card :uses {}})
  (if unpreventable
    (complete-with-result state side eid (fetch-and-clear! state :jack-out))
    (wait-for
      (resolve-jack-out-prevention-for-side state :corp)
      (complete-with-result state side eid (fetch-and-clear! state :jack-out)))))

;; EXPOSE PREVENTION

(defn prevent-expose
  [state side eid card]
  (if (get-in @state [:prevent :expose])
    (if (<= (count (get-in @state [:prevent :expose :remaining])) 1)
      (do (swap! state update-in [:prevent :expose] merge {:prevented :all :remaining []})
          (trigger-event-sync state side eid (if (= side :corp) :corp-prevent :runner-prevent) {:type :expose
                                                                                                :amount 1}))
      (resolve-ability
        state side eid
        {:prompt "Prevent which card from being exposed?"
         :choices (req (sort-by :title (get-in @state [:prevent :expose :remaining])))
         :effect (req (swap! state update-in [:prevent :expose :remaining] (fn [v] (filterv #(not (same-card? % target)) v)))
                      (swap! state update-in [:prevent :expose :prevented] (fnil inc 0)))}
        card nil))
    (do (println "tried to prevent expose outside of an expose prevention window")
        (effect-completed state side eid))))

(defn resolve-expose-prevention-for-side
  [state side eid]
  (resolve-keyed-prevention-for-side
    state side eid :expose
    {:data-type :sequential
     :prompt (fn [state remainder] (str "Prevent " (enumerate-str (map #(card-str state % {:visible (= side :corp)}) remainder) "or") " from being exposed?"))
     :waiting "your opponent to prevent an Expose"
     :option (fn [state remainder] (str "Allow " (quantify (count remainder) "card") " to be exposed"))}))

(defn resolve-expose-prevention
  [state side eid targets {:keys [unpreventable card] :as args}]
  (swap! state assoc-in [:prevent :expose]
         {:count (count targets) :remaining targets :prevented 0 :source-player side :source-card card :uses {}})
  (wait-for
    (trigger-event-simult state side :expose-interrupt nil {:cards targets})
    (let [new-targets (filterv #(not (or (rezzed? %) (nil? %))) (map #(get-card state %) targets))]
      (swap! state assoc-in [:prevent :expose :remaining] new-targets)
      (swap! state assoc-in [:prevent :expose :counnt] (count new-targets))
      (if (or unpreventable (not (seq new-targets)))
        (complete-with-result state side eid (fetch-and-clear! state :expose))
        (let [active-side (:active-player @state)
              responding-side (other-side active-side)]
          (wait-for
            (resolve-expose-prevention-for-side state active-side)
            (wait-for
              (resolve-expose-prevention-for-side state responding-side)
              (complete-with-result state side eid (fetch-and-clear! state :expose)))))))))

;; BAD PUBLICITY PREVENTION

(defn prevent-bad-publicity [state side eid n] (prevent-numeric state side eid :bad-publicity n))

(defn resolve-bad-pub-prevention-for-side
  [state side eid]
  (resolve-keyed-prevention-for-side
    state side eid :bad-publicity
    {:prompt (fn [state remainder] (str "Prevent any of the " (get-in @state [:prevent :bad-publicity :count]) " bad publicity?"
                                        (when-not (= (get-in @state [:prevent :bad-publicity :count]) remainder)
                                          (str "(" remainder " remaining)"))))
     :waiting "your opponent to prevent bad publicity"
     :option (fn [state remainder] (str "Allow " remainder " bad publicity"))}))

(defn resolve-bad-pub-prevention
  [state side eid n {:keys [unpreventable card] :as args}]
  (swap! state assoc-in [:prevent :bad-publicity]
         {:count n :remaining n :prevented 0 :source-player side :source-card card :uses {}})
  (if (or unpreventable (not (pos? n)))
    (complete-with-result state side eid (fetch-and-clear! state :bad-publicity))
    (let [active-side (:active-player @state)
          responding-side (other-side active-side)]
      (wait-for
        (resolve-bad-pub-prevention-for-side state active-side)
        (wait-for
          (resolve-bad-pub-prevention-for-side state responding-side)
          (complete-with-result state side eid (fetch-and-clear! state :bad-publicity)))))))

;; TAG PREVENTION

(defn prevent-tag [state side eid n] (prevent-numeric state side eid :tag n))

(defn prevent-up-to-n-tags
  [n]
  (letfn [(remainder [state] (get-in @state [:prevent :tag :remaining]))
          (max-to-avoid [state n] (if (= n :all) (remainder state) (min (remainder state) n)))]
    {:prompt "Choose how many tags to avoid"
     :req (req (get-in @state [:prevent :tag]))
     :choices {:number (req (max-to-avoid state n))
               :default (req (max-to-avoid state n))}
     :async true
     :msg (msg "avoid " (quantify target "tag"))
     :effect (req (prevent-tag state side eid target))
     :cancel-effect (req (prevent-tag state side eid 0))}))

(defn resolve-tag-prevention-for-side
  [state side eid]
  (resolve-keyed-prevention-for-side
    state side eid :tag
    {:prompt (fn [state remainder] (str "Prevent any of the " (get-in @state [:prevent :tag :count]) " tags?"
                                        (when-not (= (get-in @state [:prevent :tag :count]) remainder)
                                          (str "(" remainder " remaining)"))))
     :waiting "your opponent to prevent tags"
     :option (fn [state remainder] (str "Allow " (quantify remainder "remaining tag")))}))

(defn resolve-tag-prevention
  [state side eid n {:keys [unpreventable card] :as args}]
  (swap! state assoc-in [:prevent :tag]
         {:count n :remaining n :prevented 0 :source-player side :source-card card :uses {}})
    (if (or unpreventable (not (pos? n)))
      (complete-with-result state side eid (fetch-and-clear! state :tag))
      (wait-for (trigger-event-simult state side :tag-interrupt nil card)
                  (let [active-side (:active-player @state)
                        responding-side (other-side active-side)]
                    (wait-for
                      (resolve-tag-prevention-for-side state active-side)
                      (wait-for
                        (resolve-tag-prevention-for-side state responding-side)
                        (complete-with-result state side eid (fetch-and-clear! state :tag))))))))
