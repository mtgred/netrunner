(ns game.core.prevention
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [game.core.board :refer [all-active all-active-installed]]
   [game.core.card :refer [get-card installed? resource? rezzed? same-card?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.choose-one :refer [choose-one-helper]]
   [game.core.cost-fns :refer [card-ability-cost]]
   [game.core.eid :refer [complete-with-result effect-completed]]
   [game.core.effects :refer [any-effects get-effects]]
   [game.core.engine :refer [resolve-ability trigger-event-simult trigger-event-sync]]
   [game.core.flags :refer [can-trash? untrashable-while-resources? untrashable-while-rezzed?]]
   [game.core.payment :refer [->c can-pay?]]
   [game.core.prompts :refer [clear-wait-prompt]]
   [game.core.say :refer [enforce-msg]]
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
                                                  ((get-in % [:ability :req]) state side eid card [(get-in @state [:prevent key])]))]
                             (and (not cannot-play?) payable? not-used-too-many-times? ability-req?))
                          abs)]
    (seq (map #(assoc % :card card) playable?))))

(defn- floating-prevention-abilities
  [state side eid key]
  (let [evs (get-effects state side :prevention)
        abs (filter #(= (:prevents %) key) evs)
        playable? (filter #(let [payable? (can-pay? state side eid (:card %) nil (seq (card-ability-cost state side (:ability %) (:card %) [])))
                                 not-used-too-many-times? (or (not (:max-uses %))
                                                              (not (get-in @state [:prevent key :uses (:cid (:card %))]))
                                                              (< (get-in @state [:prevent key :uses (:cid (:card %))]) (:max-uses %)))
                                 ability-req? (or (not (get-in % [:ability :req]))
                                                  ((get-in % [:ability :req]) state side eid (:card %) [(get-in @state [:prevent key])]))]
                             (and payable? not-used-too-many-times? ability-req?))
                          abs)]
    (seq playable?)))


(defn- gather-prevention-abilities
  [state side eid key]
  (concat (mapcat #(relevant-prevention-abilities state side eid key %) (all-active state side))
          (floating-prevention-abilities state side eid key)))

(defn prevent-numeric
  [state side eid key n]
  (if (get-in @state [:prevent key])
    (do (if (= n :all)
          (swap! state update-in [:prevent key] merge {:prevented :all :remaining 0})
          (do (swap! state update-in [:prevent key :prevented] + n)
              (swap! state update-in [:prevent key :remaining] #(max 0 (- % n)))))
        (trigger-event-sync state side eid (if (= side :corp) :corp-prevent :runner-prevent) {:type key
                                                                                              :amount n}))
    (do (println "tried to prevent " (name key) " outside of a " (name key) "  prevention window (eid: " eid ")")
        (effect-completed state side eid))))

(defn- fetch-and-clear!
  "get the prevent map for a key and also dissoc it from the state"
  [state key]
  (let [res (get-in @state [:prevent key])]
    (if (seq (:prevent-stack @state))
      (do (swap! state assoc :prevent (first (:prevent-stack @state)))
          (swap! state update :prevent-stack rest))
      (swap! state dissoc :prevent))
    res))

(defn- push-prevention!
  [state key map]
  (when (:prevent @state)
    (swap! state assoc :prevent-stack (concat [(:prevent @state)] (:prevent-stack @state))))
  (swap! state assoc-in [:prevent key] map))

(defn- trigger-prevention
  "Triggers an ability as having prevented something"
  [state side eid key prevention]
  ;; this marks the player as having acted, so we can play the priority game
  ;; Note that this requires the following concession:
  ;;   * All abilities should either use the prompt system set up here
  ;;   * Or if they do not, clicking the ability MUST act
  ;; The consequence of ignoring this is the potential for a silly player to pretend to act, do nothing, and flip priority
  (let [abi {:async true
             :effect (req (swap! state assoc-in [:prevent key :priority-passes] 0)
                          (swap! state update-in [:prevent key :uses (->> prevention :card :cid)] (fnil inc 0))
                          (resolve-ability state side eid (:ability prevention) card [(get-in @state [:prevent key])]))}]
    (resolve-ability
      state side (assoc eid :source (:card prevention) :source-type :ability)
      (if (:prompt prevention)
        {:optional {:prompt (:prompt prevention)
                    :yes-ability abi}}
        abi)
      (:card prevention) nil)))

(defn- build-prevention-option
  "Builds a menu item for firing a prevention ability"
  [prevention key]
  {:option (or (:label prevention) (->> prevention :card :printed-title))
   :ability {:async true
             :effect (req (trigger-prevention state side eid key prevention))}})

(defn- resolve-keyed-prevention-for-side
  [state side eid key {:keys [prompt waiting option data-type] :as args}]
  (let [remainder (get-in @state [:prevent key :remaining])
        prompt  (if (string? prompt)  prompt  (prompt state remainder))
        waiting (if (string? waiting) waiting (waiting state remainder))
        option  (if (string? option)  option  (option state remainder))]
    (if (or (if (= data-type :sequential)
              (not (seq remainder))
              nil)
            ;;(not (pos? remainder))) -> the CR says these numbers can go to (or below) 0 withoutout actually closing the interrupt,
            ;;even though most abilities cannot interact with them
            (get-in @state [:prevent key :passed]))
      (do (swap! state dissoc-in [:prevent key :passed])
          (effect-completed state side eid))
      (let [preventions (gather-prevention-abilities state side eid key)]
        (if (empty? preventions)
          (effect-completed state side eid)
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

(defn resolve-prevent-effects-with-priority
  "Resolves prevention effects for a given key, automatically passing priority back and forth while doing so"
  [state side eid key prev-fn]
  (if (= 2 (get-in @state [:prevent key :priority-passes]))
    (complete-with-result state side eid (fetch-and-clear! state key))
    (wait-for (prev-fn state side)
              (swap! state update-in [:prevent key :priority-passes] (fnil inc 1))
              (resolve-prevent-effects-with-priority state (other-side side) eid key prev-fn))))

;; TRASH PREVENTION

(defn prevent-trash-installed-by-type
  [label types cost valid-context?]
  (letfn [(relevant [state card]
            (filter #(and (contains? types (:type %))
                          ;; note that because of the way prompts work, you select before the cost is paid, so things like fall guy need this hack
                          (or (not= cost [(->c :trash-can)]) (not (same-card? card %)))
                          (installed? %))
                    (map :card (get-in @state [:prevent :trash :remaining]))))]
    {:prevents :trash
     :type :ability
     :label label
     :ability {:req (req
                      (and (seq (relevant state card))
                           (not (:unpreventable context))
                           (valid-context? context)
                           (can-pay? state side eid card nil cost)))
               :async true
               :effect (req
                         (wait-for (resolve-ability
                                     state side
                                     (if (= 1 (count (relevant state card)))
                                       {:msg (msg "prevent " (->> (relevant state card) first :title) " from being trashed")
                                        :cost cost
                                        :effect (req (swap! state assoc-in [:prevent :trash :remaining] []))}
                                       {:prompt (str "Choose a " (enumerate-str (map str/lower-case types) "or") " to save from being trashed")
                                        :cost cost
                                        :choices (req (relevant state card))
                                        :msg (msg "prevent " (:title target) " from being trashed")
                                        :effect (req (swap! state update-in [:prevent :trash :remaining] (fn [s] (filterv #(not (same-card? (:card %) target)) s))))})
                                     card nil)
                                   (swap! state update-in [:prevent :trash :remaining] (fn [ctx] (filterv #(get-card state (:card %)) ctx)))
                                   (effect-completed state side eid)))}}))

(defn resolve-trash-for-side
  [state side eid]
  (resolve-keyed-prevention-for-side
    state side eid :trash
    {:data-type :sequential
     :prompt (fn [state remainder]
               (if (= side :runner)
                 (cond
                   (= 1 (count (get-in @state [:prevent :trash :remaining])))
                   (str "Prevent " (->> (get-in @state [:prevent :trash :remaining]) :card :title) " from being trashed?")
                   (>= 5 (count (get-in @state [:prevent :trash :remaining])))
                   (str "Prevent any of " (enumerate-str (sort (map #(->> % :card :title) (get-in @state [:prevent :trash :remaining]))) "or") " from being trashed?")
                   :else
                   (str "Prevent any of " (count (get-in @state [:prevent :trash :remaining])) " cards from being trashed?"))
                 "Choose an interrupt")) ;; note - for corp, this is only marilyn campaign
     :waiting "your opponent to resolve trash prevention triggers"
     :option (fn [state remainder] (str "Allow " (quantify (count (get-in @state [:prevent :trash :remaining])) "card") " to be trashed"))}))

(defn resolve-trash-prevention
  [state side eid targets {:keys [unpreventable game-trash cause cause-card] :as args}]
  (let [untrashable (keep identity (map #(cond
                                           (and (not game-trash)
                                                (untrashable-while-rezzed? %))
                                           [%  "cannot be trashed while installed"]
                                           (and (= side :runner)
                                                (not (can-trash? state side %)))
                                           [% "cannot be trashed"]
                                           (and (= side :corp)
                                                (untrashable-while-resources? %)
                                                (> (count (filter resource? (all-active-installed state :runner))) 1))
                                           [% "cannot be trashed while there are other resources installed"]
                                           :else nil)
                                        targets))
        trashable (if untrashable
                    (vec (set/difference (set targets) (set (map first untrashable))))
                    (vec targets))
        untrashable (mapv (fn [[c reason]] {:card c :destination :discard :reason reason}) untrashable)
        trashable   (mapv (fn [c] {:card c :destination :discard}) trashable)]
    (doseq [{:keys [card reason]} untrashable]
      (when reason
        (enforce-msg state card reason)))
    (push-prevention! state :trash
                      {:count (count trashable) :remaining trashable :untrashable untrashable :prevented 0 :source-player side :source-card cause-card :priority-passes 0
                       :type type :unpreventable unpreventable :cause cause :game-trash game-trash :uses {}})
    (if (not (seq trashable))
      (complete-with-result state side eid (fetch-and-clear! state :trash))
      (resolve-prevent-effects-with-priority state (:active-player @state) eid :trash resolve-trash-for-side))))

;; DAMAGE PREVENTION + PRE-DAMAGE PREVENTION

(defn damage-type
  [state key]
  (get-in @state [:prevent key :type]))

(defn damage-pending
  [state key]
  (get-in @state [:prevent key :remaining]))

(defn damage-boost
  [state side eid key n]
  (when (pos? (damage-pending state key))
    (swap! state update-in [:prevent key :remaining] + n))
  (effect-completed state side eid))

(defn damage-name
  [state key]
  (case (damage-type state key)
    :meat "meat"
    :brain "core"
    :core "core"
    :net "net"
    "neat"))

(defn prevent-damage
  [state side eid key n]
  (when (pos? (damage-pending state key))
    (if (= n :all)
      (swap! state update-in [:prevent key] merge {:remaining 0 :prevented :all})
      (do (swap! state update-in [:prevent key :remaining] #(max 0 (- % n)))
          (swap! state update-in [:prevent key :prevented] (fnil #(+ n %) n)))))
  (effect-completed state side eid))

(defn prevent-up-to-n-damage
  [n key types]
  (letfn [(remainder [state] (get-in @state [:prevent key :remaining]))
          (max-to-avoid [state n] (if (= n :all) (remainder state) (min (remainder state) n)))]
    {:prompt (msg "Choose how much " (damage-name state key) " damage prevent")
     :req (req (and (pos? (get-in @state [:prevent key :remaining]))
                    (not (get-in @state [:prevent key :unpreventable]))
                    (or (not types)
                        (contains? types (get-in @state [:prevent key :type])))))
     :choices {:number (req (max-to-avoid state n))
               :default (req (max-to-avoid state n))}
     :async true
     :msg (msg "prevent " target " " (damage-name state key) " damage")
     :effect (req (prevent-damage state side eid key target))
     :cancel-effect (req (prevent-damage state side eid key 0))}))

(defn resolve-pre-damage-for-side
  [state side eid]
  (resolve-keyed-prevention-for-side
    state side eid :pre-damage
    {:prompt (fn [state remainder]
               (if (= side :runner)
                 (str "Prevent " (damage-pending state :pre-damage) " " (damage-name state :pre-damage) " damage?")
                 (str "There is " (damage-pending state :pre-damage) " pending " (damage-name state :pre-damage) " damage")))
     :waiting "your opponent to resolve pre-damage triggers"
     :option "Pass priority"}))

;; NOTE - PRE-DAMAGE EFFECTS HAPPEN BEFORE DAMAGE EFFECTS, AND ARE THE CONSTANT ABILITIES (IE GURU DAVINDER, MURESH BODYSUIT, THE CLEANERS, ETC)
;; AND MAY JUST CLOSE THE WINDOW ALL TOGETHER IF ALL DAMAGE IS PREVENTED

(defn resolve-damage-for-side
  [state side eid]
  (resolve-keyed-prevention-for-side
    state side eid :damage
    {:prompt (fn [state remainder]
               (if (= side :runner)
                 (str "Prevent " (damage-pending state :damage) " " (damage-name state :damage) " damage?")
                 (str "There is " (damage-pending state :damage) " pending " (damage-name state :damage) " damage")))
     :waiting "your opponent to resolve damage triggers"
     :option "Pass priority"}))

(defn resolve-damage-prevention
  [state side eid type n {:keys [unpreventable unboostable card] :as args}]
  (push-prevention! state :pre-damage
                    {:count n :remaining n :prevented 0 :source-player side :source-card card :priority-passes 0
                     :type type :unpreventable unpreventable :unboostable unboostable :uses {}})
  (wait-for (trigger-event-simult state side :pre-damage-flag nil {:card card :type type :count n})
            (wait-for (resolve-prevent-effects-with-priority state (:active-player @state) :pre-damage resolve-pre-damage-for-side)
                      (swap! state assoc-in [:prevent :damage] async-result)
                      (swap! state assoc-in [:prevent :damage :priority-passes] 0)
                      (resolve-prevent-effects-with-priority state (:active-player @state) eid :damage resolve-damage-for-side))))

;; ENCOUNTER PREVENTION
(def prevent-encounter
  (fn [state side eid] (prevent-numeric state side eid :encounter 1)))

(defn resolve-encounter-prevention-for-side
  [state side eid]
  (resolve-keyed-prevention-for-side
    state side eid :encounter
    {:prompt (fn [state remainder] (str "Prevent " (get-in @state [:prevent :encounter :title]) " ability?"))
     :waiting "your opponent to prevent a \"when encountered\" ability"
     :option (fn [state remainder] (str "Allow " (get-in @state [:prevent :encounter :title])))}))

(defn resolve-encounter-prevention
  [state side eid {:keys [unpreventable card title] :as args}]
  (push-prevention! state :encounter
                    {:count 1 :remaining 1 :title title :prevented 0 :source-player side :source-card card :uses {}})
  (if unpreventable
    (complete-with-result state side eid (fetch-and-clear! state :encounter))
    (resolve-prevent-effects-with-priority state (:active-player @state) eid :encounter resolve-encounter-prevention-for-side)))

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
  (push-prevention! state :end-run
                    {:count 1 :remaining 1 :prevented 0 :source-player side :source-card card :uses {}})
  (wait-for
    (trigger-event-simult state side :end-run-interrupt nil {:card card :source-eid eid})
    (if unpreventable
      (complete-with-result state side eid (fetch-and-clear! state :end-run))
      (resolve-prevent-effects-with-priority state (:active-player @state) eid :end-run resolve-end-run-prevention-for-side))))

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
  (push-prevention! state :jack-out
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
  (push-prevention! state :expose
                    {:count (count targets) :remaining targets :prevented 0 :source-player side :source-card card :uses {}})
  (wait-for
    (trigger-event-simult state side :expose-interrupt nil {:cards targets})
    (let [new-targets (filterv #(not (or (rezzed? %) (nil? %))) (map #(get-card state %) targets))]
      (swap! state assoc-in [:prevent :expose :remaining] new-targets)
      (swap! state assoc-in [:prevent :expose :count] (count new-targets))
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
  (push-prevention! state :bad-publicity
                    {:count n :remaining n :prevented 0 :source-player side :source-card card :uses {}})
  (if (or unpreventable (not (pos? n)))
    (complete-with-result state side eid (fetch-and-clear! state :bad-publicity))
    (resolve-prevent-effects-with-priority state (:active-player @state) eid :bad-publicity resolve-bad-pub-prevention-for-side)))

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
  (push-prevention! state :tag
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
