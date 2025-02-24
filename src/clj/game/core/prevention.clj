(ns game.core.prevention
  (:require
   [game.core.board :refer [all-installed]]
   [game.core.card-defs :refer [card-def]]
   [game.core.cost-fns :refer [card-ability-cost]]
   [game.core.def-helpers :refer [choose-one-helper]]
   [game.core.eid :refer [complete-with-result effect-completed]]
   [game.core.effects :refer [any-effects]]
   [game.core.engine :refer [resolve-ability trigger-event-simult trigger-event-sync]]
   [game.core.payment :refer [can-pay?]]
   [game.utils :refer [dissoc-in quantify]]
   [game.macros :refer [msg req wait-for]]
   [jinteki.utils :refer [other-side]]))

;; so how is this going to work?
;; each player, starting with the active player, gets a chance to prevent effects
;; we get a list of all cards that have prevention effects, and create a prompt with all the specific prevention abilities, along with:
;;  * are they repeatable?
;;  * the source card
;;  * is it an ability, an interrupt, or a triggered event?
;;
;; Relevant Cards:
;; Jesminder, Quianju PT (forced interrupts) - these are pre-tag events, we can skip them
;; Forger (interrupt, ability)               - move this to the pre-tag event/interrupt, then we can skip it
;; No One Home (event, avoid any number of tags) (done)
;; On the lam (ability, avoid up to three tags) (done)
;; Decoy (ability, avoid up to 1 tags) - done
;; New Angeles City Hall (ability, avoid 1 tag, repeatable)
;; Dorm Computer (aura/event, avoid all tags)
;;
;; Plan of attack:
;; * Rework Forger to be an interrupt (done)
;; * Rework the 'forced-to-avoid-tag' flag as a static-ability (see jesminder) (done)
;; * Rework NOH, On the Lam, Decoy, NACH, Dorm Computer to have prevention
;;     abilities I can scry the state for, like:
;;
;;  :prevention [{:type tag
;;                :type :ability
;;                :label "(No One Home) Avoid any number of tags"
;;                :ability {:optional true
;;                          :cost [(->c :trash-can 1)]
;;                          :req (req (and (no-event state side :tag)
;;                                         (no-event state side :net-damage)))
;;                          :optional true
;;                          :yes-ability {:async true
;;                                        :effect (req (do-whatever-trace))}]
;;
;;  :prevention [{:prevent tag
;;                :type :effect
;;                :mandatory true
;;                :max-uses 1
;;                :label "(Dorm Computer) Avoid all tags"
;;                :ability {:msg "avoid all tags"
;;                          :req (req (this-card-is-run-source state)) ;; - dorm computer
;;                          :effect (req (prevent state side :tag :all))}}]

(defn- relevant-prevention-abilities
  [state side eid key card]
  (let [abs (filter #(= (:prevents %) key) (:prevention (card-def card)))
        with-card (map #(assoc % :card card) abs)
        ;; filter only to ones that are playable and can be played
        playable? (filter #(let [cannot-play? (and (= (:type %) :ability)
                                                   (any-effects state side :prevent-paid-ability true? card [(:ability %) 0]))
                                 payable? (can-pay? state side eid card nil (seq (card-ability-cost state side (:ability %) card [])))
                                 ;; todo - account for card being disabled
                                 not-used-too-many-times? (or (not (:max-uses %))
                                                          (not (get-in @state [:prevent :tags :uses (:cid card)]))
                                                          (< (get-in @state [:prevent :tags :uses (:cid card)]) (:max-uses %)))
                                 ability-req? (or (not (get-in % [:ability :req]))
                                                  ((get-in % [:ability :req]) state side eid card nil))]
                             (and (not cannot-play?) payable? not-used-too-many-times? ability-req?))
                          abs)]
    (seq (map #(assoc % :card card) playable?))))

(defn- gather-prevention-abilities
  [state side eid key]
  (mapcat #(relevant-prevention-abilities state side eid key %) (all-installed state side)))

(defn prevent-tag
  [state side eid n]
  (if (get-in @state [:prevent :tags])
    (do (if (= n :all)
          (swap! state update-in [:prevent :tags] merge {:prevented :all :remaining 0})
          (do (swap! state update-in [:prevent :tags :prevented] + n)
              (swap! state update-in [:prevent :tags :remaining] #(max 0 (- % n)))))
        (trigger-event-sync state side eid (if (= side :corp) :corp-prevent :runner-prevent) {:type :tag
                                                                                              :amount n}))
    (do (println "tried to prevent tags outside of a tag prevention window")
        (effect-completed state side eid))))

(defn prevent-up-to-n-tags
  [n]
  (letfn [(remainder [state] (get-in @state [:prevent :tags :remaining]))
          (max-to-avoid [state n] (if (= n :all) (remainder state) (min (remainder state) n)))]
    {:prompt "Choose how many tags to avoid"
     :req (req (get-in @state [:prevent :tags]))
     :choices {:number (req (max-to-avoid state n))
               :default (req (max-to-avoid state n))}
     :async true
     :msg (msg "avoid " (quantify target "tag"))
     :effect (req (prevent-tag state side eid target))
     :cancel-effect (req (prevent-tag state side eid 0))}))

(defn- fetch-and-clear!
  "get the prevent map for a key and also dissoc it from the state"
  [state key]
  (let [res (get-in @state [:prevent key])]
    (swap! state dissoc-in [:prevent key])
    res))

(defn- build-prevention-option
  [prevention]
  {:option (:label prevention)
   :ability {:async true
             :req (:req (:ability prevention))
             :effect (req
                       (swap! state update-in [:prevent :tags :uses (->> prevention :card :cid)] (fnil inc 0))
                       (resolve-ability
                         state side (assoc eid :source (:card prevention) :source-type :ability)
                         (if (:choice prevention)
                           {:optional {:prompt (:choice prevention)
                                       :yes-ability (:ability prevention)}}
                           (:ability prevention))
                         (:card prevention) nil))}})

(defn- resolve-tag-prevention-for-side
  [state side eid]
  (let [remainder (get-in @state [:prevent :tags :remaining])]
    (if (or (not (pos? remainder)) (get-in @state [:prevent :tags :passed]))
      (do (swap! state dissoc-in [:prevent :tags :passed])
          (effect-completed state side eid))
      (let [preventions (gather-prevention-abilities state side eid :tag)]
        (if (empty? preventions)
          (effect-completed state side eid)
          (wait-for (resolve-ability
                      state side
                      (choose-one-helper
                        {:prompt (str "Prevent any of the " (get-in @state [:prevent :tags :count]) " tags?"
                                      (when-not (= (get-in @state [:prevent :tags :count]) remainder)
                                        (str "(" remainder " remaining)")))}
                        (concat (mapv build-prevention-option preventions)
                                [{:option (str "Allow " (quantify remainder "remaining tag"))
                                  :ability {:effect (req (swap! state assoc-in [:prevent :tags :passed] true))}}]))
                      nil nil)
                    (resolve-tag-prevention-for-side state side eid)))))))

(defn resolve-tag-prevention
  [state side eid n {:keys [unpreventable card] :as args}]
  (swap! state assoc-in [:prevent :tags]
         {:count n :remaining n :prevented 0 :source-player side :source-card card :uses {}})
    (if (or unpreventable (not (pos? n)))
      (complete-with-result state side eid (fetch-and-clear! state :tags))
      ;; interrupts and static abilities happen first
      (wait-for (trigger-event-simult state side :tag-interrupt nil card)
                  (let [active-side (:active-player @state)
                        responding-side (other-side active-side)]
                    (wait-for
                      (resolve-tag-prevention-for-side state active-side)
                      (wait-for
                        (resolve-tag-prevention-for-side state responding-side)
                        (complete-with-result state side eid (fetch-and-clear! state :tags))))))))
