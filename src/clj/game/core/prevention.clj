(ns game.core.prevention
  (:require
   [game.core.eid :refer [complete-with-result effect-completed]]
   [game.core.engine :refer [trigger-event-simult trigger-event-sync]]
   [game.utils :refer [dissoc-in]]
   [game.macros :refer [wait-for]]))

;; so how is this going to work?
;; each player, starting with the active player, gets a chance to prevent effects
;; we get a list of all cards that have prevention effects, and create a prompt with all the specific prevention abilities, along with:
;;  * are they repeatable?
;;  * the source card
;;  * is it an ability, an interrupt, or a triggered event?
;;
;; Relevant Cards:
;; Jesminder, Quianju PT (forced interrupts) - these are pre-tag events, we can skip them
;; Forger (interrupt, ability)               - move this to the pre-tag event, then we can skip it
;; No One Home (event, avoid any number of tags)
;; On the lam (ability, avoid up to three tags)
;; Decoy (ability, avoid up to 1 tags)
;; New Angeles City Hall (ability, avoid 1 tag, repeatable)
;; Dorm Computer (aura/event, avoid all tags)
;;
;; Plan of attack:
;; * Rework Forger to be an interrupt
;; * Rework NOH, On the Lam, Decoy, NACH, Dorm Computer to have prevention
;;     abilities I can scry the state for, like:
;; * Rework the 'forced-to-avoid-tag' flag as a static-ability (see jesminder)
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

(defn- fetch-and-clear!
  [state key]
  (let [res (get-in @state [:prevent key])]
    (swap! state dissoc-in [:prevent key])
    res))

(defn resolve-tag-prevention
  [state side eid n {:keys [unpreventable card] :as args}]
  (swap! state assoc-in [:prevent :tags]
         {:count n :remaining n :prevented 0 :source-player side :source-card card})
    (if (or unpreventable (not (pos? n)))
      (complete-with-result state side eid (fetch-and-clear! state :tags))
      ;; this should hit forger, jesminder, quianju pt
      ;; then we check if remaining is > 0
      (wait-for (trigger-event-simult state side :tag-interrupt nil card)
                (println (get-in @state [:prevent]))
                (complete-with-result state side eid (fetch-and-clear! state :tags)))))
