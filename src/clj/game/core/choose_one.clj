(ns game.core.choose-one
  (:require
   [game.macros :refer [continue-ability req wait-for]]
   [game.core.engine :refer [resolve-ability]]
   [game.core.payment :refer [build-cost-string can-pay?]]
   [game.core.eid :refer [effect-completed make-eid]]
   [clojure.string :as str]))

(defn choose-one-helper
  ;; keys unique to this function:
  ;;   no-prune: can I select the same option more than once?
  ;;   no-wait-msg: do we hide the wait message from the runner?
  ;;   count: number of choices we're allowed to pick
  ([xs] (choose-one-helper nil xs))
  ([{:keys [prompt count optional no-prune no-wait-msg interactive require-meaningful-choice] :as args} xs]
   ;;the 'prompt' key cant compute 5-fns, so this needs to be disambiguated
   (if (fn? (:count args))
     {:async true
      :effect (req (let [new-count ((:count args) state side eid card targets)]
                     (continue-ability
                       state side
                       (choose-one-helper (assoc args :count new-count) xs)
                       card nil)))}
     ;; xs of the form {:option ... :req (req ...) :cost ... :ability ..}
     (let [next-optional (= optional :after-first)
           apply-optional (and optional (not next-optional))
           xs (if-not apply-optional xs (conj xs {:option "Done"}))
           base-map (select-keys args [:action :player :once :unregister-once-resolved :event
                                       :label :change-in-game-state :location :additional-cost])
           ;; is a choice payable
           payable? (fn [x state side eid card targets]
                      (when (or (not (:cost x))
                                (can-pay? state (or (:player args) side) eid card nil (:cost x)))
                        x))
           ;; cost->str for a choice
           costed-str (fn [x]
                        (let [choice-str (if-not (:cost x)
                                           (:option x)
                                           (let [cs (build-cost-string (:cost x))]
                                             (if-not (:option x) cs (str cs ": " (:option x)))))]
                          (if (:card x)
                            (assoc (:card x) :title choice-str)
                            choice-str)))
           ;; converts options to choices
           choices-fn (fn [x state side eid card targets]
                        (when (payable? x state side eid card targets)
                          (if-not (:req x)
                            (costed-str x)
                            (when ((:req x) state side eid card targets)
                              (costed-str x)))))
           ;; this lets us selectively skip the prompt if 'done' is the only choice
           meaningful-req? (when require-meaningful-choice
                             (req (let [cs (keep #(choices-fn % state side eid card targets) xs)]
                                    (and (not= cs ["Done"])
                                         (or (nil? (:req args))
                                             ((:req args) state side eid card targets))))))]
       ;; function for resolving choices: pick the matching choice, pay, resolve it, and continue
       ;; when applicable
       (letfn [(resolve-choices [xs full state side eid card target]
                 (if-not (seq xs)
                   (effect-completed state side eid )
                   (if (= target (costed-str (first xs)))
                     (let [ab (assoc (:ability (first xs))
                                     :cost (:cost (first xs)))
                           ab-side (or (:player (first xs)) side)]
                       ;; allow for resolving multiple options, like deuces wild
                       (wait-for
                         (resolve-ability
                           state ab-side (make-eid state eid)
                           ab
                           card nil) ;; below is maybe superflous
                         (if (and count (> count 1) (not= target "Done"))
                           ;; the 'Done' is already there, so can dissoc optional
                           (let [args (assoc args :count (dec count) :optional next-optional)
                                 xs (if no-prune full
                                        (vec (remove #(= target (costed-str %)) full)))]
                             (continue-ability state side (choose-one-helper args xs) card nil))
                           (effect-completed state side eid))))
                     (resolve-choices (rest xs) full state side eid card target))))]
         (merge
           base-map
           {:choices (req (into [] (map #(choices-fn % state side eid card targets) xs)))
            :waiting-prompt (or (:waiting-prompt args) (not no-wait-msg))
            :prompt (str (or (:prompt args) "Choose one")
                         ;; if we are resolving multiple
                         (when (and count (pos? count)) (str " (" count " remaining)")))
            :req (or meaningful-req? (:req args))
            ;; resolve-choices demands async
            :async true
            ;; interactive expects a 5-fn or nil
            ;; but I want to just be able to say True or False
            :interactive (when interactive (if-not (fn? interactive) (req interactive) interactive))
            :effect (req (resolve-choices xs xs state side eid card target))}))))))

(defn cost-option
  [cost side]
  {:cost cost
   :ability {:display-side side
             :msg :cost}})
