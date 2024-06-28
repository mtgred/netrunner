(ns game.core.optional
  (:require
    [game.core.card :refer [get-card]]
    [game.core.eid :refer [effect-completed make-eid]]
    [game.core.engine :refer [can-trigger? register-ability-type register-once resolve-ability]]
    [game.core.payment :refer [can-pay?]]
    [game.core.prompts :refer [show-prompt]]
    [game.core.toasts :refer [toast]]
    [game.core.update :refer [update!]]
    [game.macros :refer [effect req wait-for]]
    [clojure.string :as string]))

(defn optional-ability
  "Shows a 'Yes/No' prompt and resolves the given ability's :yes-ability if Yes is chosen, and :no-ability otherwise.
  If ability has an :autoresolve entry, first call it as a 5-function, and if it returns 'Yes' or 'No'
  resolve the ability as if prompt was displayed and Yes/No was chosen."
  ([state side eid card message ability targets]
   (letfn [(prompt-fn [prompt-choice]
             (let [yes-ability (:yes-ability ability)
                   no-ability (:no-ability ability)
                   end-effect (:end-effect ability)
                   new-eid (make-eid state eid)
                   ability-to-do (if (and (= (:value prompt-choice) "Yes")
                                          yes-ability
                                          (can-pay? state side eid card (:title card) (:cost yes-ability)))
                                   (assoc yes-ability :once (:once ability))
                                   no-ability)]
               (wait-for (resolve-ability state side new-eid ability-to-do card targets)
                         (when end-effect
                           (end-effect state side new-eid card targets))
                         (effect-completed state side eid))))]
     (let [autoresolve-fn (:autoresolve ability)
           autoresolve-answer (when autoresolve-fn
                                (autoresolve-fn state side eid card targets))
           choices [(when (can-pay? state side eid card (:title card) (:cost (:yes-ability ability)))
                      "Yes")
                    "No"]]
       (case autoresolve-answer
         "Yes" (prompt-fn {:value "Yes"})
         "No" (prompt-fn {:value "No"})
         (do (when autoresolve-fn
               (toast state side (str "This prompt can be skipped by clicking "
                                      (:title card) " and toggling autoresolve")))
             (show-prompt state side eid card message choices
                          prompt-fn (assoc ability :targets targets))))))))

(defn- check-optional
  "Checks if there is an optional ability to resolve"
  [state side {:keys [eid optional] :as ability} card targets]
  (assert (not (contains? optional :async)) "Put :async in the :yes-ability")
  (if (can-trigger? state side eid optional card targets)
    (resolve-ability
      state side
      (-> ability
          (dissoc :optional :once :req)
          (assoc :async true
                 :effect (req (optional-ability state (or (:player optional) side) eid card (:prompt optional) optional targets))))
      card targets)
    (effect-completed state side eid)))

(register-ability-type :optional #'check-optional)

(defn never?
  "Returns true if is argument is :never."
  [x]
  (= :never x))

(defn set-autoresolve
  "Makes a card ability which lets the user toggle auto-resolve on an ability. Setting is stored under [:special toggle-kw]."
  [toggle-kw ability-name]
  {:autoresolve true
   :label (str "Toggle auto-resolve on " ability-name)
   :prompt (str "Set auto-resolve on " ability-name " to:")
   :choices ["Always" "Never" "Ask"]
   :effect (effect (update! (assoc-in card [:special toggle-kw] (keyword (string/lower-case target))))
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
