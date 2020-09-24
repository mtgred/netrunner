(in-ns 'game.core)

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
                   ability-to-do (if (and (= prompt-choice "Yes")
                                          yes-ability
                                          (can-pay? state side eid card (:title card) (:cost yes-ability)))
                                   yes-ability
                                   no-ability)]
               (wait-for (resolve-ability state side new-eid ability-to-do card targets)
                         (when end-effect
                           (end-effect state side new-eid card nil))
                         (effect-completed state side eid))))]
     (let [autoresolve-fn (:autoresolve ability)
           autoresolve-answer (when autoresolve-fn
                                (autoresolve-fn state side eid card targets))]
       (case autoresolve-answer
         "Yes" (prompt-fn "Yes")
         "No" (prompt-fn "No")
         (do (when autoresolve-fn
               (toast state side (str "This prompt can be skipped by clicking "
                                      (:title card) " and toggling autoresolve")))
             (show-prompt state side eid card message ["Yes" "No"]
                          prompt-fn ability)))))))

(defn- check-optional
  "Checks if there is an optional ability to resolve"
  [state side {:keys [eid optional] :as ability} card targets]
  (if (can-trigger? state side eid optional card targets)
    (resolve-ability
      state side
      (-> ability
          (dissoc :optional)
          (assoc :async true
                 :effect (req (optional-ability state (or (:player optional) side) eid card (:prompt optional) optional targets))))
      card targets)
    (effect-completed state side eid)))

(register-ability-type :optional check-optional)
