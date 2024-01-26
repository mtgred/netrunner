(ns game.test-framework.asserts 
  (:require
   [clojure.test :as t]
   [kaocha.hierarchy :as k.hierarchy]
   [kaocha.output :as k.output]
   [kaocha.report :as k.report]))

(defmethod t/assert-expr 'last-log-contains?
  [msg form]
  `(let [state# ~(nth form 1)
         content# ~(nth form 2)
         log# (-> @state# :log last :text)
         found# ~form]
     (t/do-report
       {:type (if found# :pass :fail)
        :actual log#
        :expected content#
        :message ~msg})
     found#))

(defmethod t/assert-expr 'second-last-log-contains?
  [msg form]
  `(let [state# ~(nth form 1)
         content# ~(nth form 2)
         log# (-> @state# :log butlast last :text)
         found# ~form]
     (t/do-report
       {:type (if found# :pass :fail)
        :actual log#
        :expected content#
        :message ~msg})
     found#))

(defmethod t/assert-expr 'last-n-log-contains?
  [msg form]
  `(let [state# ~(nth form 1)
         n# ~(nth form 2)
         content# ~(nth form 3)
         log# (:text (nth (-> @state# :log reverse) n#))
         found# ~form]
     (t/do-report
       {:type (if found# :pass :fail)
        :actual log#
        :expected content#
        :message ~msg})
     found#))

(defmethod t/assert-expr 'prompt-is-type?
  [msg form]
  `(let [state# ~(nth form 1)
         side# ~(nth form 2)
         given-type# ~(nth form 3)
         prompt-type# (-> @state# side# :prompt :prompt-type)
         found# ~form]
     (t/do-report
       {:type (if found# :pass :fail)
        :actual prompt-type#
        :expected given-type#
        :message ~msg})
     found#))

(defmethod t/assert-expr 'prompt-is-card?
  [msg form]
  `(let [state# ~(nth form 1)
         side# ~(nth form 2)
         card# ~(nth form 3)
         prompt-card# (-> @state# side# :prompt :card)
         found# ~form]
     (t/do-report
       {:type (if found# :pass :fail)
        :actual prompt-card#
        :expected card#
        :message ~msg})
     found#))

(defmethod t/assert-expr 'no-prompt?
  [msg form]
  `(let [state# ~(nth form 1)
         side# ~(nth form 2)
         prompt# (-> @state# side# :prompt)
         prompt-type# (-> @state# side# :prompt :prompt-type)
         found# ~form]
     (t/do-report
       {:type (if found# :pass :fail)
        :actual (select-keys (first prompt#) [:msg :prompt-type])
        :expected "No prompt or :prompt-type of :run"
        :message ~msg})
     found#))

(defmethod t/assert-expr 'changed?
  [msg [_changed bindings & body]]
  (let [exprs (take-nth 2 bindings)
        amts (take-nth 2 (drop 1 bindings))
        init-binds (repeatedly gensym)
        end-binds (repeatedly gensym)
        pairs (mapv vector
                    amts
                    (map #(list `quote %) exprs)
                    init-binds
                    end-binds)]
    `(let [~@(interleave init-binds exprs)
           _# (do ~@body)
           ~@(interleave end-binds exprs)]
       (doseq [[amt# expr# init# end#] ~pairs
               :let [expected# (+ init# amt#)
                     actual-change# (- end# init#)]]
         (t/do-report
           {:type (if (= actual-change# amt#) :pass :changed-fail)
            :expected amt#
            :actual actual-change#
            :message (format "%s\n%s => (%s to %s)" ~msg expr# init# end#)})))))

(defn report-failed-change [m]
  (t/with-test-out
    (println (str "\n" (k.output/colored :red "FAIL")
                  " in " (t/testing-vars-str m)))
    (when (seq t/*testing-contexts*)
      (println (t/testing-contexts-str)))
    (when-let [message (:message m)] 
      (println message))
    (println "expected diff:" (pr-str (:expected m)))
    (println "  actual diff:" (pr-str (:actual m)))))

(do
  (k.hierarchy/derive! :changed-fail :kaocha/known-key)
  (k.hierarchy/derive! :changed-fail :kaocha/fail-type)
  nil)

(defmethod k.report/fail-summary :changed-fail [m]
  (report-failed-change m))

;; Just in case someone uses a non-kaocha runner
(defmethod clojure.test/report :changed-fail [m]
  (t/inc-report-counter :fail)
  (report-failed-change m))
