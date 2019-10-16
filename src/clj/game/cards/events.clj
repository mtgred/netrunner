(ns game.cards.events
  (:require [game.core :refer :all]
            [game.core.card :refer :all]
            [game.core.effects :refer [register-floating-effect]]
            [game.core.eid :refer [make-eid make-result effect-completed]]
            [game.core.card-defs :refer [card-def]]
            [game.core.prompts :refer [show-wait-prompt clear-wait-prompt]]
            [game.core.toasts :refer [toast]]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability]]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [clojure.stacktrace :refer [print-stack-trace]]
            [jinteki.utils :refer :all]))

(defn- cutlery
  [subtype]
  {:implementation "Ice trash is manual"
   :async true
   :makes-run true
   :prompt "Choose a server:"
   :choices (req runnable-servers)
   :effect (effect (toast "Click this card to trash the passed ice")
                   (make-run eid target nil card))
   :abilities [{:label (str "Trash " subtype " ice")
                :once :per-run
                :req (req (and run
                               (has-subtype? (nth run-ices run-position) subtype)
                               (rezzed? (nth run-ices run-position))))
                :async true
                :msg (msg "trash " (card-str state (nth run-ices run-position)))
                :effect (effect (trash eid (nth run-ices run-position) nil))}]})

;; Card definitions
(def card-definitions
  {"Account Siphon"
   {:req (req hq-runnable)
    :makes-run true
    :async true
    :effect (effect (make-run
                      eid :hq
                      {:req (req (= target :hq))
                       :replace-access
                       {:msg (msg "force the Corp to lose " (min 5 (:credit corp))
                                  " [Credits], gain " (* 2 (min 5 (:credit corp)))
                                  " [Credits] and take 2 tags")
                        :async true
                        :effect (req (wait-for (gain-tags state :runner 2)
                                               (gain-credits state :runner (* 2 (min 5 (:credit corp))))
                                               (lose-credits state :corp (min 5 (:credit corp)))
                                               (effect-completed state side eid)))}}
                      card))}

   "Always Have a Backup Plan"
   {:implementation "Bypass is manual"
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :async true
    :makes-run true
    :msg (msg "make a run on " target)
    :effect (req (wait-for (make-run state side target nil card)
                           (let [card (get-card state card)]
                             (if (:run-again card)
                               (make-run state side eid target nil card {:ignore-costs true})
                               (effect-completed state side eid)))))
    :events [{:event :unsuccessful-run-ends
              :optional {:req (req (not (:run-again card)))
                         :prompt "Make another run on the same server?"
                         :yes-ability {:effect (effect (update! (assoc card :run-again true)))}}}]}

   "Amped Up"
   {:msg "gain [Click][Click][Click] and suffer 1 brain damage"
    :effect (effect (gain :click 3)
                    (damage eid :brain 1 {:unpreventable true :card card}))}

   "Another Day, Another Paycheck"
   {:events [{:event :agenda-stolen
              :trace {:base 0
                      :unsuccessful
                      {:effect (effect (gain-credits
                                         :runner (+ (:agenda-point runner) (:agenda-point corp))))
                       :msg (msg (str "gain " (+ (:agenda-point runner) (:agenda-point corp)) " [Credits]"))}}}]}

   "Apocalypse"
   (let [corp-trash {:async true
                     :effect (req (let [ai (all-installed state :corp)
                                        onhost (filter #(= '(:onhost) (:zone %)) ai)
                                        unhosted (->> ai
                                                      (remove #(= '(:onhost) (:zone %)))
                                                      (sort-by #(vec (:zone %)))
                                                      (reverse))
                                        allcorp (concat onhost unhosted)]
                                    (trash-cards state :runner eid allcorp)))}
         runner-facedown {:effect (req (let [installedcards (all-active-installed state :runner)
                                             ishosted (fn [c] (or (= ["onhost"] (get c :zone)) (= '(:onhost) (get c :zone))))
                                             hostedcards (filter ishosted installedcards)
                                             nonhostedcards (remove ishosted installedcards)]
                                         (doseq [oc hostedcards :let [c (get-card state oc)]]
                                           (flip-facedown state side c))
                                         (doseq [oc nonhostedcards :let [c (get-card state oc)]]
                                           (flip-facedown state side c))))}]
     {:req (req (and (some #{:hq} (:successful-run runner-reg))
                     (some #{:rd} (:successful-run runner-reg))
                     (some #{:archives} (:successful-run runner-reg))))
      :async true
      ;; trash cards from right to left
      ;; otherwise, auto-killing servers would move the cards to the next server
      ;; so they could no longer be trashed in the same loop
      :msg "trash all installed Corp cards and turn all installed Runner cards facedown"
      :effect (req (wait-for
                     (resolve-ability state side corp-trash card nil)
                     (continue-ability state side runner-facedown card nil)))})

   "Because I Can"
   {:async true
    :makes-run true
    :prompt "Choose a server"
    :choices (req (filter #(can-run-server? state %) remotes))
    :effect (effect (make-run
                      eid target
                      {:req (req (is-remote? target))
                       :replace-access
                       {:msg "shuffle all cards in the server into R&D"
                        :effect (req (doseq [c (:content run-server)]
                                       (move state :corp c :deck))
                                     (shuffle! state :corp :deck))}}
                      card))}

   "Black Hat"
   {:trace {:base 4
            :unsuccessful
            {:effect (effect (register-events
                               card [{:event :pre-access
                                      :duration :end-of-turn
                                      :req (req (#{:hq :rd} target))
                                      :effect (effect (access-bonus target 2))}]))}}}

   "Blueberry!™ Diesel"
   {:async true
    :prompt "Move a card to the bottom of the stack?"
    :not-distinct true
    :choices (req (conj (vec (take 2 (:deck runner))) "No"))
    :effect (req (when-not (string? target)
                   (move state side target :deck))
                 (system-msg state side
                             (str "looks at the top 2 cards of the stack"
                                  (when-not (string? target)
                                    " and adds one to the bottom of the stack")))
                 (system-msg state side "uses Blueberry!™ Diesel to draw 2 cards")
                 (draw state :runner eid 2 nil))}

   "Blackmail"
   {:async true
    :makes-run true
    :req (req (has-bad-pub? state))
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :msg "prevent ICE from being rezzed during this run"
    :effect (effect (register-run-flag!
                      card
                      :can-rez
                      (fn [state side card]
                        (if (ice? card)
                          ((constantly false)
                           (toast state :corp "Cannot rez ICE on this run due to Blackmail"))
                          true)))
                    (make-run eid target nil card))}

   "Bribery"
   {:implementation "ICE chosen for cost increase is specified at start of run, not on approach"
    :async true
    :makes-run true
    :prompt "How many credits?"
    :choices :credit
    :msg (msg "increase the rez cost of the first unrezzed ICE approached by " target " [Credits]")
    :effect (effect (continue-ability
                      (let [bribery-x target]
                        {:prompt "Choose a server"
                         :choices (req runnable-servers)
                         :async true
                         :effect (req (let [server (unknown->kw target)
                                            run-ices (get-in @state (concat [:corp :servers] [server] [:ices]))
                                            foremost-ice (last (remove rezzed? run-ices))]
                                        (register-floating-effect
                                          state side card
                                          {:type :rez-additional-cost
                                           :duration :end-of-run
                                           :req (req (and (same-card? foremost-ice target)
                                                          (not (get-in @state [:per-run (:cid card)]))))
                                           :value [:credit bribery-x]})
                                        (register-events
                                          state side card
                                          [{:event :rez
                                            :duration :end-of-run
                                            :req (req (and (same-card? foremost-ice target)
                                                           (not (get-in @state [:per-run (:cid card)]))))
                                            :effect (req (swap! state assoc-in [:per-run (:cid card)] true))}])
                                        (make-run state side eid target nil card)))})
                      card nil))}

   "Brute-Force-Hack"
   {:req (req (some #(and (ice? %)
                          (rezzed? %)
                          (can-pay? state side eid card nil
                                    [:credit (rez-cost state side %)]))
                    (all-installed state :corp)))
    :effect
    (req (let [affordable-ice
               (seq (filter
                      identity
                      (for [ice (all-installed state :corp)
                            :when (and (ice? ice)
                                       (rezzed? ice))
                            :let [cost (rez-cost state side ice)]]
                        (when (can-pay? state side eid card nil [:credit cost])
                          [(:cid ice) cost]))))]
           (continue-ability
             state side
             {:prompt "How many [Credits]?"
              :choices :credit
              :msg (msg "spends " target " [Credit] on Brute-Force-Hack")
              :effect (effect (continue-ability
                                {:choices {:req #(and (rezzed? %)
                                                      (some (fn [c] (and (= (first c)
                                                                            (:cid %))
                                                                         (<= (second c) target)))
                                                            affordable-ice))}
                                 :msg (msg "derez " (card-str state target))
                                 :effect (effect (derez target))}
                                card nil))}
             card nil)))}

   "Build Script"
   {:msg "gain 1 [Credits] and draw 2 cards"
    :async true
    :effect (effect (gain-credits 1) (draw eid 2 nil))}

   "By Any Means"
   {:events [{:event :access
              :duration :end-of-turn
              :req (req (not (in-discard? target)))
              :interactive (req true)
              :async true
              :msg (msg "trash " (:title target) " at no cost and suffer 1 meat damage")
              :effect (req (wait-for (trash state side (assoc target :seen true) nil)
                                     (swap! state assoc-in [:runner :register :trashed-card] true)
                                     (damage state :runner eid :meat 1 {:unboostable true})))}]}

   "Calling in Favors"
   {:msg (msg "gain " (count (filter #(and (has-subtype? % "Connection") (resource? %))
                                     (all-active-installed state :runner))) " [Credits]")
    :effect (effect (gain-credits (count (filter #(and (has-subtype? % "Connection")
                                                       (resource? %))
                                                 (all-active-installed state :runner)))))}

   "Career Fair"
   {:prompt "Select a resource to install from your Grip"
    :req (req (some #(and (resource? %)
                          (can-pay? state side eid card nil
                                    [:credit (install-cost state side % {:cost-bonus -3})]))
                    (:hand runner)))
    :choices {:five (req (and (resource? target)
                              (in-hand? target)
                              (can-pay? state side eid card nil
                                        [:credit (install-cost state side target {:cost-bonus -3})])))}
    :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target {:cost-bonus -3}))}

   "Careful Planning"
   {:prompt "Choose a card in or protecting a remote server"
    :choices {:req #(is-remote? (second (:zone %)))}
    :effect (effect (add-icon card target "CP" "red")
                    (system-msg (str "prevents the rezzing of " (card-str state target)
                                     " for the rest of this turn via Careful Planning"))
                    (register-events
                      card
                      [{:event :runner-turn-ends
                        :duration :end-of-turn
                        :effect (effect (remove-icon card target))}])
                    (register-turn-flag! card :can-rez
                                         (fn [state side card]
                                           (if (same-card? card target)
                                             ((constantly false)
                                              (toast state :corp "Cannot rez the rest of this turn due to Careful Planning"))
                                             true))))}

   "CBI Raid"
   (letfn [(cbi-final [chosen original]
             {:prompt (str "The top cards of R&D will be " (join  ", " (map :title chosen)) ".")
              :choices ["Done" "Start over"]
              :async true
              :effect (req (if (= target "Done")
                             (do (doseq [c (reverse chosen)]
                                   (move state :corp c :deck {:front true}))
                                 (clear-wait-prompt state :runner)
                                 (effect-completed state side eid))
                             (continue-ability state side (cbi-choice original '() (count original) original)
                                               card nil)))})
           (cbi-choice [remaining chosen n original]
             {:prompt "Choose a card to move next onto R&D"
              :choices remaining
              :async true
              :effect (req (let [chosen (cons target chosen)]
                             (if (< (count chosen) n)
                               (continue-ability state side (cbi-choice (remove-once #(= target %) remaining)
                                                                        chosen n original) card nil)
                               (continue-ability state side (cbi-final chosen original) card nil))))})]
     {:req (req hq-runnable)
      :async true
      :makes-run true
      :effect (effect (make-run
                        eid :hq
                        {:replace-access
                         {:msg "force the Corp to add all cards in HQ to the top of R&D"
                          :async true
                          :mandatory true
                          :effect (req (show-wait-prompt state :runner "Corp to add all cards in HQ to the top of R&D")
                                       (let [from (:hand corp)]
                                         (if (pos? (count from))
                                           (continue-ability state :corp (cbi-choice from '() (count from) from) card nil)
                                           (do (clear-wait-prompt state :runner)
                                               (effect-completed state side eid)))))}}
                        card))})

   "Code Siphon"
   {:req (req rd-runnable)
    :async true
    :makes-run true
    :effect (effect
              (make-run
                eid :rd
                {:replace-access
                 (let [rd-ice (fn [state] (* -3 (count (get-in @state [:corp :servers :rd :ices]))))]
                   {:async true
                    :prompt "Choose a program to install"
                    :msg (msg "install " (:title target) " and take 1 tag")
                    :choices (req (filter #(and (program? %)
                                                (runner-can-install? state side % false)
                                                (can-pay? state side eid card nil
                                                          [:credit (install-cost state side % {:cost-bonus (rd-ice state)})]))
                                          (:deck runner)))
                    :effect (effect (trigger-event :searched-stack nil)
                                    (shuffle! :deck)
                                    (runner-install
                                      (assoc eid :source card :source-type :runner-install)
                                      target {:cost-bonus (rd-ice state)})
                                    (gain-tags eid 1))})}
                card))}

   "Cold Read"
   {:implementation "Used programs restriction not enforced"
    :async true
    :prompt "Choose a server"
    :data {:counter {:credit 4}}
    :makes-run true
    :choices (req runnable-servers)
    :effect (effect (make-run eid target nil card))
    :interactions {:pay-credits {:type :credit}}
    :events [{:event :run-ends
              :prompt "Choose a program that was used during the run to trash "
              :choices {:req program?}
              :msg (msg "trash " (:title target))
              :async true
              :effect (effect (trash eid target {:unpreventable true}))}]}

   "Compile"
   (letfn [(compile-fn [where]
             {:prompt "Choose a program to install"
              :choices (req (cancellable (filter program? (get runner where))))
              :async true
              :effect (req (when (= :deck where)
                             (trigger-event state side :searched-stack nil)
                             (shuffle! state side :deck))
                           (runner-install state side (assoc eid :source card :source-type :runner-install)
                                           (assoc-in target [:special :compile-installed] true)
                                           {:ignore-all-cost true}))})]
     {:implementation "Trigger only on first encounter not enforced"
      :prompt "Choose a server"
      :msg "make a run and install a program on encounter with the first piece of ICE"
      :choices (req runnable-servers)
      :async true
      :makes-run true
      :effect (effect (make-run eid target nil card))
      :abilities [{:label "Install a program using Compile"
                   :prompt "Install a program from your Stack or Heap?"
                   :choices ["Stack" "Heap"]
                   :msg (msg "install a program from their " target)
                   :effect (effect (continue-ability
                                     (compile-fn (if (= "Stack" target) :deck :discard))
                                     card nil))}]
      :events [{:event :run-ends
                :effect (req (when-let [compile-installed (some #(when (get-in % [:special :compile-installed]) %)
                                                                (all-installed state :runner))]
                               (system-msg state side (str "moved " (:title compile-installed)
                                                           " to the bottom of the Stack at the end of the run from Compile"))
                               (move state :runner compile-installed :deck)))}]})

   "Contaminate"
   {:effect (req (resolve-ability
                   state side
                   {:msg (msg "place 3 virus tokens on " (:title target))
                    :choices {:req #(and (installed? %)
                                         (runner? %)
                                         (zero? (get-virus-counters state %)))}
                    :effect (req (add-counter state :runner target :virus 3))}
                   card nil))}

   "Corporate \"Grant\""
   {:events [{:event :runner-install
              ;; there are no current interactions where we'd want Grant to not be last, and this fixes a bug with Hayley
              :silent (req true)
              :req (req (first-event? state side :runner-install))
              :msg "force the Corp to lose 1 [Credit]"
              :effect (effect (lose-credits :corp 1))}]}

   "Corporate Scandal"
   {:msg "give the Corp 1 additional bad publicity"
    :implementation "No enforcement that this Bad Pub cannot be removed"
    :effect (req (swap! state update-in [:corp :bad-publicity :additional] inc))
    :leave-play (req (swap! state update-in [:corp :bad-publicity :additional] dec))}

   "Credit Crash"
   {:async true
    :makes-run true
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :effect (effect (make-run eid target nil card))
    :events [{:event :pre-access-card
              :once :per-run
              :async true
              :req (req (not= (:type target) "Agenda"))
              :effect (req (let [c target
                                 cost (:cost c)
                                 title (:title c)]
                             (if (can-pay? state :corp eid card nil [:credit cost])
                               (do (show-wait-prompt state :runner "Corp to decide whether or not to prevent the trash")
                                   (continue-ability
                                     state :corp
                                     {:optional
                                      {:prompt (msg "Spend " cost " [Credits] to prevent the trash of " title "?")
                                       :player :corp
                                       :yes-ability {:effect (req (lose-credits state :corp cost)
                                                                  (system-msg state :corp (str "spends " cost " [Credits] to prevent "
                                                                                               title " from being trashed at no cost"))
                                                                  (clear-wait-prompt state :runner))}
                                       :no-ability {:msg (msg "trash " title " at no cost")
                                                    :async true
                                                    :effect (effect (clear-wait-prompt :runner)
                                                                    (trash eid (assoc c :seen true) nil))}}}
                                     card nil))
                               (do (system-msg state side (str "uses Credit Crash to trash " title " at no cost"))
                                   (trash state side eid (assoc c :seen true) nil)))))}]}

   "Credit Kiting"
   {:req (req (some #{:hq :rd :archives} (:successful-run runner-reg)))
    :prompt "Select a card to install from your Grip"
    :choices {:five (req (and (not (event? target))
                              (in-hand? target)
                              (can-pay? state side eid card nil
                                        [:credit (install-cost state side target {:cost-bonus -8})])))}
    :async true
    :effect (req (wait-for (runner-install state :runner (make-eid state {:source card :source-type :runner-install})
                                           target {:cost-bonus -8})
                           (gain-tags state :runner eid 1)))}

   "Cyber Threat"
   {:prompt "Choose a server"
    :choices (req runnable-servers)
    :async true
    :makes-run true
    :effect (req (let [serv target]
                   (continue-ability
                     state :corp
                     (if (seq (filter #(and (installed? %)
                                            (not (rezzed? %))
                                            (ice? %)
                                            (can-pay? state side eid card nil
                                                      [:credit (rez-cost state side %)]))
                                      (all-installed state :corp)))
                       {:optional
                        {:prompt (msg "Rez a piece of ICE protecting " serv "?")
                         :yes-ability {:async true
                                       :prompt (msg "Select a piece of ICE protecting " serv " to rez")
                                       :player :corp
                                       :choices {:req #(and (installed? %)
                                                            (not (rezzed? %))
                                                            (ice? %)
                                                            (can-pay? state side eid card nil
                                                                      [:credit (rez-cost state side %)]))}
                                       :effect (effect (rez :corp eid target nil))
                                       :cancel-effect
                                       (effect (register-run-flag!
                                                 card
                                                 :can-rez
                                                 (fn [state side card]
                                                   (if (ice? card)
                                                     ((constantly false)
                                                      (toast state :corp "Cannot rez ICE on this run due to Cyber Threat"))
                                                     true)))
                                               (make-run eid serv nil card))}
                         :no-ability {:async true
                                      :effect (effect (register-run-flag!
                                                        card
                                                        :can-rez
                                                        (fn [state side card]
                                                          (if (ice? card)
                                                            ((constantly false)
                                                             (toast state :corp "Cannot rez ICE on this run due to Cyber Threat"))
                                                            true)))
                                                      (make-run eid serv nil card))
                                      :msg (msg "make a run on " serv " during which no ICE can be rezzed")}}}
                       {:async true
                        :effect (effect (register-run-flag!
                                          card
                                          :can-rez
                                          (fn [state side card]
                                            (if (ice? card)
                                              ((constantly false)
                                               (toast state :corp "Cannot rez ICE on this run due to Cyber Threat"))
                                              true)))
                                        (make-run eid serv nil card))
                        :msg (msg "make a run on " serv " during which no ICE can be rezzed")})
                     card nil)))}

   "Data Breach"
   {:async true
    :makes-run true
    :req (req rd-runnable)
    :effect (req (wait-for (make-run state side :rd nil card)
                           (let [card (get-card state card)]
                             (if (:run-again card)
                               (make-run state side eid :rd nil card)
                               (effect-completed state side eid)))))
    :events [{:event :successful-run-ends
              :optional {:req (req (and (not (:run-again card))
                                        (= [:rd] (:server target))))
                         :prompt "Make another run on R&D?"
                         :yes-ability {:effect (effect (clear-wait-prompt :corp)
                                                       (update! (assoc card :run-again true)))}}}]}

   "Day Job"
   {:additional-cost [:click 3]
    :msg "gain 10 [Credits]"
    :effect (effect (gain-credits 10))}

   "Deep Data Mining"
   {:async true
    :makes-run true
    :req (req rd-runnable)
    :effect (effect (make-run eid :rd nil card))
    :events [{:event :successful-run
              :silent (req true)
              :effect (effect (access-bonus :rd (max 0 (min 4 (available-mu state)))))}]}

   "Déjà Vu"
   {:prompt "Choose a card to add to Grip" :choices (req (cancellable (:discard runner) :sorted))
    :msg (msg "add " (:title target) " to their Grip")
    :effect (req (move state side target :hand)
                 (when (has-subtype? target "Virus")
                   (resolve-ability state side
                                    {:prompt "Choose a virus to add to Grip"
                                     :msg (msg "add " (:title target) " to their Grip")
                                     :choices (req (cancellable
                                                     (filter #(has-subtype? % "Virus") (:discard runner)) :sorted))
                                     :effect (effect (move target :hand))} card nil)))}

   "Demolition Run"
   {:req (req (or rd-runnable hq-runnable))
    :prompt "Choose a server"
    :choices ["HQ" "R&D"]
    :makes-run true
    :async true
    :effect (effect (make-run eid target nil card nil))
    :interactions {:access-ability
                   {:label "Trash card"
                    :msg (msg "trash " (:title target) " at no cost")
                    :async true
                    :effect (effect (trash eid (assoc target :seen true) nil))}}}

   "Deuces Wild"
   (let [all [{:effect (effect (gain-credits 3))
               :msg "gain 3 [Credits]"}
              {:async true
               :effect (effect (draw eid 2 nil))
               :msg "draw 2 cards"}
              {:effect (effect (lose-tags 1))
               :msg "remove 1 tag"}
              {:prompt "Select 1 piece of ice to expose"
               :msg "expose 1 ice and make a run"
               :choices {:req #(and (installed? %)
                                    (ice? %))}
               :async true
               :effect (req (wait-for (expose state side target)
                                      (continue-ability
                                        state side
                                        {:prompt "Choose a server"
                                         :choices (req runnable-servers)
                                         :async true
                                         :effect (effect (make-run eid target))}
                                        card nil)))}]
         choice (fn choice [abis]
                  {:prompt "Choose an ability to resolve"
                   :choices (map #(capitalize (:msg %)) abis)
                   :async true
                   :effect (req (let [chosen (some #(when (= target (capitalize (:msg %))) %) abis)]
                                  (wait-for
                                    (resolve-ability state side chosen card nil)
                                    (if (= (count abis) 4)
                                      (continue-ability state side (choice (remove-once #(= % chosen) abis)) card nil)
                                      (effect-completed state side eid)))))})]
     {:async true
      :effect (effect (continue-ability (choice all) card nil))})

   "Diana's Hunt"
   {:implementation "One program per encounter not enforced"
    :prompt "Choose a server"
    :msg "make a run and install a program on encounter with each ICE"
    :choices (req runnable-servers)
    :async true
    :makes-run true
    :effect (effect (make-run eid target nil card))
    :abilities [{:label "Install a program using Diana's Hunt"
                 :async true
                 :req (req current-ice)
                 :effect
                 (effect
                   (continue-ability
                     {:prompt "Choose a program in your grip to install"
                      :choices {:req #(and (in-hand? %)
                                           (program? %)
                                           (runner-can-install? state side % false))}
                      :msg (msg "install " (:title target) ", ignoring all costs")
                      :effect (req (let [diana-card (assoc-in target [:special :diana-installed] true)]
                                     (update! state side (update-in card [:special :diana] conj diana-card))
                                     (runner-install state side eid diana-card {:ignore-all-cost true})))}
                     card nil))}]
    :events [{:event :run-ends
              :effect (req (doseq [c (get-in card [:special :diana])]
                             (let [installed (find-cid (:cid c) (all-installed state side))]
                               (when (get-in installed [:special :diana-installed])
                                 (system-msg state side (str "trashes " (:title c) " at the end of the run from Diana's Hunt"))
                                 (trash state side installed {:unpreventable true})))))}]}

   "Diesel"
   {:msg "draw 3 cards"
    :async true
    :effect (effect (draw eid 3 nil))}

   "Direct Access"
   {:async true
    :makes-run true
    :effect (req (doseq [s [:corp :runner]]
                   (disable-identity state s))
                 (continue-ability
                   state side
                   {:prompt "Choose a server"
                    :choices (req runnable-servers)
                    :async true
                    :effect (effect (make-run eid target nil card))}
                   card nil))
    :abilities [(set-autoresolve :auto-reshuffle "reshuffle")]
    :events [{:event :run-ends
              :async true
              :effect (req (doseq [s [:corp :runner]]
                             (enable-identity state s))
                           (continue-ability
                             state :runner
                             {:optional
                              {:autoresolve (get-autoresolve :auto-reshuffle)
                               :prompt "Shuffle Direct Access into the Stack?"
                               :yes-ability
                               {:msg (msg "shuffles Direct Access into the Stack")
                                :effect (effect (move (get-card state card) :deck)
                                                (shuffle! :deck)
                                                (unregister-events card))}}}
                             card nil))}]}

   "Dirty Laundry"
   {:async true
    :makes-run true
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :effect (effect (make-run eid target nil card))
    :events [{:event :successful-run-ends
              :silent (req true)
              :msg "gain 5 [Credits]"
              :effect (effect (gain-credits :runner 5))}]}

   "Diversion of Funds"
   {:req (req hq-runnable)
    :async true
    :makes-run true
    :effect (effect (make-run
                      eid :hq
                      {:req (req (= target :hq))
                       :replace-access
                       (let [five-or-all (fn [corp] (min 5 (:credit corp)))]
                         {:msg (msg "force the Corp to lose " (five-or-all corp)
                                    "[Credits], and gain " (five-or-all corp) "[Credits]")
                          :effect (effect (lose-credits :corp (five-or-all corp))
                                          (gain-credits :runner (five-or-all corp)))})}
                      card))}

   "Divide and Conquer"
   {:req (req archives-runnable)
    :makes-run true
    :async true
    :effect (effect (make-run eid :archives nil card))
    :events [{:event :end-access-phase
              :async true
              :req (req (= :archives (:from-server target)))
              :effect (req (wait-for (do-access state side [:hq] {:no-root true})
                                     (do-access state side eid [:rd] {:no-root true})))}]}

   "Drive By"
   {:choices {:req #(let [topmost (get-nested-host %)]
                      (and (is-remote? (second (:zone topmost)))
                           (= (last (:zone topmost)) :content)
                           (not (:rezzed %))))}
    :async true
    :effect (req (wait-for (expose state side target) ;; would be nice if this could return a value on completion
                           (if async-result ;; expose was successful
                             (if (#{"Asset" "Upgrade"} (:type target))
                               (do (system-msg state :runner (str "uses Drive By to trash " (:title target)))
                                   (trash state side (assoc target :seen true))
                                   ;; Turn on Reprisal cards
                                   (swap! state assoc-in [:runner :register :trashed-card] true)
                                   (effect-completed state side eid))
                               (effect-completed state side eid))
                             (effect-completed state side eid))))}

   "Early Bird"
   {:async true
    :makes-run true
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :msg (msg "make a run on " target " and gain [Click]")
    :effect (effect (gain :click 1)
                    (make-run eid target nil card))}

   "Easy Mark"
   {:msg "gain 3 [Credits]"
    :effect (effect (gain-credits 3))}

   "Embezzle"
   {:async true
    :makes-run true
    :req (req hq-runnable)
    :effect
    (effect
      (make-run eid :hq
                {:req (req (= target :hq))
                 :replace-access
                 {:mandatory true
                  :prompt "Choose a card type"
                  :choices ["Asset" "Upgrade" "Operation" "ICE"]
                  :msg (msg "reveal 2 cards from HQ and trash all "
                            target (when (not= "ICE" (:type target)) "s"))
                  :effect (req (let [cards-to-reveal (take 2 (shuffle (:hand corp)))
                                     cards-to-trash (filter #(is-type? % target) cards-to-reveal)
                                     credits (* 4 (count cards-to-trash))]
                                 (system-msg state side
                                             (str "uses Embezzle to reveal "
                                                  (join " and " (map :title cards-to-reveal))
                                                  " from HQ"))
                                 (reveal state side cards-to-reveal)
                                 (when (pos? credits)
                                   (system-msg state side
                                               (str " uses Embezzle to trash "
                                                    (join " and " (map :title cards-to-trash))
                                                    " from HQ and gain "
                                                    credits " [Credits]"))
                                   (trash-cards state :runner (map #(assoc % :seen true) cards-to-trash))
                                   (gain-credits state :runner credits))))}}
                card))}

   "Emergency Shutdown"
   {:req (req (some #{:hq} (:successful-run runner-reg)))
    :msg (msg "derez " (:title target))
    :choices {:req #(and (ice? %)
                         (rezzed? %))}
    :effect (effect (derez target))}

   "Emergent Creativity"
   (letfn [(ec [trash-cost to-trash]
             {:async true
              :prompt "Choose a hardware or program to install"
              :msg (msg "trash " (if (empty? to-trash) "no cards" (join ", " (map :title to-trash)))
                        " and install " (:title target)
                        " lowering the cost by " trash-cost)
              :choices (req (cancellable (filter #(or (program? %)
                                                      (hardware? %)
                                                      (can-pay? state side eid card nil
                                                                [:credit (install-cost state side % {:cost-bonus (- trash-cost)})]))
                                                 (:deck runner)) :sorted))
              :effect (req (trigger-event state side :searched-stack nil)
                           (shuffle! state side :deck)
                           (doseq [c to-trash]
                             (trash state side c {:unpreventable true}))
                           (runner-install state side (make-eid state {:source card :source-type :runner-install})
                                           target {:cost-bonus (- trash-cost)})
                           (effect-completed state side eid))})]
     {:prompt "Choose Hardware and Programs to trash from your Grip"
      :choices {:req #(and (or (hardware? %)
                               (program? %))
                        (in-hand? %))
                :max (req (count (:hand runner)))}
      :cancel-effect (effect (resolve-ability (ec 0 []) card nil))
      :effect (req (let [trash-cost (apply + (map :cost targets))
                         to-trash targets]
                     (resolve-ability state side (ec trash-cost to-trash) card nil)))})

   "Employee Strike"
   {:msg "disable the Corp's identity"
    :disable-id true
    :effect (effect (disable-identity :corp))
    :leave-play (effect (enable-identity :corp))}

   "En Passant"
   {:async true
    :req (req (let [server (first (flatten (turn-events state side :run)))]
                (and (:successful-run runner-reg)
                     (seq (filter (complement rezzed?) (get-in @state (concat [:corp :servers] [server] [:ices])))))))
    :effect (effect
              (continue-ability
                (let [server (first (flatten (turn-events state side :run)))]
                  {:async true
                   :prompt (str "Choose an unrezzed piece of ICE protecting "
                                (zone->name server) " that you passed on your last run")
                   :choices {:req #(and (ice? %)
                                        (not (rezzed? %))
                                        (= server (second (:zone %))))}
                   :msg (msg "trash " (card-str state target))
                   :effect (req (swap! state assoc-in [:runner :register :trashed-card] true)
                                (trash state side eid target nil))})
                card nil))}

   "Encore"
   {:req (req (and (some #{:hq} (:successful-run runner-reg))
                   (some #{:rd} (:successful-run runner-reg))
                   (some #{:archives} (:successful-run runner-reg))))
    :rfg-instead-of-trashing true
    :msg "take an additional turn after this one"
    :effect (req (swap! state update-in [:runner :extra-turns] (fnil inc 0)))}

   "Escher"
   (letfn [(es [] {:async true
                   :prompt "Select two pieces of ICE to swap positions"
                   :choices {:req #(and (installed? %)
                                        (ice? %))
                             :max 2}
                   :effect (req (if (= (count targets) 2)
                                  (do (swap-ice state side (first targets) (second targets))
                                      (continue-ability state side (es) card nil))
                                  (do (system-msg state side "has finished rearranging ICE")
                                      (effect-completed state side eid))))})]
     {:async true
      :makes-run true
      :req (req hq-runnable)
      :effect (effect (make-run eid :hq
                                {:replace-access
                                 {:mandatory true
                                  :async true
                                  :msg "rearrange installed ICE"
                                  :effect (effect (continue-ability (es) card nil))}}
                                card))})

   "Eureka!"
   {:effect (req (let [topcard (first (:deck runner))
                       caninst (and (or (hardware? topcard)
                                        (program? topcard)
                                        (resource? topcard))
                                    (can-pay? state side eid card nil
                                              [:credit (install-cost state side topcard {:cost-bonus -10})]))]
                   (if caninst
                     (resolve-ability
                       state side
                       {:optional {:prompt (msg "Install " (:title topcard) "?")
                                   :yes-ability {:effect (effect (runner-install eid topcard {:cost-bonus -10}))}
                                   :no-ability {:effect (effect (trash topcard {:unpreventable true})
                                                                (reveal topcard)
                                                                (system-msg (str "reveals and trashes "
                                                                                 (:title topcard))))}}}
                       card nil)
                     (do (reveal state side topcard)
                         (trash state side topcard {:unpreventable true})
                         (system-msg state side (str "reveals and trashes " (:title topcard)))))))}

   "Exclusive Party"
   {:msg (msg "draw 1 card and gain "
              (count (filter #(= (:title %) "Exclusive Party") (:discard runner)))
              " [Credits]")
    :async true
    :effect (req (wait-for (draw state side 1 nil)
                           (gain-credits state side (count (filter #(= (:title %) "Exclusive Party") (:discard runner))))
                           (effect-completed state side eid)))}

   "Executive Wiretaps"
   {:msg (msg "reveal cards in HQ: " (join ", " (sort (map :title (:hand corp)))))
    :effect (effect (reveal (:hand corp)))}

   "Exploit"
   {:req (req (and (some #{:hq} (:successful-run runner-reg))
                   (some #{:rd} (:successful-run runner-reg))
                   (some #{:archives} (:successful-run runner-reg))))
    :prompt "Choose up to 3 pieces of ICE to derez"
    :choices {:max 3 :req #(and (rezzed? %) (ice? %))}
    :msg (msg "derez " (join ", " (map :title targets)))
    :effect (req (doseq [c targets]
                   (derez state side c)))}

   "Exploratory Romp"
   {:async true
    :makes-run true
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :effect (effect (make-run
                      eid target
                      {:replace-access
                       {:prompt "Advancements to remove from a card in or protecting this server?"
                        :choices ["0" "1" "2" "3"]
                        :async true
                        :mandatory true
                        :effect (req (let [c (str->int target)]
                                       (show-wait-prompt state :corp "Runner to remove advancements")
                                       (continue-ability
                                         state side
                                         {:choices {:req #(and (contains? % :advance-counter)
                                                               (= (first (:server run)) (second (:zone %))))}
                                          :msg (msg "remove " (quantify c "advancement token")
                                                    " from " (card-str state target))
                                          :effect (req (let [to-remove (min c (get-counters target :advancement))]
                                                         (add-prop state :corp target :advance-counter (- to-remove))
                                                         (clear-wait-prompt state :corp)
                                                         (effect-completed state side eid)))}
                                         card nil)))}}
                      card))}

   "Express Delivery"
   {:prompt "Choose a card to add to your Grip" :choices (req (take 4 (:deck runner)))
    :msg "look at the top 4 cards of their Stack and add 1 of them to their Grip"
    :effect (effect (move target :hand) (shuffle! :deck))}

   "Falsified Credentials"
   {:prompt "Choose a type"
    :choices ["Agenda" "Asset" "Upgrade"]
    :msg (msg "guess " target)
    :async true
    :effect (effect
              (continue-ability
                (let [chosen-type target]
                  {:choices {:req #(let [topmost (get-nested-host %)]
                                     (and (is-remote? (second (:zone topmost)))
                                          (= (last (:zone topmost)) :content)
                                          (not (rezzed? %))))}
                   :async true
                   :effect (req             ;taken from Drive By - maybe refactor
                                (wait-for (expose state side target)
                                          (if (and async-result ;; expose was successful
                                                   (= chosen-type (:type target)))
                                            (continue-ability
                                              state :runner
                                              {:effect (effect (gain-credits 5))
                                               :msg "gain 5 [Credits] "}
                                              card nil)
                                            (effect-completed state side eid))))})
                card nil))}

   "Fear the Masses"
   {:async true
    :makes-run true
    :req (req hq-runnable)
    :effect (effect
              (make-run
                eid :hq
                {:req (req (= target :hq))
                 :replace-access
                 {:async true
                  :mandatory true
                  :msg "force the Corp to trash the top card of R&D"
                  :effect (effect (mill :corp)
                                  (continue-ability
                                    (let [n (count (filter #(same-card? :title card %) (:hand runner)))]
                                      {:prompt "Reveal how many copies of Fear the Masses?"
                                       :choices {:req #(and (in-hand? %)
                                                            (same-card? :title card %))
                                                 :max n}
                                       :msg (msg "reveal " (count targets) " copies of Fear the Masses,"
                                                 " forcing the Corp to trash " (count targets)
                                                 " additional cards from the top of R&D")
                                       :effect (effect (reveal targets)
                                                       (mill :corp (count targets)))})
                                    card nil))}}
                card))}

   "Feint"
   {:implementation "Bypass is manual"
    :async true
    :makes-run true
    :req (req hq-runnable)
    :effect (effect (make-run eid :hq nil card))
    :events [{:event :successful-run
              :effect (effect (prevent-access))}]}

   "Fisk Investment Seminar"
   {:msg "make each player draw 3 cards"
    :async true
    :effect (req (wait-for (draw state :runner 3 nil)
                           (draw state :corp eid 3 nil)))}

   "Forged Activation Orders"
   {:choices {:req #(and (ice? %)
                         (not (rezzed? %)))}
    :effect (req (let [ice target
                       serv (zone->name (second (:zone ice)))
                       icepos (ice-index state ice)]
                   (resolve-ability
                     state :corp
                     {:prompt (msg "Rez " (:title ice) " at position " icepos
                                   " of " serv " or trash it?") :choices ["Rez" "Trash"]
                      :effect (effect (resolve-ability
                                        (if (and (= target "Rez")
                                                 (<= (rez-cost state :corp ice)
                                                     (:credit corp)))
                                          {:msg (msg "force the rez of " (:title ice))
                                           :effect (effect (rez :corp ice))}
                                          {:msg (msg "trash the ICE at position " icepos " of " serv)
                                           :effect (effect (trash :corp ice))})
                                        card nil))}
                     card nil)))}

   "Forked"
   (cutlery "Sentry")

   "Frame Job"
   {:prompt "Choose an agenda to forfeit"
    :choices (req (:scored runner))
    :effect (effect (forfeit target)
                    (gain-bad-publicity :corp 1))
    :msg (msg "forfeit " (:title target) " and give the Corp 1 bad publicity")}

   "Frantic Coding"
   {:async true
    :effect
    (effect
      (continue-ability
        (let [top-ten (take 10 (:deck runner))]
          {:prompt (str "The top 10 cards of the stack are " (join ", " (map :title top-ten)) ".")
           :choices ["OK"]
           :async true
           :effect
           (effect
             (continue-ability
               {:prompt "Install a program?"
                :choices (concat
                           (->> top-ten
                                (filter #(and (program? %)
                                              (can-pay? state side eid card nil
                                                        [:credit (install-cost state side % {:cost-bonus -5})])))
                                (sort-by :title)
                                (into []))
                           ["No install"])
                :async true
                :effect (req (if (not= target "No install")
                               (let [number-of-shuffles (count (turn-events state :runner :runner-shuffle-deck))
                                     to-trash (remove #(same-card? % target) top-ten)]
                                 (wait-for (runner-install state side (make-eid state {:source card :source-type :runner-install})
                                                           target {:cost-bonus -5})
                                           (if (= number-of-shuffles (count (turn-events state :runner :runner-shuffle-deck)))
                                             (do (system-msg state side (str "trashes " (join ", " (map :title to-trash))))
                                                 (doseq [c to-trash]
                                                   (trash state side c {:unpreventable true}))
                                                 (effect-completed state side eid))
                                             (do (system-msg state side "does not have to trash cards because the stack was shuffled")
                                                 (effect-completed state side eid)))))
                               (do (doseq [c top-ten]
                                     (trash state side c {:unpreventable true}))
                                   (system-msg state side (str "trashes " (join ", " (map :title top-ten))))
                                   (effect-completed state side eid))))}
               card nil))})
        card nil))}

   "\"Freedom Through Equality\""
   {:events [{:event :agenda-stolen
              :msg "add it to their score area as an agenda worth 1 agenda point"
              :async true
              :effect (req (as-agenda state :runner eid card 1))}]}

   "Freelance Coding Contract"
   {:choices {:max 5
              :req #(and (program? %)
                         (in-hand? %))}
    :msg (msg "trash " (join ", " (map :title targets)) " and gain "
              (* 2 (count targets)) " [Credits]")
    :effect (req (doseq [c targets]
                   (trash state side c {:unpreventable true}))
                 (gain-credits state side (* 2 (count targets))))}

   "Game Day"
   {:msg (msg "draw " (- (hand-size state :runner) (count (:hand runner))) " cards")
    :async true
    :effect (effect (draw eid (- (hand-size state :runner) (count (:hand runner))) nil))}

   "Glut Cipher"
   (let [corp-choose {:show-discard true
                      :async true
                      :player :corp
                      :prompt "Select 5 cards from Archives to add to HQ"
                      :choices {:max 5
                                :all true
                                :req #(and (corp? %)
                                           (in-discard? %))}
                      :msg (msg "move "
                                (let [seen (filter :seen targets)
                                      m (count  (remove :seen targets))]
                                  (str (join ", " (map :title seen))
                                       (when (pos? m)
                                         (str (when-not (empty? seen) " and ")
                                              (quantify m "unseen card")))
                                       " into HQ, then trash 5 cards")))
                      :effect (req (doseq [c targets]
                                     (move state side c :hand))
                                   (trash-cards state :corp eid (take 5 (shuffle (:hand (:corp @state))))))}
         access-effect {:mandatory true
                        :async true
                        :req (req (<= 5 (count (:discard corp))))
                        :effect (req (show-wait-prompt
                                       state :runner
                                       "Corp to choose which cards to pick up from Archives")
                                     (wait-for (resolve-ability state side corp-choose card nil)
                                               (clear-wait-prompt state :runner)
                                               (effect-completed state side eid)))}]
     {:req (req archives-runnable)
      :async true
      :makes-run true
      :effect (effect (make-run eid :archives
                                {:req (req (= target :archives))
                                 :replace-access access-effect}
                                card))})

   "Government Investigations"
   {:flags {:prevent-secretly-spend (req 2)}}

   "Guinea Pig"
   {:msg "trash all cards in the grip and gain 10 [Credits]"
    :effect (req (doseq [c (:hand runner)]
                   (trash state :runner c {:unpreventable true}))
                 (gain-credits state :runner 10))}

   "Hacktivist Meeting"
   {:constant-effects [{:type :rez-additional-cost
                        :req (req (not (ice? target)))
                        :value [:randomly-trash-from-hand 1]}]}

   "High-Stakes Job"
   {:async true
    :makes-run true
    :prompt "Choose a server"
    :choices (req (let [unrezzed-ice #(seq (filter (complement rezzed?) (:ices (second %))))
                        bad-zones (keys (filter (complement unrezzed-ice) (get-in @state [:corp :servers])))]
                    (zones->sorted-names (remove (set bad-zones) (get-runnable-zones state side eid card nil)))))
    :effect (effect (make-run eid target nil card))
    :events [{:event :successful-run-ends
              :silent (req true)
              :msg "gain 12 [Credits]"
              :effect (effect (gain-credits :runner 12))}]}

   "Hostage"
   {:prompt "Choose a Connection"
    :choices (req (cancellable (filter #(has-subtype? % "Connection") (:deck runner)) :sorted))
    :msg (msg "add " (:title target) " to their Grip and shuffle their Stack")
    :effect (effect (trigger-event :searched-stack nil)
                    (continue-ability
                     (let [connection target
                           install-hostage {:effect (effect (runner-install (assoc eid :source card :source-type :runner-install) connection nil)
                                                            (shuffle! :deck))}
                           grip-hostage {:effect (effect (move connection :hand)
                                                         (shuffle! :deck))}]
                       (if (can-pay? state side eid card nil :credit (:cost connection))
                         {:optional {:prompt (str "Install " (:title connection) "?")
                                     :yes-ability install-hostage
                                     :no-ability grip-hostage}}
                         grip-hostage))
                     card nil))}

   "Hot Pursuit"
   {:async true
    :makes-run true
    :req (req hq-runnable)
    :effect (effect (make-run eid :hq nil card))
    :events [{:event :successful-run
              :async true
              :msg "gain 9 [Credits] and take 1 tag"
              :effect (req (wait-for (gain-tags state :runner 1)
                                     (gain-credits state :runner 9)
                                     (effect-completed state side eid)))}]}

   "Isolation"
   {:additional-cost [:resource 1]
    :msg "gain 7 [Credits]"
    :effect (effect (gain-credits 7))}

   "I've Had Worse"
   {:async true
    :effect (effect (draw eid 3 nil))
    :trash-effect {:when-inactive true
                   :async true
                   :req (req (#{:meat :net} target))
                   :effect (effect (draw :runner eid 3 nil)) :msg "draw 3 cards"}}

   "Immolation Script"
   {:async true
    :makes-run true
    :req (req archives-runnable)
    :effect (effect (make-run eid :archives nil card))
    :events [{:event :pre-access
              :async true
              :req (req (and (= target :archives)
                             ;; don't prompt unless there's at least 1 rezzed ICE matching one in Archives
                             (not-empty (clojure.set/intersection
                                          (into #{} (map :title (filter ice? (:discard corp))))
                                          (into #{} (map :title (filter rezzed? (all-installed state :corp))))))))
              :prompt "Choose a piece of ICE in Archives"
              :choices (req (filter ice? (:discard corp)))
              :effect (effect (continue-ability
                                (let [ice target]
                                  {:async true
                                   :prompt (msg "Select a rezzed copy of " (:title ice) " to trash")
                                   :choices {:req #(and (ice? %)
                                                        (rezzed? %)
                                                        (same-card? :title % ice))}
                                   :msg (msg "trash " (card-str state target))
                                   :effect (effect (trash eid target nil))})
                                card nil))}]}

   "In the Groove"
   {:events [{:event :runner-install
              :duration :end-of-turn
              :req (req (<= 1 (:cost target)))
              :interactive (req (has-subtype? target "Cybernetic"))
              :async true
              :prompt "What to get from In the Groove?"
              :choices ["Draw 1 card" "Gain 1 [Credits]"]
              :msg (msg (lower-case target))
              :effect (req (if (= target "Draw 1 card")
                             (draw state side eid 1 nil)
                             (do (gain-credits state side 1)
                                 (effect-completed state side eid))))}]}

   "Independent Thinking"
   (letfn [(cards-to-draw [targets]
             (* (count targets)
                (if (some #(and (not (facedown? %)) (has-subtype? % "Directive")) targets) 2 1)))]
     {:async true
      :prompt "Choose up to 5 installed cards to trash with Independent Thinking"
      :choices {:max 5
                :req #(and (installed? %)
                        (runner? %))}
      :effect (req (wait-for (trash-cards state side targets nil)
                             (draw state :runner eid (cards-to-draw targets) nil)))
      :msg (msg "trash " (join ", " (map :title targets)) " and draw " (quantify (cards-to-draw targets) "card"))})

   "Indexing"
   {:req (req rd-runnable)
    :async true
    :makes-run true
    :effect (effect (make-run
                      eid :rd
                      {:req (req (= target :rd))
                       :replace-access
                       {:msg "rearrange the top 5 cards of R&D"
                        :async true
                        :effect (req (show-wait-prompt state :corp "Runner to rearrange the top cards of R&D")
                                     (let [from (take 5 (:deck corp))]
                                       (if (pos? (count from))
                                         (continue-ability state side (reorder-choice :corp :corp from '()
                                                                                      (count from) from) card nil)
                                         (do (clear-wait-prompt state :corp)
                                             (effect-completed state side eid)))))}}
                      card))}

   "Infiltration"
   {:prompt "Gain 2 [Credits] or expose a card?"
    :choices ["Gain 2 [Credits]" "Expose a card"]
    :effect (effect (continue-ability
                      (if (= target "Expose a card")
                        {:choices {:req #(and (installed? %)
                                              (not (rezzed? %)))}
                         :async true
                         :effect (effect (expose eid target))}
                        {:msg "gain 2 [Credits]"
                         :effect (effect (gain-credits 2))})
                      card nil))}

   "Information Sifting"
   (letfn [(access-pile [cards pile pile-size]
             {:prompt "Choose a card to access. You must access all cards."
              :choices [(str "Card from pile " pile)]
              :async true
              :req (req (if (:max-access run)
                          (< (total-cards-accessed run) (:max-access run))
                          true))
              :effect (req (wait-for
                             (access-card state side (first cards))
                             (if (< 1 (count cards))
                               (continue-ability state side (access-pile (rest cards) pile pile-size) card nil)
                               (effect-completed state side eid))))})
           (which-pile [p1 p2]
             {:prompt "Choose a pile to access"
              :choices [(str "Pile 1 (" (quantify (count p1) "card") ")")
                        (str "Pile 2 (" (quantify (count p2) "card") ")")]
              :async true
              :effect (req (let [choice (if (starts-with? target "Pile 1") 1 2)]
                             (clear-wait-prompt state :corp)
                             (system-msg state side (str "chooses to access " target))
                             (continue-ability
                               state side
                               (access-pile (if (= 1 choice) p1 p2) choice (count (if (= 1 choice) p1 p2)))
                               card nil)))})]
     (let [access-effect
           {:async true
            :mandatory true
            :effect (req (if (< 1 (count (:hand corp)))
                           (do (show-wait-prompt state :runner "Corp to create two piles")
                               (continue-ability
                                 state :corp
                                 {:async true
                                  :prompt (msg "Select up to " (dec (count (:hand corp))) " cards for the first pile")
                                  :choices {:req #(and (in-hand? %) (corp? %))
                                            :max (req (dec (count (:hand corp))))}
                                  :effect (effect (clear-wait-prompt :runner)
                                                  (show-wait-prompt :corp "Runner to select a pile")
                                                  (continue-ability
                                                    :runner
                                                    (which-pile (shuffle targets)
                                                                (shuffle (vec (clojure.set/difference
                                                                                (set (:hand corp)) (set targets)))))
                                                    card nil))}
                                 card nil))
                           (effect-completed state side eid)))}]
       {:async true
        :makes-run true
        :req (req hq-runnable)
        :effect (effect (make-run eid :hq
                                  {:req (req (= target :hq))
                                   :replace-access access-effect}
                                  card))}))

   "Inject"
   {:effect (req (doseq [c (take 4 (get-in @state [:runner :deck]))]
                   (if (program? c)
                     (do (trash state side c {:unpreventable true})
                         (gain-credits state side 1)
                         (system-msg state side (str "trashes " (:title c) " and gains 1 [Credits]")))
                     (do (move state side c :hand)
                         (system-msg state side (str "adds " (:title c) " to Grip"))))))}

   "Injection Attack"
   {:async true
    :makes-run true
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :effect (effect (continue-ability
                      (let [server target]
                        {:async true
                         :prompt "Select an icebreaker"
                         :choices {:req #(and (installed? %)
                                              (has-subtype? % "Icebreaker"))}
                         :effect (effect (pump target 2 :end-of-run)
                                         (make-run eid server nil card))})
                      card nil))}

   "Inside Job"
   {:implementation "Bypass is manual"
    :async true
    :makes-run true
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :effect (effect (make-run eid target nil card))}

   "Insight"
   {:async true
    :effect (req
             (let [from (take 4 (:deck corp))]
               (when (pos? (count from))
                 (show-wait-prompt state :runner (str "Corp to rearrange the top " (count from) " cards of R&D"))
                 (wait-for (resolve-ability state :corp (reorder-choice :corp from) card targets)
                           (clear-wait-prompt state :runner)
                           (let [top-4 (take 4 (get-in @state [:corp :deck]))]
                             (reveal state side top-4)
                             (system-msg state :runner (str " reveals (top:) " (join ", " (map :title top-4)) " from the top of R&D")))
                           (effect-completed state side eid)))))}

   "Interdiction"
   (let [ab (effect (register-turn-flag!
                      card :can-rez
                      (fn [state side card]
                        (if (and (= (:active-player @state) :runner) (not (ice? card)))
                          ((constantly false)
                           (toast state :corp "Cannot rez non-ICE on the Runner's turn due to Interdiction"))
                          true))))]
     {:msg "prevent the Corp from rezzing non-ICE cards on the Runner's turn"
      :effect ab
      :events [{:event :runner-turn-begins
                :effect ab}]
      :leave-play (req (clear-all-flags-for-card! state side card))})

   "Itinerant Protesters"
   {:msg "reduce the Corp's maximum hand size by 1 for each bad publicity"
    :effect (req (change-hand-size state :corp (- (count-bad-pub state)))
                 (add-watch state :itin
                            (fn [k ref old new]
                              (let [bpnew (count-bad-pub (atom new))
                                    bpold (count-bad-pub (atom old))
                                    bpchange (- bpnew bpold)]
                                (when-not (zero? bpchange)
                                  (change-hand-size state :corp (- bpchange)))))))
    :leave-play (req (remove-watch state :itin)
                     (change-hand-size state :corp (count-bad-pub state)))}

   "Khusyuk"
   (let [access-revealed (fn [revealed]
                           {:async true
                            :prompt "Which of the revealed cards would you like to access (first card is on top)?"
                            :not-distinct true
                            :choices revealed
                            :req (req (not= (:max-access run) 0))
                            :effect (effect (access-card eid target))})
         select-install-cost (fn [state]
                               (let [current-values
                                     (->> (all-active-installed state :runner)
                                          (map :cost)
                                          (filter identity)
                                          (remove zero?)
                                          frequencies
                                          (merge {1 0})
                                          (into (sorted-map)))]
                                 {:async true
                                  :prompt "Select an install cost from among your installed cards."
                                  ;; We don't want to generate 99 prompt buttons, so only add 99 at the end
                                  :choices (mapv str (for [x (->> current-values keys last inc (range 1) (#(concat % [99])))]
                                                       (str x " [Credit]: "
                                                            (quantify (get current-values x 0) "card"))))
                                  :effect (effect (effect-completed
                                                    (make-result eid [(str->int (first (split target #" ")))
                                                                      (min 6 (str->int (nth (split target #" ") 2)))])))}))]
     {:async true
      :makes-run true
      :req (req rd-runnable)
      :effect (effect
                (make-run
                  eid :rd
                  {:req (req (= target :rd))
                   :replace-access
                   {:async true
                    :mandatory true
                    :effect (req (wait-for
                                   (resolve-ability state side (select-install-cost state) card nil)
                                   (let [revealed (seq (take (second async-result) (:deck corp)))]
                                     (system-msg state :runner (str "uses Khusyuk to choose an install cost of "
                                                                    (first async-result)
                                                                    " [Credit] and reveals "
                                                                    (if revealed
                                                                      (str "(top:) " (join ", " (map :title revealed))
                                                                           " from the top of R&D")
                                                                      "no cards")))
                                     (when revealed
                                       (reveal state side revealed))
                                     (wait-for
                                       (resolve-ability state side (when (and revealed (empty? (get-cards-to-access state)))
                                                                     (access-revealed revealed)) card nil)
                                       (shuffle! state :corp :deck)
                                       (system-msg state :runner "shuffles R&D")
                                       (effect-completed state side eid)))))}}
                  card))})

   "Knifed"
   (cutlery "Barrier")

   "Kraken"
   {:async true
    :req (req (:stole-agenda runner-reg))
    :prompt "Choose a server"
    :choices (req servers)
    :msg (msg "force the Corp to trash an ICE protecting " target)
    :effect (effect
              (continue-ability
                (let [serv (second (server->zone state target))
                      servname target]
                  {:player :corp
                   :async true
                   :prompt (msg "Select a piece of ICE in " target " to trash")
                   :choices {:req #(and (ice? %)
                                        (= serv (second (:zone %))))}
                   :effect (effect (system-msg (str "trashes " (card-str state target)))
                                   (trash :corp eid target nil))})
                card nil))}

   "Labor Rights"
   {:req (req (pos? (+ (count (:deck runner)) (count (:discard runner)))))
    :rfg-instead-of-trashing true
    :async true
    :effect (req (let [mill-count (min 3 (count (:deck runner)))]
                   (mill state :runner :runner mill-count)
                   (system-msg state :runner (str "trashes the top " (quantify mill-count "card") " of their stack"))
                   (let [heap-count (min 3 (count (get-in @state [:runner :discard])))]
                     (continue-ability
                       state side
                       {:prompt (str "Choose " (quantify heap-count "card") " to shuffle into the stack")
                        :show-discard true
                        :async true
                        :choices {:max heap-count
                                  :all true
                                  :not-self true
                                  :req #(and (runner? %)
                                             (in-discard? %))}
                        :effect (req (doseq [c targets]
                                       (move state side c :deck))
                                     (system-msg state :runner (str "shuffles " (join ", " (map :title targets))
                                                                    " from their Heap into their Stack, and draws 1 card"))
                                     (shuffle! state :runner :deck)
                                     (wait-for (draw state :runner 1 nil)
                                               (effect-completed state side eid)))}
                       card nil))))}

   "Lawyer Up"
   {:msg "remove 2 tags and draw 3 cards"
    :async true
    :effect (req (wait-for (draw state side 3 nil)
                           (lose-tags state side eid 2)))}

   "Lean and Mean"
   {:async true
    :makes-run true
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :msg (msg "make a run on " target
              (when (<= (count (filter program? (all-active-installed state :runner))) 3)
                ", adding +2 strength to all icebreakers"))
    :effect (req (when (<= (count (filter program? (all-active-installed state :runner))) 3)
                   (pump-all-icebreakers state side 2 :end-of-run))
                 (make-run state side eid target nil card))}

   "Leave No Trace"
   (letfn [(get-rezzed-cids [ice]
             (map :cid (filter #(and (rezzed? %)
                                     (ice? %))
                               ice)))]
     {:async true
      :makes-run true
      :prompt "Choose a server"
      :msg "make a run and derez any ICE that are rezzed during this run"
      :choices (req runnable-servers)
      :effect (req (let [old-ice-cids (get-rezzed-cids (all-installed state :corp))]
                     (update! state side (assoc-in card [:special :leave-no-trace] old-ice-cids))
                     (make-run state side eid target nil (get-card state card))))
      :events [{:event :run-ends
                :effect (req (let [new (set (get-rezzed-cids (all-installed state :corp)))
                                   old (set (get-in (get-card state card) [:special :leave-no-trace]))
                                   diff-cid (seq (clojure.set/difference new old))
                                   diff (map #(find-cid % (all-installed state :corp)) diff-cid)]
                               (doseq [ice diff]
                                 (derez state :runner ice))
                               (when-not (empty? diff)
                                 (system-msg state side (str "uses Leave No Trace to derez " (join ", " (map :title diff)))))))}]})

   "Legwork"
   {:async true
    :makes-run true
    :req (req hq-runnable)
    :effect (effect (make-run eid :hq nil card))
    :events [{:event :successful-run
              :silent (req true)
              :effect (effect (access-bonus :hq 2))}]}

   "Leverage"
   {:optional
    {:req (req (some #{:hq} (:successful-run runner-reg)))
     :player :corp
     :prompt "Take 2 bad publicity?"
     :yes-ability {:player :corp
                   :msg "takes 2 bad publicity"
                   :effect (effect (gain-bad-publicity :corp 2))}
     :no-ability {:player :runner
                  :msg "is immune to damage until the beginning of the Runner's next turn"
                  :effect (effect
                            (register-events
                              card
                              [{:event :pre-damage
                                :duration :until-runner-turn-begins
                                :effect (effect (damage-prevent :net Integer/MAX_VALUE)
                                                (damage-prevent :meat Integer/MAX_VALUE)
                                                (damage-prevent :brain Integer/MAX_VALUE))}
                               {:event :runner-turn-begins
                                :duration :until-runner-turn-begins
                                :effect (effect (unregister-floating-events :until-runner-turn-begins))}]))}}}

   "Levy AR Lab Access"
   {:msg "shuffle their Grip and Heap into their Stack and draw 5 cards"
    :rfg-instead-of-trashing true
    :async true
    :effect (effect (shuffle-into-deck :hand :discard)
                    (draw eid 5 nil))}

   "Lucky Find"
   {:msg "gain 9 [Credits]"
    :effect (effect (gain-credits 9))}

   "Mad Dash"
   {:async true
    :makes-run true
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :effect (effect (make-run eid target nil card))
    :events [{:event :agenda-stolen
              :silent (req true)
              :effect (effect (update! (assoc card :steal true)))}
             {:event :run-ends
              :async true
              :effect (req (if (:steal card)
                             (do (system-msg state :runner
                                             (str "adds Mad Dash to their score area as an agenda worth 1 agenda point"))
                                 (as-agenda state :runner eid (get-card state card) 1))
                             (do (system-msg state :runner
                                             (str "suffers 1 meat damage from Mad Dash"))
                                 (damage state side eid :meat 1 {:card card}))))}]}

   "Making an Entrance"
   (letfn [(entrance-trash [cards]
             {:prompt "Choose a card to trash"
              :choices (concat cards ["None"])
              :async true
              :msg (req (when (not= target "None") (str "trash " (:title target))))
              :effect (req (if (= target "None")
                             (if (not-empty cards)
                               (continue-ability state side (reorder-choice :runner :corp cards '()
                                                                            (count cards) cards) card nil)
                               (do (clear-wait-prompt state :corp)
                                   (effect-completed state side eid)))
                             (do (trash state side target {:unpreventable true})
                                 (continue-ability state side (entrance-trash (remove-once #(= % target) cards))
                                                   card nil))))})]
     {:msg "look at and trash or rearrange the top 6 cards of their Stack"
      :async true
      :effect (req (show-wait-prompt state :corp "Runner to rearrange the top cards of their stack")
                   (let [from (take 6 (:deck runner))]
                     (continue-ability state side (entrance-trash from) card nil)))})

   "Marathon"
   {:async true
    :makes-run true
    :prompt "Choose a server"
    :choices (req (filter #(can-run-server? state %) remotes))
    :effect (effect (make-run eid target nil card))
    :events [{:event :run-ends
              :effect (req (prevent-run-on-server state card (:server run))
                           (when (:successful run)
                             (system-msg state :runner "gains 1 [Click] and adds Marathon to their grip")
                             (gain state :runner :click 1)
                             (move state :runner card :hand)
                             (unregister-events state side card)))}]}

   "Mars for Martians"
   (letfn [(count-clan [state] (count (filter #(and (has-subtype? % "Clan") (resource? %))
                                              (all-active-installed state :runner))))]
     {:msg (msg "draw " (count-clan state) " cards and gain " (count-tags state) " [Credits]")
      :async true
      :effect (req (wait-for (draw state side (count-clan state) nil)
                             (gain-credits state side (count-tags state))
                             (effect-completed state side eid)))})

   "Mass Install"
   (letfn [(mhelper [n]
             (when (< n 3)
               {:async true
                :req (req (some #(and (program? %)
                                      (can-pay? state side eid card nil
                                                [:credit (install-cost state side %)]))
                                (:hand runner)))
                :prompt "Select a program to install"
                :choices {:five (req (and (program? target)
                                          (in-hand? target)
                                          (can-pay? state side eid card nil
                                                    [:credit (install-cost state side target)])))}
                :effect (req (wait-for (runner-install state side target nil)
                                       (continue-ability state side (mhelper (inc n)) card nil)))}))]
     {:async true
      :req (req (some #(and (program? %)
                            (can-pay? state side eid card nil
                                      [:credit (install-cost state side %)]))
                      (:hand runner)))
      :effect (effect (continue-ability (mhelper 0) card nil))})

   "Mining Accident"
   {:req (req (some #{:hq :rd :archives} (:successful-run runner-reg)))
    :rfg-instead-of-trashing true
    :async true
    :msg "make the Corp pay 5 [Credits] or take 1 bad publicity"
    :effect (effect (show-wait-prompt :runner "Corp to choose to pay or take bad publicity")
                    (continue-ability
                      {:player :corp
                       :async true
                       :prompt "Pay 5 [Credits] or take 1 Bad Publicity?"
                       :choices (concat (when (can-pay? state :corp eid card "Mining Accident" :credit 5)
                                          ["Pay 5 [Credits]"])
                                        ["Take 1 Bad Publicity"])
                       :effect (req (clear-wait-prompt state :runner)
                                    (if (= target "Pay 5 [Credits]")
                                      (do (lose-credits state :corp 5)
                                          (system-msg state side "pays 5 [Credits] from Mining Accident")
                                          (effect-completed state side eid))
                                      (do (gain-bad-publicity state :corp 1)
                                          (system-msg state side "takes 1 bad publicity from Mining Accident")
                                          (effect-completed state side eid))))}
                      card nil))}

   "Möbius"
   {:req (req rd-runnable)
    :async true
    :effect (req (wait-for (make-run state side :rd nil card)
                           (let [card (get-card state card)]
                             (if (:run-again card)
                               (make-run state side eid :rd nil card)
                               (effect-completed state side eid)))))
    :events [{:event :successful-run
              :req (req (and (:run-again card)
                             (= target :rd)))
              :msg "gain 4 [Credits]"
              :effect (effect (gain-credits 4))}
             {:event :successful-run-ends
              :interactive (req true)
              :optional {:req (req (and (not (:run-again card))
                                        (= [:rd] (:server target))))
                         :prompt "Make another run on R&D?"
                         :yes-ability {:effect (effect (clear-wait-prompt :corp)
                                                       (update! (assoc card :run-again true)))}}}]}

   "Modded"
   {:prompt "Select a program or piece of hardware to install from your Grip"
    :choices {:five (req (and (or (hardware? target)
                                  (program? target))
                              (in-hand? target)
                              (can-pay? state side eid card nil
                                        [:credit (install-cost state side target {:cost-bonus -3})])))}
    :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target {:cost-bonus -3}))}

   "Net Celebrity"
   {:recurring 1
    :interactions {:pay-credits {:req (req run)
                                 :type :recurring}}}

   "Networking"
   {:async true
    :req (req (pos? (count-tags state)))
    :msg "remove 1 tag"
    :effect (effect (lose-tags 1)
                    (continue-ability
                      {:optional
                       {:prompt "Pay 1 [Credits] to add Networking to Grip?"
                        :yes-ability
                        {:cost [:credit 1]
                         :msg "add it to their Grip"
                         :effect (effect (move card :hand))}}}
                      card nil))}

   "Notoriety"
   {:req (req (and (some #{:hq} (:successful-run runner-reg))
                   (some #{:rd} (:successful-run runner-reg))
                   (some #{:archives} (:successful-run runner-reg))))
    :async true
    :effect (req (as-agenda state :runner eid (first (:play-area runner)) 1))
    :msg "add it to their score area as an agenda worth 1 agenda point"}

   "Office Supplies"
   {:play-cost-bonus (req (- (:link runner 0)))
    :prompt "Gain 4 [Credits] or draw 4 cards?"
    :choices ["Gain 4 [Credits]" "Draw 4 cards"]
    :async true
    :msg (msg (if (= target "Gain 4 [Credits]")
                "gain 4 [Credits]"
                "draw 4 cards"))
    :effect (req (if (= target "Gain 4 [Credits]")
                   (do (gain-credits state :runner 4)
                       (effect-completed state side eid))
                   (draw state :runner eid 4 nil)))}

   "On the Lam"
   {:req (req (some resource? (all-active-installed state :runner)))
    :prompt "Choose a resource to host On the Lam"
    :choices {:req #(and (resource? %)
                         (installed? %))}
    :effect (effect (host target (assoc card :installed true))
                    (card-init (find-latest state card) {:resolve-effect false})
                    (system-msg (str "hosts On the Lam on " (:title target))))
    :interactions {:prevent [{:type #{:net :brain :meat :tag}
                              :req (req true)}]}
    :abilities [{:label "Avoid 3 tags"
                 :msg "avoid up to 3 tags"
                 :cost [:trash]
                 :effect (effect (tag-prevent :runner 3))}
                {:label "Prevent up to 3 damage"
                 :msg "prevent up to 3 damage"
                 :cost [:trash]
                 :effect (effect (damage-prevent :net 3)
                                 (damage-prevent :meat 3)
                                 (damage-prevent :brain 3))}]}

   "Out of the Ashes"
   (let [ashes-run {:prompt "Choose a server"
                    :choices (req runnable-servers)
                    :async true
                    :effect (effect (make-run eid target nil card))}
         ashes-recur (fn ashes-recur [n]
                       {:optional
                        {:prompt "Remove Out of the Ashes from the game to make a run?"
                         :yes-ability
                         {:msg "removes Out of the Ashes from the game to make a run"
                          :effect
                          (req (let [card (some #(when (= "Out of the Ashes" (:title %)) %) (:discard runner))]
                                 (move state side card :rfg)
                                 (unregister-events state side card)
                                 (wait-for (resolve-ability state side ashes-run card nil)
                                           (if (< 1 n)
                                             (continue-ability state side (ashes-recur (dec n)) card nil)
                                             (effect-completed state side eid)))))}}})
         ashes-flag [{:event :runner-phase-12
                      :priority -1
                      :once :per-turn
                      :once-key :out-of-ashes
                      :effect (effect (continue-ability
                                        (ashes-recur (count (filter #(= "Out of the Ashes" (:title %))
                                                                    (:discard runner))))
                                        card nil))}]]
     {:async true
      :makes-run true
      :prompt "Choose a server"
      :choices (req runnable-servers)
      :effect (effect (make-run eid target nil card))
      :move-zone (req (if (in-discard? card)
                        (register-events state side (assoc card :zone [:discard]) ashes-flag)
                        (unregister-events state side card {:events [{:event :runner-phase-12}]})))})

   "Paper Tripping"
   {:async true
    :req (req (pos? (count-tags state)))
    :msg "remove all tags"
    :effect (effect (lose-tags eid :all))}

   "Peace in Our Time"
   {:req (req (not (:scored-agenda corp-reg)))
    :msg "gain 10 [Credits]. The Corp gains 5 [Credits]"
    :effect (effect (gain-credits :runner 10)
                    (gain-credits :corp 5)
                    (register-turn-flag! card :can-run nil))}

   "Planned Assault"
   {:async true
    :prompt "Choose a Run event"
    :choices (req (sort-by :title
                           (filter #(and (has-subtype? % "Run")
                                         (can-pay? state side eid card nil
                                                   [:credit (play-cost state side %)]))
                                   (:deck runner))))
    :msg (msg "play " (:title target))
    :effect (effect (trigger-event :searched-stack nil)
                    (shuffle! :deck)
                    (play-instant eid target {:no-additional-cost true}))}

   "Political Graffiti"
   (let [update-agenda-points (fn [state side target amount]
                                (set-prop state side (get-card state target) :agendapoints (+ amount (:agendapoints (get-card state target))))
                                (gain-agenda-point state side amount))]
     {:async true
      :makes-run true
      :req (req archives-runnable)
      :effect (effect (make-run
                        eid :archives
                        {:req (req (= target :archives))
                         :replace-access
                         {:mandatory true
                          :prompt "Select an agenda to host Political Graffiti"
                          :choices {:req #(in-corp-scored? state side %)}
                          :msg (msg "host Political Graffiti on " (:title target) " as a hosted condition counter")
                          :effect (req (host state :runner (get-card state target)
                                             ; keep host cid in :agenda-cid because `trash` will clear :host
                                             (assoc card :installed true :agenda-cid (:cid (get-card state target))))
                                       (update-agenda-points state :corp target -1))}}
                        card))
      :events [{:event :purge
                :effect (effect (trash card {:cause :purge}))}]
      :trash-effect {:effect (req (let [current-side (get-scoring-owner state {:cid (:agenda-cid card)})]
                                    (update-agenda-points state current-side
                                                          (find-cid (:agenda-cid card) (get-in @state [current-side :scored]))
                                                          1)))}})

   "Populist Rally"
   {:req (req (seq (filter #(has-subtype? % "Seedy") (all-active-installed state :runner))))
    :msg "give the Corp 1 fewer [Click] to spend on their next turn"
    :effect (effect (lose :corp :click-per-turn 1))
    :events [{:event :corp-turn-ends
              :duration :until-corp-turn-ends
              :effect (effect (gain :corp :click-per-turn 1))}]}

   "Power Nap"
   {:effect (effect (gain-credits (+ 2 (count (filter #(has-subtype? % "Double")
                                                      (:discard runner))))))
    :msg (msg "gain " (+ 2 (count (filter #(has-subtype? % "Double") (:discard runner)))) " [Credits]")}

   "Power to the People"
   {:events [{:event :pre-steal-cost
              :duration :end-of-turn
              :once :per-turn
              :msg "gain 7 [Credits]"
              :effect (effect (gain-credits 7))}]}

   "Prey"
   {:implementation "Ice trash is manual"
    :async true
    :makes-run true
    :prompt "Choose a server:"
    :choices (req runnable-servers)
    :effect (effect (toast "Click this card to trash the passed ice")
                    (make-run eid target nil card))
    :abilities [{:label "Trash previous ice"
                 :once :per-run
                 :req (req (let [last-ice (nth run-ices run-position)]
                             (and run
                                (rezzed? last-ice)
                                (<= (get-strength last-ice) (count (all-installed state :runner))))))
                 :async true
                 :effect (effect
                           (continue-ability
                             (let [last-ice (nth run-ices run-position)]
                               {:async true
                                :cost [:installed (get-strength last-ice)]
                                :msg (msg "trash " (card-str state last-ice))
                                :effect (effect (trash eid last-ice nil))})
                             card nil))}]}

   "Process Automation"
   {:msg "gain 2 [Credits] and draw 1 card"
    :async true
    :effect (effect (gain-credits 2)
                    (draw eid 1 nil))}

   "Push Your Luck"
   (letfn [(runner-choice [choices]
             {:prompt "Spend how many credits?"
              :choices choices
              :async true
              :effect (effect (show-wait-prompt :runner "Corp to guess even or odd")
                              (clear-wait-prompt :corp)
                              (continue-ability :corp (corp-choice (str->int target)) card nil))})
           (corp-choice [spent]
             {:prompt "Guess how many credits were spent"
              :choices ["Even" "Odd"]
              :async true
              :effect (req (let [correct-guess ((if (= target "Even") even? odd?) spent)]
                             (clear-wait-prompt state :runner) (lose state :runner :credit spent)
                             (system-msg state :runner (str "spends " spent " [Credit]"))
                             (system-msg state :corp (str (if correct-guess " " " in")
                                                          "correctly guesses " (lower-case target)))
                             (wait-for (trigger-event-simult state side :reveal-spent-credits nil nil spent)
                                       (when-not correct-guess
                                         (system-msg state :runner (str "gains " (* 2 spent) " [Credits]"))
                                         (gain-credits state :runner (* 2 spent)))
                                       (effect-completed state side eid))))})]
     {:async true
      :effect (req (show-wait-prompt state :corp "Runner to spend credits")
                (let [all-amounts (range (inc (get-in @state [:runner :credit])))
                      valid-amounts (remove #(or (any-flag-fn? state :corp :prevent-secretly-spend %)
                                                 (any-flag-fn? state :runner :prevent-secretly-spend %))
                                            all-amounts)
                      choices (map str valid-amounts)]
                  (continue-ability state side (runner-choice choices) card nil)))})

   "Pushing the Envelope"
   {:msg (msg (if (<= (count (:hand runner)) 2)
                "make a run, and adds +2 strength to installed icebreakers"
                "make a run"))
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :makes-run true
    :async true
    :effect (req (when (<= (count (:hand runner)) 2)
                   (pump-all-icebreakers state side 2 :end-of-run))
                 (make-run state side eid target))}

   "Quality Time"
   {:msg "draw 5 cards"
    :async true
    :effect (effect (draw eid 5 nil))}

   "Queen's Gambit"
   {:choices ["0" "1" "2" "3"]
    :prompt "How many advancement tokens?"
    :effect (req (let [c (str->int target)]
                   (resolve-ability
                     state side
                     {:choices {:req #(and (is-remote? (second (:zone %)))
                                           (= (last (:zone %)) :content)
                                           (not (:rezzed %)))}
                      :msg (msg "add " c " advancement tokens on a card and gain " (* 2 c) " [Credits]")
                      :effect (effect (gain-credits (* 2 c))
                                      (add-prop :corp target :advance-counter c {:placed true})
                                      (register-turn-flag! card :can-access
                                                           ;; prevent access of advanced card
                                                           (fn [_ _ card] (not (same-card? target card)))))}
                     card nil)))}

   "Quest Completed"
   {:req (req (and (some #{:hq} (:successful-run runner-reg))
                   (some #{:rd} (:successful-run runner-reg))
                   (some #{:archives} (:successful-run runner-reg))))
    :choices {:req installed?} :msg (msg "access " (:title target))
    :effect (effect (access-card target))}

   "Rebirth"
   {:msg "change identities"
    :prompt "Choose an identity to become"
    :rfg-instead-of-trashing true
    :choices (req (let [is-draft-id? #(.startsWith (:code %) "00")
                        runner-identity (:identity runner)
                        is-swappable #(and (= "Identity" (:type %))
                                           (= (:faction runner-identity) (:faction %))
                                           (not (is-draft-id? %))
                                           (not= (:title runner-identity) (:title %)))
                        swappable-ids (filter is-swappable (server-cards))]
                    (cancellable swappable-ids :sorted)))
    :effect (req (let [old-runner-identity (:identity runner)]
                   ;; Handle hosted cards (Ayla) - Part 1
                   (doseq [c (:hosted old-runner-identity)]
                     (move state side c :temp-hosted))
                   (disable-identity state side)
                   ;; Manually reduce the runner's link by old link
                   (lose state :runner :link (:baselink old-runner-identity))
                   ;; Move the selected ID to [:runner :identity] and set the zone
                   (let [new-id (-> target :title server-card make-card (assoc :zone [:identity]))
                         num-old-blanks (:num-disabled old-runner-identity)]
                     (swap! state assoc-in [side :identity] new-id)
                     ;; enable-identity does not do everything that init-identity does
                     (init-identity state side new-id)
                     (when num-old-blanks
                       (dotimes [_ num-old-blanks]
                         (disable-identity state side)))))
                 ;; Handle hosted cards (Ayla) - Part 2
                 (doseq [c (get-in @state [:runner :temp-hosted])]
                   ;; Currently assumes all hosted cards are hosted facedown (Ayla)
                   (host state side (get-in @state [:runner :identity]) c {:facedown true})))}

   "Reboot"
   (letfn [(install-cards [state side eid card to-install titles]
             (if-let [f (first to-install)]
               (wait-for (runner-install state :runner f {:facedown true :no-msg true})
                         (install-cards state side eid card (rest to-install) titles))
               (do
                 (move state side (find-latest state card) :rfg)
                 (system-msg state :runner (str "uses Reboot to install " (join ", " titles) " facedown"))
                 (effect-completed state side eid))))]
     {:async true
      :makes-run true
      :rfg-instead-of-trashing true
      :req (req archives-runnable)
      :effect (effect
                (make-run
                  eid :archives
                  {:req (req (= target :archives))
                   :replace-access
                   {:mandatory true
                    :async true
                    :prompt "Choose up to five cards to install"
                    :show-discard true
                    :choices {:max 5
                              :req #(and (in-discard? %)
                                         (runner? %))}
                    :effect (effect (install-cards eid card targets (map :title targets)))}}
                  card))})

   "Recon"
   {:implementation "Jack out is manual"
    :async true
    :makes-run true
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :effect (effect (make-run eid target nil card))}

   "Rejig"
   (let [valid-target? (fn [card] (and (runner? card)
                                       (or (program? card)
                                           (hardware? card))))
         pick-up {:async true
                  :prompt "Select a program or piece of hardware to add to your Grip"
                  :choices {:req #(and (valid-target? %)
                                       (installed? %))}
                  :effect (req (move state side target :hand)
                               (effect-completed state side (make-result eid (:cost target))))}
         put-down (fn [bonus]
                    {:async true
                     :prompt "Select a program or piece of hardware to install"
                     :choices
                     {:five (req (and (valid-target? target)
                                      (can-pay? state side eid card nil
                                                [:credit (install-cost state side target
                                                                       {:cost-bonus (- bonus)})])))}
                     :effect (effect (runner-install (assoc eid :source card :source-type :runner-install)
                                                     target {:cost-bonus (- bonus)}))})]
     {:req (req (some valid-target? (all-installed state :runner)))
      :effect (req (wait-for (resolve-ability state side pick-up card nil)
                             (continue-ability state side
                                               (put-down async-result)
                                               card nil)))})

   "Reshape"
   {:prompt "Select two non-rezzed ICE to swap positions"
    :choices {:req #(and (installed? %) (not (rezzed? %)) (ice? %)) :max 2}
    :msg (msg "swap the positions of " (card-str state (first targets)) " and " (card-str state (second targets)))
    :effect (req (when (= (count targets) 2)
                   (swap-ice state side (first targets) (second targets))))}

   "Retrieval Run"
   {:async true
    :makes-run true
    :req (req archives-runnable)
    :effect (effect (make-run
                      eid :archives
                      {:req (req (= target :archives))
                       :replace-access
                       {:async true
                        :prompt "Choose a program to install"
                        :msg (msg "install " (:title target))
                        :choices (req (filter program? (:discard runner)))
                        :effect (effect (runner-install (assoc eid :source card :source-type :runner-install)
                                                        target {:ignore-all-cost true}))}}
                      card))}

   "Rigged Results"
   (letfn [(runner-choice [choices]
             {:prompt "Spend how many credits?"
              :choices choices
              :async true
              :effect (effect (show-wait-prompt :runner "Corp to guess")
                              (clear-wait-prompt :corp)
                              (continue-ability :corp (corp-choice choices (str->int target)) card nil))})
           (corp-choice [choices spent]
             {:prompt "Guess how many credits were spent"
              :choices choices
              :async true
              :effect (req (clear-wait-prompt state :runner)
                           (lose state :runner :credit spent)
                           (system-msg state :runner (str "spends " spent " [Credit]"))
                           (system-msg state :corp (str " guesses " target " [Credit]"))
                           (wait-for (trigger-event-simult state side :reveal-spent-credits nil nil spent)
                                     (if (not= spent (str->int target))
                                       (continue-ability state :runner (choose-ice) card nil)
                                       (effect-completed state side eid))))})
           (choose-ice []
             {:async true
              :prompt "Select a piece of ICE to bypass"
              :choices {:req ice?}
              :msg (msg "make a run and bypass " (card-str state target))
              :effect (effect (make-run eid (second (:zone target)) nil card))})]
     {:async true
      :effect (req (show-wait-prompt state :corp "Runner to spend credits")
                (let [all-amounts (range (min 3 (inc (get-in @state [:runner :credit]))))
                      valid-amounts (remove #(or (any-flag-fn? state :corp :prevent-secretly-spend %)
                                                 (any-flag-fn? state :runner :prevent-secretly-spend %))
                                            all-amounts)
                      choices (map str valid-amounts)]
                  (continue-ability state side (runner-choice choices) card nil)))})

   "Rip Deal"
   {:async true
    :makes-run true
    :rfg-instead-of-trashing true
    :req (req hq-runnable)
    :effect
    (effect (make-run
              eid :hq
              {:req (req (= target :hq))
               :replace-access
               {:async true
                :effect
                (effect
                  (continue-ability
                    (let [n (min (-> corp :hand count) (access-count state side :hq-access))
                          heap (count (:discard runner))]
                      (if (pos? heap)
                        {:show-discard true
                         :prompt (str "Choose " (quantify (min n heap) "card") " to move from the Heap to your Grip")
                         :async true
                         :msg (msg "take " (join ", " (map :title targets)) " from their Heap to their Grip")
                         :choices {:max (min n heap)
                                   :all true
                                   :req #(and (runner? %)
                                              (in-discard? %))}
                         :effect (req (doseq [c targets]
                                        (move state side c :hand))
                                      (do-access state side eid (:server run) {:hq-root-only true}))}
                        {:async true
                         :msg (msg "take no cards from their Heap to their Grip")
                         :effect (req (do-access state side eid (:server run) {:hq-root-only true}))}))
                    card nil))}}
              card nil))}

   "Rumor Mill"
   (letfn [(eligible? [card] (and (:uniqueness card)
                                  (or (asset? card)
                                      (upgrade? card))
                                  (not (has-subtype? card "Region"))))
           (rumor [state] (filter eligible? (concat (all-installed state :corp)
                                                    (get-in @state [:corp :hand])
                                                    (get-in @state [:corp :deck])
                                                    (get-in @state [:corp :discard]))))]
     {:leave-play (req (doseq [c (rumor state)]
                         (enable-card state :corp c)))
      :effect (req (doseq [c (rumor state)]
                     (disable-card state :corp c)))
      :events [{:event :corp-install
                :req (req (eligible? target))
                :effect (effect (disable-card :corp target))}]})

   "Run Amok"
   (letfn [(get-rezzed-cids [ice]
             (map :cid (filter #(and (rezzed? %)
                                     (ice? %))
                               ice)))]
     {:async true
      :makes-run true
      :prompt "Choose a server"
      :choices (req runnable-servers)
      :effect (effect (update! (assoc-in card [:special :run-amok] (get-rezzed-cids (all-installed state :corp))))
                      (make-run eid target nil (get-card state card)))
      :events [{:event :run-ends
                :async true
                :effect (req (let [new (set (get-rezzed-cids (all-installed state :corp)))
                                   old (set (get-in (get-card state card) [:special :run-amok]))
                                   diff-cid (seq (clojure.set/difference new old))
                                   diff (map #(find-cid % (all-installed state :corp)) diff-cid)]
                               (continue-ability
                                 state side
                                 (when (seq diff)
                                   {:async true
                                    :prompt "Select an ice to trash"
                                    :choices {:req #(some (partial same-card? %) diff)
                                              :all true}
                                    :effect (effect (trash eid target nil))})
                                 card nil)))}]})

   "Running Interference"
   {:async true
    :makes-run true
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :effect (effect (register-floating-effect
                      card
                      {:type :rez-additional-cost
                       :duration :end-of-run
                       :req (req (ice? target))
                       :value (req [:credit (:cost target)])})
                    (make-run eid target nil card))}

   "Satellite Uplink"
   {:async true
    :req (req (some #(not (rezzed? %)) (all-installed state :corp)))
    :choices {:max 2
              :req #(and (corp? %)
                         (installed? %)
                         (not (rezzed? %)))}
    :effect (req (if (pos? (count targets))
                   (wait-for (expose state side target)
                             (if (= 2 (count targets))
                               (expose state side eid (second targets))
                               (effect-completed state side eid)))
                   (effect-completed state side eid)))}

   "Scavenge"
   {:async true
    :req (req (some #(and (program? %)
                          (installed? %))
                    (all-active-installed state :runner)))
    :prompt "Select an installed program to trash"
    :choices {:req #(and (program? %)
                         (installed? %))}
    :effect (req (let [trashed target
                       tcost (:cost trashed)]
                   (wait-for
                     (trash state side target {:unpreventable true})
                     (continue-ability
                       state side
                       {:async true
                        :prompt "Select a program to install from your Grip or Heap"
                        :show-discard true
                        :choices
                        {:five (req (and (program? target)
                                         (or (in-hand? target)
                                             (in-discard? target))
                                         (can-pay? state side eid card nil
                                                   [:credit (install-cost state side target
                                                                          {:cost-bonus (- tcost)})])))}
                        :msg (msg "trash " (:title trashed)
                                  " and install " (:title target)
                                  ", lowering the cost by " tcost " [Credits]")
                        :effect (effect (runner-install (assoc eid :source card :source-type :runner-install)
                                                        target {:cost-bonus (- tcost)}))}
                       card nil))))}

   "Scrubbed"
   {:implementation "Encounter effect is manual"
    :abilities [{:label "Lower ice strength"
                 :effect (effect (update! (assoc-in card [:special :scrubbed-target] current-ice))
                                 (update-all-ice))}]
    :constant-effects [{:type :ice-strength
                        :req (req (same-card? target (get-in card [:special :scrubbed-target])))
                        :value -2}]
    :events [{:event :run-ends
              :effect (effect (update! (dissoc-in card [:special :scrubbed-target])))}]}

   "Showing Off"
   {:async true
    :makes-run true
    :req (req rd-runnable)
    :effect (effect (make-run
                      eid :rd
                      {:replace-access
                       {:msg "access cards from the bottom of R&D"
                        :mandatory true
                        :async true
                        :effect (effect (do-access eid (:server run)))}}
                      card))
    :events [{:event :pre-access
              :silent (req true)
              :effect (req (swap! state assoc-in [:runner :rd-access-fn] reverse))}
             {:event :run-ends
              :effect (req (swap! state assoc-in [:runner :rd-access-fn] seq))}]}

   "Singularity"
   {:async true
    :makes-run true
    :prompt "Choose a server"
    :choices (req (filter #(can-run-server? state %) remotes))
    :effect (effect (make-run
                      eid target
                      {:req (req (is-remote? target))
                       :replace-access
                       {:mandatory true
                        :async true
                        :msg "trash all cards in the server at no cost"
                        :effect (effect (trash-cards eid (:content run-server)))}}
                      card))}

   "Social Engineering"
   {:prompt "Select an unrezzed piece of ICE"
    :choices {:req #(and (not (rezzed? %))
                         (ice? %))}
    :effect (effect
              (continue-ability
                (let [ice target]
                  {:msg (msg "select " (card-str state ice))
                   :effect (effect (register-events
                                     card
                                     [{:event :rez
                                       :duration :end-of-turn
                                       :req (req (same-card? target ice))
                                       :msg (msg "gain " (rez-cost state side (get-card state target)) " [Credits]")
                                       :effect (effect (gain-credits :runner (rez-cost state side (get-card state target))))}]))})
                card nil))}

   "Spear Phishing"
   {:implementation "Bypass is manual"
    :async true
    :makes-run true
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :effect (effect (make-run eid target nil card))}

   "Spec Work"
   {:async true
    :additional-cost [:program 1]
    :msg "gain 4 [Credits] and draw 2 cards"
    :effect (effect (gain-credits 4)
                    (draw eid 2 nil))}

   "Special Order"
   {:prompt "Choose an Icebreaker"
    :choices (req (cancellable (filter #(has-subtype? % "Icebreaker") (:deck runner)) :sorted))
    :msg (msg "add " (:title target) " to their grip and shuffle their stack")
    :effect (effect (trigger-event :searched-stack nil)
                    (shuffle! :deck)
                    (move target :hand))}

   "Spooned"
   (cutlery "Code Gate")

   "Spot the Prey"
   {:prompt "Select 1 non-ICE card to expose"
    :msg "expose 1 card and make a run"
    :choices {:req #(and (installed? %)
                         (not (ice? %))
                         (corp? %))}
    :async true
    :makes-run true
    :effect (req (wait-for (expose state side target)
                           (continue-ability
                             state side
                             {:prompt "Choose a server"
                              :choices (req runnable-servers)
                              :async true
                              :effect (effect (make-run eid target))}
                             card nil)))}

   "Stimhack"
   {:async true
    :makes-run true
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :effect (effect (gain-next-run-credits 9)
                    (make-run eid target nil card))
    :events [{:event :run-ends
              :msg "take 1 brain damage"
              :effect (effect (damage eid :brain 1 {:unpreventable true
                                                    :card card}))}]}

   "Sure Gamble"
   {:msg "gain 9 [Credits]"
    :effect (effect (gain-credits 9))}

   "Surge"
   {:msg (msg "place 2 virus tokens on " (:title target))
    :choices {:req #(and (has-subtype? % "Virus") (:added-virus-counter %))}
    :effect (req (add-counter state :runner target :virus 2))}

   "SYN Attack"
   {:async true
    :effect (effect (show-wait-prompt "Corp to choose an option for SYN Attack")
                    (continue-ability
                      {:player :corp
                       :prompt "Discard 2 cards or draw 4 cards?"
                       :choices (concat (when (<= 2 (count (:hand corp)))
                                          ["Discard 2"])
                                        ["Draw 4"])
                       :effect (req (if (= target "Draw 4")
                                      (do (draw state :corp 4)
                                          (clear-wait-prompt state :runner)
                                          (system-msg state :corp "draws 4 cards from SYN Attack"))
                                      (continue-ability
                                        state :corp
                                        {:prompt "Choose 2 cards to discard"
                                         :choices {:max 2
                                                   :req #(and (in-hand? %) (corp? %))}
                                         :effect (effect (trash-cards :corp targets)
                                                         (clear-wait-prompt :runner)
                                                         (system-msg :corp "discards 2 cards from SYN Attack"))}
                                        card nil)))}
                      card nil))}

   "System Outage"
   {:events [{:event :corp-draw
              :req (req (not (first-event? state side :corp-draw)))
              :msg "force the Corp to lose 1 [Credits]"
              :effect (effect (lose-credits :corp 1))}]}

   "System Seizure"
   {:events [{:event :pump-breaker
              :once :per-turn
              :effect (req (let [last-pump (assoc (last (:effects @state)) :duration :end-of-run)]
                             (swap! state assoc :effects
                                    (->> (:effects @state)
                                         butlast
                                         (into [])
                                         (#(conj % last-pump)))))
                           (update-breaker-strength state side target))}]}

   "Test Run"
   {:prompt "Install a program from your Stack or Heap?"
    :choices ["Stack" "Heap"]
    :msg (msg "install a program from their " target)
    :effect (effect
              (continue-ability
                (let [where target]
                  {:prompt "Choose a program to install"
                   :choices (req (cancellable
                                   (filter program? ((if (= where "Heap") :discard :deck) runner))))
                   :effect (effect (trigger-event :searched-stack nil)
                                   (shuffle! :deck)
                                   (register-events
                                     card
                                     (let [program target]
                                       [{:event :runner-turn-ends
                                         :duration :end-of-turn
                                         :req (req (let [program (find-cid (:cid program) (all-installed state :runner))]
                                                     (get-in program [:special :test-run])))
                                         :msg (msg "move " (:title program) " to the top of the stack")
                                         :effect (effect (move (find-cid (:cid program) (all-installed state :runner))
                                                               :deck {:front true}))}]))
                                   (runner-install (assoc eid :source card :source-type :runner-install)
                                                   (assoc-in target [:special :test-run] true)
                                                   {:ignore-all-cost true}))})
                card nil))}

   "The Maker's Eye"
   {:req (req rd-runnable)
    :makes-run true
    :async true
    :effect (effect (make-run eid :rd nil card))
    :events [{:event :successful-run
              :silent (req true)
              :req (req (= target :rd))
              :effect (effect (access-bonus :rd 2))}]}

   "The Noble Path"
   {:async true
    :makes-run true
    :effect (req (wait-for (trash-cards state side eid (:hand runner) nil)
                    (continue-ability
                      state side
                      {:async true
                       :prompt "Choose a server"
                       :choices (req runnable-servers)
                       :msg (msg "trash their Grip and make a run on " target ", preventing all damage")
                       :effect (effect (make-run eid target nil card))}
                      card nil)))
    :events [{:event :pre-damage
              :duration :end-of-run
              :effect (effect (damage-prevent :net Integer/MAX_VALUE)
                              (damage-prevent :meat Integer/MAX_VALUE)
                              (damage-prevent :brain Integer/MAX_VALUE))}]}

   "The Price of Freedom"
   {:additional-cost [:connection 1]
    :msg "prevent the Corp from advancing cards during their next turn"
    :rfg-instead-of-trashing true
    :events [{:event :corp-turn-begins
              :duration :until-start-of-runner-turn
              :effect (effect (register-turn-flag!
                                card :can-advance
                                (fn [state side card]
                                  ((constantly false)
                                   (toast state :corp "Cannot advance cards this turn due to The Price of Freedom." "warning"))))
                              ;; This is a hack
                              (unregister-floating-events :until-start-of-runner-turn))}]}

   "Three Steps Ahead"
   {:events [{:event :runner-turn-ends
              :duration :end-of-turn
              :msg (msg "gain " (* 2 (count (:successful-run runner-reg))) " [Credits]")
              :effect (effect (gain-credits (* 2 (count (:successful-run runner-reg)))))}]}

   "Tinkering"
   {:req (req (some #(and (ice? %)
                          (installed? %))
                    (all-installed state :corp)))
    :prompt "Select a piece of ICE"
    :choices {:req #(and (installed? %)
                         (ice? %))}
    :msg (msg "make " (card-str state target) " gain Sentry, Code Gate, and Barrier until the end of the turn")
    :effect (effect (update! (assoc target :subtype (combine-subtypes true (:subtype target) "Sentry" "Code Gate" "Barrier")))
                    (update! (assoc-in (get-card state target) [:special :tinkering] true))
                    (update-all-ice)
                    (add-icon card (get-card state target) "T" "green")
                    (register-events
                      card
                      (let [ice (get-card state target)
                            stypes (:subtype target)]
                        [{:event :runner-turn-ends
                          :duration :end-of-turn
                          :req (req (get-in (get-card state ice) [:special :tinkering]))
                          :effect (effect (remove-icon card (get-card state ice))
                                          (update! (assoc (get-card state ice) :subtype stypes))
                                          (update! (dissoc-in (get-card state ice) [:special :tinkering]))
                                          (update-all-ice))}])))}

   "Trade-In"
   ;; Basically a hack. Ideally the additional cost cause the cost trash to be passed in as targets
   (letfn [(trashed-hw [state] (last (get-in @state [:runner :discard])))]
     {:additional-cost [:hardware 1]
      :msg (msg (let [{:keys [title cost]} (trashed-hw state)]
                  (str "trash " title " and gain " (quot cost 2) " [Credits]")))
      :effect (req (let [{:keys [cost]} (trashed-hw state)]
                     (gain-credits state :runner (quot cost 2))
                     (continue-ability state :runner
                                       {:prompt "Choose a Hardware to add to your Grip from your Stack"
                                        :choices (req (filter hardware?
                                                              (:deck runner)))
                                        :msg (msg "add " (:title target) " to their Grip (and shuffle their Stack)")
                                        :effect (effect (trigger-event :searched-stack nil)
                                                        (shuffle! :deck)
                                                        (move target :hand))}
                                       card nil)))})

   "Traffic Jam"
   {:effect (effect (update-all-advancement-costs))
    :leave-play (effect (update-all-advancement-costs))
    :events [{:event :pre-advancement-cost
              :effect (req (advancement-cost-bonus
                             state side (count (filter #(= (:title %) (:title target)) (:scored corp)))))}]}

   "Uninstall"
   {:req (req (some #(or (hardware? %)
                         (program? %))
                    (all-active-installed state :runner)))
    :choices {:req #(and (installed? %)
                         (not (facedown? %))
                         (or (hardware? %)
                             (program? %)))}
    :msg (msg "move " (:title target) " to their Grip")
    :effect (effect (move target :hand))}

   "Unscheduled Maintenance"
   {:events [{:event :corp-install
              :req (req (ice? target))
              :effect (effect (register-turn-flag!
                                card :can-install-ice
                                (fn [state side card]
                                  (if (ice? card)
                                    ((constantly false)
                                     (toast state :corp "Cannot install ICE the rest of this turn due to Unscheduled Maintenance"))
                                    true))))}]
    :leave-play (effect (clear-turn-flag! card :can-install-ice))}

   "Vamp"
   {:async true
    :makes-run true
    :req (req hq-runnable)
    :effect (effect (make-run
                      eid :hq
                      {:req (req (= target :hq))
                       :replace-access
                       {:async true
                        :prompt "How many [Credits]?"
                        :choices :credit
                        :msg (msg "take 1 tag and make the Corp lose " target " [Credits]")
                        :effect (effect (lose-credits :corp target)
                                        (gain-tags eid 1))}}
                      card))}

   "Wanton Destruction"
   {:async true
    :makes-run true
    :req (req hq-runnable)
    :effect (effect (make-run
                      eid :hq
                      {:req (req (= target :hq))
                       :replace-access
                       {:msg (msg "force the Corp to discard " target " cards from HQ at random")
                        :prompt "How many [Click] do you want to spend?"
                        :choices (req (map str (range 1 (inc (:click runner)))))
                        :effect (req (let [n (str->int target)]
                                       (wait-for (pay-sync state :runner card :click n)
                                                 (trash-cards state :corp eid (take n (shuffle (:hand corp)))))))}}
                      card))}

   "Watch the World Burn"
   (letfn [(rfg-card-event [burned-card]
             [{:event :pre-access-card
               :duration :end-of-game
               :req (req (same-card? :title burned-card target))
               :msg (msg (str "remove " (:title burned-card) " from the game"))
               :effect (effect (move :corp target :rfg))}])]
     {:async true
      :makes-run true
      :prompt "Choose a server"
      :choices (req (filter #(can-run-server? state %) remotes))
      :effect (effect (make-run eid target nil card))
      :events [{:event :pre-access-card
                :req (req (and (not= (:type target) "Agenda")
                               (:successful run)))
                :once :per-run
                :msg (msg "remove " (:title target) " from the game, and watch for other copies of " (:title target) " to burn")
                :effect (effect (move :corp target :rfg)
                                (register-events card (rfg-card-event target)))}]})

   "White Hat"
   (letfn [(finish-choice [choices]
             (let [choices (filter #(not= "None" %) choices)]
               (when (not-empty choices)
                 {:effect (req (doseq [c choices]
                                 (move state :corp c :deck))
                               (shuffle! state :corp :deck))
                  :msg (str "shuffle " (join ", " (map :title choices)) " into R&D")})))
           (choose-cards [hand chosen]
             {:prompt "Choose a card in HQ to shuffle into R&D"
              :player :runner
              :choices (conj (vec (clojure.set/difference hand chosen))
                             "None")
              :async true
              :effect (req (if (and (empty? chosen)
                                    (not= "None" target))
                             (continue-ability state side (choose-cards hand (conj chosen target)) card nil)
                             (continue-ability state side (finish-choice (conj chosen target)) card nil)))})]
     {:req (req (some #{:hq :rd :archives} (:successful-run runner-reg)))
      :trace {:base 3
              :unsuccessful
              {:async true
               :msg "reveal all cards in HQ"
               :effect (effect (reveal (:hand corp))
                               (continue-ability :runner (choose-cards (set (:hand corp)) #{}) card nil))}}})

   "Windfall"
   {:effect (effect (shuffle! :deck)
                    (resolve-ability
                      {:effect (req (let [topcard (first (:deck runner))
                                          cost (:cost topcard)]
                                      (trash state side topcard)
                                      (when-not (event? topcard)
                                        (gain-credits state side cost))
                                      (system-msg state side
                                                  (str "shuffles their Stack and trashes " (:title topcard)
                                                       (when-not (event? topcard)
                                                         (str " to gain " cost " [Credits]"))))))}
                      card nil))}})
