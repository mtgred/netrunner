(ns game.cards.events
  (:require [game.core :refer :all]
            [game.utils :refer :all]
            [jinteki.utils :refer :all]
            [clojure.string :as string]))

(defn- cutlery
  [subtype]
  {:makes-run true
   :on-play {:async true
             :prompt "Choose a server:"
             :choices (req runnable-servers)
             :effect (effect (make-run eid target card))}
   :events [{:event :subroutines-broken
             :async true
             :req (req (and (has-subtype? target subtype)
                            (every? :broken (:subroutines target))
                            (let [pred #(and (has-subtype? (first %) subtype)
                                             (every? :broken (:subroutines (first %))))]
                              (first-event? state side :subroutines-broken pred))))
             :msg (msg "trash " (card-str state target))
             :effect (effect (trash eid target nil))}]})

;; Card definitions

(defcard "Account Siphon"
  {:makes-run true
   :on-play {:req (req hq-runnable)
             :async true
             :effect (effect (make-run eid :hq card))}
   :events [(successful-run-replace-access
              {:target-server :hq
               :this-card-run true
               :ability
               {:async true
                :msg (msg "force the Corp to lose " (min 5 (:credit corp))
                          " [Credits], gain " (* 2 (min 5 (:credit corp)))
                          " [Credits] and take 2 tags")
                :effect (req (wait-for (gain-tags state :runner 2)
                                       (wait-for (gain-credits state :runner (* 2 (min 5 (:credit corp))))
                                                 (lose-credits state :corp eid (min 5 (:credit corp))))))}})]})

(defcard "Always Have a Backup Plan"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :choices (req runnable-servers)
             :async true
             :msg (msg "make a run on " target)
             :effect (req (wait-for (make-run state side target card)
                                    (let [card (get-card state card)]
                                      (if (get-in card [:special :run-again])
                                        (make-run state side eid target card {:ignore-costs true})
                                        (effect-completed state side eid)))))}
   :events [{:event :run-ends
             :optional {:req (req (and (not (get-in card [:special :run-again]))
                                       (:unsuccessful target)))
                        :player :runner
                        :prompt "Make another run on the same server?"
                        :yes-ability
                        {:effect (req (let [last-run (get-in @state [:runner :register :last-run])
                                            run-ice (get-in @state (concat [:corp :servers] (:server last-run) [:ices]))
                                            pos (:position last-run)
                                            ice (when (and pos
                                                           (pos? pos)
                                                           (<= pos (count run-ice)))
                                                  (get-card state (nth run-ice (dec pos))))]
                                        (update! state side (update card :special
                                                                    assoc
                                                                    :run-again true
                                                                    :run-again-ice ice))))}}}
            {:event :encounter-ice
             :once :per-run
             :req (req (and (get-in card [:special :run-again])
                            (same-card? (:ice context) (get-in card [:special :run-again-ice]))))
             :msg (msg "bypass " (:title (:ice context)))
             :effect (req (bypass-ice state))}]})

(defcard "Amped Up"
  {:on-play
   {:msg "gain [Click][Click][Click] and suffer 1 brain damage"
    :async true
    :effect (effect (gain :click 3)
                    (damage eid :brain 1 {:unpreventable true :card card}))}})

(defcard "Another Day, Another Paycheck"
  {:events [{:event :agenda-stolen
             :trace {:base 0
                     :unsuccessful
                     {:async true
                      :effect (effect (gain-credits :runner eid (+ (:agenda-point runner) (:agenda-point corp))))
                      :msg (msg (str "gain " (+ (:agenda-point runner) (:agenda-point corp)) " [Credits]"))}}}]})

(defcard "Apocalypse"
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
                                        (doseq [oc hostedcards
                                                :let [c (get-card state oc)]
                                                :when (not (:condition c))]
                                          (flip-facedown state side c))
                                        (doseq [oc nonhostedcards
                                                :let [c (get-card state oc)]]
                                          (flip-facedown state side c))))}]
    {:on-play {:req (req (and (some #{:hq} (:successful-run runner-reg))
                              (some #{:rd} (:successful-run runner-reg))
                              (some #{:archives} (:successful-run runner-reg))))
               :async true
               ;; trash cards from right to left
               ;; otherwise, auto-killing servers would move the cards to the next server
               ;; so they could no longer be trashed in the same loop
               :msg "trash all installed Corp cards and turn all installed Runner cards facedown"
               :effect (req (wait-for
                              (resolve-ability state side corp-trash card nil)
                              (continue-ability state side runner-facedown card nil)))}}))

(defcard "Because I Can"
  {:makes-run true
   :on-play {:async true
             :prompt "Choose a server"
             :choices (req (filter #(can-run-server? state %) remotes))
             :effect (effect (make-run eid target card))}
   :events [(successful-run-replace-access
              {:target-server :remote
               :this-card-run true
               :ability
               {:msg "shuffle all cards in the server into R&D"
                :effect (req (doseq [c (:content run-server)]
                               (move state :corp c :deck))
                             (shuffle! state :corp :deck))}})]})

(defcard "Black Hat"
  {:on-play
   {:trace
    {:base 4
     :unsuccessful
     {:effect (effect (register-events
                        card [{:event :pre-access
                               :duration :end-of-turn
                               :req (req (#{:hq :rd} target))
                               :effect (effect (access-bonus target 2))}]))}}}})

(defcard "Blackmail"
  {:makes-run true
   :on-play {:req (req (has-bad-pub? state))
             :prompt "Choose a server"
             :choices (req runnable-servers)
             :msg "prevent ice from being rezzed during this run"
             :async true
             :effect (effect (register-run-flag!
                               card
                               :can-rez
                               (fn [state side card]
                                 (if (ice? card)
                                   ((constantly false)
                                    (toast state :corp "Cannot rez ice on this run due to Blackmail"))
                                   true)))
                             (make-run eid target card))}})

(defcard "Blueberry!™ Diesel"
  {:on-play {:async true
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
                          (draw state :runner eid 2 nil))}})

(defcard "Bravado"
  ; Bravado only counts distinct pieces of ice that were passed.
  ; That means if a piece of ice was reinstalled and then repassed, it needs to be counted twice.
  ; This is handled by tracking :card-moved and counting them in :special :bravado-moved.
  (letfn [(iced-servers [state side eid card]
            (filter #(-> (get-in @state (cons :corp (server->zone state %))) :ices count pos?)
                    (zones->sorted-names (get-runnable-zones state side eid card nil))))]
    {:makes-run true
     :on-play {:async true
               :req (req (pos? (count (iced-servers state side eid card))))
               :prompt "Choose an iced server"
               :choices (req (iced-servers state side eid card))
               :effect (effect (register-events
                                 card
                                 [{:event :pass-ice
                                   :duration :end-of-run
                                   :effect (effect (update! (update-in (get-card state card) [:special :bravado-passed] conj (:cid (:ice context)))))}])
                         (make-run eid target (get-card state card)))}
     :events [{:event :run-ends
               :silent (req true)
               :msg (msg "gain "
                      (+ 6 (count (distinct (get-in card [:special :bravado-passed])))
                         (get-in card [:special :bravado-moved] 0))
                      " [Credits]")
               :async true
               :effect (effect (gain-credits :runner eid (+ 6 (count (distinct (get-in card [:special :bravado-passed])))
                                                            (get-in card [:special :bravado-moved] 0))))}
              {:event :card-moved
               :silent (req true)
               :req (req (in-coll? (get-in card [:special :bravado-passed] []) (:cid target)))
               :effect (effect (update! (update-in card [:special :bravado-moved] (fnil inc 0)))
                         (update! (update-in (get-card state card) [:special :bravado-passed]
                                             (fn [cids] (remove #(= % (:cid target)) cids)))))}]}))

(defcard "Bribery"
  {:makes-run true
   :on-play
   {:async true
    :prompt "How many credits do you want to pay?"
    :choices :credit
    :msg (msg "increase the rez cost of the first unrezzed piece of ice approached by " target " [Credits]")
    :effect (effect
              (continue-ability
                (let [bribery-x target]
                  {:prompt "Choose a server"
                   :choices (req runnable-servers)
                   :async true
                   :effect (effect
                             (register-events
                               card
                               [{:event :approach-ice
                                 :duration :end-of-run
                                 :unregister-once-resolved true
                                 :req (req (and (not (rezzed? (:ice context)))
                                                (first-run-event? state side :approach-ice
                                                                  (fn [targets]
                                                                    (let [context (first targets)]
                                                                      (not (rezzed? (:ice context))))))))
                                 :effect (effect
                                           (register-floating-effect
                                             card
                                             (let [approached-ice (:ice context)]
                                               {:type :rez-additional-cost
                                                :duration :end-of-run
                                                :unregister-once-resolved true
                                                :req (req (same-card? approached-ice target))
                                                :value [:credit bribery-x]})))}])
                             (make-run eid target card))})
                card nil))}})

(defcard "Brute-Force-Hack"
  {:on-play
   {:req (req (some #(and (ice? %)
                          (rezzed? %)
                          (can-pay? state side eid card nil
                                    [:credit (rez-cost state side %)]))
                    (all-installed state :corp)))
    :async true
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
                                {:choices {:card #(and (rezzed? %)
                                                       (some (fn [c] (and (= (first c)
                                                                             (:cid %))
                                                                          (<= (second c) target)))
                                                             affordable-ice))}
                                 :msg (msg "derez " (card-str state target))
                                 :effect (effect (derez target))}
                                card nil))}
             card nil)))}})

(defcard "Build Script"
  {:on-play
   {:msg "gain 1 [Credits] and draw 2 cards"
    :async true
    :effect (req (wait-for (gain-credits state side 1)
                           (draw state side eid 2 nil)))}})

(defcard "By Any Means"
  {:on-play
   {:effect
    (effect
      (register-events
        card
        [{:event :access
          :duration :end-of-turn
          :req (req (not (in-discard? target)))
          :interactive (req true)
          :async true
          :msg (msg "trash " (:title target) " at no cost and suffer 1 meat damage")
          :effect (req (wait-for (trash state side (assoc target :seen true) nil)
                                 (swap! state assoc-in [:runner :register :trashed-card] true)
                                 (damage state :runner eid :meat 1 {:unboostable true})))}]))}})

(defcard "Calling in Favors"
  {:on-play
   {:msg (msg "gain " (count (filter #(and (has-subtype? % "Connection") (resource? %))
                                     (all-active-installed state :runner))) " [Credits]")
    :async true
    :effect (effect (gain-credits eid (count (filter #(and (has-subtype? % "Connection")
                                                           (resource? %))
                                                     (all-active-installed state :runner)))))}})

(defcard "Career Fair"
  {:on-play
   {:prompt "Select a resource to install from your Grip"
    :req (req (some #(and (resource? %)
                          (can-pay? state side (assoc eid :source card :source-type :runner-install) % nil
                                    [:credit (install-cost state side % {:cost-bonus -3})]))
                    (:hand runner)))
    :choices {:req (req (and (resource? target)
                             (in-hand? target)
                             (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                       [:credit (install-cost state side target {:cost-bonus -3})])))}
    :async true
    :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target {:cost-bonus -3}))}})

(defcard "Careful Planning"
  {:on-play
   {:prompt "Choose a card in or protecting a remote server"
    :choices {:card #(is-remote? (second (get-zone %)))}
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
                                             true))))}})

(defcard "CBI Raid"
  (letfn [(cbi-final [chosen original]
            {:player :corp
             :prompt (str "The top cards of R&D will be " (string/join  ", " (map :title chosen)) ".")
             :choices ["Done" "Start over"]
             :async true
             :effect (req (if (= target "Done")
                            (do (doseq [c (reverse chosen)]
                                  (move state :corp c :deck {:front true}))
                                (effect-completed state side eid))
                            (continue-ability state side (cbi-choice original '() (count original) original)
                                              card nil)))})
          (cbi-choice [remaining chosen n original]
            {:player :corp
             :prompt "Choose a card to move next onto R&D"
             :choices remaining
             :async true
             :effect (effect
                       (continue-ability
                         (let [chosen (cons target chosen)]
                           (if (< (count chosen) n)
                             (cbi-choice (remove-once #(= target %) remaining) chosen n original)
                             (cbi-final chosen original)))
                         card nil))})]
    {:makes-run true
     :on-play {:req (req hq-runnable)
               :async true
               :effect (effect (make-run eid :hq card))}
     :events [(successful-run-replace-access
                {:target-server :hq
                 :mandatory true
                 :this-card-run true
                 :ability
                 {:msg "force the Corp to add all cards in HQ to the top of R&D"
                  :player :corp
                  :waiting-prompt "Corp to make a decision"
                  :async true
                  :effect (effect
                            (continue-ability
                              (let [from (:hand corp)]
                                (when (pos? (count from))
                                  (cbi-choice from '() (count from) from)))
                              card nil))}})]}))

(defcard "Code Siphon"
  (letfn [(rd-ice [state]
            (* -3 (count (get-in @state [:corp :servers :rd :ices]))))]
    {:makes-run true
     :on-play {:req (req rd-runnable)
               :async true
               :effect (effect (make-run eid :rd card))}
     :events [(successful-run-replace-access
                {:target-server :rd
                 :this-card-run true
                 :ability
                 {:async true
                  :prompt "Choose a program to install"
                  :msg (msg "install " (:title target) " and take 1 tag")
                  :choices (req (filter #(and (program? %)
                                              (runner-can-install? state side % false)
                                              (can-pay? state side (assoc eid :source card :source-type :runner-install) % nil
                                                        [:credit (install-cost state side % {:cost-bonus (rd-ice state)})]))
                                        (:deck runner)))
                  :effect (req (trigger-event state side :searched-stack nil)
                               (shuffle! state side :deck)
                               (wait-for (runner-install state side target {:cost-bonus (rd-ice state)})
                                         (gain-tags state side eid 1)))}})]}))

(defcard "Cold Read"
  {:implementation "Used programs restriction not enforced"
   :makes-run true
   :data {:counter {:credit 4}}
   :on-play {:async true
             :prompt "Choose a server"
             :choices (req runnable-servers)
             :effect (effect (make-run eid target card))}
   :interactions {:pay-credits {:type :credit}}
   :events [{:event :run-ends
             :player :runner
             :prompt "Choose a program that was used during the run"
             :choices {:card #(and (program? %)
                                   (installed? %))}
             :msg (msg "trash " (:title target))
             :async true
             :effect (effect (trash eid target {:unpreventable true}))}]})

(defcard "Compile"
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
    {:makes-run true
     :on-play {:prompt "Choose a server"
               :msg "make a run and install a program on encounter with the first piece of ice"
               :choices (req runnable-servers)
               :async true
               :effect (effect (make-run eid target card))}
     :events [{:event :encounter-ice
               :optional
               {:prompt "Install a program?"
                :once :per-run
                :yes-ability
                {:async true
                 :prompt "Install from where?"
                 :choices (req (if (not (zone-locked? state :runner :discard)) ["Stack" "Heap"] ["Stack"] ))
                 :msg (msg "install a program from their " target)
                 :effect (effect (continue-ability
                                   (compile-fn (if (= "Stack" target) :deck :discard))
                                   card nil))}}}
              {:event :run-ends
               :effect (req (let [compile-installed (first (filterv #(get-in % [:special :compile-installed])
                                                                    (all-active-installed state :runner)))]
                              (when (some? compile-installed)
                                (system-msg state :runner (str "moved " (:title compile-installed)
                                                               " to the bottom of the Stack"))
                                (move state :runner compile-installed :deck))))}]}))

(defcard "Contaminate"
  {:on-play
   {:msg (msg "place 3 virus tokens on " (:title target))
    :choices {:req (req (and (installed? target)
                             (runner? target)
                             (zero? (get-virus-counters state target))))}
    :async true
    :effect (effect (add-counter :runner eid target :virus 3 nil))}})

(defcard "Corporate \"Grant\""
  {:events [{:event :runner-install
             :req (req (first-event? state side :runner-install))
             :msg "force the Corp to lose 1 [Credit]"
             :async true
             :effect (effect (lose-credits :corp eid 1))}]})

(defcard "Corporate Scandal"
  {:on-play {:msg "give the Corp 1 additional bad publicity"
             :implementation "No enforcement that this Bad Pub cannot be removed"
             :effect (req (swap! state update-in [:corp :bad-publicity :additional] inc))}
   :leave-play (req (swap! state update-in [:corp :bad-publicity :additional] dec))})

(defcard "Creative Commission"
  {:on-play {:msg (msg "gain 5 [Credits]"
                       (when (pos? (:click runner))
                         " and lose [Click]"))
             :async true
             :effect (req (when (pos? (:click runner))
                            (lose state :runner :click 1))
                          (gain-credits state :runner eid 5))}})

(defcard "Credit Crash"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :choices (req runnable-servers)
             :async true
             :effect (effect (make-run eid target card))}
   :events [{:event :pre-access-card
             :once :per-run
             :async true
             :req (req (not (agenda? target)))
             :effect (req (let [c target
                                cost (or (and (or (asset? c)
                                                  (upgrade? c)
                                                  (ice? c))
                                              (rez-cost state side c))
                                         (and (operation? c)
                                              (play-cost state side c)))
                                title (:title c)]
                            (if (can-pay? state :corp eid card nil [:credit cost])
                              (continue-ability
                                state :corp
                                {:optional
                                 {:waiting-prompt "Corp to choose an option"
                                  :prompt (msg "Spend " cost " [Credits] to prevent the trash of " title "?")
                                  :player :corp
                                  :yes-ability {:async true
                                                :effect (req (system-msg state :corp (str "spends " cost " [Credits] to prevent "
                                                                                          title " from being trashed at no cost"))
                                                             (lose-credits state :corp eid cost))}
                                  :no-ability {:msg (msg "trash " title " at no cost")
                                               :async true
                                               :effect (effect (trash eid (assoc c :seen true) nil))}}}
                                card nil)
                              (do (system-msg state side (str "uses Credit Crash to trash " title " at no cost"))
                                  (trash state side eid (assoc c :seen true) nil)))))}]})

(defcard "Credit Kiting"
  {:on-play
   {:req (req (some #{:hq :rd :archives} (:successful-run runner-reg)))
    :prompt "Select a card to install from your Grip"
    :choices {:req (req (and (not (event? target))
                             (in-hand? target)
                             (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                       [:credit (install-cost state side target {:cost-bonus -8})])))}
    :async true
    :effect (req (let [new-eid (make-eid state {:source card :source-type :runner-install})]
                   (wait-for (runner-install state :runner new-eid target {:cost-bonus -8})
                             (gain-tags state :runner eid 1))))}})

(defcard "Cyber Threat"
  {:makes-run true
   :on-play
   {:prompt "Choose a server"
    :choices (req runnable-servers)
    :async true
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
                        {:prompt (msg "Rez a piece of ice protecting " serv "?")
                         :yes-ability {:async true
                                       :prompt (msg "Select a piece of ice protecting " serv " to rez")
                                       :player :corp
                                       :choices {:card #(and (installed? %)
                                                             (not (rezzed? %))
                                                             (ice? %)
                                                             (can-pay? state side eid card nil
                                                                       [:credit (rez-cost state side %)]))}
                                       :effect (effect (rez :corp eid target))
                                       :cancel-effect
                                       (effect (register-run-flag!
                                                 card
                                                 :can-rez
                                                 (fn [state side card]
                                                   (if (ice? card)
                                                     ((constantly false)
                                                      (toast state :corp "Cannot rez ice on this run due to Cyber Threat"))
                                                     true)))
                                               (make-run eid serv card))}
                         :no-ability {:async true
                                      :effect (effect (register-run-flag!
                                                        card
                                                        :can-rez
                                                        (fn [state side card]
                                                          (if (ice? card)
                                                            ((constantly false)
                                                             (toast state :corp "Cannot rez ice on this run due to Cyber Threat"))
                                                            true)))
                                                      (make-run eid serv card))
                                      :msg (msg "make a run on " serv " during which no ice can be rezzed")}}}
                       {:async true
                        :effect (effect (register-run-flag!
                                          card
                                          :can-rez
                                          (fn [state side card]
                                            (if (ice? card)
                                              ((constantly false)
                                               (toast state :corp "Cannot rez ice on this run due to Cyber Threat"))
                                              true)))
                                        (make-run eid serv card))
                        :msg (msg "make a run on " serv " during which no ice can be rezzed")})
                     card nil)))}})

(defcard "Data Breach"
  {:makes-run true
   :on-play {:async true
             :req (req rd-runnable)
             :effect (req (wait-for (make-run state side :rd card)
                                    (let [card (get-card state card)]
                                      (if (:run-again card)
                                        (make-run state side eid :rd card)
                                        (effect-completed state side eid)))))}
   :events [{:event :run-ends
             :optional {:req (req (and (:successful target)
                                       (not (:run-again card))
                                       (= [:rd] (:server target))))
                        :prompt "Make another run on R&D?"
                        :yes-ability {:effect (effect (clear-wait-prompt :corp)
                                                      (update! (assoc card :run-again true)))}}}]})

(defcard "Day Job"
  {:on-play
   {:additional-cost [:click 3]
    :msg "gain 10 [Credits]"
    :async true
    :effect (effect (gain-credits eid 10))}})

(defcard "Deep Data Mining"
  {:makes-run true
   :on-play {:async true
             :req (req rd-runnable)
             :effect (effect (make-run eid :rd card))}
   :events [{:event :successful-run
             :req (req (and (= :rd (target-server context))
                            this-card-run))
             :silent (req true)
             :effect (effect (access-bonus :rd (max 0 (min 4 (available-mu state)))))}]})

(defcard "Déjà Vu"
  {:on-play
   {:req (req (not (zone-locked? state :runner :discard)))
    :prompt "Choose a card to add to Grip"
    :choices (req (cancellable (:discard runner) :sorted))
    :msg (msg "add " (:title target) " to their Grip")
    :async true
    :effect (effect (move target :hand)
                    (continue-ability
                      (when (has-subtype? target "Virus")
                        {:prompt "Choose a virus to add to Grip"
                         :msg (msg "add " (:title target) " to their Grip")
                         :choices (req (cancellable (filter #(has-subtype? % "Virus") (:discard runner)) :sorted))
                         :effect (effect (move target :hand))})
                      card nil))}})

(defcard "Demolition Run"
  {:makes-run true
   :on-play {:req (req (or rd-runnable hq-runnable))
             :prompt "Choose a server"
             :choices ["HQ" "R&D"]
             :async true
             :effect (effect (make-run eid target card))}
   :interactions {:access-ability
                  {:label "Trash card"
                   :msg (msg "trash " (:title target) " at no cost")
                   :async true
                   :effect (effect (trash eid (assoc target :seen true) nil))}}})

(defcard "Deuces Wild"
  (let [all [{:effect (effect (gain-credits eid 3))
              :async true
              :msg "gain 3 [Credits]"}
             {:async true
              :effect (effect (draw eid 2 nil))
              :msg "draw 2 cards"}
             {:async true
              :effect (effect (lose-tags eid 1))
              :msg "remove 1 tag"}
             {:prompt "Select 1 piece of ice to expose"
              :msg "expose 1 ice and make a run"
              :choices {:card #(and (installed? %)
                                    (ice? %))}
              :async true
              :effect (req (wait-for (expose state side target)
                                     (continue-ability
                                       state side
                                       {:prompt "Choose a server"
                                        :choices (req runnable-servers)
                                        :async true
                                        :effect (effect (make-run eid target))}
                                       card nil)))
              :cancel-effect (effect (continue-ability
                                       {:prompt "Choose a server"
                                        :choices (req runnable-servers)
                                        :async true
                                        :effect (effect (make-run eid target))}
                                       card nil))}]
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
    {:on-play
     {:async true
      :effect (effect (continue-ability (choice all) card nil))}}))

(defcard "Diana's Hunt"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :msg "make a run"
             :choices (req runnable-servers)
             :async true
             :effect (effect (make-run eid target card))}
   :events [{:event :encounter-ice
             :optional
             {:req (req (seq (filter program? (:hand runner))))
              :prompt "Install a program from your grip?"
              :yes-ability
              {:prompt "Choose a program in your grip to install"
               :async true
               :choices {:card #(and (in-hand? %)
                                     (program? %))}
               :msg (msg "install " (:title target) ", ignoring all costs")
               :effect (effect (runner-install eid (assoc-in target [:special :diana-installed] true) {:ignore-all-cost true}))}}}
            {:event :run-ends
             :async true
             :effect (req (let [installed-cards (filter #(get-in % [:special :diana-installed]) (all-active-installed state :runner))]
                            (if (seq installed-cards)
                              (do
                                (system-msg state :runner (str "trashes " (count installed-cards)
                                                               " cards (" (string/join ", " (map :title installed-cards))
                                                                          ") at the end of the run from Diana's Hunt"))
                                (trash-cards state :runner eid installed-cards {:unpreventable true}))
                              (effect-completed state side eid))))}]})

(defcard "Diesel"
  {:on-play
   {:msg "draw 3 cards"
    :async true
    :effect (effect (draw eid 3 nil))}})

(defcard "Direct Access"
  {:makes-run true
   :on-play {:async true
             :effect (req (doseq [s [:corp :runner]]
                            (disable-identity state s))
                          (continue-ability
                            state side
                            {:prompt "Choose a server"
                             :choices (req runnable-servers)
                             :async true
                             :effect (effect (make-run eid target card))}
                            card nil))}
   :events [{:event :run-ends
             :unregister-once-resolved true
             :async true
             :effect (req (doseq [s [:corp :runner]]
                            (enable-identity state s))
                          (continue-ability
                            state :runner
                            {:optional
                             {:prompt "Shuffle Direct Access into the Stack?"
                              :yes-ability
                              {:msg (msg "shuffles Direct Access into the Stack")
                               :effect (effect (move (get-card state card) :deck)
                                               (shuffle! :deck))}}}
                            card nil))}]})

(defcard "Dirty Laundry"
  {:makes-run true
   :on-play {:async true
             :prompt "Choose a server"
             :choices (req runnable-servers)
             :effect (effect (make-run eid target card))}
   :events [{:event :run-ends
             :req (req (and (:successful target)
                            this-card-run))
             :msg "gain 5 [Credits]"
             :async true
             :effect (effect (gain-credits :runner eid 5))}]})

(defcard "Diversion of Funds"
  (letfn [(five-or-all [corp] (min 5 (:credit corp)))]
    {:makes-run true
     :on-play {:req (req hq-runnable)
               :async true
               :effect (effect (make-run eid :hq card))}
     :events [(successful-run-replace-access
                {:target-server :hq
                 :this-card-run true
                 :ability
                 {:msg (msg "force the Corp to lose " (five-or-all corp)
                            " [Credits], and gain " (five-or-all corp)
                            " [Credits]")
                  :async true
                  :effect (req (wait-for (gain-credits state :runner (five-or-all corp))
                                         (lose-credits state :corp eid (five-or-all corp))))}})]}))

(defcard "Divide and Conquer"
  {:makes-run true
   :on-play {:req (req archives-runnable)
             :async true
             :effect (effect (make-run eid :archives card))}
   :events [{:event :end-access-phase
             :async true
             :req (req (and (= :archives (:from-server target))
                            (:successful run)))
             :effect (req (wait-for (do-access state side [:hq] {:no-root true})
                                    (do-access state side eid [:rd] {:no-root true})))}]})

(defcard "Drive By"
  {:on-play
   {:choices {:card #(let [topmost (get-nested-host %)]
                       (and (is-remote? (second (get-zone topmost)))
                            (= (last (get-zone topmost)) :content)
                            (not (:rezzed %))))}
    :async true
    :effect (req (wait-for (expose state side target)
                           (if-let [target async-result]
                             (if (or (asset? target)
                                     (upgrade? target))
                               (do (system-msg state :runner (str "uses Drive By to trash " (:title target)))
                                   (trash state :runner eid (assoc target :seen true) nil))
                               (effect-completed state side eid))
                             (effect-completed state side eid))))}})

(defcard "Early Bird"
  {:makes-run true
   :on-play
   {:prompt "Choose a server"
    :choices (req runnable-servers)
    :msg (msg "make a run on " target " and gain [Click]")
    :async true
    :effect (effect (gain :click 1)
                    (make-run eid target card))}})

(defcard "Easy Mark"
  {:on-play
   {:msg "gain 3 [Credits]"
    :async true
    :effect (effect (gain-credits eid 3))}})

(defcard "Embezzle"
  {:makes-run true
   :on-play {:async true
             :req (req hq-runnable)
             :effect (effect (make-run eid :hq card))}
   :events [(successful-run-replace-access
              {:target-server :hq
               :this-card-run true
               :mandatory true
               :ability
               {:prompt "Choose a card type"
                :choices ["Asset" "Upgrade" "Operation" "ICE"]
                :msg (msg "reveal 2 cards from HQ and trash all "
                          target (when (not (= "ICE" target)) "s"))
                :async true
                :effect (req (let [cards-to-reveal (take 2 (shuffle (:hand corp)))
                                   cards-to-trash (filter #(is-type? % target) cards-to-reveal)
                                   credits (* 4 (count cards-to-trash))]
                               (system-msg state side
                                           (str "uses Embezzle to reveal "
                                                (string/join " and " (map :title cards-to-reveal))
                                                " from HQ"))
                               (wait-for
                                 (reveal state side cards-to-reveal)
                                 (if (pos? credits)
                                   (do (system-msg
                                         state side
                                         (str " uses Embezzle to trash "
                                              (string/join " and " (map :title cards-to-trash))
                                              " from HQ and gain "
                                              credits " [Credits]"))
                                       (wait-for (trash-cards state :runner (map #(assoc % :seen true) cards-to-trash))
                                                 (gain-credits state :runner eid credits)))
                                   (effect-completed state side eid)))))}})]})

(defcard "Emergency Shutdown"
  {:on-play
   {:req (req (some #{:hq} (:successful-run runner-reg)))
    :msg (msg "derez " (:title target))
    :choices {:card #(and (ice? %)
                          (rezzed? %))}
    :effect (effect (derez target))}})

(defcard "Emergent Creativity"
  (letfn [(ec [trash-cost to-trash]
            {:async true
             :prompt "Choose a hardware or program to install"
             :msg (msg "trash " (if (empty? to-trash) "no cards" (string/join ", " (map :title to-trash)))
                       " and install " (:title target)
                       " lowering the cost by " trash-cost)
             :choices (req (cancellable (filter #(and (or (program? %)
                                                          (hardware? %))
                                                      (can-pay? state side (assoc eid :source card :source-type :runner-install) % nil
                                                                [:credit (install-cost state side % {:cost-bonus (- trash-cost)})]))
                                                (:deck runner)) :sorted))
             :effect (req (trigger-event state side :searched-stack nil)
                          (shuffle! state side :deck)
                          (runner-install state side (assoc eid :source card :source-type :runner-install)
                                          target {:cost-bonus (- trash-cost)}))})]
    {:on-play
     {:prompt "Choose Hardware and Programs to trash from your Grip"
      :choices {:card #(and (or (hardware? %)
                                (program? %))
                         (in-hand? %))
                :max (req (count (:hand runner)))}
      :cancel-effect (effect (continue-ability (ec 0 []) card nil))
      :async true
      :effect (req (let [trash-cost (reduce + (keep :cost targets))
                         to-trash targets]
                     (wait-for (trash-cards state side to-trash {:unpreventable true})
                               (continue-ability state side (ec trash-cost to-trash) card nil))))}}))

(defcard "Employee Strike"
  {:on-play {:msg "disable the Corp's identity"
             :effect (effect (disable-identity :corp))}
   :disable-id true
   :leave-play (effect (enable-identity :corp))})

(defcard "En Passant"
  {:on-play
   {:req (req (and (:successful-run runner-reg)
                   (->> (:events (:last-run runner-reg))
                        (filter #(= :pass-ice (first %)))
                        (map second)
                        (keep #(get-card state (:ice (first %))))
                        (filter (complement rezzed?))
                        seq)))
    :prompt "Choose an unrezzed piece of ice that you passed on your last run"
    :choices {:req (req (some #(same-card? target %)
                              (->> (:events (:last-run runner-reg))
                                   (filter #(= :pass-ice (first %)))
                                   (map second)
                                   (keep #(get-card state (:ice (first %))))
                                   (filter (complement rezzed?)))))}
    :msg (msg "trash " (card-str state target))
    :async true
    :effect (effect (trash eid target nil))}})

(defcard "Encore"
  {:on-play
   {:req (req (and (some #{:hq} (:successful-run runner-reg))
                   (some #{:rd} (:successful-run runner-reg))
                   (some #{:archives} (:successful-run runner-reg))))
    :rfg-instead-of-trashing true
    :msg "take an additional turn after this one"
    :effect (req (swap! state update-in [:runner :extra-turns] (fnil inc 0)))}})

(defcard "Escher"
  (letfn [(es [] {:async true
                  :prompt "Select two pieces of ice to swap positions"
                  :choices {:card #(and (installed? %)
                                        (ice? %))
                            :max 2}
                  :effect (req (if (= (count targets) 2)
                                 (do (swap-ice state side (first targets) (second targets))
                                     (continue-ability state side (es) card nil))
                                 (do (system-msg state side "has finished rearranging ice")
                                     (effect-completed state side eid))))})]
    {:makes-run true
     :on-play {:req (req hq-runnable)
               :async true
               :effect (effect (make-run eid :hq card))}
     :events [(successful-run-replace-access
                {:target-server :hq
                 :this-card-run true
                 :mandatory true
                 :ability
                 {:async true
                  :msg "rearrange installed ice"
                  :effect (effect (continue-ability (es) card nil))}})]}))

(defcard "Eureka!"
  {:on-play
   {:async true
    :effect (req (let [topcard (first (:deck runner))
                       caninst (and (or (hardware? topcard)
                                        (program? topcard)
                                        (resource? topcard))
                                    (can-pay? state side (assoc eid :source card :source-type :runner-install) topcard nil
                                              [:credit (install-cost state side topcard {:cost-bonus -10})]))]
                   (if caninst
                     (continue-ability
                       state side
                       {:optional
                        {:prompt (msg "Install " (:title topcard) "?")
                         :yes-ability {:async true
                                       :effect (effect (runner-install eid topcard {:cost-bonus -10}))}
                         :no-ability {:async true
                                      :effect (req (wait-for
                                                     (reveal state side topcard)
                                                     (system-msg (str "reveals and trashes "
                                                                      (:title topcard)))
                                                     (trash eid topcard {:unpreventable true})))}}}
                       card nil)
                     (wait-for (reveal state side topcard)
                               (system-msg state side (str "reveals and trashes " (:title topcard)))
                               (trash state side eid topcard {:unpreventable true})))))}})

(defcard "Exclusive Party"
  {:on-play
   {:msg (msg "draw 1 card and gain "
              (count (filter #(= (:title %) "Exclusive Party") (:discard runner)))
              " [Credits]")
    :async true
    :effect (req (wait-for (draw state side 1 nil)
                           (gain-credits state side eid (count (filter #(= (:title %) "Exclusive Party") (:discard runner))))))}})

(defcard "Executive Wiretaps"
  {:on-play
   {:msg (msg "reveal cards in HQ: " (string/join ", " (sort (map :title (:hand corp)))))
    :async true
    :effect (effect (reveal eid (:hand corp)))}})

(defcard "Exploit"
  {:on-play
   {:req (req (and (some #{:hq} (:successful-run runner-reg))
                   (some #{:rd} (:successful-run runner-reg))
                   (some #{:archives} (:successful-run runner-reg))))
    :prompt "Choose up to 3 pieces of ice to derez"
    :choices {:max 3
              :card #(and (rezzed? %)
                          (ice? %))}
    :msg (msg "derez " (string/join ", " (map :title targets)))
    :effect (req (doseq [c targets]
                   (derez state side c)))}})

(defcard "Exploratory Romp"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :choices (req runnable-servers)
             :async true
             :effect (effect (make-run eid target card))}
   :events [(successful-run-replace-access
              {:mandatory true
               :this-card-run true
               :ability
               {:req (req (some #(and (pos? (get-counters % :advancement))
                                      (= (first (:server run)) (second (get-zone %))))
                                (all-installed state :corp)))
                :prompt "Advancements to remove from a card in or protecting this server?"
                :choices ["0" "1" "2" "3"]
                :async true
                :waiting-prompt "Runner to choose an option"
                :effect (req (let [c (str->int target)]
                               (continue-ability
                                 state side
                                 {:choices {:card #(and (pos? (get-counters % :advancement))
                                                        (= (first (:server run)) (second (get-zone %))))}
                                  :msg (msg "remove " (quantify c "advancement token")
                                            " from " (card-str state target))
                                  :effect (req (let [to-remove (min c (get-counters target :advancement))]
                                                 (add-prop state :corp target :advance-counter (- to-remove))))}
                                 card nil)))}})]})

(defcard "Express Delivery"
  {:on-play
   {:prompt "Choose a card to add to your Grip"
    :choices (req (take 4 (:deck runner)))
    :msg "look at the top 4 cards of their Stack and add 1 of them to their Grip"
    :effect (effect (move target :hand)
                    (shuffle! :deck))}})

(defcard "Falsified Credentials"
  {:on-play
   {:prompt "Choose a type"
    :choices ["Agenda" "Asset" "Upgrade"]
    :msg (msg "guess " target)
    :async true
    :effect (effect
              (continue-ability
                (let [chosen-type target]
                  {:choices {:card #(let [topmost (get-nested-host %)]
                                      (and (is-remote? (second (get-zone topmost)))
                                           (= (last (get-zone topmost)) :content)
                                           (not (rezzed? %))))}
                   :async true
                   :effect (req (wait-for (expose state side target)
                                          (continue-ability
                                            state :runner
                                            (if (and async-result ;; expose was successful
                                                     (= chosen-type (:type target)))
                                              {:async true
                                               :effect (effect (gain-credits eid 5))
                                               :msg "gain 5 [Credits]"})
                                            card nil)))})
                card nil))}})

(defcard "Fear the Masses"
  {:makes-run true
   :on-play {:req (req hq-runnable)
             :async true
             :effect (effect (make-run eid :hq card))}
   :events [(successful-run-replace-access
              {:target-server :hq
               :this-card-run true
               :mandatory true
               :ability
               {:async true
                :msg "force the Corp to trash the top card of R&D"
                :effect (req (wait-for
                               (mill state :corp :corp 1)
                               (continue-ability
                                 state side
                                 (let [n (count (filter #(same-card? :title card %) (:hand runner)))]
                                   {:async true
                                    :prompt "Reveal how many copies of Fear the Masses?"
                                    :choices {:card #(and (in-hand? %)
                                                          (same-card? :title card %))
                                              :max n}
                                    :msg (msg "reveal " (count targets) " copies of Fear the Masses,"
                                              " forcing the Corp to trash " (count targets)
                                              " additional cards from the top of R&D")
                                    :effect (req (wait-for
                                                   (reveal state :runner targets)
                                                   (mill state :corp eid :corp (count targets))))})
                                 card nil)))}})]})

(defcard "Feint"
  {:makes-run true
   :on-play {:req (req hq-runnable)
             :async true
             :effect (effect (make-run eid :hq card))}
   :events [{:event :encounter-ice
             :req (req (< (get-in card [:special :bypass-count] 0) 2))
             :msg (msg "bypass " (:title (:ice context)))
             :effect (req (bypass-ice state)
                          (update! state side (update-in card [:special :bypass-count] (fnil inc 0))))}
            {:event :successful-run
             :effect (effect (prevent-access))}]})

(defcard "Fisk Investment Seminar"
  {:on-play
   {:msg "make each player draw 3 cards"
    :async true
    :effect (req (wait-for (draw state :runner 3 nil)
                           (draw state :corp eid 3 nil)))}})

(defcard "Forged Activation Orders"
  {:on-play
   {:choices {:card #(and (ice? %)
                          (not (rezzed? %)))}
    :async true
    :effect (req (let [ice target
                       serv (zone->name (second (get-zone ice)))
                       icepos (card-index state ice)]
                   (continue-ability
                     state :corp
                     {:prompt (str "Rez " (:title ice) " at position " icepos
                                   " of " serv " or trash it?")
                      :choices ["Rez" "Trash"]
                      :async true
                      :waiting-prompt "Corp to choose an option"
                      :effect (effect (continue-ability
                                        (if (and (= target "Rez")
                                                 (<= (rez-cost state :corp ice)
                                                     (:credit corp)))
                                          {:msg (msg "force the rez of " (:title ice))
                                           :async true
                                           :effect (effect (rez :corp eid ice))}
                                          {:msg (msg "trash the ice at position " icepos " of " serv)
                                           :async true
                                           :effect (effect (trash :corp eid ice))})
                                        card nil))}
                     card nil)))}})

(defcard "Forked"
  (cutlery "Sentry"))

(defcard "Frame Job"
  {:on-play
   {:prompt "Choose an agenda to forfeit"
    :choices (req (:scored runner))
    :msg (msg "forfeit " (:title target) " and give the Corp 1 bad publicity")
    :effect (effect (forfeit target)
                    (gain-bad-publicity :corp 1))}})

(defcard "Frantic Coding"
  {:on-play
   {:async true
    :effect
    (effect
      (continue-ability
        (let [top-ten (take 10 (:deck runner))]
          {:prompt (str "The top 10 cards of the stack are " (string/join ", " (map :title top-ten)) ".")
           :choices ["OK"]
           :async true
           :effect
           (effect
             (continue-ability
               {:prompt "Install a program?"
                :choices (concat
                           (->> top-ten
                                (filter #(and (program? %)
                                              (can-pay? state side (assoc eid :source card :source-type :runner-install) % nil
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
                                             (do (system-msg state side (str "trashes " (string/join ", " (map :title to-trash))))
                                                 (trash-cards state side eid to-trash {:unpreventable true}))
                                             (do (system-msg state side "does not have to trash cards because the stack was shuffled")
                                                 (effect-completed state side eid)))))
                               (do (system-msg state side (str "trashes " (string/join ", " (map :title top-ten))))
                                   (trash-cards state side eid top-ten {:unpreventable true}))))}
               card nil))})
        card nil))}})

(defcard "\"Freedom Through Equality\""
  {:events [{:event :agenda-stolen
             :msg "add it to their score area as an agenda worth 1 agenda point"
             :async true
             :effect (req (as-agenda state :runner eid card 1))}]})

(defcard "Freelance Coding Contract"
  {:on-play
   {:choices {:max 5
              :card #(and (program? %)
                          (in-hand? %))}
    :msg (msg "trash " (string/join ", " (map :title targets)) " and gain "
              (* 2 (count targets)) " [Credits]")
    :async true
    :effect (req (wait-for (trash-cards state side targets {:unpreventable true})
                           (gain-credits state side eid (* 2 (count targets)))))}})

(defcard "Game Day"
  {:on-play
   {:msg (msg "draw " (- (hand-size state :runner) (count (:hand runner))) " cards")
    :async true
    :effect (effect (draw eid (- (hand-size state :runner) (count (:hand runner))) nil))}})

(defcard "Glut Cipher"
  {:makes-run true
   :on-play {:req (req archives-runnable)
             :async true
             :effect (effect (make-run eid :archives card))}
   :events [(successful-run-replace-access
              {:target-server :archives
               :this-card-run true
               :mandatory true
               :ability
               {:req (req (<= 5 (count (:discard corp))))
                :show-discard true
                :async true
                :player :corp
                :waiting-prompt "Corp to make a decision"
                :prompt "Select 5 cards from Archives to add to HQ"
                :choices {:max 5
                          :all true
                          :card #(and (corp? %)
                                      (in-discard? %))}
                :msg (msg "move "
                          (let [seen (filter :seen targets)
                                m (count  (remove :seen targets))]
                            (str (string/join ", " (map :title seen))
                                 (when (pos? m)
                                   (str (when-not (empty? seen) " and ")
                                        (quantify m "unseen card")))
                                 " into HQ, then trash 5 cards")))
                :effect (req (doseq [c targets]
                               (move state side c :hand))
                             (trash-cards state :corp eid (take 5 (shuffle (:hand (:corp @state))))))}})]})

(defcard "Government Investigations"
  {:flags {:prevent-secretly-spend (req 2)}})

(defcard "Guinea Pig"
  {:on-play
   {:msg "trash all cards in the grip and gain 10 [Credits]"
    :async true
    :effect (req (wait-for (trash-cards state side (:hand runner) {:unpreventable true})
                           (gain-credits state :runner eid 10)))}})

(defcard "Hacktivist Meeting"
  {:constant-effects [{:type :rez-additional-cost
                       :req (req (not (ice? target)))
                       :value [:randomly-trash-from-hand 1]}]})

(defcard "Harmony AR Therapy"
  (letfn [(choose-end [to-shuffle]
            (let [to-shuffle (sort to-shuffle)]
              {:msg (msg "shuffle " (count to-shuffle)" cards back into the stack: " (string/join ", " to-shuffle))
               :effect (req (doseq [c-title to-shuffle]
                              (let [c (some #(when (= (:title %) c-title) %) (:discard runner))]
                                (move state side c :deck)))
                            (shuffle! state side :deck))}))
          (choose-next [to-shuffle target remaining]
            (let [remaining (if (= "Done" target)
                              remaining
                              (remove #(= % target) remaining))
                  to-shuffle (if (= "Done" target)
                               to-shuffle
                               (if target
                                 (concat to-shuffle [target])
                                 []))
                  remaining-choices (- 5 (count to-shuffle))
                  finished? (or (= "Done" target)
                                (= 0 remaining-choices)
                                (empty? remaining))]
              {:prompt (msg (if finished?
                              (str "Shuffling: " (string/join ", " to-shuffle))
                              (str "Choose up to " remaining-choices
                                   (when (not-empty to-shuffle)
                                     " more")
                                   " cards."
                                   (when (not-empty to-shuffle)
                                     (str "[br]Shuffling: " (string/join ", " to-shuffle))))))
               :async true
               :choices (req (if finished?
                               ["OK" "Start over"]
                               (concat remaining (when (not-empty to-shuffle) ["Done"]))))
               :effect (req (if finished?
                              (if (= "OK" target)
                                (continue-ability state side (choose-end to-shuffle) card nil)
                                (continue-ability state side (choose-next '() nil (distinct (map :title (:discard runner)))) card nil))
                              (continue-ability state side (choose-next to-shuffle target remaining) card nil)))}))]
    {:on-play
     {:rfg-instead-of-trashing true
      :waiting-prompt "Runner to make a decision"
      :async true
      :effect (req (if (and (not (zone-locked? state :runner :discard))
                            (pos? (count (:discard runner))))
                     (continue-ability state side (choose-next '() nil (sort (distinct (map :title (:discard runner))))) card nil)
                     (do (system-msg state :runner (str "uses " (:title card) " to shuffle their Stack"))
                         (shuffle! state :runner :deck)
                         (effect-completed state side eid))))}}))

(defcard "High-Stakes Job"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :choices (req (let [unrezzed-ice #(seq (filter (complement rezzed?) (:ices (second %))))
                                 bad-zones (keys (filter (complement unrezzed-ice) (get-in @state [:corp :servers])))]
                             (zones->sorted-names (remove (set bad-zones) (get-runnable-zones state side eid card nil)))))
             :async true
             :effect (effect (make-run eid target card))}
   :events [{:event :run-ends
             :req (req (and (:successful target)
                            this-card-run))
             :msg "gain 12 [Credits]"
             :async true
             :effect (effect (gain-credits :runner eid 12))}]})

(defcard "Hostage"
  {:on-play
   {:prompt "Choose a Connection"
    :choices (req (cancellable (filter #(has-subtype? % "Connection") (:deck runner)) :sorted))
    :msg (msg "add " (:title target) " to their Grip and shuffle their Stack")
    :async true
    :effect (effect (trigger-event :searched-stack nil)
                    (continue-ability
                      (let [connection target]
                        (if (can-pay? state side (assoc eid :source card :source-type :runner-install) connection nil
                                      [:credit (install-cost state side connection)])
                          {:optional {:prompt (str "Install " (:title connection) "?")
                                      :yes-ability {:async true
                                                    :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) connection nil)
                                                                    (shuffle! :deck))}
                                      :no-ability {:effect (effect (move connection :hand)
                                                                   (shuffle! :deck))}}}
                          {:effect (effect (move connection :hand)
                                           (shuffle! :deck))}))
                      card nil))}})

(defcard "Hot Pursuit"
  {:makes-run true
   :on-play {:req (req hq-runnable)
             :async true
             :effect (effect (make-run eid :hq card))}
   :events [{:event :successful-run
             :async true
             :msg "gain 9 [Credits] and take 1 tag"
             :req (req (and (= :hq (target-server context))
                            this-card-run))
             :effect (req (wait-for (gain-tags state :runner 1)
                                    (gain-credits state :runner eid 9)))}]})

(defcard "I've Had Worse"
  {:on-play {:async true
             :effect (effect (draw eid 3 nil))}
   :on-trash {:when-inactive true
              :interactive (req true)
              :async true
              :req (req (#{:meat :net} (:cause target)))
              :msg "draw 3 cards"
              :effect (effect (draw :runner eid 3 nil))}})

(defcard "Immolation Script"
  {:makes-run true
   :on-play {:req (req archives-runnable)
             :async true
             :effect (effect (make-run eid :archives card))}
   :events [{:event :pre-access
             :async true
             :req (req (and (= target :archives)
                            ;; don't prompt unless there's at least 1 rezzed piece of ice matching one in Archives
                            (not-empty (clojure.set/intersection
                                         (into #{} (map :title (filter ice? (:discard corp))))
                                         (into #{} (map :title (filter rezzed? (all-installed state :corp))))))))
             :prompt "Choose a piece of ice in Archives"
             :choices (req (filter ice? (:discard corp)))
             :effect (effect (continue-ability
                               (let [ice target]
                                 {:async true
                                  :prompt (msg "Select a rezzed copy of " (:title ice) " to trash")
                                  :choices {:card #(and (ice? %)
                                                        (rezzed? %)
                                                        (same-card? :title % ice))}
                                  :msg (msg "trash " (card-str state target))
                                  :effect (effect (trash eid target nil))})
                               card nil))}]})

(defcard "In the Groove"
  {:events [{:event :runner-install
             :duration :end-of-turn
             :req (req (<= 1 (:cost (:card context))))
             :interactive (req (or (has-subtype? (:card context) "Cybernetic")
                                   (first-event? state side :runner-install)))
             :async true
             :prompt "What to get from In the Groove?"
             :choices ["Draw 1 card" "Gain 1 [Credits]"]
             :msg (msg (string/lower-case target))
             :effect (req (if (= target "Draw 1 card")
                            (draw state side eid 1 nil)
                            (gain-credits state side eid 1)))}]})

(defcard "Independent Thinking"
  (letfn [(cards-to-draw [targets]
            (* (count targets)
               (if (some #(and (not (facedown? %)) (has-subtype? % "Directive")) targets) 2 1)))]
    {:on-play
     {:prompt "Choose up to 5 installed cards to trash"
      :choices {:max 5
                :card #(and (installed? %)
                         (runner? %))}
      :msg (msg "trash " (string/join ", " (map :title targets))
             " and draw " (quantify (cards-to-draw targets) "card"))
      :async true
      :effect (req (wait-for (trash-cards state side targets nil)
                             (draw state :runner eid (cards-to-draw targets) nil)))}}))

(defcard "Indexing"
  {:makes-run true
   :on-play {:req (req rd-runnable)
             :async true
             :effect (effect (make-run eid :rd card))}
   :events [(successful-run-replace-access
              {:target-server :rd
               :this-card-run true
               :ability
               {:msg "rearrange the top 5 cards of R&D"
                :waiting-prompt "Runner to make a decision"
                :async true
                :effect (effect
                          (continue-ability
                            (let [from (take 5 (:deck corp))]
                              (when (pos? (count from))
                                (reorder-choice :corp :corp from '() (count from) from)))
                            card nil))}})]})

(defcard "Infiltration"
  {:on-play
   {:prompt "Gain 2 [Credits] or expose a card?"
    :choices ["Gain 2 [Credits]" "Expose a card"]
    :async true
    :effect (effect (continue-ability
                      (if (= target "Expose a card")
                        {:choices {:card #(and (installed? %)
                                               (not (rezzed? %)))}
                         :async true
                         :effect (effect (expose eid target))}
                        {:msg "gain 2 [Credits]"
                         :async true
                         :effect (effect (gain-credits eid 2))})
                      card nil))}})

(defcard "Information Sifting"
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
            {:player :runner
             :waiting-prompt "Runner to choose an option"
             :prompt "Choose a pile to access"
             :choices [(str "Pile 1 (" (quantify (count p1) "card") ")")
                       (str "Pile 2 (" (quantify (count p2) "card") ")")]
             :async true
             :effect (req (let [choice (if (string/starts-with? target "Pile 1") 1 2)]
                            (system-msg state side (str "chooses to access " target))
                            (continue-ability
                              state side
                              (access-pile (if (= 1 choice) p1 p2) choice (count (if (= 1 choice) p1 p2)))
                              card nil)))})]
    (let [access-effect
          {:player :corp
           :req (req (<= 1 (count (:hand corp))))
           :async true
           :waiting-prompt "Corp to make a decision"
           :prompt (msg "Select up to " (dec (count (:hand corp))) " cards for the first pile")
           :choices {:card #(and (in-hand? %)
                                 (corp? %))
                     :max (req (dec (count (:hand corp))))}
           :effect (effect (continue-ability
                             (which-pile (shuffle targets)
                                         (shuffle (vec (clojure.set/difference
                                                         (set (:hand corp)) (set targets)))))
                             card nil))}]
      {:makes-run true
       :on-play {:req (req hq-runnable)
                 :async true
                 :effect (effect (make-run eid :hq card))}
       :events [(successful-run-replace-access
                  {:target-server :hq
                   :this-card-run true
                   :mandatory true
                   :ability access-effect})]})))

(defcard "Inject"
  {:on-play
   {:async true
    :effect (req (let [cards (take 4 (:deck runner))
                       programs (filter program? cards)
                       others (remove program? cards)]
                   (wait-for
                     (reveal state side cards)
                     (if (seq programs)
                       (wait-for (trash-cards state side programs {:unpreventable true})
                                 (system-msg state side (str "trashes "
                                                             (string/join ", " (map :title programs))
                                                             " and gains "
                                                             (count programs) " [Credits]"))
                                 (wait-for (gain-credits state side (count programs))
                                           (doseq [c others]
                                             (move state side c :hand)
                                             (system-msg state side (str "adds " (:title c) " to the grip")))
                                           (effect-completed state side eid)))
                       (do (doseq [c others]
                             (move state side c :hand)
                             (system-msg state side (str "adds " (:title c) " to the grip")))
                           (effect-completed state side eid))))))}})

(defcard "Injection Attack"
  {:makes-run true
   :on-play
   {:prompt "Choose a server"
    :choices (req runnable-servers)
    :async true
    :effect (effect (continue-ability
                      (let [server target]
                        {:prompt "Select an icebreaker"
                         :choices {:card #(and (installed? %)
                                               (has-subtype? % "Icebreaker"))}
                         :async true
                         :effect (effect (pump target 2 :end-of-run)
                                         (make-run eid server card))})
                      card nil))}})

(defcard "Inside Job"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :choices (req runnable-servers)
             :async true
             :effect (effect (make-run eid target card))}
   :events [{:event :encounter-ice
             :req (req (first-run-event? state side :encounter-ice))
             :once :per-run
             :msg (msg "bypass " (:title (:ice context)))
             :effect (req (bypass-ice state))}]})

(defcard "Insight"
  {:on-play
   {:async true
    :player :corp
    :waiting-prompt "Corp to make a decision"
    :effect (req (wait-for
                   (resolve-ability state :corp (reorder-choice :corp (take 4 (:deck corp))) card targets)
                   (let [top-4 (take 4 (get-in @state [:corp :deck]))]
                     (system-msg state :runner (str " reveals (top:) "
                                                    (string/join ", " (map :title top-4))
                                                    " from the top of R&D"))
                     (reveal state :runner eid top-4))))}})

(defcard "Interdiction"
  (let [ab (effect (register-turn-flag!
                     card :can-rez
                     (fn [state side card]
                       (if (and (= (:active-player @state) :runner) (not (ice? card)))
                         ((constantly false)
                          (toast state :corp "Cannot rez non-ice on the Runner's turn due to Interdiction"))
                         true))))]
    {:on-play {:msg "prevent the Corp from rezzing non-ice cards on the Runner's turn"
               :effect ab}
     :events [{:event :runner-turn-begins
               :effect ab}]
     :leave-play (req (clear-all-flags-for-card! state side card))}))

(defcard "Isolation"
  {:on-play
   {:additional-cost [:resource 1]
    :msg "gain 7 [Credits]"
    :async true
    :effect (effect (gain-credits eid 7))}})

(defcard "Itinerant Protesters"
  {:on-play {:msg "reduce the Corp's maximum hand size by 1 for each bad publicity"}
   :constant-effects [(corp-hand-size+ (req (- (count-bad-pub state))))]})

(defcard "Jailbreak"
  {:makes-run true
   :on-play {:req (req (or rd-runnable hq-runnable))
             :prompt "Choose a server"
             :choices ["HQ" "R&D"]
             :async true
             :effect (effect (make-run eid target card))}
   :events [{:event :successful-run
             :silent (req true)
             :async true
             :req (req (and (or (= :hq (target-server context))
                                (= :rd (target-server context)))
                            this-card-run))
             :effect (req (if (= :hq (target-server context))
                            (access-bonus state :runner :hq 1)
                            (access-bonus state :runner :rd 1))
                          (draw state side eid 1 nil))}]})

(defcard "Khusyuk"
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
                                         (keep :cost)
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
                                 :effect (effect (complete-with-result
                                                   eid [(str->int (first (string/split target #" ")))
                                                        (min 6 (str->int (nth (string/split target #" ") 2)))]))}))
        access-effect
        {:async true
         :effect (req (wait-for
                        (resolve-ability state side (select-install-cost state) card nil)
                        (let [revealed (seq (take (second async-result) (:deck corp)))]
                          (system-msg state :runner (str "uses Khusyuk to choose an install cost of "
                                                         (first async-result)
                                                         " [Credit] and reveals "
                                                         (if revealed
                                                           (str "(top:) " (string/join ", " (map :title revealed))
                                                                " from the top of R&D")
                                                           "no cards")))
                          (wait-for
                            (resolve-ability
                              state side
                              (when revealed
                                {:async true
                                 :effect (effect (reveal eid revealed))})
                              card nil)
                            (wait-for
                              (resolve-ability state side (when (and revealed (not (get-only-card-to-access state)))
                                                            (access-revealed revealed))
                                               card nil)
                              (shuffle! state :corp :deck)
                              (system-msg state :runner "shuffles R&D")
                              (effect-completed state side eid))))))}]
    {:makes-run true
     :on-play {:req (req rd-runnable)
               :async true
               :effect (effect (make-run eid :rd card))}
     :events [(successful-run-replace-access
                {:target-server :rd
                 :this-card-run true
                 :mandatory true
                 :ability access-effect})]}))

(defcard "Knifed"
  (cutlery "Barrier"))

(defcard "Kraken"
  {:on-play
   {:req (req (:stole-agenda runner-reg))
    :prompt "Choose a server"
    :choices (req servers)
    :msg (msg "force the Corp to trash a piece of ice protecting " target)
    :async true
    :effect (effect
              (continue-ability
                (let [serv (second (server->zone state target))
                      servname target]
                  {:player :corp
                   :async true
                   :prompt (msg "Select a piece of ice in " target " to trash")
                   :choices {:card #(and (ice? %)
                                         (= serv (second (get-zone %))))}
                   :effect (effect (system-msg (str "trashes " (card-str state target)))
                                   (trash :corp eid target nil))})
                card nil))}})

(defcard "Labor Rights"
  {:on-play
   {:req (req (pos? (+ (count (:deck runner)) (count (:discard runner)))))
    :rfg-instead-of-trashing true
    :async true
    :effect (req (let [mill-count (min 3 (count (:deck runner)))]
                   (wait-for (mill state :runner :runner mill-count)
                             (system-msg state :runner (str "trashes the top " (quantify mill-count "card") " of their stack"))
                             (let [heap-count (min 3 (count (get-in @state [:runner :discard])))]
                               (continue-ability
                                 state side
                                 (if (not (zone-locked? state :runner :discard))
                                   {:prompt (str "Choose " (quantify heap-count "card") " to shuffle into the stack")
                                    :show-discard true
                                    :async true
                                    :choices {:max heap-count
                                              :all true
                                              :not-self true
                                              :card #(and (runner? %)
                                                          (in-discard? %))}
                                    :effect (req (doseq [c targets]
                                                   (move state side c :deck))
                                                 (system-msg state :runner (str "shuffles " (string/join ", " (map :title targets))
                                                                                " from their Heap into their Stack, and draws 1 card"))
                                                 (shuffle! state :runner :deck)
                                                 (draw state :runner eid 1 nil))}

                                   {:effect (effect
                                              (do (system-msg state :runner "shuffles their Stack and draws 1 card")
                                                  (shuffle! state :runner :deck)
                                                  (draw state :runner eid 1 nil)))})
                                 card nil)))))}})

(defcard "Lawyer Up"
  {:on-play
   {:msg "remove 2 tags and draw 3 cards"
    :async true
    :effect (req (wait-for (lose-tags state side 2)
                           (draw state side eid 3 nil)))}})

(defcard "Lean and Mean"
  {:makes-run true
   :on-play
   {:prompt "Choose a server"
    :choices (req runnable-servers)
    :msg (msg "make a run on " target
              (when (<= (count (filter program? (all-active-installed state :runner))) 3)
                ", adding +2 strength to all icebreakers"))
    :async true
    :effect (req (when (<= (count (filter program? (all-active-installed state :runner))) 3)
                   (pump-all-icebreakers state side 2 :end-of-run))
                 (make-run state side eid target card))}})

(defcard "Leave No Trace"
  (letfn [(get-rezzed-cids [ice]
            (map :cid (filter #(and (rezzed? %)
                                    (ice? %))
                              ice)))]
    {:makes-run true
     :on-play {:prompt "Choose a server"
               :msg "make a run and derez all ice that is rezzed during this run"
               :choices (req runnable-servers)
               :async true
               :effect (req (let [old-ice-cids (get-rezzed-cids (all-installed state :corp))]
                              (update! state side (assoc-in card [:special :leave-no-trace] old-ice-cids))
                              (make-run state side eid target (get-card state card))))}
     :events [{:event :run-ends
               :effect (req (let [new (set (get-rezzed-cids (all-installed state :corp)))
                                  old (set (get-in (get-card state card) [:special :leave-no-trace]))
                                  diff-cid (seq (clojure.set/difference new old))
                                  diff (map #(find-cid % (all-installed state :corp)) diff-cid)]
                              (doseq [ice diff]
                                (derez state :runner ice))
                              (when-not (empty? diff)
                                (system-msg state :runner (str "uses Leave No Trace to derez " (string/join ", " (map :title diff)))))))}]}))

(defcard "Legwork"
  {:makes-run true
   :on-play {:req (req hq-runnable)
             :async true
             :effect (effect (make-run eid :hq card))}
   :events [{:event :successful-run
             :silent (req true)
             :req (req (and (= :hq (target-server context))
                            this-card-run))
             :effect (effect (access-bonus :hq 2))}]})

(defcard "Leverage"
  {:on-play
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
                                :effect (effect (unregister-floating-events :until-runner-turn-begins))}]))}}}})

(defcard "Levy AR Lab Access"
  {:on-play
   {:msg (msg (if (not (zone-locked? state :runner :discard))
                "shuffle their Grip and Heap into their Stack and draw 5 cards"
                "shuffle their Grip into their Stack and draw 5 cards"))
    :rfg-instead-of-trashing true
    :async true
    :effect (effect (shuffle-into-deck :hand :discard)
                    (draw eid 5 nil))}})

(defcard "Lucky Find"
  {:on-play
   {:msg "gain 9 [Credits]"
    :async true
    :effect (effect (gain-credits eid 9))}})

(defcard "Mad Dash"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :choices (req runnable-servers)
             :async true
             :effect (effect (make-run eid target card))}
   :events [{:event :run-ends
             :async true
             :req (req this-card-run)
             :effect (req (if (:did-steal target)
                            (do (system-msg state :runner
                                            (str "adds Mad Dash to their score area as an agenda worth 1 agenda point"))
                                (as-agenda state :runner eid (get-card state card) 1))
                            (do (system-msg state :runner
                                            (str "suffers 1 meat damage from Mad Dash"))
                                (damage state side eid :meat 1 {:card card}))))}]})

(defcard "Making an Entrance"
  (letfn [(entrance-trash [cards]
            {:prompt "Choose a card to trash"
             :choices (concat cards ["Done"])
             :async true
             :effect (req (if (= target "Done")
                            (continue-ability
                              state side
                              (when (seq cards)
                                (reorder-choice :runner :corp cards '()
                                                (count cards) cards))
                              card nil)
                            (wait-for (trash state side target {:unpreventable true})
                                      (system-msg state side (str "trash " (:title target)))
                                      (continue-ability
                                        state side
                                        (when-let [cards (seq (remove-once #(= % target) cards))]
                                          (entrance-trash cards))
                                        card nil))))})]
    {:on-play
     {:msg "look at and trash or rearrange the top 6 cards of their Stack"
      :async true
      :waiting-prompt "Runner to make a decision"
      :effect (effect (continue-ability (entrance-trash (take 6 (:deck runner))) card nil))}}))

(defcard "Marathon"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :choices (req (filter #(can-run-server? state %) remotes))
             :async true
             :effect (effect (make-run eid target card))}
   :events [{:event :run-ends
             :req (req this-card-run)
             :effect (req (prevent-run-on-server state card (first (:server target)))
                          (when (:successful target)
                            (system-msg state :runner "gains 1 [Click] and adds Marathon to their grip")
                            (gain state :runner :click 1)
                            (move state :runner card :hand)
                            (unregister-events state side card)))}]})

(defcard "Mars for Martians"
  (letfn [(count-clan [state] (count (filter #(and (has-subtype? % "Clan") (resource? %))
                                             (all-active-installed state :runner))))]
    {:on-play
     {:msg (msg "draw " (count-clan state) " cards and gain " (count-tags state) " [Credits]")
      :async true
      :effect (req (wait-for (draw state side (count-clan state) nil)
                             (gain-credits state side eid (count-tags state))))}}))

(defcard "Mass Install"
  (letfn [(mhelper [n]
            (when (< n 3)
              {:async true
               :req (req (some #(and (program? %)
                                     (can-pay? state side (assoc eid :source card :source-type :runner-install) % nil
                                               [:credit (install-cost state side %)]))
                               (:hand runner)))
               :prompt "Select a program to install"
               :choices {:req (req (and (program? target)
                                        (in-hand? target)
                                        (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                                  [:credit (install-cost state side target)])))}
               :effect (req (wait-for (runner-install state side target nil)
                                      (continue-ability state side (mhelper (inc n)) card nil)))}))]
    {:on-play
     {:async true
      :req (req (some #(and (program? %)
                            (can-pay? state side (assoc eid :source card :source-type :runner-install) % nil
                                      [:credit (install-cost state side %)]))
                      (:hand runner)))
      :effect (effect (continue-ability (mhelper 0) card nil))}}))

(defcard "Mining Accident"
  {:on-play
   {:req (req (some #{:hq :rd :archives} (:successful-run runner-reg)))
    :rfg-instead-of-trashing true
    :msg "make the Corp pay 5 [Credits] or take 1 bad publicity"
    :waiting-prompt "Corp to choose an option"
    :player :corp
    :prompt "Pay 5 [Credits] or take 1 Bad Publicity?"
    :choices (req [(when (can-pay? state :corp eid card "Mining Accident" :credit 5)
                     "Pay 5 [Credits]")
                   "Take 1 Bad Publicity"])
    :async true
    :effect (req (if (= target "Pay 5 [Credits]")
                   (do (system-msg state side "pays 5 [Credits] from Mining Accident")
                       (lose-credits state :corp eid 5))
                   (do (system-msg state side "takes 1 bad publicity from Mining Accident")
                       (gain-bad-publicity state :corp 1)
                       (effect-completed state side eid))))}})

(defcard "Möbius"
  {:on-play
   {:req (req rd-runnable)
    :async true
    :effect (req (wait-for (make-run state side :rd card)
                           (let [card (get-card state card)]
                             (if (get-in card [:special :run-again])
                               (make-run state side eid :rd card)
                               (effect-completed state side eid)))))}
   :events [{:event :successful-run
             :req (req (and (get-in card [:special :run-again])
                            (= :rd (target-server context))))
             :msg "gain 4 [Credits]"
             :async true
             :effect (effect (gain-credits eid 4))}
            {:event :run-ends
             :interactive (req true)
             :optional {:req (req (and (:successful target)
                                       (not (get-in card [:special :run-again]))
                                       (= [:rd] (:server target))))
                        :prompt "Make another run on R&D?"
                        :yes-ability {:effect (effect (clear-wait-prompt :corp)
                                                      (update! (assoc-in card [:special :run-again] true)))}}}]})

(defcard "Modded"
  {:on-play
   {:prompt "Select a program or piece of hardware to install from your Grip"
    :choices {:req (req (and (or (hardware? target)
                                 (program? target))
                             (in-hand? target)
                             (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                       [:credit (install-cost state side target {:cost-bonus -3})])))}
    :async true
    :effect (effect (runner-install (assoc eid :source card :source-type :runner-install) target {:cost-bonus -3}))}})

(defcard "Moshing"
  {:on-play
   {:additional-cost [:trash-from-hand 3]
    :msg "draw 3 cards and gain 3 [Credits]"
    :async true
    :effect (req (wait-for (draw state side 3 nil)
                           (gain-credits state side eid 3)))}})

(defcard "Mutual Favor"
  {:on-play
   {:prompt "Choose an Icebreaker"
    :choices (req (cancellable (filter #(has-subtype? % "Icebreaker") (:deck runner)) :sorted))
    :msg (msg "add " (:title target) " to their grip and shuffle their stack")
    :async true
    :effect (effect (trigger-event :searched-stack nil)
                    (continue-ability
                      (let [icebreaker target]
                        (if (and (:successful-run runner-reg)
                                 (can-pay? state side (assoc eid :source card :source-type :runner-install) icebreaker nil
                                           [:credit (install-cost state side icebreaker)]))
                          {:optional
                           {:prompt "Do you want to install it?"
                            :yes-ability
                            {:async true
                             :msg (msg " install " (:title icebreaker))
                             :effect (req (runner-install state side (assoc eid :source card :source-type :runner-install) icebreaker nil)
                                          (shuffle! state side :deck))}
                            :no-ability
                            {:effect (req (move state side icebreaker :hand)
                                          (shuffle! state side :deck))}}}
                          {:effect (req (move state side icebreaker :hand)
                                        (shuffle! state side :deck))}))
                      card nil))}})

(defcard "Net Celebrity"
  {:recurring 1
   :interactions {:pay-credits {:req (req run)
                                :type :recurring}}})

(defcard "Networking"
  {:on-play
   {:async true
    :req (req (pos? (count-real-tags state)))
    :msg "remove 1 tag"
    :effect (req (wait-for (lose-tags state side 1)
                           (continue-ability
                             state side
                             {:optional
                              {:prompt "Pay 1 [Credits] to add Networking to Grip?"
                               :yes-ability
                               {:cost [:credit 1]
                                :msg "add it to their Grip"
                                :effect (effect (move card :hand))}}}
                             card nil)))}})

(defcard "Notoriety"
  {:on-play
   {:req (req (and (some #{:hq} (:successful-run runner-reg))
                   (some #{:rd} (:successful-run runner-reg))
                   (some #{:archives} (:successful-run runner-reg))))
    :msg "add it to their score area as an agenda worth 1 agenda point"
    :async true
    :effect (req (as-agenda state :runner eid (first (:play-area runner)) 1))}})

(defcard "Office Supplies"
  {:on-play
   {:play-cost-bonus (req (- (get-link state)))
    :prompt "Gain 4 [Credits] or draw 4 cards?"
    :choices ["Gain 4 [Credits]" "Draw 4 cards"]
    :msg (msg (if (= target "Gain 4 [Credits]")
                "gain 4 [Credits]"
                "draw 4 cards"))
    :async true
    :effect (req (if (= target "Gain 4 [Credits]")
                   (gain-credits state :runner eid 4)
                   (draw state :runner eid 4 nil)))}})

(defcard "On the Lam"
  {:on-play {:req (req (some resource? (all-active-installed state :runner)))
             :prompt "Choose a resource to host On the Lam"
             :choices {:card #(and (resource? %)
                                   (installed? %))}
             :effect (effect (host target (assoc card :installed true :condition true))
                             (card-init (find-latest state card) {:resolve-effect false})
                             (system-msg (str "hosts On the Lam on " (:title target))))}
   :interactions {:prevent [{:type #{:net :brain :meat :tag}
                             :req (req true)}]}
   :abilities [{:label "Avoid 3 tags"
                :msg "avoid up to 3 tags"
                :async true
                :cost [:trash]
                :effect (effect (tag-prevent :runner eid 3))}
               {:label "Prevent up to 3 damage"
                :msg "prevent up to 3 damage"
                :cost [:trash]
                :effect (effect (damage-prevent :net 3)
                                (damage-prevent :meat 3)
                                (damage-prevent :brain 3))}]})

(defcard "Out of the Ashes"
  (let [ashes-run {:prompt "Choose a server"
                   :choices (req runnable-servers)
                   :async true
                   :effect (effect (make-run eid target card))}
        ashes-recur (fn ashes-recur [n]
                      {:optional
                       {:req (req (not (zone-locked? state :runner :discard)))
                        :prompt "Remove Out of the Ashes from the game to make a run?"
                        :yes-ability
                        {:msg "removes Out of the Ashes from the game to make a run"
                         :effect
                         (req (let [card (some #(when (= "Out of the Ashes" (:title %)) %) (:discard runner))]
                                (move state side card :rfg)
                                (unregister-events state side card {:events [{:event :runner-phase-12}]})
                                (wait-for (resolve-ability state side ashes-run card nil)
                                          (if (< 1 n)
                                            (continue-ability state side (ashes-recur (dec n)) card nil)
                                            (effect-completed state side eid)))))}}})
        ashes-flag [{:event :runner-phase-12
                     :location :discard
                     :condition :in-discard
                     :once :per-turn
                     :once-key :out-of-ashes
                     :effect (effect (continue-ability
                                       (ashes-recur (count (filter #(= "Out of the Ashes" (:title %))
                                                                   (:discard runner))))
                                       card nil))}]]
    {:makes-run true
     :on-play {:prompt "Choose a server"
               :choices (req runnable-servers)
               :async true
               :effect (effect (make-run eid target card))}
     :move-zone (req (if (in-discard? card)
                       (register-events state side card ashes-flag)
                       (unregister-events state side card {:events [{:event :runner-phase-12}]})))}))

(defcard "Overclock"
  {:makes-run true
   :data {:counter {:credit 5}}
   :interactions {:pay-credits {:type :credit}}
   :on-play {:prompt "Choose a server"
             :choices (req runnable-servers)
             :async true
             :effect (effect (make-run eid target card))}})

(defcard "Paper Tripping"
  {:on-play
   {:req (req (pos? (count-real-tags state)))
    :msg "remove all tags"
    :async true
    :effect (effect (lose-tags eid :all))}})

(defcard "Peace in Our Time"
  {:on-play
   {:req (req (not (:scored-agenda corp-reg-last)))
    :msg "gain 10 [Credits]. The Corp gains 5 [Credits]"
    :async true
    :effect (req (wait-for (gain-credits state :runner 10)
                           (register-turn-flag! state side card :can-run nil)
                           (gain-credits state :corp eid 5)))}})

(defcard "Planned Assault"
  {:on-play
   {:prompt "Choose a Run event"
    :choices (req (sort-by :title
                           (filter #(and (has-subtype? % "Run")
                                         (can-pay? state side (assoc eid :source card :source-type :play) % nil
                                                   [:credit (play-cost state side %)]))
                                   (:deck runner))))
    :msg (msg "play " (:title target))
    :async true
    :effect (effect (trigger-event :searched-stack nil)
                    (shuffle! :deck)
                    (play-instant eid target {:no-additional-cost true}))}})

(defcard "Political Graffiti"
  {:makes-run true
   :on-play {:req (req archives-runnable)
             :async true
             :effect (effect (make-run eid :archives card))}
   :constant-effects [{:type :agenda-value
                       :req (req (same-card? (:host card) target))
                       :value -1}]
   :events [{:event :purge
             :async true
             :effect (req (wait-for (trash state side card {:cause :purge})
                                    (update-all-agenda-points state side)
                                    (effect-completed state side eid)))}
            (successful-run-replace-access
              {:target-server :archives
               :this-card-run true
               :mandatory true
               :ability
               {:prompt "Select an agenda to host Political Graffiti"
                :choices {:req (req (in-corp-scored? state side target))}
                :msg (msg "host Political Graffiti on " (:title target) " as a hosted condition counter")
                :effect (effect (host :runner (get-card state target) (assoc card :installed true :seen true :condition true))
                                (update-all-agenda-points))}})]})

(defcard "Populist Rally"
  {:on-play {:req (req (seq (filter #(has-subtype? % "Seedy") (all-active-installed state :runner))))
             :msg "give the Corp 1 fewer [Click] to spend on their next turn"
             :effect (effect (lose :corp :click-per-turn 1))}
   :events [{:event :corp-turn-ends
             :duration :until-corp-turn-ends
             :effect (effect (gain :corp :click-per-turn 1))}]})

(defcard "Power Nap"
  {:on-play
   {:async true
    :msg (msg "gain " (+ 2 (count (filter #(has-subtype? % "Double") (:discard runner)))) " [Credits]")
    :effect (effect (gain-credits eid (+ 2 (count (filter #(has-subtype? % "Double")
                                                          (:discard runner))))))}})

(defcard "Power to the People"
  {:events [{:event :pre-steal-cost
             :duration :end-of-turn
             :once :per-turn
             :msg "gain 7 [Credits]"
             :async true
             :effect (effect (gain-credits eid 7))}]})

(defcard "Prey"
  {:makes-run true
   :on-play {:prompt "Choose a server:"
             :choices (req runnable-servers)
             :async true
             :effect (effect (make-run eid target card))}
   :events [{:event :pass-ice
             :req (req (and (rezzed? (:ice context))
                            (not-used-once? state {:once :per-run} card)
                            (<= (get-strength (:ice context)) (count (all-installed state :runner)))))
             :async true
             :effect
             (effect
               (continue-ability
                 (let [ice (:ice context)]
                   (if (pos? (get-strength ice))
                     {:optional
                      {:prompt (str "Use Prey to trash " (quantify (get-strength ice) "card")
                                    " to trash " (:title ice) "?")
                       :once :per-run
                       :yes-ability
                       {:async true
                        :cost [:installed (get-strength ice)]
                        :msg (msg "trash " (card-str state ice))
                        :effect (effect (trash eid ice nil))}}}
                     {:optional
                      {:prompt (str "Use Prey to trash " (:title ice) "?")
                       :once :per-run
                       :yes-ability
                       {:async true
                        :msg (msg "trash " (card-str state ice))
                        :effect (effect (trash eid ice nil))}}}))
                 card nil))}]})

(defcard "Process Automation"
  {:on-play
   {:msg "gain 2 [Credits] and draw 1 card"
    :async true
    :effect (req (wait-for (gain-credits state side 2)
                           (draw state side eid 1 nil)))}})

(defcard "Push Your Luck"
  (letfn [(corp-choice [spent]
            {:player :corp
             :waiting-prompt "Corp to choose an option"
             :prompt "Even or odd?"
             :choices ["Even" "Odd"]
             :async true
             :effect (req (let [correct-guess ((if (= target "Even") even? odd?) spent)]
                            (wait-for
                              (lose-credits state :runner spent)
                              (system-msg state :runner (str "spends " spent " [Credit]"))
                              (system-msg state :corp (str (if correct-guess " " " in")
                                                           "correctly guesses " (string/lower-case target)))
                              (wait-for
                                (trigger-event-simult state side :reveal-spent-credits nil nil spent)
                                (if correct-guess
                                  (effect-completed state side eid)
                                  (do (system-msg state :runner (str "gains " (* 2 spent) " [Credits]"))
                                      (gain-credits state :runner eid (* 2 spent))))))))})
          (runner-choice [choices]
            {:player :runner
             :prompt "How many credits do you want to spend?"
             :waiting-prompt "Runner to make a decision"
             :choices choices
             :async true
             :effect (effect (continue-ability :corp (corp-choice (str->int target)) card nil))})]
    {:on-play
     {:async true
      :effect (req (let [all-amounts (range (inc (get-in @state [:runner :credit])))
                         valid-amounts (remove #(or (any-flag-fn? state :corp :prevent-secretly-spend %)
                                                    (any-flag-fn? state :runner :prevent-secretly-spend %))
                                               all-amounts)
                         choices (map str valid-amounts)]
                     (continue-ability state side (runner-choice choices) card nil)))}}))

(defcard "Pushing the Envelope"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :choices (req runnable-servers)
             :msg (msg (if (<= (count (:hand runner)) 2)
                         "make a run, and adds +2 strength to installed icebreakers"
                         "make a run"))
             :async true
             :effect (req (when (<= (count (:hand runner)) 2)
                            (pump-all-icebreakers state side 2 :end-of-run))
                          (make-run state side eid target))}})

(defcard "Quality Time"
  {:on-play
   {:msg "draw 5 cards"
    :async true
    :effect (effect (draw eid 5 nil))}})

(defcard "Queen's Gambit"
  {:on-play
   {:choices ["0" "1" "2" "3"]
    :prompt "How many advancement tokens?"
    :async true
    :effect (effect
              (continue-ability
                (let [c (str->int target)]
                  {:choices {:card #(and (is-remote? (second (get-zone %)))
                                         (= (last (get-zone %)) :content)
                                         (not (:rezzed %)))}
                   :msg (msg "add " c " advancement tokens on a card and gain " (* 2 c) " [Credits]")
                   :async true
                   :effect (req (wait-for (gain-credits state side (* 2 c))
                                          (add-prop state :corp target :advance-counter c {:placed true})
                                          (register-turn-flag!
                                            state side
                                            card :can-access
                                            ;; prevent access of advanced card
                                            (fn [_ _ card] (not (same-card? target card))))
                                          (effect-completed state side eid)))})
                card nil))}})

(defcard "Quest Completed"
  {:on-play
   {:req (req (and (some #{:hq} (:successful-run runner-reg))
                   (some #{:rd} (:successful-run runner-reg))
                   (some #{:archives} (:successful-run runner-reg))))
    :choices {:card installed?}
    :msg (msg "access " (:title target))
    :async true
    :effect (effect (access-card eid target))}})

(defcard "Rebirth"
  {:on-play
   {:prompt "Choose an identity to become"
    :rfg-instead-of-trashing true
    :choices (req (let [is-draft-id? #(.startsWith (:code %) "00")
                        runner-identity (:identity runner)
                        is-swappable #(and (= "Identity" (:type %))
                                           (= (:faction runner-identity) (:faction %))
                                           (not (is-draft-id? %))
                                           (not= (:title runner-identity) (:title %)))
                        swappable-ids (filter is-swappable (server-cards))]
                    (cancellable swappable-ids :sorted)))
    :msg "change identities"
    :effect (req (let [old-runner-identity (:identity runner)]
                   ;; Handle hosted cards (Ayla) - Part 1
                   (doseq [c (:hosted old-runner-identity)]
                     (move state side c :temp-hosted))
                   (disable-identity state side)
                   ;; Move the selected ID to [:runner :identity] and set the zone
                   (let [new-id (-> target :title server-card make-card (assoc :zone [:identity]))
                         num-old-blanks (:num-disabled old-runner-identity)]
                     (swap! state assoc-in [side :identity] new-id)
                     (card-init state side new-id)
                     (when num-old-blanks
                       (dotimes [_ num-old-blanks]
                         (disable-identity state side)))))
                 ;; Handle hosted cards (Ayla) - Part 2
                 (doseq [c (get-in @state [:runner :temp-hosted])]
                   ;; Currently assumes all hosted cards are hosted facedown (Ayla)
                   (host state side (get-in @state [:runner :identity]) c {:facedown true})))}})

(defcard "Reboot"
  (letfn [(install-cards [state side eid card to-install titles]
            (if-let [f (first to-install)]
              (wait-for (runner-install state :runner f {:facedown true :no-msg true})
                        (install-cards state side eid card (rest to-install) titles))
              (do
                (move state side (find-latest state card) :rfg)
                (system-msg state :runner (str "uses Reboot to install " (string/join ", " titles) " facedown"))
                (effect-completed state side eid))))]
    {:makes-run true
     :on-play {:rfg-instead-of-trashing true
               :req (req archives-runnable)
               :async true
               :effect (effect (make-run eid :archives card))}
     :events [(successful-run-replace-access
                {:target-server :archives
                 :this-card-run true
                 :mandatory true
                 :ability
                 {:req (req (not (zone-locked? state :runner :discard)))
                  :async true
                  :prompt "Choose up to five cards to install"
                  :show-discard true
                  :choices {:max 5
                            :card #(and (in-discard? %)
                                        (runner? %))}
                  :effect (effect (install-cards eid card targets (map :title targets)))}})]}))

(defcard "Recon"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :choices (req runnable-servers)
             :async true
             :effect (effect (make-run eid target card))}
   :events [{:event :encounter-ice
             :optional
             {:prompt "Jack out?"
              :req (req (first-run-event? state side :encounter-ice))
              :yes-ability {:async true
                            :msg "jack out"
                            :effect (effect (jack-out eid))}}}]})

(defcard "Rejig"
  (let [valid-target? (fn [card] (and (runner? card)
                                      (or (program? card)
                                          (hardware? card))))
        pick-up {:async true
                 :prompt "Select a program or piece of hardware to add to your Grip"
                 :choices {:card #(and (valid-target? %)
                                       (installed? %))}
                 :effect (req (move state side target :hand)
                              (effect-completed state side (make-result eid (:cost target))))}
        put-down (fn [bonus]
                   {:async true
                    :prompt "Select a program or piece of hardware to install"
                    :choices
                    {:req (req (and (valid-target? target)
                                    (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                              [:credit (install-cost state side target
                                                                     {:cost-bonus (- bonus)})])))}
                    :effect (effect (runner-install (assoc eid :source card :source-type :runner-install)
                                                    target {:cost-bonus (- bonus)}))})]
    {:on-play
     {:req (req (some valid-target? (all-installed state :runner)))
      :effect (req (wait-for (resolve-ability state side pick-up card nil)
                             (continue-ability state side
                                               (put-down async-result)
                                               card nil)))}}))

(defcard "Reshape"
  {:on-play
   {:prompt "Select two pieces of unrezzed ice to swap positions"
    :choices {:card #(and (installed? %)
                          (not (rezzed? %))
                          (ice? %))
              :max 2
              :all true}
    :msg (msg "swap the positions of " (card-str state (first targets))
              " and " (card-str state (second targets)))
    :effect (effect (swap-ice (first targets) (second targets)))}})

(defcard "Retrieval Run"
  {:makes-run true
   :on-play {:req (req archives-runnable)
             :async true
             :effect (effect (make-run eid :archives card))}
   :events [(successful-run-replace-access
              {:target-server :archives
               :this-card-run true
               :ability
               {:async true
               :req (req (not (zone-locked? state :runner :discard)))
                :prompt "Choose a program to install"
                :msg (msg "install " (:title target))
                :choices (req (filter program? (:discard runner)))
                :effect (effect (runner-install (assoc eid :source card :source-type :runner-install)
                                                target {:ignore-all-cost true}))}})]})

(defcard "Rigged Results"
  (letfn [(choose-ice []
            {:player :runner
             :waiting-prompt "Runner to make a decision"
             :prompt "Select a piece of ice to bypass"
             :choices {:card ice?}
             :msg (msg "make a run and bypass " (card-str state target))
             :async true
             :effect (effect (register-events
                               card
                               (let [target-ice target]
                                 [{:event :encounter-ice
                                   :req (req (same-card? target-ice (:ice context)))
                                   :msg (msg "bypass " (:title (:ice context)))
                                   :effect (req (bypass-ice state))}]))
                             (make-run eid (second (get-zone target)) card))})
          (corp-choice [choices spent]
            {:player :corp
             :waiting-prompt "Corp to make a decision"
             :prompt "How many credits were spent?"
             :choices choices
             :async true
             :effect (req (wait-for
                            (lose-credits state :runner spent)
                            (system-msg state :runner (str "spends " spent " [Credit]"))
                            (system-msg state :corp (str " guesses " target " [Credit]"))
                            (wait-for (trigger-event-simult state side :reveal-spent-credits nil nil spent)
                                      (if (not= spent (str->int target))
                                        (continue-ability state :runner (choose-ice) card nil)
                                        (effect-completed state side eid)))))})
          (runner-choice [choices]
            {:player :runner
             :waiting-prompt "Runner to make a decision"
             :prompt "How many credits do you want to spend?"
             :choices choices
             :async true
             :effect (effect (continue-ability (corp-choice choices (str->int target)) card nil))})]
    {:on-play
     {:async true
      :effect (req (let [all-amounts (range (min 3 (inc (get-in @state [:runner :credit]))))
                         valid-amounts (remove #(or (any-flag-fn? state :corp :prevent-secretly-spend %)
                                                    (any-flag-fn? state :runner :prevent-secretly-spend %))
                                               all-amounts)
                         choices (map str valid-amounts)]
                     (continue-ability state side (runner-choice choices) card nil)))}}))

(defcard "Rip Deal"
  {:makes-run true
   :on-play {:rfg-instead-of-trashing true
             :req (req hq-runnable)
             :async true
             :effect (effect (make-run eid :hq card))}
   :events [(successful-run-replace-access
              {:target-server :hq
               :this-card-run true
               :mandatory true
               :ability
               {:async true
                :req (req (not (zone-locked? state :runner :discard)))
                :effect
                (req (wait-for
                       ;; todo: remove this when replacement effects are fixed
                       (trigger-event-sync state side :pre-access :hq)
                       (continue-ability
                         state side
                         (let [n (min (-> corp :hand count) (:base (num-cards-to-access state side :hq nil)))
                               heap (count (:discard runner))]
                           (if (pos? heap)
                             {:show-discard true
                              :prompt (str "Choose " (quantify (min n heap) "card")
                                           " to move from the Heap to your Grip")
                              :async true
                              :msg (msg "take " (string/join ", " (map :title targets))
                                        " from their Heap to their Grip")
                              :choices {:max (min n heap)
                                        :all true
                                        :card #(and (runner? %)
                                                    (in-discard? %))}
                              :effect (req (doseq [c targets]
                                             (move state side c :hand))
                                           (effect-completed state side eid))}
                             {:async true
                              :msg (msg "take no cards from their Heap to their Grip")
                              :effect (req (effect-completed state side eid))}))
                         card nil)))}})]})

(defcard "Rumor Mill"
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
     :on-play {:effect (req (doseq [c (rumor state)]
                              (disable-card state :corp c)))}
     :events [{:event :corp-install
               :req (req (eligible? (:card context)))
               :effect (effect (disable-card :corp (:card context)))}]}))

(defcard "Run Amok"
  (letfn [(get-rezzed-cids [ice]
            (map :cid (filter #(and (rezzed? %)
                                    (ice? %))
                              ice)))]
    {:makes-run true
     :on-play {:prompt "Choose a server"
               :choices (req runnable-servers)
               :async true
               :effect (effect (update! (assoc-in card [:special :run-amok] (get-rezzed-cids (all-installed state :corp))))
                         (make-run eid target (get-card state card)))}
     :events [{:event :run-ends
               :req (req this-card-run)
               :async true
               :effect (req (let [new (set (get-rezzed-cids (all-installed state :corp)))
                                  old (set (get-in (get-card state card) [:special :run-amok]))
                                  diff-cid (seq (clojure.set/difference new old))
                                  diff (map #(find-cid % (all-installed state :corp)) diff-cid)]
                              (continue-ability
                                state :runner
                                (when (seq diff)
                                  {:async true
                                   :prompt "Select an ice to trash"
                                   :choices {:card #(some (partial same-card? %) diff)
                                             :all true}
                                   :effect (effect (trash eid target nil))})
                                card nil)))}]}))

(defcard "Running Interference"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :choices (req runnable-servers)
             :async true
             :effect (effect (register-floating-effect
                               card
                               {:type :rez-additional-cost
                                :duration :end-of-run
                                :req (req (ice? target))
                                :value (req [:credit (:cost target)])})
                             (make-run eid target card))}})

(defcard "Satellite Uplink"
  {:on-play
   {:req (req (some #(not (rezzed? %)) (all-installed state :corp)))
    :choices {:max 2
              :card #(and (corp? %)
                          (installed? %)
                          (not (rezzed? %)))}
    :async true
    :effect (req (if (pos? (count targets))
                   (wait-for (expose state side target)
                             (if (= 2 (count targets))
                               (expose state side eid (second targets))
                               (effect-completed state side eid)))
                   (effect-completed state side eid)))}})

(defcard "Scavenge"
  {:on-play
   {:req (req (some #(and (program? %)
                          (installed? %))
                    (all-active-installed state :runner)))
    :prompt "Select an installed program to trash"
    :choices {:card #(and (program? %)
                          (installed? %))}
    :async true
    :effect (req (let [trashed target
                       tcost (:cost trashed)]
                   (wait-for
                     (trash state side target {:unpreventable true})
                     (continue-ability
                       state side
                       {:async true
                        :prompt (if (not (zone-locked? state :runner :discard))
                                  "Select a program to install from your Grip or Heap"
                                  "Select a program to install from your Grip")
                        :show-discard  (not (zone-locked? state :runner :discard))
                        :choices
                        {:req (req (and (program? target)
                                        (or (in-hand? target)
                                            (and (in-discard? target)
                                                 (not (zone-locked? state :runner :discard))))
                                        (can-pay? state side (assoc eid :source card :source-type :runner-install) target nil
                                                  [:credit (install-cost state side target
                                                                         {:cost-bonus (- tcost)})])))}
                        :msg (msg "trash " (:title trashed)
                                  " and install " (:title target)
                                  ", lowering the cost by " tcost " [Credits]")
                        :effect (effect (runner-install (assoc eid :source card :source-type :runner-install)
                                                        target {:cost-bonus (- tcost)}))}
                       card nil))))}})

(defcard "Scrubbed"
  {:events [{:event :encounter-ice
             :once :per-turn
             :effect (effect
                       (register-floating-effect
                         card
                         (let [target-ice (:ice context)]
                           {:type :ice-strength
                            :duration :end-of-run
                            :req (req (same-card? target target-ice))
                            :value -2}))
                       (update-all-ice))}]})

(defcard "Showing Off"
  {:makes-run true
   :on-play {:req (req rd-runnable)
             :async true
             :effect (effect (make-run eid :rd card))}
   :events [(successful-run-replace-access
              {:target-server :rd
               :this-card-run true
               :can-access true
               :mandatory true
               :ability {:msg "access cards from the bottom of R&D"
                         :async true
                         :effect (effect (do-access eid (:server run)))}})
            {:event :pre-access
             :silent (req true)
             :effect (req (swap! state assoc-in [:runner :rd-access-fn] reverse))}
            {:event :run-ends
             :effect (req (swap! state assoc-in [:runner :rd-access-fn] seq))}]})

(defcard "Singularity"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :choices (req (filter #(can-run-server? state %) remotes))
             :async true
             :effect (effect (make-run eid target card))}
   :events [(successful-run-replace-access
              {:target-server :remote
               :this-card-run true
               :mandatory true
               :ability
               {:async true
                :msg "trash all cards in the server at no cost"
                :effect (effect (trash-cards eid (:content run-server)))}})]})

(defcard "Social Engineering"
  {:on-play
   {:prompt "Select an unrezzed piece of ice"
    :choices {:card #(and (not (rezzed? %))
                          (installed? %)
                          (ice? %))}
    :msg (msg "select " (card-str state target))
    :effect (effect
              (register-events
                card
                (let [ice target]
                  [{:event :rez
                    :duration :end-of-turn
                    :req (req (same-card? (:card context) ice))
                    :msg (msg "gain " (rez-cost state side (get-card state (:card context))) " [Credits]")
                    :async true
                    :effect (effect (gain-credits :runner eid (rez-cost state side (get-card state (:card context)))))}])))}})

(defcard "Spear Phishing"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :choices (req runnable-servers)
             :async true
             :effect (effect (make-run eid target card))}
   :events [{:event :encounter-ice
             :req (req (= 1 run-position))
             :msg (msg "bypass " (:title (:ice context)))
             :effect (req (bypass-ice state))}]})

(defcard "Spec Work"
  {:on-play
   {:additional-cost [:program 1]
    :msg "gain 4 [Credits] and draw 2 cards"
    :async true
    :effect (req (wait-for (gain-credits state side 4)
                           (draw state side eid 2 nil)))}})

(defcard "Special Order"
  {:on-play
   {:prompt "Choose an Icebreaker"
    :choices (req (cancellable (filter #(has-subtype? % "Icebreaker") (:deck runner)) :sorted))
    :msg (msg "add " (:title target) " to their grip and shuffle their stack")
    :effect (effect (trigger-event :searched-stack nil)
                    (shuffle! :deck)
                    (move target :hand))}})

(defcard "Spooned"
  (cutlery "Code Gate"))

(defcard "Spot the Prey"
  {:makes-run true
   :on-play
   {:prompt "Select 1 non-ice card to expose"
    :msg "expose 1 card and make a run"
    :choices {:card #(and (installed? %)
                          (not (ice? %))
                          (corp? %))}
    :async true
    :effect (req (wait-for (expose state side target)
                           (continue-ability
                             state side
                             {:prompt "Choose a server"
                              :choices (req runnable-servers)
                              :async true
                              :effect (effect (make-run eid target))}
                             card nil)))}})

(defcard "Stimhack"
  {:makes-run true
   :on-play {:prompt "Choose a server"
             :choices (req runnable-servers)
             :async true
             :effect (effect (gain-next-run-credits 9)
                             (make-run eid target card))}
   :events [{:event :run-ends
             :req (req this-card-run)
             :msg "take 1 brain damage"
             :effect (effect (damage eid :brain 1 {:unpreventable true
                                                   :card card}))}]})

(defcard "Sure Gamble"
  {:on-play
   {:msg "gain 9 [Credits]"
    :async true
    :effect (effect (gain-credits eid 9))}})

(defcard "Surge"
  (letfn [(placed-virus-cards [state]
            (->> (turn-events state :runner :counter-added)
                 (filter #(= :virus (:counter-type (second %))))
                 (map first)
                 (keep #(get-card state %))
                 (seq)))]
    {:on-play
     {:req (req (placed-virus-cards state))
      :choices {:req (req (some #(same-card? % target) (placed-virus-cards state)))}
      :msg (msg "place 2 virus tokens on " (:title target))
      :effect (effect (add-counter :runner target :virus 2))}}))

(defcard "SYN Attack"
  {:on-play
   {:player :corp
    :waiting-prompt "Corp to choose an option"
    :prompt "Discard 2 cards or draw 4 cards?"
    :choices (req [(when (<= 2 (count (:hand corp)))
                     "Discard 2")
                   "Draw 4"])
    :async true
    :effect (req (if (= target "Draw 4")
                   (wait-for (draw state :corp 4 nil)
                             (system-msg state :corp
                                         (str "uses SYN Attack to draw "
                                              (quantify (count async-result) "card")))
                             (effect-completed state side eid))
                   (continue-ability
                     state :corp
                     {:prompt "Choose 2 cards to discard"
                      :choices {:max 2
                                :all true
                                :card #(and (in-hand? %)
                                            (corp? %))}
                      :async true
                      :effect (effect (system-msg :runner
                                                  (str "uses SYN Attack to force the "
                                                       "Corp to trash 2 cards from HQ"))
                                      (trash-cards :corp eid targets {:unpreventable true}))}
                     card nil)))}})

(defcard "System Outage"
  {:events [{:event :corp-draw
             :req (req (not (first-event? state side :corp-draw)))
             :msg "force the Corp to lose 1 [Credits]"
             :async true
             :effect (effect (lose-credits :corp eid 1))}]})

(defcard "System Seizure"
  (let [ability {:req (req (get-in card [:special :ss-target]))
                 :effect (effect (update! (dissoc-in card [:special :ss-target])))}]
    {:events [{:event :pump-breaker
               :req (req (or (not (get-in card [:special :ss-target]))
                             (same-card? target (get-in card [:special :ss-target]))))
               :effect (req (when-not (get-in card [:special :ss-target])
                              (update! state side (assoc-in card [:special :ss-target] target)))
                            (let [new-pump (assoc (second targets) :duration :end-of-run)]
                              (swap! state assoc :effects
                                     (->> (:effects @state)
                                          (remove #(= (:uuid %) (:uuid new-pump)))
                                          (#(conj % new-pump))
                                          (into []))))
                            (update-breaker-strength state side target))}
              (assoc ability :event :corp-turn-ends)
              (assoc ability :event :runner-turn-ends)]}))

(defcard "Test Run"
  {:on-play
   {:prompt (req (if (not (zone-locked? state :runner :discard))
                   "Install a program from your Stack or Heap?"
                   "Install a program from your Stack?" ))
    :choices (req (if (not (zone-locked? state :runner :discard))
                    ["Stack" "Heap"]
                    ["Stack"]))
    :msg (msg "install a program from their " target)
    :async true
    :effect (effect
              (continue-ability
                (let [where target]
                  {:prompt "Choose a program to install"
                   :choices (req (cancellable
                                   (filter program? ((if (= where "Heap") :discard :deck) runner))))
                   :async true
                   :effect (req (trigger-event state side :searched-stack nil)
                                (shuffle! state side :deck)
                                (wait-for (runner-install state side (make-eid state {:source card :source-type :runner-install})
                                                          target {:ignore-all-cost true})
                                          (if async-result
                                            (let [installed-card (update! state side (assoc-in async-result [:special :test-run] true))]
                                              (register-events
                                                state side installed-card
                                                [{:event :runner-turn-ends
                                                  :duration :end-of-turn
                                                  :req (req (get-in (find-latest state installed-card) [:special :test-run]))
                                                  :msg (msg "move " (:title installed-card) " to the top of the stack")
                                                  :effect (effect (move (find-latest state installed-card) :deck {:front true}))}])
                                              (effect-completed state side eid))
                                            (effect-completed state side eid))))})
                card nil))}})

(defcard "The Maker's Eye"
  {:makes-run true
   :on-play {:req (req rd-runnable)
             :async true
             :effect (effect (make-run eid :rd card))}
   :events [{:event :successful-run
             :silent (req true)
             :req (req (and (= :rd (target-server context))
                            this-card-run))
             :effect (effect (access-bonus :rd 2))}]})

(defcard "The Noble Path"
  {:makes-run true
   :on-play {:async true
             :effect (req (wait-for
                            (trash-cards state side (:hand runner))
                            (continue-ability
                              state side
                              {:async true
                               :prompt "Choose a server"
                               :choices (req runnable-servers)
                               :msg (msg "trash their grip and make a run on " target
                                         ", preventing all damage")
                               :effect (effect (make-run eid target card))}
                              card nil)))}
   :events [{:event :pre-damage
             :duration :end-of-run
             :effect (effect (damage-prevent :net Integer/MAX_VALUE)
                             (damage-prevent :meat Integer/MAX_VALUE)
                             (damage-prevent :brain Integer/MAX_VALUE))}]})

(defcard "The Price of Freedom"
  {:on-play {:additional-cost [:connection 1]
             :rfg-instead-of-trashing true
             :msg "prevent the Corp from advancing cards during their next turn"}
   :events [{:event :corp-turn-begins
             :duration :until-runner-turn-begins
             :effect (effect (register-turn-flag!
                               card :can-advance
                               (fn [state side card]
                                 ((constantly false)
                                  (toast state :corp "Cannot advance cards this turn due to The Price of Freedom." "warning")))))}]})

(defcard "Three Steps Ahead"
  {:on-play
   {:effect (effect (register-events
                      card
                      [{:event :runner-turn-ends
                        :duration :end-of-turn
                        :unregister-once-resolved true
                        :msg (msg "gain " (* 2 (count (:successful-run runner-reg))) " [Credits]")
                        :async true
                        :effect (effect (gain-credits eid (* 2 (count (:successful-run runner-reg)))))}]))}})

(defcard "Tinkering"
  {:on-play
   {:req (req (some #(and (ice? %)
                          (installed? %))
                    (all-installed state :corp)))
    :prompt "Select a piece of ice"
    :choices {:card #(and (installed? %)
                          (ice? %))}
    :msg (msg "make " (card-str state target) " gain Sentry, Code Gate, and Barrier until the end of the turn")
    :effect (effect (register-floating-effect
                      card
                      (let [ice target]
                        {:type :gain-subtype
                         :duration :end-of-turn
                         :req (req (same-card? ice target))
                         :value ["Sentry" "Code Gate" "Barrier"]}))
                    (add-icon card (get-card state target) "T" "green"))}})

(defcard "Trade-In"
  ;; TODO: look at me plz
  (letfn [(trashed-hw [state] (last (get-in @state [:runner :discard])))]
    {:on-play
     {:additional-cost [:hardware 1]
      :msg (msg (let [{:keys [title cost]} (trashed-hw state)]
                  (str "trash " title " and gain " (quot cost 2) " [Credits]")))
      :async true
      :effect (req (let [{:keys [cost]} (trashed-hw state)]
                     (wait-for (gain-credits state :runner (quot cost 2))
                               (continue-ability
                                 state :runner
                                 {:prompt "Choose a Hardware to add to your Grip from your Stack"
                                  :choices (req (filter hardware?
                                                        (:deck runner)))
                                  :msg (msg "add " (:title target) " to their Grip (and shuffle their Stack)")
                                  :effect (effect (trigger-event :searched-stack nil)
                                                  (shuffle! :deck)
                                                  (move target :hand))}
                                 card nil))))}}))

(defcard "Traffic Jam"
  {:constant-effects [{:type :advancement-requirement
                       :value (req (->> (:scored corp)
                                        (filter #(= (:title %) (:title target)))
                                        (count)))}]})

(defcard "Tread Lightly"
  {:on-play
   {:prompt "Choose a server"
    :choices (req runnable-servers)
    :makes-run true
    :async true
    :effect (effect (register-floating-effect
                      card
                      {:type :rez-additional-cost
                       :duration :end-of-run
                       :req (req (ice? target))
                       :value (req [:credit 3])})
                    (make-run eid target card))}})

(defcard "Uninstall"
  {:on-play
   {:req (req (some #(or (hardware? %)
                         (program? %))
                    (all-active-installed state :runner)))
    :choices {:card #(and (installed? %)
                          (not (facedown? %))
                          (or (hardware? %)
                              (program? %)))}
    :msg (msg "move " (:title target) " to their Grip")
    :effect (effect (move target :hand))}})

(defcard "Unscheduled Maintenance"
  {:events [{:event :corp-install
             :req (req (ice? (:card context)))
             :effect (effect (register-turn-flag!
                               card :can-install-ice
                               (fn [state side card]
                                 (if (ice? card)
                                   ((constantly false)
                                    (toast state :corp "Cannot install ice the rest of this turn due to Unscheduled Maintenance"))
                                   true))))}]
   :leave-play (effect (clear-turn-flag! card :can-install-ice))})

(defcard "Vamp"
  {:makes-run true
   :on-play {:req (req hq-runnable)
             :async true
             :effect (effect (make-run eid :hq card))}
   :events [(successful-run-replace-access
              {:target-server :hq
               :this-card-run true
               :ability
               {:async true
                :prompt "How many [Credits]?"
                :choices :credit
                :msg (msg "take 1 tag and make the Corp lose " target " [Credits]")
                :effect (req (wait-for (lose-credits state :corp target)
                                       (gain-tags state side eid 1)))}} )]})

(defcard "VRcation"
  {:on-play
   {:msg (msg "draw 4 cards"
              (when (pos? (:click runner))
                " and lose [Click]"))
    :async true
    :effect (req (when (pos? (:click runner))
                   (lose state :runner :click 1))
                 (draw state :runner eid 4 nil))}})

(defcard "Wanton Destruction"
  {:makes-run true
   :on-play {:req (req hq-runnable)
             :async true
             :effect (effect (make-run eid :hq card))}
   :events [(successful-run-replace-access
              {:target-server :hq
               :this-card-run true
               :ability
               {:msg (msg "force the Corp to discard " target " cards from HQ at random")
                :prompt "How many [Click] do you want to spend?"
                :choices (req (map str (range 0 (inc (:click runner)))))
                :async true
                :effect (req (let [n (str->int target)]
                               (wait-for (pay state :runner card :click n)
                                         (system-msg state :runner (:msg async-result))
                                         (trash-cards state :corp eid (take n (shuffle (:hand corp)))))))}})]})

(defcard "Watch the World Burn"
  (letfn [(rfg-card-event [burned-card]
            [{:event :pre-access-card
              :duration :end-of-game
              :req (req (same-card? :title burned-card target))
              :msg (msg (str "remove " (:title burned-card) " from the game"))
              :effect (effect (move :corp target :rfg))}])]
    {:makes-run true
     :on-play {:prompt "Choose a server"
               :choices (req (filter #(can-run-server? state %) remotes))
               :async true
               :effect (effect (make-run eid target card))}
     :events [{:event :pre-access-card
               :req (req (and (not (agenda? target))
                              (:successful run)))
               :once :per-run
               :msg (msg "remove " (:title target) " from the game, and watch for other copies of " (:title target) " to burn")
               :effect (effect (move :corp target :rfg)
                         (register-events card (rfg-card-event target)))}]}))

(defcard "White Hat"
  (letfn [(finish-choice [choices]
            (let [choices (filter #(not= "None" %) choices)]
              (when (not-empty choices)
                {:effect (req (doseq [c choices]
                                (move state :corp c :deck))
                              (shuffle! state :corp :deck))
                 :msg (str "shuffle " (string/join ", " (map :title choices)) " into R&D")})))
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
    {:on-play
     {:trace
      {:base 3
       :req (req (some #{:hq :rd :archives} (:successful-run runner-reg)))
       :unsuccessful
       {:async true
        :msg (msg "reveal all cards in HQ" (when-let [hand (seq (:hand corp))]
                                             (str ": " (string/join ", " (map :title hand)))))
        :effect (req (wait-for
                       (reveal state side (:hand corp))
                       (continue-ability state :runner (choose-cards (set (:hand corp)) #{}) card nil)))}}}}))

(defcard "Wildcat Strike"
  {:on-play
   {:player :corp
    :waiting-prompt "Corp to choose an option"
    :prompt "Choose one"
    :choices ["Runner gains 6 [Credits]" "Runner draws 4 cards"]
    :async true
    :effect (req (if (= target "Runner gains 6 [Credits]")
                   (do (system-msg state :corp "chooses 6 [Credits] for the Runner")
                       (gain-credits state :runner eid 6))
                   (do (system-msg state :corp "chooses 4 cards for the Runner")
                       (draw state :runner eid 4 nil))))}})

(defcard "Windfall"
  {:on-play
   {:async true
    :effect (req (shuffle! state side :deck)
                 (let [topcard (first (:deck (:runner @state)))
                       cost (:cost topcard)]
                   (wait-for (trash state side topcard nil)
                             (wait-for (gain-credits state side (if (event? topcard) 0 cost))
                                       (system-msg state side
                                                   (str "shuffles their Stack and trashes " (:title topcard)
                                                        (when-not (event? topcard)
                                                          (str " to gain " cost " [Credits]"))))
                                       (effect-completed state side eid)))))}})
