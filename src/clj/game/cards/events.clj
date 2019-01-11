(ns game.cards.events
  (:require [game.core :refer :all]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability]]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [clojure.stacktrace :refer [print-stack-trace]]
            [jinteki.utils :refer [str->int other-side is-tagged? count-tags has-subtype?]]
            [jinteki.cards :refer [all-cards]]))

(defn- run-event
  ([] (run-event nil))
  ([run-ability] (run-event nil run-ability))
  ([cdef run-ability] (run-event cdef run-ability nil))
  ([cdef run-ability pre-run-effect]
   (run-event cdef run-ability pre-run-effect nil))
  ([cdef run-ability pre-run-effect post-run-effect]
   (merge {:prompt "Choose a server"
           :choices (req runnable-servers)
           :effect (effect ((or pre-run-effect (effect)) eid card targets)
                           (run target run-ability card)
                           ((or post-run-effect (effect)) eid card targets))}
          cdef)))

(defn- cutlery
  [_subtype]
  ;; Subtype does nothing currently, but might be used if trashing is properly implemented
  {:implementation "Ice trash is manual, always enables Reprisals"
   :async true
   :effect (req (continue-ability state :runner
                                  (run-event nil nil nil
                                             (req (swap! state assoc-in [:runner :register :trashed-card] true)))
                                  card nil))})

(def card-definitions
  {"Account Siphon"
   {:req (req hq-runnable)
    :makes-run true
    :effect (effect (run :hq {:req (req (= target :hq))
                              :replace-access
                              {:msg (msg "force the Corp to lose " (min 5 (:credit corp))
                                         " [Credits], gain " (* 2 (min 5 (:credit corp)))
                                         " [Credits] and take 2 tags")
                               :async true
                               :effect (req (wait-for (gain-tags state :runner 2)
                                                      (do (gain-credits state :runner (* 2 (min 5 (:credit corp))))
                                                          (lose-credits state :corp (min 5 (:credit corp)))
                                                          (effect-completed state side eid))))}}
                         card))}

   "Amped Up"
   {:msg "gain [Click][Click][Click] and suffer 1 brain damage"
    :effect (effect (gain :click 3) (damage eid :brain 1 {:unpreventable true :card card}))}

   "Another Day, Another Paycheck"
   {:events {:agenda-stolen
             {:trace {:base 0
                      :unsuccessful
                      {:effect (effect (gain-credits
                                         :runner (+ (:agenda-point runner) (:agenda-point corp))))
                       :msg (msg (str "gain " (+ (:agenda-point runner) (:agenda-point corp)) " [Credits]"))}}}}}

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
   (run-event
    {:choices (req (filter #(can-run-server? state %) remotes))}
    {:req (req (is-remote? target))
     :replace-access {:msg "shuffle all cards in the server into R&D"
                      :effect (req (doseq [c (:content run-server)]
                                     (move state :corp c :deck))
                                   (shuffle! state :corp :deck))}})

   "Blackmail"
   (run-event
    {:req (req has-bad-pub)
     :msg "prevent ICE from being rezzed during this run"}
    nil
    (effect (register-run-flag!
              card
              :can-rez
              (fn [state side card]
                (if (ice? card)
                  ((constantly false)
                    (toast state :corp "Cannot rez ICE on this run due to Blackmail"))
                  true)))))

   "Black Hat"
   {:trace {:base 4
            :unsuccessful {:effect (effect (register-events (:events (card-def card))
                                                            (assoc card :zone '(:discard))))}}
    :events {:pre-access {:req (req (#{:hq :rd} target))
                          :effect (effect (access-bonus 2))}
             :runner-turn-ends {:effect (effect (unregister-events card))}}}

   "Bribery"
   {:prompt "How many credits?"
    :choices :credit
    :msg (msg "increase the rez cost of the first unrezzed ICE approached by " target " [Credits]")
    :effect (effect (resolve-ability (run-event) card nil))}

   "Brute-Force-Hack"
   {:implementation "Runner must calculate the right number of credits including other game effects for the planned target ICE"
    :prompt "How many [Credits]?" :choices :credit
    :effect (effect (system-msg (str "spends " target " [Credit] on Brute-Force-Hack"))
                    (resolve-ability {:choices {:req #(and (ice? %)
                                                           (rezzed? %)
                                                           (<= (:cost %) target))}
                                      :effect (effect (derez target))
                                      :msg (msg "derez " (:title target))} card nil))}

   "Build Script"
   {:msg "gain 1 [Credits] and draw 2 cards"
    :effect (effect (gain-credits 1) (draw 2))}

   "By Any Means"
   {:effect (effect (register-events (:events (card-def card))
                                     (dissoc card :zone)))
    :events {:runner-turn-ends {:effect (effect (unregister-events card))}
             :access {:req (req (not= [:discard] (:zone target)))
                      :interactive (req true)
                      :async true
                      :msg (msg "trash " (:title target) " at no cost and suffer 1 meat damage")
                      :effect (req (wait-for (trash state side (assoc target :seen true) nil)
                                             (do (swap! state assoc-in [:runner :register :trashed-card] true)
                                                 (damage state :runner eid :meat 1 {:unboostable true}))))}}}

   "Calling in Favors"
   {:msg (msg "gain " (count (filter #(and (has-subtype? % "Connection") (is-type? % "Resource"))
                                     (all-active-installed state :runner))) " [Credits]")
    :effect (effect (gain-credits (count (filter #(and (has-subtype? % "Connection") (is-type? % "Resource"))
                                                 (all-active-installed state :runner)))))}

   "Career Fair"
   {:prompt "Select a resource to install from your Grip"
    :choices {:req #(and (is-type? % "Resource")
                         (in-hand? %))}
    :effect (effect (install-cost-bonus [:credit -3]) (runner-install target))}

   "Careful Planning"
   {:prompt  "Choose a card in or protecting a remote server"
    :choices {:req #(is-remote? (second (:zone %)))}
    :end-turn {:effect (effect (remove-icon card target))}
    :effect (effect (add-icon card target "CP" "red")
                    (system-msg (str "prevents the rezzing of " (card-str state target)
                                     " for the rest of this turn via Careful Planning"))
                    (register-turn-flag! card :can-rez
                                         (fn [state side card]
                                           (if (= (:cid card) (:cid target))
                                             ((constantly false)
                                               (toast state :corp "Cannot rez the rest of this turn due to Careful Planning"))
                                             true))))}

   "CBI Raid"
   (letfn [(cbi-final [chosen original]
             {:prompt (str "The top cards of R&D will be " (clojure.string/join  ", " (map :title chosen)) ".")
              :choices ["Done" "Start over"]
              :async true
              :effect (req (if (= target "Done")
                             (do (doseq [c (reverse chosen)] (move state :corp c :deck {:front true}))
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
            :effect (effect (run :hq {:replace-access
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
    :effect (effect (run :rd
                         {:replace-access
                          {:async true
                           :prompt "Choose a program to install"
                           :msg (msg "install " (:title target) " and take 1 tag")
                           :choices (req (filter #(is-type? % "Program") (:deck runner)))
                           :effect (effect (trigger-event :searched-stack nil)
                                           (shuffle! :deck)
                                           (install-cost-bonus [:credit (* -3 (count (get-in corp [:servers :rd :ices])))])
                                           (runner-install target)
                                           (gain-tags eid 1))}}
                         card))}

   "Cold Read"
   (let [end-effect {:prompt "Choose a program that was used during the run to trash "
                     :choices {:req #(card-is? % :type "Program")}
                     :msg (msg "trash " (:title target))
                     :effect (effect (trash target {:unpreventable true}))}]
     {:async true
      :prompt "Choose a server"
      :recurring 4
      :choices (req runnable-servers)
      :effect (req (let [c (move state side (assoc card :zone '(:discard)) :play-area {:force true})]
                     (card-init state side c {:resolve-effect false})
                     (game.core/run state side (make-eid state) target
                                    {:end-run {:async true
                                               :effect (effect (trash c)
                                                               (continue-ability end-effect card nil))}}
                                    c)))})

   "Contaminate"
   {:effect (req (resolve-ability
                   state side
                   {:msg (msg "place 3 virus tokens on " (:title target))
                    :choices {:req #(and (installed? %)
                                         (= (:side %) "Runner")
                                         (zero? (get-virus-counters state %)))}
                    :effect (req (add-counter state :runner target :virus 3))}
                   card nil))}

  "Corporate \"Grant\""
  {:events {:runner-install {:silent (req true) ;; there are no current interactions where we'd want Grant to not be last, and this fixes a bug with Hayley
                             :req (req (first-event? state side :runner-install))
                             :msg "force the Corp to lose 1 [Credit]"
                             :effect (effect (lose-credits :corp 1))}}}

   "Corporate Scandal"
   {:msg "give the Corp 1 additional bad publicity"
    :implementation "No enforcement that this Bad Pub cannot be removed"
    :effect (req (swap! state update-in [:corp :has-bad-pub] inc))
    :leave-play (req (swap! state update-in [:corp :has-bad-pub] dec))}

   "Credit Crash"
   {:prompt "Choose a server" :choices (req runnable-servers)
    :effect (effect (run target nil card)
                    (register-events (:events (card-def card))
                                     (assoc card :zone '(:discard))))
    :events {:pre-access-card
             {:once :per-run
              :async true
              :req (req (not= (:type target) "Agenda"))
              :effect (req (let [c target
                                 cost (:cost c)
                                 title (:title c)]
                             (if (can-pay? state :corp nil :credit cost)
                               (do (show-wait-prompt state :runner "Corp to decide whether or not to prevent the trash")
                                   (continue-ability state :corp
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
                                                                    (trash-no-cost eid c))}}}
                                    card nil))
                               (do (system-msg state side (str "uses Credit Crash to trash " title " at no cost"))
                                   (trash-no-cost state side eid c)))))}
             :run-ends {:effect (effect (unregister-events card))}}}

   "Credit Kiting"
   {:req (req (some #{:hq :rd :archives} (:successful-run runner-reg)))
    :prompt "Select a card to install from your Grip"
    :choices {:req #(and (or (is-type? % "Hardware")
                             (is-type? % "Program")
                             (is-type? % "Resource"))
                         (in-hand? %))}
    :async true
    :effect (req (install-cost-bonus state :runner [:credit -8])
                 (wait-for (runner-install state :runner target nil)
                           (gain-tags state eid :runner 1)))}

   "Cyber Threat"
   {:prompt "Choose a server"
    :choices (req runnable-servers)
    :async true
    :effect (req (let [serv target]
                   (continue-ability
                     state :corp
                     {:optional
                      {:prompt (msg "Rez a piece of ICE protecting " serv "?")
                       :yes-ability {:prompt (msg "Select a piece of ICE protecting " serv " to rez")
                                     :player :corp
                                     :choices {:req #(and (not (:rezzed %))
                                                          (= (last (:zone %)) :ices))}
                                     :effect (req (rez state :corp target nil))}
                       :no-ability {:effect (effect (game.core/run eid serv nil card))
                                    :msg (msg "make a run on " serv " during which no ICE can be rezzed")}}}
                    card nil)))}

   "Data Breach"
   {:req (req rd-runnable)
    :async true
    :effect (req (let [db-eid (make-eid state)
                       events (:events (card-def card))]
                   (register-events state side
                                    (assoc-in events [:successful-run-ends :eid] db-eid)
                                    (assoc card :zone '(:discard)))
                   (wait-for (game.core/run state side db-eid :rd nil card)
                             (let [card (get-card state (assoc card :zone '(:discard)))]
                               (unregister-events state side card)
                               (when (:run-again card)
                                 (game.core/run state side db-eid :rd nil card))
                               (update! state side (dissoc card :run-again))))))
    :events {:successful-run-ends
             {:optional {:req (req (= [:rd] (:server target)))
                         :prompt "Make another run on R&D?"
                         :yes-ability {:effect (effect (clear-wait-prompt :corp)
                                                       (update! (assoc card :run-again true)))}}}}}

   "Day Job"
   {:additional-cost [:click 3]
    :msg "gain 10 [Credits]" :effect (effect (gain-credits 10))}

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

   "Deep Data Mining"
   {:req (req rd-runnable)
    :effect (effect (run :rd nil card)
                    (register-events (:events (card-def card)) (assoc card :zone '(:discard))))
    :events {:successful-run {:silent (req true)
                              :effect (effect (access-bonus (max 0 (min 4 (available-mu state)))))}
             :run-ends {:effect (effect (unregister-events card))}}}

   "Demolition Run"
   {:req (req (or rd-runnable hq-runnable))
    :prompt "Choose a server"
    :choices ["HQ" "R&D"]
    :effect (effect (run target nil card)
                    (resolve-ability
                      {:effect (req (let [c (move state side (last (:discard runner)) :play-area)]
                                      (card-init state side c {:resolve-effect false})
                                      (register-events state side
                                                       {:run-ends {:effect (effect (trash c))}} c)))}
                     card nil))
    :events {:run-ends nil}
    :interactions {:trash-ability
                   {:label "[Demolition Run]: Trash card"
                    :msg (msg "trash " (:title target) " at no cost")
                    :async true
                    :effect (effect (trash-no-cost eid target))}}}

   "Deuces Wild"
   (let [all [{:effect (effect (gain-credits 3))
               :msg "gain 3 [Credits]"}
              {:effect (effect (draw 2))
               :msg "draw 2 cards"}
              {:effect (effect (lose-tags 1))
               :msg "remove 1 tag"}
              {:prompt "Select 1 piece of ice to expose"
               :msg "expose 1 ice and make a run"
               :choices {:req #(and (installed? %) (ice? %))}
               :async true
               :effect (req (wait-for (expose state side target)
                                      (continue-ability
                                        state side
                                        {:prompt "Choose a server"
                                         :choices (req runnable-servers)
                                         :async true
                                         :effect (effect (game.core/run eid target))}
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

   "Compile"
   {:implementation "Trigger only on first encounter not enforced"
    :prompt "Choose a server"
    :msg "make a run and install a program on encounter with the first piece of ICE"
    :choices (req runnable-servers)
    :async true
    :abilities [{:label "Install a program using Compile"
                 :async true
                 :effect (effect (resolve-ability
                                   {:prompt "Install a program from Stack or Heap?"
                                    :choices ["Stack" "Heap"]
                                    :msg (msg "install " (:title target))
                                    :effect (effect (resolve-ability
                                                      (let [chosen-source target]
                                                        {:prompt (str "Choose a program in your " chosen-source " to install")
                                                         :choices (req (cancellable (filter #(is-type? % "Program")
                                                                                            ((if (= chosen-source "Heap") :discard :deck) runner))))
                                                         :effect (req (runner-install state side (assoc-in target [:special :compile-installed] true) {:ignore-all-cost true})
                                                                      (when (= chosen-source "Stack")
                                                                        (shuffle! state :runner :deck)))})
                                                      card nil))}
                                   card nil))}]
    :effect (effect (run target nil card)
                    (prompt! card (str "Click Compile in the Temporary Zone to install a Program") ["OK"] {})
                    (resolve-ability
                      {:effect (req (let [c (move state side (last (:discard runner)) :play-area)]
                                         (card-init state side c {:resolve-effect false})))}
                      card nil))
    :events {:run-ends {:effect (req
                                 (let [compile-installed (first (filter #(get-in % [:special :compile-installed]) (game.core/all-installed state :runner)))]
                                   (when (not (empty? compile-installed))
                                     (system-msg state side (str "moved " (:title compile-installed) " to the bottom of the Stack at the end of the run from Compile"))
                                     (move state :runner compile-installed :deck)))
                                 (unregister-events state side card)
                                 (trash state side card))}}}

   "Dianas Hunt"
   {:implementation "One program per encounter not enforced"
    :prompt "Choose a server"
    :msg "make a run and install a program on encounter with each ICE"
    :choices (req runnable-servers)
    :async true
    :abilities [{:label "Install a program using Diana's Hunt?"
                 :async true
                 :effect (effect (resolve-ability
                                   {:prompt "Choose a program in your Grip to install"
                                    :choices {:req #(and (is-type? % "Program")
                                                         (runner-can-install? state side % false)
                                                         (in-hand? %))}
                                    :msg (msg "install " (:title target))
                                    :effect (req (let [diana-card (assoc-in target [:special :diana-installed] true)]
                                                   (runner-install state side diana-card {:ignore-all-cost true})
                                                   (swap! state update :diana #(conj % diana-card))))}
                                   card nil))}]
    :effect (effect (run target nil card)
                    (prompt! card (str "Click Diana's Hunt in the Temporary Zone to install a Program") ["OK"] {})
                    (resolve-ability
                      {:effect (req (let [c (move state side (last (:discard runner)) :play-area)]
                                      (card-init state side c {:resolve-effect false})
                                      (register-events state side
                                                       {:run-ends {:effect (req (let [hunt (:diana @state)]
                                                                                  (doseq [c hunt]
                                                                                    (let [installed (find-cid (:cid c) (all-installed state side))]
                                                                                      (when (get-in installed [:special :diana-installed])
                                                                                        (system-msg state side (str "trashes " (:title c) " at the end of the run from Diana's Hunt"))
                                                                                        (trash state side installed {:unpreventable true}))))
                                                                                  (swap! state dissoc :diana)
                                                                                  (unregister-events state side card)
                                                                                  (trash state side c)))}} c)))}
                      card nil))
    :events {:run-ends nil}}

   "Diesel"
   {:msg "draw 3 cards" :effect (effect (draw 3))}

   "Dirty Laundry"
   (run-event
    {:end-run {:req (req (:successful run))
               :msg "gain 5 [Credits]"
               :effect (effect (gain-credits :runner 5))}})

   "Diversion of Funds"
   {:req (req hq-runnable)
    :effect (effect (run :hq
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
    :effect
    (effect (run :archives
                 {:end-run
                  {:async true
                   :effect
                   (req (wait-for (do-access state side [:hq] {:no-root true})
                                  (do-access state side eid [:rd] {:no-root true})))}}
                 card))}

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
   (run-event
    {:msg (msg "make a run on " target " and gain [Click]")}
    nil
    (effect (gain :click 1)))

   "Easy Mark"
   {:msg "gain 3 [Credits]" :effect (effect (gain-credits 3))}

   "Embezzle"
   (letfn [(name-string [cards]
             (join " and " (map :title cards)))] ; either 'card' or 'card1 and card2'
    {:req (req hq-runnable)
     :effect (effect
              (run :hq {:req (req (= target :hq))
                        :replace-access
                        {:mandatory true
                         :msg (msg "reveal 2 cards from HQ and trash all "
                                   target (when (not= "ICE" (:type target)) "s"))
                         :prompt "Choose a card type"
                         :choices ["Asset" "Upgrade" "Operation" "ICE"]
                         :effect (req (let [chosen-type target
                                            cards-to-reveal (take 2 (shuffle (:hand corp)))
                                            cards-to-trash (filter #(is-type? % chosen-type) cards-to-reveal)]
                                        (system-msg state side (str " reveals " (name-string cards-to-reveal) " from HQ"))
                                        (when-not (empty? cards-to-trash)
                                          (system-msg state side (str " trashes " (name-string cards-to-trash)
                                                                      " from HQ and gain " (* 4 (count cards-to-trash)) "[Credits]"))
                                          (doseq [c cards-to-trash]
                                            (trash state :runner (assoc c :seen true)))
                                          (gain-credits state :runner (* 4 (count cards-to-trash))))))}}
                card))})

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
                       " and install " (:title target) " lowering the cost by " trash-cost)
             :choices (req (cancellable (filter #(or (is-type? % "Program")
                                                     (is-type? % "Hardware"))
                                                (:deck runner)) :sorted))
             :effect (req (trigger-event state side :searched-stack nil)
                          (shuffle! state side :deck)
                          (doseq [c to-trash]
                            (trash state side c {:unpreventable true}))
                          (install-cost-bonus state side [:credit (- trash-cost)])
                          (runner-install state side target)
                          (effect-completed state side eid))})]
   {:prompt "Choose Hardware and Programs to trash from your Grip"
    :choices {:req #(and (or (is-type? % "Hardware")
                             (is-type? % "Program"))
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

   "Encore"
   {:req (req (and (some #{:hq} (:successful-run runner-reg))
                   (some #{:rd} (:successful-run runner-reg))
                   (some #{:archives} (:successful-run runner-reg))))
    :effect (req (swap! state update-in [:runner :extra-turns] (fnil inc 0))
                 (move state side (first (:play-area runner)) :rfg))
    :msg "take an additional turn after this one"}

   "En Passant"
   {:req (req (:successful-run runner-reg))
    :effect (req (let [runtgt (first (flatten (turn-events state side :run)))
                       serv (zone->name runtgt)]
                   (resolve-ability state side
                     {:prompt (msg "Choose an unrezzed piece of ICE protecting " serv " that you passed on your last run")
                      :choices {:req #(and (ice? %)
                                           (not (rezzed? %)))}
                      :msg (msg "trash " (card-str state target))
                      :effect (req (trash state side target)
                                   (swap! state assoc-in [:runner :register :trashed-card] true))}
                    card nil)))}

   "Escher"
   (letfn [(es [] {:prompt "Select two pieces of ICE to swap positions"
                   :choices {:req #(and (installed? %) (ice? %)) :max 2}
                   :effect (req (if (= (count targets) 2)
                                  (do (swap-ice state side (first targets) (second targets))
                                      (resolve-ability state side (es) card nil))
                                  (system-msg state side "has finished rearranging ICE")))})]
     {:req (req hq-runnable)
            :effect (effect (run :hq {:replace-access
                                {:msg "rearrange installed ICE"
                                 :effect (effect (resolve-ability (es) card nil))}} card))})

   "Eureka!"
   {:effect (req (let [topcard (first (:deck runner))
                       caninst (or (is-type? topcard "Hardware")
                                   (is-type? topcard "Program")
                                   (is-type? topcard "Resource"))]
                   (if caninst
                     (resolve-ability
                       state side
                       {:optional {:prompt (msg "Install " (:title topcard) "?")
                                   :yes-ability {:effect (effect (install-cost-bonus [:credit -10])
                                                                 (runner-install topcard))}
                                   :no-ability {:effect (effect (trash topcard {:unpreventable true})
                                                                (system-msg (str "reveals and trashes "
                                                                                 (:title topcard))))}}} card nil)
                     (do (trash state side topcard {:unpreventable true})
                         (system-msg state side (str "reveals and trashes " (:title topcard)))))))}

   "Exclusive Party"
   {:msg (msg "draw 1 card and gain "
              (count (filter #(= (:title %) "Exclusive Party") (:discard runner)))
              " [Credits]")
    :effect (effect (draw) (gain-credits (count (filter #(= (:title %) "Exclusive Party") (:discard runner)))))}

   "Executive Wiretaps"
   {:msg (msg "reveal cards in HQ: " (join ", " (map :title (:hand corp))))}

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
   (run-event
     {:replace-access {:prompt "Advancements to remove from a card in or protecting this server?"
                       :choices ["0", "1", "2", "3"]
                       :async true
                       :effect (req (let [c (str->int target)]
                                      (show-wait-prompt state :corp "Runner to remove advancements")
                                      (continue-ability state side
                                        {:choices {:req #(and (contains? % :advance-counter)
                                                              (= (first (:server run)) (second (:zone %))))}
                                         :msg (msg "remove " (quantify c "advancement token")
                                                   " from " (card-str state target))
                                         :effect (req (let [to-remove (min c (get-counters target :advancement))]
                                                        (add-prop state :corp target :advance-counter (- to-remove))
                                                        (clear-wait-prompt state :corp)
                                                        (effect-completed state side eid)))}
                                        card nil)))}})

   "Express Delivery"
   {:prompt "Choose a card to add to your Grip" :choices (req (take 4 (:deck runner)))
    :msg "look at the top 4 cards of their Stack and add 1 of them to their Grip"
    :effect (effect (move target :hand) (shuffle! :deck))}

   "Falsified Credentials"
   {:prompt "Choose a type"
    :choices ["Agenda" "Asset" "Upgrade"]
    :msg (msg "to guess " target)
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
   {:req (req hq-runnable)
    :effect (effect (run :hq {:req (req (= target :hq))
                              :replace-access
                              {:async true
                               :mandatory true
                               :msg "force the Corp to trash the top card of R&D"
                               :effect (req (mill state :corp)
                                            (let [n (count (filter #(= (:title card) (:title %)) (:hand runner)))]
                                              (if (pos? n)
                                                (continue-ability state side
                                                  {:prompt "Reveal how many copies of Fear the Masses?"
                                                   :choices {:number (req n)}
                                                   :effect (req (when (pos? target)
                                                                  (mill state :corp target)
                                                                  (system-msg state side
                                                                              (str "reveals " target " copies of Fear the Masses,"
                                                                                   " forcing the Corp to trash " target " cards"
                                                                                   " from the top of R&D"))))}
                                                 card nil)
                                                (effect-completed state side eid))))}}
                         card))}

   "Feint"
   {:req (req hq-runnable)
    :implementation "Bypass is manual"
    :effect (effect (run :hq nil card) (register-events (:events (card-def card))
                                                        (assoc card :zone '(:discard))))
    ;; Don't need a msg since game will print that card access is prevented
    :events {:successful-run {:effect (effect (prevent-access))}
             :run-ends {:effect (effect (unregister-events card))}}}

   "Fisk Investment Seminar"
   {:msg "make each player draw 3 cards"
    :effect (effect (draw 3) (draw :corp 3))}

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
                                        (if (and (= target "Rez") (<= (rez-cost state :corp ice) (:credit corp)))
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
    :events {:runner-shuffle-deck nil}
    :effect
    (req (let [topten (take 10 (:deck runner))]
           (prompt! state :runner card (str "The top 10 cards of the Stack are "
                                            (join ", " (map :title topten))) ["OK"] {})
           (continue-ability
             state side
             {:prompt "Install a program?"
              :choices (conj (vec (sort-by :title (filter #(and (is-type? % "Program")
                                                                (can-pay? state side nil
                                                                          (modified-install-cost state side % [:credit -5])))
                                                          topten))) "No install")
              :async true
              :effect (req (if (not= target "No install")
                             (do (register-events state side
                                                  {:runner-shuffle-deck
                                                   {:effect (effect (update! (assoc card :shuffle-occurred true)))}}
                                                  (assoc card :zone '(:discard)))
                                 (install-cost-bonus state side [:credit -5])
                                 (let [to-trash (remove #(= (:cid %) (:cid target)) topten)]
                                   (wait-for (runner-install state side target nil)
                                             (let [card (get-card state (assoc card :zone '(:discard)))]
                                               (if (not (:shuffle-occurred card))
                                                 (do (system-msg state side (str "trashes " (join ", " (map :title to-trash))))
                                                     (doseq [c to-trash] (trash state side c {:unpreventable true}))
                                                     (effect-completed state side eid))
                                                 (do (system-msg state side "does not have to trash cards because the stack was shuffled")
                                                     (effect-completed state side eid)))))))
                             (do (doseq [c topten] (trash state side c {:unpreventable true}))
                                 (system-msg state side (str "trashes " (join ", " (map :title topten)))))))} card nil)))}

   "\"Freedom Through Equality\""
   {:events {:agenda-stolen {:msg "add it to their score area as an agenda worth 1 agenda point"
                             :async true
                             :effect (req (as-agenda state :runner eid card 1))}}}

   "Freelance Coding Contract"
   {:choices {:max 5
              :req #(and (is-type? % "Program")
                         (in-hand? %))}
    :msg (msg "trash " (join ", " (map :title targets)) " and gain "
              (* 2 (count targets)) " [Credits]")
    :effect (req (doseq [c targets]
                   (trash state side c {:unpreventable true}))
                 (gain-credits state side (* 2 (count targets))))}

   "Game Day"
   {:msg (msg "draw " (- (hand-size state :runner) (count (:hand runner))) " cards")
    :effect (effect (draw (- (hand-size state :runner) (count (:hand runner)))))}

  "Glut Cipher"
  (let [corp-choose {:show-discard true
                     :async true
                     :player :corp
                     :prompt (msg "Select 5 cards from Archives to add to HQ")
                     :choices {:max 5
                               :all true
                               :req #(and (= (:side %) "Corp")
                                          (= (:zone %) [:discard]))}
                     :msg (msg "move "
                               (let [seen (filter :seen targets)
                                     m (count  (remove :seen targets))]
                                 (str (join ", " (map :title seen))
                                      (when (pos? m)
                                        (str (when-not (empty? seen) " and ")
                                             (quantify m "unseen card")))
                                      " into HQ, then trash 5 cards")))
                     :effect (req (wait-for
                                    (resolve-ability state side
                                                     {:effect (req (doseq [c targets]
                                                                     (move state side c :hand)))}
                                                     card targets)
                                    (continue-ability state side
                                                      {:async true
                                                       :effect (req (doseq [c (take 5 (shuffle (:hand corp)))]
                                                                      (trash state :corp c))
                                                                    (clear-wait-prompt state :runner)
                                                                    (effect-completed state :runner eid))}
                                                      card nil)))}
        access-effect {:mandatory true
                       :async true
                       :req (req (>= (count (:discard corp)) 5))
                       :effect (req (show-wait-prompt
                                      state :runner
                                      "Corp to choose which cards to pick up from Archives") ;; For some reason it just shows successful-run-trigger-message, but this works!?
                                    (continue-ability state side
                                                      corp-choose
                                                      card nil))}]
    {:req (req archives-runnable)
     :makes-run true
     :effect (effect (run :archives
                          {:req (req (= target :archives))
                           :replace-access access-effect}
                          card))})

   "Government Investigations"
   {:flags {:psi-prevent-spend (req 2)}}

   "Guinea Pig"
   {:msg "trash all cards in the grip and gain 10 [Credits]"
    :effect (req (doseq [c (:hand runner)]
                   (trash state :runner c {:unpreventable true}))
                 (gain-credits state :runner 10))}

   "Hacktivist Meeting"
   {:implementation "Does not prevent rez if HQ is empty"
    :events {:rez {:req (req (and (not (ice? target))
                                  (pos? (count (:hand corp)))))
                   ;; FIXME the above condition is just a bandaid, proper fix would be preventing the rez altogether
                   :msg "force the Corp to trash 1 card from HQ at random"
                   :effect (effect (trash (first (shuffle (:hand corp)))))}}}

   "High-Stakes Job"
   (run-event
    {:choices (req (let [unrezzed-ice #(seq (filter (complement rezzed?) (:ices (second %))))
                         bad-zones (keys (filter (complement unrezzed-ice) (get-in @state [:corp :servers])))]
                     (zones->sorted-names (remove (set bad-zones) (get-runnable-zones state)))))}
    {:end-run {:req (req (:successful run))
               :msg "gain 12 [Credits]"
               :effect (effect (gain-credits :runner 12))}})

   "Hostage"
   {:prompt "Choose a Connection"
    :choices (req (cancellable (filter #(has-subtype? % "Connection") (:deck runner)) :sorted))
    :msg (msg "add " (:title target) " to their Grip and shuffle their Stack")
    :effect (req (let [connection target]
                   (trigger-event state side :searched-stack nil)
                   (resolve-ability
                     state side
                     {:prompt (str "Install " (:title connection) "?")
                      :choices ["Yes" "No"]
                      :effect (req (let [d target]
                                     (resolve-ability state side
                                       {:effect (req (shuffle! state side :deck)
                                                     (if (= "Yes" d)
                                                       (runner-install state side connection)
                                                       (move state side connection :hand)))} card nil)))}
                     card nil)))}

   "Hot Pursuit"
   {:req (req hq-runnable)
    :makes-run true
    :effect (effect (run :hq {:req (req (= target :hq))
                              :successful-run {:async true
                                               :msg "gain 9 [Credits] and take 1 tag"
                                               :effect (req (wait-for (gain-tags state :runner 1)
                                                                      (gain-credits state :runner 9)
                                                                      (effect-completed state side eid)))}} card))}

   "Ive Had Worse"
   {:effect (effect (draw 3))
    :trash-effect {:when-inactive true
                   :req (req (#{:meat :net} target))
                   :effect (effect (draw :runner 3)) :msg "draw 3 cards"}}

   "Immolation Script"
   {:req (req archives-runnable)
    :effect (effect (run :archives nil card)
                    (register-events (:events (card-def card)) (assoc card :zone '(:discard))))
    :events {:pre-access
             {:async true
              :req (req (and (= target :archives)
                             ;; don't prompt unless there's at least 1 rezzed ICE matching one in Archives
                             (not-empty (clojure.set/intersection
                                          (into #{} (map :title (filter #(ice? %) (:discard corp))))
                                          (into #{} (map :title (filter #(rezzed? %) (all-installed state :corp))))))))
              :effect (req (continue-ability state side
                             {:async true
                              :prompt "Choose a piece of ICE in Archives"
                              :choices (req (filter ice? (:discard corp)))
                              :effect (req (let [icename (:title target)]
                                             (continue-ability state side
                                               {:async true
                                                :prompt (msg "Select a rezzed copy of " icename " to trash")
                                                :choices {:req #(and (ice? %)
                                                                     (rezzed? %)
                                                                     (= (:title %) icename))}
                                                :msg (msg "trash " (card-str state target))
                                                :effect (req (trash state :corp target)
                                                             (unregister-events state side card)
                                                             (effect-completed state side eid))}
                                               card nil)))}
                            card nil))}}}

   "Independent Thinking"
   (letfn [(cards-to-draw [targets]
             (* (count targets)
                (if (some #(and (not (facedown? %)) (has-subtype? % "Directive")) targets) 2 1)))]
     {:async true
      :prompt "Choose up to 5 installed cards to trash with Independent Thinking"
      :choices {:max 5
                :req #(and (installed? %)
                           (= (:side %) "Runner"))}
      :effect (req (wait-for (trash-cards state side targets nil)
                             (draw state :runner eid (cards-to-draw targets) nil)))
      :msg (msg "trash " (join ", " (map :title targets)) " and draw " (quantify (cards-to-draw targets) "card"))})

   "Indexing"
   {:req (req rd-runnable)
    :async true
    :effect (effect (run :rd
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
   {:prompt "Gain 2 [Credits] or expose a card?" :choices ["Gain 2 [Credits]" "Expose a card"]
    :effect (effect (continue-ability (if (= target "Expose a card")
                                        {:choices {:req installed?}
                                         :async true
                                         :effect (effect (expose eid target))}
                                         {:msg "gain 2 [Credits]" :effect (effect (gain-credits 2))})
                                      card nil))}

   "Information Sifting"
   (letfn [(access-pile [cards pile pile-size]
             {:prompt "Choose a card to access. You must access all cards."
              :choices [(str "Card from pile " pile)]
              :async true
              :effect (req (wait-for
                             (access-card state side (first cards))
                             (if (< 1 (count cards))
                               (continue-ability state side (access-pile (next cards) pile pile-size) card nil)
                               (effect-completed state side eid))))})
           (which-pile [p1 p2]
             {:prompt "Choose a pile to access"
              :choices [(str "Pile 1 (" (quantify (count p1) "card") ")")
                        (str "Pile 2 (" (quantify (count p2) "card") ")")]
              :async true
              :effect (req (let [choice (if (.startsWith target "Pile 1") 1 2)]
                             (clear-wait-prompt state :corp)
                             (system-msg state side (str "chooses to access " target))
                             (continue-ability state side
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
                                  :choices {:req #(and (in-hand? %) (card-is? % :side :corp))
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
       {:req (req hq-runnable)
        :effect (effect (run :hq {:req (req (= target :hq))
                                  :replace-access access-effect}
                             card))}))

   "Inject"
   {:effect (req (doseq [c (take 4 (get-in @state [:runner :deck]))]
                   (if (is-type? c "Program")
                     (do (trash state side c {:unpreventable true})
                         (gain-credits state side 1)
                         (system-msg state side (str "trashes " (:title c) " and gains 1 [Credits]")))
                     (do (move state side c :hand)
                         (system-msg state side (str "adds " (:title c) " to Grip"))))))}

   "Injection Attack"
   (run-event
    {:async true}
    nil
    nil
    (effect (continue-ability
             {:prompt "Select an icebreaker"
              :choices {:req #(and (installed? %) (has-subtype? % "Icebreaker"))}
              :effect (effect (pump target 2 :all-run))}
             card nil)))

   "Inside Job"
   {:implementation "Bypass is manual"
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :effect (effect (run target nil card))}

   "Insight"
   {:async true
    :effect (req
             (let [from (take 4 (:deck corp))]
               (when (pos? (count from))
                 (do (show-wait-prompt state :runner
                                       (str "Corp to rearrange the top " (count from) " cards of R&D"))
                     (wait-for
                      (resolve-ability state :corp (reorder-choice :corp from) card targets)
                      (do (clear-wait-prompt state :runner)
                          (system-msg state :runner (str " reveals (top:) " ; here (get-in @state ...) is needed to refresh ref. to deck after reordering
                                                         (join ", " (map :title (take 4 (get-in @state [:corp :deck])))) " from the top of R&D"))
                          (effect-completed state side eid)))))))}
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
      :events {:runner-turn-begins {:effect ab}}
      :leave-play (req (clear-all-flags-for-card! state side card))})

   "Itinerant Protesters"
   {:msg "reduce the Corp's maximum hand size by 1 for each bad publicity"
    :effect (req (change-hand-size state :corp (- (:bad-publicity corp)))
                 (add-watch state :itin
                   (fn [k ref old new]
                     (let [bpnew (get-in new [:corp :bad-publicity])
                           bpold (get-in old [:corp :bad-publicity])
                           bpchange (- bpnew bpold)]
                       (when-not (zero? bpchange)
                         (change-hand-size state :corp (- bpchange)))))))
    :leave-play (req (remove-watch state :itin)
                     (change-hand-size state :corp (:bad-publicity corp)))}

   "Knifed"
   (cutlery "Barrier")

   "Kraken"
   {:req (req (:stole-agenda runner-reg)) :prompt "Choose a server" :choices (req servers)
    :msg (msg "force the Corp to trash an ICE protecting " target)
    :effect (req (let [serv (next (server->zone state target))
                       servname target]
                   (resolve-ability
                     state :corp
                     {:prompt (msg "Select a piece of ICE in " target " to trash")
                      :choices {:req #(and (= (last (:zone %)) :ices)
                                           (= serv (rest (butlast (:zone %)))))}
                      :effect (req (trash state :corp target)
                                   (system-msg state side (str "trashes "
                                    (card-str state target))))}
                    card nil)))}

   "Labor Rights"
   {:req (req (pos? (+ (count (:deck runner)) (count (:discard runner)))))
    :effect (req (let [mill-count (min 3 (count (:deck runner)))]
                   (mill state :runner :runner mill-count)
                   (system-msg state :runner (str "trashes the top " (quantify mill-count "card") " of their Stack"))
                   (let [heap-count (min 3 (count (get-in @state [:runner :discard])))]
                     (continue-ability
                       state side
                       {:prompt (str "Choose " (quantify heap-count "card") " to shuffle into the stack")
                        :show-discard true
                        :choices {:max heap-count
                                  :all true
                                  :not-self true
                                  :req #(and (= (:side %) "Runner")
                                             (in-discard? %))}
                        :effect (req (doseq [c targets] (move state side c :deck))
                                     (system-msg state :runner (str "shuffles " (join ", " (map :title targets))
                                                                    " from their Heap into their Stack, and draws 1 card"))
                                     (shuffle! state :runner :deck)
                                     (draw state :runner 1)
                                     (move state side (find-latest state card) :rfg)
                                     (system-msg state :runner "removes Labor Rights from the game"))}
                       card nil))))}

   "Lawyer Up"
   {:msg "remove 2 tags and draw 3 cards"
    :effect (effect (draw 3) (lose-tags 2))}

   "Lean and Mean"
   {:prompt "Choose a server"
    :choices (req runnable-servers)
    :async true
    :msg (msg "make a run on " target (when (< (count (filter #(is-type? % "Program") (all-active-installed state :runner))) 4)
                                        ", adding +2 strength to all icebreakers"))
    :effect (req (when (< (count (filter #(is-type? % "Program") (all-active-installed state :runner))) 4)
                   (doseq [c (filter #(has-subtype? % "Icebreaker") (all-active-installed state :runner))]
                     (pump state side c 2 :all-run)))
                 (game.core/run state side (make-eid state) target nil card))}

   "Leave No Trace"
   (letfn [(get-rezzed-cids [ice]
             (map :cid (filter #(and (rezzed? %)
                                     (is-type? % "ICE"))
                               ice)))]
     {:prompt "Choose a server"
      :msg "make a run and derez any ICE that are rezzed during this run"
      :choices (req runnable-servers)
      :async true
      :effect (req
                (let [old-ice-cids (get-rezzed-cids (all-installed state :corp))]
                  (swap! state assoc :lnt old-ice-cids)
                  (register-events state side (:events (card-def card)) (assoc card :zone '(:discard)))
                  (game.core/run state side (make-eid state) target nil card)))
      :events {:run-ends {:effect (req (let [new (set (get-rezzed-cids (all-installed state :corp)))
                                             old (set (:lnt @state))
                                             diff-cid (seq (clojure.set/difference new old))
                                             diff (map #(find-cid % (all-installed state :corp)) diff-cid)]
                                         (doseq [ice diff]
                                           (derez state :runner ice))
                                         (when-not (empty? diff)
                                           (system-msg state side (str "derezzes " (join ", " (map :title diff)) " via Leave No Trace")))
                                         (swap! state dissoc :lnt)
                                         (unregister-events state side card)))}}})

   "Legwork"
   {:req (req hq-runnable)
    :effect (effect (run :hq nil card) (register-events (:events (card-def card))
                                                        (assoc card :zone '(:discard))))
    :events {:successful-run {:silent (req true)
                              :effect (effect (access-bonus 2))}
             :run-ends {:effect (effect (unregister-events card))}}}

   "Leverage"
   {:req (req (some #{:hq} (:successful-run runner-reg)))
    :player :corp
    :prompt "Take 2 bad publicity?"
    :choices ["Yes" "No"]
    :effect (req (if (= target "Yes")
                   (do (gain-bad-publicity state :corp 2)
                       (system-msg state :corp "takes 2 bad publicity"))
                   (do (register-events state side
                                        {:pre-damage {:effect (effect (damage-prevent :net Integer/MAX_VALUE)
                                                                      (damage-prevent :meat Integer/MAX_VALUE)
                                                                      (damage-prevent :brain Integer/MAX_VALUE))}
                                         :runner-turn-begins {:effect (effect (unregister-events card))}}
                                        (assoc card :zone '(:discard)))
                       (system-msg state :runner "is immune to damage until the beginning of the Runner's next turn"))))
    ; This :events is a hack so that the unregister-events above will fire.
    :events {:runner-turn-begins nil :pre-damage nil}}

   "Levy AR Lab Access"
   {:msg "shuffle their Grip and Heap into their Stack and draw 5 cards"
    :effect (effect (shuffle-into-deck :hand :discard) (draw 5)
                    (move (first (:play-area runner)) :rfg))}

   "Lucky Find"
   {:msg "gain 9 [Credits]"
    :effect (effect (gain-credits 9))}

   "Mad Dash"
   {:prompt "Choose a server"
    :choices (req runnable-servers)
    :async true
    :effect (effect (run target nil card)
                    (register-events (:events (card-def card)) (assoc card :zone '(:discard))))
    :events {:agenda-stolen {:silent (req true)
                             :effect (effect (update! (assoc card :steal true)))}
             :run-ends {:async true
                        :effect (req (if (:steal card)
                                       (wait-for (as-agenda state :runner (get-card state card) 1)
                                                 (system-msg state :runner
                                                             (str "adds Mad Dash to their score area as an agenda worth 1 agenda point")))
                                       (do (system-msg state :runner
                                                       (str "suffers 1 meat damage from Mad Dash"))
                                           (damage state side eid :meat 1 {:card card})))
                                     (unregister-events state side card))}}}

   "Making an Entrance"
   (letfn [(entrance-trash [cards]
             {:prompt "Choose a card to trash"
              :choices (cons "None" cards)
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
   (run-event
     {:choices (req (filter #(can-run-server? state %) remotes))}
     {:end-run {:effect (req (prevent-run-on-server state card (:server run))
                             (when (:successful run)
                               (system-msg state :runner "gains 1 [Click] and adds Marathon to their grip")
                               (gain state :runner :click 1)
                               (move state :runner (assoc card :zone [:discard]) :hand)))}})


   "Mars for Martians"
   {:msg (msg "draw " (count (filter #(and (has-subtype? % "Clan") (is-type? % "Resource"))
                                     (all-active-installed state :runner)))
              " cards and gain " (count-tags state) " [Credits]")
    :effect (effect (draw (count (filter #(and (has-subtype? % "Clan") (is-type? % "Resource"))
                                         (all-active-installed state :runner))))
                    (gain-credits (count-tags state)))}

   "Mass Install"
   (let [mhelper (fn mi [n] {:prompt "Select a program to install"
                             :choices {:req #(and (is-type? % "Program")
                                                  (in-hand? %))}
                             :effect (req (runner-install state side target)
                                            (when (< n 3)
                                              (resolve-ability state side (mi (inc n)) card nil)))})]
     {:effect (effect (resolve-ability (mhelper 1) card nil))})

   "Mining Accident"
   (letfn [(mining [] {:player :corp
                       :async true
                       :prompt "Pay 5 [Credits] or take 1 Bad Publicity?"
                       :choices ["Pay 5 [Credits]" "Take 1 Bad Publicity"]
                       :effect (req (cond

                                      (and (= target "Pay 5 [Credits]") (can-pay? state :corp nil :credit 5))
                                      (do (lose-credits state :corp 5)
                                          (system-msg state side "pays 5 [Credits] from Mining Accident")
                                          (clear-wait-prompt state :runner)
                                          (effect-completed state side eid))

                                      (= target "Pay 5 [Credits]")
                                      (do (can-pay? state :corp "Mining Accident" :credit 5)
                                          (continue-ability state side (mining) card nil))

                                      (= target "Take 1 Bad Publicity")
                                      (do (gain-bad-publicity state :corp 1)
                                          (system-msg state side "takes 1 bad publicity from Mining Accident")
                                          (clear-wait-prompt state :runner)
                                          (effect-completed state side eid))))})]
   {:req (req (some #{:hq :rd :archives} (:successful-run runner-reg)))
    :async true
    :effect (req (move state side (first (:play-area runner)) :rfg)
                 (show-wait-prompt state :runner "Corp to choose to pay or take bad publicity")
                 (continue-ability state side (mining) card nil))
    :msg "make the Corp pay 5 [Credits] or take 1 bad publicity"})

   "Möbius"
   {:req (req rd-runnable)
    :async true
    :effect (req (let [mob-eid (make-eid state)
                       events (:events (card-def card))]
                   (register-events state side
                                    (assoc-in events [:successful-run-ends :eid] mob-eid)
                                    (assoc card :zone '(:discard)))
                   (wait-for (game.core/run state side mob-eid :rd nil card)
                             (let [card (get-card state (assoc card :zone '(:discard)))]
                               (unregister-events state side card)
                               (when (:run-again card)
                                 (game.core/run state side mob-eid :rd nil card)
                                 (register-events state side {:successful-run
                                                              {:req (req (= target :rd))
                                                               :msg "gain 4 [Credits]"
                                                               :effect (effect (gain-credits 4)
                                                                               (unregister-events card))}}

                                               (assoc card :zone '(:discard))))
                            (update! state side (dissoc card :run-again))))))
    :events {:successful-run nil
             :successful-run-ends {:interactive (req true)
                                   :optional {:req (req (= [:rd] (:server target)))
                                              :prompt "Make another run on R&D?"
                                              :yes-ability {:effect (effect (clear-wait-prompt :corp)
                                                                            (update! (assoc card :run-again true)))}}}}}

   "Modded"
   {:prompt "Select a program or piece of hardware to install from your Grip"
    :choices {:req #(and (or (is-type? % "Hardware")
                             (is-type? % "Program"))
                         (in-hand? %))}
    :effect (effect (install-cost-bonus [:credit -3]) (runner-install target))}

   "Net Celebrity"
   {:recurring 1}

   "Networking"
   {:msg "remove 1 tag"
    :effect (effect (lose-tags 1))
    :optional {:prompt "Pay 1 [Credits] to add Networking to Grip?"
               :yes-ability {:cost [:credit 1]
                             :msg "add it to their Grip"
                             :effect (effect (move (last (:discard runner)) :hand))}}}

   "Notoriety"
   {:req (req (and (some #{:hq} (:successful-run runner-reg))
                   (some #{:rd} (:successful-run runner-reg))
                   (some #{:archives} (:successful-run runner-reg))))
    :async true
    :effect (req (as-agenda state :runner eid (first (:play-area runner)) 1))
    :msg "add it to their score area as an agenda worth 1 agenda point"}

   "Office Supplies"
   {:play-cost-bonus (req [:credit (- (:link runner 0))])
    :effect (effect (continue-ability
                      {:prompt "Gain 4 [Credits] or draw 4 cards?"
                       :choices ["Gain 4 [Credits]" "Draw 4 cards"]
                       :effect (req (cond
                                      (= target "Gain 4 [Credits]")
                                      (do
                                        (system-msg state :runner (str "uses Office Supplies to gain 4 [Credits]"))
                                        (gain-credits state :runner 4))
                                      (= target "Draw 4 cards")
                                      (do
                                        (system-msg state :runner (str "uses Office Supplies to draw 4 cards"))
                                        (draw state :runner 4))))}
                      card nil))}

   "On the Lam"
   {:req (req (some #(is-type? % "Resource") (all-active-installed state :runner)))
    :prompt "Choose a resource to host On the Lam"
    :choices {:req #(and (is-type? % "Resource")
                         (installed? %))}
    :effect (effect (host target (assoc card :zone [:discard] :installed true))
                    (system-msg (str "hosts On the Lam on " (:title target))))
    :interactions {:prevent [{:type #{:net :brain :meat :tag}
                              :req (req true)}]}
    :abilities [{:label "[Trash]: Avoid 3 tags"
                 :msg "avoid up to 3 tags"
                 :effect (effect (tag-prevent :runner 3)
                                 (trash card {:cause :ability-cost}))}
                {:label "[Trash]: Prevent up to 3 damage"
                 :msg "prevent up to 3 damage"
                 :effect (effect (damage-prevent :net 3)
                                 (damage-prevent :meat 3)
                                 (damage-prevent :brain 3)
                                 (trash card {:cause :ability-cost}))}]}

   "Out of the Ashes"
   (let [ashes-run {:prompt "Choose a server"
                    :choices (req runnable-servers)
                    :async true
                    :effect (effect (run eid target nil card))}
         ashes-recur (fn ashes-recur [n]
                       {:prompt "Remove Out of the Ashes from the game to make a run?"
                        :choices ["Yes" "No"]
                        :effect (req (if (= target "Yes")
                                       (let [card (some #(when (= "Out of the Ashes" (:title %)) %) (:discard runner))]
                                         (system-msg state side "removes Out of the Ashes from the game to make a run")
                                         (move state side card :rfg)
                                         (unregister-events state side card)
                                         (wait-for (resolve-ability state side ashes-run card nil)
                                                   (if (< 1 n)
                                                     (continue-ability state side (ashes-recur (dec n)) card nil)
                                                     (effect-completed state side eid))))))})
         ashes-flag {:runner-phase-12 {:priority -1
                                       :once :per-turn
                                       :once-key :out-of-ashes
                                       :effect (effect (continue-ability
                                                         (ashes-recur (count (filter #(= "Out of the Ashes" (:title %))
                                                                                     (:discard runner))))
                                                         card nil))}}]
   (run-event
    {:move-zone (req (if (= [:discard] (:zone card))
                       (register-events state side ashes-flag (assoc card :zone [:discard]))
                       (unregister-events state side card)))
     :events {:runner-phase-12 nil}}
    nil))

   "Paper Tripping"
   {:msg "remove all tags" :effect (effect (lose-tags :all))}

   "Peace in Our Time"
   {:req (req (not (:scored-agenda corp-reg)))
    :msg "gain 10 [Credits]. The Corp gains 5 [Credits]"
    :effect (req (gain-credits state :runner 10)
                 (gain-credits state :corp 5)
                 (apply prevent-run-on-server
                        state card (get-zones state))
                 (register-events state side
                   {:runner-turn-ends {:effect (req (apply enable-run-on-server state card (get-zones state)))}}
                  (assoc card :zone '(:discard))))
    :events {:runner-turn-ends nil}}

   "Planned Assault"
   {:msg (msg "play " (:title target))
    :choices (req (cancellable (filter #(and (has-subtype? % "Run")
                                             (<= (:cost %) (:credit runner))) (:deck runner)) :sorted))
    :prompt "Choose a Run event" :effect (effect (trigger-event :searched-stack nil)
                                                 (shuffle! :deck)
                                                 (play-instant target {:no-additional-cost true}))}

   "Political Graffiti"
   (let [update-agenda-points (fn [state side target amount]
                               (set-prop state side (get-card state target) :agendapoints (+ amount (:agendapoints (get-card state target))))
                               (gain-agenda-point state side amount))]
     {:req (req archives-runnable)
      :events {:purge {:effect (effect (trash card {:cause :purge}))}}
      :trash-effect {:effect (req (let [current-side (get-scoring-owner state {:cid (:agenda-cid card)})]
                                    (update-agenda-points state current-side (find-cid (:agenda-cid card) (get-in @state [current-side :scored])) 1)))}
      :effect (effect (run :archives
                        {:req (req (= target :archives))
                         :replace-access
                         {:prompt "Select an agenda to host Political Graffiti"
                          :choices {:req #(in-corp-scored? state side %)}
                          :msg (msg "host Political Graffiti on " (:title target) " as a hosted condition counter")
                          :effect (req (host state :runner (get-card state target)
                                         ; keep host cid in :agenda-cid because `trash` will clear :host
                                         (assoc card :zone [:discard] :installed true :agenda-cid (:cid (get-card state target))))
                                       (update-agenda-points state :corp target -1))}} card))})

   "Populist Rally"
   {:req (req (seq (filter #(has-subtype? % "Seedy") (all-active-installed state :runner))))
    :msg "give the Corp 1 fewer [Click] to spend on their next turn"
    :effect (effect (lose :corp :click-per-turn 1)
                    (register-events (:events (card-def card))
                                     (assoc card :zone '(:discard))))
    :events {:corp-turn-ends {:effect (effect (gain :corp :click-per-turn 1)
                                              (unregister-events card))}}}

   "Power Nap"
   {:effect (effect (gain-credits (+ 2 (count (filter #(has-subtype? % "Double")
                                                      (:discard runner))))))
    :msg (msg "gain " (+ 2 (count (filter #(has-subtype? % "Double") (:discard runner)))) " [Credits]")}

   "Power to the People"
   {:effect (effect (register-events {:pre-steal-cost
                                      {:once :per-turn :effect (effect (gain-credits 7))
                                                       :msg "gain 7 [Credits]"}
                                      :runner-turn-ends
                                      {:effect (effect (unregister-events card))}}
                    (assoc card :zone '(:discard))))
    :events {:pre-steal-cost nil :runner-turn-ends nil}}

   "Prey"
   (run-event {:implementation "Ice trash is manual"} nil)

   "Process Automation"
   {:msg "gain 2 [Credits] and draw 1 card"
    :effect (effect (gain-credits 2) (draw 1))}

   "Push Your Luck"
   {:effect (effect (show-wait-prompt :runner "Corp to guess Odd or Even")
                    (resolve-ability
                      {:player :corp :prompt "Guess whether the Runner will spend an Odd or Even number of credits with Push Your Luck"
                       :choices ["Even" "Odd"]
                       :msg "force the Corp to make a guess"
                       :effect (req (let [guess target]
                                      (clear-wait-prompt state :runner)
                                      (resolve-ability
                                        state :runner
                                        {:choices :credit :prompt "How many credits?"
                                         :msg (msg "spend " target " [Credits]. The Corp guessed " guess)
                                         :effect (req (when (or (and (= guess "Even") (odd? target))
                                                                (and (= guess "Odd") (even? target)))
                                                        (system-msg state :runner (str "gains " (* 2 target) " [Credits]"))
                                                        (gain-credits state :runner (* 2 target))))} card nil)))}
                      card nil))}

   "Pushing the Envelope"
   (letfn [(hsize [s] (count (get-in s [:runner :hand])))]
   {:msg (msg (if (<= (hsize @state) 2)
           "make a run, and adds +2 strength to installed icebreakers"
           "make a run"))
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :async true
    :effect (req (when (<= (hsize @state) 2)
                   (let [breakers (filter #(has-subtype? % "Icebreaker") (all-active-installed state :runner))]
                     (doseq [t breakers] (pump state side t 2 :all-run))))
                 (game.core/run state side (make-eid state) target))})

   "Quality Time"
   {:msg "draw 5 cards" :effect (effect (draw 5))}

   "Queens Gambit"
   {:choices ["0", "1", "2", "3"] :prompt "How many advancement tokens?"
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
    :choices (req (let [is-draft-id? #(.startsWith (:code %) "00")
                        runner-identity (:identity runner)
                        is-swappable #(and (= "Identity" (:type %))
                                           (= (:faction runner-identity) (:faction %))
                                           (not (is-draft-id? %))
                                           (not= (:title runner-identity) (:title %)))
                        swappable-ids (filter is-swappable (vals @all-cards))]
                    (cancellable swappable-ids :sorted)))
     :effect (req
               (let [old-runner-identity (:identity runner)]
                 ;; Handle hosted cards (Ayla) - Part 1
                 (doseq [c (:hosted old-runner-identity)]
                   (move state side c :temp-hosted))

                 (move state side (last (:discard runner)) :rfg)
                 (disable-identity state side)

                 ;; Manually reduce the runner's link by old link
                 (lose state :runner :link (:baselink old-runner-identity)))

               ;; Move the selected ID to [:runner :identity] and set the zone
               (let [new-id (-> target :title server-card make-card (assoc :zone [:identity]))]
                 (swap! state assoc-in [side :identity] new-id)
                 ;; enable-identity does not do everything that init-identity does
                 (init-identity state side new-id))

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
     {:req (req archives-runnable)
      :makes-run true
      :effect (effect
                (run :archives
                     {:req (req (= target :archives))
                      :replace-access
                      {:prompt "Choose up to five cards to install"
                       :choices {:max 5
                                 :req #(and (in-discard? %) (= (:side %) "Runner") (not= (:cid %) (:cid card)))}
                       :mandatory true
                       :async true
                       :cancel-effect (req (move state side (find-latest state card) :rfg)
                                           (effect-completed state side eid))
                       :effect (req (install-cards state side eid card targets (map :title targets)))}}
                     card))})

   "Recon"
   (run-event)

   "Reshape"
   {:prompt "Select two non-rezzed ICE to swap positions"
    :choices {:req #(and (installed? %) (not (rezzed? %)) (ice? %)) :max 2}
    :msg (msg "swap the positions of " (card-str state (first targets)) " and " (card-str state (second targets)))
    :effect (req (when (= (count targets) 2)
                   (swap-ice state side (first targets) (second targets))))}

   "Retrieval Run"
   {:req (req archives-runnable)
    :effect (effect (run :archives
                      {:req (req (= target :archives))
                       :replace-access
                       {:prompt "Choose a program to install"
                        :msg (msg "install " (:title target))
                        :choices (req (filter #(is-type? % "Program") (:discard runner)))
                        :effect (effect (runner-install target {:ignore-all-cost true}))}}
                      card))}

   "Rigged Results"
   (letfn [(choose-ice []
             {:prompt "Select a piece of ICE to bypass"
              :choices {:req #(ice? %)}
              :msg (msg "bypass " (card-str state target))
              :effect (effect (run (second (:zone target))))})
           (corp-choice [spent]
             {:prompt "Guess how many credits were spent"
              :choices ["0" "1" "2"]
              :async true
              :effect (req (system-msg state :runner (str "spends " spent "[Credit]. "
                                       (-> corp :user :username) " guesses " target "[Credit]"))
                           (clear-wait-prompt state :runner)
                           (lose-credits state :runner spent)
                           (if (not= (str spent) target)
                             (continue-ability state :runner (choose-ice) card nil)
                             (effect-completed state side eid)))})
           (runner-choice [cr]
             {:prompt "Spend how many credits?"
              :choices (take cr ["0" "1" "2"])
              :async true
              :effect (effect (show-wait-prompt :runner "Corp to guess")
                              (clear-wait-prompt :corp)
                              (continue-ability :corp (corp-choice (str->int target)) card nil))})]
   {:effect (effect (show-wait-prompt :corp "Runner to spend credits")
                    (continue-ability (runner-choice (inc (min 2 (:credit runner)))) card nil))})

   "Rip Deal"
   {:req (req hq-runnable)
    :effect (effect (run :hq {:req (req (= target :hq))
                              :replace-access
                                   {:async true
                                    :effect (req (let [n (min (-> @state :corp :hand count) (access-count state side :hq-access))
                                                       heap (-> @state :runner :discard count (- 1))]
                                                   (move state side (find-cid (:cid card) (:discard runner)) :rfg)
                                                   (if (pos? heap)
                                                     (resolve-ability state side
                                                                      {:show-discard true
                                                                       :prompt (str "Choose " (min n heap) " card(s) to move from the Heap to your Grip")
                                                                       :async true
                                                                       :msg (msg "take " (join ", " (map :title targets)) " from their Heap to their Grip")
                                                                       :choices {:max (min n heap)
                                                                                 :all true
                                                                                 :req #(and (= (:side %) "Runner")
                                                                                            (in-discard? %))}
                                                                       :effect (req (doseq [c targets] (move state side c :hand))
                                                                                    (do-access state side eid (:server run) {:hq-root-only true}))} card nil)
                                                     (resolve-ability state side
                                                                      {:async true
                                                                       :msg (msg "take no cards from their Heap to their Grip")
                                                                       :effect (req (do-access state side eid (:server run) {:hq-root-only true}))} card nil))))}} card))}

   "Rumor Mill"
   (letfn [(eligible? [card] (and (:uniqueness card)
                                  (or (card-is? card :type "Asset")
                                      (card-is? card :type "Upgrade"))
                                  (not (has-subtype? card "Region"))))
           (rumor [state] (filter eligible? (concat (all-installed state :corp)
                                  (get-in @state [:corp :hand])
                                  (get-in @state [:corp :deck])
                                  (get-in @state [:corp :discard]))))]
   {:leave-play (req (doseq [c (rumor state)]
                       (enable-card state :corp c)))
    :effect (req (doseq [c (rumor state)]
                   (disable-card state :corp c)))
    :events {:corp-install {:req (req (eligible? target))
                            :effect (effect (disable-card :corp target))}}})

   "Run Amok"
   {:implementation "Ice trash is manual"
    :prompt "Choose a server" :choices (req runnable-servers)
    :effect (effect (run target {:end-run {:msg " trash 1 piece of ICE that was rezzed during the run"}} card))}

   "Running Interference"
   (run-event
    {:events {:pre-rez nil
              :run-ends nil}}
    nil
    nil
    (effect (register-events {:pre-rez {:req (req (ice? target))
                                        :effect (effect (rez-cost-bonus (:cost target)))}
                              :run-ends {:effect (effect (unregister-events card))}}
                             (assoc card :zone '(:discard)))))

   "Satellite Uplink"
   {:choices {:max 2 :req installed?}
    :async true
    :effect (req (let [[card1 card2] targets]
                   (wait-for (expose state side card1)
                             (expose state side eid card2))))}

   "Scavenge"
   {:prompt "Select an installed program to trash"
    :choices {:req #(and (is-type? % "Program")
                         (installed? %))}
    :effect (req (let [trashed target tcost (- (:cost trashed)) st state si side]
                   (trash state side trashed)
                   (resolve-ability
                     state side
                     {:prompt "Select a program to install from your Grip or Heap"
                      :show-discard true
                      :choices {:req #(and (is-type? % "Program")
                                           (#{[:hand] [:discard]} (:zone %))
                                           (can-pay? st si nil (modified-install-cost st si % [:credit tcost])))}
                      :effect (effect (install-cost-bonus [:credit (- (:cost trashed))])
                                      (runner-install target))
                      :msg (msg "trash " (:title trashed) " and install " (:title target))} card nil)))}

   "Scrubbed"
   {:events (let [sc {:effect (req (update! state side (dissoc card :scrubbed-target)))}]
                 {:encounter-ice {:once :per-turn
                                  :effect (effect (update! (assoc card :scrubbed-target target))
                                                  (update-ice-strength current-ice))}
                  :pre-ice-strength {:req (req (= (:cid target) (get-in card [:scrubbed-target :cid])))
                                     :effect (effect (ice-strength-bonus -2 target))}
                  :run-ends sc})}

   "Showing Off"
   {:req (req rd-runnable)
    :effect (effect (run :rd
                      {:replace-access
                       {:msg "access cards from the bottom of R&D"
                        :async true
                        :effect (req
                                  ;; Not sure why this is done
                                  (wait-for (resolve-ability state side
                                                             {:effect (effect (register-events (:events (card-def card))
                                                                                               (assoc card :zone '(:discard))))}
                                                             card nil)
                                            (do-access state side eid (:server run))))}} card))
    :events {:pre-access {:silent (req true)
                          :effect (req (swap! state assoc-in [:corp :deck]
                                              (rseq (into [] (get-in @state [:corp :deck])))))}
             :run-ends {:effect (req (swap! state assoc-in [:corp :deck]
                                            (rseq (into [] (get-in @state [:corp :deck]))))
                                     (unregister-events state side card))}}}

   "Singularity"
   (run-event
    {:choices (req (filter #(can-run-server? state %) remotes))}
    {:req (req (is-remote? target))
     :replace-access {:mandatory true
                      :msg "trash all cards in the server at no cost"
                      :effect (req (doseq [c (:content run-server)]
                                     (trash state side c)))}})

   "Social Engineering"
   {:prompt "Select an unrezzed piece of ICE"
    :choices {:req #(and (= (last (:zone %)) :ices) (not (rezzed? %)) (ice? %))}
    :effect (req (let [ice target
                       serv (zone->name (second (:zone ice)))]
              (resolve-ability
                 state :runner
                 {:msg (msg "select the piece of ICE at position " (ice-index state ice) " of " serv)
                  :effect (effect (register-events {:pre-rez-cost
                                                    {:req (req (= target ice))
                                                     :effect (req (let [cost (rez-cost state side (get-card state target))]
                                                                    (gain-credits state :runner cost)))
                                                     :msg (msg "gain " (rez-cost state side (get-card state target)) " [Credits]")}}
                                  (assoc card :zone '(:discard))))}
               card nil)))
    :events {:pre-rez-cost nil}
    :end-turn {:effect (effect (unregister-events card))}}

   "Spear Phishing"
   {:implementation "Bypass is manual"
    :prompt "Choose a server"
    :choices (req runnable-servers)
    :effect (effect (run target nil card))}

   "Special Order"
   {:prompt "Choose an Icebreaker"
    :effect (effect (trigger-event :searched-stack nil)
                    (shuffle! :deck)
                    (system-msg (str "adds " (:title target) " to their Grip and shuffles their Stack"))
                    (move target :hand))
    :choices (req (cancellable (filter #(has-subtype? % "Icebreaker") (:deck runner)) :sorted))}

   "Spooned"
   (cutlery "Code Gate")

   "Spot the Prey"
   {:prompt "Select 1 non-ICE card to expose"
    :msg "expose 1 card and make a run"
    :choices {:req #(and (installed? %) (not (ice? %)) (= (:side %) "Corp"))}
    :async true
    :effect (req (wait-for (expose state side target)
                           (continue-ability
                             state side
                             {:prompt "Choose a server"
                              :choices (req runnable-servers)
                              :async true
                              :effect (effect (game.core/run eid target))}
                             card nil)))}

   "Stimhack"
   (run-event
    nil
    {:end-run {:msg "take 1 brain damage"
               :effect (effect (damage eid :brain 1 {:unpreventable true :card card}))}}
    (effect (gain-run-credits 9)))

   "Sure Gamble"
   {:msg "gain 9 [Credits]" :effect (effect (gain-credits 9))}

   "Surge"
   {:msg (msg "place 2 virus tokens on " (:title target))
    :choices {:req #(and (has-subtype? % "Virus") (:added-virus-counter %))}
    :effect (req (add-counter state :runner target :virus 2))}

   "SYN Attack"
   {:effect (req (if (< (count (:hand corp)) 2)
                   (draw state :corp 4)
                   (do (show-wait-prompt state :runner "Corp to choose an option for SYN Attack")
                       (resolve-ability state :corp
                         {:prompt "Discard 2 cards or draw 4 cards?"
                          :choices ["Discard 2" "Draw 4"]
                          :effect (req (if (= target "Draw 4")
                                         (do (draw state :corp 4)
                                             (system-msg state :corp (str "draws 4 cards from SYN Attack"))
                                             (clear-wait-prompt state :runner))
                                         (resolve-ability state :corp
                                           {:prompt "Choose 2 cards to discard"
                                            :choices {:max 2 :req #(and (in-hand? %) (= (:side %) "Corp"))}
                                            :effect (effect (trash-cards :corp targets)
                                                            (system-msg :corp (str "discards 2 cards from SYN Attack"))
                                                            (clear-wait-prompt :runner))}
                                          card nil)))}
                        card nil))))}

   "System Outage"
   {:events {:corp-draw {:req (req (not (first-event? state side :corp-draw)))
                         :msg "force the Corp to lose 1 [Credits]"
                         :effect (effect (lose-credits :corp 1))}}}

   "System Seizure"
  {:effect (effect (register-events (:events (card-def card)) (assoc card :zone '(:discard))))
   :events {:pump-breaker {:silent (req true)
                           :req (req (or (and (has-flag? state side :current-run :system-seizure)
                                              (run-flag? state side (second targets) :system-seizure))
                                         (not (get-in @state [:per-turn (:cid card)]))))
                           :effect (req (update! state side (update-in (second targets) [:pump :all-run] (fnil #(+ % (first targets)) 0)))
                                        (register-run-flag! state side card :system-seizure (fn [_ _ c] (= (:cid c) (:cid (second targets)))))
                                        (update-breaker-strength state side (second targets))
                                        (swap! state assoc-in [:per-turn (:cid card)] targets))}}
   :move-zone (req (when (= [:discard] (:zone card))
                     (unregister-events state side card)))}

   "Test Run"
   {:prompt "Install a program from your Stack or Heap?"
    :choices (cancellable ["Stack" "Heap"])
    :msg (msg "install a program from their " target)
    :effect (effect (resolve-ability
                      {:prompt "Choose a program to install"
                       :choices (req (cancellable
                                       (filter #(is-type? % "Program")
                                               ((if (= target "Heap") :discard :deck) runner))))
                       :effect (effect (trigger-event :searched-stack nil)
                                       (shuffle! :deck)
                                       (runner-install (assoc-in target [:special :test-run] true) {:ignore-all-cost true}))
                       :end-turn
                       {:req (req (get-in (find-cid (:cid target) (all-installed state :runner)) [:special :test-run]))
                        :msg (msg "move " (:title target) " to the top of their Stack")
                        :effect (req (move state side (find-cid (:cid target) (all-installed state :runner))
                                           :deck {:front true}))}}
                      card targets))}

   "The Makers Eye"
   {:req (req rd-runnable)
    :effect (effect (run :rd nil card)
                    (register-events (:events (card-def card)) (assoc card :zone '(:discard))))
    :events {:successful-run {:silent (req true)
                              :req (req (= target :rd))
                              :effect (effect (access-bonus 2))}
             :run-ends {:effect (effect (unregister-events card))}}}

   "The Noble Path"
   {:effect (req (doseq [c (:hand runner)]
                   (trash state side c))
                 (register-events state side
                                  {:pre-damage {:effect (effect (damage-prevent :net Integer/MAX_VALUE)
                                                                (damage-prevent :meat Integer/MAX_VALUE)
                                                                (damage-prevent :brain Integer/MAX_VALUE))}
                                   :run-ends {:effect (effect (unregister-events card))}}
                                  (assoc card :zone '(:discard)))
                 (resolve-ability state side
                   {:prompt "Choose a server"
                    :choices (req runnable-servers)
                    :msg (msg "trash their Grip and make a run on " target ", preventing all damage")
                    :effect (req (let [runtgt [(last (server->zone state target))]
                                       ices (get-in @state (concat [:corp :servers] runtgt [:ices]))]
                                   (swap! state assoc :per-run nil
                                                      :run {:server runtgt :position (count ices)
                                                            :access-bonus 0 :run-effect nil})
                                   (gain-run-credits state :runner (:bad-publicity corp))
                                   (swap! state update-in [:runner :register :made-run] #(conj % (first runtgt)))
                                   (trigger-event state :runner :run runtgt)))} card nil))
    :events {:pre-damage nil :run-ends nil}}

   "The Price of Freedom"
   {:additional-cost [:connection 1]
    :msg "prevent the Corp from advancing cards during their next turn"
    :effect (effect (register-events (:events (card-def card)) (assoc card :zone '(:rfg)))
                    (move (first (:play-area runner)) :rfg))
    :events {:corp-turn-begins
             {:effect (effect (register-turn-flag! card :can-advance
                                (fn [state side card]
                                  ((constantly false)
                                   (toast state :corp "Cannot advance cards this turn due to The Price of Freedom." "warning"))))
                              (unregister-events card))}}}

   "Three Steps Ahead"
   {:end-turn {:effect (effect (gain-credits (* 2 (count (:successful-run runner-reg)))))
               :msg (msg "gain " (* 2 (count (:successful-run runner-reg))) " [Credits]")}}

   "Tinkering"
   {:prompt "Select a piece of ICE"
    :choices {:req #(and (= (last (:zone %)) :ices) (ice? %))}
    :effect (req (let [ice target
                       serv (zone->name (second (:zone ice)))
                       stypes (:subtype ice)]
              (resolve-ability
                 state :runner
                 {:msg (msg "make " (card-str state ice) " gain Sentry, Code Gate, and Barrier until the end of the turn")
                  :effect (effect (update! (assoc ice :subtype (combine-subtypes true (:subtype ice) "Sentry" "Code Gate" "Barrier")))
                                  (update-ice-strength (get-card state ice))
                                  (add-icon card (get-card state ice) "T" "green")
                                  (register-events {:runner-turn-ends
                                                    {:effect (effect (remove-icon card (get-card state ice))
                                                                     (update! (assoc (get-card state ice) :subtype stypes)))}}
                                  (assoc card :zone '(:discard))))}
               card nil)))
    :events {:runner-turn-ends nil}}

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
                                        :choices (req (filter #(is-type? % "Hardware")
                                                              (:deck runner)))
                                        :msg (msg "add " (:title target) " to their Grip (and shuffle their Stack)")
                                        :effect (effect (trigger-event :searched-stack nil)
                                                        (shuffle! :deck)
                                                        (move target :hand))}
                                       card nil)))})

   "Traffic Jam"
   {:effect (effect (update-all-advancement-costs))
    :leave-play (effect (update-all-advancement-costs))
    :events {:pre-advancement-cost
             {:effect (req (advancement-cost-bonus
                             state side (count (filter #(= (:title %) (:title target)) (:scored corp)))))}}}

   "Uninstall"
   {:choices {:req #(and (installed? %)
                         (not (facedown? %))
                         (#{"Program" "Hardware"} (:type %)))}
    :msg (msg "move " (:title target) " to their Grip")
    :effect (effect (move target :hand))}

   "Unscheduled Maintenance"
   {:events {:corp-install {:req (req (ice? target))
                            :effect (effect (register-turn-flag!
                                              card :can-install-ice
                                              (fn [state side card]
                                                (if (ice? card)
                                                  ((constantly false)
                                                   (toast state :corp "Cannot install ICE the rest of this turn due to Unscheduled Maintenance"))
                                                  true))))}}
    :leave-play (effect (clear-turn-flag! card :can-install-ice))}

   "Vamp"
   {:req (req hq-runnable)
    :effect (effect (run :hq {:req (req (= target :hq))
                              :replace-access
                              {:async true
                               :prompt "How many [Credits]?" :choices :credit
                               :msg (msg "take 1 tag and make the Corp lose " target " [Credits]")
                               :effect (effect (lose-credits :corp target)
                                               (gain-tags eid 1))}} card))}

   "Wanton Destruction"
   {:req (req hq-runnable)
    :effect (effect (run :hq {:req (req (= target :hq))
                              :replace-access
                              {:msg (msg "force the Corp to discard " target " cards from HQ at random")
                               :prompt "How many [Click] do you want to spend?"
                               :choices (req (map str (range 1 (inc (:click runner)))))
                               :effect (req (let [n (str->int target)]
                                              (when (pay state :runner card :click n)
                                                (trash-cards state :corp (take n (shuffle (:hand corp)))))))}} card))}

   "Watch the World Burn"
   (letfn [(rfg-card-event [burn-name]
             {:pre-access-card
              {:req (req (= (:title target) burn-name))
               :msg (msg (str "uses the previously played Watch the World Burn to remove " burn-name " from the game"))
               :effect (req (move state :corp target :rfg))}})]
     {:makes-run true
      :prompt "Choose a server"
      :choices (req (filter #(can-run-server? state %) remotes))
      :effect (effect (run target nil card)
                      (register-events (:events (card-def card))
                                       (dissoc card :zone)))
      :events {:pre-access-card {:req (req (and (not= (:type target) "Agenda")
                                                (get-in @state [:run :successful])))
                                 :once :per-run
                                 :effect (req (let [t (:title target)]
                                                (system-msg state :runner (str "to remove " t " from the game, and watch for other copies of " t " to burn"))
                                                (move state :corp target :rfg)
                                                ;; in the below, the new :cid ensures that when unregister-events is called, the rfg-card-event is left alone
                                                (register-events state side (rfg-card-event t) (dissoc (assoc card :cid (make-cid)) :zone))))}
               :run-ends {:effect (effect (unregister-events (dissoc card :zone)))}}})

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
             :effect (effect (continue-ability :runner (choose-cards (set (:hand corp)) #{}) card nil))}}})

   "Windfall"
   {:effect (effect (shuffle! :deck)
                    (resolve-ability
                      {:effect (req (let [topcard (first (:deck runner))
                                          cost (:cost topcard)]
                                      (trash state side topcard)
                                      (when-not (is-type? topcard "Event")
                                        (gain-credits state side cost))
                                      (system-msg state side
                                                  (str "shuffles their Stack and trashes " (:title topcard)
                                                       (when-not (is-type? topcard "Event")
                                                         (str " to gain " cost " [Credits]"))))))}
                     card nil))}})
