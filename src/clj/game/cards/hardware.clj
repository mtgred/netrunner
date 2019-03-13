(ns game.cards.hardware
  (:require [game.core :refer :all]
            [game.utils :refer :all]
            [game.macros :refer [effect req msg wait-for continue-ability]]
            [clojure.string :refer [split-lines split join lower-case includes? starts-with?]]
            [clojure.stacktrace :refer [print-stack-trace]]
            [jinteki.utils :refer [str->int other-side is-tagged? count-tags has-subtype?]]))

;; Card definitions
(def card-definitions
  {"Acacia"
   {:events {:pre-purge {:effect (req (let [counters (number-of-virus-counters state)]
                                        (update! state side (assoc-in (get-card state card) [:special :numpurged] counters))))}
             :purge {:async true
                     :effect (effect (show-wait-prompt  :corp "Runner to decide if they will use Acacia")
                                     (continue-ability
                                       {:optional
                                        {:player :runner
                                         :prompt "Use Acacia?"
                                         :yes-ability {:effect (req (let [counters (- (get-in (get-card state card) [:special :numpurged])
                                                                                      (number-of-virus-counters state))]
                                                                      (gain-credits state side counters)
                                                                      (system-msg state side (str "uses Acacia and gains " counters "[Credit]"))
                                                                      (trash state side card)
                                                                      (clear-wait-prompt state :corp)
                                                                      (effect-completed state side eid)))}
                                         :no-ability {:effect (effect (clear-wait-prompt :corp)
                                                                      (effect-completed eid))}}}
                                       card nil))}}}

   "Adjusted Matrix"
   {:implementation "Click Adjusted Matrix to use ability."
    :req (req (not-empty (filter #(has-subtype? % "Icebreaker") (all-active-installed state :runner))))
    :prompt "Choose Icebreaker on which to install Adjusted Matrix"
    :choices {:req #(and (= (:side %) "Runner") (has-subtype? % "Icebreaker") (installed? %))}
    :msg (msg "host it on " (card-str state target))
    :effect (effect (update! (assoc target :subtype (combine-subtypes false (-> target :subtype) "AI")))
                    (host (get-card state target) (get-card state card)))
    :abilities [{:cost [:click 1]
                 :req (req run)
                 :msg "break ice subroutine"}]
    :events {:pre-card-moved {:req (req (= (:cid target) (:cid card)))
                              :effect (effect (update! (assoc (-> card :host) :subtype (-> card :host :subtype (remove-subtypes-once ["AI"])))))}}}

   "Akamatsu Mem Chip"
   {:in-play [:memory 1]}

   "Archives Interface"
   {:events
    {:pre-access
     {:async true
      :interactive (req true)
      :req (req (and (= target :archives)
                     (not= (:max-access run) 0)
                     (not-empty (:discard corp))))
      :effect (req (swap! state update-in [:corp :discard] #(map (fn [c] (assoc c :seen true)) %))
                   (continue-ability
                     state side
                     {:optional
                      {:prompt "Use Archives Interface to remove a card from the game instead of accessing it?"
                       :yes-ability {:prompt "Choose a card in Archives to remove from the game instead of accessing"
                                     :choices (req (:discard corp))
                                     :msg (msg "remove " (:title target) " from the game")
                                     :effect (effect (move :corp target :rfg))}}} card nil))}}}

   "Astrolabe"
   {:in-play [:memory 1]
    :events {:server-created {:msg "draw 1 card"
                              :async true
                              :effect (effect (draw :runner eid 1 nil))}}}

   "Autoscripter"
   {:events {:runner-install {:silent (req true)
                              :req (req (and (is-type? target "Program")
                                             ;; only trigger on Runner's turn
                                             (= (:active-player @state) :runner)
                                             ;; only trigger when playing a Program from grip
                                             (some #{:hand} (:previous-zone target))
                                             ;; check that we haven't played a Program from the grip this turn
                                             ;; which translates to just one case of playing a Program in turn-events
                                             (first-event? state :runner :runner-install
                                                           (fn [[card _]] (and (some #{:hand} (:previous-zone card))
                                                                               (is-type? card "Program"))))))
                              :msg "gain [Click]"
                              :effect (effect (gain :click 1))}
             :unsuccessful-run {:effect (effect (trash card)
                                                (system-msg "trashes Autoscripter"))}}}

   "Blackguard"
   {:in-play [:memory 2]
    :events {:expose
             {:msg (msg "attempt to force the rez of " (:title target))
              :async true
              :effect (req (let [c target
                                 cdef (card-def c)
                                 cname (:title c)]
                             (if (:additional-cost cdef)
                               (do (show-wait-prompt state :runner (str "Corp to decide if they will rez " cname))
                                   (continue-ability
                                     state side
                                     {:optional
                                      {:prompt (msg "Pay additional cost to rez " cname "?")
                                       :player :corp
                                       :yes-ability {:effect (effect (rez :corp c)
                                                                     (clear-wait-prompt :runner))}
                                       :no-ability {:effect (effect (system-msg :corp (str "declines to pay additional costs"
                                                                                           " and is not forced to rez " cname))
                                                                    (clear-wait-prompt :runner))}}}
                                     card nil))
                               (do (rez state :corp target)
                                   (effect-completed state side eid)))))}}}

   "Bookmark"
   {:abilities [{:label "Host up to 3 cards from your Grip facedown"
                 :cost [:click 1] :msg "host up to 3 cards from their Grip facedown"
                 :choices {:max 3
                           :req #(and (= (:side %) "Runner")
                                      (in-hand? %))}
                 :effect (req (doseq [c targets]
                                (host state side (get-card state card) c {:facedown true})))}
                {:label "Add all hosted cards to Grip" :cost [:click 1] :msg "add all hosted cards to their Grip"
                 :effect (req (doseq [c (:hosted card)]
                                (move state side c :hand)))}
                {:label "[Trash]: Add all hosted cards to Grip" :msg "add all hosted cards to their Grip"
                 :effect (req (doseq [c (:hosted card)]
                                (move state side c :hand))
                              (update! state side (dissoc card :hosted))
                              (trash state side (get-card state card) {:cause :ability-cost}))}]}

   "Box-E"
   {:in-play [:memory 2 :hand-size 2]}

   "Brain Cage"
   {:in-play [:hand-size 3]
    :effect (effect (damage eid :brain 1 {:card card}))}

   "Brain Chip"
   (let [runner-points (fn [s] (max (get-in s [:runner :agenda-point] 0) 0))]
     {:effect (req (gain state :runner
                         :memory (runner-points @state)
                         :hand-size (runner-points @state))
                   (add-watch state (keyword (str "brainchip" (:cid card)))
                              (fn [k ref old new]
                                (let [bonus (- (runner-points new) (runner-points old))]
                                  (when-not (zero? bonus)
                                    (gain state :runner
                                          :memory bonus
                                          :hand-size bonus))))))
      :leave-play (req (remove-watch state (keyword (str "brainchip" (:cid card))))
                       (lose state :runner
                             :memory (runner-points @state)
                             :hand-size (runner-points @state)))})

   "Capstone"
   {:abilities [{:req (req (pos? (count (:hand runner))))
                 :cost [:click 1]
                 :effect (req (let [handsize (count (:hand runner))]
                                (resolve-ability
                                  state side
                                  {:prompt "Select any number of cards to trash from your Grip"
                                   :choices {:max handsize
                                             :req #(and (= (:side %) "Runner")
                                                        (in-hand? %))}
                                   :effect (req (let [trashed (count targets)
                                                      remaining (- handsize trashed)]
                                                  (doseq [c targets]
                                                    (when (not (empty? (filter #(= (:title c) (:title %))
                                                                               (all-active-installed state :runner))))
                                                      (draw state side)))
                                                  (trash-cards state side targets)
                                                  (system-msg state side
                                                              (str "spends [Click] to use Capstone to trash "
                                                                   (join ", " (map :title targets)) " and draw "
                                                                   (- (count (get-in @state [:runner :hand])) remaining) " cards"))))}
                                  card nil)))}]}

   "Chop Bot 3000"
   {:flags {:runner-phase-12 (req (>= 2 (count (all-installed state :runner))))}
    :abilities [{:msg (msg "trash " (:title target))
                 :choices {:req #(and (= (:side %) "Runner") (:installed %))
                           :not-self true}
                 :async true
                 :effect (req (wait-for (trash state :runner target)
                                        (continue-ability state side
                                                          {:prompt "Draw 1 card or remove 1 tag" :msg (msg (.toLowerCase target))
                                                           :choices ["Draw 1 card" "Remove 1 tag"]
                                                           :async true
                                                           :effect (req (if (= target "Draw 1 card")
                                                                          (draw state side eid 1 nil)
                                                                          (do (lose-tags state :runner 1)
                                                                              (effect-completed state side eid))))} card nil)))}]}

   "Clone Chip"
   {:abilities [{:prompt "Select a program to install from your Heap"
                 :priority true
                 :show-discard true
                 :req (req (and (not (seq (get-in @state [:runner :locked :discard])))
                                (not (install-locked? state side))))
                 :choices {:req #(and (is-type? % "Program")
                                      (= (:zone %) [:discard]))}
                 :effect (req (when (>= (:credit runner) (:cost target))
                                (runner-install state side target)
                                (trash state side card {:cause :ability-cost})
                                (system-msg state side (str "uses " (:title card) " to install " (:title target)))))}]}

   "Comet"
   {:in-play [:memory 1]
    :events {:play-event {:req (req (first-event? state side :play-event))
                          :effect (req (system-msg state :runner
                                                   (str "can play another event without spending a [Click] by clicking on Comet"))
                                       (update! state side (assoc card :comet-event true)))}}
    :abilities [{:req (req (:comet-event card))
                 :prompt "Select an Event in your Grip to play"
                 :choices {:req #(and (is-type? % "Event")
                                      (in-hand? %))}
                 :msg (msg "play " (:title target))
                 :effect (effect (play-instant target)
                                 (update! (dissoc (get-card state card) :comet-event)))}]}

   "Cortez Chip"
   {:abilities [{:prompt "Select a piece of ICE"
                 :choices {:req ice?}
                 :effect (req (let [ice target]
                                (update! state side (assoc card :cortez-target ice))
                                (trash state side (get-card state card) {:cause :ability-cost})
                                (system-msg state side
                                            (str "trashes Cortez Chip to increase the rez cost of " (card-str state ice)
                                                 " by 2 [Credits] until the end of the turn"))))}]
    :trash-effect {:effect (effect (register-events {:pre-rez {:req (req (= (:cid target) (:cid (:cortez-target card))))
                                                               :effect (effect (rez-cost-bonus 2))}
                                                     :runner-turn-ends {:effect (effect (unregister-events card))}
                                                     :corp-turn-ends {:effect (effect (unregister-events card))}}
                                                    (get-card state card)))}
    :events {:pre-rez nil :runner-turn-ends nil :corp-turn-ends nil}}

   "Cyberdelia"
   {:implementation "Credit gain is manually triggered."
    :in-play [:memory 1]
    :abilities [{:msg "gain 1 [Credits] for breaking all subroutines on a piece of ice"
                 :once :per-turn
                 :effect (effect (gain-credits 1))}]}

   "Cyberfeeder"
   {:recurring 1}

   "CyberSolutions Mem Chip"
   {:in-play [:memory 2]}

   "Cybsoft MacroDrive"
   {:recurring 1}

   "Daredevil"
   {:in-play [:memory 2]
    :events {:run-big {:once :per-turn
                       :req (req (first-event? state side :run-big))
                       :msg "draw two cards"
                       :async true
                       :effect (effect (draw eid 2 nil))}}}

   "Dedicated Processor"
   {:implementation "Click Dedicated Processor to use ability"
    :req (req (not-empty (filter #(has-subtype? % "Icebreaker") (all-active-installed state :runner))))
    :hosting {:req #(and (has-subtype? % "Icebreaker")
                         (not (has-subtype? % "AI"))
                         (installed? %))}
    :abilities [{:cost [:credit 2]
                 :req (req run)
                 :effect (effect (pump (get-card state (:host card)) 4))
                 :msg (msg (str "pump the strength of " (get-in card [:host :title]) " by 4"))}]}

   "Deep Red"
   {:implementation "MU use restriction not enforced"
    :in-play [:memory 3]
    :events {:runner-install
             {:optional
              {:req (req (has-subtype? target "Caïssa"))
               :prompt "Use Deep Red?" :priority 1
               :yes-ability {:async true
                             :effect (req (let [cid (:cid target)]
                                            (continue-ability
                                              state side
                                              {:async true
                                               :prompt "Choose the just-installed Caïssa to have Deep Red trigger its [Click] ability"
                                               :choices {:req #(= cid (:cid %))}
                                               :msg (msg "trigger the [Click] ability of " (:title target)
                                                         " without spending [Click]")
                                               :effect (req (gain state :runner :click 1)
                                                            (play-ability state side {:card target :ability 0})
                                                            (effect-completed state side eid))}
                                              card nil)))}}}}}

   "Desperado"
   {:in-play [:memory 1]
    :events {:successful-run {:silent (req true)
                              :msg "gain 1 [Credits]" :effect (effect (gain-credits 1))}}}

   "Dinosaurus"
   {:abilities [{:label "Install a non-AI icebreaker on Dinosaurus"
                 :req (req (empty? (:hosted card))) :cost [:click 1]
                 :prompt "Select a non-AI icebreaker in your Grip to install on Dinosaurus"
                 :choices {:req #(and (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "AI"))
                                      (in-hand? %))}
                 :effect (effect (runner-install target {:host-card card :no-mu true})
                                 (update! (assoc-in (get-card state card) [:special :dino-breaker] (:cid target))))}
                {:label "Host an installed non-AI icebreaker on Dinosaurus"
                 :req (req (empty? (:hosted card)))
                 :prompt "Select an installed non-AI icebreaker to host on Dinosaurus"
                 :choices {:req #(and (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "AI"))
                                      (installed? %))}
                 :msg (msg "host " (:title target))
                 :effect (req (free-mu state (:memoryunits target))
                              (->> target
                                   (get-card state)
                                   (host state side card)
                                   (update-breaker-strength state side))
                              (update! state side (assoc-in (get-card state card) [:special :dino-breaker] (:cid target))))}]
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (first (:hosted card)))))
                                    :effect (effect (breaker-strength-bonus 2))}
             :card-moved {:req (req (= (:cid target) (get-in (get-card state card) [:special :dino-breaker])))
                          :effect (effect (update! (dissoc-in card [:special :dino-breaker]))
                                          (use-mu (:memoryunits target)))}}}

   "Doppelgänger"
   {:in-play [:memory 1]
    :events {:runner-install
             {:req (req (= card target))
              :silent (req true)
              :effect (effect (update! (assoc card :dopp-active true)))}
             :runner-turn-begins
             {:effect (effect (update! (assoc card :dopp-active true)))}
             :successful-run-ends
             {:interactive (req true)
              :optional
              {:req (req (:dopp-active card))
               :player :runner
               :prompt "Use Doppelgänger to run again?"
               :yes-ability {:prompt "Choose a server"
                             :async true
                             :choices (req runnable-servers)
                             :msg (msg "make a run on " target)
                             :makes-run true
                             :effect (effect (update! (dissoc card :dopp-active))
                                             (clear-wait-prompt :corp)
                                             (make-run eid target))}}}}}

   "Dorm Computer"
   {:data {:counter {:power 4}}
    :abilities [{:counter-cost [:power 1]
                 :cost [:click 1]
                 :req (req (not run))
                 :prompt "Choose a server"
                 :choices (req runnable-servers)
                 :msg "make a run and avoid all tags for the remainder of the run"
                 :makes-run true
                 :effect (effect (update! (assoc card :dorm-active true))
                                 (make-run target))}]
    :events {:pre-tag {:req (req (:dorm-active card))
                       :effect (effect (tag-prevent :runner Integer/MAX_VALUE))
                       :msg "avoid all tags during the run"}
             :run-ends {:effect (effect (update! (dissoc card :dorm-active)))}}}

   "Dyson Fractal Generator"
   {:recurring 1}

   "Dyson Mem Chip"
   {:in-play [:memory 1 :link 1]}

   "e3 Feedback Implants"
   {:implementation "Usage restriction not enforced"
    :abilities [{:cost [:credit 1] :msg "break 1 additional subroutine"}]}

   "Ekomind"
   (let [update-base-mu (fn [state n] (swap! state assoc-in [:runner :memory :base] n))]
     {:effect (req (update-base-mu state (count (get-in @state [:runner :hand])))
                   (add-watch state :ekomind (fn [k ref old new]
                                               (let [hand-size (count (get-in new [:runner :hand]))]
                                                 (when (not= (count (get-in old [:runner :hand])) hand-size)
                                                   (update-base-mu ref hand-size))))))
      :leave-play (req (remove-watch state :ekomind))})

   "EMP Device"
   {:abilities [{:req (req (:run @state))
                 :msg "prevent the Corp from rezzing more than 1 piece of ICE for the remainder of the run"
                 :effect (effect (register-events
                                   {:rez {:req (req (ice? target))
                                          :effect (effect (register-run-flag!
                                                            card :can-rez
                                                            (fn [state side card]
                                                              (if (ice? card)
                                                                ((constantly false)
                                                                 (toast state :corp "Cannot rez ICE the rest of this run due to EMP Device"))
                                                                true))))}
                                    :run-ends {:effect (effect (unregister-events card))}} (assoc card :zone '(:discard)))
                                 (trash card {:cause :ability-cost}))}]
    :events {:rez nil
             :run-ends nil}}

   "Feedback Filter"
   {:interactions {:prevent [{:type #{:net :brain}
                              :req (req true)}]}
    :abilities [{:cost [:credit 3]
                 :msg "prevent 1 net damage"
                 :effect (effect (damage-prevent :net 1))}
                {:label "[Trash]: Prevent up to 2 brain damage"
                 :msg "prevent up to 2 brain damage"
                 :effect (effect (trash card {:cause :ability-cost})
                                 (damage-prevent :brain 2))}]}

   "Flame-out"
   (let [turn-end {:async true
                   :effect (req (unregister-events state :runner card)
                                (if-let [hosted (first (:hosted card))]
                                  (do
                                    (system-msg state :runner (str "trashes " (:title hosted) " from Flame-out"))
                                    (trash state side eid hosted nil))
                                  (effect-completed state side eid)))}]
     {:implementation "Credit usage restriction not enforced"
      :data {:counter {:credit 9}}
      :abilities [{:label "Take 1 [Credits] from Flame-out"
                   :req (req (and (not-empty (:hosted card))
                                  (pos? (get-counters card :credit))))
                   :counter-cost [:credit 1]
                   :effect (req (gain-credits state :runner 1)
                                (system-msg state :runner "takes 1 [Credits] from Flame-out")
                                (register-events
                                  state :runner
                                  {:runner-turn-ends turn-end
                                   :corp-turn-ends turn-end}
                                  (get-card state card)))}
                  {:label "Take all [Credits] from Flame-out"
                   :req (req (and (not-empty (:hosted card))
                                  (pos? (get-counters card :credit))))
                   :effect (req (let [credits (get-counters card :credit)]
                                  (gain-credits state :runner credits)
                                  (update! state :runner (dissoc-in card [:counter :credit]))
                                  (system-msg state :runner (str "takes " credits "[Credits] from Flame-out"))
                                  (register-events
                                    state :runner
                                    {:runner-turn-ends turn-end
                                     :corp-turn-ends turn-end}
                                    (get-card state card))))}
                  {:label "Install a program on Flame-out"
                   :req (req (empty? (:hosted card)))
                   :cost [:click 1]
                   :prompt "Select a program in your Grip to install on Flame-out"
                   :choices {:req #(and (is-type? % "Program")
                                        (in-hand? %))}
                   :effect (effect (runner-install target {:host-card card})
                                   (update! (assoc-in (get-card state card) [:special :flame-out] (:cid target))))}
                  {:label "Host an installed program on Flame-out"
                   :req (req (empty? (:hosted card)))
                   :prompt "Select an installed program to host on Flame-out"
                   :choices {:req #(and (is-type? % "Program")
                                        (installed? %))}
                   :msg (msg "host " (:title target))
                   :effect (req (->> target
                                     (get-card state)
                                     (host state side card))
                                (update! state side (assoc-in (get-card state card) [:special :flame-out] (:cid target))))}]
      :events {:card-moved {:req (req (= (:cid target) (get-in (get-card state card) [:special :flame-out])))
                            :effect (effect (update! (dissoc-in card [:special :flame-out])))}
               :runner-turn-ends nil
               :corp-turn-ends nil}})

   "Forger"
   {:interactions {:prevent [{:type #{:tag}
                              :req (req true)}]}
    :in-play [:link 1]
    :abilities [{:msg "avoid 1 tag" :label "[Trash]: Avoid 1 tag"
                 :effect (effect (tag-prevent :runner 1) (trash card {:cause :ability-cost}))}
                {:msg "remove 1 tag" :label "[Trash]: Remove 1 tag"
                 :effect (effect (trash card {:cause :ability-cost}) (lose-tags 1))}]}

   "Friday Chip"
   (let [ability {:msg (msg "move 1 virus counter to " (:title target))
                  :req (req (and (pos? (get-counters card :virus))
                                 (pos? (count-virus-programs state))))
                  :choices {:req is-virus-program?}
                  :effect (req (add-counter state :runner card :virus -1)
                               (add-counter state :runner target :virus 1))}]
     {:abilities [(set-autoresolve :auto-accept "Friday Chip")]
      :effect (effect (toast "Tip: You can toggle automatically adding virus counters by clicking Friday Chip."))
      :events {:runner-turn-begins ability
               :runner-trash {:async true
                              :req (req (some #(card-is? % :side :corp) targets))
                              :effect (req (let [amt-trashed (count (filter #(card-is? % :side :corp) targets))
                                                 sing-ab {:optional {:prompt "Place a virus counter on Friday Chip?"
                                                                     :autoresolve (get-autoresolve :auto-accept)
                                                                     :yes-ability {:effect (effect (system-msg
                                                                                                     :runner
                                                                                                     "places 1 virus counter on Friday Chip")
                                                                                                   (add-counter :runner card :virus 1))}}}
                                                 mult-ab {:prompt "Place virus counters on Friday Chip?"
                                                          :choices {:number (req amt-trashed)
                                                                    :default (req amt-trashed)}
                                                          :effect (effect (system-msg :runner
                                                                                      (str "places "
                                                                                           (quantify target "virus counter")
                                                                                           " on Friday Chip"))
                                                                          (add-counter :runner card :virus target))}
                                                 ab (if (> amt-trashed 1) mult-ab sing-ab)]
                                             (continue-ability state side ab card targets)))}}})

   "Gebrselassie"
   {:abilities [{:msg (msg "host it on an installed non-AI icebreaker")
                 :cost [:click 1]
                 :choices {:req #(and (installed? %)
                                      (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "AI")))}
                 :effect (req (when-let [host (get-card state (:host card))]
                                (update! state side (dissoc-in host [:pump :all-turn]))
                                (update-breaker-strength state side host))
                              (host state side target card))}]
    :events {:pump-breaker {:silent (req true)
                            :req (req (= (:cid (second targets)) (:cid (:host card))))
                            :effect (effect (update! (update-in (second targets) [:pump :all-turn] (fnil #(+ % (first targets)) 0)))
                                            (update-breaker-strength (second targets)))}}
    :leave-play (req (when-let [host (get-card state (:host card))]
                       (update! state side (dissoc-in host [:pump :all-turn]))
                       (update-breaker-strength state side host)))}

   "GPI Net Tap"
   {:implementation "Trash and jack out effect is manual"
    :abilities [{:req (req (and (ice? current-ice) (not (rezzed? current-ice))))
                 :async true
                 :effect (effect (expose eid current-ice))}]}

   "Grimoire"
   {:in-play [:memory 2]
    :events {:runner-install {:silent (req true)
                              :req (req (has-subtype? target "Virus"))
                              :effect (effect (add-counter target :virus 1))}}}

   "Heartbeat"
   {:in-play [:memory 1]
    :interactions {:prevent [{:type #{:net :brain :meat}
                              :req (req true)}]}
    :abilities [{:msg (msg "prevent 1 damage, trashing a facedown " (:title target))
                 :choices {:req #(and (= (:side %) "Runner") (:installed %))}
                 :priority 50
                 :effect (effect (trash target {:unpreventable true})
                                 (damage-prevent :brain 1)
                                 (damage-prevent :meat 1)
                                 (damage-prevent :net 1))}]}

   "Hijacked Router"
   {:events {:server-created {:effect (effect (lose-credits :corp 1))
                              :msg "force the Corp to lose 1 [Credits]"}
             :successful-run {:req (req (= target :archives))
                              :optional {:prompt "Trash Hijacked Router to force the Corp to lose 3 [Credits]?"
                                         :yes-ability {:async true
                                                       :effect (req (system-msg state :runner "trashes Hijacked Router to force the Corp to lose 3 [Credits]")
                                                                    (wait-for (trash state :runner card {:unpreventable true})
                                                                              (lose-credits state :corp 3)
                                                                              (effect-completed state side eid)))}}}}}

   "Hippo"
   {:implementation "Subroutine and first encounter requirements not enforced"
    :abilities [{:label "Remove Hippo from the game: trash outermost piece of ICE if all subroutines were broken"
                 :req (req (and run
                                (pos? (count run-ices))))
                 :async true
                 :effect (req (let [ice (last run-ices)]
                                (system-msg
                                  state :runner
                                  (str "removes Hippo from the game to trash " (card-str state ice)))
                                (move state :runner card :rfg)
                                (trash state :runner eid ice nil)))}]}

   "HQ Interface"
   {:in-play [:hq-access 1]}

   "Knobkierie"
   {:implementation "MU usage restriction not enforced"
    :in-play [:memory 3]
    :events {:successful-run
             {:req (req (and (first-event? state :runner :successful-run)
                             (pos? (count-virus-programs state))))
              :optional {:prompt "Place a virus counter?"
                         :autoresolve (get-autoresolve :auto-add)
                         :yes-ability {:prompt "Select an installed virus program for Knobkierie to add a virus counter to"
                                       :choices {:req #(and (installed? %)
                                                            (has-subtype? % "Virus")
                                                            (is-type? % "Program"))}
                                       :msg (msg "place 1 virus counter on " (:title target))
                                       :effect (effect (add-counter target :virus 1))}}}}
    :abilities [(set-autoresolve :auto-add "Knobkierie")]}

   "Lemuria Codecracker"
   {:abilities [{:cost [:click 1 :credit 1]
                 :req (req (some #{:hq} (:successful-run runner-reg)))
                 :choices {:req installed?}
                 :effect (effect (expose eid target))
                 :msg "expose 1 card"}]}

   "LLDS Memory Diamond"
   {:in-play [:link 1 :memory 1 :hand-size 1]}

   "LLDS Processor"
   (let [llds {:effect (req (let [cards (:llds-target card)]
                              (update! state side (dissoc card :llds-target))
                              (doseq [c cards]
                                (update-breaker-strength state side
                                                         (find-cid (:cid c) (all-active-installed state :runner))))))}]
     {:events
      {:runner-turn-ends llds :corp-turn-ends llds
       :runner-install {:silent (req true)
                        :req (req (has-subtype? target "Icebreaker"))
                        :effect (effect (update! (update-in card [:llds-target] #(conj % target)))
                                        (update-breaker-strength target))}
       :pre-breaker-strength {:req (req (some #(= (:cid target) (:cid %)) (:llds-target card)))
                              :effect (effect (breaker-strength-bonus 1))}}})

   "Lockpick"
   {:recurring 1}

   "Logos"
   {:in-play [:memory 1 :hand-size 1]
    :events {:agenda-scored
             {:player :runner :prompt "Choose a card" :msg (msg "add 1 card to their Grip from their Stack")
              :choices (req (cancellable (:deck runner)))
              :effect (effect (trigger-event :searched-stack nil)
                              (shuffle! :deck)
                              (move target :hand))}}}

   "Mâché"
   {:abilities [{:label "Draw 1 card"
                 :msg "draw 1 card"
                 :counter-cost [:power 3]
                 :async true
                 :effect (effect (draw :runner eid 1 nil))}]
    :events {:runner-trash {:once :per-turn
                            :req (req (and (card-is? target :side :corp)
                                           (:access @state)
                                           (:trash target)))
                            :effect (effect (system-msg (str "places " (:trash target) " power counters on Mâché"))
                                            (add-counter card :power (:trash target)))}}}

   "Māui"
   {:in-play [:memory 2]
    :recurring (effect (set-prop card :rec-counter (count (:ices (get-in @state [:corp :servers :hq])))))
    :effect (effect (set-prop card :rec-counter (count (:ices (get-in @state [:corp :servers :hq])))))}

   "Maw"
   (let [ability {:label "Trash a card from HQ"
                  :req (req (and (= 1 (get-in @state [:runner :register :no-trash-or-steal]))
                                 (pos? (count (:hand corp)))
                                 (not= (first (:zone target)) :discard)))
                  :once :per-turn
                  :msg "force the Corp to trash a random card from HQ"
                  :effect (req (let [card-to-trash (first (shuffle (:hand corp)))
                                     card-seen? (= (:cid target) (:cid card-to-trash))
                                     card-to-trash (if card-seen? (assoc card-to-trash :seen true)
                                                     card-to-trash)]
                                 ;; toggle access flag to prevent Hiro issue #2638
                                 (swap! state dissoc :access)
                                 (trash state :corp card-to-trash)
                                 (swap! state assoc :access true)))}]
     {:in-play [:memory 2]
      :abilities [ability]
      :events {:post-access-card ability}})

   "Maya"
   {:in-play [:memory 2]
    :abilities [{:once :per-turn
                 :async true
                 :label "Move this accessed card to bottom of R&D"
                 :req (req (when-let [accessed-card (-> @state :runner :prompt first :card)]
                             (in-deck? accessed-card)))
                 :msg "move the card just accessed to the bottom of R&D"
                 :effect (req (let [accessed-card (-> @state :runner :prompt first :card)]
                                (move state :corp accessed-card :deck)
                                (wait-for (gain-tags state :runner (make-eid state) 1)
                                          (close-access-prompt state side))))}
                {:once :per-turn
                 :label "Move a previously accessed card to bottom of R&D"
                 :effect (effect (resolve-ability
                                   {:async true
                                    ;; only allow targeting cards that were accessed this turn
                                    :choices {:req #(some (fn [accessed-card]
                                                            (= (:cid %) (:cid accessed-card)))
                                                          (map first (turn-events state side :access)))}
                                    :msg (msg "move " (:title target) " to the bottom of R&D")
                                    :effect (req (move state :corp target :deck)
                                                 (gain-tags state :runner eid 1)
                                                 (swap! state update-in [side :prompt] rest)
                                                 (when-let [run (:run @state)]
                                                   (when (and (:ended run)
                                                              (empty? (get-in @state [:runner :prompt])))
                                                     (handle-end-run state :runner))))}
                                   card nil))}]}

   "MemStrips"
   {:implementation "MU usage restriction not enforced"
    :in-play [:memory 3]}

   "Mind's Eye"
   {:in-play [:memory 1]
    :implementation "Power counters added automatically"
    :events {:successful-run {:silent (req true)
                              :req (req (= target :rd))
                              :effect (effect (add-counter card :power 1))}}
    :abilities [{:async true
                 :cost [:click 1]
                 :counter-cost [:power 3]
                 :msg "access the top card of R&D"
                 :effect (req (do-access state side eid [:rd] {:no-root true}))}]}

   "Mirror"
   {:in-play [:memory 2]
    :events {:successful-run
             {:async true
              :req (req (= target :rd))
              :effect (effect (continue-ability
                                {:prompt "Select a card and replace 1 spent [Recurring Credits] on it"
                                 :choices {:req #(< (get-counters % :recurring) (:recurring (card-def %) 0))}
                                 :msg (msg "replace 1 spent [Recurring Credits] on " (:title target))
                                 :effect (effect (add-prop target :rec-counter 1))}
                                card nil))}}}

   "Monolith"
   (let [mhelper (fn mh [n] {:prompt "Select a program to install"
                             :choices {:req #(and (is-type? % "Program")
                                                  (in-hand? %))}
                             :effect (req (install-cost-bonus state side [:credit -4])
                                          (runner-install state side target nil)
                                          (when (< n 3)
                                            (resolve-ability state side (mh (inc n)) card nil)))})]
     {:interactions {:prevent [{:type #{:net :brain}
                                :req (req true)}]}
      :in-play [:memory 3]
      :effect (effect (resolve-ability (mhelper 1) card nil))
      :abilities [{:msg (msg "prevent 1 brain or net damage by trashing " (:title target))
                   :priority 50
                   :choices {:req #(and (is-type? % "Program")
                                        (in-hand? %))}
                   :prompt "Choose a program to trash from your Grip"
                   :effect (effect (trash target)
                                   (damage-prevent :brain 1)
                                   (damage-prevent :net 1))}]})

   "Muresh Bodysuit"
   {:events {:pre-damage {:once :per-turn :once-key :muresh-bodysuit
                          :req (req (= target :meat))
                          :msg "prevent the first meat damage this turn"
                          :effect (effect (damage-prevent :meat 1))}}}

   "Net-Ready Eyes"
   {:effect (effect (damage eid :meat 2 {:unboostable true :card card})) :msg "suffer 2 meat damage"
    :events {:run {:choices {:req #(and (installed? %)
                                        (has-subtype? % "Icebreaker"))}
                   :msg (msg "give " (:title target) " +1 strength")
                   :effect (effect (pump target 1 :all-run))}}}

   "NetChip"
   {:abilities [{:label "Install a program on NetChip"
                 :req (req (empty? (:hosted card)))
                 :effect (req (let [n (count (filter #(= (:title %) (:title card)) (all-active-installed state :runner)))]
                                (resolve-ability
                                  state side
                                  {:cost [:click 1]
                                   :prompt "Select a program in your Grip to install on NetChip"
                                   :choices {:req #(and (is-type? % "Program")
                                                        (runner-can-install? state side % false)
                                                        (<= (:memoryunits %) n)
                                                        (in-hand? %))}
                                   :msg (msg "host " (:title target))
                                   :effect (effect (runner-install target {:host-card card :no-mu true})
                                                   (update! (assoc (get-card state card)
                                                                   :hosted-programs
                                                                   (cons (:cid target) (:hosted-programs card)))))}
                                  card nil)))}
                {:label "Host an installed program on NetChip"
                 :req (req (empty? (:hosted card)))
                 :effect (req (let [n (count (filter #(= (:title %) (:title card)) (all-active-installed state :runner)))]
                                (resolve-ability
                                  state side
                                  {:prompt "Select an installed program to host on NetChip"
                                   :choices {:req #(and (is-type? % "Program")
                                                        (<= (:memoryunits %) n)
                                                        (installed? %))}
                                   :msg (msg "host " (:title target))
                                   :effect (effect (host card target)
                                                   (free-mu (:memoryunits target))
                                                   (update! (assoc (get-card state card)
                                                                   :hosted-programs
                                                                   (cons (:cid target) (:hosted-programs card)))))}
                                  card nil)))}]
    :events {:card-moved {:req (req (some #{(:cid target)} (:hosted-programs card)))
                          :effect (effect (update! (assoc card
                                                          :hosted-programs
                                                          (remove #(= (:cid target) %) (:hosted-programs card))))
                                          (use-mu (:memoryunits target)))}}}

   "Obelus"
   {:in-play [:memory 1]
    :effect (req (change-hand-size state :runner (count-tags state)))
    :leave-play (req (change-hand-size state :runner (- (count-tags state))))
    :events {:successful-run-ends {:once :per-turn
                                   :req (req (and (#{:rd :hq} (first (:server target)))
                                                  (first-event? state side :successful-run-ends
                                                                #(#{:rd :hq} (first (:server (first %)))))))
                                   :msg (msg "draw " (total-cards-accessed target) " cards")
                                   :async true
                                   :effect (effect (draw eid (total-cards-accessed target) nil))}
             ;; Events for tracking hand size
             :runner-gain-tag {:effect (req (change-hand-size state :runner target))}
             :runner-lose-tag {:effect (req (change-hand-size state :runner (- target)))}
             :runner-additional-tag-change {:effect (req (change-hand-size state :runner target))}}}

   "Omni-drive"
   {:recurring 1
    :abilities [{:label "Install and host a program of 1[Memory Unit] or less on Omni-drive"
                 :req (req (empty? (:hosted card)))
                 :cost [:click 1]
                 :prompt "Select a program of 1[Memory Unit] or less to install on Omni-drive from your grip"
                 :choices {:req #(and (is-type? % "Program")
                                      (<= (:memoryunits %) 1)
                                      (in-hand? %))}
                 :msg (msg "host " (:title target))
                 :effect (effect (runner-install target {:host-card card :no-mu true})
                                 (update! (assoc (get-card state card) :Omnidrive-prog (:cid target))))}
                {:label "Host an installed program of 1[Memory Unit] or less on Omni-drive"
                 :prompt "Select an installed program of 1[Memory Unit] or less to host on Omni-drive"
                 :choices {:req #(and (is-type? % "Program")
                                      (<= (:memoryunits %) 1)
                                      (installed? %))}
                 :msg (msg "host " (:title target))
                 :effect (effect (host card target)
                                 (free-mu (:memoryunits target))
                                 (update! (assoc (get-card state card) :Omnidrive-prog (:cid target))))}]
    :events {:card-moved {:req (req (= (:cid target) (:Omnidrive-prog (get-card state card))))
                          :effect (effect (update! (dissoc card :Omnidrive-prog))
                                          (use-mu (:memoryunits target)))}}}

   "Paragon"
   {:in-play [:memory 1]
    :events {:successful-run
             {:req (req (first-event? state side :successful-run))
              :async true
              :interactive (get-autoresolve :autofire (complement never?))
              :silent (get-autoresolve :autofire never?)
              :effect (effect
                        (show-wait-prompt :corp "Runner to decide if they will use Paragon")
                        (continue-ability
                          {:optional
                           {:player :runner
                            :autoresolve (get-autoresolve :auto-fire)
                            :prompt "Use Paragon?"
                            :yes-ability
                            {:msg "gain 1 [Credit] and look at the top card of Stack"
                             :async true
                             :effect (effect
                                       (gain-credits :runner 1)
                                       (continue-ability
                                         {:player :runner
                                          :optional
                                          {:prompt (msg "Add " (:title (first (:deck runner))) " to bottom of Stack?")
                                           :yes-ability
                                           {:msg "add the top card of Stack to the bottom"
                                            :effect (effect (move :runner (first (:deck runner)) :deck)
                                                            (clear-wait-prompt :corp))}
                                           :no-ability {:effect (effect (clear-wait-prompt :corp))}}}
                                         card nil))}
                            :no-ability {:effect (effect (clear-wait-prompt :corp)
                                                         (system-msg "does not add the top card of the Stack to the bottom"))}}}
                          card nil))}}
    :abilities [(set-autoresolve :auto-fire "Paragon")]}

   "Patchwork"
   (letfn [(patchwork-discount [cost-type bonus-fn]
             {:async true
              :req (req (and (get-in card [:special :patchwork])
                             (= "Runner" (:side target))
                             ;; We need at least one card (that is not the card played) in hand
                             (not-empty (remove (partial same-card? target) (:hand runner)))))
              :effect (req (let [playing target]
                             (continue-ability
                               state side
                               {:prompt (str "Trash a card to lower the " cost-type " cost of " (:title playing) " by 2 [Credits].")
                                :priority 2
                                :choices {:req #(and (in-hand? %)
                                                     (= "Runner" (:side %))
                                                     (not (same-card? % playing)))}
                                :msg (msg "trash " (:title target) " to lower the " cost-type " cost of "
                                          (:title playing) " by 2 [Credits]")
                                :effect (effect (trash target {:unpreventable true})
                                                (bonus-fn [:credit -2])
                                                (update! (dissoc-in card [:special :patchwork])))
                                :cancel-effect (effect (effect-completed eid))}
                               card nil)))})]
     {:in-play [:memory 1]
      :implementation "Click Patchwork before playing/installing a card."
      :abilities [{:once :per-turn
                   :effect (effect (update! (assoc-in card [:special :patchwork] true))
                             (toast "Your next card played will trigger Patchwork." "info"))}]
      :events {:pre-play-instant (patchwork-discount "play" play-cost-bonus)
               :pre-install (patchwork-discount "install" install-cost-bonus)
               :runner-turn-ends {:effect (effect (update! (dissoc-in card [:special :patchwork])))}
               :corp-turn-ends {:effect (effect (update! (dissoc-in card [:special :patchwork])))}}})

   "Plascrete Carapace"
   {:data [:counter {:power 4}]
    :interactions {:prevent [{:type #{:meat}
                              :req (req true)}]}
    :abilities [{:counter-cost [:power 1]
                 :msg "prevent 1 meat damage"
                 :effect (req (damage-prevent state side :meat 1)
                              (when (zero? (get-counters (get-card state card) :power))
                                (trash state side card {:unpreventable true})))}]}

   "Polyhistor"
   (let [abi {:optional
              {:prompt "Draw 1 card to force the Corp to draw 1 card?"
               :yes-ability {:msg "draw 1 card and force the Corp to draw 1 card"
                             :async true
                             :effect (req (wait-for (draw state :runner 1)
                                                    (draw state :corp eid 1 nil)))}
               :no-ability {:effect (req (system-msg state side (str "does not use Polyhistor"))
                                         (effect-completed state side eid))}}}]
     {:in-play [:link 1 :memory 1]
      :events {:pass-ice {:req (req (and (= (:server run) [:hq])
                                         (= (:position run) 1) ; trigger when last ICE passed
                                         (pos? (count (:deck runner)))))
                          :async true
                          :once :per-turn
                          :effect (req (continue-ability state :runner abi card nil))}
               :run {:req (req (and (= (:server run) [:hq])
                                    (zero? (:position run)) ; trigger on unprotected HQ
                                    (pos? (count (:deck runner)))))
                     :async true
                     :once :per-turn
                     :effect (req (continue-ability state :runner abi card nil))}}})

   "Prepaid VoicePAD"
   {:recurring 1}

   "Public Terminal"
   {:recurring 1}

   "Q-Coherence Chip"
   {:in-play [:memory 1]
    :events (let [e {:req (req (= (last (:zone target)) :program))
                     :effect (effect (trash card)
                                     (system-msg (str "trashes Q-Coherence Chip")))}]
              {:runner-trash e :corp-trash e})}

   "Qianju PT"
   {:flags {:runner-phase-12 (req true)}
    :abilities [{:label "Lose [Click], avoid 1 tag (start of turn)"
                 :once :per-turn
                 :req (req (:runner-phase-12 @state))
                 :effect (effect (update! (assoc card :qianju-active true)))
                 :msg "lose [Click] and avoid the first tag received until their next turn"}]
    :events {:corp-turn-ends {:effect (effect (update! (dissoc card :qianju-active)))}
             :runner-turn-begins {:req (req (:qianju-active card))
                                  :effect (effect (lose :click 1))}
             :pre-tag {:req (req (:qianju-active card))
                       :msg "avoid the first tag received"
                       :effect (effect (tag-prevent :runner 1)
                                       (update! (dissoc card :qianju-active)))}}}

   "R&D Interface"
   {:in-play [:rd-access 1]}

   "Rabbit Hole"
   {:in-play [:link 1]
    :effect
    (effect (resolve-ability
              {:optional {:req (req (some #(when (= (:title %) "Rabbit Hole") %) (:deck runner)))
                          :prompt "Install another Rabbit Hole?" :msg "install another Rabbit Hole"
                          :yes-ability {:effect (req (when-let [c (some #(when (= (:title %) "Rabbit Hole") %)
                                                                        (:deck runner))]
                                                       (trigger-event state side :searched-stack nil)
                                                       (shuffle! state :runner :deck)
                                                       (runner-install state side c)))}}} card nil))}

   "Ramujan-reliant 550 BMI"
   {:interactions {:prevent [{:type #{:net :brain}
                              :req (req true)}]}
    :abilities [{:req (req (not-empty (:deck runner)))
                 :effect (req (let [n (count (filter #(= (:title %) (:title card)) (all-active-installed state :runner)))]
                                (resolve-ability
                                  state side
                                  {:prompt "Choose how much damage to prevent"
                                   :priority 50
                                   :choices {:number (req (min n (count (:deck runner))))}
                                   :msg (msg "trash " (join ", " (map :title (take target (:deck runner))))
                                             " from their Stack and prevent " target " damage")
                                   :effect (effect (damage-prevent :net target)
                                                   (damage-prevent :brain target)
                                                   (mill :runner target)
                                                   (trash card {:cause :ability-cost}))} card nil)))}]}

   "Recon Drone"
   ; eventmap uses reverse so we get the most recent event of each kind into map
   (letfn [(eventmap [s]
             (into {} (reverse (get s :turn-events))))]
     {:interactions {:prevent [{:type #{:net :brain :meat}
                                :req (req (:access @state))}]}
      :abilities [{:req (req (= (:cid (second (:pre-damage (eventmap @state))))
                                (:cid (first (:pre-access-card (eventmap @state))))))
                   :effect (effect (resolve-ability
                                     {:prompt "Choose how much damage to prevent"
                                      :priority 50
                                      :choices {:number (req (min (last (:pre-damage (eventmap @state)))
                                                                  (:credit runner)))}
                                      :msg (msg "prevent " target " damage")
                                      :effect (effect (trash card {:cause :ability-cost})
                                                      (damage-prevent (first (:pre-damage (eventmap @state))) target)
                                                      (lose-credits target))}
                                     card nil))}]})

   "Record Reconstructor"
   {:events
    {:successful-run
     {:req (req (= (get-in @state [:run :server]) [:archives]))
      :effect (req (let [rr card]
                     (swap! state assoc-in [:run :run-effect :replace-access]
                            {:effect (effect (resolve-ability
                                               {:prompt "Choose one faceup card to add to the top of R&D"
                                                :choices (req (filter #(:seen %) (:discard corp)))
                                                :msg (msg "add " (:title target) " to the top of R&D")
                                                :effect (req (move state :corp target :deck {:front true}))}
                                               rr nil))})))}}}

   "Reflection"
   {:in-play [:memory 1 :link 1]
    :events {:jack-out {:msg (msg "force the Corp to reveal "
                                  (:title (first (shuffle (:hand corp))))
                                  " from HQ")}}}

   "Replicator"
   (letfn [(hardware-and-in-deck? [target runner]
             (and (is-type? target "Hardware")
                  (some #(= (:title %) (:title target)) (:deck runner))))]
     {:events {:runner-install
               {:interactive (req (hardware-and-in-deck? target runner))
                :silent (req (not (hardware-and-in-deck? target runner)))
                :optional {:prompt "Use Replicator to add a copy?"
                           :req (req (hardware-and-in-deck? target runner))
                           :yes-ability {:msg (msg "add a copy of " (:title target) " to their Grip")
                                         :effect (effect (trigger-event :searched-stack nil)
                                                   (shuffle! :deck)
                                                   (move (some #(when (= (:title %) (:title target)) %)
                                                               (:deck runner)) :hand))}}}}})

   "Respirocytes"
   (let [ability {:once :per-turn
                  :msg "draw 1 card and add a power counter to itself"
                  :async true
                  :effect (req (wait-for (draw state :runner 1 nil)
                                         (do (add-counter state side (get-card state card) :power 1)
                                             (if (= (get-counters (get-card state card) :power) 3)
                                               (do (system-msg state :runner "trashes Respirocytes as it reached 3 power counters")
                                                   (trash state side eid card {:unpreventable true}))
                                               (effect-completed state side eid)))))}
         watch-id (fn [card] (keyword (str "respirocytes-" (:cid card))))]
     {:effect (req (add-watch state (watch-id card)
                              (fn [k ref old new]
                                (when (and (seq (get-in old [:runner :hand]))
                                           (empty? (get-in new [:runner :hand])))
                                  (resolve-ability ref side ability card nil))))
                   (damage state side eid :meat 1 {:unboostable true :card card}))
      :msg "suffer 1 meat damage"
      :trash-effect {:effect (req (remove-watch state (watch-id card)))}
      :leave-play (req (remove-watch state (watch-id card)))
      :events {:runner-turn-begins {:req (req (empty? (get-in @state [:runner :hand])))
                                    :effect (effect (resolve-ability ability card nil))}
               :corp-turn-begins {:req (req (empty? (get-in @state [:runner :hand])))
                                  :effect (effect (resolve-ability ability card nil))}}})

   "Rubicon Switch"
   {:abilities [{:cost [:click 1]
                 :once :per-turn
                 :async true
                 :prompt "How many [Credits]?" :choices :credit
                 :effect (effect (system-msg (str "spends a [Click] and " target " [Credit] on Rubicon Switch"))
                                 (resolve-ability {:choices {:req #(and (ice? %)
                                                                        (= :this-turn (:rezzed %))
                                                                        (<= (:cost %) target))}
                                                   :effect (effect (derez target))
                                                   :msg (msg "derez " (:title target))} card nil))}]}

   "Security Chip"
   {:abilities [{:label "[Trash]: Add [Link] strength to a non-Cloud icebreaker until the end of the run"
                 :msg (msg "add " (:link runner) " strength to " (:title target) " until the end of the run")
                 :req (req (:run @state))
                 :prompt "Select one non-Cloud icebreaker"
                 :choices {:req #(and (has-subtype? % "Icebreaker")
                                      (not (has-subtype? % "Cloud"))
                                      (installed? %))}
                 :effect (effect (pump target (:link runner) :all-run)
                                 (trash (get-card state card) {:cause :ability-cost}))}
                {:label "[Trash]: Add [Link] strength to any Cloud icebreakers until the end of the run"
                 :msg (msg "add " (:link runner) " strength to " (count targets) " Cloud icebreakers until the end of the run")
                 :req (req (:run @state))
                 :prompt "Select any number of Cloud icebreakers"
                 :choices {:max 50
                           :req #(and (has-subtype? % "Icebreaker")
                                      (has-subtype? % "Cloud")
                                      (installed? %))}
                 :effect (req (doseq [t targets]
                                (pump state side t (:link runner) :all-run)
                                (update-breaker-strength state side t))
                              (trash state side (get-card state card) {:cause :ability-cost}))}]}

   "Security Nexus"
   {:implementation "Bypass is manual"
    :in-play [:memory 1 :link 1]
    :abilities [{:req (req (:run @state))
                 :once :per-turn
                 :async true
                 :msg "force the Corp to initiate a trace"
                 :label "Trace 5 - Give the Runner 1 tag and end the run"
                 :trace {:base 5
                         :successful {:msg "give the Runner 1 tag and end the run"
                                      :effect (effect (gain-tags :runner eid 1)
                                                      (end-run))}
                         :unsuccessful {:msg "bypass the current ICE"}}}]}

   "Severnius Stim Implant"
   {:abilities [{:cost [:click 1]
                 :prompt "Choose a server to run with Severnius Stim Implant"
                 :choices ["HQ" "R&D"]
                 :effect (req (let [n (count (:hand runner))
                                    srv target
                                    kw (if (= "R&D" target) :rd :hq)]
                                (resolve-ability
                                  state side
                                  {:prompt "Choose at least 2 cards in your Grip to trash with Severnius Stim Implant"
                                   :choices {:max n
                                             :req #(and (= (:side %) "Runner")
                                                        (in-hand? %))}
                                   :msg (msg "trash " (pluralize "card" (count targets))
                                             " and access " (pluralize "additional card" (quot (count targets) 2)))
                                   :effect (req (let [bonus (quot (count targets) 2)]
                                                  (trash-cards state side (make-eid state) targets
                                                               {:unpreventable true
                                                                :suppress-event true})
                                                  (make-run state side srv nil card)
                                                  (register-events state side
                                                                   {:pre-access
                                                                    {:silent (req true)
                                                                     :effect (effect (access-bonus kw bonus))}
                                                                    :run-ends {:effect (effect (unregister-events card))}}
                                                                   card)))}
                                  card nil)))}]
    :events {:pre-access nil
             :run-ends nil}}

   "Şifr"
   {:in-play [:memory 2]
    :abilities [{:once :per-turn
                 :req (req (rezzed? current-ice))
                 :msg (msg "lower their maximum hand size by 1 and lower the strength of " (:title current-ice) " to 0")
                 :effect (effect (lose :runner :hand-size 1)
                                 (update! (assoc card :sifr-target current-ice :sifr-used true))
                                 (update-ice-strength current-ice))}]
    :events {:runner-turn-begins {:req (req (:sifr-used card))
                                  :effect (effect (gain :runner :hand-size 1)
                                                  (update! (dissoc card :sifr-used)))}
             :pre-ice-strength {:req (req (= (:cid target) (get-in card [:sifr-target :cid])))
                                :effect (req (let [ice-str (:current-strength target)]
                                               (ice-strength-bonus state side (- ice-str) target)))}
             :run-ends {:effect (effect (update! (dissoc card :sifr-target)))}}}

   "Silencer"
   {:recurring 1}

   "Skulljack"
   {:effect (effect (damage eid :brain 1 {:card card}))
    :events {:pre-trash {:effect (effect (trash-cost-bonus -1))}}}

   "Spinal Modem"
   {:in-play [:memory 1]
    :recurring 2
    :events {:successful-trace {:req (req run)
                                :effect (effect (system-msg (str "suffers 1 brain damage from Spinal Modem"))
                                                (damage eid :brain 1 {:card card}))}}}

   "Sports Hopper"
   {:in-play [:link 1]
    :abilities [{:label "Draw 3 cards"
                 :msg "draw 3 cards"
                 :async true
                 :effect (req (wait-for (trash state :runner card {:cause :ability-cost}) (draw state :runner eid 3 nil)))}]}

   "Spy Camera"
   {:abilities [{:cost [:click 1]
                 :async true
                 :label "Look at the top X cards of your Stack"
                 :msg "look at the top X cards of their Stack and rearrange them"
                 :effect (req (show-wait-prompt state :corp "Runner to rearrange the top cards of their stack")
                              (let [n (count (filter #(= (:title %) (:title card))
                                                     (all-active-installed state :runner)))
                                    from (take n (:deck runner))]
                                (if (pos? (count from))
                                  (continue-ability state side (reorder-choice :runner :corp from '()
                                                                               (count from) from) card nil)
                                  (do (clear-wait-prompt state :corp)
                                      (effect-completed state side eid)))))}
                {:label "[Trash]: Look at the top card of R&D"
                 :msg "trash it and look at the top card of R&D"
                 :effect (effect (prompt! card (str "The top card of R&D is " (:title (first (:deck corp)))) ["OK"] {})
                                 (trash card {:cause :ability-cost}))}]}

   "The Gauntlet"
   {:implementation "Requires Runner to manually (and honestly) set how many ICE were broken directly protecting HQ"
    :in-play [:memory 2]
    :events {:post-successful-run {:req (req (and (= :hq target)
                                                  run))
                                   :silent (req true)
                                   :async true
                                   :effect (effect (continue-ability
                                                     {:prompt "How many ICE protecting HQ did you break all subroutines on?"
                                                      ;; Makes number of ice on server (HQ) the upper limit.
                                                      ;; This should work since trashed ice do not count according to UFAQ
                                                      :choices {:number (req (count (get-in @state [:corp :servers :hq :ices])))}
                                                      :effect (effect (access-bonus :hq target))}
                                                     card nil))}}}

   "The Personal Touch"
   {:hosting {:req #(and (has-subtype? % "Icebreaker")
                         (installed? %))}
    :effect (effect (update-breaker-strength (:host card)))
    :events {:pre-breaker-strength {:req (req (= (:cid target) (:cid (:host card))))
                                    :effect (effect (breaker-strength-bonus 1))}}}

   "The Toolbox"
   {:in-play [:link 2 :memory 2]
    :recurring 2}

   "Titanium Ribs"
   {:events
    {:pre-resolve-damage
     {:async true
      :req (req (and (pos? (last targets))
                     (runner-can-choose-damage? state)
                     (not (get-in @state [:damage :damage-replace]))))
      :effect (req (let [dtype target
                         src (second targets)
                         dmg (last targets)]
                     (when (> dmg (count (:hand runner)))
                       (flatline state))
                     (when (= dtype :brain)
                       (swap! state update-in [:runner :brain-damage] #(+ % dmg))
                       (swap! state update-in [:runner :hand-size :mod] #(- % dmg)))
                     (show-wait-prompt state :corp "Runner to use Titanium Ribs to choose cards to be trashed")
                     (wait-for (resolve-ability
                                 state side
                                 {:async true
                                  :prompt (msg "Select " dmg " cards to trash for the " (name dtype) " damage")
                                  :player :runner
                                  :choices {:max dmg
                                            :all true
                                            :req #(and (in-hand? %)
                                                       (= (:side %) "Runner"))}
                                  :msg (msg "trash " (join ", " (map :title targets)))
                                  :effect (req (clear-wait-prompt state :corp)
                                               (doseq [c targets]
                                                 (trash state side c {:cause dtype :unpreventable true}))
                                               (trigger-event state side :damage-chosen)
                                               (damage-defer state side dtype 0)
                                               (effect-completed state side eid))}
                                 card nil)
                               (trigger-event-sync state side eid :damage dtype src dmg))))}
     :damage-chosen {:effect (effect (enable-runner-damage-choice))}}
    :async true
    :effect (effect (enable-runner-damage-choice)
                    (system-msg (str "suffers 2 meat damage from installing Titanium Ribs"))
                    (damage eid :meat 2 {:unboostable true :card card}))
    :leave-play (req (swap! state update-in [:damage] dissoc :damage-choose-runner))}

   "Top Hat"
   (letfn [(ability [n]
             {:async true
              :mandatory true
              :prompt "Which card from the top of R&D would you like to access? (Card 1 is on top.)"
              :choices (take n ["1" "2" "3" "4" "5"])
              :effect (effect (system-msg (str "accesses the card at position " (str->int target) " of R&D"))
                              (access-card eid (nth (:deck corp) (dec (str->int target))) "an unseen card"))})]
     {:events {:successful-run
               {:req (req (= target :rd))
                :interactive (req true)
                :optional {:prompt "Use Top Hat to choose one of the top 5 cards in R&D to access?"
                           :yes-ability {:effect (req (swap! state assoc-in [:run :run-effect :replace-access]
                                                             (ability (count (:deck corp)))))}}}}})

   "Turntable"
   {:in-play [:memory 1]
    :events {:agenda-stolen
             {:interactive (req true)
              :req (req (not (empty? (:scored corp))))
              :async true
              :effect (req
                        (let [stolen target]
                          (continue-ability
                            state side
                            {:optional
                             {:prompt (msg "Swap " (:title stolen) " for an agenda in the Corp's score area?")
                              :yes-ability
                              {:async true
                               :effect (req
                                         (continue-ability
                                           state side
                                           {:prompt (str "Select a scored Corp agenda to swap with " (:title stolen))
                                            :choices {:req #(in-corp-scored? state side %)}
                                            :effect (req (let [scored target]
                                                           (swap-agendas state side scored stolen)
                                                           (system-msg state side (str "uses Turntable to swap "
                                                                                       (:title stolen) " for " (:title scored)))
                                                           (effect-completed state side eid)))}
                                           card targets))}}}
                            card targets)))}}}

   "Ubax"
   (let [ability {:req (req (:runner-phase-12 @state))
                  :msg "draw 1 card"
                  :label "Draw 1 card (start of turn)"
                  :once :per-turn
                  :async true
                  :effect (effect (draw eid 1 nil))}]
     {:in-play [:memory 1]
      :flags {:runner-turn-draw true
              :runner-phase-12 (req (< 1 (count (filter #(card-flag? % :runner-turn-draw true)
                                                        (cons (get-in @state [:runner :identity])
                                                              (all-active-installed state :runner))))))}
      :events {:runner-turn-begins ability}
      :abilities [ability]})

   "Unregistered S&W '35"
   {:abilities
    [{:cost [:click 2]
      :req (req (some #{:hq} (:successful-run runner-reg)))
      :label "trash a Bioroid, Clone, Executive or Sysop"
      :prompt "Select a Bioroid, Clone, Executive, or Sysop to trash"
      :choices {:req #(and (rezzed? %)
                           (or (has-subtype? % "Bioroid")
                               (has-subtype? % "Clone")
                               (has-subtype? % "Executive")
                               (has-subtype? % "Sysop"))
                           (or (and (= (last (:zone %)) :content) (is-remote? (second (:zone %))))
                               (= (last (:zone %)) :onhost)))}
      :msg (msg "trash " (:title target)) :effect (effect (trash target))}]}

   "Vigil"
   (let [ability {:req (req (and (:runner-phase-12 @state) (= (count (:hand corp)) (hand-size state :corp))))
                  :msg "draw 1 card"
                  :label "Draw 1 card (start of turn)"
                  :once :per-turn
                  :async true
                  :effect (effect (draw eid 1 nil))}]
     {:in-play [:memory 1]
      :events {:runner-turn-begins ability}
      :abilities [ability]})

   "Window"
   {:abilities [{:cost [:click 1] :msg "draw 1 card from the bottom of their Stack"
                 :effect (effect (move (last (:deck runner)) :hand))}]}

   "Zamba"
   {:implementation "Credit gain is automatic"
    :in-play [:memory 2]
    :events {:expose {:effect (effect (gain-credits :runner 1))
                      :msg "gain 1 [Credits]"}}}

   "Zer0"
   {:abilities [{:cost [:click 1 :net-damage 1]
                 :once :per-turn
                 :msg "gain 1 [Credits] and draw 2 cards"
                 :async true
                 :effect (effect (gain-credits 1)
                                 (draw eid 2 nil))}]}})
