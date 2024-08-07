(ns nr.gameboard.diagrams
  (:require
   [nr.appstate :refer [app-state]]
   [nr.translations :refer [tr]]
   [nr.utils :refer [render-icons]]))

(defn- bullet
  [tag text]
  (let [tag (nth "abcdefghi" tag)]
    [:div
     [:td [:label (str tag ") ")]]
     [:td [:div text]]]))

(defn turn-timing-pane []
  (fn []
    [:div.diagram
     [:section [:h3 (tr [:diagrams.turn.corp-turn "Corporation Turn"])]]
     [:section
      [:h4 (tr [:diagrams.turn.corp-draw-phase "5.6.1: Draw Phase"])]
      (bullet 0 (render-icons (tr [:diagrams.turn.corp-draw-phase-a "Corporation gains allotted clicks (default: [click][click][click])"])))
      (bullet 1 (tr [:diagrams.turn.corp-draw-phase-b "Paid ability window. Corp may rez non-ice cards or score agendas during this window"]))
      (bullet 2 (tr [:diagrams.turn.corp-draw-phase-c "Corporation recurring credits refill"]))
      (bullet 3 (tr [:diagrams.turn.corp-draw-phase-d "The turn formally begins. Turn begins events resolve"]))
      (bullet 4 (tr [:diagrams.turn.corp-draw-phase-e "The corporation performs their mandatory draw"]))
      (bullet 5 (tr [:diagrams.turn.corp-draw-phase-f "Proceed to the action phase (5.6.2)"]))]
     [:section
      [:h4 (tr [:diagrams.turn.corp-action-phase "5.6.2: Action Phase"])]
      (bullet 0 (tr [:diagrams.turn.corp-action-phase-a "Paid ability window. Corp may rez non-ice cards or score agendas during this window"]))
      (bullet 1 (render-icons (tr [:diagrams.turn.corp-action-phase-b "If the corporation has unspent [Clicks], they take an action"])))
      (bullet 2 (tr [:diagrams.turn.corp-action-phase-c "If an action occured, return to (a)"]))
      (bullet 3 (tr [:diagrams.turn.corp-action-phase-d "The action phase is complete. Proceed to the discard phase (5.6.3)"]))]
     [:section
      [:h4 (tr [:diagrams.turn.corp-discard-phase "5.6.3: Discard phase"])]
      (bullet 0 (tr [:diagrams.turn.corp-discard-phase-a "The corporation discards to maximum hand size, if applicable"]))
      (bullet 1 (tr [:diagrams.turn.corp-discard-phase-b "Paid ability window. Corp may rez non-ice cards during this window"]))
      (bullet 2 (render-icons (tr [:diagrams.turn.corp-discard-phase-c "If the corporation has any [Clicks] remaining, they lose those [Clicks]"])))
      (bullet 3 (tr [:diagrams.turn.corp-discard-phase-d "The Corporations turn formally ends. Turn end triggers resolve"]))
      (bullet 4 (tr [:diagrams.turn.corp-discard-phase-e "Proceed to the Runner turn"]))]

     [:section [:h3 (tr [:diagrams.turn.runner-turn "Runner Turn"])]]
     [:section
      [:h4 (tr [:diagrams.turn.runner-action-phase "5.7.1: Action Phase"])]
      (bullet 0 (render-icons (tr [:diagrams.turn.runner-action-phase-a "Runner gains allotted clicks (default: [click][click][click][click])"])))
      (bullet 1 (tr [:diagrams.turn.runner-action-phase-b "Paid ability window. Corp may rez non-ice cards"]))
      (bullet 2 (tr [:diagrams.turn.runner-action-phase-c "Runner recurring credits refill"]))
      (bullet 3 (tr [:diagrams.turn.runner-action-phase-d "The turn formally begins. Turn begins events resolve"]))
      (bullet 4 (tr [:diagrams.turn.runner-action-phase-e "Paid ability window. Corp may rez non-ice cards"]))
      (bullet 5 (render-icons (tr [:diagrams.turn.runner-action-phase-f "If the Runner has unspent [Clicks], they take an action"])))
      (bullet 6 (tr [:diagrams.turn.runner-action-phase-g "If an action occured, return to (e)"]))
      (bullet 7 (tr [:diagrams.turn.runner-action-phase-h "The action phase is complete. Proceed to the discard phase (5.7.2)"]))]
     [:section
      [:h4 (tr [:diagrams.turn.runner-discard-phase "5.7.2: Discard Phase"])]
      (bullet 0 (tr [:diagrams.turn.runner-discard-phase-a "The runner discards to maximum handsize, if applicable"]))
      (bullet 1 (tr [:diagrams.turn.runner-discard-phase-b "Paid ability window. Corp may rez non-ice cards"]))
      (bullet 2 (render-icons (tr [:diagrams.turn.runner-discard-phase-c "If the runner has any [Clicks] remaining, they lose those [Clicks]"])))
      (bullet 3 (tr [:diagrams.turn.runner-discard-phase-d "The Runners turn formally ends. Turn end triggers resolve"]))
      (bullet 4 (tr [:diagrams.turn.runner-discard-phase-e "Proceed to the Corporation turn"]))]]))

(defn run-timing-pane []
  (fn []
    [:div.diagram
     [:section
      [:h3 (tr [:diagrams.run-timing.header "Timing Structure of a Run"])]
      [:div (tr [:diagrams.run-timing.disclaimer "This structure has been simplified for clarity. For complete rules, see the Null Signal Games website."])]]
     [:section
      [:h4 (tr [:diagrams.run-timing.initiation "6.9.1: Initiation Phase"])]
      (bullet 0 (tr [:diagrams.run-timing.initiation-a "Runner declares a server"]))
      (bullet 1 (tr [:diagrams.run-timing.initiation-b "Runner gains Bad Publicity credits"]))
      (bullet 2 (tr [:diagrams.run-timing.initiation-c "Run formally begins - Run events fire"]))
      (bullet 3 (tr [:diagrams.run-timing.initiation-d "Proceed to the outermost ice, if applicable, and begin the approach phase (6.9.2)"]))
      (bullet 4 (tr [:diagrams.run-timing.initiation-e "Otherwise, proceed to the movement phase (6.9.4)"]))]
     [:section
      [:h4 (tr [:diagrams.run-timing.approach "6.9.2: Approach Ice Phase"])]
      (bullet 0 (tr [:diagrams.run-timing.approach-a "You are now approaching the ice. Approach events resolve"]))
      (bullet 1 (tr [:diagrams.run-timing.approach-b "Paid Ability Window. Corp may rez the approached ice, or non-ice cards, during this window"]))
      (bullet 2 (tr [:diagrams.run-timing.approach-c "If approached ice is rezzed, continue to encounter phase (6.9.3)"]))
      (bullet 3 (tr [:diagrams.run-timing.approach-d "Otherwise, proceed to the movement phase (6.9.4)"]))]
     [:section
      [:h4 (tr [:diagrams.run-timing.encounter "6.9.3: Encounter Ice Phase"])]
      (bullet 0 (tr [:diagrams.run-timing.encounter-a "You are now encountering this ice. Encounter events resolve"]))
      (bullet 1 (tr [:diagrams.run-timing.encounter-b "Paid ability window. Encountered ice may be interfaced during this window"]))
      (bullet 2 (tr [:diagrams.run-timing.encounter-c "If there are unbroken subroutines to resolve, the corporation resolves the topmost unbroken subroutine. If they do, repeat this step"]))
      (bullet 3 (tr [:diagrams.run-timing.encounter-d "The encounter is complete. Proceed to the movement phase (6.9.4)"]))]
     [:section
      [:h4 (tr [:diagrams.run-timing.movement "6.9.4: Movement Phase"])]
      (bullet 0 (tr [:diagrams.run-timing.movement-a "If you were encountering or approaching an ice, you pass it. Pass-Ice events resolve"]))
      (bullet 1 (tr [:diagrams.run-timing.movement-b "If there are no more ice inwards from the passed ice, 'when you pass all ice on the server' events resolve"]))
      (bullet 2 (tr [:diagrams.run-timing.movement-c "Paid ability window"]))
      (bullet 3 (tr [:diagrams.run-timing.movement-d "The runner may jack out. If they do, proceed to the run ends phase (6.9.6)"]))
      (bullet 4 (tr [:diagrams.run-timing.movement-e "The runner proceeds to the next position inwards, if applicable"]))
      (bullet 5 (tr [:diagrams.run-timing.movement-f "Paid ability window. The corporation may rez non-ice cards"]))
      (bullet 6 (tr [:diagrams.run-timing.movement-g "If you are approaching another ice, return to the approach ice phase (6.9.2)"]))
      (bullet 7 (tr [:diagrams.run-timing.movement-h "The runner approaches the attacked server. Approach events resolve"]))
      (bullet 8 (tr [:diagrams.run-timing.movement-i "Continue to the success phase (6.9.5)"]))]
     [:section
      [:h4 (tr [:diagrams.run-timing.success "6.9.5: Success Phase"])]
      (bullet 0 (tr [:diagrams.run-timing.success-a "The run is declared successful. Successful run events are met"]))
      (bullet 1 (tr [:diagrams.run-timing.success-b "The runner breaches the attacked server"]))
      (bullet 2 (tr [:diagrams.run-timing.success-c "The success phase is complete. Continue to the run ends phase (6.9.6)"]))]
     [:section
      [:h4 (tr [:diagrams.run-timing.run-ends "6.9.6: Run Ends Phase"])]
      (bullet 0 (tr [:diagrams.run-timing.run-ends-a "Any open priority windows complete or are closed"]))
      (bullet 1 (tr [:diagrams.run-timing.run-ends-b "The runner loses any unspent bad publicity credits"]))
      (bullet 2 (tr [:diagrams.run-timing.run-ends-c "If the success phase was not reached and the server still exists, the run becomes unsuccessful"]))
      (bullet 3 (tr [:diagrams.run-timing.run-ends-d "The run ends. Run ends events resolve"]))]]))
