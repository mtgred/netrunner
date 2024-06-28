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
     [:section [:h3 "Corporation Turn"]]
     [:section
      [:h4 "5.6.1: Draw Phase"]
      (bullet 0 (render-icons "Corporation gains allotted clicks (default: [click][click][click])"))
      (bullet 1 "Paid ability window. Corp may rez non-ice cards or score agendas during this window")
      (bullet 2 "Corporation recurring credits refill")
      (bullet 3 "The turn formally begins. Turn begins events resolve")
      (bullet 4 "The corporation performs their mandatory draw")
      (bullet 5 "Proceed to the action phase (5.6.2)")]
     [:section
      [:h4 "5.6.2: Action Phase"]
      (bullet 0 "Paid ability window. Corp may rez non-ice cards or score agendas during this window")
      (bullet 1 (render-icons "If the corporation has unspent [Clicks], they take an action"))
      (bullet 2 "If an action occured, return to (a)")
      (bullet 3 "The action phase is complete. Proceed to the discard phase (5.6.3)")]
     [:section
      [:h4 "5.6.3: Discard phase"]
      (bullet 0 "The corporation discards to maximum hand size, if applicable")
      (bullet 1 "Paid ability window. Corp may rez non-ice cards during this window")
      (bullet 2 (render-icons "If the corporation has any [Clicks] remaining, they lose those [Clicks]"))
      (bullet 3 "The Corporations turn formally ends. Turn end triggers resolve")
      (bullet 4 "Proceed to the Runner turn")]

     [:section [:h3 "Runner Turn"]]
     [:section
      [:h4 "5.7.1: Action Phase"]
      (bullet 0 (render-icons "Runner gains allotted clicks (default: [click][click][click][click])"))
      (bullet 1 "Paid ability window. Corp may rez non-ice cards")
      (bullet 2 "Runner recurring credits refill")
      (bullet 3 "The turn formally begins. Turn begins events resolve")
      (bullet 4 "Paid ability window. Corp may rez non-ice cards")
      (bullet 5 (render-icons "If the Runner has unspent [Clicks], they take an action"))
      (bullet 6 "If an action occured, return to (e)")
      (bullet 7 "The action phase is complete. Proceed to the discard phase (5.7.2)")]
     [:section
      [:h4 "5.7.2: Discard Phase"]
      (bullet 0 "The runner discards to maximum handsize, if applicable")
      (bullet 1 "Paid ability window. Corp may rez non-ice cards")
      (bullet 2 (render-icons "If the runner has any [Clicks] remaining, they lose those [Clicks]"))
      (bullet 3 "The Runners turn formally ends. Turn end triggers resolve")
      (bullet 4 "Proceed to the Corporation turn")]]))

(defn run-timing-pane []
  (fn []
    [:div.diagram
     [:section
      [:h3 "Timing Structure of a Run"]
      [:div "This structure has been simplified for clarity. For complete rules, see the Null Signal Games website."]]
     [:section
      [:h4 "6.9.1: Initiation Phase"]
      (bullet 0 "Runner declares a server")
      (bullet 1 "Runner gains Bad Publicity credits")
      (bullet 2 "Run formally begins - Run events fire")
      (bullet 3 "Proceed to the outermost ice, if applicable, and begin the approach phase (6.9.2)")
      (bullet 4 "Otherwise, proceed to the movement phase (6.9.4)")]
     [:section
      [:h4 "6.9.2: Approach Ice Phase"]
      (bullet 0 "You are now approaching the ice. Approach events resolve")
      (bullet 1 "Paid Ability Window. Corp may rez the approached ice, or non-ice cards, during this window")
      (bullet 2 "If approached ice is rezzed, continue to encounter phase (6.9.3)")
      (bullet 3 "Otherwise, proceed to the movement phase (6.9.4)")]
     [:section
      [:h4 "6.9.3: Encounter Ice Phase"]
      (bullet 0 "You are now encountering this ice. Encounter events resolve")
      (bullet 1 "Paid ability window. Encountered ice may be interfaced during this window")
      (bullet 2 "If there are unbroken subroutines to resolve, the corporation resolves the topmost unbroken subroutine. If they do, repeat this step")
      (bullet 3 "The encounter is complete. Proceed to the movement phase (6.9.4)")]
     [:section
      [:h4 "6.9.4: Movement Phase"]
      (bullet 0 "If you were encountering or approaching an ice, you pass it. Pass-Ice events resolve")
      (bullet 1 "If there are no more ice inwards from the passed ice, 'when you pass all ice on the server' events resolve")
      (bullet 2 "Paid ability window")
      (bullet 3 "The runner may jack out. If they do, proceed to the run ends phase (6.9.6)")
      (bullet 4 "The runner proceeds to the next position inwards, if applicable")
      (bullet 5 "Paid ability window. The corporation may rez non-ice cards")
      (bullet 6 "If you are approaching another ice, return to the approach ice phase (6.9.2)")
      (bullet 7 "The runner approaches the attacked server. Approach events resolve")
      (bullet 8 "Continue to the success phase (6.9.5)")]
     [:section
      [:h4 "6.9.5: Success Phase"]
      (bullet 0 "The run is declared successful. Successful run events are met")
      (bullet 1 "The runner breaches the attacked server")
      (bullet 2 "The success phase is complete. Continue to the run ends phase (6.9.6)")]
     [:section
      [:h4 "6.9.6: Run Ends Phase"]
      (bullet 0 "Any open priority windows complete or are closed")
      (bullet 1 "The runner loses any unspent bad publicity credits")
      (bullet 2 "If the success phase was not reached and the server still exists, the run becomes unsuccessful")
      (bullet 3 "The run ends. Run ends events resolve")]]))
