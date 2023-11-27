(ns nr.gameboard.settings
  (:require
   [nr.account :refer [post-options]]
   [nr.appstate :refer [app-state]]
   [nr.translations :refer [tr]]))

(defn settings-pane []
  (fn []
    [:div.settings
     [:section
      [:h4 (tr [:ingame-settings.card-stacking "Card settings"])]
      [:div
       [:label [:input {:type "checkbox"
                        :value true
                        :checked (get-in @app-state [:options :stacked-cards])
                        :on-change #(swap! app-state assoc-in [:options :stacked-cards] (.. % -target -checked))}]
        (tr [:ingame-settings.stack-cards "Stack cards"])]]
      [:div
       [:label [:input {:type "checkbox"
                        :value true
                        :checked (get-in @app-state [:options :labeled-unrezzed-cards])
                        :on-change #(swap! app-state assoc-in [:options :labeled-unrezzed-cards] (.. % -target -checked))}]
        (tr [:ingame-settings.stack-cards "Label unrezzed cards"])]]
      [:div
       [:label [:input {:type "checkbox"
                        :value true
                        :checked (get-in @app-state [:options :labeled-cards])
                        :on-change #(swap! app-state assoc-in [:options :labeled-cards] (.. % -target -checked))}]
        (tr [:ingame-settings.stack-cards "Label face up cards"])]]]

     [:section
      [:h4 (tr [:ingame-settings.card-stacking "Sorting"])]
      [:div
       [:label [:input {:type "checkbox"
                        :value true
                        :checked (get-in @app-state [:options :archives-sorted])
                        :on-change #(swap! app-state assoc-in [:options :archives-sorted] (.. % -target -checked))}]
        (tr [:ingame-settings.stack-cards "Sort Archives"])]]
      [:div
       [:label [:input {:type "checkbox"
                        :value true
                        :checked (get-in @app-state [:options :heap-sorted])
                        :on-change #(swap! app-state assoc-in [:options :heap-sorted] (.. % -target -checked))}]
        (tr [:ingame-settings.stack-cards "Sort Heap"])]]]

     [:section
      [:h4 (tr [:ingame-settings.runner-board-order "Runner board order"])]
      (doall (for [option [{:name (tr [:ingame-settings.runner-classic "classic"]) :ref "jnet"}
                           {:name (tr [:ingame-settings.runner-reverse "reversed"]) :ref "irl"}]]
               [:div.radio {:key (:name option)}
                [:label [:input {:type "radio"
                                 :name "runner-board-order"
                                 :value (:ref option)
                                 :on-change #(swap! app-state assoc-in [:options :runner-board-order] (.. % -target -value))
                                 :checked (= (get-in @app-state [:options :runner-board-order]) (:ref option))}]
                 (:name option)]]))]

     [:section
      [:h4 (tr [:ingame-settings.runner-board-order "Board overlap"])]
      [:div
       [:label [:input {:type "checkbox"
                        :value true
                        :checked (get-in @app-state [:options :sides-overlap])
                        :on-change #(swap! app-state assoc-in [:options :sides-overlap] (.. % -target -checked))}]
        (tr [:ingame-settings.sides-overlap "Runner and Corp may overlap"])]]]

     [:section
      [:h4 (tr [:ingame-settings.card-backs "Card backs"])]
      (doall (for [option [{:name (tr [:settings.nsg "NSG"]) :ref "nsg"}
                           {:name (tr [:settings.ffg "FFG"]) :ref "ffg"}]]
               [:div.radio {:key (:name option)}
                [:label [:input {:type "radio"
                                 :name "card-back"
                                 :value (:ref option)
                                 :on-change #(swap! app-state assoc-in [:options :card-back] (.. % -target -value))
                                 :checked (= (get-in @app-state [:options :card-back]) (:ref option))}]
                 (:name option)]]))]

     [:section
      [:h4 (tr [:ingame-settings.preview-zoom "Card preview zoom"])]
      (doall (for [option [{:name "Card Image" :ref "image"}
                           {:name "Card Text" :ref "text"}]]
               [:div.radio {:key (:name option)}
                [:label [:input {:type "radio"
                                 :name "card-zoom"
                                 :value (:ref option)
                                 :on-change #(swap! app-state assoc-in [:options :card-zoom] (.. % -target -value))
                                 :checked (= (get-in @app-state [:options :card-zoom]) (:ref option))}]
                 (:name option)]]))
      [:label [:input {:type "checkbox"
                       :name "pin-zoom"
                       :checked (get-in @app-state [:options :pin-zoom])
                       :on-change #(swap! app-state assoc-in [:options :pin-zoom] (.. % -target -checked))}]
       (tr [:settings.pin-zoom "Keep zoomed cards on screen"])]]

     [:section
      [:h4 (tr [:ingame-settings.card-images "Card images"])]
      [:div
       [:label [:input {:type "checkbox"
                        :name "use-high-res"
                        :checked (= "high" (get-in @app-state [:options :card-resolution]))
                        :on-change #(swap! app-state assoc-in [:options :card-resolution] (if (.. % -target -checked) "high" "default"))}]
        (tr [:ingame-settings.high-res "Enable high resolution card images"])]]]

     [:section
      [:h4 (tr [:ingame-settings.alt-art "Alt arts"])]
      [:div
       [:label [:input {:type "checkbox"
                        :name "show-alt-art"
                        :checked (get-in @app-state [:options :show-alt-art])
                        :on-change #(swap! app-state assoc-in [:options :show-alt-art] (.. % -target -checked))}]
        (tr [:ingame-settings.show-alt "Show alternate card arts"])]]]
     [:button {:on-click #(post-options "/profile" (constantly nil))} (tr [:ingame-settings.save "Save"])]]))
