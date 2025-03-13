(ns nr.gameboard.settings
  (:require
   [nr.account :refer [post-options]]
   [nr.appstate :refer [app-state]]
   [nr.translations :refer [tr]]))

(defn settings-pane []
  (fn []
    [:div.settings
     [:section
      [:h4 [tr [:ingame-settings_card-stacking "Card settings"]]]
      [:div
       [:label [:input {:type "checkbox"
                        :value true
                        :checked (get-in @app-state [:options :stacked-cards])
                        :on-change #(swap! app-state assoc-in [:options :stacked-cards] (.. % -target -checked))}]
        [tr [:ingame-settings_stack-cards "Stack cards"]]]]
      [:div
       [:label [:input {:type "checkbox"
                        :value true
                        :checked (get-in @app-state [:options :labeled-unrezzed-cards])
                        :on-change #(swap! app-state assoc-in [:options :labeled-unrezzed-cards] (.. % -target -checked))}]
        [tr [:ingame-settings_label-unrezzed-cards "Label unrezzed cards"]]]]
      [:div
       [:label [:input {:type "checkbox"
                        :value true
                        :checked (get-in @app-state [:options :labeled-cards])
                        :on-change #(swap! app-state assoc-in [:options :labeled-cards] (.. % -target -checked))}]
        [tr [:ingame-settings_label-faceup-cards "Label face up cards"]]]]
      [:div
       [:label [:input {:type "checkbox"
                        :value true
                        :checked (get-in @app-state [:options :ghost-trojans])
                        :on-change #(swap! app-state assoc-in [:options :ghost-trojans] (.. % -target -checked))}]
        [tr [:ingame-settings_ghost-trojans "Display hosted trojans in rig"]]]]
      [:div
       [:label [:input {:type "checkbox"
                        :value true
                        :checked (get-in @app-state [:options :display-encounter-info])
                        :on-change #(swap! app-state assoc-in [:options :display-encounter-info] (.. % -target -checked))}]
        [tr [:ingame-settings_display-encounter-info "Always display encounter info"]]]]]

     [:section
      [:h4 [tr [:ingame-settings_game-settings "Gameplay Settings"]]]
      [:div
       [:label [:input {:type "checkbox"
                        :value true
                        :checked (get-in @app-state [:options :pass-on-rez])
                        :on-change #(swap! app-state assoc-in [:options :pass-on-rez] (.. % -target -checked))}]
        [tr [:ingame-settings_pass-on-rez "Pass priority when rezzing ice"]]]]]

     [:section
      [:h4 [tr [:ingame-settings_card-sorting "Sorting"]]]
      [:div
       [:label [:input {:type "checkbox"
                        :value true
                        :checked (get-in @app-state [:options :archives-sorted])
                        :on-change #(swap! app-state assoc-in [:options :archives-sorted] (.. % -target -checked))}]
        [tr [:ingame-settings_sort-archives "Sort Archives"]]]]
      [:div
       [:label [:input {:type "checkbox"
                        :value true
                        :checked (get-in @app-state [:options :heap-sorted])
                        :on-change #(swap! app-state assoc-in [:options :heap-sorted] (.. % -target -checked))}]
        [tr [:ingame-settings_sort-heap "Sort Heap"]]]]]

     [:section
      [:h4 [tr [:ingame-settings_runner-board-order "Runner board order"]]]
      (doall (for [option [{:name [tr [:ingame-settings_runner-classic "classic"]] :ref "jnet"}
                           {:name [tr [:ingame-settings_runner-reverse "reversed"]] :ref "irl"}]]
               [:div.radio {:key (:name option)}
                [:label [:input {:type "radio"
                                 :name "runner-board-order"
                                 :value (:ref option)
                                 :on-change #(swap! app-state assoc-in [:options :runner-board-order] (.. % -target -value))
                                 :checked (= (get-in @app-state [:options :runner-board-order]) (:ref option))}]
                 (:name option)]]))]
     [:section
      [:h4 [tr [:ingame-settings_log-timestamps "Log timestamps"]]]
      [:div
       [:label [:input {:type "checkbox"
                        :value true
                        :checked (get-in @app-state [:options :log-timestamps])
                        :on-change #(swap! app-state assoc-in [:options :log-timestamps] (.. % -target -checked))}]
        [tr [:ingame-settings_toggle-log-timestamps "Show log timestamps"]]]]]

     [:section
      [:h4 [tr [:ingame-settings_board-overlap "Board overlap"]]]
      [:div
       [:label [:input {:type "checkbox"
                        :value true
                        :checked (get-in @app-state [:options :sides-overlap])
                        :on-change #(swap! app-state assoc-in [:options :sides-overlap] (.. % -target -checked))}]
        [tr [:ingame-settings_sides-overlap "Runner and Corp may overlap"]]]]]

     [:section
      [:h4 [tr [:ingame-settings_card-backs "Card backs"]]]
      (doall (for [option [{:name [tr [:settings_nsg "NSG"]] :ref "nsg"}
                           {:name [tr [:settings_ffg "FFG"]] :ref "ffg"}]]
               [:div.radio {:key (:name option)}
                [:label [:input {:type "radio"
                                 :name "card-back"
                                 :value (:ref option)
                                 :on-change #(swap! app-state assoc-in [:options :card-back] (.. % -target -value))
                                 :checked (= (get-in @app-state [:options :card-back]) (:ref option))}]
                 (:name option)]]))]

     [:section
      [:h4 [tr [:ingame-settings_preview-zoom "Card preview zoom"]]]
      (doall (for [option [{:name [tr [:ingame-settings_card-image "Card Image"]] :ref "image"}
                          {:name [tr [:ingame-settings_card-text "Card Text"]] :ref "text"}]]
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
       [tr [:settings_pin-zoom "Keep zoomed cards on screen"]]]]

     [:section
      [:h4 [tr [:ingame-settings_card-images "Card images"]]]
      [:div
       [:label [:input {:type "checkbox"
                        :name "use-high-res"
                        :checked (= "high" (get-in @app-state [:options :card-resolution]))
                        :on-change #(swap! app-state assoc-in [:options :card-resolution] (if (.. % -target -checked) "high" "default"))}]
        [tr [:ingame-settings_high-res "Enable high resolution card images"]]]]]

     [:section
      [:h4 [tr [:ingame-settings_alt-art "Alt arts"]]]
      [:div
       [:label [:input {:type "checkbox"
                        :name "show-alt-art"
                        :checked (get-in @app-state [:options :show-alt-art])
                        :on-change #(swap! app-state assoc-in [:options :show-alt-art] (.. % -target -checked))}]
        [tr [:ingame-settings_show-alt "Show alternate card arts"]]]]]
     [:button {:on-click #(post-options (constantly nil))} [tr [:ingame-settings_save "Save"]]]]))
