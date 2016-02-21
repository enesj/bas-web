(ns jus.views
  (:require [re-com.core     :as re-com              :refer [h-box v-box box gap line row-button label checkbox horizontal-bar-tabs vertical-bar-tabs title p]
             :refer-macros [handler-fn]]
            [re-com.buttons                :refer [row-button-args-desc]]
            [re-com.util                   :refer [enumerate]]
            [jus.md-circle-icon-button :refer [icons example-icons]]
            [jus.utils                 :refer [panel-title title2 args-table material-design-hyperlink github-hyperlink status-text]]
            [jus.data-table :refer [data-table]]
            [reagent.core :as reagent]))


(def col-widths
  {:name {:type :label :width "70%" :field nil} :from {:type :label :width "10%" :field nil} :to {:type :label :width "10%" :field nil} }  )


(def rows       {"1" {:id "1" :sort 0 :name "Time range 1" :from "18:00" :to "22:30"}
                    "2" {:id "2" :sort 1 :name "Time range 2" :from "18:00" :to "22:30"}
;;                     "3" {:id "3" :sort 1 :name "Time range 2 with some extra text appended to the end." :from "18:00" :to "22:30"}
                    "4" {:id "4" :sort 2 :name "Time range 3" :from "06:00" :to "18:00"}})


(defn row-button-demo
    []
  (let [selected-icon (reagent/atom (:id (first icons)))]
    (fn []
      [v-box
       :size     "auto"
       :gap      "10px"
       :children [[panel-title  "Table with row buttons "]
                  [h-box
                   :gap      "100px"
                   :children [[v-box
                               :gap      "10px"
                               :children [[title2 "Demo"]
                                          [v-box
                                           :gap "20px"
                                           :children [
                                                       [data-table rows col-widths nil nil]
                                                      [gap :size "40px"]
                                                      [line]
                                                      [title :level :level3 :label "Row Button States"]
                                                      [:p "Row buttons have three distinct states."]
                                                      [example-icons selected-icon]
                                                      [v-box
                                                       :gap      "8px"
                                                       :children [[h-box
                                                                   :gap      "2px"
                                                                   :align    :center
                                                                   :children [[label :label "States: ["]
                                                                              [row-button
                                                                               :md-icon-name    @selected-icon
                                                                               :mouse-over-row? false
                                                                               :tooltip         ":mouse-over-row? set to false (invisible)"
                                                                               :on-click        #()]
                                                                              [row-button
                                                                               :md-icon-name    @selected-icon
                                                                               :mouse-over-row? true
                                                                               :tooltip         ":mouse-over-row? set to true (semi-visible)"
                                                                               :on-click        #()]
                                                                              [row-button
                                                                               :md-icon-name    @selected-icon
                                                                               :tooltip         ":disabled? set to true"
                                                                               :disabled?       true
                                                                               :on-click        #()]
                                                                              [label :label "]"]]]]]]]]]]]]])))


;; core holds a reference to panel, so need one level of indirection to get figwheel updates
(defn panel
  []
  [row-button-demo])

(defn main-panel []
    [re-com/v-box
     :margin "30"
     :height "100%"
     :children [[title :label "TBD"]]])
