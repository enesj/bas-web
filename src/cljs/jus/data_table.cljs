(ns jus.data-table
	(:use [com.rpl.specter :only [transform setval FIRST LAST ALL keypath filterer srange comp-paths compiled-select collect-one compiled-setval]])
	(:require [re-com.core :refer [h-box v-box box gap line row-button label checkbox horizontal-bar-tabs vertical-bar-tabs title p hyperlink-href]
						 :refer-macros [handler-fn]]
						[re-com.buttons :refer [row-button-args-desc]]
						[re-com.util :refer [enumerate]]
						[re-com.popover :refer [popover-tooltip]]
						[jus.md-circle-icon-button :refer [icons example-icons]]
						[jus.utils :refer [panel-title title2 args-table material-design-hyperlink github-hyperlink status-text]]
						[ajax.core :refer [GET]]
						[reagent.core :as reagent]))



(defn columns [col-widths]
	(into [] (transform [ALL FIRST] #(if-not (= "JUS" (name %)) (clojure.string/capitalize (name %)) (name %)) col-widths)))


(defn tooltip-label [label1 label tip show-tooltip? ]
	(let [label-kw (keyword label1)
				showing? (reagent/atom (label-kw @show-tooltip?))
				ic-label [:div
									{:style         {:color "#555657"}
									 :on-mouse-over #(swap! show-tooltip? assoc-in [label-kw] 1 )
									 :on-mouse-out  #(swap! show-tooltip? assoc-in [label-kw] nil)}
									label]]
		[box :class "display-inline-flex" :align :center :child [popover-tooltip
																														 :label tip
																														 :position :below-center
																														 :showing?  showing?
																														 :anchor ic-label]]))

(defn data-row
	[row col-widths  level show-tooltip?]
  	(let [mouse-over-row? true]
			;(println "render")
		[h-box
		 :class "rc-div-table-row"
		 :style (if (:selected row) {:border-style "outset" :border-color "#6F99AD" :border-width "3px"})
		 ;:attr {
			;			:on-mouse-over (handler-fn (reset! mouse-over row))
			;			:on-mouse-out  (handler-fn (reset! mouse-over nil))
			;			;:on-click   (handler-fn  (apply (:click row) (:path-data row)  ))
			;			}
		 :children [(doall (for [param col-widths]
												 (let [label1 (key param)
															 params (val param)]
													 ;(println  (:disabled? (first (:children params))) )
													 (with-meta
														 (condp = (:type params)
															 :label [label :label (label1 row) :width (:width params) :on-click
																			 (if (= (:action params) :click) (handler-fn ((:function params) (:id row) (:level row))))
																			 :attr {:on-double-click   #((:double-click params) row) }
																			 ]
                               :href [hyperlink-href :label (label1 row) :href (if (:link-d row) (:link-d row) "http://google.com") :target "_blank" :style {:width (:width params)} ]
															 :label-tooltip [label :label (tooltip-label label1 (label1 row) (str (:tooltip params) (label1 (:tooltip  row) ) ) show-tooltip?)	:width (:width params)]
															 :check-box [h-box :width (:width params) :children [[checkbox :model (label1 row) :disabled? (:disabled? params) :on-change #((:action params) row)]]]
															 :row-button [h-box :gap (:gap params) :width (:width params) :justify (:justify params)
																						:children
																						[(doall (for [r-b (:children params)]
																											^{:key (:id r-b)}
																											[row-button :md-icon-name (:md-icon-name r-b) :mouse-over-row? mouse-over-row? :tooltip (:tooltip r-b)
																																				 :tooltip-position (:tooltip-position r-b) :disabled?  ((:disabled? r-b) row) :on-click #((:action r-b) level (:id row) (:link-n row) row)]))]]
															 nil)
														 {:key (str label1 (:id row))}))))]]))


(defn data-table
	[rows col-widths-types level]
		(fn [rows col-widths-types level]
			[v-box
			 :align :start
			 :gap "10px"
			 :width "100%"
			 :children [[v-box
									 :class "rc-div-table"
									 :width "99%"
									 ;:style {:font-size (when @large-font "24px")}
									 :children [
															[h-box
															 :class "rc-div-table-header"
															 :children
															 [;[label :label "Sort" :width "6%"]
																(for [[label1 params] (columns col-widths-types)]
																	^{:key label1} [label :label (cond (:icon params) (:icon params)
																																		 (:label params) (:label params)
																																		 :else label1) :width (:width params)])]]
															;(if-not level
															(for [row (sort-by :naslov (vals rows))]
																^{:key (:id row)} [data-row row col-widths-types level  (reagent/atom nil)])
															;)
															]]]]))
