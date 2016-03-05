(ns jus.data-table
	(:use [com.rpl.specter :only [transform setval FIRST LAST ALL keypath filterer srange comp-paths compiled-select collect-one compiled-setval]])
	(:require [re-com.core :refer [h-box v-box box gap line row-button label checkbox horizontal-bar-tabs vertical-bar-tabs title p hyperlink-href slider button]
						 :refer-macros [handler-fn]]
						[re-com.buttons :refer [row-button-args-desc]]
						[re-com.util :refer [enumerate]]
						[re-com.popover :refer [popover-tooltip]]
						[reforms.reagent :include-macros true :as f]
						[ajax.core :refer [GET POST json-response-format json-request-format url-request-format ajax-request]]
						[jus.md-circle-icon-button :refer [icons example-icons]]
						[jus.utils :refer [panel-title title2 args-table material-design-hyperlink github-hyperlink status-text]]
						[jus.helper :as h]
						[ajax.core :refer [GET]]
						[reagent.core :as r]))


(def table-state (r/atom {:table-size       60 :table-height nil :veza nil :path nil
												:delete-modal     {:show? false :level nil :child nil} :nova-naredba-modal {:show? false :edit? false :naslov nil :direktiva nil :link nil :file nil}
												:delete-jus-modal {:show? false :jusid nil}}))


(def jus-data (r/atom {:data nil}))

(def nova-naredba-atom (r/atom {:jusid nil :naslov nil :direktiva nil :link nil :file nil :glasnik nil :naredba nil :X-CSRF-Token nil}))

(def colors
	{:0 "#33cbeb"
	 :1 "#B5c113"
	 :2 "#c7eb33"
	 :3 "#f39f05"
	 :4 "#B5c113"
	 :5 "#B5c113"
	 :6 "#B5D113"
	 :7 "#B5D113"})


(defn columns [col-widths]
	(into [] (transform [ALL FIRST] #(if-not (= "JUS" (name %)) (clojure.string/capitalize (name %)) (name %)) col-widths)))

(defn delete-dialog-template [label process-ok process-cancel]
	[h-box
	 :gap "12px"
	 :children [[title :level :level3 :style {:font-weight "bold" :align-self "center"} :label label]
							[button
							 :label "Briši"
							 :class "btn-primary"
							 :on-click process-ok]
							[button
							 :label "Cancel"
							 :on-click process-cancel]]])

(defn cljs-ajax-upload-file [element-id anti-forgery-token]
	(let [el (.getElementById js/document element-id)
				name (.-name el)
				file (aget (.-files el) 0)
				form-data (doto
										(js/FormData.)
										(.append name file))]
		(if file
			(do (ajax-request
						{:uri             "/jus/upload"
						 :method          "POST"
						 :body            form-data
						 :handler         #()
						 :headers         {:X-CSRF-Token anti-forgery-token}
						 :response-format (json-response-format {:keywords? true})})
					(.-name file)))))


(defn upload-component []
	[:input {:style {:margin-bottom "10px"}
					 :type  "file"
					 :name  "upload-file"
					 :id    "upload-file"}])


(defn naredba-exist [naslov]
	(let [n-exist (first (filter #(= naslov (:JUSopis %)) (:data @jus-data)))]
		;(println "e " (:JUSId n-exist))
		(if n-exist (:JUSId n-exist) nil)))

(def postojeca (r/atom nil))

(defn add-nova-naredba-dialog-template [process-ok process-cancel]
	[v-box
	 :width "700px"
	 :children [(if (= (:naredba @nova-naredba-atom) 1)
								(f/panel
									"Dodaj naredbu: "
									(f/form
										(f/text "Naslov" nova-naredba-atom [:naslov] )
										(f/text "Glasnik" nova-naredba-atom [:glasnik])
										(f/text "Direktiva" nova-naredba-atom [:direktiva])
										(f/text "Link" nova-naredba-atom [:link])
										(f/text "File" nova-naredba-atom [:file])
										(upload-component)
										(f/form-buttons
											(f/button-primary "Snimi" #(process-ok))
											(f/button-default "Cancel" #(process-cancel)))))
								(f/panel
									"Dodaj naredbu: "
									(f/form
										(f/text {:on-blur  #(reset! postojeca (naredba-exist (h/get-value "naslov")))
																						:style (if @postojeca {:border-color "red"} {:border-color "lightgray"}) }
														"Naslov" nova-naredba-atom [:naslov]
														:warn-fn #(do (if @postojeca "Ova naredba postoji!"))
														)

										(f/text "Glasnik" nova-naredba-atom [:glasnik])
										(f/text "File" nova-naredba-atom [:file])
										(upload-component)
										(f/form-buttons
											(f/button-primary "Snimi" #(process-ok @postojeca))
											(f/button-default "Cancel" #(process-cancel))))))]])

(defn edit-nova-naredba-dialog-template [process-ok process-cancel]
	[v-box
	 :width "700px"
	 :children [
							(if (= (:naredba @nova-naredba-atom) 1)
								(f/panel
									"Uredi naredbu: "
									(f/form
										(f/text "Naslov" nova-naredba-atom [:naslov])
										(f/text "Glasnik" nova-naredba-atom [:glasnik])
										(f/text "Direktiva" nova-naredba-atom [:direktiva])
										(f/text "Link" nova-naredba-atom [:link])
										(f/text "File" nova-naredba-atom [:file])
										(upload-component)
										(f/form-buttons
											(f/button-primary "Snimi" #(process-ok))
											(f/button-default "Cancel" #(process-cancel)))
										))
								(f/panel
									"Dodaj naredbu: "
									(f/form
										(f/text "Naslov" nova-naredba-atom [:naslov])
										(f/text "Glasnik" nova-naredba-atom [:glasnik])
										(f/text "File" nova-naredba-atom [:file])
										(upload-component)
										(f/form-buttons
											(f/button-primary "Snimi" #(process-ok))
											(f/button-default "Cancel" #(process-cancel))))))]])


(defn tooltip-label [label1 label tip show-tooltip? ]
	(let [label-kw (keyword label1)
				showing? (r/atom (label-kw @show-tooltip?))
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
			;(println "row")
		[h-box
		 :class "rc-div-table-row"
		 :style (if (:selected row) {:border-style "outset" :border-color "#6F99AD" :border-width "3px"})
		 :children [(doall (for [param col-widths]
												 (let [column (key param)
															 params (val param)]
													 (with-meta
														 (condp = (:type params)
															 :label [label :label (column row) :width (:width params) :on-click
																			 (if (= (:action params) :click) (handler-fn ((:function params) row)))
																			 :attr {:on-double-click   #((:double-click params) row)}]
                               :href [hyperlink-href :label (column row) :href (if (:link-d row) (:link-d row) "http://bas.gov.ba") :target "_blank" :style {:width (:width params)} ]
															 :label-tooltip [label :label (tooltip-label column (column row) (str (:tooltip params) (column (:tooltip  row) ) ) show-tooltip?)	:width (:width params)]
															 :check-box [h-box :width (:width params) :children [[checkbox :model (column row) :disabled? (:disabled? params) :on-change #((:action params) row)]]]
															 :slider [h-box :width (:width params)  :children [[slider :width "30px" :min (:min params) :max (:max params) :step (:setp params) :model (column row) :disabled? nil :on-change #((:action params) row %)]]]
															 :row-button [h-box :gap (:gap params) :width (:width params) :justify (:justify params)
																						:children
																						[(doall (for [r-b (:children params)]
																											^{:key (:id r-b)}
																											[row-button :md-icon-name (:md-icon-name r-b) :mouse-over-row? mouse-over-row? :tooltip (:tooltip r-b)
																																				 :tooltip-position (:tooltip-position r-b) :disabled?  ((:disabled? r-b) row) :on-click #((:action r-b) level row)]))]] nil)
														 {:key (str column (:id row))}))))]]))



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
									 :children [
															[h-box
															 :class "rc-div-table-header"
															 :style {:color (level colors)}
															 :children
															 [(for [[label1 params] (columns col-widths-types)]
																	^{:key label1} [label :label (cond (:icon params) (:icon params)
																																		 (:label params) (:label params)
																																		 :else label1) :width (:width params)])]]
															(for [row  (sort-by :naslov (vals rows))]
																^{:key (:id row)} [data-row row col-widths-types level  (r/atom nil)])]]]]))
