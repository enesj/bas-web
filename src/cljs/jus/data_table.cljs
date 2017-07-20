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

(def path (r/atom nil))

(def table-state (r/atom {:table-height     nil :veza nil :path nil
                          :delete-modal     {:show? false :level nil :child nil} :nova-naredba-modal {:show? false :edit? false :naslov nil :direktiva nil :link nil :file nil}
                          :delete-jus-modal {:show? false :jusid nil}}))


(def jus-data (r/atom {:data nil}))

(def nova-naredba-atom (r/atom {:jusid nil :naslov nil :direktiva nil :link nil :file nil :glasnik nil :naredba nil :obavezan nil :X-CSRF-Token nil}))

(def colors
  {:0  "DarkBlue"
   :1  "Green"
   :2  "CadetBlue"
   :3  "GoldenRod"
   :4  "IndianRed"
   :5  "LimeGreen"
   :6  "Olive"
   :7  "GoldenRod"
   :8  "IndianRed"
   :9  "LimeGreen"
   :10 "Olive"
   :11 "GoldenRod"
   :12 "IndianRed"
   :13 "LimeGreen"
   :14 "Olive"})


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
    (if n-exist (:JUSId n-exist) nil)))

(def postojeca (r/atom nil))

(defn naredba-dialog-template [process-ok process-cancel]
  [v-box
   :width "700px"
   :children [(case (:naredba @nova-naredba-atom)
                1 (f/panel
                    "Dodaj naredbu: "
                    (f/form
                      (f/text "Naslov" nova-naredba-atom [:naslov])
                      (f/text "Glasnik" nova-naredba-atom [:glasnik])
                      (f/text "Direktiva" nova-naredba-atom [:direktiva])
                      (f/text "Link" nova-naredba-atom [:link])
                      (f/text "Napomena" nova-naredba-atom [:napomena])
                      (f/text "File" nova-naredba-atom [:file])
                      (upload-component)
                      (f/form-buttons
                        (f/button-primary "Snimi" #(process-ok))
                        (f/button-default "Cancel" #(process-cancel)))))
                (or 2 3) (f/panel
                           "Dodaj naredbu: "
                           (f/form
                             (if (:jusid @nova-naredba-atom)
                               (f/text "Naslov" nova-naredba-atom [:naslov])
                               (f/text {:on-blur #(reset! postojeca (naredba-exist (h/get-value "naslov")))
                                        :style   (if @postojeca {:border-color "red"} {:border-color "lightgray"})}
                                       "Naslov" nova-naredba-atom [:naslov]
                                       :warn-fn #(if @postojeca "Ova naredba postoji!")))

                             (f/text "Glasnik" nova-naredba-atom [:glasnik])
                             (f/text "Napomena" nova-naredba-atom [:napomena])
                             (f/text "File" nova-naredba-atom [:file])
                             (upload-component)
                             (f/form-buttons
                               (f/button-primary "Snimi" #(process-ok @postojeca))
                               (f/button-default "Cancel" #(process-cancel)))))
                0 (f/panel
                    "Dodaj JUS: "
                    (f/form
                      (f/text "JUS" nova-naredba-atom [:jusid])
                      (f/text "Naslov" nova-naredba-atom [:naslov])
                      (f/text "Godina" nova-naredba-atom [:godina])
                      (f/text "Napomena" nova-naredba-atom [:napomena])
                      (f/form-buttons
                        (f/button-primary "Snimi" #(process-ok))
                        (f/button-default "Cancel" #(process-cancel))))))]])



(defn tooltip-label [label1 label tip show-tooltip?]
  (let [label-kw (keyword label1)
        showing? (r/atom (label-kw @show-tooltip?))
        ic-label [:div
                  {:style         {:color "#555657"}
                   :on-mouse-over #(swap! show-tooltip? assoc-in [label-kw] 1)
                   :on-mouse-out  #(swap! show-tooltip? assoc-in [label-kw] nil)}
                  label]]
    [box :class "display-inline-flex" :align :center :child [popover-tooltip
                                                             :label tip
                                                             :position :below-center
                                                             :showing? showing?
                                                             :anchor ic-label]]))

(def obavezan {:0 "lightgray"
               :1 "orange"
               :2 "red"})

(defn data-row
  [row col-widths level show-tooltip?]
  (let [mouse-over-row? true]
    [h-box
     :class "rc-div-table-row"
     :style (if (:selected row) {:border-style "outset" :border-color "#6F99AD" :border-width "3px"})
     :children [(doall (for [param col-widths]
                         (let [column (key param)
                               params (val param)]
                           (with-meta
                             (condp = (:type params)
                               :label [label :label (if (:tooltip params) (tooltip-label column (column row) (str (:tooltip params) (column (:tooltip row))) show-tooltip?) (column row)) :width (:width params)
                                       :on-click (if (= (:action params) :click) (handler-fn ((:function params) row)))
                                       :attr {:on-double-click #((:double-click params) row)}]
                               :href [hyperlink-href :label (column row) :href (if (:link-d row) (:link-d row) "http://bas.gov.ba") :target "_blank" :style {:width (:width params)}]
                               :check-box [h-box :width (:width params) :children [[checkbox :model (column row)
                                                                                    :label (if (:tooltip params) (tooltip-label column (if-not (clojure.string/blank? (column (:tooltip row))) [:i.zmdi.zmdi-hc-fw-rc.zmdi-comment-text] nil) (str (:tooltip params) (column (:tooltip row))) show-tooltip?) nil)
                                                                                    :disabled? (:disabled? params) :on-change #((:action params) row)]]]
                               :slider [h-box :width (:width params) :style {:border (str "1px solid " ((keyword (str (column row))) obavezan))} :children [[slider :width "30px" :min (:min params) :max (:max params) :step (:setp params) :model (column row) :disabled? nil
                                                                                                                                                             :on-change #((:action params) row %)]]]

                               :row-button [h-box :gap (:gap params) :width (:width params) :justify (:justify params)
                                            :children
                                            [(doall (for [r-b (:children params)]
                                                      ^{:key (:id r-b)}
                                                      [row-button :md-icon-name (:md-icon-name r-b) :mouse-over-row? mouse-over-row? :tooltip (:tooltip r-b)
                                                       :tooltip-position (:tooltip-position r-b) :disabled? ((:disabled? r-b) row) :on-click #((:action r-b) level row)]))]] nil)
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
                            (for [row (sort-by :naslov (vals rows))]
                              ^{:key (:id row)} [data-row row col-widths-types level (r/atom nil)])]]]]))
