(ns jus.table
	(:require [jus.views :as views]
						[jus.db :as db]
						[jus.md-circle-icon-button :refer [icons example-icons]]
						[reagent.core :as r]))




(def data-source
[{:key "1"
	:id       "1"
	 :text     "No 1"
	 :icon "glyphicon glyphicon-leaf"
	 :opened   true
	 :selected true
	 :children [

							{:key "2"
							 :id       "2"
							 :text     "No 2"
							 :icon "glyphicon glyphicon-folder-open"
							 :opened   true
							 :selected true
							 :children []
							 }

							{:key "3"
							 :id       "3"
							 :text     "No 3"
							 :icon "glyphicon glyphicon-folder-open"
							 :opened   true
							 :selected true
							 :children []
							 }
							]
	 }]
	)



(defn make-wrapped-component
	"Wrap a React component in such a way that the raw JS component is accessible.
	 Useful for for calling methods on the native React component."
	[component]
	(fn [attrs & children]
		(r/create-class
			{:component-did-mount
			 (fn [this]
				 (when-let [component-ref (:ref attrs)]
					 (reset! component-ref (aget this "refs" "_this"))))

			 :reagent-render
			 (fn [attrs & children]
				 (apply
					 r/create-element
					 component
					 (-> attrs
							 (dissoc :ref)
							 (assoc :ref "_this")
							 (clojure.set/rename-keys {:class :className})
							 reagent.impl.component/camelify-map-keys            ; maybe best not to use reagent.impl.* functions...
							 reagent.impl.component/map-to-js
							 )
					 children))})))


;; Usage:
(def my-wrapped-component (make-wrapped-component  js/Treeview))



(defn my-component []
	(let [my-atom (atom nil)]
		(fn []
			[:div
			 [my-wrapped-component {:ref my-atom :dataSource  (clj->js data-source)	:onTreenodeClick #(println "sd")}]
			 ;[:div {:on-click #(.onTreenodeClick @my-atom)} "Click me to call callFunction"]
			 ])))

;; NOTE: callFunction would need an extern for advanced compilation




(defn mount-root []
	(r/render [views/main-panel]
						(.getElementById js/document "app")))

(defn ^:export init []
	(mount-root))

