(ns jus.table
  (:require [jus.views :as views]
            [jus.db :as db]
            [jus.md-circle-icon-button :refer [icons example-icons]]
            [reagent.core :as r]))






(defn mount-root []
  (r/render [views/main-panel]
            (.getElementById js/document "app")))

(defn ^:export init []
  (mount-root))

