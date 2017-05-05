(ns jus.routes.home
  (:require [compojure.core :refer [defroutes GET POST]]
            [jus.layout :as layout]
            [ring.util.response :refer [response]]))

(defn home-page []
  (layout/render "home/index.html"))

(defn contact-page []
  (layout/render "home/contact.html"))

(defn tos-page []
  (layout/render "home/tos.html"))

(defn cookies-page []
  (layout/render "home/cookies.html"))

(defn jus-pretraga []
  (layout/render "home/jus-pretraga.html"))

(defn jus-admin []
  (layout/render "home/jus.html"))



(defroutes home-routes
           (GET "/contact" [] (contact-page))
           (GET "/tos" [] (tos-page))
           (GET "/cookies" [] (cookies-page))
           (GET "/" [] (home-page))
           (GET "/jus/pretraga" [] (jus-pretraga))
           (GET "/jus" [] (jus-admin)))
