(ns jus.core
  (:require [taoensso.timbre :as timbre]
            [reloaded.repl :refer [go]]
            [jus.cljccore :as cljc]
            [jus.components.components :refer [prod-system]])
  (:gen-class))

(defn -main [& args]
  (reloaded.repl/set-init! prod-system)
  (go)
  (cljc/foo-cljc "hello from cljx")
  (timbre/info "server started."))
