(defproject bas-web "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}

  :source-paths ["src/clj" "src/cljs" "src/cljc"]

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.170"]
                 [org.clojure/core.cache "0.6.4"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                 [ring "1.4.0"]
                 [lib-noir "0.9.9"]
                 [ring/ring-anti-forgery "1.0.0"]
                 [compojure "1.4.0"]
                 [reagent "0.5.1"]
                 [reagent-reforms "0.4.3"]
                 [re-frame "0.6.0"]
                 [re-com "0.8.0"]
                 [environ "1.0.0"]
                 [leiningen "2.5.1"]
                 [http-kit "2.1.19"]
                 [selmer "0.8.5"]
                 [prone "0.8.2"]
                 [im.chit/cronj "1.4.3"]
                 [com.taoensso/timbre "4.1.4"]
                 [noir-exception "0.2.5"]
                 [buddy/buddy-auth "0.6.0"]
                 [buddy/buddy-hashers "0.6.0"]
                 [log4j "1.2.17" :exclusions [javax.mail/mail
                                              javax.jms/jms
                                              com.sun.jdmk/jmxtools
                                              com.sun.jmx/jmxri]]
                 [org.clojure/java.jdbc "0.3.7"]
                 [korma "0.4.2"]
                 [com.h2database/h2 "1.4.187"]
                 [org.xerial/sqlite-jdbc "3.8.10.1"]
                 [com.draines/postal "1.11.3"]
                 [jarohen/nomad "0.7.1"]
                 [de.sveri/clojure-commons "0.2.0"]
                 [clojure-miniprofiler "0.4.0"]
                 [cheshire "5.5.0"]
                 [org.danielsz/system "0.2.0"]
                 [datascript "0.13.3"]
                 [cljs-ajax "0.5.3"]
                 [ring-transit "0.1.4"]
                 [com.lucasbradstreet/cljs-uuid-utils "1.0.2"]
                 [net.tanesha.recaptcha4j/recaptcha4j "0.0.8"]
                 [com.taoensso/tower "3.0.2"]
                 [org.clojure/core.typed "0.3.11"]
                 [prismatic/plumbing "0.5.0"]
                 [prismatic/schema "1.0.3"]
                 [com.rpl/specter "0.8.0"]
                 [joplin.jdbc "0.3.6"]
                 [joplin.core "0.3.6"]]

  :plugins [[de.sveri/closp-crud "0.1.4"]
            [lein-cljsbuild "1.1.1"]]

  :closp-crud {:jdbc-url               "jdbc:sqlite:./db/jus-new.sqlite"
               :migrations-output-path "./resources/migrators/sqlite"
               :clj-src                "src/clj"
               :ns-db                  "jus.db"
               :ns-routes              "jus.routes"
               :ns-layout              "jus.layout"
               :templates              "resources/templates"}

  :min-lein-version "2.5.3"

  ; leaving this commented because of: https://github.com/cursiveclojure/cursive/issues/369
  ;:hooks [leiningen.cljsbuild]

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]

  :cljsbuild
  {:builds {:dev {:source-paths ["src/cljs" "src/cljc" "env/dev/cljs"]
                  :figwheel     {:css-dirs   ["resources/public/css"] ;; watch and update CSS
                                 :on-jsload  "closp-new.dev/main"}
                  :compiler     {:main       "closp-new.dev"
                                 :asset-path "/js/compiled/out"
                                 :output-to  "resources/public/js/compiled/app.js"
                                 :output-dir "resources/public/js/compiled/out"}}
            :adv {:source-paths ["src/cljs" "src/cljc"]
                  :compiler     {:output-to   "resources/public/js/compiled/app.js"
                                 ; leaving this commented because of: https://github.com/cursiveclojure/cursive/issues/369
                                 :jar           true
                                 :optimizations :advanced
                                 :pretty-print  false}}}}


  :profiles {:dev     {:repl-options {:init-ns jus.user
                                      :nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]
                                      :timeout 240000}

                       :plugins      [[lein-ring "0.9.0"]
                                      [lein-figwheel "0.5.0-2"]
                                      [test2junit "1.1.1"]]

                       :dependencies [[org.bouncycastle/bcprov-jdk15on "1.52"]
                                      [org.apache.httpcomponents/httpclient "4.5.1"]
                                      [clj-webdriver "0.7.2"]
                                      [org.seleniumhq.selenium/selenium-java "2.48.2"]
                                      [com.cemerick/piggieback "0.2.1"]
                                      [figwheel-sidecar "0.5.0-1"]
                                      [ring-mock "0.1.5"]
                                      [ring/ring-devel "1.4.0"]
                                      [pjstadig/humane-test-output "0.7.0"]]

                       :injections   [(require 'pjstadig.humane-test-output)
                                      (pjstadig.humane-test-output/activate!)]}

             :uberjar {:auto-clean  false                         ; not sure about this one
                       :omit-source true
                       :optimizations :advanced
                       :aot         :all
                       :cljsbuild {:jar true
                                   :builds {:adv
                                            {:source-paths ["src/cljs" "src/cljc"]
                                             :compiler     {:optimizations :advanced
                                                            :pretty-print false}}}}}}

  :test-paths ["test/clj" "integtest/clj"]

  :test-selectors {:unit        (complement :integration)
                   :integration :integration
                   :cur         :cur                                 ; one more selector for, give it freely to run only
                   ; the ones you need currently
                   :all         (constantly true)}

  :test2junit-output-dir "test-results"

  :main jus.core

  :uberjar-name "jus-new.jar"

  :aliases {"rel-jar"  ["do" "clean," "cljsbuild" "once" "adv," "uberjar"]
            "unit"     ["do" "test" ":unit"]
            "integ"    ["do" "test" ":integration"]

            ; migration utilities
            "migrate"  ["run" "-m" "joplin.alias/migrate" "joplin.edn" "sqlite-dev-env" "sqlite-dev"]
            "rollback" ["run" "-m" "joplin.alias/rollback" "joplin.edn" "sqlite-dev-env" "sqlite-dev"]
            "reset"    ["run" "-m" "joplin.alias/reset" "joplin.edn" "sqlite-dev-env" "sqlite-dev"]})
