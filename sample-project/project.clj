(defproject sample-project "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.typed "0.3.32"  :classifier "slim"]]

  ;; Use lein-environ plugin if you want to set checker configuration in profile.
  :plugins [[lein-environ "1.0.0"]]

  ;; Configuration here may be overridden by namespace metadata.
  :profiles {:dev {:env {:squiggly {:checkers [:eastwood :typed]
                                    :eastwood-exclude-linters [:unlimited-use]
                                    :eastwood-options {;; :builtin-config-files ["myconfigfile.clj"]
                                                       }}}}})

