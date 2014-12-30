(defproject acyclic/squiggly-clojure "0.1.2-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [environ "1.0.0"]
                 [org.clojure/core.typed "0.2.72"]
                 [jonase/eastwood "0.2.1" :exclusions [org.clojure/clojure]]
                 [jonase/kibit "0.0.8"]]


  :deploy-repositories [["clojars" :clojars]
                        ;["clojars" {:url https://clojars.org/repo :creds :gpg}]
                        ]

  :scm {:name git
        :url "https://github.com/pnf/squiggly-clojure"}

  )

