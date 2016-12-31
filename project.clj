(defproject acyclic/squiggly-clojure "0.1.7"
  :description "Flycheck for Clojure"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [environ "1.0.0"]
                 [org.clojure/core.typed "0.3.26"]
                 [org.clojure/data.json "0.2.6"]
                 [jonase/eastwood "0.2.3" :exclusions [org.clojure/clojure]]
                 [jonase/kibit "0.1.3"]]


  :deploy-repositories [["clojars" :clojars]
                        ;["clojars" {:url https://clojars.org/repo :creds :gpg}]
                        ]

  :scm {:name git
        :url "https://github.com/pnf/squiggly-clojure"}

  )

