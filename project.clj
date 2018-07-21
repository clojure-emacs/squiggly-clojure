(defproject acyclic/squiggly-clojure "0.1.9-SNAPSHOT"
  :description "Flycheck for Clojure"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [environ "1.1.0"]
                 [org.clojure/core.typed "0.5.3" :exclusions [org.clojure/clojure] :classifier "slim"]
                 [org.clojure/data.json "0.2.6"]
                 [jonase/eastwood "0.2.8" :exclusions [org.clojure/clojure]]
                 [jonase/kibit "0.1.6"]]


  :deploy-repositories [["clojars" :clojars]
                        ;["clojars" {:url https://clojars.org/repo :creds :gpg}]
                        ]

  :scm {:name git
        :url "https://github.com/pnf/squiggly-clojure"}

  )
