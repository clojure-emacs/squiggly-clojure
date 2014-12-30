(ns sample-project.core
  {:squiggly {:checkers [:kibit :eastwood :typed]
              :eastwood-exclude-linters [:unlimited-use]}}
  (:require [clojure.core.typed])
  (:use [clojure.stacktrace])  ;; warning suppressed by :eastwood-exclude-linters
  )

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
 

(defn fly-tests []

  (inc "foo")

  (map inc [1 2 3])

  (+ 3))

;; Substitute in Local Variables region to disable one or more checkers.
;; flycheck-disabled-checkers: (clojure-cider-typed clojure-cider-kibit clojure-cider-eastwood)
;; Local Variables:
;; flycheck-disabled-checkers: ()
;; End:
