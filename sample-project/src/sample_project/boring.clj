(ns sample-project.boring
  {:squiggly {:checkers [:kibit :eastwood :typed]
              :eastwood-exclude-linters [:unlimited-use]}}
  (:require [clojure.core.typed])
  (:use [clojure.stacktrace])  ;; warning suppressed by :eastwood-exclude-linters
  )

;; no errors here
(defn fly-tests []

  (=
   (inc 3)
   (map inc [1 2 3]))

)

;; Substitute in Local Variables region to disable one or more checkers.
;; flycheck-disabled-checkers: (clojure-cider-typed clojure-cider-kibit clojure-cider-eastwood)
;; Local Variables:
;; flycheck-disabled-checkers: ()
;; End:
