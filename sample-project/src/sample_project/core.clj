(ns sample-project.core
  {:squiggly {:checkers [:kibit :eastwood :typed]
              :eastwood-exclude-linters [:unlimited-use]
              :eastwood-options {:not-a-real-option "foo"}}}
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


(defn some-function [x]
  ;; #break
  (reduce + (range x))
  ;; The undefined variable here will cause an exception in type-checker. The exception
  ;; will be shown as an error at the first line of this file, but of course no other
  ;; type linting will be reported.
  ;; (+ bah)
  )

;; Substitute in Local Variables region to disable one or more checkers.
;; flycheck-disabled-checkers: (clojure-cider-typed clojure-cider-kibit clojure-cider-eastwood)
;; Local Variables:
;; flycheck-disabled-checkers: ()
;; End:
