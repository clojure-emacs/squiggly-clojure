(ns sample-project.core
  (:require [clojure.core.typed]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))


(defn fly-tests []

  (inc "foo")

  (map inc [1 2 3])

  (+ 3))
