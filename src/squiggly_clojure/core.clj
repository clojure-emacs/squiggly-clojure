(ns  squiggly-clojure.core)


(defn check-ew [ns]
  (do (require 'eastwood.lint)
      (eastwood.lint/eastwood {:source-paths ["src"] :namespaces [ns] } )))

(defn check-tc [ns]
  (do (require 'clojure.core.typed)
      (require 'clojure.data.json)
      (clojure.data.json/write-str
       (map (fn [e] (assoc (:env (ex-data e)) :msg (.getMessage e)))
            (:delayed-errors (clojure.core.typed/check-ns-info ns))))))


(defn check-kb [fname]
  (require 'kibit.check)
  (require 'clojure.data.json)
  (def _squiggly (atom []))
  (kibit.check/check-file
   fname
   :reporter (fn [e] (swap! _squiggly conj
                           (-> e
                             (update-in [:expr] print-str)
                             (update-in [:alt] print-str)))))
  (clojure.data.json/write-str @_squiggly))




