(ns squiggly-clojure.core
  (:require eastwood.lint
            kibit.check
            clojure.data.json
            clojure.core.typed))



(defn check-ew [ns & [opts]]
  (let [ls (:warnings (eastwood.lint/lint {:source-paths ["src"] :namespaces [ns]}))
        ws (map #(assoc (select-keys % [:line :column :msg])
                        :file (str (:uri %))
                        :level :warning) ls)]
    (clojure.data.json/write-str ws)))

(defn check-tc [ns]
  (clojure.data.json/write-str
   (map (fn [e] (assoc (:env (ex-data e))
                      :level :error
                      :msg (.getMessage e)))
        (:delayed-errors (clojure.core.typed/check-ns-info ns)))))


(defn check-kb [fname]
  (let  [_squiggly (atom [])]
        (kibit.check/check-file
         fname
         :reporter (fn [e] (swap! _squiggly conj
                                 (hash-map
                                  :msg (str "Kibit suggests using\n" (print-str (:alt e)) "\ninstead of \n" (print-str (:expr e)))
                                  :file (:file e)
                                  :line (:line e)
                                  :level :warning))))
        (clojure.data.json/write-str @_squiggly)))

