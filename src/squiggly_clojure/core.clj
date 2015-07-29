(ns squiggly-clojure.core
  (:require eastwood.lint
            kibit.check
            clojure.data.json
            clojure.core.typed
            environ.core
            ))

(defn env [ns]
  (or (:squiggly (meta (the-ns ns)))
      (:squiggly environ.core/env)))

(defn do-lint? [checker ns]
  (and (find-ns ns)
       (if-let [checkers (:checkers (env ns))]
         (some #{checker} checkers) true)))

(defn eastwood-exclude-linters [ns]
  (if-let [excl (:eastwood-exclude-linters (env ns))]
    excl []))

(defn permissive-reader [tag value] value)

(defmacro exception->error [form]
  `(try ~form
        (catch Exception e#
          (clojure.data.json/write-str (list {:line 1, :column 1 :msg (str e#) :level :error})))))

(defn check-ew [ns & [opts]]
  (if-not (do-lint? :eastwood ns) "[]"
          (exception->error (binding [*default-data-reader-fn* permissive-reader]
                              (let [ls (:warnings (eastwood.lint/lint {:source-paths ["src"]
                                                                       :namespaces [ns]
                                                                       :continue-on-exception true
                                                                       :exclude-linters (eastwood-exclude-linters ns)
                                                                       }))
                                    ws (map #(assoc (select-keys % [:line :column :msg])
                                                    :file (str (:uri %))
                                                    :level :warning) ls)]
                                (println ws)
                                (clojure.data.json/write-str ws))))))

(def ns->type-map (atom {}))
(defn set-file-mapping! [ns tmap]
  (swap! ns->type-map assoc ns
         (into {} (for [[{:keys [line column]} t] tmap]
                    [[line column] t]))))


(defn check-tc [ns]
  (if-not (do-lint? :typed ns) "[]"
          (exception->error 
           (binding [*default-data-reader-fn* permissive-reader]
             (let [cni  (clojure.core.typed/check-ns-info ns :file-mapping (:typed-file-mapping (env ns)))
                   errs (map (fn [e] (assoc (:env (ex-data e))
                                           :level :error
                                           :msg (.getMessage e)))
                             (:delayed-errors cni))
                   tmap (:file-mapping cni)]
               (set-file-mapping! ns tmap)
               (clojure.data.json/write-str errs))))))

(defn build-type-map [ns]
(let [cni  (clojure.core.typed/check-ns-info ns :file-mapping true)
      tmap (:file-mapping cni)]
  (set-file-mapping! ns tmap)
  (count tmap)))

(defn check-kb [ns fname]
  (if-not (do-lint? :kibit ns) "[]"
          (exception->error
           (binding [*default-data-reader-fn* permissive-reader]
             (let  [_squiggly (atom [])]
               (kibit.check/check-file
                fname
                :reporter (fn [e] (swap! _squiggly conj
                                        (hash-map
                                         :msg (str "Kibit suggests using\n" (print-str (:alt e)) "\ninstead of \n" (print-str (:expr e)))
                                         :file (:file e)
                                         :line (:line e)
                                         :level :warning))))
               (clojure.data.json/write-str @_squiggly))))))

