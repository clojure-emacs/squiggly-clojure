(ns  squiggly-clojure.core)


;; Temporarily, until this is released by eastwood

(comment eastwood-lint-lint
  "Invoke Eastwood from REPL or other Clojure code, and return a map
containing these keys:
  :warnings - a sequence of maps representing individual warnings.
      The warning map contents are documented below.
  :err - nil if there were no exceptions thrown or other errors that
      stopped linting before it completed.  A keyword identifying a
      kind of error if there was.  See the source file
      src/eastwood/lint.clj inside Eastwood for defmethod's of
      error-msg.  Each is specialized on a keyword value that is one
      possible value the :err key can take.  The body of each method
      shows how Eastwood shows to the user each kind of error when it
      is invoked from the command line via Leiningen, serves as a kind
      of documentation for what the value of the :err-data key
      contains.
  :err-data - Some data describing the error if :err's value is not
      nil.  See :err above for where to find out more about its
      contents.
Keys in a warning map:
  :uri-or-file-name - string containing file name where warning
      occurs, relative to :cwd directory of options map, if it is a
      file inside of that directory, or a URI object,
      e.g. \"cases/testcases/f02.clj\"
  :line - line number in file for warning, e.g. 20.  The first line in
      the file is 1, not 0.  Note: In some cases this key may not be
      present, or the value may be nil.  This is an areas where
      Eastwood will probably improve in the future, but best to handle
      it for now, perhaps by replacing it with line 1 as a
      placeholder.
  :column - column number in file for warning, e.g. 42.  The first
      character in the file is column 1, not 0.  Same comments apply
      for :column as for :line.
  :linter - keyword identifying the linter, e.g. :def-in-def
  :msg - string describing the warning message, e.g. \"There is a def
      of i-am-inner-defonce-sym nested inside def
      i-am-outer-defonce-sym\"
  :uri - object with class URI of the file, *or* a URI within a JAR
       file, e.g.  #<URI file:/Users/jafinger/clj/eastwood/0.2.0/eastwood/cases/testcases/f02.clj>
  :namespace-sym - symbol containing namespace, e.g. testcases.f02,
  :file - string containing resource name, relative to some
      unspecified path in the Java classpath,
      e.g. \"testcases/f02.clj\""
  [opts]
  )


(defn check-ew [ns & [opts]]
  (do (require 'eastwood.lint)
      (require 'clojure.data.json)
      (let [lint-warnings (atom [])
            opts (merge {:cwd (.getCanonicalFile (clojure.java.io/file "."))} opts)
            cb (fn cb [info]
                 (case (:kind info)
                   :lint-warning (swap! lint-warnings conj (:warn-data info))
                   :default))  ; do nothing with other kinds of callbacks
            opts (if (contains? opts :callback)
                   opts
                   (assoc opts :callback cb))
            {:keys
             [err err-data warning-count exception-count]
             :as ret}              (eastwood.lint/eastwood-core opts)
            ws (map #(assoc (select-keys % [:line :column :msg])
                            :file (str (:uri %))
                            :level :warning) @lint-warnings)]
        ;;{:warnings @lint-warnings :err err   :err-data err-data}
        (clojure.data.json/write-str ws))
      ;(eastwood.lint/eastwood {:source-paths ["src"] :namespaces [ns] } )
      ))

(defn check-tc [ns]
  (do (require 'clojure.core.typed)
      (require 'clojure.data.json)
      (clojure.data.json/write-str
       (map (fn [e] (assoc (:env (ex-data e))
                          :level :error
                          :msg (.getMessage e)))
            (:delayed-errors (clojure.core.typed/check-ns-info ns))))))


(defn check-kb [fname]
  (require 'kibit.check)
  (require 'clojure.data.json)
  (let  [_squiggly (atom [])]
        (kibit.check/check-file
         fname
         :reporter (fn [e] (swap! _squiggly conj
                                 (-> e
                                   (assoc :level :warning)
                                   (update-in [:expr] print-str)
                                   (update-in [:alt] print-str))))))
  (clojure.data.json/write-str @_squiggly))




