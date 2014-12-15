squiggly-clojure
================

Flycheck checker for Clojure, using eastwood and core.typed.

### Dependencies in emacs:

* cider
* flycheck
* flycheck-pos-tip

The latter is strongly recommended, so that linting and type errors don't clash with cider
auto-documentation.

~~~.el
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
  '(custom-set-variables
   '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))
~~~
  
### Dependencies in Clojure:

* eastwood
* cider
* core.typed

If any these are missing, the corresponding checks will fail
silently but the others will run.  Note that all checks run within
the cider REPL.  Eastwood, at least, will load (but not evaluate)
the source file, so beware of side effects.

You can include the following as regular ```:dependencies``` or
in ```:profiles {:dev {:dependencies [ ... ] }} ```:

~~~.clj
  [org.clojure/core.typed "0.2.72"]
  [jonase/eastwood "0.2.0" :exclusions [org.clojure/clojure]]
  [jonase/kibit "0.0.8"]
~~~


### TODO:
* Deal better with catastrophic failure of a checker.  Currently, we silently ignore exceptions.
* Configuration options, in case you want to check different things.
* Performance optimizations: throttling and narrowing.
