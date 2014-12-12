squiggly-clojure
================

Flycheck checker for Clojure, using eastwood and core.typed.

Dependencies in emacs:

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
  
Dependencies in Clojure:

* eastwood
* cider
* core.typed

~~~.clj
  [org.clojure/core.typed "0.2.72"]
  [jonase/eastwood "0.2.0" :exclusions [org.clojure/clojure]]
~~~

TODO:
* Deal better with catastrophic failure of either eastwood or core.typed.
* Deal better with missing clojure dependencies or lack of a REPL.
* Configuration options, in case you want to check different things.
* Performance optimizations: throttling and narrowing.
