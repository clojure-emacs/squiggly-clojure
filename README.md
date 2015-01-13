squiggly-clojure
================

![type error](./cant/squiggle1.png)

Flycheck checker for Clojure, using
[eastwood](https://github.com/jonase/eastwood),
[core.typed](http://typedclojure.org/)
and
[kibit](https://github.com/jonase/kibit)
via
[cider](https://github.com/clojure-emacs/cider).

See this [blog post](http://blog.podsnap.com/squiggly.html) for more.

### Warning!

Please read the documentation for each linter.  Some of them come with warnings.

### Installation

The package ~~is available~~ will soon be on [melpa](http://melpa.org/):

    M-x package-install squiggly-clojure

(In the mean time, just put the the ```squiggly-clojure.el``` somwewhere on your
load path.)

Add to your ```.emacs```:

~~~.el
(eval-after-load 'flycheck '(flycheck-clojure-setup))
(global-flycheck-mode)
~~~

Installing [flycheck-pos-tip](https://github.com/flycheck/flycheck-pos-tip) as well is strongly recommended, so that linting and type errors don't clash with cider's eldoc information.

    M-x package-install flycheck-pos-tip

And add this to your `.emacs`:

~~~.el
(require-package 'flycheck)
(require-package 'flycheck-clojure)
(require-package 'flycheck-pos-typ)
(require 'flycheck)
(eval-after-load 'flycheck '(flycheck-clojure-setup))
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'flycheck
  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
~~~

### Dependencies in Clojure:

The clojure code used to invoke the various specific linters is in

~~~.clj
[acyclic/squiggly-clojure "0.1.2-SNAPSHOT"]
~~~

You should probably add that to your `profiles.clj`.

It pulls in

~~~.clj
  [org.clojure/core.typed "0.2.72"]
  [jonase/eastwood "0.2.1" :exclusions [org.clojure/clojure]]
  [jonase/kibit "0.0.8"]
~~~

### Configuration

See the `sample-project` subdirectory for examples of the configuration methods
described below.

#### From Emacs

Squiggly Clojure comprises three Flycheck checkers, `clojure-cider-typed`,
`clojure-cider-kibit` and `clojure-cider-eastwood`.  You may exclude one or
more of these by including them in the `flycheck-disabled-checkers` list.
This can be done via the `Local Variables:` block at the end of a `.clj` file,
e.g.

~~~.clj
;; Local Variables:
;; flycheck-disabled-checkers: (clojure-cider-kibit)
;; End:
~~~

but the following two methods are preferable for such persistent settings.

#### In `project.clj`

Add or merge

~~~.clj
  :plugins [[lein-environ "1.0.0"]]
~~~

and set an `:env` option map that includes `:squiggly` , e.g.

~~~.clj
{:env {:squiggly {:checkers [:eastwood]
                  :eastwood-exclude-linters [:unlimited-use]}}}
~~~

Here, you specify *included* checkers, as `:eastwood`, `:kibit` or `:typed`.  If you
set `:eastwood-exclude-linters`, it will be passed directly to Eastwood as described
in its documentation.  This configuration will apply to all source files in the
project unless overridden by...

#### Namespace metadata

E.g.

~~~.clj
(ns sample-project.core
  {:squiggly {:checkers [:eastwood :typed]
              :eastwood-exclude-linters [:unlimited-use]}}
  (:require [clojure.core.typed])
  (:use [clojure.stacktrace])     ;; warning suppressed by :eastwood-exclude-linters
  )
~~~


#### Precedence 

If set in the metadata, the value of `:squiggly` fully overrides anything set in the
`project.clj`: no fancy merging is performed.

And note that, if a checker is in `flycheck-disabled-checkers`, it will never be invoked
no matter what you set in Clojure code.

### Debugging

Many things can go wrong.

* Neither ```eastwood``` nor ```kibit``` is usually run repeatedly and in
  the REPL, so we may trigger bugs that don't matter to other people.
* There may be circumstances under which the checkers return output
  in an unanticipated form, which will then be unparseable in emacs.
* I don't really know emacs lisp.  Could be an issue.

If, due to one of these or other problems, ```flycheck``` does not
receive the proper callbacks, it may be stuck in a state where it
will never try to check again.  To reset (modulo some memory leaks perhaps)
try turning ```flycheck-mode``` off and then on.

If something mysterious is happening, you may find it helpful to look at the
`*nrepl-messages*` buffer, where CIDER silently logs all traffic between EMACS
and Clojure.  Among other things, you'll find here the Clojure expressions that
were evaluated to initiate the checking, so you can run these directly from the REPL
yourself.





### TODO:
* Deal better with catastrophic failure of a checker.  Currently, we silently ignore exceptions.
* Performance optimizations: throttling and narrowing.
