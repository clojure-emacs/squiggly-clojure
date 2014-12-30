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
(eval-after-load 'flycheck
  '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
~~~

### Dependencies in Clojure:

The clojure code used to invoke the various specific linters is in

~~~.clj
[acyclic/squiggly-clojure "0.1.1-SNAPSHOT"]
~~~

You should probably add that to your `profiles.clj`.

It pulls in

~~~.clj
  [org.clojure/core.typed "0.2.72"]
  [jonase/eastwood "0.2.1" :exclusions [org.clojure/clojure]]
  [jonase/kibit "0.0.8"]
~~~

and there is currently no way to load linters selectively.

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

If something mysterious is happening, you can set ```squiggly-clojure-chat-level``` to a number
higher than 1 and examine the contents of ```**Messages**```.  (You can also set it to 0 and
shut off verbal communication entirely.)


### TODO:
* Deal better with catastrophic failure of a checker.  Currently, we silently ignore exceptions.
* Configuration options, in case you want to check different things.
* Performance optimizations: throttling and narrowing.
