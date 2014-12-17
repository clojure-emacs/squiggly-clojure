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

### Dependencies in emacs:

* cider
* flycheck
* flycheck-pos-tip

The latter is strongly recommended, so that linting and type errors don't clash with cider
auto-documentation.

~~~.el
(add-hook 'cider-mode-hook (lambda () (flycheck-mode 1)))
(eval-after-load 'flycheck
  '(custom-set-variables
   '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))
~~~
  
### Dependencies in Clojure:

The clojure code to invoke the various specific linters is in

~~~.clj
[acyclic/squiggly-clojure "0.1.0-SNAPSHOT"]

~~~

It pulls in

~~~.clj
  [org.clojure/core.typed "0.2.72"]
  [jonase/eastwood "0.2.0" :exclusions [org.clojure/clojure]]
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
