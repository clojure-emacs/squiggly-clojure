;;; squiggly-clojure.el --- Flycheck: Clojure support    -*- lexical-binding: t; -*-
;;
;; Author: Peter Fraenkel <pnf@podsnap.com>
;; URL:
;; Version: 1.1.0
;; Package-Requires: ((cider "0.8.1") (flycheck "0.22-cvs1"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.


;; Copyright © 2014 Peter Fraenkel
;;
;;; Commentary:
;; Flycheck support for Clojure


;;; Code:

(require 'cider-client)
(require 'flycheck)
(require 'json)

;;;###autoload
(defgroup squiggly-clojure nil
  "Clojure support for Flycheck")

;;;###autoload
(defcustom squiggly-clojure-chat-level 1
  "Debug level."
  :group 'squiggly-clojure
  :type '(choice
	  (const :tag "Trace" 3)
	  (const :tag "Debug" 2)
	  (const :tag "Info" 1)
	  (const :tag "Muted" 0)))

;;;###autoload
(defcustom squiggly-clojure-checkers '(eastwood kibit typed)
  "Set of clojure checkers to apply."
  :group 'squiggly-clojure
  :type '(set
	  (const :tag "Eastwood" eastwood)
	  (const :tag "Kibit" kibit)
	  (const :tag "Typed Clojure" typed)))


(defun squiggly-clojure-message (level msg)
  "When chat level >= LEVEL, display MSG."
  (when (>= squiggly-clojure-chat-level level)
    (message msg)))

(defun squiggly-clojure-message-cb (level)
  "Create callback that prints msg when chat level >= LEVEL."
  (lambda (_buffer msg) (when (>= squiggly-clojure-chat-level level)
		     (message msg))))

(defun squiggly-get-rec-from-alist (al ks)
  "Extract a list of the values in AL with keys KS."
  (mapcar (lambda (k) (cdr (assoc k al))) ks))

;; Nb. the clojure output gets double-escaped, so we double-decode.
(defun squiggly-parse-json (s)
  "Extract file, line, column and msg fields from an alist, which was probably created by parsing the JSON form of core.typed output in parameter S."
  (squiggly-clojure-message 2 s)
  (mapcar (lambda (w) (squiggly-get-rec-from-alist w '(file line column msg level)))
	  (json-read-from-string (json-read-from-string s))))


(defun squiggly-cmdf-ew (ns)
  "Generate core.typed command from NS."
  (format
   "(do (require 'squiggly-clojure.core) (squiggly-clojure.core/check-ew '%s))" ns))

(defun squiggly-cmdf-tc (ns)
  "Generate core.typed command from NS."
  (format
   "(do (require 'squiggly-clojure.core) (squiggly-clojure.core/check-tc '%s))" ns))


;; Kibit command; just add filename.
(defun squiggly-cmdf-kb (fname)
  "Generate kibit command from FNAME."
  (format
   "(do (require 'squiggly-clojure.core) (squiggly-clojure.core/check-kb \"%s\"))" fname))

(defun squiggly-tuple-to-error (w checker buffer fname)
  "Convert W of form '(file, line, column, message) to flycheck error object.
Uses CHECKER, BUFFER, FNAME unmodified."
  (pcase-let* ((`(,file ,line ,column ,msg ,level) w))
    (flycheck-error-new-at line column (intern level) msg
			   :checker checker
			   :buffer buffer
			   :filename fname)))

(defun flycheck-clj-cider-start (checker callback)
  "Invoked by flycheck, which provides CHECKER for identification.
Error objects are passed in a list to the CALLBACK function."
  (let* ((buffer (current-buffer))
	 (fname  (buffer-file-name buffer))
	 (ns     (clojure-find-ns))
	 (cmd-ew (squiggly-cmdf-ew ns))
	 (cmd-tc (squiggly-cmdf-tc ns))
	 (cmd-kb (squiggly-cmdf-kb fname))
	 (errors ()))

    ;; cider-eval requests are queued

    (when (memq 'eastwood squiggly-clojure-checkers)
      (squiggly-clojure-message 2 cmd-ew)
      (cider-tooling-eval cmd-ew
			  (nrepl-make-response-handler
			   buffer
			   (lambda (_buffer value)
			     (mapc (lambda (w) (push (squiggly-tuple-to-error w checker buffer fname) errors))
				   (squiggly-parse-json value))
			     (squiggly-clojure-message 1 "Finished eastwood check."))
			   (squiggly-clojure-message-cb 2)
			   (squiggly-clojure-message-cb 2)
			   nil
			   (lambda (_buffer ex _rex _sess) (squiggly-clojure-message 1 (format "Eastwood not run: %s" ex))))))

    (when (memq 'typed squiggly-clojure-checkers)
      (squiggly-clojure-message 2 cmd-tc)
      (cider-tooling-eval cmd-tc
			  (nrepl-make-response-handler
			   buffer
			   (lambda (_buffer value)
			     (squiggly-clojure-message 1 "Finished core.typed check.")
			     (mapc (lambda (w) (push (squiggly-tuple-to-error w checker buffer fname) errors))
				   (squiggly-parse-json value)))
			   (squiggly-clojure-message-cb 2)
			   (squiggly-clojure-message-cb 2)
			   nil
			   (lambda (_buffer ex _rex _sess) (squiggly-clojure-message 1 (format "Typecheck not run: %s" ex))))))

    (when (memq 'kibit squiggly-clojure-checkers)
      (squiggly-clojure-message 2 cmd-kb)
      (cider-tooling-eval
       cmd-kb
       (nrepl-make-response-handler
	buffer
	(lambda (_buffer value)
	  (squiggly-clojure-message 1 "Finished kibit check.")
	  (mapc (lambda (w) (push (squiggly-tuple-to-error w checker buffer fname) errors))
		(squiggly-parse-json value)))
	(squiggly-clojure-message-cb 2)
	(squiggly-clojure-message-cb 2)
	nil
	(lambda (_buffer ex _rex _sess) (squiggly-clojure-message 1 (format "Kibit not run: %s %s" cmd-kb ex))))))

    (squiggly-clojure-message 2 "Launched all checkers.")
    (cider-tooling-eval "true"
		(nrepl-make-response-handler
		 buffer
		 (lambda (_buffer _value)
		   (squiggly-clojure-message 1 "Finished all clj checks.")
		   (funcall callback 'finished errors))
		 nil
		 nil
		 '()))
    ))

(flycheck-define-generic-checker 'clojure-cider-checker
  "A syntax checker for Clojure using Cider"
  :start #'flycheck-clj-cider-start
  :modes '(clojure-mode)
  :predicate ((lambda () cider-mode))
  )

(add-to-list 'flycheck-checkers 'clojure-cider-checker)

(provide 'squiggly-clojure)
;;; squiggly-clojure.el ends here


