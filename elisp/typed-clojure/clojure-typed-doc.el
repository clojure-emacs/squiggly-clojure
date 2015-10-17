;;; clojure-typed-doc.el --- Display inferred Clojure types -*- lexical-binding: t -*-

;; Copyright © 2014 Peter Fraenkel
;;
;; Author: Peter Fraenkel <pnf@podsnap.com>
;; Maintainer: Peter Fraenkel <pnf@podsnap.com>
;; URL: https://github.com/clojure-emacs/squiggly-clojure
;; Version: 1.1.0
;; Package-Requires: ((cider "0.8.1") (emacs "24"))

;; This file is not part of GNU Emacs.

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

;;; Commentary:

;; Add type display to cider.

;;; Code:

(require 'cider-client)
(require 'flycheck)

(defun cider-clojure-typed-eval (input callback)
  "Send the request INPUT and register the CALLBACK as the response handler.
Uses the tooling session, with no specified namespace."
  (cider-nrepl-request:eval input callback))


(defun infer-clojure-types (&optional ns)
  "Run check-ns-info in Clojure for the given NS, or the current one as inferred by CIDER if omitted."
  (interactive)
  (let ((ns (or ns (clojure-find-ns))))
    (cider-clojure-typed-eval
     (format "(do (require 'squiggly-clojure.core) (squiggly-clojure.core/build-type-map '%s))" ns)
     (nrepl-make-response-handler
      (current-buffer)
      (lambda (buffer value) (message (format "Type inference for %s return %s results."
					 ns value)))
      nil ; stdout
      nil ; stderr
      (lambda (_)
	(message (format "Type inference for %s inexplicably finished." ns)))
      (lambda (_buffer ex _rootex _sess)
	(message (format "Type inference for %s failed with error %s" ns ex)))))))

(defun inferred-type-at-point (&optional ns)
  "Report inferred type for symbol at point in namespace NS, which is inferred if missing."
  (interactive)
  (let* ((c (+ 1 (current-column)))
	(l (line-number-at-pos))
	(ns (or ns (clojure-find-ns)))
	(cmd (format "(get-in @squiggly-clojure.core/ns->type-map ['%s [%d %d]])"
		     ns l c)))
    (message cmd)
    (cider-clojure-typed-eval
     cmd
     (nrepl-make-response-handler
      (current-buffer)
      (lambda (buffer value) (message (format "Type %s" value)))
      nil ; stdout
      nil ; stderr
      nil ; completion without value
      (lambda (_buffer ex _rootex _sess) (message "Horrible error: %s" ex))))))


(provide 'clojure-typed-doc)

;;; clojure-typed-doc.el ends here
