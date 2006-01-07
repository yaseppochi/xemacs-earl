;;; neon-test-user.el --- configuration file for the neon-test.el test suite

;; Copyright (C) 2006  Stephen J. Turnbull

;; To configure, rename this file to "neon-test-user.el", change the values
;; as described below, and byte-compile (optional).

;; Configuration: Replace the string with a server that does WebDAV.

(setq test-server "http://somewhere.over-the-rainbow.invalid/")

;; Configuration: Replace the string with a non-collection resource.

(setq test-path "/path/nonexistent/too")

;; Configuration: Replace the literal strings "user" and "secret" with
;; a username and password authorized to access properties of the resource.

(defun test-auth-cb (iggy pop)
  "Callback to authenticate as \"user\" with password \"secret\".
IGGY is a string, the HTTP realm offered by the server \(currently ignored).
POP is an integer, the current count of previous \(failed) attempts \(we give
  up after 3 failures).
The literals \"user\" and \"secret\" are passed back verbatim.

To configure, replace the string literals in the code in \"neon-test-user.el\"."
  ;; IGGY is ignored in this sample callback.
  ;; We restrict consecutive failures to 3.  neon will try indefinitely, so
  ;; we must do the restriction.
  (if (>= pop 3)
      pop				; hackish way to abort authentication
    '("user" . "secret")))

;; Currently unused, specifies RFC 2617 basic authentication with the same
;; user and password ("user:secret") as test-auth-cb above.  The token
;; following "Basic" is simply "user:secret" BASE64-encoded, for example by
;; M-x base64-encode-region.

(defvar test-auth-header '("Authorization" "Basic dXNlcjpzZWNyZXQ="))

(provide 'neon-test-user)
;;; end neon-test-user.el
