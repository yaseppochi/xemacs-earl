;;; neon-test.el --- ad hoc test suite for the neonapi ELL module.

;; Copyright (C) 2006  Stephen J. Turnbull

;;; Commentary

;; To configure, create a file named "neon-test-user.el" on load-path.

;; In it set the values of `neon-test-server' (an URL containing the scheme
;;  and authority parts), `neon-test-path-public' (an absolute path),
;; (an absolute path) `neon-test-path-private', `neon-test-user' (a string),
;; and `neon-test-secret' (a string).  Optionally, define a function
;; `neon-test-auth-cb'.  (This file provides a default definition which
;; should be sufficient for testing.)
;; We also provide a variable to define an authentication header, which is
;; a list of two strings (the tag and the contents).

;; This file also contains a LISP emulation of the algorithms used in the
;; callbacks in the C code.

;; Troubleshooting

;; Configuring any webserver is non-trivial; configuring DAV is painful.
;; Here are some based on personal experience (and hints only! this is a
;; troubleshooting checklist, not a DAV configuration tutorial).  See your
;; webserver's documentation for details.

;; 1. You should have a webserver that listens on port 80 (as almost all do
;;    by default).  The server's domain or IP address is configured in this
;;    test suite by setting `neon-test-server' to an appropriate string.
;; 2. This webserver should have a publicly available resource.  On most,
;;    the index of the DocumentRoot will do.  To configure the test suite,
;;    set `neon-test-path-public' to this path (e.g., "/").
;; 3. The webserver should have DAV configured.  For Apache with mod_dav, the
;;    LoadModule and AddModule directories should be specified.  (This is
;;    usually done by uncommenting the directives in httpd.conf or similar.)
;; 4. DAV should be turned on for a "private" resource (usually a directory).
;;    In Apache this is done with the Dav On directive in the scope of a
;;    <Directory> or <Location> element.  "/DAV/" will be used in examples.
;;    To configure the test suite, set `neon-test-path-private' to "/DAV/",
;;    and `neon-test-user' and `neon-test-secret' to a user and password
;;    granted access to "/DAV" by HTTP Basic Authentication.
;; 5. In Apache, DAV requires a lock database, which is an existing directory
;;    configured with the DAVLockDB directive.  It must be writable by Apache.
;;    It should be outside of the webserver's document space.
;; 6. DAV requires provision for authentication.  Currently this test module
;;    presumes HTTP Basic Authentication.  This is implemented in Apache by
;;    mod_auth.  Use the LoadModule and AddModule directives.
;; 7. Basic Authentication needs to be configured for <Location /DAV/>.  Use
;;    the AuthType Basic directive to turn on Basic Authentication, optionally
;;    an AuthName directive to set the authentication realm, the AuthUserFile
;;    directive to configure a password file (which should never be in the
;;    webserver's document space), optionally an AuthGroupFile, and a Require
;;    directive to specify the principal who must be authenticated to access
;;    the documents.  For Apache, you can create the AuthUserFile with the
;;    htpasswd utility.

;;; Code

;; #### This doesn't seem to work in place.
(unless (require 'neon "neon" 'no-error)
  (require 'neon "neon/neon"))

;; This file could probably live in site-packages?
;; #### There needs to be a general way to deal with site-specific tests
;; like this.
;; #### why not simply
;; (require 'neon-test-user (expand-file-name "neon-test-user"))
;; (let ((test-configuration (expand-file-name "../../neon-test-user.el")))
;;   (when (file-readable-p test-configuration)
;;     (load-file test-configuration)))
(require 'neon-test-user)

;; Configuration variables

(defvar neon-test-server "http://somewhere.over.the.rainbow.invalid/"
  "A WebDAV server's root URL.")

(defvar neon-test-path-public "/"
  "A public resource on `neon-test-server'.
Default is \"/\", which should normally suffice.")

(defvar neon-test-path-private "/path/nonexistent/too"
  "A private (DAV) noncollection resource on `neon-test-server'.")

(defvar neon-test-user "gandalf"
  "A user on `neon-test-server'.")

(defvar neon-test-secret "Speak, friend, and enter."
  "`neon-test-user's password on `neon-test-server'.")

;; Default predefined Basic Authorization header.
;; May be overridden in neon-test-user.el, but it's currently unused.
(defvar neon-test-auth-header
  (list "Authorization"
	(concat "Basic "
		(base64-encode-string
		 (concat neon-test-user ":" neon-test-secret))))
  "An HTTP header implementing RFC 2617 basic authentication with user
`neon-test-user' and password `neon-test-secret', like `neon-test-auth-cb'.
Currently unused by the test suite.")

;; Default authorization callback.
;; May be overridden in neon-test-user.el.
(unless (fboundp 'neon-test-auth-cb)
  (defun neon-test-auth-cb (iggy pop)
    "Authenticate as `neon-test-user' with password `neon-test-secret'.
IGGY is a string, the HTTP realm expected to be offered by the server
  \(currently ignored). 
POP is an integer, the current count of previous \(failed) attempts \(we give
  up after 3 failures).
Returns a cons of the values of `neon-test-user' and `neon-test-secret'."
    ;; IGGY is ignored in this sample callback.
    ;; We restrict consecutive failures to 3.  neon will try indefinitely, so
    ;; we must do the restriction.
    (if (>= pop 3)
	pop				; hackish way to abort authentication
      (cons neon-test-user neon-test-secret))))

;; Internal variables

(defvar neon-parses nil
  "Stack of recent webdav-xml parse trees computed.")

;; The workhorse functions

;; #### These aren't quite ready for prime time (neon.el), but they're close.

(defun neon-request-test-buffer-name (session path method reader
				      &optional accepter body auth)
  (format "*%s%s %s%s%s accept %s*"
	  (if (eq reader 'webdav-xml) "parsed " "")
	  (let ((protocol (assoc method neon-http-descriptions)))
	    (if protocol (nth 1 protocol) "unknown protocol"))
	  method
	  (if auth " w/ auth," "")
	  (if body " w/ body," "")
	  (cond ((eq accepter 'accept-always) "always")
		(t "success"))))

(defun neon-request-test (session path method reader
			  &optional accepter body auth)
  "Return buffer with response for SESSION and PATH using METHOD and READER.
SESSION is a session handle.
PATH is a string giving the path to a resource on SESSION's server.
METHOD is a HTTP method (including WebDAV extensions).
READER is either `raw' or `webdav-xml'.  If `raw', the response is inserted
  verbatim in a buffer.  If `webdav-xml, the response is parsed into LISP
  data, the parse is pushed on `neon-parses', and then printed to the buffer.
  `webdav-xml' is not useful for all methods.
Optional ACCEPTER is `accept-always', or `accept-success', defaulting
  to `accept-success'.  It is good practice to specify the default value
  explicitly.
Optional BODY is a string to be send with the request.  Appropriate format
  depends on METHOD.
Optional AUTH is a boolean indicating whether a authentication callback was
  provided for SESSION.  It is used only to construct the buffer name."

  (let* ((bufname (neon-request-test-buffer-name session path method reader
						 accepter body auth))
	 (buf (get-buffer-create bufname)))
    (setq reader (cond ((eq reader 'raw) buf)
		       ((eq reader 'webdav-xml) 'webdav-xml)
		       (t (error 'wrong-type-argument
				 "reader symbol in test"
				 reader))))
    (condition-case info
	(save-excursion
	  (set-buffer buf)
	  (erase-buffer)
	  (neon-request-create              session method path)
	  (neon-add-response-body-reader    session reader accepter)
	  (when body
	    (neon-set-request-body-buffer   session body))
	  (let ((response (neon-request-dispatch session)))
	    ;; when the reader is a buffer, it contains the response
	    (when (eq reader 'webdav-xml)
	      (push response neon-parses)
	      (insert (format "%S" response))))
	  buf)				; normal function return value
      (error
       (warn "neon request %s\nwith conditions: %s\n%s"
	     bufname
	     (car info)
	     (cdr info))))))		; returns nil

;; The test requests

(let ((mh (neon-make-session-handle neon-test-server)))
  (save-excursion
    (set-buffer
     (neon-request-test mh neon-test-path-public "OPTIONS"
			'raw 'accept-always nil nil))
    (goto-char (point-max))
    (let ((headers (plist-get (object-plist mh) 'last-response-headers)))
      (while headers
	(insert (format "%S %S\n" (car headers) (cadr headers)))
	(setq headers (cddr headers)))))
  (neon-request-test mh neon-test-path-public "HEAD"
		     'raw        'accept-always nil nil)
  (neon-request-test mh neon-test-path-public "GET"
		     'raw        'accept-always nil nil)

  ;; set authentication for the WebDAV tests
  (neon-session-set-auth mh #'neon-test-auth-cb nil)

  (neon-request-test mh neon-test-path-private "PROPFIND"
		     'raw        'accept-always nil nil)
  (neon-request-test mh neon-test-path-private "PROPFIND"
		     'raw        'accept-always webdav-sourceprop-xml nil)
  (neon-request-test mh neon-test-path-private "PROPFIND"
		     'webdav-xml 'accept-always webdav-allprop-xml    nil)
  (neon-request-test mh neon-test-path-private "PROPFIND"
		     'webdav-xml 'accept-always webdav-sourceprop-xml nil)
  (neon-request-test mh neon-test-path-private "PROPFIND"
		     'webdav-xml 'accept-2xx    webdav-allprop-xml    nil))

;; For easy access to the various results
(list-buffers)

;;; LISP emulation of the algorithms

;; The argument lists are incomplete.

;; All of the callbacks take an integer argument called "state" which
;; libneon can use to track the state of the parse.  We don't use it since
;; we compute the whole tree anyway.  It is omitted from the arguments
;; because it occurs before required arguments in the start callback.  If
;; we find a use it can be included, but I recommend it be called "parent".

;; What is called "state" here is called "userdata" by libneon.

;; The cdata callback also takes a parameter "len", which is unnecessary
;; in LISP.

;; The end callback also takes parameters "nspace" and "name", which can be
;; used for consistency checks or to use the same callback for a subset of
;; defined elements.  We only have the one, so this is eliminated.

;; The libneon conventions for returns are annoyingly inconsistent.  The
;; start callback may return a negative integer to abort the parse, 0 to
;; decline the element (which in libneon causes another handler to be
;; activated), or a non-root state (positive integer).  The cdata and end
;; callbacks may return non-zero to abort the parse or zero to accept the
;; content.  We simply return success (1 for the start callback, 0 for the
;; others).

(defvar state nil "State for neon WebDEV parser emulator.")

(defun start-cb (state ns nm ap)
  (let ((current (aref state 1)))
    (setcdr current (cons (list nm ns ap) (cdr current)))
    (setq current (cdr current))
    (let ((tail (cdr (cdr (car current)))))
      (setcdr tail current)
      (aset state 1 tail)))
  1)

(defun cdata-cb (state cdata)
  (let ((current (aref state 1)))
    (setcdr current (cons cdata (cdr current)))
    (aset state 1 (cdr current)))
  0)

(defun end-cb (state)
  (let ((current (aref state 1)))
    (aset state 1 (cdr current))
    (setcdr current nil))
  0)

(defun init-emulator ()
  (let ((header (cons nil nil)))
    (setq state (make-vector 2 header))))

;;; end neon-test.el
