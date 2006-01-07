;;; neon-test.el --- ad hoc test suite for the neon_api ELL module.

;; Copyright (C) 2006  Stephen J. Turnbull

;; To configure, edit the file "neon-test-user.el".  A sample implementation
;; is provided as "neon-test-sample-user.el".

;; This file also contains a LISP emulation of the algorithms used in the
;; callbacks in the C code.

(unless (require 'neon-api "neon_api" 'no-error)
  (require 'neon-api "neon/neon_api"))

(require 'neon-test-user (expand-file-name "neon-test-user"))
;(setq test-path "/Blogs/Software/TestWebDAVPage")
;(setq test-path "/Blogs/Software/FrontPage")
;(setq test-path "/toolbar.el-xemacs-19.11")

(defvar neon-parses nil
  "Stack of recent webdav-xml parse trees computed.")

(defconst neon-http-methods '("HEAD" "GET" "PUT" "POST"))

(defconst neon-webdav-methods
  '("DELETE" "MKCOL" "COPY" "MOVE" "PROPFIND" "PROPPATCH" "LOCK"))

;; Sample WebDAV XML PROPFIND requests

(defconst allprop-xml
  (concat "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n"
	  "<D:propfind xmlns:D=\"DAV:\">\n"
	  "  <D:allprop />\n"
	  "</D:propfind>\n")
  "An XML request for all properties on the resource.")

(defconst sourceprop-xml
  (concat "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n"
	  "<D:propfind xmlns:D=\"DAV:\">\n"
	  "  <D:prop>\n"
	  "    <D:source />\n"
	  "  </D:prop>\n"
	  "</D:propfind>\n")
  "An XML request for all properties on the resource.")

;; Useful utilities
;; #### refactor these functions!

(defun neon-parse-coalesce-cdata (parse)
  "Return a copy of PARSE with adjacent cdata terms merged.
PARSE is a LISPy version of a neon WebDAV XML response.
Should be applied before `neon-parse-clean-whitespace'.

libneon doesn't coalesce adjacent cdata into a single cdata (in fact it
seems to always return vertical whitespace as a separate component) from
other whitespace or text.  It also doesn't bother to coalesce across
internal buffer boundaries, so a word may be split in the middle:
  \"[...1018 characters]<el>word</el>\"
will result in \"word\" being split into \"wo\" and \"rd\"."
  (let ((result '()))
    (while parse
      (cond ((listp (car parse))
	     (push (neon-parse-coalesce-cdata (car parse)) result))
	    ((stringp (car parse))
	     (let ((cdata (list (car parse))))
	       (while (stringp (cadr parse))
		 (push (cadr parse) cdata)
		 (setq parse cdr parse))
	       (push (apply #'concat (nreverse cdata)) result)))
	    (t (push (car parse) result)))
      (setq parse (cdr parse)))
    (nreverse result)))

(defun neon-parse-clean-whitespace (parse)
  "Return a copy of PARSE with whitespace-only cdata terms removed from it.
PARSE is a LISPy version of a neon WebDAV XML response."
  (let ((result '()))
    (while parse
      (cond ((listp (car parse))
	     (push (neon-parse-clean-whitespace (car parse)) result))
	    ((and (stringp (car parse))
		  (string-match "^[ \t\n]*$" (car parse)))
	     nil)			; do nothing
	    (t (push (car parse) result)))
      (setq parse (cdr parse)))
    (nreverse result)))

;; The workhorse functions

(defun neon-request-test-buffer-name (session path method reader
				      &optional accepter body auth)
  (format "*%s%s %s%s%s accept %s*"
	  (if (eq reader 'webdav-xml) "parsed " "")
	  (cond ((member method neon-http-methods) "HTTP")
		((member method neon-webdav-methods) "WebDAV")
		(t "unknown protocol"))
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
	    (when (eq reader 'webdav-xml)
	      (push response neon-parses))
	    (insert (format "%S" response)))
	  buf)				; normal function return value
      (error
       (warn "neon request failed in %s\nwith conditions: %s\n%s"
	     bufname
	     (car info)
	     (cdr info))))))		; returns nil

;; The test requests

;; initialize a session
(setq mh (neon-make-session-handle test-server))

;; (defun neon-request-test (session path method reader
;;			     &optional accepter body auth)
(neon-request-test mh test-path "HEAD" 'raw 'accept-always nil nil)

(neon-request-test mh test-path "GET"  'raw 'accept-always nil nil)

;; set authentication for the WebDAV tests
(neon-session-set-auth mh #'blogkami-auth-cb nil)

(neon-request-test mh test-path "PROPFIND"
		   'raw        'accept-always nil            nil)
(neon-request-test mh test-path "PROPFIND"
		   'raw        'accept-always sourceprop-xml nil)
(neon-request-test mh test-path "PROPFIND"
		   'webdav-xml 'accept-always allprop-xml    nil)
(neon-request-test mh test-path "PROPFIND"
		   'webdav-xml 'accept-always sourceprop-xml nil)
(neon-request-test mh test-path "PROPFIND"
		   'webdav-xml 'accept-2xx    allprop-xml    nil)

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
