;;; neon.el --- utilities for use with the neonapi module

;; Copyright (C) 2005  Stephen J. Turnbull  <stephen@xemacs.org>

;; All rights reserved, except as expressly indicated below.

;; This program is not considered part of XEmacs.

;; You may use, copy, modify, and distribute this software under the terms
;; of the GNU General Public License, version 2 or later at your option.

;; Author:		Stephen J. Turnbull <stephen@xemacs.org>
;; Creation-Date:	2005-11-24

(unless (require 'neonapi nil 'no-error)
  (require 'neonapi "neon/neonapi"))

;; Version information from the libneon I built the first version from.
;; #### This should be fixed to get the information at build time.
(put 'neonapi 'libneon-version "0.25.4")
(put 'neonapi 'libneon_version_major 0)
(put 'neonapi 'libneon_version_minor 25)
(put 'neonapi 'libneon_version_patch 4)

;; #### pull generally useful stuff into here from neon-test.el.

(defconst neon-http-descriptions
  '(("OPTIONS"	"HTTP/1.1"	"RFC 2616")
    ("HEAD"	"HTTP/1.1"	"RFC 2616")
    ("GET"	"HTTP/1.1"	"RFC 2616")
    ("POST"	"HTTP/1.1"	"RFC 2616")
    ("PUT"	"HTTP/1.1"	"RFC 2616")
    ("DELETE"	"HTTP/1.1"	"RFC 2616")
    ("TRACE"	"HTTP/1.1"	"RFC 2616")
    ("CONNECT"	"HTTP/1.1"	"RFC 2616")
    ("MKCOL"	"WebDAV"	"RFC 2518")
    ("COPY"	"WebDAV"	"RFC 2518")
    ("MOVE"	"WebDAV"	"RFC 2518")
    ("PROPFIND"	"WebDAV"	"RFC 2518")
    ("PROPPATCH"	"WebDAV"	"RFC 2518")
    ("LOCK"	"WebDAV"	"RFC 2518")
    ("UNLOCK"	"WebDAV"	"RFC 2518"))
  "Descriptions of HTTP methods.
A list of lists of strings.  The first element of each is the name of an HTTP
method.  The second is the common name of the defining standard, and the third
is the normative reference.")

;; obsolete
(defconst neon-http-methods
  '("OPTIONS" "HEAD" "GET" "PUT" "POST" "DELETE" "TRACE"))

;; obsolete
(defconst neon-webdav-methods
  '("MKCOL" "COPY" "MOVE" "PROPFIND" "PROPPATCH" "LOCK" "UNLOCK"))

(defconst webdav-compliance-value-alist
  '(("1"			"RFC 2518 Level 1 suite")
    ("2"			"RFC 2518 Level 2 suite")
    ("version-control"		"RFC 3253")
    ("ordered-collections"	"RFC 3744")
    ("access-control"		"RFC 4316"))
  "Alist mapping DAV header features to conformance references.
Alist elements are lists to support future enhancements.")

;; Common WebDAV XML PROPFIND requests

(defconst webdav-allprop-xml
  (concat "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
	  "<D:propfind xmlns:D=\"DAV:\">\n"
	  " <D:allprop />\n"
	  "</D:propfind>\n")
  "A WebDAV XML request for all properties on the resource.")

(defconst webdav-sourceprop-xml
  (concat "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
	  "<D:propfind xmlns:D=\"DAV:\">\n"
	  " <D:prop>\n"
	  "  <D:source />\n"
	  " </D:prop>\n"
	  "</D:propfind>\n")
  "A WebDAV XML request for the source property on the resource.")

;; Useful utilities
;; #### refactor these functions!

(defun neon-parse-coalesce-cdata (parse)
  "Return a copy of PARSE with adjacent cdata terms merged.
PARSE is a LISPy version of a neon WebDAV XML response.
Should NOT be applied after `neon-parse-clean-whitespace'.

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
		 (setq parse (cdr parse)))
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

(defun neon-request-buffer-name (session path method reader
				 &optional accepter body auth)
  "Return a descriptive buffer name for a neon request.
SESSION is ignored (should be a session handle).
PATH is ignored (should be a string).
METHOD is a string naming an HTTP method (including WebDAV extensions).
READER is a buffer or symbol.  If `webdav-xml', the description includes
  \"parsed\".
Optional ACCEPTER is any object.  If `accept-always' or `accept-success',
  the description says so, otherwise it's a \"custom accepter\".
Optional BODY if non-nil adds \"w/ body\" to the description.
Optional AUTH if non-nil adds \"w/ auth\" to the description.

The ignored arguments are for compatibility with `neon-simple-request', and
may be used in a future version."
  (format "*%s%s %s%s%s accept %s*"
	  (if (eq reader 'webdav-xml) "parsed " "")
	  (let ((protocol (assoc method neon-http-descriptions)))
	    (if protocol (nth 1 protocol) "unknown protocol"))
	  method
	  (if auth " w/ auth," "")
	  (if body " w/ body," "")
	  (cond ((eq accepter 'accept-always)  "always")
		;; #### deprecate this?
		((eq accepter 'accept-2xx)     "success")
		((eq accepter 'accept-success) "success")
		(t "custom accepter"))))

(defun neon-simple-request (session path method reader
			    &optional accepter body auth)
  "Return buffer with response for SESSION and PATH using METHOD and READER.
SESSION is a session handle.
PATH is a string giving the path to a resource on SESSION's server.
METHOD is a HTTP method (including WebDAV extensions).
READER may be an existing buffer object, `raw', or `webdav-xml'.
  If a buffer, the response is inserted verbatim in it.
  If `raw', the response is inserted verbatim in a new buffer, whose name is
  generated with `neon-request-buffer-name'.
  If `webdav-xml, the response is parsed into LISP data and returned.
  `webdav-xml' is useful primarily for some WebDAV methods.
Optional ACCEPTER is `accept-always', or `accept-success', passed directly
  to `neon-add-response-body-reader'.  This will be extended to accept a
  LISP callback.  Default is `accept-always'.
Optional BODY is a string, passed directly to `neon-set-request-body-buffer'.
  Appropriate format depends on METHOD.
Optional AUTH is an authentication callback to be added to SESSION."

  (let* ((accepter (if (null accepter) 'accept-always accepter))
	 (bufname (neon-request-buffer-name session path method reader
					    accepter body auth))
	 (buf (cond ((eq reader 'raw) (get-buffer-create bufname))
		    ((bufferp reader) reader)
		    ((eq reader 'webdav-xml) nil)
		    (t (error 'wrong-type-argument
			      "reader in `neon-simple-request'"
			      reader)))))
    (condition-case info
	(save-excursion
	  (when buf
	    (set-buffer buf)
	    (unless (eq buf reader)
	      (erase-buffer buf)
	      ;; #### try to force sane buffer position; why needed?!?
	      (goto-char (point-max) buf)))
	  (neon-request-create              session method path)
	  (neon-add-response-body-reader    session (or buf reader) accepter)
	  (when body
	    (neon-set-request-body-buffer   session body))
	  ;; #### check this
	  (when auth
	    (neon-session-set-auth          session auth nil))
	  ;; normal function return value
	  (neon-request-dispatch session))
      (error
       (warn "neon request %s failed\nwith conditions: %s\n%s"
	     bufname
	     (car info)
	     (cdr info))))))		; returns nil

(defun neon-server-options (session path &optional body auth)
  "Return WebDAV options for resource given by SESSION and PATH as string.
Optional BODY is a string to post to the server.
Optional AUTH is an authentication callback."
  (set-buffer
   (neon-simple-request session path "OPTIONS" 'raw 'accept-always body auth))
  (goto-char (point-max))
  (let ((here (point))
	(result "")
	(case-fold-search t))
    (insert "\n")
    (let ((response-headers (plist-get (object-plist session)
				       'last-response-headers)))
      (while response-headers
	(insert (format "%S" (car response-headers)))
	(setq response-headers (cdr response-headers))))
    (goto-char here)
    (while (search-forward "^DAV:.*$" nil 'always-move)
      (setq result (concat (match-string 0)
			   (if (< 0 (length result)) "\n" "")
			   result)))
    result))

(provide 'neon)

;;; neon.el ends here
