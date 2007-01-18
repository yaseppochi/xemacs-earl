;;; earl-utils.el --- utilities for managing web resources

;; Copyright (C) 2007  Stephen J. Turnbull

;; Author:		Stephen J. Turnbull <stephen@xemacs.org>
;; Creation-Date:	18 January 2007
;; Keywords:		user, comm, www

;;; Commentary:

;; cadaver commands for consideration for EARL

;;  about
;;  cat		earl-find-resource, earl-insert-resource
;;  cd
;;  checkin
;;  checkout
;;  chexec
;;  close
;;  copy
;;  delete
;;  describe
;;  discover
;;  echo
;;  edit
;;  get		earl-fetch-resource
;;  help
;;  history
;;  label
;;  lcd
;;  less	earl-find-resource, earl-insert-resource
;;  lls
;;  lock
;;  logout
;;  lpwd
;;  ls
;;  mget
;;  mkcol
;;  move
;;  mput
;;  open	earl-find-resource
;;  propdel
;;  propget
;;  propnames
;;  propset
;;  put
;;  pwd
;;  quit
;;  rmcol
;;  search
;;  set
;;  showlocks
;;  steal
;;  uncheckout
;;  unlock
;;  unset
;;  version

;;; Change Log:

;;	* Use `git log'.

;;; Code:

;; EARL utilities -- C-c u

(defvar earl-map (make-sparse-keymap "EARL keymap")
  "Sparse keymap for binding EARL functions.")

(unless earl-map
  (define-key earl-map [f] #'earl-find-resource)
  )

;; earl-find-resource
;;
;; Download a resource into a buffer and display it.
;; Downloading is done by a configurable backend.  In early versions the
;; backend will be used for any URL, and will signal an error if it is not
;; competent to handle the URL.  Later, various heuristics will be used to
;; determine an appropriate backend.
;; Potential backends include `neon', `curl', `url.el', `find-file', `efs',
;; and `tramp' at least.
;; Note that the terminology used here is somewhat different from typical
;; browsers.  The document as received from the server is the *raw document*,
;; while the *source document* is the resource as stored on the server,
;; untransformed.  It's not obvious how to define the source document where
;; discovery of the source is not supported (basically, non-DAV servers,
;; although some frameworks, like Zope, may provide platform-dependent
;; means like a different TCP port).

(defun* earl-find-resource (url &optional prefix
			    &key fetch-source (formatter #'ignore)
			    (backend 'neon) readonly format-in-place)
  "Access URL, and display the document returned in the current window.
URL must be a valid URL, understood by the backend.
Optional PREFIX currently has no semantics.
KEYS contains a plist of keyword arguments.  Currently no keys are implemented,
and no support is implemented for interactive entry of keyword arguments.
Planned keys include:
:backend The backend used to access the URL and transport the reponse.
  The currently available backend is `neon'.
:fetch-source Fetch the document source (e.g., using DAV).  Default is nil.
:formatter A function which reformats the document after insertion in the
  buffer.  The formatter function takes the buffer, the beginning of the
  inserted text, and the end of the inserted text as arguments.
  The currently available formatter is `ignore' (i.e., raw).
:format-in-place Format the document in place, possibly destroying the raw
  form.  When nil, the raw document is preserved in a buffer with the same
  name as the current buffer, with \"RAW:\" prepended to it.  Defaults to nil.
  (The default is nil to ease debugging, and is subject to change with
  experience if resource consumption seems to be a problem.)
:readonly Set the inserted contents to read-only.  Defaults to nil.
Cf. `earl-find-resource-other-window' and `earl-find-resource-other-frame'."
  (interactive "sURL to access: \nP")
  ;; #### should do a full parse here
  ;; #### need to deal with characters illegal in an URL
  (string-match "^\\(?:.*/\\)\\([^/]*\\)$" url)
  ;; #### need to deal with edge cases: empty URL, collection URL, URN
  ;; #### need to deal with buffer issues: non-empty, can't create, read-only
  (let ((buf (get-buffer-create (match-string 1))))
    (condition-case data
	(earl-insert-resource url buf :backend 'neon)
      (error (error (car data) (cdr data))))  ; #### just resignal
    (switch-to-buffer buf))
  )

(defun* earl-insert-resource (url &optional prefix
			      &key (backend 'neon) (formatter #'ignore))
  "Access URL, and insert the formatted document returned at point.
URL must be a valid URL, understood by the backend.
Optional PREFIX currently has no semantics.
KEYS contains a plist of keyword arguments.  Currently no keys are supported,
and no support is provided for interactive entry of keyword arguments.
Planned keys include:
:backend A symbol naming the backend to use for access and transport.
  The currently available backend is `neon'.
:formatter A function which reformats the document after insertion in the
  buffer.  The formatter function takes the buffer, the beginning of the
  inserted text, and the end of the inserted text as arguments.
  The currently available formatter is `ignore' (i.e., raw)."
  (earl-fetch-resource url (current-buffer) :backend backend)
  )

;; #### This corresponds approximately to insert-file-contents-literally
;; except that it provides for specifying a buffer to hold the raw data.
;; Should the name reflect that?  Probably not ....

(defun* earl-fetch-resource (url &optional buffer &key (backend 'neon))
  "Access URL, and insert the raw document returned in BUFFER at point.
URL must be a valid URL, understood by the backend.
BUFFER must be an existing buffer or the name of an existing buffer.
It defaults to the current buffer.
Currently no keys are implemented.
Planned keys include:
:backend The backend used to access the URL and transport the reponse.
  The currently available backend is `neon'."
  (set-buffer (or buffer (current-buffer)))
  (insert-file-contents url)
  )

(provide 'earl-utils)

;;; earl-utils.el ends here
