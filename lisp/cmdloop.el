;;; cmdloop.el --- support functions for the top-level command loop.

;; Copyright (C) 1992-4, 1997 Free Software Foundation, Inc.
;; Copyright (C) 2001, 2002, 2003 Ben Wing.
 
;; Author: Richard Mlynarik
;; Date: 8-Jul-92
;; Maintainer: XEmacs Development Team
;; Keywords: internal, dumped

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the 
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: FSF 19.30. (Some of the stuff below is in FSF's subr.el.)
;;; Some parts synched with FSF 21.2.

;;; Commentary:

;; This file is dumped with XEmacs.

;;; Code:

(defun recursion-depth ()
  "Return the current depth in recursive edits."
  (+ command-loop-level (minibuffer-depth)))

(defun top-level ()
  "Exit all recursive editing levels."
  (interactive)
  (throw 'top-level nil))

(defun exit-recursive-edit ()
  "Exit from the innermost recursive edit or minibuffer."
  (interactive)
  (if (> (recursion-depth) 0)
      (throw 'exit nil))
  (error "No recursive edit is in progress"))

(defun abort-recursive-edit ()
  "Abort the command that requested this recursive edit or minibuffer input."
  (interactive)
  (if (> (recursion-depth) 0)
      (throw 'exit t))
  (error "No recursive edit is in progress"))

;; (defun keyboard-quit ()
;;   "Signal a `quit' condition."
;;   (interactive)
;;  (deactivate-mark)
;;   (signal 'quit nil))

;; moved here from pending-del.
(defun keyboard-quit ()
  "Signal a `quit' condition.
If this character is typed while lisp code is executing, it will be treated
 as an interrupt.
If this character is typed at top-level, this simply beeps.
If `zmacs-regions' is true, and the zmacs region is active in this buffer,
then this key deactivates the region without beeping or signalling."
  (interactive)
  (if (region-active-p)
      ;; pseudo-zmacs compatibility: don't beep if this ^G is simply
      ;; deactivating the region.  If it is inactive, beep.
      nil
    (signal 'quit nil)))

(defvar buffer-quit-function nil
  "Function to call to \"quit\" the current buffer, or nil if none.
\\[keyboard-escape-quit] calls this function when its more local actions
\(such as cancelling a prefix argument, minibuffer or region) do not apply.")

(defun keyboard-escape-quit ()
  "Exit the current \"mode\" (in a generalized sense of the word).
This command can exit an interactive command such as `query-replace',
can clear out a prefix argument or a region,
can get out of the minibuffer or other recursive edit,
cancel the use of the current buffer (for special-purpose buffers),
or go back to just one window (by deleting all but the selected window)."
  (interactive)
  (cond ((eq last-command 'mode-exited) nil)
	((> (minibuffer-depth) 0)
	 (abort-recursive-edit))
	(current-prefix-arg
	 nil)
	((region-active-p)
	 (zmacs-deactivate-region))
	((> (recursion-depth) 0)
	 (exit-recursive-edit))
	(buffer-quit-function
	 (funcall buffer-quit-function))
	((not (one-window-p t))
	 (delete-other-windows))
	((string-match "^ \\*" (buffer-name (current-buffer)))
	 (bury-buffer))))

;; `cancel-mode-internal' is a function of a misc-user event, which is
;; queued when window system directs XEmacs frame to cancel any modal
;; behavior it exposes, like mouse pointer grabbing.
;;
;; This function does nothing at the top level, but the code which
;; runs modal event loops, such as selection drag loop in `mouse-track',
;; check if misc-user function symbol is `cancel-mode-internal', and
;; takes necessary cleanup actions.
(defun cancel-mode-internal (object)
  (setq zmacs-region-stays t))

;; Someone wrote: "This should really be a ring of last errors."
;;
;; But why bother?  This stuff is not all that necessary now that we
;; have message log, anyway.
(defvar last-error nil
  "Object describing the last signaled error.")

(defcustom errors-deactivate-region nil
  "*Non-nil means that errors will cause the region to be deactivated."
  :type 'boolean
  :group 'editing-basics)

(defun command-error (error-object)
  ;; if you want a backtrace before exiting, set stack-trace-on-error.
  (let* ((inhibit-quit t)
	 (debug-on-error nil)
	 (etype (car-safe error-object)))
    (setq quit-flag nil)
    (setq standard-output t)
    (setq standard-input t)
    (setq executing-kbd-macro nil)
    (and errors-deactivate-region
	 (zmacs-deactivate-region))
    (discard-input)

    (setq last-error error-object)

    (message nil)
    (ding nil (cond ((eq etype 'undefined-keystroke-sequence)
		     (if (and (vectorp (nth 1 error-object))
			      (/= 0 (length (nth 1 error-object)))
			      (button-event-p (aref (nth 1 error-object) 0)))
			 'undefined-click
		       'undefined-key))
		    ((eq etype 'quit)
		     'quit)
		    ((memq etype '(end-of-buffer beginning-of-buffer))
		     'buffer-bound)
		    ((eq etype 'buffer-read-only)
		     'read-only)
		    (t 'command-error)))
    (display-error error-object t)

    (if (noninteractive)
        (progn
          (message "\n%s exiting.\n" emacs-program-name)
          (kill-emacs -1)))
    t))

(defun describe-last-error ()
  "Redisplay the last error-message.  See the variable `last-error'."
  (interactive)
  (if last-error
      (with-displaying-help-buffer
       (lambda ()
	 (princ "Last error was:\n" standard-output)
	 (display-error last-error standard-output)))
    (message "No error yet")))


;;#### Must be done later in the loadup sequence
;(define-key (symbol-function 'help-command) "e" 'describe-last-error)


(defun truncate-command-history-for-gc ()
  (let ((tail (nthcdr 30 command-history)))
    (if tail (setcdr tail nil)))
  (let ((tail (nthcdr 30 values)))
    (if tail (setcdr tail nil)))
  )

(add-hook 'pre-gc-hook 'truncate-command-history-for-gc)


;;;; Object-oriented programming at its finest

;; Now in src/print.c; used by Ferror_message_string and others
;(defun display-error (error-object stream) ;(defgeneric report-condition ...)
;  "Display `error-object' on `stream' in a user-friendly way."
;  (funcall (or (let ((type (car-safe error-object)))
;                 (catch 'error
;                   (and (consp error-object)
;                        (symbolp type)
;                        ;;(stringp (get type 'error-message))
;			(consp (get type 'error-conditions))
;                        (let ((tail (cdr error-object)))
;                          (while (not (null tail))
;                            (if (consp tail)
;                                (setq tail (cdr tail))
;                                (throw 'error nil)))
;                          t)
;                        ;; (check-type condition condition)
;                        (get type 'error-conditions)
;                        ;; Search class hierarchy
;                        (let ((tail (get type 'error-conditions)))
;                          (while (not (null tail))
;                            (cond ((not (and (consp tail)
;                                             (symbolp (car tail))))
;                                   (throw 'error nil))
;                                  ((get (car tail) 'display-error)
;                                   (throw 'error (get (car tail)
;                                                      'display-error)))
;                                  (t
;                                   (setq tail (cdr tail)))))
;                          ;; Default method
;                          #'(lambda (error-object stream)
;                              (let ((type (car error-object))
;                                    (tail (cdr error-object))
;                                    (first t)
;				    (print-message-label 'error))
;                                (if (eq type 'error)
;                                    (progn (princ (car tail) stream)
;                                           (setq tail (cdr tail)))
;				  (princ (or (gettext (get type 'error-message)) type)
;					 stream))
;                                (while tail
;                                  (princ (if first ": " ", ") stream)
;                                  (prin1 (car tail) stream)
;                                  (setq tail (cdr tail)
;                                        first nil))))))))
;	       #'(lambda (error-object stream)
;                   (princ (gettext "Peculiar error ") stream)
;                   (prin1 error-object stream)))
;           error-object stream))

(put 'file-error 'display-error
     #'(lambda (error-object stream)
	 (let ((type (car error-object))
	       (tail (cdr error-object))
	       (first t)
	       (print-message-label 'error))
	   (if (eq type 'file-error)
	       (progn (princ (car tail) stream)
		      (setq tail (cdr tail)))
	     (princ (or (gettext (get type 'error-message)) type)
		    stream))
	   (while tail
	     (princ (if first ": " ", ") stream)
	     (prin1 (car tail) stream)
	     (setq tail (cdr tail)
		   first nil)))))

(put 'undefined-keystroke-sequence 'display-error
     #'(lambda (error-object stream)
         (princ (key-description (car (cdr error-object))) stream)
	 ;; #### I18N3: doesn't localize properly.
         (princ (gettext " not defined.") stream) ; doo dah, doo dah.
         ))


(defcustom teach-extended-commands-p t
  "*If true, then `\\[execute-extended-command]' will teach you keybindings.
Any time you execute a command with \\[execute-extended-command] which has a
shorter keybinding, you will be shown the alternate binding before the
command executes.  There is a short pause after displaying the binding,
before executing it; the length can be controlled by
`teach-extended-commands-timeout'."
  :type 'boolean
  :group 'keyboard)

(defcustom teach-extended-commands-timeout 4
  "*How long to pause after displaying a keybinding before executing.
The value is measured in seconds.  This only applies if
`teach-extended-commands-p' is true."
  :type 'number
  :group 'keyboard)

;That damn RMS went off and implemented something differently, after
;we had already implemented it.  We can't support both properly until
;we have Lisp magic variables.
;(defvar suggest-key-bindings t
;  "*FSFmacs equivalent of `teach-extended-commands-*'.
;Provided for compatibility only.
;Non-nil means show the equivalent key-binding when M-x command has one.
;The value can be a length of time to show the message for.
;If the value is non-nil and not a number, we wait 2 seconds.")
;
;(make-obsolete-variable 'suggest-key-bindings 'teach-extended-commands-p)

(defun execute-extended-command (prefix-arg)
  "Read a command name from the minibuffer using 'completing-read'.
Then call the specified command using 'command-execute' and return its
return value.  If the command asks for a prefix argument, supply the
value of the current raw prefix argument, or the value of PREFIX-ARG
when called from Lisp."
  (interactive "P")
  ;; Note:  This doesn't hack "this-command-keys"
  (let ((prefix-arg prefix-arg))
    (setq this-command (read-command
                        ;; Note: this has the hard-wired
                        ;;  "C-u" and "M-x" string bug in common
                        ;;  with all Emacs's.
			;; (i.e. it prints C-u and M-x regardless of
			;; whether some other keys were actually bound
			;; to `execute-extended-command' and 
			;; `universal-argument'.
                        (cond ((eq prefix-arg '-)
                               "- M-x ")
                              ((equal prefix-arg '(4))
                               "C-u M-x ")
                              ((integerp prefix-arg)
                               (format "%d M-x " prefix-arg))
                              ((and (consp prefix-arg)
                                    (integerp (car prefix-arg)))
                               (format "%d M-x " (car prefix-arg)))
                              (t
                               "M-x ")))))

  (if (and teach-extended-commands-p
	   (interactive-p))
      ;; Remember the keys, run the command, and show the keys (if
      ;; any).  The funny variable names are a poor man's guarantee
      ;; that we don't get tripped by this-command doing something
      ;; funny.  Quoth our forefathers: "We want lexical scope!"
      (let ((_execute_command_keys_ (where-is-internal this-command))
	    (_execute_command_name_ this-command)) ; the name can change
	(command-execute this-command t)
	(when _execute_command_keys_
	  ;; Normally the region is adjusted in post_command_hook;
	  ;; however, it is not called until after we finish.  It
	  ;; looks ugly for the region to get updated after the
	  ;; delays, so we do it now.  The code below is a Lispified
	  ;; copy of code in event-stream.c:post_command_hook().
	  (if (and (not zmacs-region-stays)
		   (or (not (eq (selected-window) (minibuffer-window)))
		       (eq (zmacs-region-buffer) (current-buffer))))
	      (zmacs-deactivate-region)
	    (zmacs-update-region))
	  ;; Wait for a while, so the user can see a message printed,
	  ;; if any.
	  (when (sit-for 1)
	    (display-message
		'no-log
	      (format (if (cdr _execute_command_keys_)
			  "Command `%s' is bound to keys: %s"
			"Command `%s' is bound to key: %s")
		      _execute_command_name_
		      (sorted-key-descriptions _execute_command_keys_)))
	    (sit-for teach-extended-commands-timeout)
	    (clear-message 'no-log))))
    ;; Else, just run the command.
    (command-execute this-command t)))


;;; C code calls this; the underscores in the variable names are to avoid
;;; cluttering the specbind namespace (lexical scope!  lexical scope!)
;;; Putting this in Lisp instead of C slows kbd macros by 50%.
;(defun command-execute (_command &optional _record-flag)
;  "Execute CMD as an editor command.
;CMD must be a symbol that satisfies the `commandp' predicate.
;Optional second arg RECORD-FLAG non-nil
;means unconditionally put this command in `command-history'.
;Otherwise, that is done only if an arg is read using the minibuffer."
;  (let ((_prefix prefix-arg)
;        (_cmd (indirect-function _command)))
;    (setq prefix-arg nil
;          this-command _command
;          current-prefix-arg _prefix
;          zmacs-region-stays nil)
;    ;; #### debug_on_next_call = 0;
;    (cond ((and (symbolp _command)
;                (get _command 'disabled))
;           (run-hooks disabled-command-hook))
;          ((or (stringp _cmd) (vectorp _cmd))
;           ;; If requested, place the macro in the command history.  
;           ;;  For other sorts of commands, call-interactively takes
;           ;;  care of this. 
;           (if _record-flag
;               (setq command-history
;                     (cons (list 'execute-kbd-macro _cmd _prefix)
;                           command-history)))
;             (execute-kbd-macro _cmd _prefix))
;            (t
;             (call-interactively _command _record-flag)))))

(defun y-or-n-p-minibuf (prompt)
  "Ask user a \"y or n\" question.  Return t if answer is \"y\".
Takes one argument, which is the string to display to ask the question.
It should end in a space; `y-or-n-p' adds `(y or n) ' to it.
No confirmation of the answer is requested; a single character is enough.
Also accepts Space to mean yes, or Delete to mean no."
  (save-excursion
    (let* ((pre "")
           (yn (gettext "(y or n) "))
	   ;; we need to translate the prompt ourselves because of the
	   ;; strange way we handle it.
	   (prompt (gettext prompt))
           event)
      (while (stringp yn)
        (if (let ((cursor-in-echo-area t)
                  (inhibit-quit t))
              (message "%s%s%s" pre prompt yn)
              (setq event (next-command-event event))
	      (condition-case nil
		  (prog1
		      (or quit-flag (eq 'keyboard-quit (key-binding event)))
		    (setq quit-flag nil))
		(wrong-type-argument t)))
            (progn
              (message "%s%s%s%s" pre prompt yn (single-key-description event))
              (setq quit-flag nil)
              (signal 'quit '())))
        (let* ((keys (events-to-keys (vector event)))
	       (def (lookup-key query-replace-map keys)))
          (cond ((eq def 'skip)
                 (message "%s%sNo" prompt yn)
		 (setq yn nil))
                ((eq def 'act)
                 (message "%s%sYes" prompt yn)
		 (setq yn t))
		((eq def 'recenter)
		 (recenter))
		((or (eq def 'quit) (eq def 'exit-prefix))
		 (signal 'quit '()))
                ((button-release-event-p event) ; ignore them
                 nil)
                (t
                 (message "%s%s%s%s" pre prompt yn
                          (single-key-description event))
                 (ding nil 'y-or-n-p)
                 (discard-input)
                 (if (= (length pre) 0)
                     (setq pre (gettext "Please answer y or n.  ")))))))
      yn)))

(defun yes-or-no-p-minibuf (prompt)
  "Ask user a yes-or-no question.  Return t if answer is yes.
Takes one argument, which is the string to display to ask the question.
It should end in a space; `yes-or-no-p' adds `(yes or no) ' to it.
The user must confirm the answer with RET,
and can edit it until it has been confirmed."
  (save-excursion
    (let ((p (concat (gettext prompt) (gettext "(yes or no) ")))
          (ans ""))
      (while (stringp ans)
        (setq ans (downcase (read-string p nil t))) ;no history
        (cond ((string-equal ans (gettext "yes"))
               (setq ans t))
              ((string-equal ans (gettext "no"))
               (setq ans nil))
              (t
               (ding nil 'yes-or-no-p)
               (discard-input)
               (message "Please answer yes or no.")
               (sleep-for 2))))
      ans)))

(defun yes-or-no-p (prompt)
  "Ask user a yes-or-no question.  Return t if answer is yes.
The question is asked with a dialog box or the minibuffer, as appropriate.
Takes one argument, which is the string to display to ask the question.
It should end in a space; `yes-or-no-p' adds `(yes or no) ' to it.
The user must confirm the answer with RET,
and can edit it until it as been confirmed."
  (if (should-use-dialog-box-p)
      (yes-or-no-p-dialog-box prompt)
    (yes-or-no-p-minibuf prompt)))

(defun y-or-n-p (prompt)
  "Ask user a \"y or n\" question.  Return t if answer is \"y\".
Takes one argument, which is the string to display to ask the question.
The question is asked with a dialog box or the minibuffer, as appropriate.
It should end in a space; `y-or-n-p' adds `(y or n) ' to it.
No confirmation of the answer is requested; a single character is enough.
Also accepts Space to mean yes, or Delete to mean no."
  (if (should-use-dialog-box-p)
      (yes-or-no-p-dialog-box prompt)
    (y-or-n-p-minibuf prompt)))



(defun read-char ()
  "Read a character from the command input (keyboard or macro).
If a mouse click or non-ASCII character is detected, an error is
signalled.  The character typed is returned as an ASCII value.  This
is most likely the wrong thing for you to be using: consider using
the `next-command-event' function instead."
  (save-excursion
    (let ((event (next-command-event)))
      (or inhibit-quit
	  (and (event-matches-key-specifier-p event (quit-char))
	       (signal 'quit nil)))
      (prog1 (or (event-to-character event)
                 ;; Kludge.  If the event we read was a mouse-release,
                 ;; discard it and read the next one.
                 (if (button-release-event-p event)
                     (event-to-character (next-command-event event)))
                 (error "Key read has no ASCII equivalent %S" event))
        ;; this is not necessary, but is marginally more efficient than GC.
        (deallocate-event event)))))

(defun read-char-exclusive ()
  "Read a character from the command input (keyboard or macro).
If a mouse click or non-ASCII character is detected, it is discarded.
The character typed is returned as an ASCII value.  This is most likely
the wrong thing for you to be using: consider using the
`next-command-event' function instead."
  (let (event ch)
    (while (progn
	     (setq event (next-command-event))
	     (or inhibit-quit
		 (and (event-matches-key-specifier-p event (quit-char))
		      (signal 'quit nil)))
	     (setq ch (event-to-character event))
	     (deallocate-event event)
	     (null ch)))
    ch))

;;;; Input and display facilities.

;; BEGIN SYNCHED WITH FSF 21.2.

(defvar read-quoted-char-radix 8
  "*Radix for \\[quoted-insert] and other uses of `read-quoted-char'.
Legitimate radix values are 8, 10 and 16.")

(custom-declare-variable-early
 'read-quoted-char-radix 8 
 "*Radix for \\[quoted-insert] and other uses of `read-quoted-char'.
Legitimate radix values are 8, 10 and 16."
  :type '(choice (const 8) (const 10) (const 16))
  :group 'editing-basics)

(defun read-quoted-char (&optional prompt)
  "Like `read-char', but do not allow quitting.
Also, if the first character read is an octal digit,
we read any number of octal digits and return the
specified character code.  Any nondigit terminates the sequence.
If the terminator is RET, it is discarded;
any other terminator is used itself as input.

The optional argument PROMPT specifies a string to use to prompt the user.
The variable `read-quoted-char-radix' controls which radix to use
for numeric input."
  (let (;(message-log-max nil)
	done (first t) (code 0) char event
	(prompt (and prompt (gettext prompt)))
	)
    (while (not done)
      (let ((inhibit-quit first)
	    ;; Don't let C-h get the help message--only help function keys.
	    (help-char nil)
	    (help-form
	     "Type the special character you want to use,
or the octal character code.
RET terminates the character code and is discarded;
any other non-digit terminates the character code and is then used as input."))
	(and prompt (display-message 'prompt (format "%s-" prompt)))
	(setq event (next-command-event)
	      char (or (event-to-character event)
		       (signal 'error
			       (list "key read cannot be inserted in a buffer"
				     event))))
	(if inhibit-quit (setq quit-flag nil)))
      ;; Translate TAB key into control-I ASCII character, and so on.
      (and char
	   (let ((translated (lookup-key function-key-map (vector char))))
	     (if (arrayp translated)
		 (setq char (aref translated 0)))))
      (cond ((null char))
	    ((not (characterp char))
	     (setq unread-command-events (list char)
		   done t))
; 	    ((/= (logand char ?\M-\^@) 0)
; 	     ;; Turn a meta-character into a character with the 0200 bit set.
; 	     (setq code (logior (logand char (lognot ?\M-\^@)) 128)
; 		   done t))
	    ((and (<= ?0 char) (< char (+ ?0 (min 10 read-quoted-char-radix))))
	     (setq code (+ (* code read-quoted-char-radix) (- char ?0)))
	     (and prompt (setq prompt (display-message 'prompt
					(format "%s %c" prompt char)))))
	    ((and (<= ?a (downcase char))
		  (< (downcase char) (+ ?a -10 (min 26 read-quoted-char-radix))))
	     (setq code (+ (* code read-quoted-char-radix)
			   (+ 10 (- (downcase char) ?a))))
	     (and prompt (setq prompt (display-message 'prompt
					(format "%s %c" prompt char)))))
	    ((and (not first) (eq char ?\C-m))
	     (setq done t))
	    ((not first)
	     (setq unread-command-events (list char)
		   done t))
	    (t (setq code (char-to-int char)
		     done t)))
      (setq first nil))
    (int-to-char code)))

;; in passwd.el.
; (defun read-passwd (prompt &optional confirm default)
;   "Read a password, prompting with PROMPT.  Echo `.' for each character typed.
; End with RET, LFD, or ESC.  DEL or C-h rubs out.  C-u kills line.
; Optional argument CONFIRM, if non-nil, then read it twice to make sure.
; Optional DEFAULT is a default password to use instead of empty input."
;   (if confirm
;       (let (success)
; 	(while (not success)
; 	  (let ((first (read-passwd prompt nil default))
; 		(second (read-passwd "Confirm password: " nil default)))
; 	    (if (equal first second)
; 		(progn
; 		  (and (arrayp second) (fillarray second ?\0))
; 		  (setq success first))
; 	      (and (arrayp first) (fillarray first ?\0))
; 	      (and (arrayp second) (fillarray second ?\0))
; 	      (message "Password not repeated accurately; please start over")
; 	      (sit-for 1))))
; 	success)
;     (let ((pass nil)
; 	  (c 0)
; 	  (echo-keystrokes 0)
; 	  (cursor-in-echo-area t))
;       (while (progn (message "%s%s"
; 			     prompt
; 			     (make-string (length pass) ?.))
; 		    (setq c (read-char-exclusive nil t))
; 		    (and (/= c ?\r) (/= c ?\n) (/= c ?\e)))
; 	(clear-this-command-keys)
; 	(if (= c ?\C-u)
; 	    (progn
; 	      (and (arrayp pass) (fillarray pass ?\0))
; 	      (setq pass ""))
; 	  (if (and (/= c ?\b) (/= c ?\177))
; 	      (let* ((new-char (char-to-string c))
; 		     (new-pass (concat pass new-char)))
; 		(and (arrayp pass) (fillarray pass ?\0))
; 		(fillarray new-char ?\0)
; 		(setq c ?\0)
; 		(setq pass new-pass))
; 	    (if (> (length pass) 0)
; 		(let ((new-pass (substring pass 0 -1)))
; 		  (and (arrayp pass) (fillarray pass ?\0))
; 		  (setq pass new-pass))))))
;       (message nil)
;       (or pass default ""))))

;; aliased to redraw-modeline, a built-in.
; (defun force-mode-line-update (&optional all)
;   "Force the mode-line of the current buffer to be redisplayed.
; With optional non-nil ALL, force redisplay of all mode-lines."
;   (if all (save-excursion (set-buffer (other-buffer))))
;   (set-buffer-modified-p (buffer-modified-p)))

(defun momentary-string-display (string pos &optional exit-char message) 
  "Momentarily display STRING in the buffer at POS.
Display remains until next character is typed.
If the char is EXIT-CHAR (optional third arg, default is SPC) it is swallowed;
otherwise it is then available as input (as a command if nothing else).
Display MESSAGE (optional fourth arg) in the echo area.
If MESSAGE is nil, instructions to type EXIT-CHAR are displayed there."
  (or exit-char (setq exit-char ?\ ))
  (let ((inhibit-read-only t)
	;; Don't modify the undo list at all.
	(buffer-undo-list t)
	(modified (buffer-modified-p))
	(name buffer-file-name)
	insert-end)
    (unwind-protect
	(progn
	  (save-excursion
	    (goto-char pos)
	    ;; defeat file locking... don't try this at home, kids!
	    (setq buffer-file-name nil)
	    (insert-before-markers (gettext string))
	    (setq insert-end (point))
	    ;; If the message end is off screen, recenter now.
	    (if (< (window-end nil t) insert-end)
		(recenter (/ (window-height) 2)))
	    ;; If that pushed message start off the frame,
	    ;; scroll to start it at the top of the frame.
	    (move-to-window-line 0)
	    (if (> (point) pos)
		(progn
		  (goto-char pos)
		  (recenter 0))))
	  (message (or message (gettext "Type %s to continue editing."))
		   (single-key-description exit-char))
	  (let ((event (save-excursion (next-command-event))))
	    (or (eq (event-to-character event) exit-char)
		(setq unread-command-events (list event)))))
      (if insert-end
	  (save-excursion
	    (delete-region pos insert-end)))
      (setq buffer-file-name name)
      (set-buffer-modified-p modified))))

;; END SYNCHED WITH FSF 21.2.

;;; cmdloop.el ends here
