;;; tvi970.el --- terminal support for the Televideo 970

;; Author: Jim Blandy <jimb@occs.cs.oberlin.edu>, January 1992
;; Keywords: terminals

;; Copyright (C) 1992 Free Software Foundation, Inc.

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
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;;; Uses the Emacs 19 terminal initialization features --- won't work with 18.

;;; Code:

(or (lookup-key function-key-map "\e[")
    (define-key function-key-map "\e[" (make-keymap)))
;; (or (lookup-key function-key-map "\eO")
;;    (define-key function-key-map "\eO" (make-keymap)))

;; Miscellaneous keys
(mapcar (function (lambda (key-binding)
		    (define-key function-key-map
		      (car key-binding) (nth 1 key-binding))))
	'(
	  ;; These are set up by termcap or terminfo
	  ;; ("\eOP"	[kp-f1])
	  ;; ("\eOQ"	[kp-f2])
	  ;; ("\eOR"	[kp-f3])
	  ;; ("\eOS"	[kp-f4])

	  ;; These might be set by terminfo
	  ("\e[H"	[home])
	  ("\e[Z"	[backtab])
	  ("\e[i"	[print])
	  ("\e[@"	[insert])
	  ("\e[L"	[insertline])
	  ("\e[M"	[deleteline])
	  ("\e[U"	[next])		;; actually the `page' key

	  ;; These won't be set up by either
	  ("\eOm"	[kp-subtract])
	  ("\eOl"	[kp-separator])
	  ("\eOn"	[kp-decimal])
	  ("\eOM"	[kp-enter])

	  ;; These won't be set up by either
	  ("\e[K"	[key-eol])	;; Not an X keysym
	  ("\e[J"	[key-eos])	;; Not an X keysym
	  ("\e[2J"	[key-clear])	;; Not an X keysym
	  ("\e[P"	[key-dc])	;; Not an X keysym
	  ("\e[g"	[(shift tab)])	;; Not an X keysym
	  ("\e[2N"	[clearentry])	;; Not an X keysym
	  ("\e[2K"	[(shift clearentry)])	;; Not an X keysym
	  ("\e[E"	[?\C-j])	;; Not an X keysym
	  ("\e[g"	[(shift backtab)])	;; Not an X keysym
	  ("\e[?1i"	[key-sprint])	;; Not an X keysym
	  ("\e[4h"	[key-sic])	;; Not an X keysym
	  ("\e[4l"	[(shift delete)])	;; Not an X keysym
	  ("\e[Q"	[(shift insertline)])	;; Not an X keysym
	  ("\e[1Q"	[key-sdl])	;; Not an X keysym
	  ("\e[19l"	[key-seol])	;; Not an X keysym
	  ("\e[19h"	[(shift erasepage)])	;; Not an X keysym
	  ("\e[V"	[(shift page)])	;; Not an X keysym
	  ("\eS"	[send])		;; Not an X keysym
	  ("\e5"	[(shift send)])	;; Not an X keysym
	  ))

;; The numeric keypad keys.
(let ((i 0))
  (while (< i 10)
    (define-key function-key-map
      (format "\eO%c" (+ i ?p))
      (vector (intern (format "kp-%d" i))))
    (setq i (1+ i))))
;; The numbered function keys.
(let ((i 0))
  (while (< i 16)
    (define-key function-key-map
      (format "\e?%c" (+ i ?a))
      (vector (intern (format "f%d" (1+ i)))))
    (define-key function-key-map
      (format "\e?%c" (+ i ?A))
      (vector (list 'shift (intern (format "f%d" (1+ i))))))
    (setq i (1+ i))))


;;; Should keypad numbers send ordinary digits or distinct escape sequences?
(defvar tvi970-keypad-numeric nil
  "The terminal should be in numeric keypad mode iff this variable is non-nil.
Do not set this variable!  Call the function ``tvi970-set-keypad-mode''.")

(defun tvi970-set-keypad-mode (&optional arg)
  "Set the current mode of the TVI 970 numeric keypad.
In ``numeric keypad mode'', the number keys on the keypad act as
ordinary digits.  In ``alternate keypad mode'', the keys send distinct
escape sequences, meaning that they can have their own bindings,
independent of the normal number keys.
With no argument, toggle between the two possible modes.
With a positive argument, select alternate keypad mode.
With a negative argument, select numeric keypad mode."
  (interactive "P")
  (setq tvi970-keypad-numeric 
	(if (null arg)
	    (not tvi970-keypad-numeric)
	  (> (prefix-numeric-value arg) 0)))
  (send-string-to-terminal (if tvi970-keypad-numeric "\e=" "\e>")))

(tvi970-set-keypad-mode 1)

;;; tv970 ends here
