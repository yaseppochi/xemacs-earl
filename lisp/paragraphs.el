;;; paragraphs.el --- paragraph and sentence parsing

;; Copyright (C) 1985, 86, 87, 91, 94, 95, 96, 1997, 1999, 2000, 2001
;;    Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: wp, dumped

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

;;; Synched up with: FSF 21.3.

;;; Commentary:

;; This file is dumped with XEmacs.

;; This package provides the paragraph-oriented commands documented in the
;; XEmacs Reference Manual.

;; 06/11/1997 - Use char-(after|before) instead of
;;  (following|preceding)-char. -slb

;;; Code:

(defgroup paragraphs nil
  "Paragraph and sentence parsing."
  :group 'editing)

(define-minor-mode use-hard-newlines
  "Minor mode to distinguish hard and soft newlines.
When active, the functions `newline' and `open-line' add the
text-property `hard' to newlines that they insert, and a line is
only considered as a candidate to match `paragraph-start' or
`paragraph-separate' if it follows a hard newline.

Prefix argument says to turn mode on if positive, off if negative.
When the mode is turned on, if there are newlines in the buffer but no hard
newlines, ask the user whether to mark as hard any newlines preceeding a
`paragraph-start' line.  From a program, second arg INSERT specifies whether
to do this; it can be `never' to change nothing, t or `always' to force
marking, `guess' to try to do the right thing with no questions, nil
or anything else to ask the user.

Newlines not marked hard are called \"soft\", and are always internal
to paragraphs.  The fill functions insert and delete only soft newlines."
  :group 'paragraphs
  :extra-args (insert)
  (when use-hard-newlines
    ;; Turn mode on
    ;; Intuit hard newlines --
    ;;   mark as hard any newlines preceding a paragraph-start line.
    (if (or (eq insert t) (eq insert 'always)
	    (and (not (eq 'never insert))
		 (not (text-property-any (point-min) (point-max) 'hard t))
		 (save-excursion
		   (goto-char (point-min))
		   (search-forward "\n" nil t))
		 (or (eq insert 'guess)
		     (y-or-n-p "Make newlines between paragraphs hard? "))))
	(save-excursion
	  (goto-char (point-min))
	  (while (search-forward "\n" nil t)
	    (let ((pos (point)))
	      (move-to-left-margin)
	      (when (looking-at paragraph-start)
		(set-hard-newline-properties (1- pos) pos))
	      ;; If paragraph-separate, newline after it is hard too.
	      (when (looking-at paragraph-separate)
		(set-hard-newline-properties (1- pos) pos)
		(end-of-line)
		(unless (eobp)
		  (set-hard-newline-properties (point) (1+ (point)))))))))))

(defcustom paragraph-start "\f\\|[ \t]*$" "\
*Regexp for beginning of a line that starts OR separates paragraphs.
This regexp should match lines that separate paragraphs
and should also match lines that start a paragraph
\(and are part of that paragraph).

This is matched against the text at the left margin, which is not necessarily
the beginning of the line, so it should never use \"^\" as an anchor.  This
ensures that the paragraph functions will work equally well within a region
of text indented by a margin setting.

The variable `paragraph-separate' specifies how to distinguish
lines that start paragraphs from lines that separate them.

If the variable `use-hard-newlines' is non-nil, then only lines following a
hard newline are considered to match."
  :group 'paragraphs
  :type 'regexp)

;; paragraph-start requires a hard newline, but paragraph-separate does not:
;; It is assumed that paragraph-separate is distinctive enough to be believed
;; whenever it occurs, while it is reasonable to set paragraph-start to
;; something very minimal, even including "." (which makes every hard newline
;; start a new paragraph).

(defcustom paragraph-separate "[ \t\f]*$"
  "*Regexp for beginning of a line that separates paragraphs.
If you change this, you may have to change `paragraph-start' also.

A line matching this is not part of any paragraph.

This is matched against the text at the left margin, which is not necessarily
the beginning of the line, so it should not use \"^\" as an anchor.  This
ensures that the paragraph functions will work equally within a region of
text indented by a margin setting."
  :group 'paragraphs
  :type 'regexp)

(defcustom sentence-end-double-space t
  "*Non-nil means a single space does not end a sentence.
This is relevant for filling.  See also `sentence-end-without-period'
and `colon-double-space'.

This variable applies only to filling, not motion commands.  To
change the behavior of motion commands, see `sentence-end'.

If you change this, you should also change `sentence-end'.  See Info
node `Sentences'."
  :type 'boolean
  :group 'fill)

(defcustom sentence-end-without-period nil
  "*Non-nil means a sentence will end without a period.
For example, a sentence in Thai text ends with double space but
without a period."
  :type 'boolean
  :group 'fill)

(defcustom sentence-end
  (purecopy
   ;; This is a bit stupid since it's not auto-updated when the
   ;; other variables are changed, but it's still useful info.
   (concat (if sentence-end-without-period "\\w  \\|")
	   "[.?!"
	   (if (featurep 'mule)
	       (decode-coding-string "\033$B!#!%!)!*\033$A!##.#?#!\033$(0!$!%!)!*\033$(G!$!%!)!*\033(B" 'iso-2022-7bit)
	     "")
	   "][]\"')}]*"
	   (if sentence-end-double-space
	       "\\($\\| $\\|\t\\|  \\)" "\\($\\|[\t ]\\)")
	   "[ \t\n]*"))
  "*Regexp describing the end of a sentence.
The value includes the whitespace following the sentence.
All paragraph boundaries also end sentences, regardless.

The default value specifies that in order to be recognized as the end
of a sentence, the ending period, question mark, or exclamation point
must be followed by two spaces, unless it's inside some sort of quotes
or parenthesis.

See also the variable `sentence-end-double-space', the variable
`sentence-end-without-period' and Info node `Sentences'."
  :group 'paragraphs
  :type 'regexp)

(defcustom page-delimiter "^\014"
  "*Regexp describing line-beginnings that separate pages."
  :group 'paragraphs
  :type 'regexp)

(defcustom paragraph-ignore-fill-prefix nil
  "*Non-nil means the paragraph commands are not affected by `fill-prefix'.
This is desirable in modes where blank lines are the paragraph delimiters."
  :group 'paragraphs
  :type 'boolean)

(defun forward-paragraph (&optional arg)
  "Move forward to end of paragraph.
With argument ARG, do it ARG times;
a negative argument ARG = -N means move backward N paragraphs.

A line which `paragraph-start' matches either separates paragraphs
\(if `paragraph-separate' matches it also) or is the first line of a paragraph.
A paragraph end is the beginning of a line which is not part of the paragraph
to which the end of the previous line belongs, or the end of the buffer.
Returns the count of paragraphs left to move."
  (interactive "_p") ; XEmacs
  (or arg (setq arg 1))
  (let* ((opoint (point))
	 (fill-prefix-regexp
	  (and fill-prefix (not (equal fill-prefix ""))
	       (not paragraph-ignore-fill-prefix)
	       (regexp-quote fill-prefix)))
	 ;; Remove ^ from paragraph-start and paragraph-sep if they are there.
	 ;; These regexps shouldn't be anchored, because we look for them
	 ;; starting at the left-margin.  This allows paragraph commands to
	 ;; work normally with indented text.
	 ;; This hack will not find problem cases like "whatever\\|^something".
	 (parstart (if (and (not (equal "" paragraph-start))
			    (equal ?^ (aref paragraph-start 0)))
		       (substring paragraph-start 1)
		     paragraph-start))
	 (parsep (if (and (not (equal "" paragraph-separate))
			  (equal ?^ (aref paragraph-separate 0)))
		     (substring paragraph-separate 1)
		   paragraph-separate))
	 (parsep
	  (if fill-prefix-regexp
	      (concat parsep "\\|"
		      fill-prefix-regexp "[ \t]*$")
	    parsep))
	 ;; This is used for searching.
	 (sp-parstart (concat "^[ \t]*\\(?:" parstart "\\|" parsep "\\)"))
	 start found-start)
    (while (and (< arg 0) (not (bobp)))
      (if (and (not (looking-at parsep))
	       (re-search-backward "^\n" (max (1- (point)) (point-min)) t)
	       (looking-at parsep))
	  (setq arg (1+ arg))
	(setq start (point))
	;; Move back over paragraph-separating lines.
	(backward-char 1) (beginning-of-line)
	(while (and (not (bobp))
		    (progn (move-to-left-margin)
			   (looking-at parsep)))
	  (forward-line -1))
	(if (bobp)
	    nil
	  (setq arg (1+ arg))
	  ;; Go to end of the previous (non-separating) line.
	  (end-of-line)
	  ;; Search back for line that starts or separates paragraphs.
	  (if (if fill-prefix-regexp
		  ;; There is a fill prefix; it overrides parstart.
		  (let (multiple-lines)
		    (while (and (progn (beginning-of-line) (not (bobp)))
				(progn (move-to-left-margin)
				       (not (looking-at parsep)))
				(looking-at fill-prefix-regexp))
		      (unless (= (point) start)
			(setq multiple-lines t))
		      (forward-line -1))
		    (move-to-left-margin)
		    ;; This deleted code caused a long hanging-indent line
		    ;; not to be filled together with the following lines.
		    ;; ;; Don't move back over a line before the paragraph
		    ;; ;; which doesn't start with fill-prefix
		    ;; ;; unless that is the only line we've moved over.
		    ;; (and (not (looking-at fill-prefix-regexp))
		    ;;      multiple-lines
		    ;;      (forward-line 1))
		    (not (bobp)))
		(while (and (re-search-backward sp-parstart nil 1)
			    (setq found-start t)
			    ;; Found a candidate, but need to check if it is a
			    ;; REAL parstart.
			    (progn (setq start (point))
				   (move-to-left-margin)
				   (not (looking-at parsep)))
			    (not (and (looking-at parstart)
				      (or (not use-hard-newlines)
					  (get-text-property (1- start) 'hard)
					  (bobp)))))
		  (setq found-start nil)
		  (goto-char start))
		found-start)
	      ;; Found one.
	      (progn
		;; Move forward over paragraph separators.
		;; We know this cannot reach the place we started
		;; because we know we moved back over a non-separator.
		(while (and (not (eobp))
			    (progn (move-to-left-margin)
				   (looking-at parsep)))
		  (forward-line 1))
		;; If line before paragraph is just margin, back up to there.
		(end-of-line 0)
		(if (> (current-column) (current-left-margin))
		    (forward-char 1)
		  (skip-chars-backward " \t")
		  (if (not (bolp))
		      (forward-line 1))))
	    ;; No starter or separator line => use buffer beg.
	    (goto-char (point-min))))))

    (while (and (> arg 0) (not (eobp)))
      ;; Move forward over separator lines...
      (while (and (not (eobp))
		  (progn (move-to-left-margin) (not (eobp)))
		  (looking-at parsep))
	(forward-line 1))
      (unless (eobp) (setq arg (1- arg)))
      ;; ... and one more line.
      (forward-line 1)
      (if fill-prefix-regexp
	  ;; There is a fill prefix; it overrides parstart.
	  (while (and (not (eobp))
		      (progn (move-to-left-margin) (not (eobp)))
		      (not (looking-at parsep))
		      (looking-at fill-prefix-regexp))
	    (forward-line 1))
	(while (and (re-search-forward sp-parstart nil 1)
		    (progn (setq start (match-beginning 0))
			   (goto-char start)
			   (not (eobp)))
		    (progn (move-to-left-margin)
			   (not (looking-at parsep)))
		    (or (not (looking-at parstart))
			(and use-hard-newlines
			     (not (get-text-property (1- start) 'hard)))))
	  (forward-char 1))
	(if (< (point) (point-max))
	    (goto-char start))))
    (constrain-to-field nil opoint t)
    ;; Return the number of steps that could not be done.
    arg))

(defun backward-paragraph (&optional arg)
  "Move backward to start of paragraph.
With argument ARG, do it ARG times;
a negative argument ARG = -N means move forward N paragraphs.

A paragraph start is the beginning of a line which is a
`first-line-of-paragraph' or which is ordinary text and follows a
paragraph-separating line; except: if the first real line of a
paragraph is preceded by a blank line, the paragraph starts at that
blank line.

See `forward-paragraph' for more information."
  (interactive "_p") ; XEmacs
  (or arg (setq arg 1))
  (forward-paragraph (- arg)))

(defun mark-paragraph (&optional arg)
  "Put point at beginning of this paragraph, mark at end.
The paragraph marked is the one that contains point or follows point.

With argument ARG, puts mark at end of a following paragraph, so that
the number of paragraphs marked equals ARG.

If ARG is negative, point is put at end of this paragraph, mark is put
at beginning of this or a previous paragraph.

If this command is repeated, it marks the next ARG paragraphs after (or
before, if arg is negative) the ones already marked."
  (interactive "p")
  (unless arg (setq arg 1))
  (when (zerop arg)
    (error "Cannot mark zero paragraphs"))
  (cond ((and (eq last-command this-command) (mark t))
	 (set-mark
	  (save-excursion
	    (goto-char (mark))
	    (forward-paragraph arg)
	    (point))))
	(t
	 (forward-paragraph arg)
	 (push-mark nil t t)
	 (backward-paragraph arg))))

(defun kill-paragraph (arg)
  "Kill forward to end of paragraph.
With arg N, kill forward to Nth end of paragraph;
negative arg -N means kill backward to Nth start of paragraph."
  (interactive "*p") ; XEmacs
  (kill-region (point) (progn (forward-paragraph arg) (point))))

(defun backward-kill-paragraph (arg)
  "Kill back to start of paragraph.
With arg N, kill back to Nth start of paragraph;
negative arg -N means kill forward to Nth end of paragraph."
  (interactive "*p") ; XEmacs
  (kill-region (point) (progn (backward-paragraph arg) (point))))

(defun transpose-paragraphs (arg)
  "Interchange this (or next) paragraph with previous one."
  (interactive "*p")
  (transpose-subr 'forward-paragraph arg))

(defun start-of-paragraph-text ()
  (let ((opoint (point)) npoint)
    (forward-paragraph -1)
    (setq npoint (point))
    (skip-chars-forward " \t\n")
    ;; If the range of blank lines found spans the original start point,
    ;; try again from the beginning of it.
    ;; Must be careful to avoid infinite loop
    ;; when following a single return at start of buffer.
    (if (and (>= (point) opoint) (< npoint opoint))
	(progn
	  (goto-char npoint)
	  (if (> npoint (point-min))
	      (start-of-paragraph-text))))))

(defun end-of-paragraph-text ()
  (let ((opoint (point)))
    (forward-paragraph 1)
    (if (eq (char-before (point)) ?\n) (backward-char 1))
    (if (<= (point) opoint)
	(progn
	  (forward-char 1)
	  (if (< (point) (point-max))
	      (end-of-paragraph-text))))))

(defun forward-sentence (&optional arg)
  "Move forward to next `sentence-end'.  With argument, repeat.
With negative argument, move backward repeatedly to `sentence-beginning'.

The variable `sentence-end' is a regular expression that matches ends of
sentences.  A paragraph boundary also terminates a sentence."
  (interactive "_p") ; XEmacs
  (or arg (setq arg 1))
  (let ((opoint (point)))
    (while (< arg 0)
      (let ((pos (point))
	    (par-beg (save-excursion (start-of-paragraph-text) (point))))
       (if (and (re-search-backward sentence-end par-beg t)
		(or (< (match-end 0) pos)
		    (re-search-backward sentence-end par-beg t)))
	   (goto-char (match-end 0))
	 (goto-char par-beg)))
      (setq arg (1+ arg)))
    (while (> arg 0)
      (let ((par-end (save-excursion (end-of-paragraph-text) (point))))
       (if (re-search-forward sentence-end par-end t)
	   (skip-chars-backward " \t\n")
	 (goto-char par-end)))
      (setq arg (1- arg)))
    (constrain-to-field nil opoint t)))

(defun backward-sentence (&optional arg)
  "Move backward to start of sentence.  With arg, do it arg times.
See `forward-sentence' for more information."
  (interactive "_p") ; XEmacs
  (or arg (setq arg 1))
  (forward-sentence (- arg)))

(defun kill-sentence (&optional arg)
  "Kill from point to end of sentence.
With arg, repeat; negative arg -N means kill back to Nth start of sentence."
  (interactive "*p") ; XEmacs
  (kill-region (point) (progn (forward-sentence arg) (point))))

(defun backward-kill-sentence (&optional arg)
  "Kill back from point to start of sentence.
With arg, repeat, or kill forward to Nth end of sentence if negative arg -N."
  (interactive "*p") ; XEmacs
  (kill-region (point) (progn (backward-sentence arg) (point))))

(defun mark-end-of-sentence (arg)
  "Put mark at end of sentence.  Arg works as in `forward-sentence'.
If this command is repeated, it marks the next ARG sentences after the
ones already marked."
  (interactive "p")
  ;; FSF Version:
;   (push-mark
;    (save-excursion
;      (if (and (eq last-command this-command) (mark t))
; 	 (goto-char (mark)))
;      (forward-sentence arg)
;      (point))
;    nil t))
  (mark-something 'mark-end-of-sentence 'forward-sentence arg))

(defun mark-end-of-line (arg)
  "Put mark at end of line.  Arg works as in `end-of-line'."
  (interactive "p")
  (mark-something 'mark-end-of-line 'end-of-line arg))

(defun transpose-sentences (arg)
  "Interchange this (next) and previous sentence."
  (interactive "*p")
  (transpose-subr 'forward-sentence arg))

;;; Local Variables:
;;; coding: iso-2022-7bit
;;; End:

;;; paragraphs.el ends here
