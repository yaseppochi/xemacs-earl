;;; cyrillic.el --- Support for Cyrillic -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1995,1999 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 1997 MORIOKA Tomohiko
;; Copyright (C) 2001, 2002 Ben Wing.

;; Keywords: multilingual, Cyrillic

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

;; The character set ISO8859-5 is supported.  KOI-8 and ALTERNATIVNYJ are
;; converted to ISO8859-5 internally.

;; Windows-1251 support deleted because XEmacs has automatic support.

;;; Code:

;; Cyrillic syntax
(modify-syntax-entry 'cyrillic-iso8859-5 "w")
(modify-syntax-entry ?,L-(B ".")
(modify-syntax-entry ?,Lp(B ".")
(modify-syntax-entry ?,L}(B ".")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CYRILLIC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ISO-8859-5

; (make-charset 'cyrillic-iso8859-5 
; 	      "Right-Hand Part of Latin/Cyrillic Alphabet (ISO/IEC 8859-5): ISO-IR-144"
; 	      '(dimension
; 		1
; 		registry "ISO8859-5"
; 		chars 96
; 		columns 1
; 		direction l2r
; 		final ?L
; 		graphic 1
; 		short-name "RHP of ISO8859/5"
; 		long-name "RHP of Cyrillic (ISO 8859-5): ISO-IR-144"
; 		))

(make-coding-system
 'iso-8859-5 'iso2022
 "ISO-8859-5 (Cyrillic)"
 '(charset-g0 ascii
   charset-g1 cyrillic-iso8859-5
   charset-g2 t
   charset-g3 t
   mnemonic "ISO8/Cyr"
   ))

(set-language-info-alist
 "Cyrillic-ISO" '((charset cyrillic-iso8859-5)
		  (tutorial . "TUTORIAL.ru")
		  (coding-system iso-8859-5)
		  (coding-priority iso-8859-5)
		  (input-method . "cyrillic-yawerty")
		  (features cyril-util)
		  (sample-text . "Russian (,L@caaZXY(B)	,L7T`PRabRcYbU(B!")
		  (documentation . "Support for Cyrillic ISO-8859-5."))
 '("Cyrillic"))

;; KOI-8

(eval-and-compile

(defvar cyrillic-koi8-r-decode-table
  [
   0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
   16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31
   32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47
   48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63
   64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79
   80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95
   96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111
   112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127
   ?$B(!(B ?$B("(B ?$B(#(B ?$B($(B ?$B(&(B ?$B(%(B ?$B('(B ?$B()(B ?$B(((B ?$B(*(B ?$B(+(B 32  ?$(G#'(B ?$(G#+(B ?$(G#/(B 32
   32  ?$(C"F(B 32  32  ?$B"#(B 32  ?$B"e(B ?$A!V(B ?$A!\(B ?$A!](B ?,L (B  32  ?,A0(B  ?,A2(B  ?,A7(B  ?,Aw(B
   ?$(G#D(B 32  32  ?,Lq(B  32  32  32  32  32  32  32  32  32  32  32  ?$(G#E(B
   32  32  ?$(G#G(B ?,L!(B  32  32  32  32  32  32  32  32  ?$(G#F(B 32  32  ?,A)(B
   ?,Ln(B  ?,LP(B  ?,LQ(B  ?,Lf(B  ?,LT(B  ?,LU(B  ?,Ld(B  ?,LS(B  ?,Le(B  ?,LX(B  ?,LY(B  ?,LZ(B  ?,L[(B  ?,L\(B  ?,L](B  ?,L^(B 
   ?,L_(B  ?,Lo(B  ?,L`(B  ?,La(B  ?,Lb(B  ?,Lc(B  ?,LV(B  ?,LR(B  ?,Ll(B  ?,Lk(B  ?,LW(B  ?,Lh(B  ?,Lm(B  ?,Li(B  ?,Lg(B  ?,Lj(B 
   ?,LN(B  ?,L0(B  ?,L1(B  ?,LF(B  ?,L4(B  ?,L5(B  ?,LD(B  ?,L3(B  ?,LE(B  ?,L8(B  ?,L9(B  ?,L:(B  ?,L;(B  ?,L<(B  ?,L=(B  ?,L>(B 
   ?,L?(B  ?,LO(B  ?,L@(B  ?,LA(B  ?,LB(B  ?,LC(B  ?,L6(B  ?,L2(B  ?,LL(B  ?,LK(B  ?,L7(B  ?,LH(B  ?,LM(B  ?,LI(B  ?,LG(B  ?,LJ(B ]
  "Cyrillic KOI8-R decoding table.")

(defvar cyrillic-koi8-r-encode-table
  (let ((table (make-vector 256 32))
	(i 0))
    (while (< i 256)
      (let* ((ch (aref cyrillic-koi8-r-decode-table i))
	     (split (split-char ch)))
	(cond ((eq (car split) 'cyrillic-iso8859-5)
	       (aset table (logior (nth 1 split) 128) i)
	       )
	      ((eq ch 32))
	      ((eq (car split) 'ascii)
	       (aset table ch i)
	       )))
      (setq i (1+ i)))
    table)
  "Cyrillic KOI8-R encoding table.")

)

(define-ccl-program ccl-decode-koi8
  `(3
    ((read r0)
     (loop
      (write-read-repeat r0 ,cyrillic-koi8-r-decode-table))))
  "CCL program to decode KOI8.")

(define-ccl-program ccl-encode-koi8
  `(1
    ((read r0)
     (loop
      (if (r0 != ,(charset-id 'cyrillic-iso8859-5))
	  (write-read-repeat r0)
	((read r0)
	 (write-read-repeat r0 , cyrillic-koi8-r-encode-table))))))
  "CCL program to encode KOI8.")

;; (define-coding-system-alias 'koi8-r 'cyrillic-koi8)
;; (define-coding-system-alias 'koi8 'cyrillic-koi8)

(make-coding-system
 'koi8-r 'ccl
 "KOI8-R (Cyrillic)"
 '(decode ccl-decode-koi8
   encode ccl-encode-koi8
   mnemonic "KOI8"))

;; `iso-8-1' is not correct, but XEmacs doesn't have a `ccl' category
(coding-system-put 'koi8-r 'category 'iso-8-1)

;; (define-ccl-program ccl-encode-koi8-font
;;   `(0
;;     ((r1 |= 128)
;;      (r1 = r1 ,cyrillic-koi8-r-encode-table)))
;;   "CCL program to encode Cyrillic chars to KOI font.")

;; (setq font-ccl-encoder-alist
;;       (cons (cons "koi8" ccl-encode-koi8-font) font-ccl-encoder-alist))

(defvar cyrillic-koi8-r-to-external-code-table
  (let ((table (make-char-table 'generic))
	(i 0)
	(len (length cyrillic-koi8-r-decode-table)))
    (while (< i len)
      (let ((ch (aref cyrillic-koi8-r-decode-table i)))
	(if (characterp ch)
	    (put-char-table ch i table)))
      (incf i)))
  "Table to convert from characters to their Koi8-R code.")

(set-language-info-alist
 "Cyrillic-KOI8" '((charset cyrillic-iso8859-5)
		   (coding-system koi8-r)
		   (coding-priority koi8-r)
		   (input-method . "cyrillic-yawerty")
		   (features cyril-util)
		   (locale "ru")
		   (mswindows-locale . "RUSSIAN")
		   (tutorial . "TUTORIAL.ru")
		   (sample-text . "Russian (,L@caaZXY(B)	,L7T`PRabRcYbU(B!")
		   (documentation . "Support for Cyrillic KOI8-R."))
 '("Cyrillic"))

;;; WINDOWS-1251 deleted; we support it automatically in XEmacs

;;; ALTERNATIVNYJ

(eval-and-compile

(defvar cyrillic-alternativnyj-decode-table
  [
   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
   16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31
   32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47
   48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63
   64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79
   80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95
   96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111
   112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127
   ?,L0(B  ?,L1(B  ?,L2(B  ?,L3(B  ?,L4(B  ?,L5(B  ?,L6(B  ?,L7(B  ?,L8(B  ?,L9(B  ?,L:(B  ?,L;(B  ?,L<(B  ?,L=(B  ?,L>(B  ?,L?(B
   ?,L@(B  ?,LA(B  ?,LB(B  ?,LC(B  ?,LD(B  ?,LE(B  ?,LF(B  ?,LG(B  ?,LH(B  ?,LI(B  ?,LJ(B  ?,LK(B  ?,LL(B  ?,LM(B  ?,LN(B  ?,LO(B
   ?,LP(B  ?,LQ(B  ?,LR(B  ?,LS(B  ?,LT(B  ?,LU(B  ?,LV(B  ?,LW(B  ?,LX(B  ?,LY(B  ?,LZ(B  ?,L[(B  ?,L\(B  ?,L](B  ?,L^(B  ?,L_(B
   32  32  32  32  32  32  32  32  32  32  32  32  32  32  32  32
   32  32  32  32  32  32  32  32  32  32  32  32  32  32  32  32
   32  32  32  32  32  32  32  32  32  32  32  32  32  32  32  32
   ?,L`(B  ?,La(B  ?,Lb(B  ?,Lc(B  ?,Ld(B  ?,Le(B  ?,Lf(B  ?,Lg(B  ?,Lh(B  ?,Li(B  ?,Lj(B  ?,Lk(B  ?,Ll(B  ?,Lm(B  ?,Ln(B  ?,Lo(B
   ?,L!(B  ?,Lq(B   32  32  32  32  32  32  32  32  32  32  32  32  32 ?,Lp(B]
  "Cyrillic ALTERNATIVNYJ decoding table.")

(defvar cyrillic-alternativnyj-encode-table
  (let ((table (make-vector 256 32))
	(i 0))
    (while (< i 256)
      (let* ((ch (aref cyrillic-alternativnyj-decode-table i))
	     (split (split-char ch)))
	(if (eq (car split) 'cyrillic-iso8859-5)
	    (aset table (logior (nth 1 split) 128) i)
	  (if (/= ch 32)
	      (aset table ch i))))
      (setq i (1+ i)))
    table)
  "Cyrillic ALTERNATIVNYJ encoding table.")
  
)


(define-ccl-program ccl-decode-alternativnyj
  `(3
    ((read r0)
     (loop
      (write-read-repeat r0 ,cyrillic-alternativnyj-decode-table))))
  "CCL program to decode Alternativnyj.")

(define-ccl-program ccl-encode-alternativnyj
  `(1
    ((read r0)
     (loop
      (if (r0 != ,(charset-id 'cyrillic-iso8859-5))
	  (write-read-repeat r0)
	((read r0)
	 (write-read-repeat r0 ,cyrillic-alternativnyj-encode-table))))))
  "CCL program to encode Alternativnyj.")

;; (define-coding-system-alias 'alternativnyj 'cyrillic-alternativnyj)

(make-coding-system
 'alternativnyj 'ccl
 "Alternativnyj (Cyrillic)"
 '(decode ccl-decode-alternativnyj
   encode ccl-encode-alternativnyj
   mnemonic "Cy.Alt"))

;; `iso-8-1' is not correct, but XEmacs doesn't have `ccl' category
(coding-system-put 'alternativnyj 'category 'iso-8-1)

;; (define-ccl-program ccl-encode-alternativnyj-font
;;   '(0
;;     ((r1 |= 128)
;;      (r1 = r1 ,cyrillic-alternativnyj-encode-table)))
;;   "CCL program to encode Cyrillic chars to Alternativnyj font.")

;; (setq font-ccl-encoder-alist
;;       (cons (cons "alternativnyj" ccl-encode-alternativnyj-font)
;;             font-ccl-encoder-alist))

(defvar cyrillic-alternativnyj-to-external-code-table
  (let ((table (make-char-table 'generic))
	(i 0)
	(len (length cyrillic-alternativnyj-decode-table)))
    (while (< i len)
      (let ((ch (aref cyrillic-alternativnyj-decode-table i)))
	(if (characterp ch)
	    (put-char-table ch i table)))
      (incf i)))
  "Table to convert from characters to their Alternativnyj code.")

(set-language-info-alist
 "Cyrillic-ALT" '((charset cyrillic-iso8859-5)
		  (coding-system alternativnyj)
		  (coding-priority alternativnyj)
		  (input-method . "cyrillic-yawerty")
		  (features cyril-util)
		  (tutorial . "TUTORIAL.ru")
		  (sample-text . "Russian (,L@caaZXY(B)	,L7T`PRabRcYbU(B!")
		  (documentation . "Support for Cyrillic ALTERNATIVNYJ."))
 '("Cyrillic"))

;;; cyrillic.el ends here
