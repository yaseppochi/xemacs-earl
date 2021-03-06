/* machine description file for Whitechapel Computer Works MG1 (ns16000 based).
   Copyright (C) 1985 Free Software Foundation, Inc.
   MG-1 version by L.M.McLoughlin

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: FSF 19.31. */

/* The following line tells the configuration script what sort of 
   operating system this machine is likely to run.
   USUAL-OPSYS="note"

NOTE-START
We are in the dark about what operating system runs on the Whitechapel
systems.  Consult share-lib/MACHINES for information on which
operating systems Emacs has already been ported to; one of them might
work.  If you find an existing system name that works or write your
own configuration files, please let the Free Software Foundation in on
your work; we'd like to distribute this information.
NOTE-END  */

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   vax, m68000, ns16000, pyramid, orion, tahoe and APOLLO
   are the ones defined so far.  */
/* Say this machine is a 16000 and an mg1, cpp says its a 32000 */
#define ns16000
#define mg1

/* Data type of load average, as read out of kmem.  */
/* mg1 its an unsigned long */
#define LOAD_AVE_TYPE unsigned long

/* Convert that into an integer that is 100 for a load average of 1.0  */
#define	FSCALE	1000.0
#define LOAD_AVE_CVT(x) (int) (((double) (x)) * 100.0 / FSCALE)

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */
/* mapping seems screwy */
#define NO_REMAP

/* Avoids a compiler bug */
/* borrowed from sequent.h */
