/* alliant-2800.h - Alliant FX/2800 machine running Concentrix 2800.
   Copyright (C) 1990 Free Software Foundation, Inc.

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
   USUAL-OPSYS="bsd4-3"  */

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   vax, m68000, ns16000, pyramid, orion, tahoe and APOLLO
   are the ones defined so far.  */

#define ALLIANT
#define ALLIANT_2800
#define sun			/* Use X support for Sun keyboard stuff. */
#define C_OPTIMIZE_SWITCH "-Og"	/* No concurrent code allowed here. */

/* Concentrix uses a different kernel symbol for load average. */

#undef  LDAV_SYMBOL		/* Undo definition in s-bsd4-2.h */
#define LDAV_SYMBOL "_Loadavg"

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE long

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) (x * 100 / LOADAVG_SCALE)

/* include <sys/param.h> for the definition of LOADAVG_SCALE, and also
   LOADAVG_SIZE, the number of items in the Loadavg array. */

#define UNEXEC "unexfx2800.o"
#define LIBS_MACHINE "-lalliant"

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */
/* Actually, Alliant CONCENTRIX does paging "right":
   data pages are copy-on-write, which means that the pure data areas
   are shared automatically and remapping is not necessary.  */

#define NO_REMAP

/* Alliant needs special crt0.o because system version is not reentrant */

#define START_FILES "crt0.o"

/* Alliant dependent code for dumping executing image.
   See crt0.c code for alliant.  */

#define ADJUST_EXEC_HEADER {\
extern int _curbrk, _setbrk;\
_setbrk = _curbrk;\
hdr.a_bss_addr = bss_start;\
unexec_text_start = hdr.a_text_addr;}

#if 0 /* RMSmacs */
/* Use the K&R version of the DEFUN macro.  */
#define USE_NONANSI_DEFUN
#endif
