/* machine description file for hp9000 series 800 machines.
   Copyright (C) 1987 Free Software Foundation, Inc.

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
   USUAL-OPSYS="hpux"  */

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   Ones defined so far include vax, m68000, ns16000, pyramid,
   orion, tahoe, APOLLO and many others */
#ifndef hp9000s800
#	define hp9000s800
#endif


#ifdef __hpux
/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   Ones defined so far include vax, m68000, ns16000, pyramid,
   orion, tahoe, APOLLO and many others */
#ifndef hp9000s800
#     define hp9000s800
#endif

/* Data type of load average, as read out of kmem.  */

#define LOAD_AVE_TYPE double

/* Convert that into an integer that is 100 for a load average of 1.0  */

#define LOAD_AVE_CVT(x) ((int) (x * 100.0))

/* the data segment on this machine always starts at address 0x40000000. */

#ifdef DATA_START
#undef DATA_START
#endif
#ifdef TEXT_START
#undef TEXT_START
#endif

#define DATA_START    0x40000000
#define TEXT_START    0x00000000

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#define NO_REMAP

/* This machine requires completely different unexec code
   which lives in a separate file.  Specify the file name.  */

#define UNEXEC "unexhp9k800.o"

#define LIBS_MACHINE
#define LIBS_DEBUG


/* The symbol in the kernel where the load average is found
   is named _avenrun.  At this time there are two major flavors
   of hp-ux (there is the s800 and s300 (s200) flavors).  The
   differences are thusly moved to the corresponding machine description file.
*/

/* no underscore please */
#define LDAV_SYMBOL "avenrun"

#if 0   /* Supposedly no longer true.  */
/* In hpux, for unknown reasons, S_IFLNK is defined even though
   symbolic links do not exist.
   Make sure our conditionals based on S_IFLNK are not confused.

   Here we assume that stat.h is included before config.h
   so that we can override it here.  */

#undef S_IFLNK
#endif

#endif /* __hpux */
