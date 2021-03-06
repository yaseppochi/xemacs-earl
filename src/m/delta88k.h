/* Machine description file for Motorola System V/88 machines
   Copyright (C) 1985 Free Software Foundation, Inc.

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
   USUAL-OPSYS="usg5-3"  */

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically:
   Ones defined so far include vax, m68000, ns16000, pyramid,
   orion, tahoe, APOLLO and many others */

#ifndef m88000     /* Some 88k C compilers already define this */
#define m88000
#endif

/* Now define a symbol for the cpu type, if your compiler
   does not define it automatically.  */


/* Data type of load average, as read out of kmem.  */
/* No load average on Motorola machines. */
/* #define LOAD_AVE_TYPE double */

/* Convert that into an integer that is 100 for a load average of 1.0  */
/* #define LOAD_AVE_CVT(x) ((int) ((x) * 100.0)) */

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#define NO_REMAP

/*
 * we have the wrong name for networking libs
 */
#ifdef USG5_4
/* rms: not needed; LIB_X11_LIB deals with this.  */
/* #define LIBX11_SYSTEM -lX11 */
#else
#undef LIB_X11_LIB /* We don't have the shared libs as assumed in usg5-3.h. */
#undef LIBX11_SYSTEM
#define LIBX11_SYSTEM "-lnsl -lbsd"
#endif /* USG5_4 */


/* previously defined in usg5-4, if we choose to use that.  */
/* XEmacs: smarch@quaver.urbana.mcd.mot.com (Steve March) says
   we always need -lgen and usg5-4.h doesn't use it. */
#undef LIBS_SYSTEM
#ifdef USG5_4
#define LIBS_SYSTEM "-lsocket -lnsl -lelf -lgen"
#else
#define LIBS_SYSTEM "-lbsd -lg"
#endif /* USG5_4 */

#define NO_SIOCTL_H

/* XEmacs change -- smarch@quaver.urbana.mcd.mot.com */
#ifndef NOT_C_CODE
#include <dirent.h>
#endif

#define USE_GETOBAUD
