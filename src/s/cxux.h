/* Header file for Harris CXUX.
   Copyright (C) 1994 Free Software Foundation, Inc.

This file is part of XEmacs.

XEmacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

XEmacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: FSF 19.31. */

/*
 *	Define symbols to identify the version of Unix this is.
 *	Define all the symbols that apply correctly.
 */

#define USG5
#define USG

#ifndef	_CX_UX
#define	_CX_UX 1
#endif

/* Define this symbol if you are running CX/UX 7.0 or later (7.0 introduced
 * support for ELF files, and while we still build emacs in COFF format, the
 * way it is linked is different for 7.0).
 */
/* #define USING_CX_UX_7 */

#ifdef USING_CX_UX_7
#define LINKER "/usr/sde/coff/usr/bin/ld"
#define LD_SWITCH_SYSTEM "-L/usr/sde/coff/usr/lib -zzero_word"
#define START_FILES "pre-crt0.o /usr/sde/coff/usr/lib/crt0.o /usr/sde/coff/usr/lib/m88100.o"
#else	/* !USING_CX_UX_7 */
#ifdef	_M88K
#define	START_FILES "pre-crt0.o /lib/crt0.o"
#else
#define	START_FILES "cxux-crt0.o /lib/crt0.o"
#endif
#endif	/* USING_CX_UX_7 */

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "usg-unix-v"

#define C_SWITCH_SYSTEM "-Xa"

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'a' means it is /dev/ptya0  */

#define FIRST_PTY_LETTER 'A'
#define	PTY_ITERATION	for (c = 'A'; c <= 'P'; c++) for (i = 0; i < 16; i++)

/* If your system uses COFF (Common Object File Format) then define the
   preprocessor symbol "COFF". */

#define COFF

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

#define MAIL_USE_FLOCK


/* The symbol in the kernel where the load average is found
   is named _avenrun.  */

#define LDAV_SYMBOL "_avenrun"

#define KERNEL_FILE "/unix"

/* There are too many kludges required to redefine malloc - use the system
   one */
#define SYSTEM_MALLOC

/* const really does work, but I can't get configure to run the C compiler
 * with the right options so it figures that out.
 */
#undef const

/*
 * <pwd.h> already declares getpwuid, and with a uid_t argument in ANSI C
 * mode.  Define this so xrdb.c will compile
 */
#ifdef	__STDC__
#define	DECLARE_GETPWUID_WITH_UID_T
#endif
