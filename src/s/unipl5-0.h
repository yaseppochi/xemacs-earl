/* Definitions file for XEmacs running on UniSoft's UniPlus 5.0
   Support for this system is not finished; don't expect this to work.
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.

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

#define USG				/* System III, System V, etc */

#define USG5

#define UNIPLUS

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "unisoft-unix"

/* Letter to use in finding device name of first pty,
  if system supports pty's.  'a' means it is /dev/ptya0  */

/* #define FIRST_PTY_LETTER 'a' */

#define NO_SUBPROCESSES

/* If your system uses COFF (Common Object File Format) then define the
   preprocessor symbol "COFF". */

/* #define COFF */

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

/* #define MAIL_USE_FLOCK */

/* The file containing the kernel's symbol table is called /unix.  */

#define KERNEL_FILE "/unix"

/* The symbol in the kernel where the load average is found
   is named avenrun.  */

#define LDAV_SYMBOL "avenrun"

/* Special hacks needed to make Emacs run on this system.  */

/* On USG systems the system calls are interruptible by signals
 that the user program has elected to catch.  Thus the system call
 must be retried in these cases.  To handle this without massive
 changes in the source code, we remap the standard system call names
 to names for our own functions in sysdep.c that do the system call
 with retries. */

#define INTERRUPTIBLE_OPEN
#define INTERRUPTIBLE_IO

/* Compiler bug bites when default ADDR_CORRECT is used.  */

#define ADDR_CORRECT(x) (x)

/* Special library needed for linking for Uniplus */

#define LIBS_SYSTEM "-lnet"

/* A system-specific loader switch is needed.  */

#define LD_SWITCH_SYSTEM "-N -L/lib/libg /usr/lib/unshared.ld"
