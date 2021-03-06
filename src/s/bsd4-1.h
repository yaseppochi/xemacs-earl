/* Definitions file for XEmacs running on bsd 4.1.
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

#define BSD4_1

#define BSD

/* SYSTEM_TYPE should indicate the kind of system you are using.
 It sets the Lisp variable system-type.  */

#define SYSTEM_TYPE "berkeley-unix"

/* First pty name is /dev/ptyp0.  */

#define FIRST_PTY_LETTER 'p'
 
/* If your system uses COFF (Common Object File Format) then define the
   preprocessor symbol "COFF". */

/* #define COFF */

/* define MAIL_USE_FLOCK if the mailer uses flock
   to interlock access to /usr/spool/mail/$USER.
   The alternative is that a lock file named
   /usr/spool/mail/$USER.lock.  */

/* #define MAIL_USE_FLOCK */

/* The file containing the kernel's symbol table is called /vmunix.  */

#define KERNEL_FILE "/vmunix"

/* The symbol in the kernel where the load average is found
   is named _avenrun.  */

#define LDAV_SYMBOL "_avenrun"

/* Special hacks needed to make Emacs run on this system.  */

#define lstat stat

/* sys_open handles the necessary 4.2 features for open.  */

#define ENCAPSULATE_OPEN

/* Names of flags for open.  */
#define O_RDONLY 0
#define O_WRONLY 1
#define O_RDWR 2
#define O_EXCL	2000
#define O_CREAT 1000

/* Special library needed for linking for 4.1.  */
#define LIBS_SYSTEM "-ljobs"
