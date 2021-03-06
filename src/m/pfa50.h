/* Machine description file for PFU A-series.
   Copyright (C) 1988 Free Software Foundation, Inc.

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

/* Say this machine is a 68000 */

#define m68000
#define mc68000 1

/* Define NO_REMAP if memory segmentation makes it not work well
   to change the boundary between the text section and data section
   when Emacs is dumped.  If you define this, the preloaded Lisp
   code will not be sharable; but that's better than failing completely.  */

#define NO_REMAP

/* Define TEXT_START_ADDR if your linker don't set execute point to _start.
   If it needed, temacs always CORE-DUMP.	*/

#define TEXT_START_ADDR __start

/* Define START_FILES if your machine used _start.
 */

#define START_FILES "crt0.o"

/* Define LD_SWITCH_MACHINE if your linker needs it.
 */

#define LD_SWITCH_MACHINE "-e __start"

/* SX/A has alloca in the PW library.  */

#define LIB_STANDARD "-lPW -lc"

/* SX/A uses terminfo and lib/curses   */

#define TERMINFO

/* SX/A use SystemV style getdents/readdir. */

#define NO_SIOCTL_H

#define BROKEN_SIGIO
