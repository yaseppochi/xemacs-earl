/* Static Heap management routines for XEmacs.
   Copyright (C) 1994, 1998 Free Software Foundation, Inc.

This file is part of XEmacs.

XEmacs is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

XEmacs is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with XEmacs; see the file COPYING.  If not, write to the Free
Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.*/

#include <config.h>
#include "lisp.h"

#include "sysfile.h"

#include <unistd.h>
#include <sheap-adjust.h>

#define STATIC_HEAP_BASE	0x800000
#define STATIC_HEAP_SLOP	0x40000
#define STATIC_HEAP_SIZE \
(STATIC_HEAP_BASE + SHEAP_ADJUSTMENT + STATIC_HEAP_SLOP)
#define BLOCKSIZE	(1<<12)
#define ALLOC_UNIT (BLOCKSIZE-1)
#define ALLOC_MASK ~((unsigned long)(ALLOC_UNIT))
#define ALIGN_ALLOC(addr) ((((unsigned long)addr) + ALLOC_UNIT) & ALLOC_MASK)

char	static_heap_buffer[STATIC_HEAP_SIZE]={0};
char*	static_heap_base=static_heap_buffer;
char*	static_heap_ptr=static_heap_buffer;
unsigned long	static_heap_size=STATIC_HEAP_SIZE;
int 	static_heap_initialized=0;
int 	static_heap_dumped=0;

void *more_static_core ( ptrdiff_t increment );
void *more_static_core ( ptrdiff_t increment )
{
  int size = (int) increment;
  void *result;

  if (!static_heap_initialized)
    {
#ifdef VALMASK
      if (((unsigned long) static_heap_base & ~VALMASK) != 0)
	{
	  printf ("error: The heap was allocated in upper memory.\n");
	  exit (-1);
	}
#endif
      static_heap_base=(char*)ALIGN_ALLOC(static_heap_buffer);
      static_heap_ptr=static_heap_base;
      static_heap_size=STATIC_HEAP_SIZE -
	(static_heap_base-static_heap_buffer);
#ifdef CYGWIN
      sbrk(BLOCKSIZE);		/* force space for fork to work */
#endif
      static_heap_initialized=1;
    }

  result = static_heap_ptr;

  /* we don't need to align - handled by gmalloc.  */

  if (size < 0)
    {
      if (static_heap_ptr + size < static_heap_base)
	{
	  return 0;
	}
    }
  else
    {
      if (static_heap_ptr + size >= static_heap_base + static_heap_size)
	{
	  printf (
"\nRequested %d bytes, static heap exhausted!  base is %p, current ptr\n"
"is %p. You have exhausted the static heap. \n"
"\n"
"If you are simply trying to compile, remove sheap-adjust.h\n"
"and recompile from the top level. If this doesn't\n"
"work then STATIC_HEAP_SLOP (defined in this file) is too small.\n"
"\n"
"If you want to run temacs, change SHEAP_ADJUSTMENT in sheap-adjust.h\n"
"to 0 or a +ve number. Generally you should *not* try to run temacs\n"
"with a static heap, you should dump first.\n",
          size, static_heap_base, static_heap_ptr);

	  exit(-1);
	  return 0;
	}
    }
  static_heap_ptr += size;

  return result;
}

static void
sheap_adjust_h (long adjust)
{
  FILE *stream = retry_fopen ("sheap-adjust.h", "w");

  if (stream == NULL)
    report_file_error ("Opening sheap adjustment file",
		       build_string ("sheap-adjust.h"));

  fprintf (stream,
	   "/*\tDo not edit this file!\n"
	   "\tAutomatically generated by XEmacs */\n"
	   "# define SHEAP_ADJUSTMENT (%ld)\n", adjust);
  retry_fclose (stream);
}

void report_sheap_usage (int die_if_pure_storage_exceeded);
void
report_sheap_usage (int die_if_pure_storage_exceeded)
{
  int rc = 0;

  Bytecount lost = STATIC_HEAP_SIZE
    - (static_heap_ptr - static_heap_buffer);
  char buf[200];
  sprintf (buf, "Static heap usage: %ldk of %ldk, slop is %ldk",
               (long) ((static_heap_ptr - static_heap_buffer) /1024),
	   (long) (STATIC_HEAP_SIZE / 1024),
	   (long) STATIC_HEAP_SLOP / 1024);

  if (lost > STATIC_HEAP_SLOP)
    {
      sprintf (buf + strlen (buf), " -- %ldk wasted", (long)(lost/1024));
      if (die_if_pure_storage_exceeded)
	{
	  sheap_adjust_h(STATIC_HEAP_SLOP - lost);
	  sprintf (buf + strlen (buf), " -- reset to %ldk", 
		   (long) ((STATIC_HEAP_SIZE + STATIC_HEAP_SLOP - lost) /
			   1024));
	  rc = -1;
	}
      message ("%s\n", buf);
    }

  if (rc < 0)
    {
      unlink ("SATISFIED");
      stderr_out ("Static heap size adjusted, don't panic!  I will restart the `make'\n");
      exit (0);
    }
}


