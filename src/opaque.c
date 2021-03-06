/* Opaque Lisp objects.
   Copyright (C) 1993, 1994, 1995 Sun Microsystems, Inc.
   Copyright (C) 1995, 1996, 2002 Ben Wing.

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
along with XEmacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Synched up with: Not in FSF. */

/* Written by Ben Wing, October 1993. */

/* "Opaque" is used internally to hold keep track of allocated memory
   so it gets GC'd properly, and to store arbitrary data in places
   where a Lisp_Object is required and which may get GC'd. (e.g.  as
   the argument to record_unwind_protect()).  Once created in C,
   opaque objects cannot be resized.

   OPAQUE OBJECTS SHOULD NEVER ESCAPE TO THE LISP LEVEL.  Some code
   depends on this.  As such, opaque objects are a generalization
   of the Qunbound marker.
 */

#include <config.h>
#include "lisp.h"
#include "opaque.h"

#ifndef MC_ALLOC
Lisp_Object Vopaque_ptr_free_list;
#endif /* not MC_ALLOC */

/* Should never, ever be called. (except by an external debugger) */
static void
print_opaque (Lisp_Object obj, Lisp_Object printcharfun,
	      int UNUSED (escapeflag))
{
  const Lisp_Opaque *p = XOPAQUE (obj);

  write_fmt_string
    (printcharfun,
     "#<INTERNAL OBJECT (XEmacs bug?) (opaque, size=%lu) 0x%lx>",
     (long)(p->size), (unsigned long) p);
}

inline static Bytecount
aligned_sizeof_opaque (Bytecount opaque_size)
{
  return MAX_ALIGN_SIZE (offsetof (Lisp_Opaque, data) + opaque_size);
}

static Bytecount
sizeof_opaque (const void *header)
{
  return aligned_sizeof_opaque (((const Lisp_Opaque *) header)->size);
}

/* Return an opaque object of size SIZE.
   If DATA is OPAQUE_CLEAR, the object's data is memset to '\0' bytes.
   If DATA is OPAQUE_UNINIT, the object's data is uninitialized.
   Else the object's data is initialized by copying from DATA. */
Lisp_Object
make_opaque (const void *data, Bytecount size)
{
  Lisp_Opaque *p = (Lisp_Opaque *)
    BASIC_ALLOC_LCRECORD (aligned_sizeof_opaque (size), &lrecord_opaque);
  p->size = size;

  if (data == OPAQUE_CLEAR)
    memset (p->data, '\0', size);
  else if (data == OPAQUE_UNINIT)
    DO_NOTHING;
  else
    memcpy (p->data, data, size);

  {
    return wrap_opaque (p);
  }
}

/* This will not work correctly for opaques with subobjects! */

static int
equal_opaque (Lisp_Object obj1, Lisp_Object obj2, int UNUSED (depth))
{
  Bytecount size;
  return ((size = XOPAQUE_SIZE (obj1)) == XOPAQUE_SIZE (obj2) &&
	  !memcmp (XOPAQUE_DATA (obj1), XOPAQUE_DATA (obj2), size));
}

/* This will not work correctly for opaques with subobjects! */

static Hashcode
hash_opaque (Lisp_Object obj, int UNUSED (depth))
{
  if (XOPAQUE_SIZE (obj) == sizeof (unsigned long))
    return *((Hashcode *) XOPAQUE_DATA (obj));
  else
    return memory_hash (XOPAQUE_DATA (obj), XOPAQUE_SIZE (obj));
}

static const struct memory_description opaque_description[] = {
  { XD_END }
};

DEFINE_LRECORD_SEQUENCE_IMPLEMENTATION ("opaque", opaque,
					1, /*dumpable-flag*/
					0, print_opaque, 0,
					equal_opaque, hash_opaque,
					opaque_description,
					sizeof_opaque, Lisp_Opaque);

/* stuff to handle opaque pointers */

/* Should never, ever be called. (except by an external debugger) */
static void
print_opaque_ptr (Lisp_Object obj, Lisp_Object printcharfun,
		  int UNUSED (escapeflag))
{
  const Lisp_Opaque_Ptr *p = XOPAQUE_PTR (obj);

  write_fmt_string
    (printcharfun,
     "#<INTERNAL OBJECT (XEmacs bug?) (opaque-ptr, adr=0x%lx) 0x%lx>",
     (long)(p->ptr), (unsigned long) p);
}

static int
equal_opaque_ptr (Lisp_Object obj1, Lisp_Object obj2, int UNUSED (depth))
{
  return (XOPAQUE_PTR (obj1)->ptr == XOPAQUE_PTR (obj2)->ptr);
}

static Hashcode
hash_opaque_ptr (Lisp_Object obj, int UNUSED (depth))
{
  return (Hashcode) XOPAQUE_PTR (obj)->ptr;
}

static const struct memory_description opaque_ptr_description[] = {
  { XD_END }
};

DEFINE_LRECORD_IMPLEMENTATION ("opaque-ptr", opaque_ptr,
			       0, /*dumpable-flag*/
			       0, print_opaque_ptr, 0,
			       equal_opaque_ptr, hash_opaque_ptr,
			       opaque_ptr_description, Lisp_Opaque_Ptr);

Lisp_Object
make_opaque_ptr (void *val)
{
#ifdef MC_ALLOC
  Lisp_Object res = 
    wrap_pointer_1 (alloc_lrecord_type (Lisp_Opaque_Ptr,
					 &lrecord_opaque_ptr));
#else /* not MC_ALLOC */
  Lisp_Object res = alloc_managed_lcrecord (Vopaque_ptr_free_list);
#endif /* not MC_ALLOC */
  set_opaque_ptr (res, val);
  return res;
}

/* Be very very careful with this.  Same admonitions as with
   free_cons() apply. */

void
free_opaque_ptr (Lisp_Object ptr)
{
#ifdef MC_ALLOC
  free_lrecord (ptr);
#else /* not MC_ALLOC */
  free_managed_lcrecord (Vopaque_ptr_free_list, ptr);
#endif /* not MC_ALLOC */
}

#ifndef MC_ALLOC
void
reinit_opaque_early (void)
{
  Vopaque_ptr_free_list = make_lcrecord_list (sizeof (Lisp_Opaque_Ptr),
					      &lrecord_opaque_ptr);
  staticpro_nodump (&Vopaque_ptr_free_list);
}
#endif /* not MC_ALLOC */

void
init_opaque_once_early (void)
{
  INIT_LRECORD_IMPLEMENTATION (opaque);
  INIT_LRECORD_IMPLEMENTATION (opaque_ptr);

#ifndef MC_ALLOC
  reinit_opaque_early ();
#endif /* not MC_ALLOC */
}
