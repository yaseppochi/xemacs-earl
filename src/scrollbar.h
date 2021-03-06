/* Define scrollbar instance.
   Copyright (C) 1994, 1995 Board of Trustees, University of Illinois.

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

#ifndef INCLUDED_scrollbar_h_
#define INCLUDED_scrollbar_h_

#ifdef HAVE_SCROLLBARS

struct scrollbar_instance
{
  struct LCRECORD_HEADER header;

  /* Used by the frame caches. */
  struct scrollbar_instance *next;

  /* Pointer back to the mirror structure attached to. */
  struct window_mirror *mirror;

  /* This flag indicates if the scrollbar is currently in use. */
  char scrollbar_is_active;

  /* This flag indicates if a data parameter has changed. */
  char scrollbar_instance_changed;

  /* A structure of auxiliary data specific to the device type.
     struct x_scrollbar_data is used for X window frames; defined in
     scrollbar-x.h */
  void *scrollbar_data;
};

DECLARE_LRECORD (scrollbar_instance, struct scrollbar_instance);
#define XSCROLLBAR_INSTANCE(x) XRECORD (x, scrollbar_instance, struct scrollbar_instance)
#define wrap_scrollbar_instance(p) wrap_record (p, scrollbar_instance)
#define SCROLLBAR_INSTANCEP(x) RECORDP (x, scrollbar_instance)
#define CHECK_SCROLLBAR_INSTANCE(x) CHECK_RECORD (x, scrollbar_instance)
#define CONCHECK_SCROLLBAR_INSTANCE(x) CONCHECK_RECORD (x, scrollbar_instance)

#define SCROLLBAR_INSTANCE_FRAME(inst) (inst->mirror->frame)

void init_frame_scrollbars (struct frame *f);
void init_device_scrollbars (struct device *d);
void init_global_scrollbars (struct device *d);
void free_frame_scrollbars (struct frame *f);
void release_window_mirror_scrollbars (struct window_mirror *mir);
void update_window_scrollbars (struct window *w,
			       struct window_mirror *mirror,
			       int active, int horiz_only);
#ifdef MEMORY_USAGE_STATS
int compute_scrollbar_instance_usage (struct device *d,
				      struct scrollbar_instance *inst,
				      struct overhead_stats *ovstats);
#endif

extern Lisp_Object Vscrollbar_width, Vscrollbar_height;

extern Lisp_Object Qscrollbar_line_up;
extern Lisp_Object Qscrollbar_line_down;
extern Lisp_Object Qscrollbar_page_up;
extern Lisp_Object Qscrollbar_page_down;
extern Lisp_Object Qscrollbar_to_top;
extern Lisp_Object Qscrollbar_to_bottom;
extern Lisp_Object Qscrollbar_vertical_drag;

extern Lisp_Object Qscrollbar_char_left;
extern Lisp_Object Qscrollbar_char_right;
extern Lisp_Object Qscrollbar_page_left;
extern Lisp_Object Qscrollbar_page_right;
extern Lisp_Object Qscrollbar_to_left;
extern Lisp_Object Qscrollbar_to_right;
extern Lisp_Object Qscrollbar_horizontal_drag;

#endif /* HAVE_SCROLLBARS */

#endif /* INCLUDED_scrollbar_h_ */
