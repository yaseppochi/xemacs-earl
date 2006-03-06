/* SVG Canvas Widget for XEmacs.
   Copyright (C) 2006 Free Software Foundation, Inc.
   Copyright (C) 1999 Edward A. Falk

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

/**************************************************************************\
*		  SVG Canvas for the Lucid Widget Library		   *
*									   *
*		      See header comment in xlwsvg.c.			   *
\**************************************************************************/

/*
 * xlwsvgP.h - SVG canvas widget
 *   by Stephen J. Turnbull <stephen@xemacs.org>
 * based on GaugeP.h - Gauge widget
 *   by Edward A. Falk <falk@falconer.vip.best.com>, dated July 9, 1997
 *   adapted to XEmacs by Andy Piper <andy@xemacs.org> #### date?
 */

#ifndef _included_xlwsvgP_h
#define _included_xlwsvgP_h

/***********************************************************************
 *
 * SVGCanvas Widget Private Data
 *
 * SVGCanvas has little in common with the label widget, but can make use
 * of some label resources, so is subclassed from label. #### Really??
 *
 ***********************************************************************/

#include "xlwsvg.h"
#include ATHENA_LabelP_h_

/* New fields for the SVGCanvas widget class record */

typedef struct { XtPointer extension; } SVGCanvasClassPart;

/* Full class record declaration */
typedef struct _SVGCanvasClassRec {
  CoreClassPart		core_class;
  SimpleClassPart	simple_class;
#ifdef	_ThreeDP_h
  /* do we ever need 3D? */
  ThreeDClassPart	threeD_class;
#endif
  LabelClassPart	label_class;
  SVGCanvasClassPart	svg_canvas_class;
} SVGCanvasClassRec;

extern SVGCanvasClassRec svgCanvasClassRec;

/* New fields for the SVGCanvas widget record */
typedef struct {
  /* resources */
  String	*svgSource;
  String	*labels;
#if 0
  XtOrientation orientation;
  Boolean	autoScaleUp;	/* scales automatically */
  Boolean	autoScaleDown;	/* scales automatically */
  int		update;		/* update interval */
#endif

  /* private state */
#ifdef NEED_TO_CACHE_WINDOW_INFO
  Display	*dpy;
  int		scr;
  Window	win;
  unsigned long	event_mask;
  GC		gc;
  Visual	*visual;
  Colormap	cmap;
  int		depth;
#endif

#if CURSOR_CODE_IS_FIXED
  Cursor	arrow;
  Cursor	watch;
#endif

  /* SVG/Cairo-specific components */
  cairo_t	*cr;
  svg_cairo_t	*svgc;

  /* display modes - resources? */
  int		x_flip, y_flip;
  int		fit_mode;
  int		full_mode;

  /* graphic geometry - resources? */
  unsigned int	width, height;
  double	tx;		/* window origin in graphic space */
  double	ty;
  double	zoom;		/* scale factor */
  double	tolerance;	/* "smoothness" */

  /* buffer for fast redisplay */
  Pixmap	pix;

  /* redisplay "dirty" flag */
  int		needs_refresh;

  /* X server vocabulary */
  Atom		wm_delete_window_atom;

#ifdef XEMACS_CANT_READ_FILES
  char		**svg_files;
  int		svg_nfile;
  int		svg_curfile;
#else
  XtPointer	buffer;		/* ?? Lisp_Object which is bufferp
				   maybe this should be an lstream? */
#endif
} SVGCanvasPart;


/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _SVGCanvasRec {
  CorePart	core;
  SimplePart	simple;
#ifdef	_ThreeDP_h
  /* do we ever need 3D? */
  ThreeDPart  	threeD;
#endif
  LabelPart	label;
  SVGCanvasPart	svgCanvas;
} SVGCanvasRec;

#endif /* _included_xlwsvgP_h */
