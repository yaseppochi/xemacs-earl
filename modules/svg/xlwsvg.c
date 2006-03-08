/* SVG Widget for XEmacs.
   Copyright (C) 2006 Free Software Foundation, Inc.
   Copyright © 2002 USC/Information Sciences Institute
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
Boston, MA 02111-1307, USA.

Portions of this file were copied from xsvg.c in the xsvg-0.2.1
distribution.  See README.license.xsvg for the xsvg license terms. */

/***************************************************************************\
*		  SVG Canvas for the Lucid Widget Library		    *
*									    *
* This widget implements a canvas for rendering Scalable Vector Graphics    *
* (SVG) objects via libsvg-cairo, which as its name suggests is a thin	    *
* wrapper around libsvg and cairo.  libsvg in turn depends on libxml2 (or   *
* possibly expat).  #### check other dependencies.			    *
*   The plan is to adapt the general organization and possibly some code    *
* from Ed Falk's "Gauge" widget as adapted by Andy Piper to lwlib.	    *
*   The reasons for picking this widget rather than one of the Athena or    *
* Xt widgets are (1) it is dynamic, and it would be nice if the SVG canvas  *
* could be dynamic (for WYSYWIG editing as well as for the animation that   *
* the SVG standard specifies) and (2) the Gauge widget is problematic in    *
* XEmacs, regularly causing crashes and other problems -- maybe I'll learn  *
* enough to clean it up!						    *
*   -- stephen, 2006-03-01						    *
*									    *
* Edward Falk wrote:							    *
*   Note: for fun and demonstration purposes, I have added selection	    *
* capabilities to this widget.  If you select the widget, you create a	    *
* primary selection containing the current value of the widget in both	    *
* integer and string form.  If you copy into the widget, the primary	    *
* selection is converted to an integer value and the gauge is set to that   *
* value.								    *
* stephen adds:								    *
*   I wonder if similar capabilities (selections capable of sending source  *
* and/or pixmaps) would be worth building in.				    *
*									    *
* Implementation strategy						    *
*									    *
* For a first cut we'll take a somewhat cheesy but interesting approach:    *
* implement the SVG glyph as an (active) widget rather than an image	    *
* glyph.								    *
*   - The callbacks then stay, and would all be Lisp functions.		    *
*   - The key bindings go away (since XEmacs will handle those events).	    *
*   - The cursor code will need to call out to XEmacs.			    *
*									    *
* It's not obvious how easy it will be to convert to a "normal" image	    *
* glyph (ie, one which is an eimage pixel buffer object).		    *
*									    *
*   I wonder if it would make sense to convert eimages to cairos?	    *
*									    *
*   A dynamic model would involve double-buffering.  Since inputting the    *
* SVG and exposure events from the display are asynchronous, the work	    *
* buffer and the backing store (the source of the copy in SVGCanvasExpose)  *
* need to be different.  (If the display supports backing store, this	    *
* could be optimized.)							    *
*									    *
*   Although the Gauge widget does make some use of Label and Simple	    *
* resources, it's not clear this is appropriate for the SVGCanvas.  IMO,    *
* the whole Athena widget set probably should be refactored into the	    *
* content handling parts and "surround" gadgetry which would manage borders *
* and other such presentational scutwork.				    *
*   OTOH, this can probably be accomplished at the lwlib level, wrapping    *
* the new abstractions around the widget set implementations.		    *
\***************************************************************************/

/*
 * xlwsvg.c - SVG canvas widget
 *   by Stephen J. Turnbull <stephen@xemacs.org>
 * based on Gauge.c - Gauge widget
 *   by Edward A. Falk <falk@falconer.vip.best.com>, dated July 8, 1997
 *   adapted to XEmacs by Andy Piper <andy@xemacs.org> #### date?
 * based on 
 */

/* TODO
 *
 *   1. Fix all ####.
 *   2. Remove all YAGNIs from xlwsvg.c, xlwsvg.h, and xlwsvgP.h.
 *   3. Remove dependence on Xaw.
 */

#include <config.h>
#include "lisp.h"		/* #### does it make sense to bind widgets
				   so closely to Lisp?  I think so, but... */
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <X11/IntrinsicP.h>
#include <X11/Xatom.h>
#include <X11/StringDefs.h>
#include ATHENA_XawInit_h_
#include "xlwsvgP.h"		/* includes xlwsvg.h which defines YAGNI */

#ifndef YAGNI
#include "../src/xmu.h"
#ifdef HAVE_XMU
#include <X11/Xmu/Atoms.h>
#include <X11/Xmu/Drawing.h>
#include <X11/Xmu/StdSel.h>
#endif

#include "opaque.h"		/* more Lisp stuff from xlwgauge.c */
#include "sysdep.h"
#include "buffer.h"
#include "process.h"		/* for report_process_error */
#endif /* YAGNI */
#ifdef HAVE_SHLIB
# include "emodules.h"
#endif

#ifndef YAGNI
#define	MS_PER_SEC 1000
#endif

/****************************************************************
 *
 * SVGCanvas resources
 *
 ****************************************************************/

#define offset(field) XtOffsetOf (SVGCanvasRec, svgCanvas.field)
static XtResource resources[] = {
  { XtNsvgSource, XtCSVGSource, XtRString, sizeof(String*),
    offset (svgSource), XtRString, (XtPointer) 0 },

  /* Orientation, translation, and scaling should probably be all combined
     into a transformation matrix (surely Cairo and SVG have such types),
     with convenience routines (perhaps in Lisp) to handle common cases. */
};
#undef offset

/* member functions */

static void SVGCanvasClassInit (void);
static void SVGCanvasInit (Widget, Widget, ArgList, Cardinal*);
static void SVGCanvasDestroy (Widget);
static void SVGCanvasResize (Widget);
static void SVGCanvasExpose (Widget, XEvent*, Region);
static Boolean SVGCanvasSetValues (Widget, Widget, Widget, ArgList, Cardinal*);
static XtGeometryResult SVGCanvasQueryGeometry (Widget, XtWidgetGeometry*,
						XtWidgetGeometry*);

/* action procs */

#ifndef YAGNI
static void SVGCanvasSelect (Widget, XEvent*, String*, Cardinal*);
static void SVGCanvasPaste  (Widget, XEvent*, String*, Cardinal*);
#endif

/* internal privates */

#ifndef YAGNI
static void SVGCanvasSize (SVGCanvasWidget, Dimension*, Dimension*);
static void EnableUpdate  (SVGCanvasWidget);
static void DisableUpdate (SVGCanvasWidget);

static Boolean SVGCanvasConvert (Widget, Atom*, Atom*, Atom*,
				 XtPointer*, unsigned long*, int*);
static void SVGCanvasLoseSel (Widget, Atom*);
static void SVGCanvasDoneSel (Widget, Atom*, Atom*);
static void SVGCanvasGetSelCB (Widget, XtPointer, Atom*, Atom*,
			       XtPointer, unsigned long*, int*);

static GC Get_GC (SVGCanvasWidget, Pixel);

static	XtActionsRec	actionsList[] =
{
  {"select",	SVGCanvasSelect},
  {"paste",	SVGCanvasPaste},
};

static	char	defaultTranslations[] =
	"<Btn1Up>:	select()\n\
	 <Key>F1:	select(CLIPBOARD)\n\
	 <Btn2Up>:	paste()\n\
	 <Key>F2:	paste(CLIPBOARD)";
#endif /* YAGNI */


/****************************************************************
 *
 * Full class record constant
 *
 ****************************************************************/

SVGCanvasClassRec svgCanvasClassRec = {
  /* core_class fields */
  {
    /* superclass	  	*/	(WidgetClass) &labelClassRec,
    /* class_name	  	*/	"SVGCanvas",
    /* widget_size	  	*/	sizeof (SVGCanvasRec),
    /* class_initialize   	*/	SVGCanvasClassInit,
    /* class_part_initialize	*/	NULL,
    /* class_inited       	*/	FALSE,
    /* initialize	  	*/	SVGCanvasInit,
    /* initialize_hook		*/	NULL,
    /* realize		  	*/	XtInheritRealize,	/* TODO? */
    /* actions		  	*/	NULL,            /* actionsList, */
    /* num_actions	  	*/	0,    /* XtNumber (actionsList), */
    /* resources	  	*/	resources,
    /* num_resources	  	*/	XtNumber (resources),
    /* xrm_class	  	*/	NULLQUARK,
    /* compress_motion	  	*/	TRUE,
    /* compress_exposure  	*/	TRUE,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest	  	*/	FALSE,
    /* destroy		  	*/	SVGCanvasDestroy,
    /* resize		  	*/	SVGCanvasResize,
    /* expose		  	*/	SVGCanvasExpose,
    /* set_values	  	*/	SVGCanvasSetValues,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus	 	*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private   	*/	NULL,
    /* tm_table		   	*/	NULL,    /* defaultTranslations, */
    /* query_geometry		*/	SVGCanvasQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
  },

  /* Simple class fields initialization */
  {
    /* change_sensitive		*/	XtInheritChangeSensitive
  },

#ifndef YAGNI
#ifdef	_ThreeDP_h
  /* ThreeD class fields initialization */
  {
    XtInheritXaw3dShadowDraw	/* shadowdraw 		*/
  },
#endif
#endif /* YAGNI */

  /* Label class fields initialization */
  {
    /* ignore 			*/	0
  },

  /* SVGCanvas class fields initialization */
  {
    /* extension		*/	NULL
  },
};

WidgetClass svgCanvasWidgetClass = (WidgetClass) &svgCanvasClassRec;



/****************************************************************
 *
 * Member Procedures
 *
 ****************************************************************/

static void
SVGCanvasClassInit (void)
{
    XawInitializeWidgetSet ();
}

/* ARGSUSED */
static void
SVGCanvasInit (Widget   request,
	       Widget   new_,
	       ArgList  UNUSED (args),
	       Cardinal *UNUSED (num_args))
{
  SVGCanvasWidget svgw = (SVGCanvasWidget) new_;
  int status = 0;
  /* #### which of these initializations do we need? */
  svgw->svgCanvas.cairo = NULL;
  svgw->svgCanvas.svg_cairo = NULL;

  /* YAGNIs from xsvg.c:
     Core Widget component initializes display and screen information
     translation (tx and ty), reflection (x_flip and y_flip), zoom, and
       smoothness (tolerance) are initialized as resources
     I don't think we need needs_refresh
     svg_files, svg_nfile, and svg_curfile would be initialized as
       resources, if they make sense at all
  */

  /* If size not explicitly set, set it to our preferred size now.
     If neither width nor height is set, query the svg_cairo;
     else scale the unspecified dimension to the svg_cairo's aspect ratio. */
  if (request->core.width == 0 || request->core.height == 0)
    {
      Dimension w,h;
      SVGCanvasSize (svgw, &w, &h);
      if (request->core.width == 0 && request->core.height == 0)
	new_->core.width = w;
	new_->core.height = h;
      else if (request->core.width == 0)
	new_->core.width = w*request->core.height/h;
      else if (request->core.height == 0)
	new_->core.height = h*request->core.width/w;
      svgw->core.widget_class->core_class.resize (new_);
    }

  /* xvsg does an XFlush here. */

  status = svg_cairo_create (&svgw->svgCanvas.svg_cairo);
  if (!status)
    {
      svg_cairo_set_viewport_dimension (svgw->svgCanvas.svg_cairo,
					svgw->core.width, svgw->core.height);

      /* #### this is wrong, we want to parse a string!! */
      status = svg_cairo_parse_file (svgw->svgCanvas.svg_cairo, stdin);
    }

  /* XEmacs will provide keyboard input handlers.  If we actually handle
     keyboard input in this widget at all, we should just pass the event
     up to XEmacs. */

  /* we're not toplevel so don't participate in window manager protocols */

  /* window mapping is done by Xt when realizing or so */
}

static void
SVGCanvasDestroy (Widget w)
{
  SVGCanvasWidget svgw = (SVGCanvasWidget) w;

  svg_cairo_destroy (svgw->svgCanvas.svg_cairo);
  svgw->svgCanvas.svg_cairo = NULL;
  cairo_destroy (svgw->svgCanvas.cairo);
  svgw->svgCanvas.cairo = NULL;

#ifndef YAGNI
  if (svgw->svgCanvas.selstr != NULL)
    XtFree(svgw->svgCanvas.selstr);

  if (svgw->svgCanvas.selected != None)
    XtDisownSelection (w, svgw->svgCanvas.selected, CurrentTime);

  /* Remove any timeouts associated with dynamic behavior of the widget. */
  if (svgw->svgCanvas.update > 0)
    DisableUpdate (svgw);
#endif
}


/*
 * React to size change from manager.
 * Label widget will compute some internal stuff, but we need to override.
 */

static void
SVGCanvasResize (Widget w)
{
	SVGCanvasWidget svgw = (SVGCanvasWidget) w;
#ifndef YAGNI
	/* TODO: need to call parent resize proc?  I don't think so since
	 * we're recomputing everything from scratch anyway.
	 */
#endif /* YAGNI */
}

/*
 * Repaint the widget window
 */

/* ARGSUSED */
static void
SVGCanvasExpose (Widget w, XEvent event, Region UNUSED (region))
{
  SVGCanvasWidget	svgw = (SVGCanvasWidget) w;
  register Display	*dpy = XtDisplay(w);
  register Window	win = XtWindow(w);

  XCopyArea (dpy, svgw->svgCanvas.pix, win, svgw->core.gc,
	     event->x, event->y, event->width, event->height,
	     event->x, event->y);
}


/*
 * Set specified arguments into widget
 */

static Boolean
SVGCanvasSetValues (Widget   old,
		Widget   UNUSED (request),
		Widget   new_,
		ArgList  UNUSED (args),
		Cardinal *UNUSED (num_args))
{
  SVGCanvasWidget oldsvgw = (SVGCanvasWidget) old;
  SVGCanvasWidget svgw = (SVGCanvasWidget) new_;
  Boolean was_resized = False;

#ifndef YAGNI
  if (svgw->svgCanvas.selected != None) {
    XtDisownSelection (new_, svgw->svgCanvas.selected, CurrentTime);
    svgw->svgCanvas.selected = None;
  }

  /* #### Compute WAS_RESIZED here!!! */

  if (was_resized) {
    if (svgw->label.resize)
      SVGCanvasSize (svgw, &svgw->core.width, &svgw->core.height);
    else
      SVGCanvasResize (new_);
  }

  if (svgw->svgCanvas.update != oldsvgw->svgCanvas.update)
    {
      if (svgw->svgCanvas.update > 0)
	EnableUpdate (svgw);
      else
	DisableUpdate (svgw);
    }

  if (svgw->core.background_pixel != oldsvgw->core.background_pixel)
    {
      XtReleaseGC (new_, svgw->svgCanvas.inverse_GC);
      svgw->svgCanvas.inverse_GC = Get_GC (svgw, svgw->core.background_pixel);
    }

  return was_resized || XtIsSensitive (old) != XtIsSensitive (new_);
#endif /* YAGNI */
}


static XtGeometryResult
SVGCanvasQueryGeometry (Widget w,
		    XtWidgetGeometry *intended,
		    XtWidgetGeometry *preferred)
{
  register SVGCanvasWidget svgw = (SVGCanvasWidget) w;

  if (intended->width == w->core.width  &&
      intended->height == w->core.height)
    return XtGeometryNo;

  preferred->request_mode = CWWidth | CWHeight;
  SVGCanvasSize (svgw, &preferred->width, &preferred->height);

  if ((!(intended->request_mode & CWWidth) ||
       intended->width >= preferred->width)  &&
      (!(intended->request_mode & CWHeight) ||
       intended->height >= preferred->height))
    return XtGeometryYes;
  else
    return XtGeometryAlmost;
}



/****************************************************************
 *
 * Action Procedures
 *
 ****************************************************************/

#ifndef YAGNI
static void
SVGCanvasSelect (Widget   w,
	     XEvent   *event,
	     String   *params,
	     Cardinal *num_params)
{
  SVGCanvasWidget	svgw = (SVGCanvasWidget) w;
  Atom			seln = XA_PRIMARY;

  if (svgw->svgCanvas.selected != None) {
    XtDisownSelection (w, svgw->svgCanvas.selected, CurrentTime);
    svgw->svgCanvas.selected = None;
  }

  if (*num_params > 0) {
    seln = XInternAtom (XtDisplay (w), params[0], False);
    printf ("atom %s is %ld\n", params[0], seln);
  }

  if (! XtOwnSelection (w, seln, event->xbutton.time, SVGCanvasConvert,
			SVGCanvasLoseSel, SVGCanvasDoneSel))
    {
      /* in real code, this error message would be replaced by
       * something more elegant, or at least deleted
       */

      fprintf (stderr, "SVGCanvas failed to get selection, try again\n");
    }
  else
    {
      svgw->svgCanvas.selected = TRUE;
      svgw->svgCanvas.selstr = (String) XtMalloc(4*sizeof (int));
      SVGCanvasExpose (w,0,0);
    }
}


static	Boolean
SVGCanvasConvert (Widget	w,
	      Atom	*selection,	/* usually XA_PRIMARY */
	      Atom	*target,	/* requested target */
	      Atom	*type,		/* returned type */
	      XtPointer *value,		/* returned value */
	      unsigned long	*length,	/* returned length */
	      int	*format)	/* returned format */
{
  SVGCanvasWidget	svgw = (SVGCanvasWidget) w;
  XSelectionRequestEvent *req;

  printf ("requesting selection %s:%s\n",
	  XGetAtomName (XtDisplay (w), *selection),
	  XGetAtomName (XtDisplay (w), *target));

#ifdef HAVE_XMU
  if (*target == XA_TARGETS(XtDisplay (w)))
    {
      XPointer stdTargets;
      Atom *rval;
      unsigned long stdLength;

      /* XmuConvertStandardSelection can handle this.  This function
       * will return a list of standard targets.  We prepend TEXT,
       * STRING and INTEGER to the list and return it.
       */

      req = XtGetSelectionRequest (w, *selection, NULL);
      XmuConvertStandardSelection (w, req->time, selection, target,
				   type, &stdTargets, &stdLength, format);

      *type = XA_ATOM;		/* TODO: needed? */
      *length = stdLength + 3;
      rval = (Atom *) XtMalloc (sizeof (Atom)*(stdLength+3));
      *value = (XtPointer) rval;
      *rval++ = XA_INTEGER;
      *rval++ = XA_STRING;
      *rval++ = XA_TEXT(XtDisplay(w));
      memcpy (rval, stdTargets, stdLength*sizeof (Atom));
      XtFree ((char*) stdTargets);
      *format = 8*sizeof (Atom);	/* TODO: needed? */
      return True;
    }

  else
#endif
    if (*target == XA_INTEGER)
      {
	*type = XA_INTEGER;
	*length = 1;
	*value = (XtPointer) &svgw->svgCanvas.value;
	*format = 8*sizeof (int);
	return True;
      }

    else if (*target == XA_STRING
#ifdef HAVE_XMU
	     ||
	     *target == XA_TEXT (XtDisplay (w))
#endif
	     )
      {
	*type = *target;
	*length = strlen (svgw->svgCanvas.selstr)*sizeof (char);
	*value = (XtPointer) svgw->svgCanvas.selstr;
	*format = 8;
	return True;
      }

    else
      {
	/* anything else, we just give it to XmuConvertStandardSelection() */
#ifdef HAVE_XMU
	req = XtGetSelectionRequest (w, *selection, NULL);
	if (XmuConvertStandardSelection (w, req->time, selection, target,
					 type, (XPointer *) value, length,
					 format))
	  return True;
	else
#endif
	  {
	    printf("SVGCanvas: requestor is requesting unsupported selection %s:%s\n",
		   XGetAtomName (XtDisplay(w),*selection),
		   XGetAtomName (XtDisplay(w),*target));
	    return False;
	  }
      }
}



static	void
SVGCanvasLoseSel (Widget w,
	      Atom   *UNUSED (selection))	/* usually XA_PRIMARY */
{
  SVGCanvasWidget	svgw = (SVGCanvasWidget) w;
  Display *dpy = XtDisplay (w);
  Window	win = XtWindow (w);

  if (svgw->svgCanvas.selstr != NULL) {
    XtFree (svgw->svgCanvas.selstr);
    svgw->svgCanvas.selstr = NULL;
  }

  svgw->svgCanvas.selected = False;
  XClearWindow (dpy,win);
  SVGCanvasExpose (w,0,0);
}


static	void
SVGCanvasDoneSel (Widget UNUSED (w),
	      Atom   *UNUSED (selection),	/* usually XA_PRIMARY */
	      Atom   *UNUSED (target))		/* requested target */
{
  /* selection done, anything to do? */
}


static void
SVGCanvasPaste (Widget   w,
	    XEvent   *event,
	    String   *params,
	    Cardinal *num_params)
{
  Atom		seln = XA_PRIMARY;

  if (*num_params > 0) {
    seln = XInternAtom (XtDisplay(w), params[0], False);
    printf ("atom %s is %ld\n", params[0], seln);
  }

  /* try for integer value first */
  XtGetSelectionValue (w, seln, XA_INTEGER,
		       SVGCanvasGetSelCB, (XtPointer) XA_INTEGER,
		       event->xbutton.time);
}

static	void
SVGCanvasGetSelCB (Widget    w,
	       XtPointer client,
	       Atom      *selection,
	       Atom      *type,
	       XtPointer value,
	       unsigned long    *UNUSED (length),
	       int       *UNUSED (format))
{
  Display	*dpy = XtDisplay (w);
  Atom		target = (Atom) client;
  int		*iptr;
  char		*cptr;

  if (*type == XA_INTEGER) {
    iptr = (int *) value;
    XawSVGCanvasSetValue (w, *iptr);
  }

  else if (*type == XA_STRING
#ifdef HAVE_XMU
	   ||
	   *type == XA_TEXT (dpy)
#endif
	   )
    {
      cptr = (char *)value;
      XawSVGCanvasSetValue (w, atoi(cptr));
    }

  /* failed, try string */
  else if (*type == None && target == XA_INTEGER)
    XtGetSelectionValue (w, *selection, XA_STRING,
			 SVGCanvasGetSelCB, (XtPointer) XA_STRING,
			 CurrentTime);
}
#endif /* YAGNI */


/****************************************************************
 *
 * Public Procedures
 *
 ****************************************************************/



/****************************************************************
 *
 * Private Procedures
 *
 ****************************************************************/

/* Initialize the Cairo surface.  Must be called after realization. */

static int
SVGCanvasPrepareSurface (SVGCanvasWidget svgw)
{
  Display	  *dpy = XtDisplay ((Widget) svgw);
  Window	  win = XtWindow ((Widget) svgw);

  /* allocate a work buffer */
  /* #### I think this bogosity is due to X11 not handling vacuous objects
     gracefully.  We should do that ourselves rather than "trick" X11. */
  if (!svgw->core.width)
    svgw->core.width = 1;
  if (!svgw->core.height)
    svgw->core.height = 1;
  svgw->svgCanvas.pix = XCreatePixmap (dpy, win, svgw->core.width,
				       svgw->core.height, svgw->core.depth);

  /* clear the work buffer */
  {
    XGCValues gcv;
    Screen *scr = XtScreen ((Widget) svgw);

    /* #### xsvg checks for ARGB visual for XRender and doesn't use
       WhitePixel in that case, but we don't really know how to handle
       that; anyway XEmacs chooses the visual */
    gcv.foreground = WhitePixelOfScreen (dpy, scr);
    svgw->label.normal_GC = XtGetGC (dpy, svgw->svgCanvas.pix,
				     GCForeground, &gcv);
    XFillRectangle (dpy, svgw->svgCanvas.pix, svgw->label.normal_GC, 0, 0,
		    svgw->core.width, svgw->core.height);
  }

  /* #### for dynamic display maybe we need to hang on to the surface? */
  /* #### we probably should refactor Cairo handling to make generalization
     to other XEmacs display types more straightforward */
  {
    cairo_surface_t *surface;

    if (svgw->svgCanvas.svg_cairo)
      svg_cairo_destroy (svgw->svgCanvas.svg_cairo);
    svgw->svgCanvas.svg_cairo = NULL;

    surface = cairo_xlib_surface_create (dpy,
					 svgw->svgCanvas.pix,
					 svgw->core.visual,
					 svgw->core.width,
					 svgw->core.height);
    svgw->svgCanvas.cairo = cairo_create (surface);
    cairo_surface_destroy (surface);
  }
  /* XXX: This probably doesn't need to be here (eventually) */
  cairo_set_source_rgb (svgw->svgCanvas.cairo, 1, 1, 1);

  /* #### this should be unnecessary, except we might set geometry above */
  svg_cairo_set_viewport_dimension (svgw->svgCanvas.svg_cairo,
				    svgw->core.width,
				    svgw->core.height);

#ifndef YAGNI
  /* #### This probably belongs in a realize procedure. */

  /* #### How should we handle key presses?  I guess that native widgets
     handle their own key presses.  In Xt, I think they should just queue
     an XEmacs key event.  But Andy's widgets actually execute code? */
  svgw->core.event_mask = (KeyPressMask
		     | StructureNotifyMask
		     | ExposureMask);
  XSelectInput (dpy, svgw->core.win, svgw->core.event_mask);

  /* Initialize selection data. */
  svgw->svgCanvas.selected = None;
  svgw->svgCanvas.selstr = NULL;

  /* #### What's this? */
  if (svgw->svgCanvas.update > 0)
    EnableUpdate (svgw);
#endif /* YAGNI */
}

/* Determine the preferred size for this widget. */

static void
SVGCanvasSize (SVGCanvasWidget	svgw,
	       Dimension	*wid,
	       Dimension	*hgt)
{
  /* find total height and width of contents */
  svg_cairo_get_size (svgw->svgCanvas.svg_cairo, wid, hgt);
}


#ifndef YAGNI
static	void
EnableUpdate (SVGCanvasWidget svgw)
{
  svgw->svgCanvas.intervalId =
    XtAppAddTimeOut (XtWidgetToApplicationContext ((Widget) svgw),
		     svgw->svgCanvas.update * MS_PER_SEC, SVGCanvasGetValue,
		     (XtPointer) svgw);
}

static	void
DisableUpdate (SVGCanvasWidget svgw)
{
  XtRemoveTimeOut (svgw->svgCanvas.intervalId);
}

static	GC
Get_GC (SVGCanvasWidget	svgw,
	Pixel		fg)
{
  XGCValues	values;
#define	vmask	GCForeground
#define	umask	(GCBackground|GCSubwindowMode|GCGraphicsExposures|GCDashOffset\
		|GCFont|GCDashList|GCArcMode)

  values.foreground = fg;

  return XtAllocateGC ((Widget) svgw, 0, vmask, &values, 0L, umask);
}
#endif /* YAGNI */
