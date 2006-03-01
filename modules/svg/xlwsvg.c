/* SVG Widget for XEmacs.
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
* This widget implements a canvas for rendering Scalable Vector Graphics   *
* (SVG) objects via libsvg-cairo, which as its name suggests is a thin	   *
* wrapper around libsvg and cairo.  libsvg in turn depends on libxml2 (or  *
* possibly expat).  #### check other dependencies.			   *
*   The plan is to adapt the general organization and possibly some code   *
* from Ed Falk's "Gauge" widget as adapted by Andy Piper to lwlib.	   *
*   The reasons for picking this widget rather than one of the Athena or   *
* Xt widgets are (1) it is dynamic, and it would be nice if the SVG canvas *
* could be dynamic (for WYSYWIG editing as well as for the animation that  *
* the SVG standard specifies) and (2) the Gauge widget is problematic in   *
* XEmacs, regularly causing crashes and other problems -- maybe I'll learn *
* enough to clean it up!						   *
*   -- stephen, 2006-03-01						   *
\**************************************************************************/

/*
 * xlwsvg.c - SVG canvas widget
 *   by Stephen J. Turnbull <stephen@xemacs.org>
 * based on Gauge.c - Gauge widget
 *   by Edward A. Falk <falk@falconer.vip.best.com>, dated July 8, 1997
 *   adapted to XEmacs by Andy Piper <andy@xemacs.org> #### date?
 *
 * Edward Falk wrote:
 * Note: for fun and demonstration purposes, I have added selection
 * capabilities to this widget.  If you select the widget, you create
 * a primary selection containing the current value of the widget in
 * both integer and string form.  If you copy into the widget, the
 * primary selection is converted to an integer value and the gauge is
 * set to that value.
 *
 * I wonder if similar capabilities (selections capable of sending source
 * and/or pixmaps) would be worth building in.
 */

#if 0
#define	DEF_LEN	50	/* default width (or height for vertical gauge) */
#define	MIN_LEN	10	/* minimum reasonable width (height) */
#define	TIC_LEN	6	/* length of tic marks */
#define	GA_WID	3	/* width of gauge */
#define	MS_PER_SEC 1000
#endif

#include <config.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <X11/IntrinsicP.h>
#include <X11/Xatom.h>
#include <X11/StringDefs.h>
#include ATHENA_XawInit_h_
#include "xlwsvgP.h"
#if 0
#include "../src/xmu.h"
#ifdef HAVE_XMU
#include <X11/Xmu/Atoms.h>
#include <X11/Xmu/Drawing.h>
#include <X11/Xmu/StdSel.h>
#endif
#endif

/****************************************************************
 *
 * SVGCanvas resources
 *
 ****************************************************************/

#if 0
static	char	defaultTranslations[] =
	"<Btn1Up>:	select()\n\
	 <Key>F1:	select(CLIPBOARD)\n\
	 <Btn2Up>:	paste()\n\
	 <Key>F2:	paste(CLIPBOARD)";
#endif

#define offset(field) XtOffsetOf (GaugeRec, field)
static XtResource resources[] = {
  { XtNsvgSource, XtCSVGSource, XtRString, sizeof(String *),
    offset (svgCanvas.svgSource), XtRString, (XtPointer) 0 },
#if 0
  { XtNvalue, XtCValue, XtRInt, sizeof(int),
    offset (gauge.value), XtRImmediate, (XtPointer) 0 },
  { XtNminValue, XtCMinValue, XtRInt, sizeof(int),
    offset (gauge.v0), XtRImmediate, (XtPointer) 0 },
  { XtNmaxValue, XtCMaxValue, XtRInt, sizeof(int),
    offset (gauge.v1), XtRImmediate, (XtPointer) 100 },
  { XtNntics, XtCNTics, XtRInt, sizeof(int),
    offset (gauge.ntics), XtRImmediate, (XtPointer) 0 },
  { XtNnlabels, XtCNLabels, XtRInt, sizeof(int),
    offset (gauge.nlabels), XtRImmediate, (XtPointer) 0 },
  { XtNlabels, XtCLabels, XtRStringArray, sizeof(String *),
    offset (gauge.labels), XtRStringArray, NULL },
  { XtNautoScaleUp, XtCAutoScaleUp, XtRBoolean, sizeof(Boolean),
    offset (gauge.autoScaleUp), XtRImmediate, FALSE },
  { XtNautoScaleDown, XtCAutoScaleDown, XtRBoolean, sizeof(Boolean),
    offset (gauge.autoScaleDown), XtRImmediate, FALSE },
  { XtNorientation, XtCOrientation, XtROrientation, sizeof(XtOrientation),
    offset (gauge.orientation), XtRImmediate, (XtPointer) XtorientHorizontal },
  { XtNupdate, XtCInterval, XtRInt, sizeof(int),
    offset (gauge.update), XtRImmediate, (XtPointer) 0 },
  { XtNgetValue, XtCCallback, XtRCallback, sizeof(XtPointer),
    offset (gauge.getValue), XtRImmediate, (XtPointer) NULL },
#endif
};
#undef offset

	/* member functions */

static void SVGCanvasClassInit (void);
static void SVGCanvasInit (Widget, Widget, ArgList, Cardinal *);
static void SVGCanvasDestroy (Widget);
static void SVGCanvasResize (Widget);
static void SVGCanvasExpose (Widget, XEvent *, Region);
static Boolean SVGCanvasSetValues (Widget, Widget, Widget, ArgList, Cardinal *);
static XtGeometryResult SVGCanvasQueryGeometry (Widget, XtWidgetGeometry *,
						XtWidgetGeometry *);

	/* action procs */

#if 0
static void SVGCanvasSelect (Widget, XEvent *, String *, Cardinal *);
static void SVGCanvasPaste  (Widget, XEvent *, String *, Cardinal *);
#endif

	/* internal privates */

#if 0
static void SVGCanvasSize (SVGCanvasWidget, Dimension *, Dimension *, Dimension);
static void MaxLabel (SVGCanvasWidget, Dimension *, Dimension *,
		      Dimension *, Dimension *);
static void AutoScale     (SVGCanvasWidget);
static void EnableUpdate  (SVGCanvasWidget);
static void DisableUpdate (SVGCanvasWidget);

static void SVGCanvasGetValue (XtPointer, XtIntervalId *);

static Boolean SVGCanvasConvert (Widget, Atom *, Atom *, Atom *,
				 XtPointer *, unsigned long *, int *);
static void SVGCanvasLoseSel (Widget, Atom *);
static void SVGCanvasDoneSel (Widget, Atom *, Atom *);
static void SVGCanvasGetSelCB (Widget, XtPointer, Atom *, Atom *,
			       XtPointer, unsigned long *, int *);

static GC Get_GC (SVGCanvasWidget, Pixel);
#endif

static	XtActionsRec	actionsList[] =
{
  {"select",	SVGCanvasSelect},
  {"paste",	SVGCanvasPaste},
};



/****************************************************************
 *
 * Full class record constant
 *
 ****************************************************************/

SVGCanvasClassRec gaugeClassRec = {
  {
/* core_class fields */
    /* superclass	  	*/	(WidgetClass) &labelClassRec,
    /* class_name	  	*/	"SVGCanvas",
    /* widget_size	  	*/	sizeof (SVGCanvasRec),
    /* class_initialize   	*/	SVGCanvasClassInit,
    /* class_part_initialize	*/	NULL,
    /* class_inited       	*/	FALSE,
    /* initialize	  	*/	SVGCanvasInit,
    /* initialize_hook		*/	NULL,
    /* realize		  	*/	XtInheritRealize,	/* TODO? */
    /* actions		  	*/	actionsList,
    /* num_actions	  	*/	XtNumber (actionsList),
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
    /* tm_table		   	*/	defaultTranslations,
    /* query_geometry		*/	SVGCanvasQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
  },
/* Simple class fields initialization */
  {
    /* change_sensitive		*/	XtInheritChangeSensitive
  },
#ifdef	_ThreeDP_h
/* ThreeD class fields initialization */
  {
    XtInheritXaw3dShadowDraw	/* shadowdraw 		*/
  },
#endif
/* Label class fields initialization */
  {
    /* ignore 			*/	0
  },
/* SVGCanvas class fields initialization */
  {
    /* extension		*/	NULL
  },
};

WidgetClass gaugeWidgetClass = (WidgetClass) &gaugeClassRec;




/****************************************************************
 *
 * Member Procedures
 *
 ****************************************************************/

static void
SVGCanvasClassInit (void)
{
    XawInitializeWidgetSet ();
#ifdef HAVE_XMU
    XtAddConverter (XtRString, XtROrientation, XmuCvtStringToOrientation,
    		NULL, 0);
#endif
}



/* ARGSUSED */
static void
SVGCanvasInit (Widget   request,
	   Widget   new_,
	   ArgList  UNUSED (args),
	   Cardinal *UNUSED (num_args))
{
    SVGCanvasWidget svgw = (SVGCanvasWidget) new_;

    if (svgw->gauge.v0 == 0  &&  svgw->gauge.v1 == 0) {
      svgw->gauge.autoScaleUp = svgw->gauge.autoScaleDown = TRUE;
      AutoScale (svgw);
    }

    /* If size not explicitly set, set it to our preferred size now.  */

    if (request->core.width == 0  ||  request->core.height == 0)
    {
      Dimension w,h;
      SVGCanvasSize (svgw, &w,&h, DEF_LEN);
      if (request->core.width == 0)
	new_->core.width = w;
      if (request->core.height == 0)
	new_->core.height = h;
      svgw->core.widget_class->core_class.resize (new_);
    }

    svgw->gauge.selected = None;
    svgw->gauge.selstr = NULL;

    if (svgw->gauge.update > 0)
      EnableUpdate (svgw);

    svgw->gauge.inverse_GC = Get_GC (svgw, svgw->core.background_pixel);
}

static void
SVGCanvasDestroy (Widget w)
{
	SVGCanvasWidget svgw = (SVGCanvasWidget) w;

	if (svgw->gauge.selstr != NULL)
	  XtFree(svgw->gauge.selstr);

	if (svgw->gauge.selected != None)
	  XtDisownSelection (w, svgw->gauge.selected, CurrentTime);

	XtReleaseGC (w, svgw->gauge.inverse_GC);

	if (svgw->gauge.update > 0)
	  DisableUpdate (svgw);
}


/* React to size change from manager.  Label widget will compute some
 * internal stuff, but we need to override.
 */

static void
SVGCanvasResize (Widget w)
{
	SVGCanvasWidget svgw = (SVGCanvasWidget) w;
	int	size;		/* height (width) of gauge */
	int	vmargin;	/* vertical (horizontal) margin */
	int	hmargin;	/* horizontal (vertical) margin */

	vmargin = svgw->gauge.orientation == XtorientHorizontal ?
	  svgw->label.internal_height : svgw->label.internal_width;
	hmargin = svgw->gauge.orientation == XtorientHorizontal ?
	  svgw->label.internal_width : svgw->label.internal_height;

	/* TODO: need to call parent resize proc?  I don't think so since
	 * we're recomputing everything from scratch anyway.
	 */

	/* find total height (width) of contents */

	size = GA_WID+2;			/* gauge itself + edges */

	if (svgw->gauge.ntics > 1)		/* tic marks */
	  size += vmargin + TIC_LEN;

	if (svgw->gauge.nlabels > 1)
	{
	  Dimension	lwm, lw0, lw1;	/* width of max, left, right labels */
	  Dimension	lh;

	  MaxLabel (svgw,&lwm,&lh, &lw0,&lw1);

	  if (svgw->gauge.orientation == XtorientHorizontal)
	  {
	    svgw->gauge.margin0 = lw0 / 2;
	    svgw->gauge.margin1 = lw1 / 2;
	    size += lh + vmargin;
	  }
	  else
	  {
	    svgw->gauge.margin0 =
	    svgw->gauge.margin1 = lh / 2;
	    size += lwm + vmargin;
	  }
	}
	else
	  svgw->gauge.margin0 = svgw->gauge.margin1 = 0;

	svgw->gauge.margin0 += hmargin;
	svgw->gauge.margin1 += hmargin;

	/* Now distribute height (width) over components */

	if (svgw->gauge.orientation == XtorientHorizontal)
	  svgw->gauge.gmargin = (svgw->core.height-size)/2;
	else
	  svgw->gauge.gmargin = (svgw->core.width-size)/2;

	svgw->gauge.tmargin = svgw->gauge.gmargin + GA_WID+2 + vmargin;
	if (svgw->gauge.ntics > 1)
	  svgw->gauge.lmargin = svgw->gauge.tmargin + TIC_LEN + vmargin;
	else
	  svgw->gauge.lmargin = svgw->gauge.tmargin;
}

/*
 * Repaint the widget window
 */

/* ARGSUSED */
static void
SVGCanvasExpose (Widget w,
	     XEvent *UNUSED (event),
	     Region UNUSED (region))
{
	SVGCanvasWidget svgw = (SVGCanvasWidget) w;
register Display *dpy = XtDisplay(w);
register Window	win = XtWindow(w);
	GC	gc;	/* foreground, background */
	GC	gctop, gcbot;	/* dark, light shadows */

	int	len;		/* length (width or height) of widget */
	int	e0,e1;		/* ends of the gauge */
	int	x;
	int	y;		/* vertical (horizontal) position */
	int	i;
	int	v0 = svgw->gauge.v0;
	int	v1 = svgw->gauge.v1;
	int	value = svgw->gauge.value;

	gc = XtIsSensitive (w) ? svgw->label.normal_GC : svgw->label.gray_GC;


#ifdef	_ThreeDP_h
	gctop = svgw->threeD.bot_shadow_GC;
	gcbot = svgw->threeD.top_shadow_GC;
#else
	gctop = gcbot = gc;
#endif

	if (svgw->gauge.orientation == XtorientHorizontal) {
	  len = svgw->core.width;
	} else {
	  len = svgw->core.height;
	}

	/* if the gauge is selected, signify by drawing the background
	 * in a contrasting color.
	 */

	if (svgw->gauge.selected)
	{
	  XFillRectangle (dpy,win, gc, 0,0, w->core.width,w->core.height);
	  gc = svgw->gauge.inverse_GC;
	}

	e0 = svgw->gauge.margin0;		/* left (top) end */
	e1 = len - svgw->gauge.margin1 -1;	/* right (bottom) end */

	/* Draw the SVGCanvas itself */

	y = svgw->gauge.gmargin;

	if (svgw->gauge.orientation == XtorientHorizontal)	/* horizontal */
	{
	  XDrawLine (dpy,win,gctop, e0+1,y, e1-1,y);
	  XDrawLine (dpy,win,gctop, e0,y+1, e0,y+GA_WID);
	  XDrawLine (dpy,win,gcbot, e0+1, y+GA_WID+1, e1-1, y+GA_WID+1);
	  XDrawLine (dpy,win,gcbot, e1,y+1, e1,y+GA_WID);
	}
	else							/* vertical */
	{
	  XDrawLine (dpy,win,gctop, y,e0+1, y,e1-1);
	  XDrawLine (dpy,win,gctop, y+1,e0, y+GA_WID,e0);
	  XDrawLine (dpy,win,gcbot, y+GA_WID+1,e0+1, y+GA_WID+1, e1-1);
	  XDrawLine (dpy,win,gcbot, y+1,e1, y+GA_WID,e1);
	}

	if (svgw->gauge.ntics > 1)
	{
	  y = svgw->gauge.tmargin;
	  for(i=0; i<svgw->gauge.ntics; ++i)
	  {
	    x = e0 + i*(e1-e0-1)/(svgw->gauge.ntics-1);
	    if (svgw->gauge.orientation == XtorientHorizontal) {
	      XDrawLine (dpy,win,gcbot, x,y+1, x,y+TIC_LEN-2);
	      XDrawLine (dpy,win,gcbot, x,y, x+1,y);
	      XDrawLine (dpy,win,gctop, x+1,y+1, x+1,y+TIC_LEN-2);
	      XDrawLine (dpy,win,gctop, x,y+TIC_LEN-1, x+1,y+TIC_LEN-1);
	    }
	    else {
	      XDrawLine (dpy,win,gcbot, y+1,x, y+TIC_LEN-2,x);
	      XDrawLine (dpy,win,gcbot, y,x, y,x+1);
	      XDrawLine (dpy,win,gctop, y+1,x+1, y+TIC_LEN-2,x+1);
	      XDrawLine (dpy,win,gctop, y+TIC_LEN-1,x, y+TIC_LEN-1,x+1);
	    }
	  }
	}

	/* draw labels */
	if (svgw->gauge.nlabels > 1)
	{
	  char	label[20], *s = label;
	  int	xlen, wd,h =0;

	  if (svgw->gauge.orientation == XtorientHorizontal)
	    y = svgw->gauge.lmargin + svgw->label.font->max_bounds.ascent - 1;
	  else {
	    y = svgw->gauge.lmargin;
	    h = svgw->label.font->max_bounds.ascent / 2;
	  }

	  for(i=0; i<svgw->gauge.nlabels; ++i)
	  {
	    if (svgw->gauge.labels == NULL)
	      sprintf (label, "%d", v0+i*(v1 - v0)/(svgw->gauge.nlabels - 1));
	    else
	      s = svgw->gauge.labels[i];
	    if (s != NULL) {
	      x = e0 + i*(e1-e0-1)/(svgw->gauge.nlabels-1);
	      xlen = strlen (s);
	      if (svgw->gauge.orientation == XtorientHorizontal) {
		wd = XTextWidth (svgw->label.font, s, xlen);
		XDrawString (dpy,win,gc, x-wd/2,y, s,xlen);
	      }
	      else {
		XDrawString (dpy,win,gc, y,x+h, s,xlen);
	      }
	    }
	  }
	}
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

	if (svgw->gauge.selected != None) {
	  XtDisownSelection (new_, svgw->gauge.selected, CurrentTime);
	  svgw->gauge.selected = None;
	}

	/* Changes to v0,v1,labels, ntics, nlabels require resize & redraw. */
	/* Change to value requires redraw and possible resize if autoscale */

	was_resized =
	  svgw->gauge.v0 != oldsvgw->gauge.v0  ||
	  svgw->gauge.v1 != oldsvgw->gauge.v1  ||
	  svgw->gauge.ntics != oldsvgw->gauge.ntics  ||
	  svgw->gauge.nlabels != oldsvgw->gauge.nlabels  ||
	  svgw->gauge.labels != oldsvgw->gauge.labels;

	if ((svgw->gauge.autoScaleUp && svgw->gauge.value > svgw->gauge.v1) ||
	    (svgw->gauge.autoScaleDown && svgw->gauge.value < svgw->gauge.v1/3))
	{
	  AutoScale(svgw);
	  was_resized = TRUE;
	}

	if (was_resized) {
	  if (svgw->label.resize)
	    SVGCanvasSize (svgw, &svgw->core.width, &svgw->core.height, DEF_LEN);
	  else
	    SVGCanvasResize (new_);
	}

	if (svgw->gauge.update != oldsvgw->gauge.update)
	  {
	    if (svgw->gauge.update > 0)
	      EnableUpdate (svgw);
	    else
	      DisableUpdate (svgw);
	  }

	if (svgw->core.background_pixel != oldsvgw->core.background_pixel)
	{
	  XtReleaseGC (new_, svgw->gauge.inverse_GC);
	  svgw->gauge.inverse_GC = Get_GC (svgw, svgw->core.background_pixel);
	}

	return was_resized || svgw->gauge.value != oldsvgw->gauge.value  ||
	   XtIsSensitive (old) != XtIsSensitive (new_);
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
    SVGCanvasSize (svgw, &preferred->width, &preferred->height, DEF_LEN);

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

static void
SVGCanvasSelect (Widget   w,
	     XEvent   *event,
	     String   *params,
	     Cardinal *num_params)
{
	SVGCanvasWidget	svgw = (SVGCanvasWidget) w;
	Atom		seln = XA_PRIMARY;

	if (svgw->gauge.selected != None) {
	  XtDisownSelection (w, svgw->gauge.selected, CurrentTime);
	  svgw->gauge.selected = None;
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
	  svgw->gauge.selected = TRUE;
	  svgw->gauge.selstr = (String) XtMalloc(4*sizeof (int));
	  sprintf (svgw->gauge.selstr, "%d", svgw->gauge.value);
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
	    XGetAtomName (XtDisplay (w),*selection),
	    XGetAtomName (XtDisplay (w),*target));

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
	  *value = (XtPointer) &svgw->gauge.value;
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
	  *length = strlen (svgw->gauge.selstr)*sizeof (char);
	  *value = (XtPointer) svgw->gauge.selstr;
	  *format = 8;
	  return True;
	}

	else
	{
	  /* anything else, we just give it to XmuConvertStandardSelection() */
#ifdef HAVE_XMU
	  req = XtGetSelectionRequest (w, *selection, NULL);
	  if (XmuConvertStandardSelection (w, req->time, selection, target,
	  	type, (XPointer *) value, length, format))
	    return True;
	  else
#endif
	    {
	    printf(
		"SVGCanvas: requestor is requesting unsupported selection %s:%s\n",
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

	if (svgw->gauge.selstr != NULL) {
	  XtFree (svgw->gauge.selstr);
	  svgw->gauge.selstr = NULL;
	}

	svgw->gauge.selected = False;
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
	Atom	target = (Atom) client;
	int	*iptr;
	char	*cptr;

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



/****************************************************************
 *
 * Public Procedures
 *
 ****************************************************************/


	/* Change gauge value.  Only undraw or draw what needs to be
	 * changed.
	 */

void
XawSVGCanvasSetValue (Widget   w,
		  Cardinal value)
{
	SVGCanvasWidget svgw = (SVGCanvasWidget) w;
	int	oldvalue;
	GC	gc;

	if (svgw->gauge.selected != None) {
	  XtDisownSelection(w, svgw->gauge.selected, CurrentTime);
	  svgw->gauge.selected = None;
	}

	if (!XtIsRealized (w)) {
	  svgw->gauge.value = value;
	  return;
	}

	/* need to rescale? */
	if ((svgw->gauge.autoScaleUp && (int) value > svgw->gauge.v1) ||
	    (svgw->gauge.autoScaleDown && (int) value < svgw->gauge.v1/3))
	{
	  XtVaSetValues (w, XtNvalue, value, 0);
	  return;
	}

	oldvalue = svgw->gauge.value;
	svgw->gauge.value = value;

	gc = XtIsSensitive (w) ? svgw->label.normal_GC : svgw->label.gray_GC;
}


Cardinal
XawSVGCanvasGetValue (Widget w)
{
	SVGCanvasWidget svgw = (SVGCanvasWidget) w;
	return svgw->gauge.value;
}




/****************************************************************
 *
 * Private Procedures
 *
 ****************************************************************/

/* Search the labels, find the largest one. */
/* TODO: handle vertical fonts? */

static void
MaxLabel (SVGCanvasWidget	svgw,
	  Dimension	*wid,	/* max label width */
	  Dimension	*hgt,	/* max label height */
	  Dimension	*w0,	/* width of first label */
	  Dimension	*w1)	/* width of last label */
{
	char	lstr[80], *lbl;
	int	w;
	XFontStruct *font = svgw->label.font;
	int	i;
	int	lw = 0;
	int	v0 = svgw->gauge.v0;
	int	dv = svgw->gauge.v1 - v0;
	int	n = svgw->gauge.nlabels;

	if (n > 0)
	{
	  if (--n <= 0) {n = 1; v0 += dv/2;}

	  /* loop through all labels, figure out how much room they
	   * need.
	   */
	  w = 0;
	  for(i=0; i<svgw->gauge.nlabels; ++i)
	  {
	    if (svgw->gauge.labels == NULL)	/* numeric labels */
	      sprintf (lbl = lstr,"%d", v0 + i*dv/n);
	    else
	      lbl = svgw->gauge.labels[i];

	    if (lbl != NULL) {
	      lw = XTextWidth (font, lbl, strlen(lbl));
	      w = Max (w, lw);
	    }
	    else
	      lw = 0;

	    if (i == 0 && w0 != NULL) *w0 = lw;
	  }
	  if (w1 != NULL) *w1 = lw;

	  *wid = w;
	  *hgt = font->max_bounds.ascent + font->max_bounds.descent;
	}
	else
	  *wid = *hgt = 0;
}


/* Determine the preferred size for this widget.  choose 100x100 for
 * debugging.
 */

static void
SVGCanvasSize (SVGCanvasWidget svgw,
	   Dimension   *wid,
	   Dimension   *hgt,
	   Dimension   min_len)
{
	int	w,h;		/* width, height of gauge */
	int	vmargin;	/* vertical margin */
	int	hmargin;	/* horizontal margin */

	hmargin = svgw->label.internal_width;
	vmargin = svgw->label.internal_height;

	/* find total height (width) of contents */


	/* find minimum size for undecorated gauge */

	if (svgw->gauge.orientation == XtorientHorizontal)
	{
	  w = min_len;
	  h = GA_WID+2;			/* gauge itself + edges */
	}
	else
	{
	  w = GA_WID+2;
	  h = min_len;
	}

	if (svgw->gauge.ntics > 0)
	{
	  if (svgw->gauge.orientation == XtorientHorizontal)
	  {
	    w = Max (w, svgw->gauge.ntics*3);
	    h += vmargin + TIC_LEN;
	  }
	  else
	  {
	    w += hmargin + TIC_LEN;
	    h = Max (h, svgw->gauge.ntics*3);
	  }
	}


	/* If labels are requested, this gets a little interesting.
	 * We want the end labels centered on the ends of the gauge and
	 * the centers of the labels evenly spaced.  The labels at the ends
	 * will not be the same width, meaning that the gauge itself need
	 * not be centered in the widget.
	 *
	 * First, determine the spacing.  This is the width of the widest
	 * label, plus the internal margin.  Total length of the gauge is
	 * spacing * (nlabels-1).  To this, we add half the width of the
	 * left-most label and half the width of the right-most label
	 * to get the entire desired width of the widget.
	 */
	if (svgw->gauge.nlabels > 0)
	{
	  Dimension	lwm, lw0, lw1;	/* width of max, left, right labels */
	  Dimension	lh;

	  MaxLabel (svgw,&lwm,&lh, &lw0,&lw1);

	  if (svgw->gauge.orientation == XtorientHorizontal)
	  {
	    lwm = (lwm+hmargin) * (svgw->gauge.nlabels-1) + (lw0+lw1)/2;
	    w = Max (w, lwm);
	    h += lh + vmargin;
	  }
	  else
	  {
	    lh = lh*svgw->gauge.nlabels + (svgw->gauge.nlabels - 1)*vmargin;
	    h = Max (h, lh);
	    w += lwm + hmargin;
	  }
	}

	w += hmargin*2;
	h += vmargin*2;

	*wid = w;
	*hgt = h;
}



static void
AutoScale (SVGCanvasWidget svgw)
{
	static int scales[3] = {1,2,5};
	int sptr = 0, smult=1;

	if (svgw->gauge.autoScaleDown)
	  svgw->gauge.v1 = 0;
	while (svgw->gauge.value > svgw->gauge.v1)
	{
	  if (++sptr > 2) {
	    sptr = 0;
	    smult *= 10;
	  }
	  svgw->gauge.v1 = scales[sptr] * smult;
	}
}

static	void
EnableUpdate (SVGCanvasWidget svgw)
{
	svgw->gauge.intervalId =
	  XtAppAddTimeOut (XtWidgetToApplicationContext ((Widget) svgw),
	  	svgw->gauge.update * MS_PER_SEC, SVGCanvasGetValue,
		(XtPointer) svgw);
}

static	void
DisableUpdate (SVGCanvasWidget svgw)
{
	XtRemoveTimeOut (svgw->gauge.intervalId);
}

static	void
SVGCanvasGetValue (XtPointer    clientData,
	       XtIntervalId *UNUSED (intervalId))
{
	SVGCanvasWidget	svgw = (SVGCanvasWidget) clientData;
	Cardinal	value;

	if (svgw->gauge.update > 0)
	  EnableUpdate (svgw);

	if (svgw->gauge.getValue != NULL)
	{
	  XtCallCallbackList ((Widget) svgw, svgw->gauge.getValue, (XtPointer)&value);
	  XawSVGCanvasSetValue ((Widget) svgw, value);
	}
}


static	GC
Get_GC (SVGCanvasWidget svgw,
	Pixel       fg)
{
	XGCValues	values;
#define	vmask	GCForeground
#define	umask	(GCBackground|GCSubwindowMode|GCGraphicsExposures|GCDashOffset\
		|GCFont|GCDashList|GCArcMode)

	values.foreground = fg;

	return XtAllocateGC ((Widget) svgw, 0, vmask, &values, 0L, umask);
}
