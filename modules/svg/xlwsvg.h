/* Gauge Widget for XEmacs.
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
 * xlwsvg.h - SVG canvas widget
 *   by Stephen J. Turnbull <stephen@xemacs.org>
 * based on Gauge.h - Gauge widget
 *   by Edward A. Falk <falk@falconer.vip.best.com>, dated July 8, 1997
 *   adapted to XEmacs by Andy Piper <andy@xemacs.org> #### date?
 */

#ifndef _included_xlwsvg_h
#define _included_xlwsvg_h

/* For commenting out stuff I don't understand, or don't yet see a need
   for.  Eventually these should all be removed. */
#define YAGNI "You aren't gonna need it!"

/***********************************************************************\
*									*
* SVG Canvas Widget							*
*									*
* The SVG Canvas widget provides a window for rendering Scalable	*
* Vector Graphics.							*
*									*
************************************************************************/

#include ATHENA_Label_h_

/* Resources:

 Name			Class		RepType		Default Value
 ----			-----		-------		-------------
 svgSource		SVGSource	String		None

 encoding		Encoding	unsigned char	XawTextEncoding8bit
 font			Font		XFontStruct*	XtDefaultFont
 foreground		Foreground	Pixel		XtDefaultForeground
 internalHeight		Height		Dimension	2
 internalWidth		Width		Dimension	4
 resize			Resize		Boolean		True
 background		Background	Pixel		XtDefaultBackground
 bitmap			Pixmap		Pixmap		None
 border			BorderColor	Pixel		XtDefaultForeground
 borderWidth		BorderWidth	Dimension	1
 cursor			Cursor		Cursor		None
 cursorName		Cursor		String		NULL
 destroyCallback	Callback	XtCallbackList	NULL
 height			Height		Dimension	varies
 insensitiveBorder	Insensitive	Pixmap		Gray
 mappedWhenManaged	MappedWhenManaged Boolean		True
 pointerColor		Foreground	Pixel		XtDefaultForeground
 pointerColorBackground	Background	Pixel		XtDefaultBackground
 sensitive		Sensitive	Boolean		True
 width			Width		Dimension	text width
 x			Position	Position	0
 y			Position	Position	0
*/

/*
 * Resource names not provided in StringDefs.h
 */

#ifndef XtNsvgSource
#define XtNsvgSource	"svgSource"
#define XtCSVGSource	"SVGSource"
#endif

/* Class record constants */

extern WidgetClass svgCanvasWidgetClass;

typedef struct _SVGCanvasClassRec *SVGCanvasWidgetClass;
typedef struct _SVGCanvasRec      *SVGCanvasWidget;

#ifndef YAGNI
_XFUNCPROTOBEGIN

_XFUNCPROTOEND
#endif

#endif /* _included_xlwsvg_h */
