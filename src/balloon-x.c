/*
   Copyright (c) 1997 Douglas Keller

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


#include <config.h>
#include "lisp.h"

#include "device-impl.h"
#include "console-x-impl.h"

#include "balloon_help.h"

/* #### start of hack */

static unsigned long
alloc_color (Display* dpy, const char* colorname, int light)
{
  Colormap cmap = DEVICE_X_COLORMAP (XDEVICE (get_default_device (Qx)));
  unsigned long pixel = 0;
  XColor color;

  if (XParseColor(dpy, cmap, colorname, &color) && XAllocColor(dpy, cmap, &color))
    {
      pixel = color.pixel;
    }
  else
    {
      if (light)
	{
	  printf ("Warning: could not allocate color \"%s\", using \"white\"\n",
		  colorname);
	  pixel = alloc_color (dpy, "white", True);
	}
      else
	{
	  printf ("Warning: could not allocate color \"%s\", using \"black\"\n",
		  colorname);
	  pixel = alloc_color (dpy, "black", True);
	}
    }
  return pixel;
}

static XFontStruct *
open_font (Display* dpy, const char* font_name)
{
  XFontStruct* fontStruct = NULL;

  fontStruct = XLoadQueryFont (dpy, font_name ? font_name : "fixed");
  if (fontStruct == NULL)
    {
      printf ("Warning: could not load font \"%s\", using \"fixed\".\n", font_name);
      fontStruct = XLoadQueryFont (dpy, "fixed");
      assert (fontStruct != NULL);
    }
  return fontStruct;
}

static void
init (void)
{
  static int init_p = 0;

  if (!init_p)
    {
      Pixel fg, bg, shine, shadow;
      XFontStruct* font;
      Display *dpy = DEVICE_X_DISPLAY (XDEVICE (get_default_device (Qx)));

      fg = alloc_color (dpy, "grey60", 1);
      bg = alloc_color (dpy, "black", 0);

      shine  = alloc_color (dpy, "grey80", 1);
      shadow = alloc_color (dpy, "grey40", 0);

      font = open_font (dpy, "-adobe-helvetica-medium-r-normal--12-*");

      balloon_help_create (dpy, bg, fg, shine, shadow, font);
      init_p = 1;
    }
}

/* #### end of hack */

DEFUN ("show-balloon-help", Fshow_balloon_help, 1, 1, 0, /*
Show balloon help.
*/
       (string))
{
  char *p;
  CHECK_STRING (string);

  p = (char *) XSTRING_DATA (string);

  init ();

  balloon_help_show (p);

  return Qnil;
}

DEFUN ("hide-balloon-help", Fhide_balloon_help, 0, 0, 0, /*
Hide balloon help.
*/
      ())
{
  init ();

  balloon_help_hide ();

  return Qnil;
}

DEFUN ("balloon-help-move-to-pointer", Fballoon_help_move_to_pointer, 0, 0, 0, /*
Move the balloon help to the place where the pointer currently resides.
*/
      ())
{
  init ();

  balloon_help_move_to_pointer ();

  return Qnil;
}



/************************************************************************/
/*				initialization				*/
/************************************************************************/

void
syms_of_balloon_x (void)
{
  DEFSUBR (Fshow_balloon_help);
  DEFSUBR (Fhide_balloon_help);
  DEFSUBR (Fballoon_help_move_to_pointer);
}

void
vars_of_balloon_x (void)
{
  Fprovide (intern ("c-balloon-help"));
}
