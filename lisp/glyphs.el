;;; glyphs.el --- Lisp interface to C glyphs

;; Copyright (C) 1994, 1997 Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996, 2000, 2005 Ben Wing.

;; Author: Chuck Thompson <cthomp@cs.uiuc.edu>, Ben Wing <ben@xemacs.org>
;; Maintainer: XEmacs Development Team
;; Keywords: extensions, internal, dumped

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Synched up with: Not in FSF.

;;; Authorship:

;; Prototype created 1995 by Chuck Thompson.
;; Completely rewritten by Ben Wing, 1995.
;; Various cleanups (esp. doc strings) by Ben Wing, May 2000.

;;; Commentary:

;; This file is dumped with XEmacs.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; image specifiers

(defun make-image-specifier (spec-list)
  "Return a new `image' specifier object with the specification list SPEC-LIST.
SPEC-LIST can be a list of specifications (each of which is a cons of a
locale and a list of instantiators), a single instantiator, or a list
of instantiators.  See `make-specifier' for more information about
specifiers.

The main purpose of this doc string is to describe the possible formats for
image instantiators, as given as an argument to `make-glyph' or
`set-glyph-image'.

An image instantiator should be a string or a vector of the form

 [FORMAT :KEYWORD VALUE ...]

i.e. a format symbol followed by zero or more alternating keyword-value
pairs.  The vector form of an image instantiator explicitly specifies the
format of the image and other relevant properties.  The string form
specifies only a filename or gives inline data of an unspecified format,
and XEmacs must guess the actual format.  Once it has done this, it
internally converts the instantiator into the vector format.  This is
described in more detail below.

Following is a list of the possible values for FORMAT.  After each
description, the allowable keywords for the format are listed in brackets,
followed by the possible image instance types that can be generated from
this format. (Image instance types will be discussed below.)

`nothing'
   Don't display anything; no keywords are valid for this.
   [] (nothing)
`string'
   Display this image as a text string.  Support for instantiating as
   `mono-pixmap' and `color-pixmap' should probably be added.
   [:data] (text)
`formatted-string'
   Display this image as a text string, with replaceable fields.
   Not currently implemented -- it's treated like `string'.
   [:data] (text)
`gif'
   A GIF87 or GIF89 image; only if GIF support was compiled into this
   XEmacs.  NOTE: Only the first frame of animated gifs will be displayed.
   [:data, :file] (color-pixmap, pointer)
`jpeg'
   A JPEG image; only if JPEG support was compiled into this XEmacs.
   [:data, :file] (color-pixmap, pointer)
`png'
   A PNG image; only if PNG support was compiled into this XEmacs.
   [:data, :file] (color-pixmap, pointer)
`tiff'
   A TIFF image; only if TIFF support was compiled into this XEmacs.
   [:data, :file] (color-pixmap, pointer)
`bmp'
   A MS Windows BMP image; only if MS Windows support was compiled into
   this XEmacs.
   [:data, :file] (color-pixmap, pointer)
`xbm'
   An X bitmap; exists if any window-system support was compiled into this
   XEmacs.
   [:data, :file, :foreground, :background, :mask-data, :mask-file,
   :hotspot-x, :hotspot-y] (mono-pixmap, color-pixmap, pointer)
`xpm'
   An XPM pixmap; only if XPM support was compiled into this XEmacs.
   [:data, :file, :color-symbols] (mono-pixmap, color-pixmap, pointer)
`xface'
   An X-Face bitmap, used to encode people's faces in e-mail messages;
   only if X-Face support was compiled into this XEmacs.
   [:data, :file, :foreground, :background, :mask-data, :mask-file,
   :hotspot-x, :hotspot-y] (mono-pixmap, color-pixmap, pointer)
`cursor-font'
   X and GTK only.  One of the standard cursor-font names, such as \"watch\"
   or \"right_ptr\" under X.  Under X, this is, more specifically, any
   of the standard cursor names from appendix B of the Xlib manual
   [also known as the file <X11/cursorfont.h>] minus the XC_ prefix.
   On other window systems, the valid names will be specific to the
   type of window system.
   [:data, :foreground, :background] (pointer)
`mswindows-resource'
   An MS Windows pointer resource.  Specifies a resource to retrieve
   directly from the system (an OEM resource) or from a file, particularly
   an executable file.  If the resource is to be retrieved from a file, use
   :file and optionally :resource-id.  Otherwise use :resource-id.  Always
   specify :resource-type to specify the type (cursor, bitmap or icon) of
   the resource.  Possible values for :resource-id are listed below.
   [:file, :resource-type, :resource-id] (pointer, color-pixmap)
`font'
   A glyph from a font; i.e. the name of a font, and glyph index into it
   of the form \"FONT fontname index [[mask-font] mask-index]\".
   Currently can only be instantiated as `pointer', although this should
   probably be fixed.
   [:data, :foreground, :background] (pointer)
`subwindow'
   An embedded windowing system window.
   [:pixel-width, :pixel-height] (subwindow)
`button'
   A button widget; either a push button, radio button or toggle button.
   [WIDGET-KEYWORDS, GUI-KEYWORDS, :image] (widget)
`combo-box'
   A drop list of selectable items in a widget, for editing text.
   [GUI-KEYWORDS, :width, :height, :pixel-width, :face, :items] (widget)
`edit-field'
   A text editing widget.
   [WIDGET-KEYWORDS, GUI-KEYWORDS] (widget)
`label'
   A static, text-only, widget; for displaying text.
   [WIDGET-KEYWORDS, :descriptor] (widget)
`layout'
   A widget for controlling the positioning of children underneath it.
   Through the use of nested layouts, a widget hierarchy can be created
   which can have the appearance of any standard dialog box or similar
   arrangement; all of this is counted as one \"glyph\" and could appear
   in many of the places that expect a single glyph.
   [WIDGET-KEYWORDS, GUI-KEYWORDS, :orientation, :justify, :vertically-justify,
   :horizontally-justify, :border, :margin-width, :items] (widget)
`native-layout'
   The native version of a layout widget.  #### Document me better!
   [WIDGET-KEYWORDS, GUI-KEYWORDS] (widget)
`progress-gauge'
   A sliding widget, for showing progress.
   [WIDGET-KEYWORDS, GUI-KEYWORDS, :value] (widget)
`tab-control'
   A tab widget; a series of user selectable tabs.
   [WIDGET-KEYWORDS, GUI-KEYWORDS, :orientation, :items] (widget)
`tree-view'
   A folding widget.
   [WIDGET-KEYWORDS, GUI-KEYWORDS, :items] (widget)
`scrollbar'
   A scrollbar widget.
   [GUI-KEYWORDS, :pixel-width, :face, :items] (widget)
`autodetect'
   XEmacs tries to guess what format the data is in.  If X support
   exists, the data string will be checked to see if it names a filename.
   If so, and this filename contains XBM or XPM data, the appropriate
   sort of pixmap or pointer will be created. [This includes picking up
   any specified hotspot or associated mask file.] Otherwise, if `pointer'
   is one of the allowable image-instance types and the string names a
   valid cursor-font name, the image will be created as a pointer.
   Otherwise, the image will be displayed as text.  If no X support
   exists, the image will always be displayed as text.
   [:data] (mono-pixmap, color-pixmap, pointer, text)
`inherit'
   Inherit from the background-pixmap property of a face.
   [:face] (mono-pixmap)

The valid keywords are:

:data
   Inline data.  For most formats above, this should be a string.  For
   XBM images, this should be a list of three elements: width, height, and
   a string of bit data.  This keyword is valid for all of the bitmap/pixmap
   formats, as well as `string', `formatted-string', `font', `cursor-font',
   and `autodetect'.
:file
   Data is contained in a file.  The value is the name of this file.
   If both :data and :file are specified, the image is created from
   what is specified in :data and the string in :file becomes the
   value of the `image-instance-file-name' function when applied to
   the resulting image-instance.  This keyword is valid for all of the
   bitmap/pixmap formats as well as `mswindows-resource'.
:foreground
:background
   For `xbm', `xface', `cursor-font', `widget' and `font'.  These keywords
   allow you to explicitly specify foreground and background colors.
   The argument should be anything acceptable to `make-color-instance'.
   This will cause what would be a `mono-pixmap' to instead be colorized
   as a two-color color-pixmap, and specifies the foreground and/or
   background colors for a pointer instead of black and white.
:mask-data
   For `xbm' and `xface'.  This specifies a mask to be used with the
   bitmap.  The format is a list of width, height, and bits, like for
   :data.
:mask-file
   For `xbm' and `xface'.  This specifies a file containing the mask data.
   If neither a mask file nor inline mask data is given for an XBM image,
   and the XBM image comes from a file, XEmacs will look for a mask file
   with the same name as the image file but with \"Mask\" or \"msk\"
   appended.  For example, if you specify the XBM file \"left_ptr\"
   [usually located in \"/usr/include/X11/bitmaps\"], the associated
   mask file \"left_ptrmsk\" will automatically be picked up.
:hotspot-x
:hotspot-y
   For `xbm' and `xface'.  These keywords specify a hotspot if the image
   is instantiated as a `pointer'.  Note that if the XBM image file
   specifies a hotspot, it will automatically be picked up if no
   explicit hotspot is given.
:color-symbols
   Only for `xpm'.  This specifies an alist that maps strings
   that specify symbolic color names to the actual color to be used
   for that symbolic color (in the form of a string or a color-specifier
   object).  If this is not specified, the contents of `xpm-color-symbols'
   are used to generate the alist.
:resource-id
   Only for `mswindows-resource'.  This must be either an integer (which
   directly specifies a resource number) or a string.  Valid strings are

   -- For bitmaps:

   \"close\", \"uparrow\", \"dnarrow\", \"rgarrow\", \"lfarrow\",
   \"reduce\", \"zoom\", \"restore\", \"reduced\", \"zoomd\",
   \"restored\", \"uparrowd\", \"dnarrowd\", \"rgarrowd\", \"lfarrowd\",
   \"mnarrow\", \"combo\", \"uparrowi\", \"dnarrowi\", \"rgarrowi\",
   \"lfarrowi\", \"size\", \"btsize\", \"check\", \"checkboxes\", and
   \"btncorners\".

   -- For pointers:

   \"normal\", \"ibeam\", \"wait\", \"cross\", \"up\", \"sizenwse\",
   \"sizenesw\", \"sizewe\", \"sizens\", \"sizeall\", and \"no\".

   -- For icons:

   \"sample\", \"hand\", \"ques\", \"bang\", \"note\", and \"winlogo\".
:resource-type
   Only for `mswindows-resource'.  This must be a symbol, either `cursor'
   (i.e. pointer), `icon', or `bitmap', specifying the type of resource to
   be retrieved.
:face
   Only for `inherit'.  This specifies the face to inherit from.
   For widgets this also specifies the face to use for display. It defaults
   to gui-element-face.
:pixel-width, :pixel-height
   Width and height of element, in pixels.  For `subwindow', the values
   must be integers.  For widgets, the values can be integers or
   expressions that evaluate to integers.

\[WIDGET-KEYWORDS] stands for the standard keywords accepted by widgets:
These are `:selected', `:active', `:suffix', `:keys', `:style',
`:filter', `:config', `:included', `:key-sequence', `:accelerator',
`:label', `:callback', `:initial-focus', and `:descriptor'.
#### Document me.

\[GUI-KEYWORDS] stands for keywords accepted by many widgets.
These are `:width', `:height', `:pixel-width', `:pixel-height', and `:face'.
#### Document me.

If instead of a vector, the instantiator is a string, it will be
converted into a vector by looking it up according to the specs in the
`console-type-image-conversion-list' (q.v.) for the console type of
the domain (usually a window; sometimes a frame or device) over which
the image is being instantiated.

If the instantiator specifies data from a file, the data will be read
in at the time that the instantiator is added to the image (which may
be well before when the image is actually displayed), and the
instantiator will be converted into one of the inline-data forms, with
the filename retained using a :file keyword.  This implies that the
file must exist when the instantiator is added to the image, but does
not need to exist at any other time (e.g. it may safely be a temporary
file).

NOTE: In practice, you rarely, if ever, need to actually
create an image specifier! (The function `make-image-specifier' exists
mainly for completeness.) Pretty much the only use for image specifiers is
to control how glyphs are displayed, and the image specifier associated
with a glyph (the `image' property of a glyph) is created automatically
when a glyph is created (see `make-glyph') and need not \(and cannot, for
that matter) ever be changed.  In fact, the design decision to create a
separate image specifier type, rather than make glyphs themselves be
specifiers, is debatable -- the other properties of glyphs are rarely used
and could conceivably have been incorporated into the glyph's instantiator.
The rarely used glyph types (buffer, pointer, icon) could also have been
incorporated into the instantiator.

An image specifier is used for images (pixmaps, widgets and the like).  It
is used to describe the actual image in a glyph.  It is instantiated \(see
`specifier-instance') as an image-instance.  Note that \"image\" as used in
XEmacs does not actually refer to what the term \"image\" normally means (a
picture, e.g. in .GIF or .JPG format, and called a \"pixmap\" in XEmacs),
but includes all types of graphical elements, including pixmaps, widgets
\(buttons, sliders, text fields, etc.) and even strings of text.

There is an important distinction to be made between image instantiators
and image instances, and \"image instantiator formats\" and \"image
instance types\", analogous to the distinction between source and
destination.  An image instantiator describes the source data for an image.
An image instance encapsulates the resulting window-system object used to
display the image.  Image instantiator formats are the formats of the
source: This includes familiar and less-familiar graphics formats such as
`gif', `jpeg', `png' and `xpm'; widget types such as `button', `edit-field'
and `combo-box'; and other beasts such as `string' (plain text, which could
potentially behave like text when placed in a buffer, such as wrapping),
`font' (a single character from a particular font, specified by the index
into the font), etc.  Image instance types are the (destination) types of
the resulting image instance.  Different image instance types correspond to
fundamentally different appearance and behaviors for the resulting image,
specifically:

-- `color-pixmap' (a color image);
-- `mono-pixmap' (a \"monochrome\" image, technically a two-color image
   that comes in two unspecified shades \"foreground\" and \"background\",
   determined from the face [see `make-face'] of the glyph or surrounding
   text);
-- `text' (a string of text appearing somewhere in a buffer's text or
   margins, which has an unspecified foreground, background, and font
   derived from the surrounding text or other external property and which
   behaves in many respects like an image but can wrap across the end of a
   line to the beginning of the next);
-- `pointer' (the mouse pointer for a window; this is a combination of a
   rectangular pixmap image, a monochrome mask that specifies the
   transparency of the image [i.e. in which places the underlying screen
   image can show through, and how much of it], and a \"hotspot\" that
   indicates which pixel in the pointer's image is considered the actual
   pointer location -- for example, this will be located near the tip of
   an arrow, in the middle of a crosshairs, somewhere along an i-beam, etc.);
-- `widget' (a window-system object or \"widget\" that interacts with the
   user, such as a button, edit-field or combo-box);
-- `subwindow' (a rectangular area that another program can draw into);
-- `nothing' (no display).

There is not a one-to-one mapping between source (image instantiator)
formats and destination (image instance) types.  For example, the source
format `xpm' can generate the image instance types `color-pixmap',
`mono-pixmap', or `pointer', and the image instance type `color-pixmap' can
be generated by any of `gif', `jpeg', `png', `tiff', `xpm', `xbm' and
`xface'.

In general, the user or programmer specifies the image instantiator format,
while the appropriate image instance type is determined automatically by
XEmacs from the image instantiator format, from the data in the
instantiator and from the particular situation the image (and the glyph
that holds it) is being used in. (However, it's possible to explicitly
create image instances and control their types; see `make-image-instance'.)
For example, a glyph used to specify the shape of a mouse pointer can only
result in `pointer'-type image instances, and a glyph used for an icon can
only result in `color-pixmap' image instances.  A glyph used in a buffer
can potentially result in any image instance type except for `pointer', but
particular instantiator formats have only a limited set of image instance
types they will support.  Here is an example of how the image instance type
for an `xpm' instantiator (which can potentially support `color-pixmap',
`mono-pixmap', or `pointer') is determined:

1. If the glyph is being used for a mouse pointer (hence its `glyph-type'
   is `pointer'), it can be instantiated only a `pointer'-type image instance.
2. If the glyph is being used for an icon (hence its `glyph-type' is `icon'),
   it can be instantiated only a `color-pixmap'-type image instance.
3. Otherwise, the glyph is being used somewhere inside a frame (`glyph-type'
   of `buffer') and any image instance type except `pointer' can be
   supported.  In this case, this means `color-pixmap' or `mono-pixmap'.
   Which one will result depends on the particular data being processed,
   since XPM images can specify whether they are color or mono.

Note again that \"mono\" does *NOT* simply mean \"an image with two
colors\".  The latter image has two prespecified colors, e.g. red and blue
or black and white, and will always appear with those colors, no matter
what the context.  A mono image has two *unspecified* colors, symbolically
named \"foreground\" and \"background\", and the actual values for those
colors depends on context.  A mono pixmap displayed among text will take
its foreground and background from that of the text and hence blend in
nicely; a two-color color pixmap won't do that.

Note also that `color-pixmap' image instances can be generated from the
supported pixmap formats that are inherently mono (i.e. `xbm' and `xface')
by specifying :foreground and :background values.

A table of the various image instantiator formats and the possible
destination (image instance) types that can be generated from them is as
follows:


                   color-pixmap mono-pixmap text pointer widget subwindow noth.
-------------------------------------------------------------------------------
nothing                                                                     +
string                                        +
formatted-string                              +
xbm                     +            +              +
xpm                     +            +              +
xface                   +            +              +
gif                     +                           +
jpeg                    +                           +
png                     +                           +
tiff                    +                           +
bmp                     +                           +
cursor-font                                         +
mswindows-resource      +                           +
font                                                +
subwindow                                                           +
button                                                     +
combo-box                                                  +
edit-field                                                 +
label                                                      +
layout                                                     +
native-layout                                              +
progress-gauge                                             +
tab-control                                                +
tree-view                                                  +
scrollbar                                                  +
autodetect              +            +        +     +
inherit                              +

See `make-image-instance' for a more detailed discussion of image
instance types."
  (make-specifier-and-init 'image spec-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; glyphs

(defconst built-in-glyph-specifiers
  '(image contrib-p baseline)
  "A list of the built-in glyph properties that are specifiers.")

(defun glyph-property (glyph property &optional locale)
  "Return GLYPH's value of PROPERTY in LOCALE.

If LOCALE is omitted, the GLYPH's actual value for PROPERTY will be
  returned.  For built-in properties, this will be a specifier object
  of a type appropriate to the property (e.g. a font or color
  specifier).  For other properties, this could be anything.

If LOCALE is supplied, then instead of returning the actual value,
  the specification(s) for the given locale or locale type will
  be returned.  This will only work if the actual value of
  PROPERTY is a specifier (this will always be the case for built-in
  properties, but not or not may apply to user-defined properties).
  If the actual value of PROPERTY is not a specifier, this value
  will simply be returned regardless of LOCALE.

The return value will be a list of instantiators (e.g. strings
  specifying a font or color name), or a list of specifications, each
  of which is a cons of a locale and a list of instantiators.
  Specifically, if LOCALE is a particular locale (a buffer, window,
  frame, device, or `global'), a list of instantiators for that locale
  will be returned.  Otherwise, if LOCALE is a locale type (one of
  the symbols `buffer', `window', `frame', `device', `device-class', or
  `device-type'), the specifications for all locales of that type will
  be returned.  Finally, if LOCALE is `all', the specifications for all
  locales of all types will be returned.

The specifications in a specifier determine what the value of
  PROPERTY will be in a particular \"domain\" or set of circumstances,
  which is typically a particular Emacs window along with the buffer
  it contains and the frame and device it lies within.  The value
  is derived from the instantiator associated with the most specific
  locale (in the order buffer, window, frame, device, and `global')
  that matches the domain in question.  In other words, given a domain
  (i.e. an Emacs window, usually), the specifier for PROPERTY will first
  be searched for a specification whose locale is the buffer contained
  within that window; then for a specification whose locale is the window
  itself; then for a specification whose locale is the frame that the
  window is contained within; etc.  The first instantiator that is
  valid for the domain (usually this means that the instantiator is
  recognized by the device [i.e. the X server or TTY device] that the
  domain is on.  The function `glyph-property-instance' actually does
  all this, and is used to determine how to display the glyph.

See `set-glyph-property' for the built-in property-names."
  (check-argument-type 'glyphp glyph)
  (let ((value (get glyph property)))
    (if (and locale
	     (or (memq property built-in-glyph-specifiers)
		 (specifierp value)))
	(setq value (specifier-specs value locale)))
    value))

(defun convert-glyph-property-into-specifier (glyph property)
  "Convert PROPERTY on GLYPH into a specifier, if it's not already."
  (check-argument-type 'glyphp glyph)
  (let ((specifier (get glyph property)))
    ;; if a user-property does not have a specifier but a
    ;; locale was specified, put a specifier there.
    ;; If there was already a value there, convert it to a
    ;; specifier with the value as its `global' instantiator.
    (if (not (specifierp specifier))
	(let ((new-specifier (make-specifier 'generic)))
	  (if (or (not (null specifier))
		  ;; make sure the nil returned from `get' wasn't
		  ;; actually the value of the property
		  (null (get glyph property t)))
	      (add-spec-to-specifier new-specifier specifier))
	  (setq specifier new-specifier)
	  (put glyph property specifier)))))

(defun glyph-property-instance (glyph property
				      &optional domain default no-fallback)
  "Return the instance of GLYPH's PROPERTY in the specified DOMAIN.

Under most circumstances, DOMAIN will be a particular window,
  and the returned instance describes how the specified property
  actually is displayed for that window and the particular buffer
  in it.  Note that this may not be the same as how the property
  appears when the buffer is displayed in a different window or
  frame, or how the property appears in the same window if you
  switch to another buffer in that window; and in those cases,
  the returned instance would be different.

DOMAIN defaults to the selected window if omitted.

DOMAIN can be a frame or device, instead of a window.  The value
  returned for a such a domain is used in special circumstances
  when a more specific domain does not apply; for example, a frame
  value might be used for coloring a toolbar, which is conceptually
  attached to a frame rather than a particular window.  The value
  is also useful in determining what the value would be for a
  particular window within the frame or device, if it is not
  overridden by a more specific specification.

If PROPERTY does not name a built-in property, its value will
  simply be returned unless it is a specifier object, in which case
  it will be instanced using `specifier-instance'.

Optional arguments DEFAULT and NO-FALLBACK are the same as in
  `specifier-instance'."
  (check-argument-type 'glyphp glyph)
  (let ((value (get glyph property)))
    (if (specifierp value)
	(setq value (specifier-instance value domain default no-fallback)))
    value))

(defun set-glyph-property (glyph property value &optional locale tag-set
				 how-to-add)
  "Change a property of a GLYPH.

NOTE: If you want to remove a property from a glyph, use
  `remove-glyph-property' rather than attempting to set a value of nil
   for the property.

For built-in properties, the actual value of the property is a
  specifier and you cannot change this; but you can change the
  specifications within the specifier, and that is what this function
  will do.  For user-defined properties, you can use this function
  to either change the actual value of the property or, if this value
  is a specifier, change the specifications within it.

If PROPERTY is a built-in property, the specifications to be added to
  this property can be supplied in many different ways:

  -- If VALUE is a simple instantiator (e.g. a string naming a font or
     color) or a list of instantiators, then the instantiator(s) will
     be added as a specification of the property for the given LOCALE
     (which defaults to `global' if omitted).
  -- If VALUE is a list of specifications (each of which is a cons of
     a locale and a list of instantiators), then LOCALE must be nil
     (it does not make sense to explicitly specify a locale in this
     case), and specifications will be added as given.
  -- If VALUE is a specifier (as would be returned by `glyph-property'
     if no LOCALE argument is given), then some or all of the
     specifications in the specifier will be added to the property.
     In this case, the function is really equivalent to
     `copy-specifier' and LOCALE has the same semantics (if it is
     a particular locale, the specification for the locale will be
     copied; if a locale type, specifications for all locales of
     that type will be copied; if nil or `all', then all
     specifications will be copied).

HOW-TO-ADD should be either nil or one of the symbols `prepend',
  `append', `remove-tag-set-prepend', `remove-tag-set-append', `remove-locale',
  `remove-locale-type', or `remove-all'.  See `copy-specifier' and
  `add-spec-to-specifier' for a description of what each of
  these means.  Most of the time, you do not need to worry about
  this argument; the default behavior usually is fine.

In general, it is OK to pass an instance object (e.g. as returned
  by `glyph-property-instance') as an instantiator in place of
  an actual instantiator.  In such a case, the instantiator used
  to create that instance object will be used (for example, if
  you set a font-instance object as the value of the `font'
  property, then the font name used to create that object will
  be used instead).  If some cases, however, doing this
  conversion does not make sense, and this will be noted in
  the documentation for particular types of instance objects.

If PROPERTY is not a built-in property, then this function will
  simply set its value if LOCALE is nil.  However, if LOCALE is
  given, then this function will attempt to add VALUE as the
  instantiator for the given LOCALE, using `add-spec-to-specifier'.
  If the value of the property is not a specifier, it will
  automatically be converted into a `generic' specifier.


The following symbols have predefined meanings:

 image		The image used to display the glyph.

 baseline	Percent above baseline that glyph is to be
		displayed.

 contrib-p	Whether the glyph contributes to the
		height of the line it's on.

 face		Face of this glyph (*not* a specifier)."
  (check-argument-type 'glyphp glyph)
  (if (memq property built-in-glyph-specifiers)
      (set-specifier (get glyph property) value locale tag-set how-to-add)

    ;; This section adds user defined properties.
    (if (not locale)
	(put glyph property value)
      (convert-glyph-property-into-specifier glyph property)
      (add-spec-to-specifier (get glyph property) value locale tag-set
			     how-to-add)))
  value)

(defun remove-glyph-property (glyph property &optional locale tag-set exact-p)
  "Remove a property from a glyph.
For built-in properties, this is analogous to `remove-specifier'.
See `remove-specifier' for the meaning of the LOCALE, TAG-SET, and EXACT-P
  arguments."
  (or locale (setq locale 'all))
  (if (memq property built-in-glyph-specifiers)
      (remove-specifier (glyph-property glyph property) locale tag-set exact-p)
    (if (eq locale 'all)
	(remprop glyph property)
      (convert-glyph-property-into-specifier glyph property)
      (remove-specifier (glyph-property glyph property) locale tag-set
			exact-p))))

(defun glyph-face (glyph)
  "Return the face of GLYPH."
  (glyph-property glyph 'face))

(defun set-glyph-face (glyph face)
  "Change the face of GLYPH to FACE."
;  (interactive (glyph-interactive "face"))
  (set-glyph-property glyph 'face face))

(defun glyph-image (glyph &optional locale)
  "Return the image of GLYPH in LOCALE, or nil if it is unspecified.

LOCALE may be a locale (the instantiators for that particular locale
  will be returned), a locale type (the specifications for all locales
  of that type will be returned), `all' (all specifications will be
  returned), or nil (the actual specifier object will be returned).

See `glyph-property' for more information."
  (glyph-property glyph 'image locale))

(defun glyph-image-instance (glyph &optional domain default no-fallback)
  "Return the instance of GLYPH's image in DOMAIN.

Normally DOMAIN will be a window or nil (meaning the selected window),
  and an instance object describing how the image appears in that
  particular window and buffer will be returned.

See `glyph-property-instance' for more information."
  (glyph-property-instance glyph 'image domain default no-fallback))

(defun glyph-image-property (glyph prop &optional domain default no-fallback)
  "Return property PROP of the instance of GLYPH's image in DOMAIN.

Normally DOMAIN will be a window or nil (meaning the selected window).
The value returned is dependent on the image instance type."
  (image-instance-property
   (glyph-image-instance glyph domain default no-fallback) prop))

(defun set-glyph-image (glyph spec &optional locale tag-set how-to-add)
  "Change the image of GLYPH in LOCALE.

SPEC should be an instantiator (a string or vector; see
  `make-image-specifier' for a description of possible values here),
  a list of (possibly tagged) instantiators, an alist of specifications
  (each mapping a locale to an instantiator list), or an image specifier
  object.

If SPEC is an alist, LOCALE must be omitted.  If SPEC is a
  specifier object, LOCALE can be a locale, a locale type, `all',
  or nil; see `copy-specifier' for its semantics.  Otherwise LOCALE
  specifies the locale under which the specified instantiator(s)
  will be added, and defaults to `global'.

See `set-glyph-property' for more information."
  ; (interactive (glyph-interactive "image"))
  (set-glyph-property glyph 'image spec locale tag-set how-to-add))

(defun glyph-contrib-p (glyph &optional locale)
  "Return whether GLYPH contributes to its line height.

LOCALE may be a locale (the instantiators for that particular locale
  will be returned), a locale type (the specifications for all locales
  of that type will be returned), `all' (all specifications will be
  returned), or nil (the actual specifier object will be returned).

See `glyph-property' for more information."
  (glyph-property glyph 'contrib-p locale))

(defun glyph-contrib-p-instance (glyph &optional domain default no-fallback)
  "Return the instance of GLYPH's `contrib-p' property in DOMAIN.

Normally DOMAIN will be a window or nil (meaning the selected window),
  and an instance object describing what the `contrib-p' property is in
  that particular window and buffer will be returned.

See `glyph-property-instance' for more information."
  (glyph-property-instance glyph 'contrib-p domain default no-fallback))

(defun set-glyph-contrib-p (glyph spec &optional locale tag-set how-to-add)
  "Change the contrib-p property of GLYPH in LOCALE.

SPEC should be an instantiator (t or nil), a list of (possibly
  tagged) instantiators, an alist of specifications (each mapping a
  locale to an instantiator list), or a boolean specifier object.

If SPEC is an alist, LOCALE must be omitted.  If SPEC is a
  specifier object, LOCALE can be a locale, a locale type, `all',
  or nil; see `copy-specifier' for its semantics.  Otherwise LOCALE
  specifies the locale under which the specified instantiator(s)
  will be added, and defaults to `global'.

See `set-glyph-property' for more information."
  ; (interactive (glyph-interactive "contrib-p"))
  (set-glyph-property glyph 'contrib-p spec locale tag-set how-to-add))

(defun glyph-baseline (glyph &optional locale)
  "Return the baseline of GLYPH in LOCALE, or nil if it is unspecified.

LOCALE may be a locale (the instantiators for that particular locale
  will be returned), a locale type (the specifications for all locales
  of that type will be returned), `all' (all specifications will be
  returned), or nil (the actual specifier object will be returned).

See `glyph-property' for more information."
  (glyph-property glyph 'baseline locale))

(defun glyph-baseline-instance (glyph &optional domain default no-fallback)
  "Return the instance of GLYPH's baseline in DOMAIN.

Normally DOMAIN will be a window or nil (meaning the selected window),
  and an integer or nil (specifying the baseline in that particular
  window and buffer) will be returned.

See `glyph-property-instance' for more information."
  (glyph-property-instance glyph 'baseline domain default no-fallback))

(defun set-glyph-baseline (glyph spec &optional locale tag-set how-to-add)
  "Change the baseline of GLYPH to SPEC in LOCALE.

SPEC should be an instantiator (an integer [a percentage above the
  baseline of the line the glyph is on] or nil), a list of (possibly
  tagged) instantiators, an alist of specifications (each mapping a
  locale to an instantiator list), or a generic specifier object.

If SPEC is an alist, LOCALE must be omitted.  If SPEC is a
  specifier object, LOCALE can be a locale, a locale type, `all',
  or nil; see `copy-specifier' for its semantics.  Otherwise LOCALE
  specifies the locale under which the specified instantiator(s)
  will be added, and defaults to `global'.

See `set-glyph-property' for more information."
  ; (interactive (glyph-interactive "baseline"))
  (set-glyph-property glyph 'baseline spec locale tag-set how-to-add))

(defun make-glyph (&optional spec-list type)
  "Create a new glyph of type TYPE.

A glyph in XEmacs does NOT refer to a single unit of textual display (the
XEmacs term for this is \"rune\"), but rather is an object encapsulating
a graphical element, such as an image or widget (an element such as a
button or text field; \"widget\" is the term for this under X Windows,
and it's called a \"control\" under MS Windows).  This graphical element
could appear in a buffer, a margin, a gutter, or a toolbar, or as a mouse
pointer or an icon, for example.

Creating a glyph using `make-glyph' does not specify *where* the glyph
will be used, but it does specify *what* the glyph will look like.  In
particular, SPEC-LIST is used to specify this, and it's used to
initialize the glyph's `image' property, which is an image
specifier. (Note that \"image\" as used in the context of a glyph's
`image' property or in the terms \"image specifier\", \"image
instantiator\", or \"image instance\" does not refer to what people
normally think of as an image (which in XEmacs is called a
\"pixmap\"), but to any graphical element -- a pixmap, a widget, or
even a block of text, when used in the places that call for a glyph.)

SPEC-LIST is typically an image instantiator, describing the source for the
image data.  This is either a vector of the form [FORMAT :KEYWORD DATA ...],
for example

  [jpeg :file \"/user/john/images/myimage.jpg\"]

or

  [xbm :data \"/* XPM */\nstatic char * copy[] = {\n...\"]

or it is a string, either giving a file name or directly specifying inline
data.  See `make-image-specifier' for a detailed description of valid image
instantiators.  If the instantiator is a string, XEmacs will convert it
into vector form by trying to guess whether a file name or inline data is
intended, and what kind of data is inline or in the file.  Usually it does
a pretty good job.  See `console-type-image-conversion-list' for details of
how this works.

If the instantiator specifies data from a file, the data will be read in
when `make-glyph' is called and substituted inline into the instantiator,
using the :data keyword.  This means that the file must exist when the
glyph is created, but does not need to exist afterwards (e.g. it may safely
be a temporary file).

When errors occur in the process of reading image data from a file
\(e.g. the file does not exist or the data is of the wrong format or
corrupted), no Lisp error will currently be signalled.  Instead, the
instantiator is skipped and warnings will be issued at level `debug'. \(A
glyph with no instantiators in it cannot be displayed.) Normally, such
warnings are ignored entirely, but you can change this by setting
`log-warning-minimum-level'.  This is useful if you're trying to debug why
particular instantiators are not being processed. (#### We should probably
provide a way of getting errors in such circumstances, or even make this
the default behavior.)

Technically, SPEC-LIST can also be a list of image instantiators (each one
in turn is tried until an image is successfully produced), a cons of a
locale (frame, buffer, etc.) and an instantiator, a list of such conses,
or any other form accepted by `canonicalize-spec-list'.

If you're not familiar with specifiers, you should be in order to
understand how glyphs work.  The clearest introduction to specifiers
is in the Lispref manual, available under Info. (Choose
Help->Info->Info Contents on the menubar or type \\[info].) You can
also see `make-specifier' for a capsule summary.  What's important to
keep in mind is that a specifier lets you set a different value for
any particular buffer, window, frame, device, or console.  This allows
for a great deal of flexibility; in particular, only one global glyph
needs to exist for a particular purpose (e.g. the icon used to represent
an iconified frame, the mouse pointer used over particular areas of a
frame, etc.), and in these cases you do not create your own glyph, but
rather modify the existing one.

As well as using SPEC-LIST to initialize the glyph, you can set
specifications using `set-glyph-image'.  Note that, due to a possibly
questionable historical design decision, a glyph itself is not
actually a specifier, but rather is an object containing an image
specifier (as well as other, seldom-used properties).  Therefore, you
cannot set or access specifications for the glyph's image by directly
using `set-specifier', `specifier-instance' or the like on the glyph;
instead use them on `(glyph-image GLYPH)' or use the convenience
functions `set-glyph-image', `glyph-image-instance', and
`glyph-image'.

Once you have created a glyph, you specify where it will be used as follows:

-- To insert a glyph into a buffer, create an extent in the buffer and then
   use `set-extent-begin-glyph' or `set-extent-end-glyph' to set a glyph
   to be displayed at the corresponding edge of the extent. (It is common
   to create zero-width extents for this purpose.)

-- To insert a glyph into the left or right margin of a buffer, first
   make sure the margin is visible by setting a value for the specifiers
   `left-margin-width' or `right-margin-width'. (Not strictly necessary
   when using margin glyphs with layout policy `whitespace'.) Then follow
   the same procedure above for inserting a glyph in a buffer, and then
   set a non-default layout policy for the glyph using
   `set-extent-begin-glyph-layout' or `set-extent-end-glyph-layout'.
   Alternatively, use the high-level annotations API (see
   `make-annotation'). (In point of fact, you can also use the annotations
   API for glyphs in a buffer, by setting a layout policy of `text'.)

-- To insert a glyph into the modeline, just put the glyph directly as
   one of the modeline elements. (Unfortunately you can't currently
   put a begin glyph or end glyph on one of the modeline extents --
   they're ignored.)

-- To insert a glyph into a toolbar, specify it as part of a toolbar
   instantiator (typically set on the specifier `default-toolbar').
   See `default-toolbar' for more information. (Note that it is standard
   practice to use a symbol in place of the glyph list in the toolbar
   instantiator; the symbol is evalled to get the glyph list.  This
   facilitates both creating the toolbar instantiator and modifying
   individual glyphs in a toolbar later on.  For example, you can
   change the way that the Mail toolbar button looks by modifying the
   value of the variable `toolbar-mail-icon' (in general, `toolbar-*-icon')
   and then calling `(set-specifier-dirty-flag default-toolbar)'.
   (#### Unfortunately this doesn't quite work the way it should; the
   change will appear in new frames, but not existing ones.)

-- To insert a glyph into a gutter, create or modify a gutter instantiator
   (typically set on the specifier `default-gutter').  Gutter instantiators
   consist of strings or lists of strings, so to insert a glyph, create an
   extent over the string, and use `set-extent-begin-glyph' or
   `set-extent-end-glyph' to set a glyph to be displayed at the corresponding
   edge of the extent, just like for glyphs in a buffer.

-- To use a glyph as the icon for a frame, you do not actually create a new
   glyph; rather, you change the specifications for the existing glyph
   `frame-icon-glyph'. (Remember that, because of the specifier nature of
   glyphs, you can set different values for any particular buffer or frame.)

-- To use a glyph as the mouse pointer, in general you do not create a
   new glyph, but rather you change the specifications of various existing
   glyphs, such as `text-pointer-glyph' for the pointer used over text,
   `modeline-pointer-glyph' for the pointer used over the modeline, etc.
   Do an apropos over `*-pointer-glyph' to find all of them. (Note also
   that you can temporarily set the mouse pointer to some specific shape
   by using `set-frame-pointer', which takes an image instance, as obtained
   from calling `glyph-image-instance' on a glyph of type `pointer' --
   either one of the above-mentioned variables or one you created yourself.
   (See below for what it means to create a glyph of type `pointer'.)
   This pointer will last only until the next mouse motion event is
   processed or certain other things happen, such as creating or deleting
   a window. (In fact, the above-mentioned pointer glyph variables are
   implemented as part of the default handler for mouse motion events.
   If you want to customize this behavior, take a look at `mode-motion-hook',
   or `mouse-motion-handler' if you really want to get low-level.)

-- To use a glyph to control the shape of miscellaneous redisplay effects
   such as the truncation and continuation markers, set the appropriate
   existing glyph variables, as for icons and pointers above.  See
   `continuation-glyph', `control-arrow-glyph', `hscroll-glyph',
   `invisible-text-glyph', `octal-escape-glyph', and `truncation-glyph'.
   See also `overlay-arrow-string', an odd redisplay leftover which can
   be set to a glyph you created, and will cause the glyph to be displayed
   on top of the text position specified in the marker stored in
   `overlay-arrow-position'.

-- To use a glyph in a display table (i.e. to control the appearance of
   any individual character), create the appropriate character glyphs
   and then set a specification for the specifier `current-display-table',
   which controls the appearance of characters.  You can also set an
   overriding display table for use with text displayed in a particular
   face; see `set-face-display-table' and `make-display-table'.

-- To use a glyph as the background pixmap of a face: Note that the
   background pixmap of a face is actually an image specifier -- probably
   the only place in XEmacs where an image specifier occurs outside of
   a glyph.  Similarly to how the glyph's image specifier works, you
   don't create your own image specifier, but rather add specifications
   to the existing one (using `set-face-background-pixmap').  Note that
   the image instance that is generated in order to actually display the
   background pixmap is of type `mono-pixmap', meaning that it's a two-color
   image and the foreground and background of the image get filled in with
   the corresponding colors from the face.

It is extremely rare that you will ever have to specify a value for TYPE,
which should be one of `buffer' (used for glyphs in an extent, the modeline,
the toolbar, or elsewhere in a buffer), `pointer' (used for the mouse-pointer),
or `icon' (used for a frame's icon), and defaults to `buffer'.  The only cases
where it needs to be specified is when creating icon or pointer glyphs, and
in both cases the necessary glyphs have already been created at startup and
are accessed through the appropriate variables, e.g. `text-pointer-glyph'
(or in general, `*-pointer-glyph') and `frame-icon-glyph'."
  (let ((glyph (make-glyph-internal type)))
    (and spec-list (set-glyph-image glyph spec-list))
    glyph))

(defun buffer-glyph-p (object)
  "Return t if OBJECT is a glyph of type `buffer'."
  (and (glyphp object) (eq 'buffer (glyph-type object))))

(defun pointer-glyph-p (object)
  "Return t if OBJECT is a glyph of type `pointer'."
  (and (glyphp object) (eq 'pointer (glyph-type object))))

(defun icon-glyph-p (object)
  "Return t if OBJECT is a glyph of type `icon'."
  (and (glyphp object) (eq 'icon (glyph-type object))))

(defun make-pointer-glyph (&optional spec-list)
  "Return a new `pointer-glyph' object with the specification list SPEC-LIST.
This is equivalent to calling `make-glyph', specifying a type of `pointer'.
See `make-glyph' for more information.

It is extremely unlikely that you will ever need to create a pointer glyph.
Instead, you probably want to be calling `set-glyph-image' on an existing
glyph, e.g. `text-pointer-glyph'."
  (make-glyph spec-list 'pointer))

(defun make-icon-glyph (&optional spec-list)
  "Return a new `icon-glyph' object with the specification list SPEC-LIST.
This is equivalent to calling `make-glyph', specifying a type of `icon'.
See `make-glyph' for more information.

It is extremely unlikely that you will ever need to create a icon glyph.
Instead, you probably want to be calling `set-glyph-image' on
`frame-icon-glyph'."
  (make-glyph spec-list 'icon))

(defun nothing-image-instance-p (object)
  "Return t if OBJECT is an image instance of type `nothing'."
  (and (image-instance-p object) (eq 'nothing (image-instance-type object))))

(defun text-image-instance-p (object)
  "Return t if OBJECT is an image instance of type `text'."
  (and (image-instance-p object) (eq 'text (image-instance-type object))))

(defun mono-pixmap-image-instance-p (object)
  "Return t if OBJECT is an image instance of type `mono-pixmap'."
  (and (image-instance-p object) (eq 'mono-pixmap
				     (image-instance-type object))))

(defun color-pixmap-image-instance-p (object)
  "Return t if OBJECT is an image instance of type `color-pixmap'."
  (and (image-instance-p object) (eq 'color-pixmap
				     (image-instance-type object))))

(defun pointer-image-instance-p (object)
  "Return t if OBJECT is an image instance of type `pointer'."
  (and (image-instance-p object) (eq 'pointer (image-instance-type object))))

(defun widget-image-instance-p (object)
  "Return t if OBJECT is an image instance of type `widget'."
  (and (image-instance-p object) (eq 'widget (image-instance-type object))))

(defun subwindow-image-instance-p (object)
  "Return t if OBJECT is an image instance of type `subwindow'."
  (and (image-instance-p object) (eq 'subwindow (image-instance-type object))))

;;;;;;;;;; the built-in glyphs

(defvar text-pointer-glyph (make-pointer-glyph)
  "*The shape of the mouse-pointer when over text.
This is a glyph; use `set-glyph-image' to change it.")
(set-glyph-face text-pointer-glyph 'pointer)

(defvar nontext-pointer-glyph (make-pointer-glyph)
  "*The shape of the mouse-pointer when over a buffer, but not over text.
This is a glyph; use `set-glyph-image' to change it.
If unspecified in a particular domain, `text-pointer-glyph' is used.")
(set-glyph-face nontext-pointer-glyph 'pointer)

(defvar modeline-pointer-glyph (make-pointer-glyph)
  "*The shape of the mouse-pointer when over the modeline.
This is a glyph; use `set-glyph-image' to change it.
If unspecified in a particular domain, `nontext-pointer-glyph' is used.")
(set-glyph-face modeline-pointer-glyph 'pointer)

(defvar selection-pointer-glyph (make-pointer-glyph)
  "*The shape of the mouse-pointer when over a selectable text region.
This is a glyph; use `set-glyph-image' to change it.
If unspecified in a particular domain, `text-pointer-glyph' is used.")
(set-glyph-face selection-pointer-glyph 'pointer)

(defvar busy-pointer-glyph (make-pointer-glyph)
  "*The shape of the mouse-pointer when XEmacs is busy.
This is a glyph; use `set-glyph-image' to change it.
If unspecified in a particular domain, the pointer is not changed
when XEmacs is busy.")
(set-glyph-face busy-pointer-glyph 'pointer)

(defvar toolbar-pointer-glyph (make-pointer-glyph)
  "*The shape of the mouse-pointer when over a toolbar.
This is a glyph; use `set-glyph-image' to change it.
If unspecified in a particular domain, `nontext-pointer-glyph' is used.")
(set-glyph-face toolbar-pointer-glyph 'pointer)

(defvar divider-pointer-glyph (make-pointer-glyph)
  "*The shape of the mouse-pointer when over a window divider.
This is a glyph; use `set-glyph-image' to change it.
If unspecified in a particular domain, `nontext-pointer-glyph' is used.")
(set-glyph-face divider-pointer-glyph 'pointer)

;; The following three are in C.
(if (featurep 'menubar)
    (set-glyph-face menubar-pointer-glyph 'pointer))
(if (featurep 'scrollbar)
    (set-glyph-face scrollbar-pointer-glyph 'pointer))
(set-glyph-face gc-pointer-glyph 'pointer)

;; Now add the magic access/set behavior.

(defun dontusethis-set-value-glyph-handler (sym args fun harg handler)
  (error "Use `set-glyph-image' to set `%s'" sym))
(defun dontusethis-make-unbound-glyph-handler (sym args fun harg handler)
  (error "Can't `makunbound' `%s'" sym))
(defun dontusethis-make-local-glyph-handler (sym args fun harg handler)
  (error "Use `set-glyph-image' to make local values for `%s'" sym))

(defun define-constant-glyph (sym)
  (dontusethis-set-symbol-value-handler
   sym 'set-value
   'dontusethis-set-value-glyph-handler)
  (dontusethis-set-symbol-value-handler
   sym 'make-unbound
   'dontusethis-make-unbound-glyph-handler)
  (dontusethis-set-symbol-value-handler
   sym 'make-local
   'dontusethis-make-local-glyph-handler)
  ;; Make frame properties magically work with glyph variables.
  (put sym 'const-glyph-variable t))

(define-constant-glyph 'text-pointer-glyph)
(define-constant-glyph 'nontext-pointer-glyph)
(define-constant-glyph 'modeline-pointer-glyph)
(define-constant-glyph 'selection-pointer-glyph)
(define-constant-glyph 'busy-pointer-glyph)
(define-constant-glyph 'gc-pointer-glyph)
(define-constant-glyph 'divider-pointer-glyph)
(define-constant-glyph 'toolbar-pointer-glyph)
(define-constant-glyph 'menubar-pointer-glyph)
(define-constant-glyph 'scrollbar-pointer-glyph)

(define-constant-glyph 'octal-escape-glyph)
(define-constant-glyph 'control-arrow-glyph)
(define-constant-glyph 'invisible-text-glyph)
(define-constant-glyph 'hscroll-glyph)
(define-constant-glyph 'truncation-glyph)
(define-constant-glyph 'continuation-glyph)

(define-constant-glyph 'frame-icon-glyph)

;; backwards compatibility garbage

(defun dontusethis-old-pointer-shape-handler (sym args fun harg handler)
  (let ((value (car args)))
    (if (null value)
	(remove-specifier harg 'global)
      (set-glyph-image (symbol-value harg) value))))

;; It might or might not be garbage, but it's rude.  Make these
;; `compatible' instead of `obsolete'.  -slb
(defun define-obsolete-pointer-glyph (old new)
  (define-compatible-variable-alias old new)
  (dontusethis-set-symbol-value-handler
   old 'set-value 'dontusethis-old-pointer-shape-handler new))

;;; (defvar x-pointer-shape nil)
(define-obsolete-pointer-glyph 'x-pointer-shape 'text-pointer-glyph)

;;; (defvar x-nontext-pointer-shape nil)
(define-obsolete-pointer-glyph 'x-nontext-pointer-shape 'nontext-pointer-glyph)

;;; (defvar x-mode-pointer-shape nil)
(define-obsolete-pointer-glyph 'x-mode-pointer-shape 'modeline-pointer-glyph)

;;; (defvar x-selection-pointer-shape nil)
(define-obsolete-pointer-glyph 'x-selection-pointer-shape
  'selection-pointer-glyph)

;;; (defvar x-busy-pointer-shape nil)
(define-obsolete-pointer-glyph 'x-busy-pointer-shape 'busy-pointer-glyph)

;;; (defvar x-gc-pointer-shape nil)
(define-obsolete-pointer-glyph 'x-gc-pointer-shape 'gc-pointer-glyph)

;;; (defvar x-toolbar-pointer-shape nil)
(define-obsolete-pointer-glyph 'x-toolbar-pointer-shape 'toolbar-pointer-glyph)

;; for subwindows
(defalias 'subwindow-xid 'image-instance-subwindow-id)
(defalias 'subwindow-width 'image-instance-width)
(defalias 'subwindow-height 'image-instance-height)
;;;;;;;;;; initialization

(defun init-glyphs ()
  ;; initialize default image types
  (if (featurep 'x)
    (set-console-type-image-conversion-list 'x
     `(,@(if (featurep 'xpm) '(("\\.xpm\\'" [xpm :file nil] 2)))
	 ("\\.xbm\\'" [xbm :file nil] 2)
       ,@(if (featurep 'xpm) '(("\\`/\\* XPM \\*/" [xpm :data nil] 2)))
       ,@(if (featurep 'xface) '(("\\`X-Face:" [xface :data nil] 2)))
       ,@(if (featurep 'gif) '(("\\.gif\\'" [gif :file nil] 2)
			       ("\\`GIF8[79]" [gif :data nil] 2)))
       ,@(if (featurep 'jpeg) '(("\\.jpe?g\\'" [jpeg :file nil] 2)))
       ;; all of the JFIF-format JPEG's that I've seen begin with
       ;; the following.  I have no idea if this is standard.
       ,@(if (featurep 'jpeg) '(("\\`\377\330\377\340\000\020JFIF"
				 [jpeg :data nil] 2)))
       ,@(if (featurep 'png) '(("\\.png\\'" [png :file nil] 2)))
       ,@(if (featurep 'png) '(("\\`\211PNG" [png :data nil] 2)))
       ("" [string :data nil] 2)
       ("" [nothing]))))
  ;; #### this should really be formatted-string, not string but we
  ;; don't have it implemented yet
  ;;
  ;; #define could also mean a bitmap as well as a version 1 XPM.  Who
  ;; cares.  We don't want the file contents getting converted to a
  ;; string in either case which is why the entry is there.
  (if (featurep 'tty)
      (progn
	(set-console-type-image-conversion-list
	 'tty
	 '(("^#define" [string :data "[xpm]"])
	   ("\\`X-Face:" [string :data "[xface]"])
	   ("\\`/\\* XPM \\*/" [string :data "[xpm]"])
	   ("\\`GIF87" [string :data "[gif]"])
	   ("\\`\377\330\340\000\020JFIF" [string :data "[jpeg]"])
	   ("" [string :data nil] 2)
	   ;; this last one is here for pointers and icons and such --
	   ;; strings are not allowed so they will be ignored.
	   ("" [nothing])))

	;; finish initializing truncation glyph -- created internally
	;; because it has a built-in bitmap
	(set-glyph-image truncation-glyph "$" 'global 'tty)

	;; finish initializing continuation glyph -- created internally
	;; because it has a built-in bitmap
	(set-glyph-image continuation-glyph "\\" 'global 'tty)

	;; finish initializing hscroll glyph -- created internally
	;; because it has a built-in bitmap
	(set-glyph-image hscroll-glyph "$" 'global 'tty)))

  (set-glyph-image octal-escape-glyph "\\")
  (set-glyph-image control-arrow-glyph "^")
  (set-glyph-image invisible-text-glyph " ...")
  ;; (set-glyph-image hscroll-glyph "$")

  (let ((face (make-face 'border-glyph
			 "Truncation and continuation glyphs face")))
    (set-glyph-face continuation-glyph face)
    (set-glyph-face truncation-glyph face)
    (set-glyph-face hscroll-glyph face))

  ;; finish initializing xemacs logo -- created internally because it
  ;; has a built-in bitmap
  (if (featurep 'xpm)
      (set-glyph-image xemacs-logo
		       (concat "../etc/"
			       (if emacs-beta-version
				   "xemacs-beta.xpm"
				 "xemacs.xpm"))
		       'global 'x))
  (cond ((featurep 'xpm)
	 (set-glyph-image frame-icon-glyph
			  (concat "../etc/" "xemacs-icon.xpm")
			  'global 'x))
	((featurep 'x)
	 (set-glyph-image frame-icon-glyph
			  (concat "../etc/" "xemacs-icon2.xbm")
			  'global 'x)))

  (if (featurep 'tty)
      (set-glyph-image xemacs-logo
		       "XEmacs <insert spiffy graphic logo here>"
		       'global 'tty))
)

(init-glyphs)

;;; glyphs.el ends here.
