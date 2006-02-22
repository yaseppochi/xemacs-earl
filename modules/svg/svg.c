/* xsvg - SVG viewer application for the X Window System
 *
 * Copyright © 2002 USC/Information Sciences Institute
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without
 * fee, provided that the above copyright notice appear in all copies
 * and that both that copyright notice and this permission notice
 * appear in supporting documentation, and that the name of
 * Information Sciences Institute not be used in advertising or
 * publicity pertaining to distribution of the software without
 * specific, written prior permission.  Information Sciences Institute
 * makes no representations about the suitability of this software for
 * any purpose.  It is provided "as is" without express or implied
 * warranty.
 *
 * INFORMATION SCIENCES INSTITUTE DISCLAIMS ALL WARRANTIES WITH REGARD
 * TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL INFORMATION SCIENCES
 * INSTITUTE BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA
 * OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 * Author: Carl Worth <cworth@isi.edu>
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/time.h>
#include <time.h>
#include <png.h>

#include <cairo.h>
#include <cairo-xlib.h>
#include <cairo-xlib-xrender.h>

#include <svg-cairo.h>

#include "args.h"
#include <X11/Xatom.h>
#include <X11/Xutil.h>

/* XXX: The cursor code doesn't seem to be working, (and someone
 * couldn't get it to compile on Solaris), so I'm just disabling it
 * until someone fixes it. */
#if CURSOR_CODE_IS_FIXED
#include <X11/Xcursor/Xcursor.h>
#endif

#define SHIFT 10

#define ARRAY_SIZE(A) (sizeof(A)/sizeof(A[0]))
#define MIN(a, b)     (((a) < (b)) ? (a) : (b))

typedef struct win {
    Display *dpy;
    int scr;
    Window win;
    unsigned long event_mask;
    Pixmap pix;
    GC gc;
    Visual *visual;
    Colormap cmap;
    int depth;

#if CURSOR_CODE_IS_FIXED
    Cursor arrow;
    Cursor watch;
#endif

    unsigned int width, height;
    int x_flip, y_flip;

    cairo_t *cr;
    int needs_refresh;

    svg_cairo_t *svgc;

    char **svg_files;
    int svg_nfile;
    int svg_curfile;

    double tx;
    double ty;
    double zoom;
    double tolerance;

    int fit_mode;
    int full_mode;

    Atom wm_delete_window_atom;
} win_t;

typedef struct callback_doc {
    void *callback;
    char *doc;
} callback_doc_t;

typedef int (*key_callback_t)(win_t *win);

typedef struct key_binding
{
    char *key;
    int is_alias;
    KeyCode keycode;
    key_callback_t callback;
} key_binding_t;

static void
win_init (win_t *win, Display *dpy, int argb, char *geometry, 
	  char **svg_files, int svg_nfile);

static void
win_deinit (win_t *win);

static void
win_refresh (win_t *win);

static void
win_grow_pixmap(win_t *win);

static int
win_handle_key_press(win_t *win, XKeyEvent *kev);

static void
win_handle_configure (win_t *win, XConfigureEvent *cev);

static void
win_handle_expose(win_t *win, XExposeEvent *eev);

static void
win_handle_events(win_t *win);

static void
win_reconfigure_normal (win_t *win, unsigned int width, unsigned int height);

static void
win_reconfigure_fit_mode (win_t *win, unsigned int width, unsigned int height);

/* callbacks */
static int
quit_cb(win_t *win);
static int
x_flip_cb(win_t *win);
static int
y_flip_cb(win_t *win);
static int
left_cb(win_t *win);
static int
right_cb(win_t *win);
static int
up_cb(win_t *win);
static int
down_cb(win_t *win);
static int
zoom_in_cb(win_t *win);
static int
zoom_out_cb(win_t *win);
static int
flatten_cb(win_t *win);
static int
smooth_cb(win_t *win);
static int
toggle_fit_cb(win_t *win);
static int
toggle_full_cb(win_t *win);
static int
next_svg_cb (win_t *win);
static int
prev_svg_cb (win_t *win);
static int
first_svg_cb (win_t *win);
static int
last_svg_cb (win_t *win);

static const callback_doc_t callback_doc[] = {
    { quit_cb,		"Exit the program" },
    { zoom_in_cb,	"Zoom in  (1.1X)" },
    { zoom_out_cb,	"Zoom out (1.1X)" },
    { x_flip_cb,	"Flip by inverting X values." },
    { y_flip_cb,	"Flip by inverting Y values." },
    { smooth_cb,	"Increase rendering accuracy (10X)" },
    { flatten_cb,	"Decrease rendering accuracy (10X)" },
    { toggle_fit_cb,	"Toggle fit-in-window mode" },
    { next_svg_cb,	"Show next SVG from command line" },
    { prev_svg_cb,	"Show prev SVG from command line" },
    { first_svg_cb,	"Show first SVG from command line" },
    { last_svg_cb,	"Show last SVG from command line" },
};

static key_binding_t key_binding[] = {
    /* Keysym, Alias, Keycode, callback */
    { "X",	0, 0, x_flip_cb },
    { "Y",	0, 0, y_flip_cb },
    { "Q",	0, 0, quit_cb },
    { "Left",	0, 0, left_cb },
    { "Right",	0, 0, right_cb },
    { "Up",	0, 0, up_cb },
    { "Down",	0, 0, down_cb },
    { "plus",	0, 0, zoom_in_cb },
    { "minus",	0, 0, zoom_out_cb },
    { "greater",0, 0, smooth_cb },
    { "less",	0, 0, flatten_cb },
    { "F",      0, 0, toggle_fit_cb },
    { "S",	0, 0, toggle_full_cb },
    { "space",	0, 0, next_svg_cb },
    { "BackSpace", 0, 0, prev_svg_cb },
    { "less",	0, 0, first_svg_cb },
    { "greater",0, 0, last_svg_cb },
};

int 
main (int argc, char **argv)
{
    args_t args;
    win_t win;
    Display *dpy;

    args_parse (&args, argc, argv);

    dpy = XOpenDisplay (args.display);
    if (dpy == NULL) {
	fprintf (stderr, "Failed to open display: %s\n", XDisplayName(args.display));
	return 1;
    }

    win.full_mode = 0;
    win_init (&win, dpy, args.argb, args.geometry, 
	      args.svg_files, args.svg_nfile);
    win.zoom *= args.scale;
    win.x_flip = args.flipx;
    win.y_flip = args.flipy;
    win.fit_mode = args.fit;
    
    win_handle_events (&win);
    
    win_deinit (&win);

    XCloseDisplay (dpy);

    if (args.svg_files)
	free (args.svg_files);	/* make valgrind happy */
    return 0;
}

static Visual *
find_argb_visual (Display *dpy, int scr)
{
    XVisualInfo		*xvi;
    XVisualInfo		template;
    int			nvi;
    int			i;
    XRenderPictFormat	*format;
    Visual		*visual;

    template.screen = scr;
    template.depth = 32;
    template.class = TrueColor;
    xvi = XGetVisualInfo (dpy, 
			  VisualScreenMask |
			  VisualDepthMask |
			  VisualClassMask,
			  &template,
			  &nvi);
    if (!xvi)
	return 0;
    visual = 0;
    for (i = 0; i < nvi; i++)
    {
	format = XRenderFindVisualFormat (dpy, xvi[i].visual);
	if (format->type == PictTypeDirect && format->direct.alphaMask)
	{
	    visual = xvi[i].visual;
	    break;
	}
    }

    XFree (xvi);
    return visual;
}

static void busy (win_t *win, int on)
{
#if CURSOR_CODE_IS_FIXED
    if (win->win)
	XDefineCursor (win->dpy, win->win, on ? win->watch : win->arrow);
#endif
}

static int
win_load (win_t *win)
{
    char    *svg_file = win->svg_files[win->svg_curfile];
    int	    status;

    busy (win, 1);
    XFlush (win->dpy);
    if (win->svgc)
	svg_cairo_destroy (win->svgc);

    win->svgc = 0;

    status = svg_cairo_create (&win->svgc);
    if (status) {
	fprintf (stderr, "Failed to create svg_cairo_t. Exiting.\n");
	exit(1);
    }

    svg_cairo_set_viewport_dimension (win->svgc, win->width, win->height);

    /* special case filename "-" means stdin */
    if (strcmp(svg_file,"-") == 0)
	status = svg_cairo_parse_file (win->svgc, stdin);
    else
	status = svg_cairo_parse (win->svgc, svg_file);
    if (status) {
	fprintf (stderr, "Failed to parse SVG file: %s. Exiting.\n", svg_file);
	exit(1);
    }
    return status;
}
    
static void
win_name (win_t *win)
{
    XWMHints *wmhints;
    XSizeHints *normalhints;
    XClassHint *classhint;
    char *name;
    char *base;
    char *svg_file = win->svg_files[win->svg_curfile];
    
    name = malloc (strlen ("xsvg: ") + strlen (svg_file) + 1);
    strcpy (name, "xsvg: ");
    strcat (name, svg_file);
    base = strrchr (svg_file, '/');
    if (!base)
	base = svg_file;
    else
	base++;
    
    normalhints = XAllocSizeHints ();
    normalhints->flags = 0;
    normalhints->x = 0;
    normalhints->y = 0;
    normalhints->width = win->width;
    normalhints->height = win->height;

    classhint = XAllocClassHint ();
    classhint->res_name = "xsvg";
    classhint->res_class = "Xsvg";
    
    wmhints = XAllocWMHints ();
    wmhints->flags = InputHint;
    wmhints->input = True;
    
    XmbSetWMProperties (win->dpy, win->win, name, base, 0, 0, 
			normalhints, wmhints, classhint);
    XFree (wmhints);
    XFree (classhint);
    XFree (normalhints);
    free (name);
}

static void
win_full (win_t *win)
{
    XWindowChanges  changes;
    XSizeHints	    *hints = XAllocSizeHints();
    unsigned long   mask;
    unsigned int    width, height;
    
    hints->flags = PSize | PWinGravity;
    mask = CWWidth | CWHeight;
    if (win->full_mode) {
/*	Atom		    atom[1]; */
	XWindowAttributes   root_attr;
	XGetWindowAttributes (win->dpy,
			      RootWindow (win->dpy, win->scr), &root_attr);
	hints->win_gravity = StaticGravity;
	hints->flags |= PPosition;
	hints->x = 0;
	hints->y = 0;
	width = root_attr.width;
	height = root_attr.height;
	changes.x = 0;
	changes.y = 0;
	changes.stack_mode = Above;
	mask |= CWX | CWY | CWStackMode;
/*
	atom[0] = XInternAtom (win->dpy, "_NET_WM_ACTION_FULLSCREEN", False);
	XChangeProperty (win->dpy, win->win,
			 XInternAtom (win->dpy, "_NET_WM_WINDOW_TYPE", False),
			 XA_ATOM,
			 32,
			 PropModeReplace,
			 (unsigned char *) atom, 1);
 */
    }
    else
    {
        svg_cairo_get_size (win->svgc, &width, &height);
	hints->win_gravity = NorthWestGravity;
/*
	XDeleteProperty (win->dpy, win->win,
			 XInternAtom (win->dpy, "_NET_WM_WINDOW_TYPE", False));
 */
    }
    changes.width = width;
    changes.height = height;
    
    hints->width = width;
    hints->height = height;
    XConfigureWindow (win->dpy, win->win, mask, &changes);
    XSetWMNormalHints (win->dpy, win->win, hints);
    XFree (hints);
}

static void
show_svg (win_t *win, int n)
{
    win->svg_curfile = n % win->svg_nfile;
    win_load (win);
    win_name (win);
    win->needs_refresh = 1;
}

static int
next_svg_cb (win_t *win)
{
    show_svg (win, (win->svg_curfile + 1) % win->svg_nfile);
    return 0;
}

static int
prev_svg_cb (win_t *win)
{
    show_svg (win, (win->svg_curfile - 1 + win->svg_nfile) % win->svg_nfile);
    return 0;
}

static int
first_svg_cb (win_t *win)
{
    show_svg (win, 0);
    return 0;
}

static int
last_svg_cb (win_t *win)
{
    show_svg (win, (win->svg_nfile - 1) % win->svg_nfile);
    return 0;
}

static void
win_init (win_t *win, Display *dpy, int argb, char *geometry, 
	  char **svg_files, int svg_nfile)
{
    unsigned int i;
    XGCValues gcv;
    XSetWindowAttributes attributes;
    unsigned long attributemask = 0;
    cairo_surface_t *surface;

    win->dpy = dpy;
    win->scr = DefaultScreen (dpy);

    win->tx = 0;
    win->ty = 0;
    win->x_flip = 0;
    win->y_flip = 0;
    win->zoom = 1.0;
    win->tolerance = 0.1;

    win->needs_refresh = 1;

    win->svg_files = svg_files;
    win->svg_nfile = svg_nfile;
    win->svg_curfile = 0;
    
    win->svgc = 0;
    win->win = 0;
    
    win->svg_curfile = 0;
    win_load (win);

    if (argb && (win->visual = find_argb_visual (dpy, win->scr)))
    {
	win->cmap = XCreateColormap (dpy, RootWindow (dpy, win->scr),
				     win->visual, AllocNone);
	attributes.override_redirect = False;
	attributes.background_pixel = 0;
	attributes.border_pixel = 0;
	attributes.colormap = win->cmap;
	attributemask = (CWBackPixel|
			 CWBorderPixel|
			 CWOverrideRedirect |
			 CWColormap);
	win->depth = 32;
    }
    else
    {
	win->cmap = DefaultColormap (dpy, win->scr);
	win->visual = DefaultVisual (dpy, win->scr);
	attributes.background_pixel = WhitePixel (dpy, win->scr);
	attributes.border_pixel = BlackPixel (dpy, win->scr);
	attributemask = (CWBackPixel |
			 CWBorderPixel);
	win->depth = DefaultDepth (dpy, win->scr);
    }
    
#if CURSOR_CODE_IS_FIXED
    win->arrow = XcursorLibraryLoadCursor (dpy, "left_ptr");
    win->watch = XcursorLibraryLoadCursor (dpy, "watch");
#endif
    if (win->full_mode) {
	XWindowAttributes   root_attr;
	XGetWindowAttributes (win->dpy,
			      RootWindow (win->dpy, win->scr), &root_attr);
	win->width = root_attr.width;
	win->height = root_attr.height;
    } else if (geometry) {
	int x, y;
	XParseGeometry (geometry, &x, &y, &win->width, &win->height);
    } else {
        svg_cairo_get_size (win->svgc, &win->width, &win->height);
    }

    win->win = XCreateWindow (dpy, RootWindow (dpy, win->scr),
			      0, 0, win->width, win->height, 0, win->depth,
			      InputOutput, win->visual,
			      attributemask, &attributes);

    win_name (win);
    win_full (win);

    if (!win->width)
	win->width = 1;
    if (!win->height)
	win->height = 1;
    win->pix = XCreatePixmap(dpy, win->win, win->width, win->height, win->depth);
    if (argb)
	gcv.foreground = 0;
    else
	gcv.foreground = WhitePixel(dpy, win->scr);
    win->gc = XCreateGC(dpy, win->pix, GCForeground, &gcv);
    XFillRectangle(dpy, win->pix, win->gc, 0, 0, win->width, win->height);

    for (i=0; i < ARRAY_SIZE(key_binding); i++) {
	KeySym keysym;
	keysym = XStringToKeysym(key_binding[i].key);
	if (keysym == NoSymbol)
	    fprintf(stderr, "ERROR: No keysym for \"%s\"\n", key_binding[i].key);
	else
	{
	    key_binding[i].keycode = XKeysymToKeycode(dpy, keysym);
	    if (!key_binding[i].keycode)
		fprintf(stderr, "ERROR: No key for \"%s\"\n", key_binding[i].key);
	}
    }

    surface = cairo_xlib_surface_create (dpy,
					 win->pix,
					 win->visual,
					 win->width, win->height);
    win->cr = cairo_create (surface);
    cairo_surface_destroy (surface);
    /* XXX: This probably doesn't need to be here (eventually) */
    cairo_set_source_rgb (win->cr, 1, 1, 1);

    svg_cairo_set_viewport_dimension (win->svgc, win->width, win->height);

    win->event_mask = (KeyPressMask
		       | StructureNotifyMask
		       | ExposureMask);

    XSelectInput (dpy, win->win, win->event_mask);
    win->wm_delete_window_atom = XInternAtom(dpy, "WM_DELETE_WINDOW", False);
    XSetWMProtocols(dpy, win->win, &win->wm_delete_window_atom, 1);

    XMapWindow (dpy, win->win);
}

static void
win_deinit (win_t *win)
{
    svg_cairo_destroy (win->svgc);
    win->svgc = NULL;
    cairo_destroy (win->cr);
    win->cr = NULL;
    XFreeGC (win->dpy, win->gc);
    XDestroyWindow (win->dpy, win->win);
}

static void
win_refresh (win_t *win)
{
    busy (win, 1);
    XFlush (win->dpy);

    XFillRectangle (win->dpy, win->pix, win->gc, 0, 0, win->width, win->height);

    cairo_save (win->cr);

    cairo_translate (win->cr,
		     win->x_flip ? win->width - win->tx : win->tx,
		     win->y_flip ? win->height - win->ty : win->ty);
    cairo_scale (win->cr,
		 win->x_flip ? -win->zoom : win->zoom,
		 win->y_flip ? -win->zoom : win->zoom);
    cairo_set_tolerance (win->cr, win->tolerance);

    svg_cairo_render (win->svgc, win->cr);

    cairo_restore (win->cr);

    XCopyArea (win->dpy, win->pix, win->win, win->gc,
	       0, 0, win->width, win->height,
	       0, 0);
    busy (win, 0);
}

static void
win_grow_pixmap(win_t *win)
{
    Pixmap new;
    cairo_surface_t *surface;

    if (!win->width)
	win->width = 1;
    if (!win->height)
	win->height = 1;
    new = XCreatePixmap(win->dpy, win->win, win->width, win->height,
			win->depth);
    XFillRectangle(win->dpy, new, win->gc, 0, 0, win->width, win->height);
    XCopyArea(win->dpy, win->pix, new, win->gc, 0, 0, win->width, win->height, 0, 0);
    XFreePixmap(win->dpy, win->pix);
    win->pix = new;
    cairo_destroy (win->cr);
    surface = cairo_xlib_surface_create (win->dpy,
					 win->pix,
					 win->visual,
					 win->width, win->height);
    win->cr = cairo_create (surface);
    cairo_surface_destroy (surface);
    win->needs_refresh = 1;
}

static int
win_handle_key_press(win_t *win, XKeyEvent *kev)
{
    unsigned int i;

    for (i=0; i < ARRAY_SIZE(key_binding); i++)
	if (key_binding[i].keycode == kev->keycode)
	    return (key_binding[i].callback)(win);
	
    return 0;
}

static void
win_reconfigure_normal (win_t *win, unsigned int width, unsigned int height)
{
    int has_grown = 0;

    if (width > win->width || height > win->height)
	has_grown = 1;

    win->width = width;
    win->height = height;

    svg_cairo_set_viewport_dimension (win->svgc, win->width, win->height);

    if (has_grown)
	win_grow_pixmap (win);
}

static void
win_reconfigure_fit_mode (win_t *win, unsigned int width, unsigned int height)
{
    unsigned int dflt_width, dflt_height;
    int has_grown = 0;

    if (width > win->width || height > win->height)
	has_grown = 1;

    svg_cairo_get_size (win->svgc, &dflt_width, &dflt_height);
    win->zoom = MIN ((double) width / (double) dflt_width, (double) height / (double) dflt_height);

    win->width = width;
    win->height = height;

    svg_cairo_set_viewport_dimension (win->svgc, win->width, win->height);

    win->needs_refresh = 1;

    if (has_grown)
	win_grow_pixmap (win);
}

static void
win_handle_configure (win_t *win, XConfigureEvent *cev)
{
    if (win->fit_mode)
        win_reconfigure_fit_mode (win, cev->width, cev->height);
    else
        win_reconfigure_normal (win, cev->width, cev->height);
}

static void
win_handle_expose(win_t *win, XExposeEvent *eev)
{
    XCopyArea(win->dpy, win->pix, win->win, win->gc,
	      eev->x, eev->y, eev->width, eev->height,
	      eev->x, eev->y);
}

static void
win_handle_client_message (win_t *win, XClientMessageEvent *cmev)
{
    if (cmev->format == 32
	&& (Atom) cmev->data.l[0] == win->wm_delete_window_atom)
	exit (0);
}

static void
win_handle_events(win_t *win)
{
    int done;
    XEvent xev;

    while (1) {
	if (!XPending (win->dpy) && win->needs_refresh) {
	    win_refresh(win);
	    win->needs_refresh = 0;
	}

	XNextEvent (win->dpy, &xev);

	switch(xev.xany.type) {
	case KeyPress:
	    done = win_handle_key_press(win, &xev.xkey);
	    if (done)
		return;
	    break;
	case ConfigureNotify:
	    win_handle_configure(win, &xev.xconfigure);
	    break;
	case Expose:
	    win_handle_expose(win, &xev.xexpose);
	    break;
	case ClientMessage:
	    win_handle_client_message (win, &xev.xclient);
	    break;
	}
    }
}

static int
quit_cb(win_t *win)
{
    return 1;
}

static int
x_flip_cb(win_t *win)
{
    win->x_flip = ! win->x_flip;

    win->needs_refresh = 1;

    return 0;
}

static int
y_flip_cb(win_t *win)
{
    win->y_flip = ! win->y_flip;

    win->needs_refresh = 1;

    return 0;
}

static int
left_cb(win_t *win)
{
    win->fit_mode = 0;

    if (win->x_flip)
	win->tx += SHIFT * win->zoom;
    else
	win->tx -= SHIFT * win->zoom;
    
    win->needs_refresh = 1;

    return 0;
}

static int
right_cb(win_t *win)
{
    win->fit_mode = 0;

    if (win->x_flip)
	win->tx -= SHIFT * win->zoom;
    else
	win->tx += SHIFT * win->zoom;
    
    win->needs_refresh = 1;

    return 0;
}

static int
up_cb(win_t *win)
{
    win->fit_mode = 0;

    if (win->y_flip)
	win->ty += SHIFT * win->zoom;
    else
	win->ty -= SHIFT * win->zoom;
    
    win->needs_refresh = 1;

    return 0;
}

static int
down_cb(win_t *win)
{
    win->fit_mode = 0;

    if (win->y_flip)
	win->ty -= SHIFT * win->zoom;
    else
	win->ty += SHIFT * win->zoom;

    win->needs_refresh = 1;

    return 0;
}

static int
zoom_in_cb(win_t *win)
{
    win->fit_mode = 0;

    win->zoom *= 1.1;

    win->needs_refresh = 1;

    return 0;
}

static int
zoom_out_cb(win_t *win)
{
    win->fit_mode = 0;

    win->zoom /= 1.1;

    win->needs_refresh = 1;

    return 0;
}

static int
flatten_cb(win_t *win)
{
    win->tolerance *= 10;

    win->needs_refresh = 1;

    return 0;
}

static int
smooth_cb(win_t *win)
{
    win->tolerance /= 10;

    win->needs_refresh = 1;

    return 0;
}

static int
toggle_fit_cb(win_t *win)
{
    win->fit_mode = !win->fit_mode;

    if (win->fit_mode) {
        unsigned int dflt_width, dflt_height;

        svg_cairo_get_size (win->svgc, &dflt_width, &dflt_height);
        win->zoom = MIN ((double) win->width / (double) dflt_width, (double) win->height / (double) dflt_height);

        win->tx = 0;
        win->ty = 0;

        win->needs_refresh = 1;
    }

    return 0;
}

static int
toggle_full_cb(win_t *win)
{
    win->full_mode = !win->full_mode;
    XUnmapWindow (win->dpy, win->win);
    win_full (win);
    XMapWindow (win->dpy, win->win);
    return 0;
}
