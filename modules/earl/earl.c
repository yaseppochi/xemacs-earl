/* Emacs Augmented Resource Locators
 *
 * Lisp interface to session (URL) manager for XEmacs.
 *
 * Copyright (C) 2006 Stephen J. Turnbull <stephen@xemacs.org>
 * All rights reserved, except as expressly indicated below.
 *
 * This program includes code from files covered by the following copyrights:
 *     Copyright (C) 1998, 1999 J. Kean Johnston. All rights reserved.
 *     Copyright (C) 2002 Jerry James.
 *
 * This program is not considered part of XEmacs.
 *
 * You may use, copy, modify, and distribute this software under the terms
 * of the GNU General Public License, version 2 or later at your option.
 *
 * Author:		Stephen J. Turnbull <stephen@xemacs.org>
 * Creation-Date:	2005-01-15
 */

/* Commentary: see neonapi.[hc] for commentary on provided API. */

#include <config.h>
#include "lisp.h"
#ifdef HAVE_SHLIB
# include "emodules.h"
#endif
/*
#include "file-coding.h"
#include "lstream.h"
#include "elhash.h"
*/

/* #include <stdio.h> */
/* #include "sysfile.h" */
#include "earl.h"

/* Local references to Lisp symbols
   In GCC 4.0 they can't be static. */

Lisp_Object Qearl,
  Qlast_response_headers, Qlast_response_status,
  Qsession_handlep, Qsession_handle_livep, Qtransport,
  Qurl;

/************************************************************************/
/*                 session_handle lrecord implementation                */
/* Contents:								*/
/*   session_handle_description						*/
/*   allocate_session_handle						*/
/*   finalize_session_handle						*/
/*   mark_session_handle						*/
/*   print_session_handle						*/
/*   session_handle_get							*/
/*   session_handle_put							*/
/*   session_handle_remprop						*/
/*   session_handle_plist						*/
/*   DEFINE_LRECORD_IMPLEMENTATION					*/
/************************************************************************/

static const struct memory_description session_handle_description [] = {
  { XD_LISP_OBJECT, offsetof (struct Lisp_Session_Handle, url) },
  { XD_LISP_OBJECT, offsetof (struct Lisp_Session_Handle, transport) },
  { XD_LISP_OBJECT, offsetof (struct Lisp_Session_Handle, coding_system) },
  { XD_LISP_OBJECT,
    offsetof (struct Lisp_Session_Handle, last_response_status) },
  { XD_LISP_OBJECT,
    offsetof (struct Lisp_Session_Handle, last_response_headers) },
  { XD_LISP_OBJECT, offsetof (struct Lisp_Session_Handle, plist) },
  { XD_LISP_OBJECT, offsetof (struct Lisp_Session_Handle, state) },
  { XD_END }
};

static Lisp_Session_Handle*
allocate_session_handle (void)
{
  Lisp_Session_Handle *session_handle =
    ALLOC_LCRECORD_TYPE (Lisp_Session_Handle, &lrecord_session_handle);

  session_handle->url = Qnil;
  session_handle->transport = Qnil;
  session_handle->coding_system = Qnil;
  session_handle->last_response_status = Qnil;
  session_handle->last_response_headers = Qnil;
  session_handle->plist = Qnil;
  session_handle->state = Qnil;
  session_handle->transport_data = NULL;
  /* #### UNIMPLEMENTED we need to initialize the big_ball_of_string here. */
  return session_handle;
}

static void
finalize_session_handle (void *header, int for_disksave)
{
  Lisp_Session_Handle *session_handle = (Lisp_Session_Handle *) header;

  if (for_disksave)
    invalid_operation ("Can't dump an emacs containing SESSION_HANDLE objects",
		       wrap_session_handle (session_handle));

  if (!NILP (session_handle->transport))
    {
      struct earl_transport_data *data =
	(struct earl_transport_data *) session_handle->transport_data;
      data->transport_implementation->finalize (data);
    }

  /* #### UNIMPLEMENTED we need to free the big_ball_of_string here. */
}

static Lisp_Object
mark_session_handle (Lisp_Object obj)
{
  mark_object (XSESSION_HANDLE (obj)->url);
  mark_object (XSESSION_HANDLE (obj)->transport);
  mark_object (XSESSION_HANDLE (obj)->coding_system);
  mark_object (XSESSION_HANDLE (obj)->last_response_status);
  mark_object (XSESSION_HANDLE (obj)->last_response_headers);
  mark_object (XSESSION_HANDLE (obj)->plist);
  return XSESSION_HANDLE (obj)->state;
}

static void
print_session_handle (Lisp_Object obj,
		  Lisp_Object printcharfun,
		  int UNUSED (escapeflag))
{
  Lisp_Session_Handle *session_handle = XSESSION_HANDLE (obj);

  /* #### we should be able to do better */
  if (print_readably)
    printing_unreadable_object ("#<session_handle>");

  write_c_string (printcharfun, "#<session_handle");
  if (NILP(session_handle->transport))
    write_c_string (printcharfun, " (dead)");
  else
    write_fmt_string_lisp (printcharfun, " %S", 1, session_handle->transport);
  if (!NILP (session_handle->url))
    write_fmt_string_lisp (printcharfun, " %S", 1, session_handle->url);
  write_fmt_string (printcharfun, " 0x%lx>", (unsigned long) session_handle);
}

static Lisp_Object
session_handle_get (Lisp_Object session, Lisp_Object prop)
{
  Lisp_Session_Handle *s = XSESSION_HANDLE (session);

  if (EQ (prop, Qlast_response_headers))
    return s->last_response_headers;
  else if (EQ (prop, Qlast_response_status))
    return s->last_response_status;
  else if (EQ (prop, Qurl))
    return s->url;
  else if (EQ (prop, Qcoding_system))
    return s->coding_system;
  else if (EQ (prop, Qtransport))
    return s->transport;

  /* #### We should get transport-specific properties here. */

  return external_plist_get (&s->plist, prop, 0, ERROR_ME);
}

static int
session_handle_put (Lisp_Object session, Lisp_Object prop, Lisp_Object value)
{
  Lisp_Session_Handle *s = XSESSION_HANDLE (session);

  if (EQ (prop, Qlast_response_headers)
      || EQ (prop, Qlast_response_status)
      || EQ (prop, Qurl)
      || EQ (prop, Qcoding_system)
      || EQ (prop, Qtransport))
    /* #### We should check transport-specific properties here. */
    return 0;

  external_plist_put (&s->plist, prop, value, 0, ERROR_ME);
  return 1;
}

static int
session_handle_remprop (Lisp_Object session, Lisp_Object prop)
{
  Lisp_Session_Handle *s = XSESSION_HANDLE (session);

  if (EQ (prop, Qlast_response_headers)
      || EQ (prop, Qlast_response_status)
      || EQ (prop, Qurl)
      || EQ (prop, Qcoding_system)
      || EQ (prop, Qtransport))
    /* #### We should check transport-specific properties here. */
    return -1;			/* immutable properties */
  else
    return external_remprop (&s->plist, prop, 0, ERROR_ME);
}

static Lisp_Object
session_handle_plist (Lisp_Object session)
{
  Lisp_Object retval;
  Lisp_Session_Handle *s;

  CHECK_SESSION_HANDLE (session);
  s = XSESSION_HANDLE (session);

  retval = s->plist;
  retval = cons3 (Qlast_response_status, s->last_response_status, retval);
  retval = cons3 (Qlast_response_headers, s->last_response_headers, retval);
  retval = cons3 (Qurl, s->url, retval);
  retval = cons3 (Qcoding_system, s->coding_system, retval);
  retval = cons3 (Qtransport, s->transport, retval);

#ifdef HAVE_CURL
  /* #### extract properties from the curl_handle and add to retval here */
  /* #### should we do curl_get_info here? */
#endif
#ifdef HAVE_NEON
  /* #### extract properties from the neon_data and add to retval here */
#endif
  return retval;
}

DEFINE_LRECORD_IMPLEMENTATION_WITH_PROPS ("session_handle", /* name */
			       session_handle,		    /* c_name */
			       0,			    /* dumpable */
                               mark_session_handle,	    /* marker */
			       print_session_handle,	    /* printer */
			       finalize_session_handle,	    /* nuker */
                               NULL,			    /* equal */
			       NULL,			    /* hash */
			       session_handle_description,  /* desc */
			       session_handle_get,	    /* getprop */
			       session_handle_put,	    /* putprop */
			       session_handle_remprop,	    /* remprop */
			       session_handle_plist,	    /* plist */
			       Lisp_Session_Handle);        /* structtype */


/************************************************************************/
/*			   LISP object handling 			*/
/************************************************************************/
/*                   Basic session_handle functions                     */
/* Contents:								*/
/*   make-session-handle						*/
/*   session-handle-transport						*/
/*   session-handle-p							*/
/*   session-handle-live-p						*/
/*   session-handle-plist						*/
/************************************************************************/

/* ###autoload */
DEFUN ("make-session-handle", Fmake_session_handle, 1, 2, 0, /*
Return a session handle for URL.
URL is a string, which must be a known URI scheme.  The 5-part schemes
defined by RFC 2396 (scheme://authinfo@host:port/path) are always OK.
Optional argument CODESYS is the coding system (an object or symbol, default
UTF-8) used to encode URL.  URL must not be URL-encoded; that will be done
automatically.

Use `session-handle-p' to check whether an object is a session handle, and
`session-handle-live-p' to determine whether it currently is associated
with a transport \(and possibly an open connection, depending on protocol).
\(The semantics of `session-handle-live-p' need to be refined.)

Session handles support the property interface \(`put', `get', `remprop', and
`object-plist').  The following properties are predefined and may not be set
or removed by `put' or `remprop':
    `url'			the initial URL
    `last-response-status'	the protocol status of the response to the
				last request
    `last-response-headers'	the protocol headers from the response to the
				last request
The following properties are predefined and currently cannot be set or
removed, but may be extended in the future:
    `transport'			the module used to handle connections \(`curl'
				and `neon' are implemented, others may be)
    `coding-system'		the coding system used to encode URLs and
				paths to resources
*/
       (url, codesys))
{
  Lisp_Session_Handle *session_handle = allocate_session_handle ();

  if (NILP (codesys))
    codesys = Fget_coding_system (Qutf_8);
  CHECK_CODING_SYSTEM (codesys);
  session_handle->coding_system = codesys;

  CHECK_STRING (url);
  session_handle->url = url;

  return wrap_session_handle (session_handle);
}

/* ###autoload */
DEFUN ("session-handle-p", Fsession_handle_p, 1, 1, 0, /*
Return t if OBJECT is a SESSION_HANDLE connection.
*/
       (object))
{
  return SESSION_HANDLEP (object) ? Qt : Qnil;
}

/* ###autoload */
DEFUN ("session-handle-live-p", Fsession_handle_live_p, 1, 1, 0, /*
Return non-nil if SESSION_HANDLE is an active SESSION_HANDLE connection.
*/
       (session_handle))
{
  CHECK_SESSION_HANDLE (session_handle);
  return Fget (session_handle, Qtransport, Qnil);
}
/************************************************************************/
/*				Module API				*/
/* Contents:								*/
/*   modules_of_earl							*/
/*   syms_of_earl							*/
/*   vars_of_earl							*/
/*   unload_earl							*/
/************************************************************************/

/*
 * Each dynamically loaded Emacs module is given a name at compile
 * time. This is a short name, and must be a valid part of a C
 * identifier.  This name is used to construct the name of several
 * functions which must appear in the module source code.
 * The first such function, modules_of_XXXX, should load in any dependent
 * modules. This function is optional, and the module will still load if
 * it is not present in the module.
 *
 * The second function, which is NOT optional, is syms_of_XXXX, in which
 * all functions that the module will be provided are declared. This
 * function will contain calls to DEFSUBR().
 *
 * The third function, which is also NOT optional, is vars_of_XXXX, in
 * which you declare all variables that the module provides. This
 * function will contain calls to DEFVAR_LISP(), DEFVAR_BOOL() etc.
 *
 * When declaring functions and variables in the syms_of_XXXX and
 * vars_of_XXXX functions, you use the exact same syntax that you
 * would as if this module were being compiled into the pure Emacs.
 *
 * The fourth function, which is optional, is unload_XXXX, in which actions
 * that must be taken to unload the module are listed.  XEmacs will unbind
 * functions and variables for you.  Anything else that must be done should
 * appear in this function.
 *
 * All four of these functions are declared as void functions,
 * taking no parameters.
 */

#if 0
void
modules_of_earl ()
{
}
#endif

void
syms_of_earl ()
{
  INIT_LRECORD_IMPLEMENTATION (session_handle);

  DEFSUBR (Fmake_session_handle);
  DEFSUBR (Fsession_handle_p);
  DEFSUBR (Fsession_handle_live_p);
  DEFSYMBOL_MULTIWORD_PREDICATE (Qsession_handlep);
  DEFSYMBOL_MULTIWORD_PREDICATE (Qsession_handle_livep);

  DEFSYMBOL (Qearl);
  DEFSYMBOL (Qtransport);
  DEFSYMBOL (Qlast_response_headers);
  DEFSYMBOL (Qlast_response_status);
  DEFSYMBOL (Qcoding_system);
  DEFSYMBOL (Qurl);
}

void
vars_of_earl ()
{

  Fprovide (Qearl);

#if 0
#ifdef HAVE_SHLIB
     #### Anything to do here? */
#endif
#endif
}

#ifdef HAVE_SHLIB
void
unload_earl ()
{
  /* If we create any new types by INIT_LRECORD_IMPLEMENTATION (sample_type),
     then UNDEF_LRECORD_IMPLEMENTATION (sample_type) must appear here.  Also,
     any symbols declared with DEFSYMBOL (Qsample_var) or a variant, must
     have a corresponding unstaticpro_nodump (&Qsample_var) here. */
  UNDEF_LRECORD_IMPLEMENTATION (session_handle);

  unstaticpro_nodump (&Qearl);
  unstaticpro_nodump (&Qtransport);
  unstaticpro_nodump (&Qlast_response_headers);
  unstaticpro_nodump (&Qlast_response_status);
  unstaticpro_nodump (&Qurl);
  unstaticpro_nodump (&Qcoding_system);
  /* predicate special handling */
  unstaticpro_nodump (&Qsession_handlep);
  unstaticpro_nodump (&Qsession_handle_livep);
}
#endif
