/*
 * Lisp interface to libneon for XEmacs.
 *
 * Copyright (C) 2005, 2006 Stephen J. Turnbull <stephen@xemacs.org>
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
 * Creation-Date:	2005-12-04
 * Last-Modified:	2006-01-03
 */

/* TODO
36 matches for "####" in buffer: neon_api.c
     71:  cURL docs: URL_HANDLE identifiers renamed to SESSION_HANDLE
     85:  doc and review: the whole neon API
    XXX:  reorganize this file
    XXX:  refactor: earl module
   1458:  These functions will move to the earl module.
   1481:  These symbols will move to the earl module.
    184:  transport-specific pointer blocks should be one pointer to structures
    XXX:  implement property handling
    284:  Use DEFINE_LRECORD_IMPLEMENTATION_WITH_PROPS?
    336:  need sane property handling!
    350:  extract properties from the curl_handle and add to retval here
    193:  currently a list, make it a plist?
    198:  do we ever actually use this?  should we?
    XXX:  big_ball_of_strings:
    212:  initialize the big_ball_of_strings here.
    258:  free the big_ball_of_strings here.
    401:  This string needs to be freed.
    378:  This interface may change to (&rest PLIST).
    380:  This is not called "make-neon-handle" because a more general module,
    XXX:  review coding-system usage, especially the session member
    189:  do we ever actually use this?  should we?
    391:  Quick hack, should be Qnative?
    521:  return Qnil;		// maybe should be Qt?
    531:  should we get the neon error string here, or provide an API?
    818:  This strategy has problems, because if the operation fails, the
    934:  abort if EQ (current, header) here?
    935:  abort if NILP (Fcdr (current)) here?
   1005:  the return code should be checked here, perhaps?
   1073:  ugly hack
   1090:  these are user errors, maybe should return non-zero to libneon?
   1277:  unfortunately we can't set RESPONSE here
   1315:  Use eistrings here?
   1316:  MEMORY LEAK!!
   1356:  Default is?
   1379:  the errors in the following switch will leak an ne_xml_parser
   1505:  These aren't used but could be useful, see curl_api.
   1537:  Anything to do here?  Nothing interesting is documented.
   1554:  Anything to do here?  Nothing interesting is documented.
*/

/* Commentary: see neon_api.h for commentary on provided API. */

/* API principles
 *
 * Both cURL and libneon manage HTTP sessions.  In cURL, this is implicit,
 * connections and session information are cached.  libneon manages them
 * explicitly.
 *
 * #### I have renamed the URL_HANDLE identifiers to SESSION_HANDLE, but 
 * need to document that for cURL, typical sessions are degenerate (a
 * single request).
 *
 * It seems worthwhile to keep the request members of the handle structure,
 * but treat them as cached or default, as in many cases there will be
 * substantial commonality.
 *
 * For the general interface, we have the problem that we need to clean up
 * various objects but may not have the facility linked.  This means that
 * the general module would need to provide various slots to hold functions
 * to initialize, manipulate, and finalize various components of the
 * session_handle structure.  See comment in session_finalize() below.
 *
 * #### From the point of view of the LISP programmer, the neon API below
 * really sucks badly.  Probably the &rest syntax for make-session-handle
 * with defined keys
 *
 * :transport       'neon
 * :url             URL-STRING
 * :url-coding      CODING-SYSTEM
 * :method          METHOD-SYMBOL
 * :acceptor        ACCEPTOR-SYMBOL
 * :reader          BUFFER-OR-READER-SYMBOL    // generic callback stack?
 * :reader-coding   CODING-SYSTEM
 * :headers         LIST-OF-HEADER-SPECS
 *
 * is good, but semantically we need to arrange that a LISP programmer need
 * not worry about reinitializing the handle for reuse, etc.  For reuse, we
 * need APIs to replace method, acceptor, and reader, change the coding
 * systems (bonus points for allowing recoding of existing results!), and
 * a flexible editor for the header list.
 */

#include <config.h>
#include "lisp.h"
#ifdef HAVE_SHLIB
# include "emodules.h"
#endif
#include "file-coding.h"
#include "lstream.h"
#include "elhash.h"

/* #include <stdio.h> */
#include "sysfile.h"
#ifdef HAVE_EARL
# include "earl.h"
#endif
#include "neon_api.h"		/* causes inclusion of libneon headers:
				   ne_request.h, ne_session.h, ne_string.h,
				   ne_utils.h, ne_defs.h, ne_ssl.h,
				   ne_uri.h, ne_xml.h */

/* feature control macros */

/* the #define should move to config.h if we need it
   these two features are mutually exclusive? */
#define NEON_USES_HEADER_ITERATE
#undef NEON_USES_HEADER_CATCHER
/* possible API improvement */
#undef LAZY_INITIALIZATION_IN_REQUEST

/* callback declarations */

static Lisp_Object neon_status (int status);
static Extbyte *neon_prepare_path (Lisp_Object path, Lisp_Object codesys);
static int neon_prepare_depth (Lisp_Object depth, int kidz_ok);
static Lisp_Object neon_prepare_http_status (ne_request *neon);
#ifdef NEON_USES_HEADER_CATCHER
static void neon_header_catcher (void *userdata, const char *value);
#endif
static int neon_start_cb (void *userdata, int UNUSED(parent),
			  const char *nspace, const char *name,
			  const char **atts);
static int neon_cdata_cb (void *userdata, int UNUSED(state),
			  const char *cdata, size_t len);
static int neon_end_cb (void *userdata, int UNUSED(state),
			const char *UNUSED(nspace), const char *UNUSED(name));
static int neon_write_lstream (void *stream, const char *data, size_t count);
static int neon_credentials_cb (void *userdata, const char *rlm,
				int at, char *username, char *password);

/* Local references to Lisp symbols */

static Lisp_Object Qneon_api, Qneon, Qinfinite, Qwebdav_xml, Qaccept_always,
#ifndef HAVE_EARL
  Qlast_response_headers, Qlast_response_status,
  Qsession_handlep, Qsession_handle_livep, Qtransport,
  Qurl, Qcoding_system,
#endif
  Qaccept_2xx, Qauthorization_failure, Qproxy_authorization_failure,
  Qconnection_failure, Qtimeout, Qgeneric_error;

/************************************************************************/
/*                   neon-specific structure handling                   */
/* Contents:								*/
/*   struct neon_data							*/
/*   finalize_neon_data							*/
/************************************************************************/

struct neon_data *allocate_neon_data ()
{
  return (struct neon_data *) xmalloc (sizeof (struct neon_data));
}

void finalize_neon_data (struct neon_data *neon)
{
  if (neon != NULL)
    {
      /* Maybe order doesn't matter, but let's take no prisoners^Wchances! */
      if (neon->parser != NULL)
	{
	  ne_xml_destroy (neon->parser);
	  neon->parser = NULL;
	}
      if (neon->request != NULL)
	{
	  ne_request_destroy (neon->request);
	  neon->request = NULL;
	}
      if (neon->request != NULL)
	{
	  ne_request_destroy (neon->request);
	  neon->request = NULL;
	}
    }
  neon = NULL;
}

#ifndef HAVE_EARL
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
  { XD_LISP_OBJECT, offsetof (struct Lisp_Session_Handle, stuff) },
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
  session_handle->stuff = Qnil;
#ifdef HAVE_CURL
  session_handle->curl = 0;
#endif
#ifdef HAVE_NEON
  session_handle->neon = 0;
#endif
  /* #### UNIMPLEMENTED we need to initialize the big_ball_of_strings here. */
  return session_handle;
}

static void
finalize_session_handle (void *header, int for_disksave)
{
  Lisp_Session_Handle *session_handle = (Lisp_Session_Handle *) header;

  if (for_disksave)
    invalid_operation ("Can't dump an emacs containing SESSION_HANDLE objects",
		       wrap_session_handle (session_handle));

  /* These could be a linked list of low-level-module-specific structures.
     Actually, a single session_handle->handler_info member to be cast to
     `TRANSPORT_handler_info *' should be enough, since we know the transport.  This
     would allow resetting transport, too. */
  /* This kind of task could be handled by having an array of finalizers,
     indexed by enum lowlevel { min_lowlevel=0, curl=0, neon, max_lowlevel }
     and called as
         for (ll = min_lowlevel; ll < max_lowlevel; ll++)
	   if (finalizers[ll])
	     (*finalizers[ll]) (session_handle);
  */

  finalize_neon_data (session_handle->neon);

  /* #### UNIMPLEMENTED we need to free the big_ball_of_strings here. */
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
  mark_object (XSESSION_HANDLE (obj)->state);
  return XSESSION_HANDLE (obj)->stuff;
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

  write_c_string (printcharfun, "#<session_handle ");
  if (NILP(session_handle->transport))
    write_c_string (printcharfun, "(dead) ");
  else
    write_fmt_string_lisp (printcharfun, "%S ", 1, session_handle->transport);
  if (session_handle->url)
    write_fmt_string_lisp (printcharfun, "%S", 1, session_handle->url);
  write_fmt_string (printcharfun, " 0x%lx>", (unsigned long) session_handle);
}

static Lisp_Object
session_handle_get (Lisp_Object session, Lisp_Object prop, Lisp_Object defalt)
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
  /* #### should we do curl_get_info here? */

  return Fplist_get (s->plist, prop, defalt);
}

static Lisp_Object
session_handle_put (Lisp_Object session, Lisp_Object prop, Lisp_Object value)
{
  Lisp_Session_Handle *s = XSESSION_HANDLE (session);

  if (EQ (prop, Qlast_response_headers)
      || EQ (prop, Qlast_response_status)
      || EQ (prop, Qurl)
      || EQ (prop, Qcoding_system)
      || EQ (prop, Qtransport))
    invalid_change ("read-only property", prop);

  external_plist_put (&s->plist, prop, value, 0, ERROR_ME);

  return value;
}

static Lisp_Object
session_handle_remprop (Lisp_Object session, Lisp_Object prop)
{
  Lisp_Session_Handle *s = XSESSION_HANDLE (session);

  if (EQ (prop, Qlast_response_headers)
      || EQ (prop, Qlast_response_status)
      || EQ (prop, Qurl)
      || EQ (prop, Qcoding_system)
      || EQ (prop, Qtransport))
    invalid_change ("read-only property", prop);

  return external_remprop (&s->plist, prop, 0, ERROR_ME) ? Qt : Qnil;
}

static Lisp_Object
session_handle_plist (Lisp_Object session)
{
  Lisp_Object retval;
  Lisp_Session_Handle *s;

  CHECK_SESSION_HANDLE (session_handle);
  s = XSESSION_HANDLE (session_handle);

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

DEFUN ("session-handle-live-p", Fsession_handle_live_p, 1, 1, 0, /*
Return non-nil if SESSION_HANDLE is an active SESSION_HANDLE connection.
*/
       (session_handle))
{
  CHECK_SESSION_HANDLE (session_handle);
  return Fget (session_handle, Qtransport);
}
#endif /* !HAVE_EARL */


/************************************************************************/
/*			    Lisp API functions				*/
/* Contents:								*/
/*   neon-make-session-handle						*/
/************************************************************************/

#ifndef LAZY_INITIALIZATION_IN_REQUEST
DEFUN ("neon-make-session-handle", Fneon_make_session_handle, 1, 2, 0, /*
Return a neon session for URL, wrapped in a session handle.
URL is a string, which must be a URI scheme known to neon.  Any path
  component (as defined in RFC 2396) will be ignored.
Optional argument CODESYS (default `utf-8') is used to encode URL.
URL must not be URL-encoded; that will be done automatically.

#### This API may go away in favor of the generic `make-session-handle'
with lazy initialization of neon-specific session attributes in
`neon-request-create'.
*/
       (url, codesys))
{
  /* validates URL and CODESYS */
  Lisp_Object session = Fmake_session_handle (url, codesys);
  Lisp_Session_Handle *handle = XSESSION_HANDLE (session);

  /* neon-specific
     #### maybe all of this can be done lazily in neon-request-create? */
  {
    ne_uri u;
    Extbyte *ru = NEW_LISP_STRING_TO_EXTERNAL (url, handle->coding_system);
    if (ne_uri_parse (ru, &u))
      signal_error (Qio_error, "neon: couldn't parse URL", url);
    if (!u.scheme)
      signal_error (Qio_error, "neon: no scheme in URL", url);
    if (!u.host)
      signal_error (Qio_error, "neon: no host in URL", url);
    if (!u.port && !(u.port = ne_uri_defaultport (u.scheme)))
      signal_error (Qio_error, "neon: could not determine port for URL", url);

    handle->neon = allocate_neon_data ();
    handle->neon->session = ne_session_create (u.scheme, u.host, u.port);
    ne_uri_free (&u);
  }
  handle->state = Fmake_vector (make_int (NEON_STATE_SIZE), Qnil);
  handle->transport = Qneon;

  return wrap_session_handle (handle);
}
#endif

/************************************************************************/
/*			   Session authentication			*/
/* Contents:								*/
/*   neon-session-set-auth						*/
/*   neon-session-forget-auth						*/
/************************************************************************/

/* Authentication
 *
 * libneon comment:
 *
 * The callback used to request the username and password in the given
 * realm. The username and password must be copied into the buffers
 * which are both of size NE_ABUFSIZ.  The 'attempt' parameter is zero
 * on the first call to the callback, and increases by one each time
 * an attempt to authenticate fails.
 *
 * The callback must return zero to indicate that authentication
 * should be attempted with the username/password, or non-zero to
 * cancel the request. (if non-zero, username and password are
 * ignored.)
 *
 * XEmacs API:
 *
 * CALLBACK is a funcallable which must take exactly two arguments, a string
 * REALM and an integer ATTEMPT, and return a cons of two strings.  The car
 * will be interpreted as the username and the cdr as the password.  REALM
 * is the HTTP authentication realm, and ATTEMPT counts the tries for
 * authentication made.  All strings are in the `binary' coding (ie, the
 * callback must translate in both directions).
 *
 * As of neon 0.24.7, these methods are *not* idempotent.  libneon
 * registers an internal hook which frees the credential storage which
 * has a single ID per request, but the hook can be registered
 * multiple times ... and will try to free the storage multiple times.
 * Ba-a-ad libeon, bad, bad libneon!
 *
 * Also, we can't simply always ne_forget_auth before adding; that method
 * always clears both.  So we combine these two functions into a single
 * function and require that both callbacks be specified (of course
 * specifying nil is OK).  Then we can always explicitly clear the existing
 * authentication information in preparation.
 */

DEFUN ("neon-session-set-auth", Fneon_session_set_auth, 3, 3, 0, /*
Set the authentication methods for SESSION to SERVER-CB and PROXY-CB.
If non-nil, SERVER_CB and PROXY_CB must be funcallables which take exactly
two arguments, a string REALM and an integer ATTEMPT, and return a cons of
two strings.  REALM is the HTTP authentication realm, and ATTEMPT counts
the tries for authentication made.  The car of the return value is
interpreted as the username and the cdr as the password.  Both are treated
as binary data.

WARNING: This function clears any previous authentication methods, so if
you need both server authentication and proxy authentication, you must set
both in a single call.  \(This is not a bug in XEmacs, it is a limitation of
libneon.)

This function is a no-op if both callbacks are nil.  Use `neon-forget-auth'
to clear all credentials without resetting them.

Returns no useful value \(nil).
*/
       (session, server_cb, proxy_cb))
{
  Lisp_Session_Handle *s;

  /* sanity check the session */
  CHECK_SESSION_HANDLE (session);
  s = XSESSION_HANDLE (session);
  if (!EQ (s->transport, Qneon))
    wtaerror ("URL handle is not a neon handle", session);
  if (!s->neon || !s->neon->session)
    invalid_state ("session not open", session);

  /* sanity check the callbacks */
  if (!NILP (server_cb))
    if (NILP (Ffunctionp (server_cb))
	|| XINT (Ffunction_max_args (server_cb)) != 2
	|| XINT (Ffunction_min_args (server_cb)) != 2)
      signal_error (Qwrong_type_argument,
		    "server credential is not a function of two arguments",
		    server_cb);
  if (!NILP (proxy_cb))
    if (NILP (Ffunctionp (proxy_cb))
	|| XINT (Ffunction_max_args (proxy_cb)) != 2
	|| XINT (Ffunction_min_args (proxy_cb)) != 2)
      signal_error (Qwrong_type_argument,
		    "proxy credential is not a function of two arguments",
		    proxy_cb);

  /* clear previous credentials to avoid registering cleanup callback
     multiple times */
  if (!NILP (server_cb) || !NILP (proxy_cb))
    ne_forget_auth (s->neon->session);

  /* set the callbacks */
  if (!NILP (server_cb))
    {
      ne_set_server_auth (s->neon->session,
			  &neon_credentials_cb,
			  (void *) server_cb);
      Faset (s->state, make_int (SERVER_CB), server_cb);
    }
  if (!NILP (proxy_cb))
    {
      ne_set_proxy_auth (s->neon->session,
			 &neon_credentials_cb,
			 (void *) proxy_cb);
      Faset (s->state, make_int (PROXY_CB), proxy_cb);
    }

  return Qnil;
}

DEFUN ("neon-session-forget-auth", Fneon_session_forget_auth, 1, 1, 0, /*
Clear authentication information for SESSION.
Returns nil.
*/
       (session))
{
  Lisp_Session_Handle *s;

  CHECK_SESSION_HANDLE (session);
  s = XSESSION_HANDLE (session);
  if (!EQ (s->transport, Qneon))
    wtaerror ("not a neon session", session);
  if (!s->neon || !s->neon->session)
    invalid_state ("session not open", session);
  ne_forget_auth (s->neon->session);
  return Qnil;
}

/* WebDAV functionality
 *
 * As mentioned, libneon does very little.  Ga-a-ack! that's not true.
 * It's just that (as you'd expect of a CADT product) none of the high-level
 * functionality is documented.  Here's a brief roadmap to functionality.
 * Size  File           Description
 *  4123 ne_207.h       Handling 207 Multi-Status responses
 *  1372 ne_acl.h       Set ACL on resource
 *  1872 ne_alloc.h     (internal) memory allocation functions
 *  2445 ne_auth.h      Set and clear credentials for server and/or proxy
 *  4908 ne_basic.h     HTTP GET, PUT, POST; WebDAV MOVE, COPY, DELETE, etc
 *  1718 ne_compress.h  Set a reader callback for compressed resources
 *  1446 ne_cookies.h   Declare cookie data structure and cache
 *  1635 ne_dates.h     Date parsers; RFC 1123 current time formatter
 *  1255 ne_defs.h      (internal) compiler parametrization
 *  1114 ne_i18n.h      (internal) gettext internationalization
 *  6328 ne_locks.h     WebDAV lock management
 *  4961 ne_md5.h       (internal) compiler/system parametrization
 *  9724 ne_props.h     WebDAV property access and setting
 *  1448 ne_redirect.h  Enable redirect handling
 * 10880 ne_request.h   Request handling
 *  7514 ne_session.h   Session handling
 *  6691 ne_socket.h    Socket and DNS utilities
 *  6060 ne_ssl.h       SSL certificates and authentication
 *  5112 ne_string.h    (internal) string and "dynarr" handling utilities
 *  2830 ne_uri.h       (internal) URI handling
 *  3469 ne_utils.h     (internal) neon version, status parsing, etc
 *  5589 ne_xml.h       XML parser API
 *
 * The basic interfaces
 *
 * HTTP GET, PUT, POST
 * WebDAV COPY, MOVE, DELETE, MKCOL
 *
 * The libneon functions basically take a session and a path (or two paths)
 * and maybe some auxiliary parameters.  When operating on a collection, can
 * be preceded by a depth-header-adding function.
 *
 * COPY and MOVE are special cases since they require a second path (URL?).
 * Should we treat source and target symmetrically (and require two paths),
 * or default one of them to the session's internal path?  If we default at
 * all, I think we should have the source defaulted, and the target explicit.
 *
 * In practice, it looks infeasible to reuse neon requests.  So I think that
 * all requests should have explicit paths.  For simple, one-off requests
 * we can provide a make-request API for neon, too, but the make-session
 * API should ignore any path component.  Something like
 *
 *     (make-session URI)
 *     (make-request SESSION URI)
 *
 * where SESSION can be nil.  (make-request URI &optional SESSION) is a bit
 * dubious.  If SESSION is nil, how is a relative URI to be interpreted?  If
 * URI contains a host portion which is different from SESSION's, how is
 * to be treated?
 */

/* ne_request_dispatch: Sends the given request, and reads the
 * response. Response-Status information can be retrieve with
 * ne_get_status(req).
 * #### need to be updated for neon 0.25.4.
 *
 *  NE_OK         if request sent + response read okay.
 *  NE_AUTH       user not authorised on server
 *  NE_PROXYAUTH  user not authorised on proxy server
 *  NE_CONNECT    could not connect to server/proxy server
 *  NE_TIMEOUT    connection timed out mid-request
 *  NE_ERROR      for other errors, and ne_get_error() should
 *                  return a meaningful error string
 */

/************************************************************************/
/*			   neon convenience APIs			*/
/* Contents:								*/
/*   neon-get-file							*/
/*   neon-put-file							*/
/*   neon-post-get-file							*/
/*   neon-delete							*/
/*   neon-mkcol								*/
/*   neon-copy								*/
/*   neon-move								*/
/************************************************************************/

/* It's not worth providing an API for ne_options, since it will only tell
   you about Classes 1 and 2 of WebDAV compliance and the mod_dav executable
   feature.  It doesn't support deltaV's "version-control" feature, RFC 3648's
   "ordered-collections" feature, or RFC 3744's "access-control" feature. */

DEFUN ("neon-get-file", Fneon_get_file, 3, 4, 0, /*
Get the URL described by SESSION and PATH, and put it in FILE.
FILE is the name of a local file.  Output to it will be encoded using
`file-name-coding-system'.  It will be silently overwritten if it exists.
Optional CODESYS is used to encode the string PATH, which is then URL-encoded
internally.  The response is saved to FILE as binary.
CODESYS defaults to `utf-8'.

This is a stupid function to have, provided only because it was the quickest
test that neon worked at all.
*/
      (session, path, file, codesys))
{
  int status;
  ne_session *ns = XSESSION_HANDLE (session)->neon->session;
  Extbyte *p = neon_prepare_path (path, codesys);
  int fd = qxe_interruptible_open (XSTRING_DATA (file),
				   O_WRONLY | OPEN_BINARY | O_CREAT | O_TRUNC,
				   0644);
  if (fd >= 0)
    {
      status = ne_get (ns, p, fd);
      retry_close (fd);
    }
  else
    signal_error (Qfile_error, "couldn't open for write", file);

  return neon_status (status);
}

DEFUN ("neon-put-file", Fneon_put_file, 3, 4, 0, /*
To the URL described by SESSION and PATH, put the contents of FILE.
FILE is the name of a local file.  Input from it will be decoded using
`file-name-coding-system'.
Optional CODESYS is used to encode the string PATH, which is then URL-encoded
internally.  The contents of FILE are treated as binary.
CODESYS defaults to `utf-8'.

This is a stupid function to have, provided only because it was trivial once
neon-get-file was coded.
*/
      (session, path, file, codesys))
{
  int status;
  ne_session *ns = XSESSION_HANDLE (session)->neon->session;
  Extbyte *p = neon_prepare_path (path, codesys);
  int fd = qxe_interruptible_open (XSTRING_DATA (file),
				   O_RDONLY | OPEN_BINARY,
				   0644);
  if (fd >= 0)
    {
      status = ne_put (ns, p, fd);
      retry_close (fd);
    }
  else
    signal_error (Qfile_error, "couldn't open for read", file);

  return neon_status (status);
}

DEFUN ("neon-post-get-file", Fneon_post_get_file, 4, 5, 0, /*
To the URL described by SESSION and PATH, post STRING, save response in FILE.
FILE is the name of a local file.  Output to it will be encoded using
`file-name-coding-system'.  It will be silently overwritten if it exists.
Optional CODESYS is used to encode the string PATH, which is then URL-encoded
internally.  The contents of STRING are treated as binary, and the response
is saved to FILE as binary.  CODESYS defaults to `utf-8'.

Yes, of course STRING should be STRING-OR-BUFFER.  Send money!

This is a stupid function to have, provided only because it was trivial once
neon-get-file was coded.
*/
       (session, path, string, file, codesys))
{
  int status;
  ne_session *ns = XSESSION_HANDLE (session)->neon->session;
  Extbyte *b = NEW_LISP_STRING_TO_EXTERNAL (string, Qbinary);
  Extbyte *p = neon_prepare_path (path, codesys);
  int fd = qxe_interruptible_open (XSTRING_DATA (file),
				   O_WRONLY | OPEN_BINARY | O_CREAT | O_TRUNC,
				   0644);
  if (fd >= 0)
    {
      status = ne_post (ns, p, fd, b);
      retry_close (fd);
    }
  else
    signal_error (Qfile_error, "couldn't open for write", file);

  return neon_status (status);
}

DEFUN ("neon-delete", Fneon_delete, 2, 3, 0, /*
Delete the resource described by SESSION and PATH.
Optional CODESYS is used to encode the string PATH, which is then URL-encoded
internally.  CODESYS defaults to `utf-8'.
*/
      (session, path, codesys))
{
  ne_session *ns = XSESSION_HANDLE (session)->neon->session;
  Extbyte *p = neon_prepare_path (path, codesys);
  int status = ne_delete (ns, p);
  return neon_status (status);
}

DEFUN ("neon-mkcol", Fneon_mkcol, 2, 3, 0, /*
Make a collection at the resource described by SESSION and PATH.
Optional CODESYS is used to encode the string PATH, which is then URL-encoded
internally.  CODESYS defaults to `utf-8'.
*/
      (session, path, codesys))
{
  ne_session *ns = XSESSION_HANDLE (session)->neon->session;
  Extbyte *p = neon_prepare_path (path, codesys);
  int status = ne_mkcol (ns, p);
  return neon_status (status);
}

DEFUN ("neon-copy", Fneon_copy, 3, 6, 0, /*
Copy the URL described by SESSION and path SOURCE to path TARGET.
Optional DEPTH may be 0 or `infinity', which adds an appropriate depth
header to the request.  If nil, no depth header will be added.
Optional OVERWRITE, if non-nil, enables overwriting the resource (tree) at
TARGET.  Otherwise the request fails if a resource exists at TARGET.
Optional CODESYS is used to encode the strings SOURCE and TARGET, which are
then URL-encoded internally.  CODESYS defaults to `utf-8'.
*/
       (session, source, target, depth, overwrite, codesys))
{
  ne_session *ns = XSESSION_HANDLE (session)->neon->session;
  Extbyte *s = neon_prepare_path (source, codesys);
  Extbyte *t = neon_prepare_path (target, codesys);
  int d = neon_prepare_depth (depth, 0);
  int status = ne_copy (ns, (NILP (overwrite) ? 0 : 1), d, s, t);

  return neon_status (status);
}

DEFUN ("neon-move", Fneon_move, 3, 5, 0, /*
Move the URL described by SESSION and path SOURCE to path TARGET.
Optional OVERWRITE, if non-nil, enables overwriting the resource (tree) at
TARGET.  Otherwise the request fails if a resource exists at TARGET.
Optional CODESYS is used to encode the strings SOURCE and TARGET, which are
then URL-encoded internally.  CODESYS defaults to `utf-8'.
*/
      (session, source, target, overwrite, codesys))
{
  ne_session *ns = XSESSION_HANDLE (session)->neon->session;
  Extbyte *s = neon_prepare_path (source, codesys);
  Extbyte *t = neon_prepare_path (target, codesys);
  int status = ne_move (ns, (NILP (overwrite) ? 0 : 1), s, t);

  return neon_status (status);
}

/************************************************************************/
/*			   neon request handling			*/
/************************************************************************/

/* Property handling
 *
 * It doesn't seem terribly useful to support neon-style property handling.
 * Neon does provide some convenient functions for returning single simple
 * properties.  But given the model for XML proposed below, so does LISP:
 * `caar'.  For structured properties, though, you just get the XML to
 * parse.  The exception is 207 Multistatus responses, where neon provides
 * a return value which indicates all-success vs. one or more failures.
 * Nothing to sneeze at if you're a C programmer, but since we will provide
 * an XML-to-LISP converter anyway it seems hardly useful.
 */

/* XML interface
 *
 * This interface provides very limited access to the underlying parser.
 * So there's really not going to be much here, in fact, it's going to be
 * limited to constructing a tree.  (This is a Lisp-y version of Fredrik
 * Lundh's ElementTree API.)  Later we may look at providing a generic stack
 * of SAX handlers API but that seems kinda stupid, when we can expose
 * something a lot more general via eXpat (and maybe libxml, but I'm getting
 * pretty frustrated with GNOME products).
 *
 * The initial try will construct a tree using LISP lists according to this
 * grammar:
 * ROOT : NODE
 * NODE : ( ELEMENT ATTRIBLIST [NODE|PCDATA]* )
 * ELEMENT : string
 * ATTRIBLIST : ( [KEY VALUE]* )
 * KEY : string
 * VALUE : string
 * PCDATA : string
 * The notation [X|Y] means X or Y, the brackets are for grouping and are
 * not present in the content.  The notation [X Y] means X followed by Y,
 * the brackets are for grouping and are not present in the content.
 *
 * It should be possible with a very similar approach to construct a buffer
 * with elements demarcated by extents.  N.B. with a start-closed, end-closed
 * extent, we automatically get the desired nesting.
 *
 * Algorithm
 *
 * The userdata element for a handler is a LISP array.  Indices are defined
 * in enum neon_state_index (need to be converted with make_int).
 * Initialized to userdata[CURRENT] = userdata[RESPONSE] = Fcons (Qnil, Qnil).
 * (Since we're going to special-case anyway, it may be slightly more
 * efficient to initialize current = header = Qnil or Qunbound.)
 *
 * The libneon API associates three callbacks and a userdata block with
 * each handler:
 *   void ne_xml_push_handler(ne_xml_parser *p,
 *                            ne_xml_startelm_cb *startelm, 
 *                            ne_xml_cdata_cb *cdata,
 *                            ne_xml_endelm_cb *endelm,
 *                            void *userdata);
 * The callbacks have these types:
 *   typedef int ne_xml_startelm_cb(void *userdata, int parent,
 *                                  const char *nspace, const char *name,
 *                                  const char **atts);
 *   typedef int ne_xml_cdata_cb(void *userdata, int state,
 *                               const char *cdata, size_t len);
 *   typedef int ne_xml_endelm_cb(void *userdata, int state, 
 *                                const char *nspace, const char *name);
 * where the integers parent and state identify element types.
 *
 * We will have only one handler.  Its callbacks are:
 * startelm:
 *   Set car of current to a new list (name atts . current).
 *   Set current to the cdr of the new list.
 * cdata:
 *   Set cdr of current to (cons cdata (cdr current)).
 * endelm:
 *   Set (cdr current) to nil.
 *   Set current to (cdr current).
 */

/* XML body reader
   The reader callback for the request is the generic ne_xml_parse_v
   provided by libneon.
   We provide three handler callbacks.
   We build a tree whose nodes are lists of the form
   ( NAME NAMESPACE ATTR-LIST CHILD* )
   where ATTR-LIST is a (possibly nil) plist with string keys and string
   values, and each CHILD may be a string (cdata) or a node.
   The algorithms use an auxiliary pointer `current' to the last cons of
   the most recently opened element (which always exists, since NAMESPACE,
   NAME, and ATTR-LIST are required), and an auxiliary structure, namely a
   pointer to the last element of the parent node, which is kept in the
   cdr of current.
   #### This strategy has problems, because if the operation fails, the
   tree is left in a bogus (cyclic) state.
   neon_start_cb() expects an initial state where response = current =
   Fcons (Qnil, Qnil).  The response will actually be built in
   (Fcar (Fcdr (response)).

   Algorithms in LISP:

   ;; element start callback
   (defun start-cb (state ns nm ap)
     (let ((current (aref state 1)))
       (setcdr current (cons (list nm ns ap) (cdr current)))
       (setq current (cdr current))
       (let ((tail (cdr (cdr (car current)))))
         (setcdr tail current)
	 (aset state 1 tail)))
     1)

   ;; cdata callback
   (defun cdata-cb (state cdata)
     (let ((current (aref state 1)))
       (setcdr current (cons cdata (cdr current)))
       (aset state 1 (cdr current)))
     1)

   ;; element end callback
   (defun end-cb (state)
     (let ((current (aref state 1)))
       (aset state 1 (cdr current))
       (setcdr current nil))
     1)

   ;; initialize state
   (let ((header (cons nil nil)))
     (setq state (make-vector 2 header)))
*/

/* low-level request handling */

DEFUN ("neon-request-create", Fneon_request_create, 3, 4, 0, /*
Create a request for session handle SESSION using METHOD on PATH.
Optional CODESYS is used to encode the strings METHOD and PATH.
CODESYS defaults to `utf-8'.
If SESSION already has an associated request, it is destroyed, and any
response status and headers are cleared.  Returns SESSION.
*/
       (session, method, path, codesys))
{
  Lisp_Session_Handle *s;

  CHECK_STRING (method);
  CHECK_STRING (path);
  CHECK_SESSION_HANDLE (session);
  s = XSESSION_HANDLE (session);

#ifdef LAZY_INITIALIZATION_IN_REQUEST
  if (NILP (s->transport))
    {
      ne_uri u;
      Extbyte *ru = NEW_LISP_STRING_TO_EXTERNAL (url, s->coding_system);
      if (ne_uri_parse (ru, &u))
	signal_error (Qio_error, "neon couldn't parse URL", url);
      if (!u.scheme)
	signal_error (Qio_error, "no scheme in URL", url);
      if (!u.host)
	signal_error (Qio_error, "no host in URL", url);
      if (!u.port && !(u.port = ne_uri_defaultport (u.scheme)))
	signal_error (Qio_error, "could not determine port for URL", url);

      s->neon = allocate_neon_data ();
      s->neon->session = ne_session_create (u.scheme, u.host, u.port);
      ne_uri_free (&u);
      s->state = Fmake_vector (make_int (NEON_STATE_SIZE), Qnil);
      s->transport = Qneon;
    }
#endif

  if (s->transport != Qneon)
    wtaerror ("not a neon session", session);
  if (s->neon == NULL || s->neon->session == NULL)
    invalid_state ("neon session not initialized", session);
  if (NILP (codesys))
    codesys = Fget_coding_system (Qutf_8);
  else
    codesys = Fget_coding_system (codesys);

  {
    Extbyte *m = NEW_LISP_STRING_TO_EXTERNAL (method, codesys);
    Extbyte *p = NEW_LISP_STRING_TO_EXTERNAL (path, codesys);

    /* we can't be sure a neon-request-dispatch was done */
    if (s->neon->request)
      ne_request_destroy (s->neon->request);
    s->neon->request = ne_request_create (s->neon->session, m, p);
    /* clear results of last request */
    s->last_response_headers = Qnil;
    s->last_response_status = Qnil;
#ifdef HAVE_NEON_0_24_7
    ne_add_response_header_catcher (s->neon->request,
				    &neon->header_catcher,
				    &(s->last_response_headers));
#endif
  }

  return session;
}

/* We break out various low-level APIs such as ne_add_response_body_reader()
 * and ne_set_request_body_buffer() into separate DEFUNs.  They take Lisp
 * arguments and wrap neon APIs around them.
 *
 * WebDAV-specific requests dispatch on the method.  They specify the return
 * format, eg, if it's a PROPFIND, we know it's going to return WebDAV XML.
 * `neon-request-dispatch' returns an object containing the formatted data.
 */

/* 'acceptance' callback type. Return non-zero to accept the response,
 * else zero to ignore it. */
/*
typedef int (*ne_accept_response)(
    void *userdata, ne_request *req, const ne_status *st);
*/

DEFUN ("neon-add-response-body-reader", Fneon_add_response_body_reader, 2, 4, 0, /*
Give neon request REQUEST the response reader READER and acceptor ACCEPTOR.
Optional ACCEPTER is one of the symbols `accept-always' or `accept-2xx',
  defaulting to `accept-always'.  (This may change as the module is debugged.)
Optional CODESYS, default `utf-8', is used to decode response.
READER may be a buffer or symbol, with the following semantics:
  buffer        insert the response at point and return the buffer
  `webdav-xml'  return the response as a WebDAV XML parse tree (LISP list)
Returns nil.

The webdav-xml format is simple: each element is parsed into a list of the
form ( NAME NAMESPACE ATTRIBUTE-PLIST CHILD* ) where each child may be a
list (a parsed element) or a string (CDATA).

libneon doesn't coalesce adjacent cdata into a single cdata (in fact it
seems to always return vertical whitespace as a component separate from
other whitespace or text).  It also doesn't bother to coalesce across
internal buffer boundaries, so a word may be split in the middle:
"[...1018 characters]<el>word</el>" can result in "word" being split into
"wo" and "rd", apparently because the buffer is 1KB in length.  (This would
be hideous if it were UTF-8!)  (Verified for libneon 0.24.7.)
*/
       (request, reader, accepter, codesys))
{
  ne_request *neon;
  Lisp_Session_Handle *r;
  ne_accept_response accept_filter;

  CHECK_SESSION_HANDLE (request);
  r = XSESSION_HANDLE (request);
  if (!EQ (r->transport, Qneon))
    wtaerror ("URL handle is not a neon handle", request);
  if (!r->neon || !r->neon->request)
    invalid_state ("URL handle neon request is uninitialized", request);
  if (EQ (accepter, Qaccept_always) || NILP (accepter))
    accept_filter = &ne_accept_always;
  else if (EQ (accepter, Qaccept_2xx))
    accept_filter = &ne_accept_2xx;
  else
    wtaerror ("ACCEPTER must be `accept-always' or `accept-2xx'", accepter);

  neon = r->neon->request;
  codesys = Fget_coding_system (NILP (codesys) ? Qutf_8 : codesys);
  Faset (r->state, make_int (READER), reader);

  /* It should be easy to add other lstream reader types here. */
  /* #### There seem to GC problems with the lstream(s) here.

     Also they are quite sensitive to the build; we should find some way
     to check for the configurations being the same when loading a module.

     I think this means that a proper FFI for Emacs LISP is impossible;
     internal structures like buffers and font instances do change with
     the configuration, and this will cause crashes if there is a config.h
     mismatch (or version skew).
     One possible dodge would be to have a type-specific version/feature
     descriptor in the lrecord_header. */
  if (BUFFERP (reader))
    {
      struct buffer *buf = XBUFFER(reader);
      /* #### maybe neon_write_lstream could do this lazily? */
      Lisp_Object b = make_lisp_buffer_output_stream (buf, buf->bufpt,
						      LSTR_ALLOW_QUIT);
      Lisp_Object c = make_coding_output_stream (XLSTREAM (b), codesys,
						 CODING_DECODE, 0);
      /* #### if we make this a cons of b and c we can delete the lstreams */
      Faset (r->state, make_int (READER_LSTREAM), c);
      ne_add_response_body_reader (neon, accept_filter,
				   &neon_write_lstream, (void *) c);
      /* yes, for buffers the response is the reader */
      Faset (r->state, make_int (RESPONSE), reader);
    }
  else if (EQ (reader, Qwebdav_xml))
    {
      /* ;; initialize state */
      /* (let ((header (cons nil nil))) */
      Lisp_Object header = Fcons (Qnil, Qnil);
      /* (setq state (make-vector 2 header))) */
      Faset (r->state, make_int (RESPONSE), header);
      Faset (r->state, make_int (CURRENT), header);

      /* #### unfortunately we can't set RESPONSE here */
      r->neon->parser = ne_xml_create();
      ne_xml_push_handler(r->neon->parser,
			  &neon_start_cb, &neon_cdata_cb, &neon_end_cb,
			  (void *) r->state);
      ne_add_response_body_reader (neon, accept_filter,
				   &ne_xml_parse_v, (void *) r->neon->parser);
    }
  else
    {
      wtaerror ("reader must be a buffer or the symbol `webdav-xml'", reader);
    }

  return Qnil;
}

DEFUN ("neon-set-request-body-buffer", Fneon_set_request_body_buffer, 2, 3, 0, /*
Give neon request REQUEST the string BODY as the message body.
Optional CODESYS, default `utf-8' is used to encode BODY.
Returns nil.

Yes, of course BODY should be allowed to be a buffer.  Send money!
*/
       (request, body, codesys))
{
  ne_request *neon;
  Lisp_Session_Handle *r;

  CHECK_STRING (body);
  CHECK_SESSION_HANDLE (request);
  r = XSESSION_HANDLE (request);
  if (!EQ (r->transport, Qneon))
    wtaerror ("not a neon session", request);
  if (!r->neon || !r->neon->request)
    invalid_state ("neon request is uninitialized", request);
  neon = r->neon->request;

  {
    /* #### Use eistrings here? */
    /* #### MEMORY LEAK!! */
    Extbyte *b = NEW_LISP_STRING_TO_EXTERNAL_MALLOC (body, codesys);
    size_t sz = strlen (b);
    ne_set_request_body_buffer (neon, b, sz);
  }

  return Qnil;
}

DEFUN ("neon-add-request-header", Fneon_add_request_header, 3, 3, 0, /*
To REQUEST add the NAME header with text VALUE.
The no-conversion coding system is used.
Returns nil.
*/
       (request, name, value))
{
  CHECK_SESSION_HANDLE (request);
  CHECK_STRING (name);
  CHECK_STRING (value);

  if (EQ (XSESSION_HANDLE (request)->transport, Qneon))
    {
      ne_request *r;

      if (XSESSION_HANDLE (request)->neon == NULL)
	invalid_state ("neon request not live", request);

      r = XSESSION_HANDLE (request)->neon->request;
      if (r)
	{
	  Extbyte *n =  NEW_LISP_STRING_TO_EXTERNAL (name, Qbinary);
	  Extbyte *v =  NEW_LISP_STRING_TO_EXTERNAL (value, Qbinary);
	  ne_add_request_header(r, n, v);
	}
      else
	invalid_state ("neon request not live", request);
    }
  else
    invalid_state ("not a neon session", request);
  return Qnil;
}

DEFUN ("neon-request-dispatch", Fneon_request_dispatch, 1, 1, 0, /*
Dispatch REQUEST.
Before dispatch, set response handler with `neon-add-response-body-reader'.
  #### Default is?
Also set body of request, if any, with `neon-set-request-body-buffer'.
Returns an object appropriate to the method, such as the buffer containing
the response to HTTP GET, or an "XML parse tree" from WebDAV PROPFIND.  (The
"parse tree" is actually likely to be a more Lispy object such as a plist.)

The `last-response-headers' and `last-response-status' properties are set
on the request \(session) object.  The value of the `last-response-headers'
property is a pseudo-plist with header tags as keys and contents as values.
\(This is not a true plist, since keys are strings and there can be any
number of headers with the same tag.)
The value of the `last-response-status' property is a vector containing
the major version and the minor version of the HTTP server implementation,
the response code, the class \(code / 100) of the code, and the reason
phrase \(five elements) in that order.
*/
       (request))
{
  Lisp_Session_Handle *r;

  CHECK_SESSION_HANDLE (request);
  r = XSESSION_HANDLE (request);
  if (!EQ (r->transport, Qneon))
    wtaerror ("URL handle is not a neon handle", request);
  if (!r->neon || !r->neon->request)
    invalid_state ("URL handle neon request is uninitialized", request);

  {
    ne_request *neon = r->neon->request;
    int code;

    code = ne_request_dispatch (neon);
    r->last_response_status = neon_prepare_http_status (neon);
#ifdef NEON_USES_HEADER_ITERATE
    /* #### this may not be the right place for this */
    {
      void *cur = NULL;
      const Extbyte *tag = NULL, *contents = NULL;
      while ((cur = ne_response_header_iterate (neon, cur, &tag, &contents)))
	r->last_response_headers = cons3 (build_ext_string (tag, Qbinary),
					  build_ext_string (contents, Qbinary),
					  r->last_response_headers);
    }
#endif

    /* #### I think we can destroy the request right here and right now. */
    if (r->neon->request)
      {
	ne_request_destroy (r->neon->request);
	r->neon->request = NULL;
      }
    else
      invalid_state ("WTF? No neon request!", request);

    if (BUFFERP (Faref (r->state, make_int (READER))))
      {
	/* the reader uses an lstream, (flush and) close it
	   #### we don't have access to the other_end, can't delete it. */
	Lstream *s = XLSTREAM (Faref (r->state, make_int (READER_LSTREAM)));
	Lstream_close (s); 	/* flushes */
	Faset (r->state, make_int (READER_LSTREAM), Qnil);
      }
    else if (EQ (Faref (r->state, make_int (READER)), Qwebdav_xml))
      {
	/* the reader used up a parser, we can destroy it now */
	if (r->neon->parser)
	  {
	    ne_xml_destroy (r->neon->parser);
	    r->neon->parser = NULL;
	  }
	else
	  invalid_state ("WTF? No neon parser!", request);
      }
      
    switch (code)
      {
      case NE_OK:
	break;
      case NE_AUTH:
      case NE_PROXYAUTH:
	signal_error (Qio_error, "Authorization failed", request);
      case NE_CONNECT:
	signal_error (Qio_error, "Could not connect", request);
      case NE_TIMEOUT:
	signal_error (Qio_error, "Connection timed out", request);
      case NE_ERROR:
	signal_error (Qio_error, ne_get_error (r->neon->session), request);
      default:
	invalid_state ("undocumented error in neon_request_dispatch", request);
      }
  }
  /* libneon doth suck */
  if (EQ (Faref (r->state, make_int (READER)), Qwebdav_xml))
    return Fcar (Fcdr (Faref (r->state, make_int (RESPONSE))));
  else if (BUFFERP (Faref (r->state, make_int (READER))))
    return Faref (r->state, make_int (RESPONSE));
  else
    invalid_state ("not a buffer or `webdav-xml' in request", request);
  /* not reached */
  return Qnil;
}

/************************************************************************/
/*		       callbacks, helpers, wrappers			*/
/* Contents:								*/
/*   neon_status							*/
/*   neon_prepare_path							*/
/*   neon_prepare_depth							*/
/*   neon_prepare_http_status						*/
/*   neon_write_lstream							*/
/*   neon_header_catcher						*/
/*   neon_start_cb							*/
/*   neon_cdata_cb							*/
/*   neon_end_cb							*/
/*   neon_credentials_cb						*/
/************************************************************************/

/* utilities used by convenience APIs */

static Lisp_Object
neon_status (int status)
{
  switch (status)
    {
    case NE_OK:
      return Qnil;		/* #### maybe should be Qt? */
    case NE_AUTH:
      return Qauthorization_failure;
    case NE_PROXYAUTH:
      return Qproxy_authorization_failure;
    case NE_CONNECT:
      return Qconnection_failure;
    case NE_TIMEOUT:
      return Qtimeout;
    case NE_ERROR:
      /* #### should we get the neon error string here, or provide an API? */
      return Qgeneric_error;
    default:
      signal_error (Qio_error, "ne_get returned unexpected status",
		    make_int (status));
    }
}

static Extbyte *
neon_prepare_path (Lisp_Object path, Lisp_Object codesys)
{
  Extbyte *p = 0;

  if (!STRINGP (path))
    wtaerror ("path argument must be stringp", path);
  else
    {
      /* #### I don't thing we need to GCPRO here */
      codesys = Fget_coding_system (NILP (codesys) ? Qutf_8 : codesys);
      p = NEW_LISP_STRING_TO_EXTERNAL (path, codesys);
    }
  /* I don't trust libneon?  Whatever gives you _that_ impression? */
  if (p)
    p = ne_path_escape (p);
  if (!p)
    invalid_state ("wtf! couldn't convert Lisp string to external?!", path);
  return p;
}

static int
neon_prepare_depth (Lisp_Object depth, int kidz_ok)
{
  int d;
  if (EQ (depth, make_int (0)))
    d = NE_DEPTH_ZERO;
  else if (EQ (depth, Qinfinite))
    d = NE_DEPTH_INFINITE;
  else if (kidz_ok && EQ (depth, make_int (1)))
    d = NE_DEPTH_ONE;		/* not used by WebDAV COPY */
  else
    signal_error (intern ("args-out-of-range"), "invalid WebDAV depth", depth);
  return d;
}

/*
  definition of ne_status

  typedef struct {
      int major_version;
      int minor_version;
      int code; // Status-Code value
      int klass; // Class of Status-Code (1-5)
      char *reason_phrase;
  } ne_status;
*/

static Lisp_Object
neon_prepare_http_status (ne_request *neon)
{
  const ne_status *s = ne_get_status (neon);
  Lisp_Object args[5];
  args[0] = make_int (s->major_version);
  args[1] = make_int (s->minor_version);
  args[2] = make_int (s->code);
  args[3] = make_int (s->klass);
  args[4] = build_ext_string (s->reason_phrase, Qbinary);
  return Fvector (5, args);
}

/* write out the data received from the request to an Lstream */
static int
neon_write_lstream (void *stream, const char *data, size_t count)
{
  Lstream *s = XLSTREAM ((Lisp_Object) stream);
  if (count > 0)
    {
      /* #### the return code should be checked here, perhaps? */
      Lstream_write (s, data, count);
    }
#if 0
  /* probably this is responsible for the "lstream not open errors"? */
  else
    {
      Lstream_close (s);
    }
#endif
  /* #### can we fail in a detectable way? */
  return 0;
}

#ifdef NEON_USES_HEADER_CATCHER
/* ... in neon 0.24.7 and maybe other older neons;
   this interface is gone in neon 0.25.4. */
/* Header catchers receive the whole header.
   userdata will be a pointer to a Lisp object containing a list of strings.
   #### UNTESTED! */
static void
neon_header_catcher (void *userdata, const char *value)
{
  char *s1, *s2;
  Lisp_Object *lp = (Lisp_Object *) userdata;
  Lisp_Object tag, contents;
  struct gcpro gcpro1;

  /* don't use ne_token and ne_shave because they either do evil things
     with the strings or allocate */
  /* #### does this implement RFC 2822 WS? */
  s2 = s1 = strchr (value, ':');
  /* retreat s2 until it hits a non-space */
  for (s2-- ; s2 > value && (*s2 == ' ' || *s2 == '\t'); s2--) /* empty */ ;
  tag = make_ext_string (value, s2 - value + 1, Qbinary);

  /* advance s1 until it hits a non-space */
  for (s1++ ; *s1 && (*s1 == ' ' || *s1 == '\t'); s1++) /* empty */ ;
  /* retreat s2 until it hits a non-space */
  s2 = s1 + strlen (s1);
  for (s2-- ; s2 > s1 && (*s2 == ' ' || *s2 == '\t'); s2--) /* empty */ ;
  contents = make_ext_string (s1, s2 - s1 + 1, Qbinary);

  *lp = cons3 (tag, contents, *lp);
}
#endif

/* ;; element start callback
   (defun start-cb (state ns nm ap) */
static int
neon_start_cb (void *userdata, int UNUSED(parent),
	       const char *nspace, const char *name,
	       const char **atts)
{
  Lisp_Object neon_state = (Lisp_Object) userdata;
  Lisp_Object cs = Fget_coding_system (Faref (neon_state,
					      make_int (CODING_SYSTEM)));
  Lisp_Object ns = build_ext_string (nspace, cs);
  Lisp_Object nm = build_ext_string (name, cs);
  Lisp_Object ap = Qnil;
  /* (let ((current (aref state 1))) */
  Lisp_Object current = Faref (neon_state, make_int (CURRENT));
  /* Lisp_Object tmp; */
  const char **a;
  int i;

  /* generate ap (the attribute plist) */
  for (a = atts, i = 0; *a != NULL; a++, i++)
    ap = Fcons (build_ext_string (*a, cs), ap);
  if (i % 2)
    invalid_state ("attribute array length was odd", ap);
  ap = Fnreverse (ap);

  /* (setcdr current (cons (list nm ns ap) (cdr current))) */
  Fsetcdr (current, Fcons (list3 (nm, ns, ap), XCDR (current)));
  /* (setq current (cdr current)) */
  current = XCDR (current);
  {
    /* (let ((tail (cdr (cdr (car current))))) */
    Lisp_Object tail = XCDR (XCDR (XCAR (current)));
    /* (setcdr tail current) */
    Fsetcdr (tail, current);
    /* (aset state 1 tail))) */
    Faset (neon_state, make_int (CURRENT), tail);
  }
  /* 1) */
  return NEON_XML_ACCEPT;
}

/* ;; cdata callback
   (defun cdata-cb (state cdata)

   libneon doesn't coalesce adjacent cdata into a single cdata (in fact it
   seems to always return vertical whitespace as a separate component) from
   other whitespace or text.  It also doesn't bother to coalesce across
   internal buffer boundaries, so a word may be split in the middle:
   "[...1018 characters]<el>word</el>"
   can result in "word" being split into "wo" and "rd"." */
static int
neon_cdata_cb (void *userdata, int UNUSED(state),
	       const char *cdata, size_t len)
{
  Lisp_Object neon_state = (Lisp_Object) userdata;
  Lisp_Object cs = Fget_coding_system (Faref (neon_state,
					      make_int (CODING_SYSTEM)));
  /* (let ((current (aref state 1))) */
  Lisp_Object current = Faref (neon_state, make_int (CURRENT));

  /* (setcdr current (cons cdata (cdr current))) */
  Fsetcdr (current, Fcons (make_ext_string (cdata, len, cs), XCDR (current)));
  /* (aset state 1 (cdr current))) */
  Faset (neon_state, make_int (CURRENT), XCDR (current));
  /* 1) */
  return NEON_XML_CONTINUE;
}

/* ;; element end callback
   (defun end-cb (state) */
static int
neon_end_cb (void *userdata, int UNUSED(state),
	     const char * UNUSED(nspace), const char * UNUSED(name))
{
  Lisp_Object neon_state = (Lisp_Object) userdata;
  /* (let ((current (aref state 1))) */
  Lisp_Object current = Faref (neon_state, make_int (CURRENT));

  /* #### abort if EQ (current, header) here?
     #### abort if NILP (Fcdr (current)) here? */
  /* (aset state 1 (cdr current)) */
  Faset (neon_state, make_int (CURRENT), XCDR (current));
  /* (setcdr current nil)) */
  Fsetcdr (current, Qnil);
  /* 1) */
  return NEON_XML_CONTINUE;
}

static int 
neon_credentials_cb (void *userdata, const char *rlm,
			   int at, char *username, char *password)
{
  Lisp_Object callback = (Lisp_Object) userdata;
  Lisp_Object credentials;
  struct gcpro gcpro1;

  {
    struct gcpro ngcpro1;
    Lisp_Object args[3];
    args[0] = callback;
    args[1] = build_ext_string (rlm, Qbinary);
    args[2] = make_int (at);
    NGCPRO1 (args[0]);
    ngcpro1.nvars = 3;
    credentials = Ffuncall (3, args);
    UNGCPRO;
  }

  GCPRO1 (credentials);
  /* #### ugly hack */
  if (INTP (credentials))
    return 1;
  /* more sanity checking on the callback */
  CHECK_CONS (credentials);

  {
    Lisp_Object args_out_of_range = intern ("args-out-of-range");
    /* credentials is GCPRO'd, user and pass are OK */
    Lisp_Object user = XCAR (credentials);
    Lisp_Object pass = XCDR (credentials);
    Extbyte *utmp, *ptmp;

    /* yet more sanity checking on the callback */
    CHECK_STRING (user);
    CHECK_STRING (pass);

    /* #### these are user errors, maybe should return non-zero to libneon? */
    if (XINT (Flength (user)) >= NE_ABUFSIZ)
      signal_error (args_out_of_range, "username overflows buffer", user);
    if (XINT (Flength (pass)) >= NE_ABUFSIZ)
      signal_error (args_out_of_range, "password overflows buffer", pass);
    
    LISP_STRING_TO_EXTERNAL (user, utmp, Qbinary);
    strncpy (username, utmp, NE_ABUFSIZ);
    LISP_STRING_TO_EXTERNAL (pass, ptmp, Qbinary);
    strncpy (password, ptmp, NE_ABUFSIZ);
  }
  UNGCPRO;

  return 0;
}

/************************************************************************/
/*				Module API				*/
/* Contents:								*/
/*   modules_of_neon_api						*/
/*   syms_of_neon_api							*/
/*   vars_of_neon_api							*/
/*   unload_neon_api							*/
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

#ifdef HAVE_EARL
void
modules_of_neon_api ()
{
  emodules_load ("earl.ell", "earl", "0.0.5");
}
#endif

void
syms_of_neon_api ()
{
  INIT_LRECORD_IMPLEMENTATION (session_handle);

#ifndef HAVE_EARL
  /* #### These functions will move to the earl module. */
  DEFSUBR (Fsession_handle_p);
  DEFSUBR (Fsession_handle_transport);
  DEFSUBR (Fsession_handle_live_p);
  DEFSUBR (Fsession_handle_plist);
#endif

  /* neon-specific functions. */
  DEFSUBR (Fneon_make_session_handle);
  DEFSUBR (Fneon_session_set_auth);
  DEFSUBR (Fneon_session_forget_auth);
  DEFSUBR (Fneon_get_file);
  DEFSUBR (Fneon_put_file);
  DEFSUBR (Fneon_post_get_file);
  DEFSUBR (Fneon_delete);
  DEFSUBR (Fneon_mkcol);
  DEFSUBR (Fneon_copy);
  DEFSUBR (Fneon_move);
  DEFSUBR (Fneon_request_create);
  DEFSUBR (Fneon_add_response_body_reader);
  DEFSUBR (Fneon_add_request_header);
  DEFSUBR (Fneon_set_request_body_buffer);
  DEFSUBR (Fneon_request_dispatch);

  /* neon-specific symbols. */
  DEFSYMBOL (Qneon_api);	/* feature symbol */
  DEFSYMBOL (Qneon);
  DEFSYMBOL (Qinfinite);
  DEFSYMBOL (Qwebdav_xml);
  DEFSYMBOL (Qaccept_always);
  DEFSYMBOL (Qaccept_2xx);
  DEFSYMBOL (Qauthorization_failure);
  DEFSYMBOL (Qproxy_authorization_failure);
  DEFSYMBOL (Qconnection_failure);
  DEFSYMBOL (Qtimeout);
  DEFSYMBOL (Qgeneric_error);
#ifndef HAVE_EARL
  /* #### These symbols will move to the earl module. */
  DEFSYMBOL_MULTIWORD_PREDICATE (Qsession_handlep);
  DEFSYMBOL_MULTIWORD_PREDICATE (Qsession_handle_livep);

  DEFSYMBOL (Qtransport);
  DEFSYMBOL (Qlast_response_headers);
  DEFSYMBOL (Qlast_response_status);
  DEFSYMBOL (Qcoding_system);
  DEFSYMBOL (Qurl);
#endif
}

void
vars_of_neon_api ()
{

  Fprovide (Qneon_api);

  /* #### These aren't used but could be useful, see curl_api. */
#if 0
  DEFVAR_LISP ("neon-option-hash-table", &Vneon_option_hash_table /*
Table of options available for `neon-easy-setopt'.
Key are strings naming options.  The option names are taken from enum
NEONoption in <neon/neon.h>.  They are all uppercase, and the "NEONOPT_"
prefix is omitted.
Values are lists containing a type symbol \(one of `long', `objectpoint',
`functionpoint', and `off_t') and an integer, which is the option index.
It is planned to add a list of Lisp types that can be converted to something
that is useful for the option as the 3rd element of the value list.
It is planned to add the leading comments as docstrings, to be the 4th
element of the value list corresponding to each key.
*/ );
  Vneon_option_hash_table = Qnil;

  DEFVAR_LISP ("neon-info-hash-table", &Vneon_info_hash_table /*
Table of attributes accessible via `neon-easy-getinfo'.
Keys are strings naming attributes.  The attribute names are taken from
enum NEONinfo in <neon/neon.h>.  They are all uppercase, and the "NEONINFO_"
prefix is omitted.
Values are lists containing a type symbol (one of `long', `string',
`double', and `list') and an integer, which is the attribute index.
It is planned to add docstrings, to be the 3rd element of the value list
corresponding to each key.
*/ );
  Vneon_info_hash_table = Qnil;
#endif

#if 0
#ifdef HAVE_SHLIB
  /* May need to initialize neon if loaded as a module.
     #### Anything to do here?  Nothing interesting is documented. */
#endif
#endif
  ne_debug_init (stderr, NE_DBG_HTTPAUTH);
}

#ifdef HAVE_SHLIB
void
unload_neon_api ()
{
  /* If we create any new types by INIT_LRECORD_IMPLEMENTATION (sample_type),
     then UNDEF_LRECORD_IMPLEMENTATION (sample_type) must appear here.  Also,
     any symbols declared with DEFSYMBOL (Qsample_var) or a variant, must
     have a corresponding unstaticpro_nodump (&Qsample_var) here. */
  UNDEF_LRECORD_IMPLEMENTATION (session_handle);

  /* Shut down libneon and free internal structures.
     #### Anything to do here?  Nothing interesting is documented. */

  unstaticpro_nodump (&Qneon_api);
  unstaticpro_nodump (&Qneon);
  unstaticpro_nodump (&Qinfinite);
  unstaticpro_nodump (&Qwebdav_xml);
  unstaticpro_nodump (&Qaccept_always);
  unstaticpro_nodump (&Qaccept_2xx);
  unstaticpro_nodump (&Qauthorization_failure);
  unstaticpro_nodump (&Qproxy_authorization_failure);
  unstaticpro_nodump (&Qconnection_failure);
  unstaticpro_nodump (&Qtimeout);
  unstaticpro_nodump (&Qgeneric_error);
#ifndef HAVE_EARL
  unstaticpro_nodump (&Qtransport);
  unstaticpro_nodump (&Qlast_response_headers);
  unstaticpro_nodump (&Qlast_response_status);
  /* predicate special handling */
  unstaticpro_nodump (&Qsession_handlep);
  unstaticpro_nodump (&Qsession_handle_livep);
  unstaticpro_nodump (&Qurl);
  unstaticpro_nodump (&Qcoding_system);
#endif
}
#endif
