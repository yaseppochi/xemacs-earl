#ifndef include_EARL_H
#define include_EARL_H
/* Emacs Augmented Resource Locators
 *
 * Lisp interface to session (URL) manager for XEmacs.
 *
 * Copyright (C) 2006 Stephen J. Turnbull <stephen@xemacs.org>
 * Copyright (C) 2002 Jerry James.
 * Copyright (C) 1998, 1999 J. Kean Johnston. All rights reserved.
 *
 * All rights reserved, except as expressly indicated below.
 *
 * This program is not considered part of XEmacs.
 *
 * You may use, copy, modify, and distribute this software under the terms
 * of the GNU General Public License, version 2 or later at your option.
 *
 * Author:		Stephen J. Turnbull <stephen@xemacs.org>
 * Creation-Date:	2006-01-15
 */

/* Commentary:
 *
 * This module encapsulates a web request in a Lisp_Session_Handle.
 *
 * The Lisp_Session_Handle structure maintains the abstract URL
 * information, such as the RFC 2396 five-part parse.  It also contains
 * a transport object.  The transport will actually handle the data
 * transfer, including network connections.  Many transports will be
 * delegated to external modules.  This causes a technical problem
 * because we do not want to expose the details of transports to LISP,
 * but we do want modules to be runtime loadable, and the ELL spec does
 * not expose module symbols to other except via LISP.  This includes
 * the C names of LISP variables and functions.
 *
 * You might think that we could simply replicate the access logic by
 * including the relevant headers, but this turns out not to be the case
 * in error-checking builds, because macros like CHECK_<type> access the
 * lrecord definition by name.  This means that we either forego error
 * checking, or we duplicate that logic (neither of which seem like good
 * ideas), or we expose the mutators for "read-only" properties to LISP.
 * It occurs to me that we can make them C-only by having a "password"
 * argument and check that it is Qunbound (which isn't spellable from
 * LISP), but that's pretty hideous.  :-)
 *
 * So I guess the thing to do here is to encapsulate each transport in
 * its own LISP object, and provide an earl function add a transport (or
 * alternatively a list of them, if that seems useful) to the session
 * handle.
 *
 * Requirements:
 *
 * User Interface -- Users should see URLs as pointers to abstract
 * resources that XEmacs can manipulate.  Resources can be fetched into
 * buffers, updated from buffers, created as collections, locked,
 * versioned, etc.  Access to a resource may require authorization; the
 * user must be able to present credentials for access.  The initial
 * implementation will provide all methods specified by RFC 2518 for
 * HTTP (ie, WebDAV), and some methods for all schemes supported by
 * cURL.  Access to status information (such as reply headers and the
 * status return code) should be easily available.
 *
 * There should be support for maintaining persistent state while
 * accessing several related resources, that is, for sessions.  While
 * some transports (specifically cURL) provide transparent support for
 * sessions (called "caching" in the cURL documentation, ie, remembering
 * cookies, connections, and authorization relevant to previous
 * accesses, and trying to reuse them), this is not an implementation
 * goal at the present time for this module.  However, features and
 * implementations that obstruct adding such transparent support in the
 * future should be avoided.
 *
 * Some control over transport choice will be provided.
 *
 * Application Programming Interface -- Programmers should also see URLs
 * as pointers to abstract resources.  The API should provide finer
 * control over transport choice and method configuration.
 *
 * Administration -- As the discussion above indicates, separating the
 * URL interfaces from transport implementation is going to be
 * difficult.  Nevertheless, it is very desirable to provide loadable
 * module support for external libraries.  This allows third-party
 * distributors to create binary distributions that do not require
 * support packages to be installed, but can take advantage of them if
 * and when they are installed.
 */

struct Lisp_Session_Handle
{
  struct LCRECORD_HEADER header;
  /* string - URL used to initialize the handle; READ-ONLY from Lisp? */
  Lisp_Object url;
  /* symbol - the coding system used to convert url; READ-ONLY from Lisp */
  Lisp_Object coding_system;
  /* symbol - type of the handle; READ-ONLY from Lisp
     TRANSPORT currently is used as a flag for liveness; it must be reset
     to nil if the handle can no longer be used by that transport */
  Lisp_Object transport;
  /* string, list of strings - information about last transfer */
  Lisp_Object last_response_status;
  Lisp_Object last_response_headers;
  /* property list for properties not contained in the cURL handle or
     neon request or session; properties are READ-WRITE from Lisp
     This could be used to maintain state for a handler based on url.el or
     an external process (eg, wget). */
  Lisp_Object plist;
  /* persistent r/w data passed as "userdata"; not visible from LISP
     see enum neon_state_index */
  Lisp_Object state;
#if 0
  /* #### UNUSED? */
  /* auxiliary structures used internally; not visible from LISP */
  Lisp_Object stuff;
#endif
  /* a single session_handle->transport_data member to be cast to
     `TRANSPORT_handler_info *', since we know the transport.
     This would allow resetting transport, too. */
  struct earl_transport_implementation* transport_data;
  /* #### UNIMPLEMENTED array of pointers to string data we need to free */
  Dynarr *big_ball_of_string;
};
typedef struct Lisp_Session_Handle Lisp_Session_Handle;

struct earl_transport_implementation {
  void (*finalize) (struct earl_transport_data *transport_data);
  /* #### We need getprop, putprop, and remprop methods for
     transport-specific properties. */
};

struct earl_transport_data {
  struct earl_transport_implementation *transport_implementation;
};

DECLARE_LRECORD (session_handle, Lisp_Session_Handle);
#define XSESSION_HANDLE(x) XRECORD (x, session_handle, Lisp_Session_Handle)
#define wrap_session_handle(p) wrap_record (p, session_handle)
#define SESSION_HANDLEP(x) RECORDP (x, session_handle)
#define CHECK_SESSION_HANDLE(x) CHECK_RECORD (x, session_handle)
#define CONCHECK_SESSION_HANDLE(x) CONCHECK_RECORD (x, session_handle)

MODULE_API EXFUN (Fmake_session_handle, 2);
/* MODULE_API EXFUN (Fsession_handle_p, 1); */
/* MODULE_API EXFUN (Fsession_handle_live_p, 1); */

/* utilities */

/* error macros */

#define UNIMPLEMENTED(description) signal_error (Qunimplemented, description, Qunbound)

#endif /* include_EARL_H */
