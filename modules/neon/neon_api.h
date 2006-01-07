/*
 * Lisp interface to libneon for XEmacs.
 *
 * Copyright (C) 1998, 1999 J. Kean Johnston. All rights reserved.
 * Copyright (C) 2002 Jerry James.
 * Copyright (C) 2005 Stephen J. Turnbull <stephen@xemacs.org>
 *
 * All rights reserved, except as expressly indicated below.
 *
 * This program is not considered part of XEmacs.
 *
 * You may use, copy, modify, and distribute this software under the terms
 * of the GNU General Public License, version 2 or later at your option.
 *
 * Author:		Stephen J. Turnbull <stephen@xemacs.org>
 * Creation-Date:	2005-11-23
 */

/* Commentary:
 *
 * This modules encapsulates a libneon request in a Lisp_URL_Handle.
 * Since neon requests are associated with persistent sessions, a handle
 * may also be used to identify a session.
 * This means that we must be very careful to cache the session and
 * destroy it at the appropriate time.
 */

#include <neon/ne_request.h>	/* include ne_session.h,
				   ne_utils.h, ne_string.h,
				   ne_defs.h, ne_ssl.h, ne_uri.h */
#include <neon/ne_auth.h>
#include <neon/ne_basic.h>
#include <neon/ne_xml.h>

/************************************************************************/
/*				Structures				*/
/************************************************************************/

/* neon_start_cb may return ACCEPT, DECLINE, or ABORT.
   neon_cdata_cb and neon_end_cb may return CONTINUE or ABORT. */
#define NEON_XML_ACCEPT (1)
#define NEON_XML_DECLINE (0)
#define NEON_XML_ABORT (-1)
#define NEON_XML_CONTINUE (0)

/* For convenience of allocation neon_state is a LISP vector.
   Currently for want of a better API it is stored in struct
   Lisp_Session_Handle in the `state' member (thinking about generalizing
   it).
   These are the access indicies. */
enum neon_state_index {
  READER = 0,			/* r/o; typically buffer or symbol */
  RESPONSE = 1,			/* r/w; this is the return value */
  CURRENT = 2,			/* used by the XML body reader */
  CODING_SYSTEM = 3,		/* used by the XML body reader */
  MAX_USER_DATA_LENGTH = 4
};

/* This is Session_Handle, not Neon_Handle, because a generalization is
   planned. */
struct Lisp_Session_Handle
{
  struct LCRECORD_HEADER header;
  /* property list for properties not contained in the cURL handle or
     neon request or session;
     READ-ONLY from Lisp?
     This could be used to maintain state for a handler based on url.el or
     an external process (eg, wget). */
  /* #### the type-specific handles may want to be a union */
  /* type of the handle; READ-ONLY from Lisp */
  Lisp_Object type;
  Lisp_Object property_list;
  /* the coding system used to convert url; READ-ONLY from Lisp */
  Lisp_Object coding_system;
  /* persistent r/w data passed as "userdata"; see enum neon_state_index */
  Lisp_Object state;
  /* auxiliary structures used internally */
  Lisp_Object stuff;
  Lisp_Object last_response_status;
  Lisp_Object last_response_headers;
  /* the URL string in external format
     libcurl expects the caller to allocate storage and clean it up.
     #### How about libneon?  Considering libneon suggests that this should
     be parsed into a struct (this is standardized and libneon follows the
     standard), and a method providing for generating the URL.
     #### If not, this will eventually move to the big_ball_of_strings. */
  Extbyte *url;
  /* These could be a linked list of low-level-module-specific structures.
     Actually, a single session_handle->handler_info member to be cast to
     `TYPE_handler_info *' should be enough, since we know the type.  This
     would allow resetting type, too. */
#ifdef HAVE_CURL
  /* the curl handle used by the libcurl API */
  CURL *curl_handle;
#endif
#ifdef HAVE_NEON
  /* the neon request used by the libneon API */
  ne_request *neon_request;
  /* neon sucks... */
  ne_xml_parser *neon_parser;
  /* storage for the neon session, when the neon_request is invalid */
  ne_session *neon_session;
#endif
  /* #### UNIMPLEMENTED array of pointers to string data we need to free */
  Dynarr *big_ball_of_strings;
};
typedef struct Lisp_Session_Handle Lisp_Session_Handle;

DECLARE_LRECORD (session_handle, Lisp_Session_Handle);
#define XSESSION_HANDLE(x) XRECORD (x, session_handle, Lisp_Session_Handle)
#define wrap_session_handle(p) wrap_record (p, session_handle)
#define SESSION_HANDLEP(x) RECORDP (x, session_handle)
#define CHECK_SESSION_HANDLE(x) CHECK_RECORD (x, session_handle)
#define CONCHECK_SESSION_HANDLE(x) CONCHECK_RECORD (x, session_handle)

