#ifndef include_NEON_API_H
#define include_NEON_API_H
/*
 * Lisp interface to libneon for XEmacs.
 *
 * Copyright (C) 1998, 1999 J. Kean Johnston. All rights reserved.
 * Copyright (C) 2002 Jerry James.
 * Copyright (C) 2005, 2006 Stephen J. Turnbull <stephen@xemacs.org>
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
 * Last-Modified:	2006-01-12
 */

/* Commentary:
 *
 * This modules encapsulates a libneon request in a Lisp_Session_Handle.
 * Since neon requests are associated with persistent sessions, we must be
 * very careful to cache the session and destroy it at the appropriate time.
 */

#include "../earl/earl.h"
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
  RESPONSE,			/* r/w; this is the return value */
  CURRENT,			/* used by the XML body reader */
  CODING_SYSTEM,		/* used by the XML body reader */
  READER_LSTREAM,		/* user by the buffer body reader */
  SERVER_CB,			/* server authentication callback */
  PROXY_CB,			/* proxy authentication callback */
  NEON_STATE_SIZE
};

struct neon_data {
  /* REQUIRED MEMBERS: SEE earl.h */
   struct earl_transport_implementation *transport_implementation;

  /* neon-specific data */
  /* storage for the neon session */
  ne_session *session;
  /* the neon request used by the libneon API */
  ne_request *request;
  /* neon sucks... */
  ne_xml_parser *parser;
};

#if 0 /* not needed? */
DECLARE_LRECORD (neon_data, Lisp_Neon_Data);
#define XNEON_DATA(x) XRECORD (x, neon_data, Lisp_Neon_Data)
#define wrap_neon_data(p) wrap_record (p, neon_data)
#define NEON_DATAP(x) RECORDP (x, neon_data)
#define CHECK_NEON_DATA(x) CHECK_RECORD (x, neon_data)
#define CONCHECK_NEON_DATA(x) CONCHECK_RECORD (x, neon_data)
#endif

#define NEON_DATA(handle) ((struct neon_data *) (handle->transport_data))

#endif /* include_NEON_API_H */
