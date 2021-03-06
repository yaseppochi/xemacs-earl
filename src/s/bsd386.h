/* Synched up with: FSF 19.31. */

/* s/ file for bsd386 system.  */

#include "bsd4-3.h"

#ifndef __bsdi__
#define __bsdi__ 1
#endif

#define DECLARE_GETPWUID_WITH_UID_T

#define PENDING_OUTPUT_COUNT(FILE) ((FILE)->_p - (FILE)->_bf._base)
#define A_TEXT_OFFSET(x)    (sizeof (struct exec))
#define A_TEXT_SEEK(hdr) (N_TXTOFF(hdr) + A_TEXT_OFFSET(hdr))

#define LIBS_DEBUG
#define LIBS_SYSTEM "-lutil -lcompat"

/* This silences a few compilation warnings.  */
#ifndef NOT_C_CODE
#undef BSD
#include <sys/param.h> /* To get BSD defined consistently.  */
#endif
