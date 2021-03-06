/* Synched up with: FSF 19.31. */

/* Definitions for ESIX, a variant of v.5.3 for the 386.  */
/* These are based on reports for ESIX 5.3.2 D.  */

#include "usg5-3.h"

/* Have -lg be used for debugging. */
#undef LIBS_DEBUG
#define LIBS_DEBUG "-lg"

/* If using Roell's X server, define X11R4 */
#ifdef X11R4			/* Roell's X server */
#if 0 /* XEmacs */
#define select sys_select /* Emacs select() not good enough? */
#endif
#endif /* X11R4 */

/* ESIX does not need <sys/sioctl.h>, but needs <sys/ptem.h> */
#define NO_SIOCTL_H
#define NEED_PTEM_H
