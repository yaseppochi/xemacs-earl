/* Implements elisp-programmable dialog boxes -- MS Windows interface.
   Copyright (C) 1998 Kirill M. Katsnelson <kkm@kis.ru>
   Copyright (C) 2000, 2001, 2002, 2003, 2004 Ben Wing.

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

/* This file essentially Mule-ized (except perhaps some Unicode splitting).
   5-2000. */

/* Author:
   Initially written by kkm, May 1998
*/

#include <config.h>
#include "lisp.h"

#include "buffer.h"
#include "frame-impl.h"
#include "gui.h"
#include "opaque.h"

#include "console-msw-impl.h"

#include "sysfile.h"

Lisp_Object Qdialog_box_error;

static Lisp_Object Q_initial_directory;
static Lisp_Object Q_initial_filename;
static Lisp_Object Q_filter_list;
static Lisp_Object Q_allow_multi_select;
static Lisp_Object Q_create_prompt_on_nonexistent;
static Lisp_Object Q_overwrite_prompt;
static Lisp_Object Q_file_must_exist;
static Lisp_Object Q_no_network_button;
static Lisp_Object Q_no_read_only_return;

/* List containing all dialog data structures of currently popped up
   dialogs. */
static Lisp_Object Vdialog_data_list;

/* List of popup frames wanting keyboard traversal handled */
static Lisp_Object Vpopup_frame_list;

Lisp_Object Vdefault_file_dialog_filter_alist;

/* DLUs per character metrics */
#define X_DLU_PER_CHAR	     4
#define Y_DLU_PER_CHAR	     8

/*
  Button metrics
  --------------
  All buttons have height of 15 DLU. The minimum width for a button is 32 DLU, 
  but it can be expanded to accommodate its text, so the width is calculated as
  8 DLU per button plus 4 DLU per character.
  max (32, 6 * text_length). The factor of six is rather empirical, but it
  works better than 8 which comes from the definition of a DLU. Buttons are
  spaced with 6 DLU gap. Minimum distance from the button to the left or right 
  dialog edges is 6 DLU, and the distance between the dialog bottom edge and
  buttons is 7 DLU.
*/

#define X_MIN_BUTTON	    32
#define X_BUTTON_MARGIN	     8
#define Y_BUTTON	    15
#define X_BUTTON_SPACING     6
#define X_BUTTON_FROM_EDGE   6
#define Y_BUTTON_FROM_EDGE   7

/* 
   Text field metrics
   ------------------
   Text distance from left and right edges is the same as for buttons, and the
   top margin is 11 DLU. The static control has height of 2 DLU per control
   plus 8 DLU per each line of text. Distance between the bottom edge of the
   control and the button row is 15 DLU. Minimum width of the static control
   is 100 DLU, thus giving minimum dialog weight of 112 DLU. Maximum width is
   300 DLU, and, if the text is wider than that, the text is wrapped on the
   next line. Each character in the text is considered 4 DLU wide.
*/

#define X_MIN_TEXT	   100
#define X_AVE_TEXT	   200
#define X_MAX_TEXT	   300
#define X_TEXT_FROM_EDGE      X_BUTTON_FROM_EDGE
#define Y_TEXT_FROM_EDGE    11
#define Y_TEXT_MARGIN	     2
#define Y_TEXT_FROM_BUTTON  15

#define X_MIN_TEXT_CHAR	   (X_MIN_TEXT / X_DLU_PER_CHAR)
#define X_AVE_TEXT_CHAR	   (X_AVE_TEXT / X_DLU_PER_CHAR)
#define X_MAX_TEXT_CHAR	   (X_MAX_TEXT / X_DLU_PER_CHAR)

/*
  Layout algorithm
  ----------------
  First we calculate the minimum width of the button row, excluding "from
  edge" distances. Note that the static control text can be narrower than
  X_AVE_TEXT only if both text and button row are narrower than that (so,
  even if text *can* be wrapped into 2 rows narrower than ave width, it is not 
  done). Let WBR denote the width of the button row.

  Next, the width of the static field is determined.
  First, if all lines of text fit into max (WBR, X_MAX_TEXT), the width of the
  control is the same as the width of the longest line. 
  Second, if all lines of text are narrower than X_MIN_TEXT, then width of
  the control is set to X_MIN_TEXT.
  Otherwise, width is set to max(WBR, X_AVE_TEXT). In this case, line wrapping will
  happen.

  If width of the text control is larger than that of the button row, then the
  latter is centered across the dialog, by giving it extra edge
  margins. Otherwise, minimal margins are given to the button row.
*/

#define ID_ITEM_BIAS 32

void
mswindows_register_popup_frame (Lisp_Object frame)
{
  Vpopup_frame_list = Fcons (frame, Vpopup_frame_list);
}

void
mswindows_unregister_popup_frame (Lisp_Object frame)
{
  Vpopup_frame_list = delq_no_quit (frame, Vpopup_frame_list);
}

/* Dispatch message to any dialog boxes.  Return non-zero if dispatched. */
int
mswindows_is_dialog_msg (MSG *msg)
{
  LIST_LOOP_2 (data, Vdialog_data_list)
    {
      if (qxeIsDialogMessage (XMSWINDOWS_DIALOG_ID (data)->hwnd, msg))
	return 1;
    }

  {
    LIST_LOOP_2 (popup, Vpopup_frame_list)
      {
	HWND hwnd = FRAME_MSWINDOWS_HANDLE (XFRAME (popup));
	/* This is a windows feature that allows dialog type
	   processing to be applied to standard windows containing
	   controls. */
	if (qxeIsDialogMessage (hwnd, msg))
	  return 1;
      }
  }
  return 0;
}

static const struct memory_description mswindows_dialog_id_description [] = {
  { XD_LISP_OBJECT, offsetof (struct mswindows_dialog_id, frame) },
  { XD_LISP_OBJECT, offsetof (struct mswindows_dialog_id, callbacks) },
  { XD_END }
};

static Lisp_Object
mark_mswindows_dialog_id (Lisp_Object obj)
{
  struct mswindows_dialog_id *data = XMSWINDOWS_DIALOG_ID (obj);
  mark_object (data->frame);
  return data->callbacks;
}

DEFINE_LRECORD_IMPLEMENTATION ("mswindows-dialog-id", mswindows_dialog_id,
			       0, /* dump-able flag */
			       mark_mswindows_dialog_id,
			       internal_object_printer, 0, 0, 0, 
			       mswindows_dialog_id_description,
			       struct mswindows_dialog_id);

/* Dialog procedure */
static BOOL CALLBACK 
dialog_proc (HWND hwnd, UINT msg, WPARAM w_param, LPARAM l_param)
{
  switch (msg)
    {
    case WM_INITDIALOG:
      qxeSetWindowLong (hwnd, DWL_USER, l_param);
      break;
      
    case WM_DESTROY:
      {
	Lisp_Object data;
	data = VOID_TO_LISP ((void *) qxeGetWindowLong (hwnd, DWL_USER));
	Vdialog_data_list = delq_no_quit (data, Vdialog_data_list);
      }
      break;

    case WM_COMMAND:
      {
	Lisp_Object fn, arg, data;
	struct mswindows_dialog_id *did;

	data = VOID_TO_LISP ((void *) qxeGetWindowLong (hwnd, DWL_USER));
	did = XMSWINDOWS_DIALOG_ID (data);
	if (w_param != IDCANCEL) /* user pressed escape */
	  {
	    assert (w_param >= ID_ITEM_BIAS 
		    && (EMACS_INT) w_param
		    < XVECTOR_LENGTH (did->callbacks) + ID_ITEM_BIAS);
	    
	    get_gui_callback (XVECTOR_DATA (did->callbacks)
			      [w_param - ID_ITEM_BIAS],
			      &fn, &arg);
	    mswindows_enqueue_misc_user_event (did->frame, fn, arg);
	  }
	else
	  mswindows_enqueue_misc_user_event (did->frame, Qrun_hooks,
					     Qmenu_no_selection_hook);
	va_run_hook_with_args_trapping_problems
	  (Qdialog, Qdelete_dialog_box_hook, 1, data, 0);

	DestroyWindow (hwnd);
      }
      break;

    default:
      return FALSE;
    }
  return TRUE;
}

/* Helper function which converts the supplied string STRING into Unicode and
   pushes it at the end of DYNARR */
static void
push_lisp_string_as_unicode (unsigned_char_dynarr *dynarr, Lisp_Object string)
{
  int length;
  Extbyte *uni_string;

  TO_EXTERNAL_FORMAT (LISP_STRING, string,
		      ALLOCA, (uni_string, length),
		      Qmswindows_unicode);
  Dynarr_add_many (dynarr, uni_string, length);
  Dynarr_add (dynarr, '\0');
  Dynarr_add (dynarr, '\0');
}

/* Given button TEXT, return button width in DLU */
static int
button_width (Lisp_Object text)
{
  /* !!#### do Japanese chars count as two? */
  int width =
    X_DLU_PER_CHAR *
      ibyte_string_displayed_columns (XSTRING_DATA (text),
					XSTRING_LENGTH (text));
  return max (X_MIN_BUTTON, width);
}

/* Unwind protection routine frees a dynarr opaqued into arg */
static Lisp_Object
free_dynarr_opaque_ptr (Lisp_Object arg)
{
  Dynarr_free (get_opaque_ptr (arg));
  return arg;
}

/* Unwind protection decrements dialog count */
static Lisp_Object
dialog_popped_down (Lisp_Object UNUSED (arg))
{
  popup_up_p--;
  return Qnil;
}


#define ALIGN_TEMPLATE					\
{							\
  int slippage = Dynarr_length (template_) & 3;		\
  if (slippage)						\
    Dynarr_add_many (template_, &zeroes, slippage);	\
}

static struct
{
  DWORD errmess;
  Ascbyte *errname;
} common_dialog_errors[] =
{
  { CDERR_DIALOGFAILURE, "CDERR_DIALOGFAILURE" },
  { CDERR_FINDRESFAILURE, "CDERR_FINDRESFAILURE" },
  { CDERR_INITIALIZATION, "CDERR_INITIALIZATION" },
  { CDERR_LOADRESFAILURE, "CDERR_LOADRESFAILURE" },
  { CDERR_LOADSTRFAILURE, "CDERR_LOADSTRFAILURE" },
  { CDERR_LOCKRESFAILURE, "CDERR_LOCKRESFAILURE" },
  { CDERR_MEMALLOCFAILURE, "CDERR_MEMALLOCFAILURE" },
  { CDERR_MEMLOCKFAILURE, "CDERR_MEMLOCKFAILURE" },
  { CDERR_NOHINSTANCE, "CDERR_NOHINSTANCE" },
  { CDERR_NOHOOK, "CDERR_NOHOOK" },
  { CDERR_NOTEMPLATE, "CDERR_NOTEMPLATE" },
  { CDERR_REGISTERMSGFAIL, "CDERR_REGISTERMSGFAIL" },
  { CDERR_STRUCTSIZE, "CDERR_STRUCTSIZE" },
  { PDERR_CREATEICFAILURE, "PDERR_CREATEICFAILURE" },
  { PDERR_DEFAULTDIFFERENT, "PDERR_DEFAULTDIFFERENT" },
  { PDERR_DNDMMISMATCH, "PDERR_DNDMMISMATCH" },
  { PDERR_GETDEVMODEFAIL, "PDERR_GETDEVMODEFAIL" },
  { PDERR_INITFAILURE, "PDERR_INITFAILURE" },
  { PDERR_LOADDRVFAILURE, "PDERR_LOADDRVFAILURE" },
  { PDERR_NODEFAULTPRN, "PDERR_NODEFAULTPRN" },
  { PDERR_NODEVICES, "PDERR_NODEVICES" },
  { PDERR_PARSEFAILURE, "PDERR_PARSEFAILURE" },
  { PDERR_PRINTERNOTFOUND, "PDERR_PRINTERNOTFOUND" },
  { PDERR_RETDEFFAILURE, "PDERR_RETDEFFAILURE" },
  { PDERR_SETUPFAILURE, "PDERR_SETUPFAILURE" },
  { CFERR_MAXLESSTHANMIN, "CFERR_MAXLESSTHANMIN" },
  { CFERR_NOFONTS, "CFERR_NOFONTS" },
  { FNERR_BUFFERTOOSMALL, "FNERR_BUFFERTOOSMALL" },
  { FNERR_INVALIDFILENAME, "FNERR_INVALIDFILENAME" },
  { FNERR_SUBCLASSFAILURE, "FNERR_SUBCLASSFAILURE" },
  { FRERR_BUFFERLENGTHZERO, "FRERR_BUFFERLENGTHZERO" },
};

struct param_data
{
  Extbyte *fname;
  Extbyte *unknown_fname;
  int validate;
};

static int
CALLBACK handle_directory_proc (HWND hwnd, UINT msg,
				LPARAM lParam, LPARAM lpData)
{
  Extbyte szDir[PATH_MAX_EXTERNAL];
  struct param_data *pd = (struct param_data *) lpData;
  
  switch (msg)
    {
    case BFFM_INITIALIZED:
      /* WParam is TRUE since you are passing a path. 
	 It would be FALSE if you were passing a pidl. */
      qxeSendMessage (hwnd, BFFM_SETSELECTION, TRUE, (LPARAM) pd->fname);
      break;
      
    case BFFM_SELCHANGED:
      /* Set the status window to the currently selected path. */
      if (qxeSHGetPathFromIDList ((LPITEMIDLIST) lParam, szDir))
	qxeSendMessage (hwnd, BFFM_SETSTATUSTEXT, 0, (LPARAM) szDir);
      break;
      
    case BFFM_VALIDATEFAILED:
      if (pd->validate)
	return TRUE;
      else
	pd->unknown_fname = qxetcsdup ((Extbyte *) lParam);
      break;
      
    default:
      break;
    }
  return 0;
}

static Lisp_Object
handle_directory_dialog_box (struct frame *f, Lisp_Object keys)
{
  Lisp_Object ret = Qnil;
  BROWSEINFOW bi;
  LPITEMIDLIST pidl;
  LPMALLOC pMalloc;
  struct param_data pd;
  
  xzero (pd);
  xzero (bi);
  
  bi.lParam = (LPARAM) &pd;
  bi.hwndOwner = FRAME_MSWINDOWS_HANDLE (f);
  bi.pszDisplayName = 0;
  bi.pidlRoot = 0;
  bi.ulFlags =
    BIF_RETURNONLYFSDIRS | BIF_STATUSTEXT | BIF_EDITBOX | BIF_NEWDIALOGSTYLE;
  bi.lpfn = handle_directory_proc;
  
  LOCAL_FILE_FORMAT_TO_TSTR (Fexpand_file_name (build_string (""), Qnil),
			     pd.fname);
  
  {
    EXTERNAL_PROPERTY_LIST_LOOP_3 (key, value, keys)
      {
	if (EQ (key, Q_title))
	  {
	    CHECK_STRING (value);
	    LISP_STRING_TO_EXTERNAL (value, bi.lpszTitle, Qmswindows_tstr);
	  }
	else if (EQ (key, Q_initial_directory))
	  LOCAL_FILE_FORMAT_TO_TSTR (Fexpand_file_name (value, Qnil),
				     pd.fname);
	else if (EQ (key, Q_initial_filename))
	  ;			/* do nothing */
	else if (EQ (key, Q_file_must_exist))
	  {
	    if (!NILP (value))
	      {
		pd.validate = TRUE;
		bi.ulFlags |= BIF_VALIDATE;
	      }
	    else
	      bi.ulFlags &= ~BIF_VALIDATE;
	  }
	else
	  invalid_constant ("Unrecognized directory-dialog keyword", key);
      }
  }
  
  if (SHGetMalloc (&pMalloc) == NOERROR)
    {
      pidl = qxeSHBrowseForFolder (&bi);
      if (pidl)
	{
	  Extbyte *szDir = alloca_extbytes (PATH_MAX_EXTERNAL);
	  
	  if (qxeSHGetPathFromIDList (pidl, szDir))
	    ret = tstr_to_local_file_format (szDir);
	  
	  XECOMCALL1 (pMalloc, Free, pidl);
	  XECOMCALL0 (pMalloc, Release);
	  return ret;
	}
      else if (pd.unknown_fname != 0)
	{
	  ret = tstr_to_local_file_format (pd.unknown_fname);
	  xfree (pd.unknown_fname, Extbyte *);
	}
      else while (1)
	signal_quit ();
    }
  else
    signal_error (Qdialog_box_error,
		  "Unable to create folder browser",
		  make_int (0));
  return ret;
}

static Lisp_Object
handle_file_dialog_box (struct frame *f, Lisp_Object keys)
{
  OPENFILENAMEW ofn;
  Extbyte fnbuf[8000];
  
  xzero (ofn);
  ofn.lStructSize = sizeof (ofn);
  ofn.Flags = OFN_EXPLORER;
  ofn.hwndOwner = FRAME_MSWINDOWS_HANDLE (f);
  ofn.lpstrFile = (XELPTSTR) fnbuf;
  ofn.nMaxFile = sizeof (fnbuf) / XETCHAR_SIZE;
  qxetcscpy (fnbuf, XETEXT (""));
  
  LOCAL_FILE_FORMAT_TO_TSTR (Fexpand_file_name (build_string (""), Qnil),
			     ofn.lpstrInitialDir);
  
  {
    EXTERNAL_PROPERTY_LIST_LOOP_3 (key, value, keys)
      {
	if (EQ (key, Q_initial_filename))
	  {
	    Extbyte *fnout;
	    
	    CHECK_STRING (value);
	    LOCAL_FILE_FORMAT_TO_TSTR (value, fnout);
	    qxetcscpy (fnbuf, fnout);
	  }
	else if (EQ (key, Q_title))
	  {
	    CHECK_STRING (value);
	    LISP_STRING_TO_TSTR (value, ofn.lpstrTitle);
	  }
	else if (EQ (key, Q_initial_directory))
	  LOCAL_FILE_FORMAT_TO_TSTR (Fexpand_file_name (value, Qnil),
				     ofn.lpstrInitialDir);
	else if (EQ (key, Q_file_must_exist))
	  {
	    if (!NILP (value))
	      ofn.Flags |= OFN_FILEMUSTEXIST | OFN_PATHMUSTEXIST;
	    else
	      ofn.Flags &= ~(OFN_FILEMUSTEXIST | OFN_PATHMUSTEXIST);
	  }
	else
	  invalid_constant ("Unrecognized file-dialog keyword", key);
      }
  }
  
  if (!qxeGetOpenFileName (&ofn))
    {
      DWORD err = CommDlgExtendedError ();
      if (!err)
	{
	  while (1)
	    signal_quit ();
	}
      else
	{
	  int i;
	  
	  for (i = 0; i < countof (common_dialog_errors); i++)
	    {
	      if (common_dialog_errors[i].errmess == err)
		signal_error (Qdialog_box_error,
			      "Creating file-dialog-box",
			      build_msg_string
			      (common_dialog_errors[i].errname));
	    }
	  
	  signal_error (Qdialog_box_error,
			"Unknown common dialog box error???",
			make_int (err));
	}
    }
  
  return tstr_to_local_file_format ((Extbyte *) ofn.lpstrFile);
}

static Lisp_Object
handle_question_dialog_box (struct frame *f, Lisp_Object keys)
{
  Lisp_Object_dynarr *dialog_items = Dynarr_new (Lisp_Object);
  unsigned_char_dynarr *template_ = Dynarr_new (unsigned_char);
  int button_row_width = 0;
  int text_width, text_height;
  Lisp_Object question = Qnil, title = Qnil;
  
  int unbind_count = specpdl_depth ();
  record_unwind_protect (free_dynarr_opaque_ptr,
			 make_opaque_ptr (dialog_items));
  record_unwind_protect (free_dynarr_opaque_ptr,
			 make_opaque_ptr (template_));
  
  /* A big NO NEED to GCPRO gui_items stored in the array: they are just
     pointers into KEYS list, which is GC-protected by the caller */
  
  {
    EXTERNAL_PROPERTY_LIST_LOOP_3 (key, value, keys)
      {
	if (EQ (key, Q_question))
	  {
	    CHECK_STRING (value);
	    question = value;
	  }
	else if (EQ (key, Q_title))
	  {
	    CHECK_STRING (value);
	    title = value;
	  }
	else if (EQ (key, Q_buttons))
	  {
	    /* Parse each item in the dialog into gui_item structs,
	       and stuff a dynarr of these. Calculate button row width
	       in this loop too */
	    EXTERNAL_LIST_LOOP_2 (item, value)
	      {
		if (!NILP (item))
		  {
		    Lisp_Object gitem = gui_parse_item_keywords (item);
		    Dynarr_add (dialog_items, gitem);
		    button_row_width += button_width (XGUI_ITEM (gitem)->name) 
		      + X_BUTTON_MARGIN;
		  }
	      }
	    
	    button_row_width -= X_BUTTON_MARGIN;
	  }
	else
	  invalid_constant ("Unrecognized question-dialog keyword", key);
      }
  }
  
  if (Dynarr_length (dialog_items) == 0)
    sferror ("Dialog descriptor provides no buttons", keys);
  
  if (NILP (question))
    sferror ("Dialog descriptor provides no question", keys);
  
  /* Determine the final width layout */
  {
    Ibyte *p = XSTRING_DATA (question);
    Charcount string_max = 0, this_length = 0;
    while (1)
      {
	Ichar ch = itext_ichar (p);
	INC_IBYTEPTR (p);
	
	if (ch == (Ichar)'\n' || ch == (Ichar)'\0')
	  {
	    string_max = max (this_length, string_max);
	    this_length = 0;
	  }
	else
	  ++this_length;
	
	if (ch == (Ichar)'\0')
	  break;
      }
    
    if (string_max * X_DLU_PER_CHAR > max (X_MAX_TEXT, button_row_width))
      text_width = X_AVE_TEXT;
    else if (string_max * X_DLU_PER_CHAR < X_MIN_TEXT)
      text_width = X_MIN_TEXT;
    else
      text_width = string_max * X_DLU_PER_CHAR;
    text_width = max (text_width, button_row_width);
  }
  
  /* Now calculate the height for the text control */
  {
    Ibyte *p = XSTRING_DATA (question);
    Charcount break_at = text_width / X_DLU_PER_CHAR;
    Charcount char_pos = 0;
    int num_lines = 1;
    Ichar ch;
    
    while ((ch = itext_ichar (p)) != (Ichar) '\0')
      {
	INC_IBYTEPTR (p);
	char_pos += ch != (Ichar) '\n';
	if (ch == (Ichar) '\n' || char_pos == break_at)
	  {
	    ++num_lines;
	    char_pos = 0;
	  }
      }
    text_height = Y_TEXT_MARGIN + Y_DLU_PER_CHAR * num_lines;
  }
  
  /* Ok, now we are ready to stuff the dialog template and lay out controls */
  {
    DLGTEMPLATE dlg_tem;
    DLGITEMTEMPLATE item_tem;
    int i;
    const unsigned int zeroes = 0;
    const unsigned int ones = 0xFFFFFFFF;
    const WORD static_class_id = 0x0082;
    const WORD button_class_id = 0x0080;
    
    /* Create and stuff in DLGTEMPLATE header */
    dlg_tem.style = (DS_CENTER | DS_MODALFRAME
		     | WS_CAPTION | WS_POPUP | WS_VISIBLE);
    dlg_tem.dwExtendedStyle = 0;
    dlg_tem.cdit = Dynarr_length (dialog_items) + 1;
    dlg_tem.x = 0;
    dlg_tem.y = 0;
    dlg_tem.cx = text_width + 2 * X_TEXT_FROM_EDGE;
    dlg_tem.cy = (Y_TEXT_FROM_EDGE + text_height + Y_TEXT_FROM_BUTTON
		  + Y_BUTTON + Y_BUTTON_FROM_EDGE);
    Dynarr_add_many (template_, &dlg_tem, sizeof (dlg_tem));
    
    /* We want no menu and standard class */
    Dynarr_add_many (template_, &zeroes, 4);
    
    /* And the third is the dialog title. "XEmacs" unless one is supplied.
       Note that the string must be in Unicode. */
    if (NILP (title))
      Dynarr_add_many (template_, L"XEmacs", 14);
    else
      push_lisp_string_as_unicode (template_, title);
    
    /* Next add text control. */
    item_tem.style = WS_CHILD | WS_VISIBLE | SS_LEFT | SS_NOPREFIX;
    item_tem.dwExtendedStyle = 0;
    item_tem.x = X_TEXT_FROM_EDGE;
    item_tem.y = Y_TEXT_FROM_EDGE;
    item_tem.cx = text_width;
    item_tem.cy = text_height;
    item_tem.id = 0xFFFF;
    
    ALIGN_TEMPLATE;
    Dynarr_add_many (template_, &item_tem, sizeof (item_tem));
    
    /* Right after class id follows */
    Dynarr_add_many (template_, &ones, 2);
    Dynarr_add_many (template_, &static_class_id, sizeof (static_class_id));
    
    /* Next thing to add is control text, as Unicode string */
    push_lisp_string_as_unicode (template_, question);
    
    /* Specify 0 length creation data */
    Dynarr_add_many (template_, &zeroes, 2);
    
    /* Now it's the button time */
    item_tem.y = Y_TEXT_FROM_EDGE + text_height + Y_TEXT_FROM_BUTTON;
    item_tem.x = X_BUTTON_FROM_EDGE + (button_row_width < text_width
				       ? (text_width - button_row_width) / 2
				       : 0);
    item_tem.cy = Y_BUTTON;
    item_tem.dwExtendedStyle = 0;
    
    for (i = 0; i < Dynarr_length (dialog_items); ++i)
      {
	Lisp_Object *gui_item = Dynarr_atp (dialog_items, i);
	Lisp_Gui_Item *pgui_item = XGUI_ITEM (*gui_item);
	
	item_tem.style = (WS_CHILD | WS_VISIBLE | WS_TABSTOP | BS_PUSHBUTTON
			  | (gui_item_active_p (*gui_item) ? 0 : WS_DISABLED));
	item_tem.cx = button_width (pgui_item->name);
	/* Item ids are indices into dialog_items plus offset, to avoid having
           items by reserved ids (IDOK, IDCANCEL) */
	item_tem.id = i + ID_ITEM_BIAS;
	
	ALIGN_TEMPLATE;
	Dynarr_add_many (template_, &item_tem, sizeof (item_tem));
	
	/* Right after 0xFFFF and class id atom follows */
	Dynarr_add_many (template_, &ones, 2);
	Dynarr_add_many (template_, &button_class_id,
			 sizeof (button_class_id));
	
	/* Next thing to add is control text, as Unicode string */
	{
	  Ichar accel_unused;
	  
	  push_lisp_string_as_unicode
	    (template_,
	     mswindows_translate_menu_or_dialog_item
	     (pgui_item->name, &accel_unused));
	}
	
	/* Specify 0 length creation data. */
	Dynarr_add_many (template_, &zeroes, 2);
	
	item_tem.x += item_tem.cx + X_BUTTON_SPACING;
      }
  }
  
  /* Now the Windows dialog structure is ready. We need to prepare a
     data structure for the new dialog, which will contain callbacks
     and the frame for these callbacks.  This structure has to be
     GC-protected and thus it is put into a statically protected
     list. */
  {
    Lisp_Object dialog_data;
    int i;
    struct mswindows_dialog_id *did =
      ALLOC_LCRECORD_TYPE (struct mswindows_dialog_id,
			   &lrecord_mswindows_dialog_id);
    
    dialog_data = wrap_mswindows_dialog_id (did);
    
    did->frame = wrap_frame (f);
    did->callbacks = make_vector (Dynarr_length (dialog_items), Qunbound);
    for (i = 0; i < Dynarr_length (dialog_items); i++)
      XVECTOR_DATA (did->callbacks) [i] =
	XGUI_ITEM (*Dynarr_atp (dialog_items, i))->callback;
    
    /* Woof! Everything is ready. Pop pop pop in now! */
    did->hwnd =
      qxeCreateDialogIndirectParam (NULL,
				    (LPDLGTEMPLATE) Dynarr_atp (template_, 0),
				    FRAME_MSWINDOWS_HANDLE (f), dialog_proc,
				    (LPARAM) LISP_TO_VOID (dialog_data));
    if (!did->hwnd)
      /* Something went wrong creating the dialog */
      signal_error (Qdialog_box_error, "Creating dialog", keys);
    
    Vdialog_data_list = Fcons (dialog_data, Vdialog_data_list);
    
    /* Cease protection and free dynarrays */
    unbind_to (unbind_count);
    return dialog_data;
  }
}

static Lisp_Object
mswindows_make_dialog_box_internal (struct frame* f, Lisp_Object type,
				    Lisp_Object keys)
{
  int unbind_count = specpdl_depth ();
  record_unwind_protect (dialog_popped_down, Qnil);
  popup_up_p++;

  if (EQ (type, Qfile))
    return unbind_to_1 (unbind_count, handle_file_dialog_box (f, keys));
  else if (EQ (type, Qdirectory))
    return unbind_to_1 (unbind_count, handle_directory_dialog_box (f, keys));
  else if (EQ (type, Qquestion))
    return unbind_to_1 (unbind_count, handle_question_dialog_box (f, keys));
  else if (EQ (type, Qprint))
    return unbind_to_1 (unbind_count,
			mswindows_handle_print_dialog_box (f, keys));
  else if (EQ (type, Qpage_setup))
    return unbind_to_1 (unbind_count, 
		        mswindows_handle_page_setup_dialog_box (f, keys));
  else
    signal_error (Qunimplemented, "Dialog box type", type);
  return Qnil;
}

void
console_type_create_dialog_mswindows (void)
{
  CONSOLE_HAS_METHOD (mswindows, make_dialog_box_internal);
}

void
syms_of_dialog_mswindows (void)
{
  INIT_LRECORD_IMPLEMENTATION (mswindows_dialog_id);
  
  DEFKEYWORD (Q_initial_directory);
  DEFKEYWORD (Q_initial_filename);
  DEFKEYWORD (Q_filter_list);
  DEFKEYWORD (Q_title);
  DEFKEYWORD (Q_allow_multi_select);
  DEFKEYWORD (Q_create_prompt_on_nonexistent);
  DEFKEYWORD (Q_overwrite_prompt);
  DEFKEYWORD (Q_file_must_exist);
  DEFKEYWORD (Q_no_network_button);
  DEFKEYWORD (Q_no_read_only_return);
  
  /* Errors */
  DEFERROR_STANDARD (Qdialog_box_error, Qgui_error);
}

void
vars_of_dialog_mswindows (void)
{
  Vpopup_frame_list = Qnil;
  staticpro (&Vpopup_frame_list);
  
  Vdialog_data_list = Qnil;
  staticpro (&Vdialog_data_list);
  
  DEFVAR_LISP ("default-file-dialog-filter-alist",
	       &Vdefault_file_dialog_filter_alist /*
						   */ );
  Vdefault_file_dialog_filter_alist =
    list5 (Fcons (build_msg_string ("Text Files"), build_string ("*.txt")),
	   Fcons (build_msg_string ("C Files"), build_string ("*.c;*.h")),
	   Fcons (build_msg_string ("Elisp Files"), build_string ("*.el")),
	   Fcons (build_msg_string ("HTML Files"), build_string ("*.html;*.html")),
	   Fcons (build_msg_string ("All Files"), build_string ("*.*")));
}
