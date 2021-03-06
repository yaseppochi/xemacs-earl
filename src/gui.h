/* Generic GUI code. (menubars, scrollbars, toolbars, dialogs)
   Copyright (C) 1995 Board of Trustees, University of Illinois.
   Copyright (C) 1995, 1996, 2002 Ben Wing.
   Copyright (C) 1995 Sun Microsystems, Inc.

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

/* Written by kkm on 12/24/97 */

#ifndef INCLUDED_gui_h_
#define INCLUDED_gui_h_

int separator_string_p (const Ibyte *s);
void get_gui_callback (Lisp_Object, Lisp_Object *, Lisp_Object *);
int gui_item_equal_sans_selected (Lisp_Object obj1, Lisp_Object obj2,
				  int depth);



extern int popup_up_p;

/************************************************************************/
/*			Image Instance Object				*/
/************************************************************************/

/* This structure describes gui button,
   menu item or submenu properties */
struct Lisp_Gui_Item
{
  struct LCRECORD_HEADER header;
  Lisp_Object name;		/* String */
  Lisp_Object callback;		/* Symbol or form */
  Lisp_Object callback_ex;	/* Form taking context arguments */
  Lisp_Object suffix;		/* String */
  Lisp_Object active;		/* Form */
  Lisp_Object included;		/* Form */
  Lisp_Object config;		/* Anything EQable */
  Lisp_Object filter;		/* Form */
  Lisp_Object style;		/* Symbol */
  Lisp_Object selected;		/* Form */
  Lisp_Object keys;		/* String */
  Lisp_Object accelerator;	/* Char or Symbol  */
  Lisp_Object value;		/* Anything you like */
};

DECLARE_LRECORD (gui_item, Lisp_Gui_Item);
#define XGUI_ITEM(x) XRECORD (x, gui_item, Lisp_Gui_Item)
#define wrap_gui_item(p) wrap_record (p, gui_item)
#define GUI_ITEMP(x) RECORDP (x, gui_item)
#define CHECK_GUI_ITEM(x) CHECK_RECORD (x, gui_item)
#define CONCHECK_GUI_ITEM(x) CONCHECK_RECORD (x, gui_item)

int update_gui_item_keywords (Lisp_Object gui_item, Lisp_Object item);
Lisp_Object copy_gui_item (Lisp_Object gui_item);
Lisp_Object widget_gui_parse_item_keywords (Lisp_Object item);
int gui_item_add_keyval_pair (Lisp_Object gui_item,
                              Lisp_Object key, Lisp_Object val,
                              Error_Behavior errb);
Lisp_Object gui_parse_item_keywords (Lisp_Object item);
Lisp_Object gui_parse_item_keywords_no_errors (Lisp_Object item);
void gui_add_item_keywords_to_plist (Lisp_Object plist, Lisp_Object gui_item);
int gui_item_active_p (Lisp_Object);
int gui_item_selected_p (Lisp_Object);
Lisp_Object gui_item_list_find_selected (Lisp_Object gui_item_list);
int gui_item_included_p (Lisp_Object, Lisp_Object into);
Lisp_Object gui_item_accelerator (Lisp_Object gui_item);
Lisp_Object gui_name_accelerator (Lisp_Object name);
int gui_item_id_hash (Lisp_Object, Lisp_Object gui_item, int);
Lisp_Object gui_item_display_flush_left (Lisp_Object gui_item);
Lisp_Object gui_item_display_flush_right (Lisp_Object gui_item);
Lisp_Object allocate_gui_item (void);
void gui_item_init (Lisp_Object gui_item);
Lisp_Object parse_gui_item_tree_children (Lisp_Object list);
Lisp_Object copy_gui_item_tree (Lisp_Object arg);

extern Lisp_Object Qmenu_no_selection_hook, Qdelete_dialog_box_hook;

/* this is mswindows biased but reasonably safe I think */
#define GUI_ITEM_ID_SLOTS 8
#define GUI_ITEM_ID_MIN(s) (s * 0x2000)
#define GUI_ITEM_ID_MAX(s) (0x1FFF + GUI_ITEM_ID_MIN (s))
#define GUI_ITEM_ID_BITS(x,s) (((x) & 0x1FFF) + GUI_ITEM_ID_MIN (s))

#define MAX_MENUITEM_LENGTH 128

#endif /* INCLUDED_gui_h_ */
