/* Copyright (C) 2003 Ben Wing.
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

#ifndef INCLUDED_hash_h_
#define INCLUDED_hash_h_

typedef struct
{
  const void *key;
  void	     *contents;
} hentry;

typedef int           (*hash_table_test_function) (const void *, const void *);
typedef Hashcode     (*hash_table_hash_function) (const void *);

struct hash_table
{
  hentry	*harray;
  long		zero_set;
  void		*zero_entry;
  Elemcount	size;		/* size of the hasharray */
  Elemcount	fullness;	/* number of entries in the hash table */
  hash_table_hash_function hash_function;
  hash_table_test_function test_function;
};

/* SIZE is the number of initial entries. The hash table will be grown
   automatically if the number of entries approaches the size */
struct hash_table *make_hash_table (Elemcount size);

struct hash_table *make_string_hash_table (Elemcount size);

struct hash_table *make_general_hash_table (Elemcount size,
					    hash_table_hash_function
					    hash_function,
					    hash_table_test_function
					    test_function);

/* Clear HASH-TABLE. A freshly created hash table is already cleared up. */
void clrhash (struct hash_table *hash_table);

/* Free HASH-TABLE and its substructures */
void free_hash_table (struct hash_table *hash_table);

/* Returns a hentry whose key is 0 if the entry does not exist in HASH-TABLE */
const void *gethash (const void *key, struct hash_table *hash_table,
		     const void **ret_value);

/* KEY should be different from 0 */
void puthash (const void *key, void *contents, struct hash_table *hash_table);

/* delete the entry with key KEY */
void remhash (const void *key, struct hash_table *hash_table);

typedef int (*maphash_function) (const void* key, void* contents, void* arg);

typedef int (*remhash_predicate) (const void* key, const void* contents,
                                  void* arg);

/* Call MF (key, contents, arg) for every entry in HASH-TABLE */
void maphash (maphash_function mf, struct hash_table *hash_table, void* arg);

/* Delete all objects from HASH-TABLE satisfying PREDICATE */
void map_remhash (remhash_predicate predicate,
		  struct hash_table *hash_table, void *arg);

/* Grow the table if it has less than BREATHING_ROOM elements that can be
   added before a resize will be triggered.  After the grow, the table can
   hold at least BREATHING_ROOM elements (and probably a lot more) before
   needing resizing again. */
void pregrow_hash_table_if_necessary (struct hash_table *hash_table,
				      Elemcount breathing_room);
#endif /* INCLUDED_hash_h_ */
