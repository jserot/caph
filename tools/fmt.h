/***************************************************************************************/
/*                                                                                     */
/*                This file is part of the CAPH Compiler distribution                  */
/*                            http://caph.univ-bpclermont.fr                           */
/*                                                                                     */
/*                                  Jocelyn SEROT                                      */
/*                         Jocelyn.Serot@univ-bpclermont.fr                            */
/*                                                                                     */
/*         Copyright 2011-2018 Jocelyn SEROT.  All rights reserved.                    */
/*  This file is distributed under the terms of the GNU Library General Public License */
/*      with the special exception on linking described in file ../LICENSE.            */
/*                                                                                     */
/***************************************************************************************/

#ifndef _binfmt_h
#define _binfmt_h

typedef enum { fmt_sint, fmt_uint, fmt_float, fmt_bool, fmt_unspecified } fmt_t;

int decode_fmt(char *s, fmt_t *fmt);

int decode_int(char *s, int *res);

#ifdef DEBUG
char *string_of_fmt(fmt_t fmt);
#endif

#endif
