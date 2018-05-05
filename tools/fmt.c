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

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "tyconv.h"
#include "fmt.h"

int decode_fmt(char *s, fmt_t *fmt)
{
  if ( !strcmp(s,"sint") ) { *fmt=fmt_sint; return 0; }
  else if ( !strcmp(s,"uint") ) { *fmt=fmt_uint; return 0; }
  else if ( !strcmp(s,"float") ) { *fmt=fmt_float; return 0; }
  else if ( !strcmp(s,"bool") ) { *fmt=fmt_bool; return 0; }
  else return 1;
}

int decode_int(char *s, int *res)
{
  return sscanf(s,"%d",res) == 1 ? 0 : 1;
}
