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

#ifndef _dc_h
#define _dc_h

typedef enum { Tok_SoS, Tok_EoS, Tok_Data, Tok_Nodata, Tok_Other } tok_type;

unsigned int val_of_token(fmt_t fmt, char *tok);

tok_type decode_text_token(int abbrev, char *tok);

tok_type decode_bin_token(char *bin);

void encode_bin_token(char *bin, int width, tok_type typ, int is_unsigned, unsigned int data);

#endif
