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
#include "dc.h"

unsigned int val_of_token(fmt_t fmt, char *tok)
{
 switch ( fmt ) {
 case fmt_bool: return strcmp(tok,"true") == 0 ? 1 : 0;
 case fmt_float: return float2int(atof(tok));
 case fmt_sint:
 case fmt_uint: return atoi(tok); 
 case fmt_unspecified: return 0; // should not happen
 }
}

tok_type decode_text_token(int abbrev, char *tok)
{
  if ( abbrev ) {  
    if ( strcmp(tok, "<") == 0 ) return Tok_SoS;
    else if ( strcmp(tok, ">") == 0 ) return Tok_EoS;
    else return Tok_Data;
    }
  else {  
    if ( strcmp(tok, "SoS") == 0 ) return Tok_SoS;
    else if ( strcmp(tok, "EoS") == 0 ) return Tok_EoS;
    else if ( strcmp(tok, "Data") == 0 ) return Tok_Data;
    else return Tok_Other;
    }
}

tok_type decode_bin_token(char *bin)
{
  if ( strncmp(bin, "01", 2) == 0 ) return Tok_SoS;
  else if ( strncmp(bin, "10", 2) == 0 ) return Tok_EoS;
  else if ( strncmp(bin, "11", 2) == 0 ) return Tok_Data;
  else if ( strncmp(bin, "00", 2) == 0 ) return Tok_Nodata;
  else return Tok_Other;
}

void encode_bin_token(char *bin, int width, tok_type typ, int is_unsigned, unsigned int data)
{
  int i;
  for ( i=0; i<width; i++ ) bin[i]='0';
  bin[width]=0;
  switch ( typ ) {
  case Tok_SoS:   bin[1]='1'; break;   // 01xxxxxx
  case Tok_EoS:   bin[0]='1'; break;   // 10xxxxxx
  case Tok_Data: 
    bin[0]=bin[1]='1';  // 11xxxxx
    int2bin(data, width-2, is_unsigned ? 0 : 1, bin+2);
    break;
  case Tok_Nodata: bin[0]=bin[1]='0'; break; // 00xxxxxx
  case Tok_Other: break; // 00xxxxxx
  }
}
