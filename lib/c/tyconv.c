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

#include "tyconv.h"
#include <stdlib.h>
#include <string.h>

FILE* get_args(int argc, char **argv)
{
  FILE *fp;
  if ( sizeof(uint32) != 4 ) { fprintf(stderr, "%s: warning: sizeof(uint32) != 4: float conversion may fail\n", argv[0]); exit(1); }
  if ( sizeof(float32) != 4 ) { fprintf(stderr, "%s: warning: sizeof(float) != 4: float conversion may fail\n", argv[0]); exit(1); }
  if ( argc != 2 ) { fprintf(stderr, "usage: %s <txt file>\n", argv[0]); exit(1); }
  fp = fopen(argv[1], "r");
  if ( fp == NULL ) { fprintf(stderr, "%s: cannot open file %s\n", argv[0], argv[1]); exit(1); }
  return fp;
}

void close_file(FILE *fp)
{
  fclose(fp);
}

void int2bin(int v, int width, int is_signed, char *res)
{
  int i, c;
  //res[width]=0;
  if ( c = (is_signed && v < 0) ) v = -v;
  for ( i=width-1; i>=0; i-- ) {
    res[i] = v % 2 ? '1' : '0';
    v = v / 2;
    }
  if ( c ) { // CPL2
    i = width-1;
    while ( res[i]=='0' ) i--;
    i--;
    while ( i >= 0) res[i--] = res[i]=='1' ? '0' : '1'; 
    }
}

void sint2bin(char *arg, int width, char *res) { int v; sscanf(arg, "%d", &v); int2bin(v, width, 1, res); }
void uint2bin(char *arg, int width, char *res) { int v; sscanf(arg, "%d", &v); int2bin(v, width, 0, res); }
void bool2bin(char *arg, int width, char *res)
{
  if ( strcmp(arg, "true") == 0 ) int2bin(1, width, 0, res);
  else int2bin(0, width, 0, res);
}
void float2bin(char *arg, int width, char *res)
{
  float32 v;
  sscanf(arg, "%f", &v);
  int2bin(float2int(v), 32, 0, res);
}

int bin2int(char *s, int is_signed, int width)
{
  int i, c;
  int res = 0;
  if ( c = (is_signed && s[0] == '1') ) {  // CPL2
    i = width-1;
    while ( s[i]=='0' ) i--;
    i--;
    while ( i >= 0) s[i--] = s[i]=='1' ? '0' : '1'; 
    }
  for ( i=0; i<width; i++ )
    res = res * 2 + (s[i]=='1'?1:0);
  if ( c ) res = -res;
  return res;
}

void bin2sint(char *arg, int width, char *res) { sprintf(res, "%d", bin2int(arg, 1, width)); }
void bin2uint(char *arg, int width, char *res) { sprintf(res, "%d", bin2int(arg, 0, width)); }
void bin2bool(char *arg, int width, char *res) { sprintf(res, "%s", bin2int(arg, 0, width) ? "true" : "false"); }
void bin2float(char *arg, int width, char *res) { sprintf(res, "%f", int2float(bin2int(arg, 0, 32))); }

uint32 float2int(float32 a) 
{
  union { float32 f; uint32 i; } v;
  v.f =  a;
  return v.i;
}

float32 int2float(uint32 a) 
{
  union { float32 f; uint32 i; } v;
  v.i =  a;
  return v.f;
}
