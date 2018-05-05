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

#ifndef _tyconv_h
#define _tyconv_h

#include <stdio.h>

typedef unsigned int uint32;  // This may have to be modified..
typedef float float32;        // This should hold everywhere..

FILE* get_args(int argc, char **argv);
void close_file(FILE *fp);

void int2bin(int v, int width, int is_signed, char *res);
void sint2bin(char *v, int width, char *res);
void uint2bin(char *v, int width, char *res);
void bool2bin(char *v, int width, char *res); // The [width] arg is assumed to be 1
void float2bin(char *v, int width, char *res);

int bin2int(char *s, int is_signed, int width);
void bin2sint(char *arg, int width, char *res);
void bin2uint(char *arg, int width, char *res);
void bin2bool(char *arg, int width, char *res);  // The [width] arg is assumed to be 1
void bin2float(char *arg, int width, char *res);

uint32 float2int(float32 a);
float32 int2float(uint32 a);

#endif
