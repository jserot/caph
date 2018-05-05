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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <getopt.h>
#include "fmt.h"
#include "dc.h"

void get_suffix(char *path, char **base, char **suffix)
{
  char *p, *s, *d;
  p = strrchr(path, '.'); 
  if ( p != NULL ) {
    *p = 0;
    *base = path;
    *suffix = p+1;
    }
  else {
    *base = path;
    *suffix = 0;
    }
}

void mk_fname(char *fname, char *base, char *suffix, int multi, int k)
{
  if ( multi ) {
    if ( suffix != NULL ) 
      sprintf(fname, "%s_%d.%s", base, k, suffix);
    else
      sprintf(fname, "%s_%d", base, k);
    } 
  else {
    if ( suffix != NULL ) 
      sprintf(fname, "%s.%s", base, suffix);
    else
     sprintf(fname, "%s_%d", base, k);
    }
}

typedef struct {
  int abbrev;
  int maxv;
  char *ifile;
  char *ofile;
} args_t;

typedef enum { opt_abbrev=1 } opt_index;

static struct option options[] = {
  {"abbrev", no_argument,  0, opt_abbrev},
  {0, 0, 0, 0}
};

int parse_args(int argc, char **argv, args_t *args)
{
  int c, i, option_index = 0;
  while ( 1 ) {
    c = getopt_long_only(argc, argv, "", options, &option_index);
    if ( c == -1 ) break;
    switch ( c ) {  // options
    case opt_abbrev:
      args->abbrev = 1;
      break;
    default:
      fprintf(stderr, "%s: illegal option\n", argv[0]);
      return 1;
    }
  }
  // regular args
  if ( decode_int(argv[optind], &args->maxv) != 0 ) { fprintf(stderr, "%s: illegal maxv\n", argv[0]); return 1; };
  optind++;
  args->ifile = optind < argc ? argv[optind] : NULL; 
  optind++;
  args->ofile = optind < argc ? argv[optind] : NULL; 
  return 0;
}

int convert(char* pgm, char *ifname, char *ofname, int abbrev, int maxv)
{
  FILE *fpr, *fpw;
  char bin[32], fname[64], *basename, *suffix;
  int pix;
  int w=0, h=0;
  enum { S0=0, S1, S2 } state = S0;

  fpr = fopen(ifname, "r");
  if ( fpr == NULL ) { fprintf(stderr, "%s: cannot open file %s\n", pgm, ifname); return 1; }
  fpw = fopen(ofname, "w");
  if ( fpw == NULL ) { fprintf(stderr, "%s: cannot open file %s for writting\n", pgm, ofname); return 1; }
  fprintf(fpw,"%s\n%4d%4d\n%4d\n","P2",w,h,maxv);
  while ( !feof(fpr) ) {
    if ( fscanf(fpr, "%s", bin) != 1 ) break;
    switch ( state ) {
    case S0:
      if ( (abbrev && !strcmp(bin,"<")) || !strcmp(bin,"SoS")  ) { // SoF
        h=0;
        state=S1;
        }
      else
        { fprintf(stderr, "%s: invalid entry:  %s\n", pgm, bin); return 1; }
      break;
    case S1:
      if ( (abbrev && !strcmp(bin,">")) || !strcmp(bin,"EoS")  ) { // EoF
        fprintf(fpw, "\n");
        fclose(fpw);
        fpw = fopen(ofname, "r+");
        fprintf(fpw,"%s\n%4d%4d\n%4d\n","P2",w,h,maxv);
        fclose(fpw);
        }
      else if ( (abbrev && !strcmp(bin,"<")) || !strcmp(bin,"SoS")  ) { // SoL
        w=0;
        state=S2;
        }
      else
        { fprintf(stderr, "%s: invalid entry:  %s\n", pgm, bin); return 1; }
      break;
    case S2:
      if ( (abbrev && !strcmp(bin,">")) || !strcmp(bin,"EoS")  ) { // EoL
        h++; 
        fprintf(fpw, "\n");
        state=S1;
        }
      else if ( (abbrev && sscanf(bin,"%d",&pix)==1) || (!strcmp(bin,"Data") && fscanf(fpr,"%d",&pix)==1)  ) { // Pixel
        w++;
        if ( pix < 0 ) pix = -pix; // Does not make sens to write negative values in PGM files ? 
        //m=pix>m?pix:m;
        fprintf(fpw, "%3ld ", pix);
        }
      else
        { fprintf(stderr, "%s: invalid entry:  %s\n", pgm, bin); return 1; }
      break;
    }
  }
  fclose(fpr); 
  return 0;
}

int main(int argc, char **argv)
{
  args_t args = {0, 0, NULL, NULL};
  if ( parse_args(argc, argv, &args) ) 
    { fprintf(stderr, "usage: %s [-abbrev] maxv <txt file> <pgm file>\n", argv[0]); exit(1); }
  if ( convert(argv[0], args.ifile, args.ofile, args.abbrev, args.maxv) != 0 ) return 1;
  return 0;
}
