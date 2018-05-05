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
#include "tyconv.h"
#include "fmt.h"
#include "dc.h"

typedef struct {
  int width;
  char *ifile;
  char *ofile;
  // int split_frames;
} args_t;

//typedef enum { /* opt_split_frames */ } opt_index;

static struct option options[] = {
  //{"out", required_argument,  0, opt_outf},
  // {"split_output_frames", no_argument,  0, opt_split_frames},
  {0, 0, 0, 0}
};

int parse_args(int argc, char **argv, args_t *args)
{
  int c, i, option_index = 0;
  while ( 1 ) {
    c = getopt_long_only(argc, argv, "", options, &option_index);
    if ( c == -1 ) break;
    switch ( c ) {  // options
/*     case opt_split_frames: */
/*       args->split_frames = 1; */
/*       break; */
/*     case opt_outf: */
/*       args->ofile = optarg; */
/*       break; */
    default:
      fprintf(stderr, "%s: illegal option\n", argv[0]);
      return 1;
    }
  }
  // regular args
  if ( decode_int(argv[optind++], &args->width) != 0 ) { fprintf(stderr, "%s: illegal bit width\n", argv[0]); return 1; };
  args->ifile = optind < argc ? argv[optind] : NULL; 
  optind++;
  args->ofile = optind < argc ? argv[optind] : NULL; 
  return 0;
}


int convert(char* pgm, char *ifname, char *ofname, int bitwidth)
{
  FILE *fpr, *fpw;
  char bin[128];
  int w=0, h=0, maxv=-1;
  int v;
  tok_type t;
  
  enum { S0=0, S1, S2 } state = S0;

  fpr = fopen(ifname, "r");
  if ( fpr == NULL ) { fprintf(stderr, "%s: cannot open file %s\n", pgm, ifname); return 1; }
  fpw = fopen(ofname, "w");
  if ( fpw == NULL ) { fprintf(stderr, "%s: cannot open file %s for writting\n", pgm, ofname); return 1; }
  fprintf(fpw,"%s\n%4d%4d\n%4d\n","P2",w,h,maxv);
  while ( !feof(fpr) ) {
    if ( fscanf(fpr, "%s", bin) != 1 ) break;
    t = decode_bin_token(bin);
    switch ( state ) {
    case S0:
      if ( t == Tok_SoS ) { // SoF
        h=0;
        state=S1;
        }
      else
        { fprintf(stderr, "%s: invalid entry:  %s\n", pgm, bin); return 1; }
      break;
    case S1:
      if ( t == Tok_EoS ) { // EoF
        fprintf(fpw, "\n");
        fclose(fpw);
        fpw = fopen(ofname, "r+");
        fprintf(fpw,"%s\n%4d%4d\n%4d\n","P2",w,h,maxv);
        fclose(fpw);
        }
      else if ( t == Tok_SoS  ) { // SoL
        w=0;
        state=S2;
        }
      else
        { fprintf(stderr, "%s: invalid entry:  %s\n", pgm, bin); return 1; }
      break;
    case S2:
      if ( t == Tok_EoS  ) { // EoL
        h++; 
        fprintf(fpw, "\n");
        state=S1;
        }
      else if ( t == Tok_Data  ) { // Pixel
        v = bin2int(bin+2, 0, bitwidth-2);
        if ( v > maxv ) maxv = v;
        w++;
        if ( v < 0 ) v = -v; // Does not make sens to write negative values in PGM files ? 
        //m=pix>m?pix:m;
        fprintf(fpw, "%3ld ", v);
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
  args_t args = {-1, 0, NULL};
  FILE *fpw;

  parse_args(argc, argv, &args);
#ifdef DEBUG
  dump_args(args);
#endif
  if ( args.width==-1 )
    //{ fprintf(stderr, "usage: %s [-out <file>] [-split_frames] <bitwidth> <bin file>\n", argv[0]);
    { fprintf(stderr, "usage: %s <bitwidth> <bin file> <pgm file>\n", argv[0]);
      exit(1); }
  args.width += 2;
  if ( convert(argv[0], args.ifile, args.ofile, args.width) != 0 ) return 1;
  return 0;
}

