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
  int fmt_dc;
  int abbrev;
  fmt_t fmt;
  int width;
  int split_frames;
  char *file;
  char *ofile;
} args_t;

typedef enum { opt_dc=1, opt_abbrev, opt_outf, opt_split_frames } opt_index;

static struct option options[] = {
  {"dc", no_argument,  0, opt_dc},
  {"abbrev", no_argument,  0, opt_abbrev},
  {"split_output_frames", no_argument,  0, opt_split_frames},
  {"out", required_argument,  0, opt_outf},
  {0, 0, 0, 0}
};

int parse_args(int argc, char **argv, args_t *args)
{
  int c, i, option_index = 0;
  while ( 1 ) {
    c = getopt_long_only(argc, argv, "", options, &option_index);
    if ( c == -1 ) break;
    switch ( c ) {  // options
    case opt_dc:
      args->fmt_dc = 1;
      break;
    case opt_abbrev:
      args->abbrev = 1;
      break;
    case opt_split_frames:
      args->split_frames = 1;
      break;
    case opt_outf:
      args->ofile = optarg;
      break;
    default:
      fprintf(stderr, "%s: illegal option\n", argv[0]);
      return 1;
    }
  }
  // regular args
  if ( decode_fmt(argv[optind++], &args->fmt) != 0 ) { fprintf(stderr, "%s: illegal format\n", argv[0]); return 1; };
  if ( decode_int(argv[optind++], &args->width) != 0 ) { fprintf(stderr, "%s: illegal bit width\n", argv[0]); return 1; };
  args->file = argv[optind]; 
  return 0;
}

void rem_suffix(char *fname) 
{
  char *p = strrchr(fname, '.');
  if ( p ) *p = '\0';
}

int convert(char* pgm, char *ifname, char *ofname, fmt_t fmt, int fmt_dc, int abbrev, int width, int split_frames)
{
  FILE *fpr, *fpw;
  char tok[32], bin[128], of[64];
  int i, l, k;
  int v;
  tok_type t, tt;

  fpr = fopen(ifname, "r");
  if ( fpr == NULL ) { fprintf(stderr, "%s: cannot open file %s\n", pgm, ifname); return 1; }
  if ( ofname ) {
    rem_suffix(ofname);
    k=1;
    if ( split_frames )
      sprintf(of, "%s_%d.txt", ofname, k++);
    else
      sprintf(of, "%s.txt", ofname);
    fpw = fopen(of, "w");
    if ( fpw == NULL ) { fprintf(stderr, "%s: cannot open file %s\n", pgm, of); return 1; }
    }
  else
    fpw = stdout;
  while ( !feof(fpr) ) {
    if ( fscanf(fpr, "%s", bin) != 1 ) break;
    l = strlen(bin);
    if ( fmt_dc ) {  // DC FMT
      tt = t;
      t = decode_bin_token(bin);
      switch ( t ) {
        case Tok_SoS: strcpy(tok, abbrev ? "<" : "SoS"); break;
        case Tok_EoS: strcpy(tok, abbrev ? ">" : "EoS"); break;
        case Tok_Data:
          v = bin2int(bin+2, fmt==fmt_sint, width-2);
          if ( fmt==fmt_bool ) sprintf(tok, abbrev?"%s":"Data %s", v==1?"true":"false");
          else if ( fmt==fmt_float ) sprintf(tok, abbrev?"%f":"Data %f", int2float((uint32)v));
          else sprintf(tok, abbrev?"%d":"Data %d", v);
          break;
        case Tok_Nodata: break;
        case Tok_Other: sprintf(tok, "???"); break;
        }
      fprintf(fpw,"%s ",tok);
      if ( split_frames && t == Tok_EoS && tt == Tok_EoS ) {  // End of Frame, next output file
        fclose(fpw);
        sprintf(of, "%s_%d.txt", ofname, k++);
        fpw = fopen(of, "w");
        if ( fpw == NULL ) { fprintf(stderr, "%s: cannot open file %s\n", pgm, of); return 1; }
        }
      }
    else {  // RAW FMT
      v = bin2int(bin, fmt==fmt_sint, l);
      if ( fmt==fmt_bool ) sprintf(tok, "%s", v==1?"true":"false");
      else if ( fmt==fmt_float ) sprintf(tok, "%f", int2float((uint32)v));
      else sprintf(tok, "%d", v);
      fprintf(fpw,"%s ",tok);
      }
    }
  fprintf(fpw,"\n");
  fclose(fpr); 
  return 0;
}

int main(int argc, char **argv)
{
  args_t args = {0, 0, fmt_unspecified, -1, 0, NULL};
  FILE *fpw;

  if ( sizeof(uint32) != 4 ) { fprintf(stderr, "%s: warning: sizeof(uint32) != 4: float conversion may fail\n", argv[0]); exit(1); }
  if ( sizeof(float32) != 4 ) { fprintf(stderr, "%s: warning: sizeof(float) != 4: float conversion may fail\n", argv[0]); exit(1); }
  parse_args(argc, argv, &args);
#ifdef DEBUG
  dump_args(args);
#endif
  if ( args.fmt==fmt_unspecified || args.width==-1 )
    { fprintf(stderr, "usage: %s [-dc] [-abbrev] [-out <file>] [-split_frames] <sint|uint|bool|float> <bitwidth> <bin file>\n", argv[0]);
      exit(1); }
  if ( args.fmt==fmt_bool && args.width != 1 ) { fprintf(stderr, "%s: warning: width !=1 for bool; ignored\n", argv[0]); exit(1); }
  if ( args.fmt_dc ) args.width += 2;
  if ( args.fmt == fmt_float && args.width != 32+(2*args.fmt_dc) )
    { fprintf(stderr, "%s: can only handle 32 bit floats\n", argv[0]); exit(1); }
  if ( convert(argv[0], args.file, args.ofile, args.fmt, args.fmt_dc, args.abbrev, args.width, args.split_frames) != 0 ) return 1;
  return 0;
}

