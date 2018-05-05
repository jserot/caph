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

#ifdef _WIN32   // The getline function is not included in the MinGW32 libs
ssize_t getline(char **line, size_t *linecap, FILE *fp)
{
  if ( line == NULL ) { fprintf(stderr,"txt2bin: fatal error when calling getline\n"); exit(100); }
  fgets(*line,*linecap,fp);
  return strlen(*line);
}
#endif

typedef struct {
  int fmt_eventf;
  int fmt_dc;
  int abbrev;
  int hblank;
  int vblank;
  fmt_t fmt;
  int width;
  char *ofile;
  char **files;
} args_t;

#ifdef DEBUG
void dump_args(args_t args)
{
  printf("fmt_dc=%d abbrev=%d hblank=%d vblank=%d fmt=%s bitwidth=%d ", 
         args.fmt_dc, args.abbrev, args.hblank, args.vblank, string_of_fmt(args.fmt), args.width);
  if ( args.ofile ) printf("ofile=%s ", args.ofile);
  if ( args.files ) {
    printf(" ifiles=");
    for ( int i=0; args.files[i]; i++ ) printf("%s ", args.files[i]);
    }
  printf("\n"); 
}
#endif

typedef enum { opt_dc=1, opt_eventf, opt_abbrev, opt_hblank, opt_vblank, opt_outf } opt_index;

static struct option options[] = {
  {"dc", no_argument,  0, opt_dc},
  {"eventf", no_argument,  0, opt_eventf},
  {"abbrev", no_argument,  0, opt_abbrev},
  {"hblank", required_argument, 0, opt_hblank},
  {"vblank", required_argument, 0, opt_vblank},
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
    case opt_eventf:
      args->fmt_eventf = 1;
      break;
    case opt_dc:
      args->fmt_dc = 1;
      break;
    case opt_abbrev:
      args->abbrev = 1;
      break;
    case opt_hblank:
      if ( sscanf(optarg,"%d",&args->hblank) !=1  ) { fprintf(stderr, "%s: invalid hblank value\n", argv[0]); return 1; }
      break;
    case opt_vblank:
      if ( sscanf(optarg,"%d",&args->vblank) !=1  ) { fprintf(stderr, "%s: invalid vblank value\n", argv[0]); return 1; }
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
  args->files = argv+optind; 
  return 0;
}

int convert(char* pgm, char *ifname, FILE *fpw, int fmt_eventf, fmt_t fmt, int fmt_dc, int abbrev, int width, int hblank, int vblank)
{
  int i;
  FILE *fpr;
  char tok[32], bin[128];
  unsigned int v, d1, d2, d3;
  double fv;
  tok_type t, tt;
  char *line;
  size_t linecap = 128;
  ssize_t linelen;

  fpr = fopen(ifname, "r");
  line = (char *)malloc(linecap);
  if ( fpr == NULL ) { fprintf(stderr, "%s: cannot open file %s\n", pgm, ifname); exit(1); }
  if ( fmt_eventf ) { // EVENT FILE
    while ( !feof(fpr) ) {
      linelen = getline(&line, &linecap, fpr);
      if ( linelen == 0 ) break;
      if ( line[0] == '#' ) continue;
      if ( (i = sscanf(line, "%d%d%d%s", &d1, &d2, &d3, tok)) != 4 ) break;
      v = val_of_token(fmt, tok);
      int2bin(v, width, fmt==fmt_uint ? 0 : 1, bin);
      fprintf(fpw,"%d\n%s\n",d3,bin);
      }
    }
  else {              // STREAM FILE
   if ( fmt_dc ) {    // DC
    while ( !feof(fpr) ) {
      if ( fscanf(fpr, "%s", tok) != 1 ) break;
      tt = t;
      t = decode_text_token(abbrev, tok);
      switch ( t ) {
      case Tok_SoS: encode_bin_token(bin,width,Tok_SoS,0,0); break;      // 01xxxxxx
      case Tok_EoS: encode_bin_token(bin,width,Tok_EoS,0,0); break;      // 10xxxxxx
      case Tok_Data: 
          if ( abbrev == 0 )                  // Data v
            if ( fscanf(fpr, "%s", tok) != 1 ) { fprintf(stderr, "%s: cannot read data token\n", pgm); return 3; }
          v = val_of_token(fmt, tok);
          encode_bin_token(bin, width, Tok_Data, fmt_uint, v);        // 11xxxxxx
          break;
      case Tok_Nodata: 
      case Tok_Other: { fprintf(stderr, "%s: invalid token (%s)\n", pgm, tok); return 2; }
      }
      fprintf(fpw,"%s\n",bin);
      if ( hblank > 0 && t == Tok_EoS ) {      // Horizontal blanking
        encode_bin_token(bin, width, Tok_Nodata, 0, 0); 
        for ( i=0; i<hblank; i++ )
          fprintf(fpw,"%s\n",bin);
        }
      if ( vblank > 0 && t == Tok_EoS && tt == Tok_EoS ) {  // Vertical blanking
        encode_bin_token(bin, width, Tok_Nodata, 0, 0);
        for ( i=0; i<vblank; i++ )
          fprintf(fpw,"%s\n",bin);
        }
      }
    }
   else {  // RAW
    while ( !feof(fpr) ) {
      if ( fscanf(fpr, "%s", tok) != 1 ) break;
      v = val_of_token(fmt, tok);
      int2bin(v, width, fmt==fmt_uint ? 0 : 1, bin);
      fprintf(fpw,"%s\n",bin);
      }
    }
  }
  fclose(fpr); 
  return 0;
}

int main(int argc, char **argv)
{
  int i;
  args_t args = {0, 0, 0, 0, 0, fmt_unspecified, -1, NULL};
  FILE *fpw;

  if ( sizeof(uint32) != 4 ) { fprintf(stderr, "%s: warning: sizeof(uint32) != 4: float conversion may fail\n", argv[0]); exit(2); }
  if ( sizeof(float32) != 4 ) { fprintf(stderr, "%s: warning: sizeof(float) != 4: float conversion may fail\n", argv[0]); exit(2); }
  parse_args(argc, argv, &args);
  if ( args.fmt==fmt_unspecified || args.width==-1 )
    { fprintf(stderr,
              "usage: %s [-dc] [-abbrev] [-hblank <n>] [-vblank <n>] [-out <file>] <sint|uint|float|bool> <bitwidth> <txt file(s)>\n",
              argv[0]);
      exit(1); }
  if ( args.fmt==fmt_bool && args.width != 1 ) { fprintf(stderr, "%s: warning: width !=1 for bool; ignored\n", argv[0]); exit(1); }
  if ( args.fmt_dc ) args.width+=2;
  if ( args.fmt == fmt_float && args.width != 32+(2*args.fmt_dc) )
    { fprintf(stderr, "%s: can only handle 32 bit floats\n", argv[0]); exit(1); }
  if ( args.ofile ) {
    fpw = fopen(args.ofile, "w");
    if ( fpw == NULL ) { fprintf(stderr, "%s: cannot open file %s\n", argv[0], args.ofile); exit(1); }
    }
  else
    fpw = stdout;
  for ( i=0; args.files[i] != 0; i++ ) {
    // printf("txt2bin: converting file %s\n", args.files[i]);
    if ( convert(argv[0], args.files[i], fpw, args.fmt_eventf, args.fmt, args.fmt_dc, args.abbrev, args.width, args.hblank, args.vblank) != 0 ) return 1;
    }
  fclose(fpw);
  return 0;
}
