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

typedef struct {
  char *ofile;
  char *ifile;
  int hblank;
  int vblank;
  int width;
} args_t;

#ifdef DEBUG
void dump_args(args_t args)
{
  printf("hblank=%d vblank=%d fmt=%s bitwidth=%d ", args.hblank, args.vblank, args.width);
  if ( args.ifile ) printf("ofile=%s ", args.ofile);
  if ( args.ofile ) printf("ofile=%s ", args.ofile);
  printf("\n"); 
}
#endif

typedef enum { opt_hblank=1, opt_vblank } opt_index;

static struct option options[] = {
  {"hblank", required_argument, 0, opt_hblank},
  {"vblank", required_argument, 0, opt_vblank},
  {0, 0, 0, 0}
};

int parse_args(int argc, char **argv, args_t *args)
{
  int c, i, option_index = 0;
  while ( 1 ) {
    c = getopt_long_only(argc, argv, "", options, &option_index);
    if ( c == -1 ) break;
    switch ( c ) {  // options
    case opt_hblank:
      if ( sscanf(optarg,"%d",&args->hblank) !=1  ) { fprintf(stderr, "%s: invalid hblank value\n", argv[0]); return 1; }
      break;
    case opt_vblank:
      if ( sscanf(optarg,"%d",&args->vblank) !=1  ) { fprintf(stderr, "%s: invalid vblank value\n", argv[0]); return 1; }
      break;
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

int convert(char* pgm, char *ifname, char *ofname, int hblank, int vblank, int width)
{
  FILE *fpr, *fpw;
  char tok[32];
  char bin[128];
  int i, j, k, w, h, maxv;
  unsigned int pix;

  fpr = fopen(ifname, "r");
  if ( fpr == NULL ) { fprintf(stderr, "%s: cannot open file %s\n", pgm, ifname); return 1; }
  i++;
  fpw = fopen(ofname, "w");
  if ( fpw == NULL ) { fprintf(stderr, "%s: cannot open file %s\n", pgm, ofname); return 1; }
  if ( fscanf(fpr, "%s %d %d %d", tok, &w, &h, &maxv) != 4 )
    { fprintf(stderr,"%s: bad PGM header\n", pgm); return 1; }
  if ( strcmp(tok, "P2") != 0 ) { fprintf(stderr,"%s: sorry, can only P2-type pgm files\n", pgm); return 1; }
  encode_bin_token(bin, width, Tok_SoS, 0, 0);
  fprintf(fpw,"%s\n",bin); // SoF
  //fprintf(stderr,"%s: reading %dx%d pixel (max=%d)...\n", pgm, h, w, maxv);
  for ( i=0; i<h; i++ ) {
    encode_bin_token(bin, width, Tok_SoS, 0, 0);
    fprintf(fpw,"%s\n",bin); // SoL
    for ( j=0; j<w; j++ ) {
      if ( fscanf(fpr, "%d", &pix) != 1 ) { fprintf(stderr,"%s: bad pixel\n", pgm); return 1; }
      encode_bin_token(bin, width, Tok_Data, 1, pix);
      fprintf(fpw,"%s\n",bin);         // Pixel
      }
    encode_bin_token(bin, width, Tok_EoS, 0, 0);
    fprintf(fpw,"%s\n",bin); // EoL
    encode_bin_token(bin, width, Tok_Nodata, 0, 0);
    for ( k=0; k<hblank; k++ ) fprintf(fpw,"%s\n",bin); // Horizontal blanking
    }
  encode_bin_token(bin, width, Tok_EoS, 0, 0);
  fprintf(fpw,"%s\n",bin); // EoF
  encode_bin_token(bin, width, Tok_Nodata, 0, 0);
  for ( k=0; k<vblank; k++ ) fprintf(fpw,"%s\n",bin); // Vertical blanking
  fclose(fpr); 
  fclose(fpw); 
  return 0;
}

int main(int argc, char **argv)
{
  args_t args = {NULL, NULL};
  if ( parse_args(argc, argv, &args) ) 
    { fprintf(stderr, "usage: %s [-hblank <n>] [-vblank <n>] <bitwidth> <pgm file> <bin file>\n", argv[0]); exit(1); }
  args.width += 2;
  if ( convert(argv[0], args.ifile, args.ofile, args.hblank, args.vblank, args.width) != 0 ) return 1;
  return 0;
}

