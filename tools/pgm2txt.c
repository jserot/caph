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

typedef struct {
  int abbrev;
  char *ofile;
  char *ifile;
} args_t;

#ifdef DEBUG
void dump_args(args_t args)
{
  printf("abbrev=%d ", args.abbrev);
  if ( args.ifile ) printf("ofile=%s ", args.ofile);
  if ( args.ofile ) printf("ofile=%s ", args.ofile);
  printf("\n"); 
}
#endif

typedef enum { opt_abbrev } opt_index;

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
  args->ifile = optind < argc ? argv[optind] : NULL; 
  optind++;
  args->ofile = optind < argc ? argv[optind] : NULL; 
  return 0;
}

int convert(char* pgm, char *ifname, char *ofname, int abbrev)
{
  FILE *fpr, *fpw;
  char tok[32];
  int i, j, w, h, maxv;
  unsigned int pix;

  fpr = fopen(ifname, "r");
  if ( fpr == NULL ) { fprintf(stderr, "%s: cannot open file %s\n", pgm, ifname); return 1; }
  i++;
  fpw = fopen(ofname, "w");
  if ( fpw == NULL ) { fprintf(stderr, "%s: cannot open file %s\n", pgm, ofname); return 1; }
  if ( fscanf(fpr, "%s %d %d %d", tok, &w, &h, &maxv) != 4 )
    { fprintf(stderr,"%s: bad PGM header\n", pgm); return 1; }
  if ( strcmp(tok, "P2") != 0 ) { fprintf(stderr,"%s: sorry, can only P2-type pgm files\n", pgm); return 1; }
  fprintf(fpw, "%s ", abbrev?"<":"SoS"); // SoF
  for ( i=0; i<h; i++ ) {
    fprintf(fpw, "%s ", abbrev?"<":"SoS"); // SoL
    for ( j=0; j<w; j++ ) {
      if ( fscanf(fpr, "%d", &pix) != 1 ) { fprintf(stderr,"%s: bad pixel\n", pgm); return 1; }
      fprintf(fpw, abbrev?"%d ":"Data %d ", pix); // Pixel
      }
    fprintf(fpw, "%s ", abbrev?">":"EoS"); // SoL
    }
  fprintf(fpw, "%s ", abbrev?">":"EoS"); // EoF
  fclose(fpr); 
  fclose(fpw); 
  return 0;
}

int main(int argc, char **argv)
{
  args_t args = {0, NULL, NULL};
  if ( parse_args(argc, argv, &args) ) 
    { fprintf(stderr, "usage: %s [-abbrev] <pgm file> <txt file>\n", argv[0]); exit(1); }
  if ( convert(argv[0], args.ifile, args.ofile, args.abbrev) != 0 ) return 1;
  return 0;
}

