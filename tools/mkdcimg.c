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
#include <stdlib.h>
#include <string.h>
#include <getopt.h>

typedef enum { Linear=1, Grid, Const } t_mode;

typedef struct {
  t_mode mode;
  int val;
  int nr;
  int nc;
  int abbrev;
  char *ofile;
} args_t;

static args_t args;

typedef enum { opt_linear=1, opt_grid, opt_const, opt_outf, opt_abbrev } opt_index;

static struct option options[] = {
  {"const", required_argument, 0, opt_const},
  {"linear", required_argument, 0, opt_linear},
  {"grid", required_argument, 0, opt_grid},
  {"out", required_argument, 0, opt_outf},
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
    case opt_const:
      args->mode = Const; 
      if ( sscanf(optarg,"%d",&args->val) !=1  ) { fprintf(stderr, "%s: invalid argument for mode\n", argv[0]); return 1; }
      break;
    case opt_linear:
      args->mode = Linear; 
      if ( sscanf(optarg,"%d",&args->val) !=1  ) { fprintf(stderr, "%s: invalid argument for mode\n", argv[0]); return 1; }
      break;
    case opt_grid:
      args->mode = Grid; 
      if ( sscanf(optarg,"%d",&args->val) !=1  ) { fprintf(stderr, "%s: invalid argument for mode\n", argv[0]); return 1; }
      break;
    case opt_outf:
      args->ofile = optarg;
      break;
    case opt_abbrev:
      args->abbrev = 1;
      break;
    default:
      fprintf(stderr, "%s: illegal option\n", argv[0]);
      return 1;
    }
  }
  if ( optind < argc )
    if ( sscanf(argv[optind], "%d", &args->nr) != 1 ) { fprintf(stderr, "%s: illegal value for nr\n", argv[0]); return 1; };
  optind++;
  if ( optind < argc )
    if ( sscanf(argv[optind], "%d", &args->nc) != 1 ) { fprintf(stderr, "%s: illegal value for nc\n", argv[0]); return 1; };
  return 0;
}

void write_res(FILE *of, args_t args)
{
  int i, j, k=1;
  int v;
  fprintf(of, args.abbrev ? "< " : "SoS ");
  for ( i=0; i<args.nr; i++ ) {
    fprintf(of, args.abbrev ? "< " : "SoS ");
    for ( j=0; j<args.nc; j++ ) {
      switch ( args.mode ) {
      case Const: v = args.val; break;
      case Linear: v = k++ % args.val; break;
      case Grid: v = (i+1)*args.val+(j+1); break;
      }
      fprintf(of, "%s%d ", args.abbrev ? "" : "Data ", v);
      }
    fprintf(of, args.abbrev ? "> " : "EoS ");
    }
  fprintf(of, args.abbrev ? "> " : "EoS ");
  fprintf(of, "\n");
}

int main(int argc, char **argv)
{
  args_t args = {Const, 1, 0, 0, 0, NULL};
  FILE *fpw;
  parse_args(argc, argv, &args);
  if ( args.nr==0 || args.nc==-0 )
    { fprintf(stderr,
              "usage: %s [-const val | -linear max | -grid width] [-out <file>] [-abbrev] nrows ncols\n",
              argv[0]);
      exit(1); }
  if ( args.ofile ) {
    fpw = fopen(args.ofile, "w");
    if ( fpw == NULL ) { fprintf(stderr, "%s: cannot open file %s\n", argv[0], args.ofile); exit(1); }
    }
  else
    fpw = stdout;
  write_res(fpw, args);
  fclose(fpw);
  return 0;
}
