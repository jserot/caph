// This file is manually written. Do not erase it !

#include "read_data.h"

void src_act::main(void) {
    cnt = 0;
    while ( 1 ) { 
      wait(); // clk
      if ( (o->wr_rdy()) ) {  // Output channel is ready for writing
          o->write(cnt);
          cnt = cnt+1;
          }
    }
}
