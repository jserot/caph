// This file is manually written. Do not erase it !

#include "write_result.h"

void snk_act::main(void) {
  sc_uint<8> x;
    while ( 1 ) { 
      wait(); // clk
      if ( (i->rd_rdy()) ) { // Input channel is ready for reading
          x = i->read();
          cout << "** snk_act: " << x << endl;
          }
    }
}
