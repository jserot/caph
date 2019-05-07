// This file is manually written. Do not erase it !

#ifndef _read_data_h
#define _read_data_h

#include <systemc.h>
#include "fifo.h"

SC_MODULE(src_act) {
  sc_in<bool> clk;
  sc_port<fifo_out_if<sc_uint<8>  > > o;

  void main(void);

  SC_HAS_PROCESS(src_act);

  src_act(sc_module_name name_, bool trace_=false  ) :
    modname(name_), sc_module(name_), trace(trace_) 
  {
    SC_THREAD(main);
    sensitive << clk.pos();
  }

  ~src_act() { }

  private:
    // Service
    bool trace;
    sc_module_name modname;
    // Parameters
    // Variables
    sc_uint<8>  cnt;
};
#endif
