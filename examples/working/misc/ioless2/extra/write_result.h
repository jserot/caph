// This file is manually written. Do not erase it !

#ifndef _write_result_h
#define _write_result_h

#include <systemc.h>
#include "fifo.h"

SC_MODULE(snk_act) {
  sc_in<bool> clk;
  sc_port<fifo_in_if<sc_uint<8>  > > i;

  void main(void);

  SC_HAS_PROCESS(snk_act);

  snk_act(sc_module_name name_, bool trace_=false  ) :
    modname(name_), sc_module(name_), trace(trace_) 
  {
    SC_THREAD(main);
    sensitive << clk.pos();
  }

  ~snk_act() { }

  private:
    // Service
    bool trace;
    sc_module_name modname;
};
#endif
