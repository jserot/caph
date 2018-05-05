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

#ifndef _stream_out_h
#define _stream_out_h

#include "fifo.h"
#include <systemc.h>
#include <fstream>
#include <iostream>

template <class T>
SC_MODULE(stream_out) { 
  sc_in<bool> clk; 
  sc_port<fifo_in_if<T> > inp;
  sc_out<bool> started;
  sc_out<bool> stopped;

  void main(void);

  SC_HAS_PROCESS(stream_out);
  
 stream_out(sc_module_name name_, const char *fname_, bool trace_=false, unsigned long idle_time_=0 ) :
  modname(name_), fname(fname_), sc_module(name_), trace(trace_), stop_when_idle(idle_time_), idle_time(idle_time_,SC_NS)
  {
    SC_THREAD(main);
    sensitive << clk.pos(); 
    fp.open(fname_, ios::out);
    if ( !fp ) cerr << "** " << name_ << ": cannot open file " << fname_ << endl;
    fp << std::boolalpha;
  }

    ~stream_out() { fp.close(); cout << "Wrote file " << fname << endl; cout.flush();  }

  private:
    ofstream fp;
    FILE *fpw;
    const char *fname;
    bool trace;
    sc_module_name modname;
    bool stop_when_idle;
    sc_time idle_time;
}; 

template <class T>
void stream_out<T>::main(void) {
    T d;
    sc_time last_time;
    bool _started = false;

    started.write(false);
    stopped.write(false);
    while(1) { 
      wait(); // clk
      if ( stop_when_idle && _started && sc_time_stamp() - last_time > idle_time )
        stopped.write(true);
      if ( inp->rd_rdy() ) {
        _started = true;
        started.write(true);
        last_time = sc_time_stamp();
        d = inp->read();
        if ( trace ) cout << modname << " got " << d << " at " << sc_time_stamp() << endl; 
        fp << d << " ";
        }
    }
}
#endif
