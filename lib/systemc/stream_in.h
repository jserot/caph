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

#ifndef _stream_in_h
#define _stream_in_h

#include "fifo.h"
#include <systemc.h>
#include <fstream>
#include "sc_int_io.h"

template <class T>
SC_MODULE(stream_in) { 
  sc_in<bool> clk; 
  sc_port<fifo_out_if<T> > out;
  sc_out<bool> started;

  bool read_from_raw_file(T& d);
  bool write_out(T d);

  void main();

  SC_HAS_PROCESS(stream_in);
  
 stream_in(sc_module_name name_, const char *fname_, bool trace_=false, int period_=1) :
  modname(name_), sc_module(name_), fname(fname_), trace(trace_), period(period_)
  {
    SC_THREAD(main);
    sensitive << clk.pos(); 
    fp.open(fname_, ios::in);
    if ( !fp ) cerr << "*** " << name_ << ": cannot open file " << fname_ << endl;
    if ( period < 1 ) cerr << "** error: " << name_ << " : stream_in period < 1 ! " << endl;
    fp >> std::boolalpha;
  }

  ~stream_in() { fp.close(); }

  private:
    ifstream fp;
    const char *fname;
    const char *modname;
    bool trace;
    int period; 
}; 


template <class T>
bool stream_in<T>::read_from_raw_file(T& d) {
  if ( fp.good() ) {
    if ( fp >> d ) {
#ifdef TRACE_SYSC_IO
      cout << "stream_in::read_from_file: got " << d << " from file " << fname << " at t=" << sc_time_stamp() << endl;
#endif
      return true;
      }
    else 
      return false;
    }
  else 
    return false;
}

template <class T>
bool stream_in<T>::write_out(T d)
{
  if ( out->wr_rdy() ) {
    out->write(d);
    started->write(true);
    if ( trace ) cout << modname << " put " << d << " at " << sc_time_stamp() << endl; 
    return true;
  }
  else 
    return false;
  // TO FIX : this will not work is output channel is not ready ! 
}

template <class T>
void stream_in<T>::main() { 
    bool avail;
    T d;
    int i, j;
    bool ok;
    avail = read_from_raw_file(d);
    while ( avail ) {
        for ( int k=0; k<period; k++) wait();  // clk
        ok=write_out(d);
        avail = read_from_raw_file(d);
        }
} 
#endif
