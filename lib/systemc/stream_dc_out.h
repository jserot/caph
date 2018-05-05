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

#ifndef _stream_dc_out_h
#define _stream_dc_out_h

#include "fifo.h"
#include <systemc.h>
#include <fstream>
#include <iostream>
#include <string>

template <class T>
SC_MODULE(stream_out_dc) { 
  sc_in<bool> clk; 
  sc_port<fifo_in_if<T> > inp;
  sc_out<bool> started;
  sc_out<bool> stopped;

  void main(void);

  SC_HAS_PROCESS(stream_out_dc);
  
 stream_out_dc(sc_module_name name_, char *fnames_[], bool trace_=false, bool split_output_frames_=false, unsigned long idle_time_=0  ) :
  modname(name_), sc_module(name_), trace(trace_), split_output_frames(split_output_frames_),
  stop_when_idle(idle_time_), idle_time(idle_time_,SC_NS)
  {
    for ( int i=0; fnames_[i] != 0; i++ ) fnames.push_back(std::string(fnames_[i]));
    SC_THREAD(main);
    sensitive << clk.pos(); 
  }

  ~stream_out_dc() { }

  private:
    ofstream fp;
    std::vector<std::string> fnames;
    bool trace;
    bool split_output_frames;
    sc_module_name modname;
    bool stop_when_idle;
    sc_time idle_time;
}; 

template <class T>
void stream_out_dc<T>::main(void) {
  string of;
  int ind=0;
  T d, dd;
  bool _started = false;
  sc_time last_time;

  if ( trace ) cout << modname << " opening file " << fnames[ind] << endl; 
  fp.open(fnames[ind], ios::out);
  if ( !fp ) cerr << "** " << modname << ": cannot open file " << fnames[ind] << endl;
  fp << std::boolalpha;
  started.write(false);
  stopped.write(false);
  while ( ind < fnames.size() ) { 
      wait(); // clk
      if ( stop_when_idle && _started && sc_time_stamp() - last_time > idle_time )
        stopped.write(true);
      if ( inp->rd_rdy() ) {
        _started = true;
        started.write(true);
        last_time = sc_time_stamp();
        dd = d;
        d = inp->read();
        if ( trace ) cout << modname << " got " << d << " at " << sc_time_stamp() << endl; 
        fp << d << " ";
        if ( split_output_frames && d.repr.tag == T::EoS && dd.repr.tag == T::EoS ) {   // EoF
          if ( trace ) cout << modname << " closing file " << fnames[ind] << endl; 
          cout << "Wrote file " << fnames[ind] << endl;
          fp.close();
          ind++;
          if ( ind < fnames.size() ) {
            if ( trace ) cout << modname << " opening file " << fnames[ind] << endl; 
            fp.open(fnames[ind], ios::out);
            if ( !fp ) cerr << "** " << modname << ": cannot open file " << fnames[ind] << endl;
            fp << std::boolalpha;
            }
          }
        }
    }
}
#endif
