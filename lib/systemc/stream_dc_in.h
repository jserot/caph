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

#ifndef _stream_in_dc_h
#define _stream_in_dc_h

#include "fifo.h"
#include <systemc.h>
#include <fstream>

template <class T>
SC_MODULE(stream_in_dc) { 
  sc_in<bool> clk; 
  sc_port<fifo_out_if<T> > out;
  sc_out<bool> started;

  bool read_from_raw_file(T& d);
  bool write_out(T d);

  void main();

  SC_HAS_PROCESS(stream_in_dc);
  
 stream_in_dc(sc_module_name name_, char *fnames_[], bool trace_=false, int period_=1, int h_blank_=0, int v_blank_=0) :
  modname(name_), sc_module(name_), trace(trace_), period(period_), h_blank(h_blank_), v_blank(v_blank_)
  {
    for ( int i=0; fnames_[i] != 0; i++ ) fnames.push_back(std::string(fnames_[i]));
    if ( period < 1 ) cerr << "** error: " << name_ << " : stream_in period < 1 ! " << endl;
    if ( h_blank < 0 ) cerr << "** error: " << name_ << " : horizontal blanking < 0 ! " << endl;
    if ( v_blank < 0 ) cerr << "** error: " << name_ << " : vertical blanking < 0 ! " << endl;
    SC_THREAD(main);
    sensitive << clk.pos(); 
  }

  ~stream_in_dc() { }

  private:
    ifstream fp;
    const char *fpat;
    std::vector<std::string> fnames;
    const char *modname;
    bool trace;
    int period; 
    int h_blank; 
    int v_blank; 
    void without_blanking();
    void with_blanking();
}; 


template <class T>
bool stream_in_dc<T>::read_from_raw_file(T& d) {
  if ( fp.good() ) {
    if ( fp >> d ) {
#ifdef TRACE_SYSC_IO
      cout << "stream_in_dc::read_from_file: got " << d << " from file " << fname << " at t=" << sc_time_stamp() << endl;
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
bool stream_in_dc<T>::write_out(T d)
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
void stream_in_dc<T>::without_blanking() { 
    bool avail;
    T d;
    bool ok;
    std::string fname;

    for ( int i=0; i<fnames.size(); i++ ) {
      fname = fnames[i];
      if ( trace ) cout << modname << " opening file " << fname << endl << flush; 
      fp.open(fname, ios::in);
      if ( !fp ) cerr << "*** " << modname << ": cannot open file " << fname << endl;
      fp >> std::boolalpha;
      avail = read_from_raw_file(d);
      while ( avail ) {
        for ( int k=0; k<period; k++) wait();  // clk
        ok=write_out(d);
        avail = read_from_raw_file(d);
        }
      if ( trace ) cout << modname << " closing file " << fname << endl << flush; 
      fp.close();
    }
} 

template <class T>
void stream_in_dc<T>::with_blanking() { 
    bool avail;
    T d;
    bool ok;
    enum { S0, S1, S2 } state;
    std::string fname;
    for ( int i=0; i<fnames.size(); i++ ) {
      fname = fnames[i];
      if ( trace ) cout << modname << " opening file " << fname << endl << flush; 
      fp.open(fname, ios::in);
      if ( !fp ) cerr << "*** " << modname << ": cannot open file " << fname << endl;
      fp >> std::boolalpha;
      state = S0;
      avail = read_from_raw_file(d);
      while ( avail ) {
        for ( int k=0; k<period; k++) wait();  // clk
        ok=write_out(d);
        switch ( state ) {
        case S0:
          state=S1;
          break;
        case S1:
          if ( d.repr.tag == T::EoS ) { // EoF
            if ( v_blank > 0 ) {
              if ( trace )
                cout << modname << " waiting " << v_blank << " clock cycle(s) for vertical blanking" << endl << flush; 
              for ( int j=0; j<v_blank + v_blank; j++) wait();
              }
            state=S0; 
            }
          else                          // SoL
            state=S2; 
        case S2:
          if ( d.repr.tag == T::EoS ) { // EoL
            if ( h_blank > 0 ) {
              if ( trace ) 
                cout << modname << " waiting " << h_blank << " clock cycle(s) for horizontal blanking" << endl << flush; 
              for ( int j=0; j<h_blank; j++) wait();
              }
            state=S1;  
            }
          }
        avail = read_from_raw_file(d);
        }
      if ( trace ) cout << modname << " closing file " << fname << endl << flush; 
      fp.close();
    }
} 

template <class T>
void stream_in_dc<T>::main() { 
  started.write(false);
  if ( h_blank > 0 || v_blank > 0 ) 
    with_blanking();
  else
    without_blanking();
} 
#endif
