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

#ifndef _port_in_h
#define _port_in_h

#include <systemc.h>
#include <list>
#include "io_event.h"
#include "sc_int_io.h"

template <class T>
SC_MODULE(port_in) { 
  sc_in<bool> clk; 
  sc_port<sc_signal_write_if<T> > out;  // this should be connected to a [buffer_in] channel

  void main();

  SC_HAS_PROCESS(port_in);
  
   port_in(sc_module_name name_, const char *fname_, const T ival_, bool trace_=false) :
     modname(name_), sc_module(name_), fname(fname_), trace(trace_), cur_val(ival_)
  {
    SC_THREAD(main);
    sensitive << clk.pos(); 
    if ( fname )
      read_event_file(name_, fname_, next_events);
    else
      next_events.clear();
    if ( trace ) {
      typedef typename std::list<io_event<T> >::iterator iterator;
      cout << name_ << ": read " << next_events.size() << " events ( ";
      for (iterator j = next_events.begin(); j != next_events.end(); ++j) cout << *j << ' '; 
      cout << ") from file " << fname << endl;
      }
   }

  ~port_in() { }

  protected:
    const char *fname;
    const char *modname;
    bool trace;

    T cur_val;
    ::std::list<io_event<T> > next_events;

    // Support methods
    int read_event_file(const char *modname, const char *fname, ::std::list<io_event<T> >& events);
    void write_out(T d);
}; 

template <class T>
int port_in<T>::read_event_file(const char *modname, const char *fname, ::std::list<io_event<T> >& r) {
  char tmp[128];
  ifstream fp;
  double t1, t2, t3;
  T v;
  fp.open(fname, ios::in);
  if ( !fp ) cerr << "*** " << modname << ": cannot open file " << fname << endl;
  fp >> std::boolalpha;
  while ( fp.good() && !fp.eof() ) {
    fp.getline(tmp,128);
    if ( fp.good() && tmp[0] != '#' ) {
      std::istringstream ss(tmp);
      ss >> t1 >> t2 >> t3 >> v;
      io_event<T> e(v,sc_time(t2,SC_NS));
#ifdef TRACE_SYSC_IO
      cout << "port_in::read_from_file: got " << e << " from file " << fname << endl;
#endif
      r.push_back(e);
      }
    }
  fp.close();
  return r.size();
}

template <class T>
void port_in<T>::write_out(T d)
{
    out->write(d);
    if ( trace ) cout << modname << " put " << d << " at " << sc_time_stamp() << endl; 
}

template <class T>
void port_in<T>::main() { 
    bool avail;
    sc_time next_change = next_events.empty() ? sc_time(-1,SC_NS) : next_events.front().date;
    while ( true ) {
        wait();  // clk
        if ( sc_time_stamp() == next_change ) {
          if ( trace )
            cout << modname << " changing value from " << cur_val << " to " << next_events.front().val 
                 <<  " at " << sc_time_stamp() << endl; 
          cur_val = next_events.front().val;
          next_events.pop_front();
          next_change = next_events.empty() ? sc_time(-1,SC_NS) : next_events.front().date;
          }
          write_out(cur_val);
          }
}
#endif
