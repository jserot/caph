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

#ifndef _port_out_h
#define _port_out_h

#include <systemc.h>

template <class T>
SC_MODULE(port_out) { 
  sc_in<bool> clk; 
  //sc_port<sc_signal_in_if<T> > inp;
  sc_in<T > inp;

  void main(void);

  SC_HAS_PROCESS(port_out);
  
   port_out(sc_module_name name_, const char *fname_, bool trace_=false) :
    modname(name_), sc_module(name_), fname(fname_), trace(trace_)
   {
      SC_THREAD(main);
      sensitive << clk.pos(); 
      fp.open(fname_, ios::out);
      if ( !fp ) cerr << "** " << name_ << ": cannot open file " << fname_ << endl;
      fp << std::boolalpha;
   }

    ~port_out() { fp.close(); cout << "Wrote file " << fname << endl; cout.flush();  }

  private:
    sc_module_name modname;
    const char *fname;
    ofstream fp;
    bool trace;
}; 

template <class T>
void port_out<T>::main(void) {
    T d;
    while(1) { 
      wait(inp.value_changed_event());
      d = inp.read();
      if ( trace ) cout << modname << " got " << d << " at " << sc_time_stamp() << endl; 
      fp << d << " @ " << sc_time_stamp() << endl;
      }
}
#endif
