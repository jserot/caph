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

#ifndef _io_event_h
#define _io_event_h
#include <systemc.h>

template <class T>
class io_event {
public:
  io_event(void) { date = SC_ZERO_TIME; };
  io_event(T val_, sc_time date_) : val(val_), date(date_) { }
  ~io_event() { };
  inline friend ::std::ostream& operator << ( ::std::ostream& os, const io_event<T>& e) {
    os << "<t=" << e.date << ",v=" << e.val << ">";
    return os;
    }
  inline friend ::std::istream& operator >> ( ::std::istream& is, io_event<T>& e) {
    char tmp[64];
    is.getline(tmp,64);
    if ( is.good() ) {
      std::istringstream ss(tmp);
      ss >> e.val;
      ss >> e.date;
      }
    return is;
    }
public:
  T val;
  sc_time date;
};

template <class T>
inline void sc_trace(sc_trace_file *tf, const io_event<T >& e, const std::string& n) { sc_trace(tf, "<io_event>", n);} // TO FIX
#endif
