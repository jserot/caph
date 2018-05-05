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

// A special channel for connecting (asynchronous) input ports to actors, having
// - a signal-like interface on the writter side
// - a fifo-like interface on the reader side

#ifndef _BUFFER_IN_H
#define _BUFFER_IN_H
#include "systemc.h"

template<class T>
class buffer_in
: public sc_signal_write_if<T>,
  public fifo_in_if<T>,
  public sc_prim_channel
{
public:
  sc_signal<T> m_sig; 
  sc_event m_data_written_event;
  const char* name;
public:
  explicit buffer_in( const char* name_ ) :
      name(name_),
      m_sig(name_),
	  m_data_written_event((std::string(SC_KERNEL_EVENT_PREFIX)+"_write_event").c_str()) { }

  virtual ~buffer_in() { }

  // (read-only) fifo-conformant interface
  virtual int num_available() const { return 1; }
  virtual void read( T& t) { t = m_sig.read(); }
  virtual T read() { return  m_sig.read(); }
  virtual bool nb_read( T& t) { t = m_sig.read(); return true; }
  virtual const sc_event& data_written_event() const { return m_data_written_event; }
  // virtual bool nb_write( const T& t){return fifo.nb_write(t);};
  // virtual const sc_event& data_read_event() const{return fifo.data_read_event();};
  virtual T peek() const { return m_sig.read(); }
  virtual bool rd_rdy() const { return 1; }

  // (write-only) signal-conformant interface
  virtual void write( const T& v) {
#ifdef SYSC_TRACE_BUFFERS
    cout << name << " : write( " << v << ") at t=" << sc_time_stamp() << endl; cout.flush();
#endif
    m_sig.write(v);
    m_data_written_event.notify(SC_ZERO_TIME); };

  // other methods
  virtual const char* kind() const { return "buffer_in"; }
  operator T () { return read(); }
  // buffer_in<T>& operator = ( const T& a ) { write( a ); return *this; }
  void trace( sc_trace_file* tf ) const { sc_trace( tf, m_sig.read(), m_sig.name()); }
  // virtual void print( ::std::ostream& = ::std::cout ) const;
};

template<class T>
void sc_trace(sc_trace_file *tf, const buffer_in<T> &b, const std::string &s)
{
   sc_trace( tf, b.m_sig, s);
}
#endif
