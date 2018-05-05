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

// A special channel for connecting actors to (asynchronous) output ports, having
// - a fifo-like interface on the writer side
// - a signal-like interface on the reader side

#ifndef _BUFFER_OUT_H
#define _BUFFER_OUT_H
#include "systemc.h"

template<class T>
class buffer_out
: public fifo_out_if<T>,
  public sc_signal_in_if<T>,
  public sc_prim_channel
{
protected:
  const char *name;
  sc_signal<T> m_sig; 
  sc_event m_data_read_event;

public:
  explicit buffer_out(const char* name_) :
      name(name_),
      m_sig(name_),
	  m_data_read_event((std::string(SC_KERNEL_EVENT_PREFIX)+"_read_event").c_str())
      {}

  virtual ~buffer_out() { }

  // fifo-like interface
  virtual int num_free() const { return 1; }
  virtual void write( const T& v) {
#ifdef SYSC_TRACE_BUFFERS
    cout << name << " : write( " << v << ") at t=" << sc_time_stamp() << endl; cout.flush();
#endif
    m_sig.write(v); }
  virtual bool nb_write(const T& t) { m_sig.write(t); return true; };
  virtual bool wr_rdy() const { return true; }
  virtual const sc_event& data_read_event() const { return m_data_read_event; }

  // signal-conformant interface
  virtual const sc_event& value_changed_event() const { return m_sig.value_changed_event(); }
  virtual const T& read() const { /*m_data_read_event.notify(SC_ZERO_TIME);*/ return m_sig.read(); }
  virtual const T& get_data_ref() const { return m_sig.get_data_ref(); }
  virtual bool event() const { return m_sig.event(); }

  // other methods
  virtual const char* kind() const { return "buffer_out"; }
  buffer_out<T>& operator = ( const T& a ) { write(a); return *this; }
  void trace( sc_trace_file* tf ) const { sc_trace(tf, m_sig.read(), m_sig.name()); };
  // virtual void print( ::std::ostream& = ::std::cout ) const;
  // virtual void dump( ::std::ostream& = ::std::cout ) const;
};

#endif

/* template <class T> */
/* inline */
/* void */
/* buffer_out<T>::trace(sc_trace_file* tf) const */
/* { */
/*     std::string nm = name(); */
/*     sc_trace( tf, m_sig.read(), nm); */
/* } */

/* template <class T> */
/* inline */
/* void */
/* buffer_out<T>::dump( ::std::ostream& os ) const */
/* { */
/*   os << "BUFFER_OUT " << name() << " @ t=" << sc_time_stamp() << " : " << " = " << m_sig.read() << endl; */
/* } */
