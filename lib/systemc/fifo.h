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

#ifndef _FIFO_H
#define _FIFO_H


#include "sysc/communication/sc_communication_ids.h"
#include "sysc/communication/sc_prim_channel.h"
#include "fifo_ifs.h"
#include "array_io.h"
#include "sysc/kernel/sc_event.h"
#include "sysc/kernel/sc_simcontext.h"
#include "sysc/utils/sc_string.h"
#include "sysc/tracing/sc_trace.h"
#include <typeinfo>

template <class T>
class fifo
: public fifo_in_if<T>,
  public fifo_out_if<T>,
  public sc_prim_channel
{
public:

    // constructors

    explicit fifo( int size_ = 16 )
	: sc_prim_channel( sc_gen_unique_name( "fifo" ) )
	{ init( size_ ); }

    explicit fifo( const char* name_, int size_ = 16, bool traced_ = false, bool statd_ = false )
	  : sc_prim_channel( name_ )
    { init( size_, statd_, traced_ ); }

    // destructor

    virtual ~fifo() { delete [] m_buf; }

    // interface methods

    virtual void register_port( sc_port_base&, const char* );

    // blocking read
    virtual void read( T& );
    virtual T read();

    // non-blocking read
    virtual bool nb_read( T& );

    virtual T peek() const { return ( m_buf[m_ri] ); }

    // get the number of available samples

    virtual int num_available() const { return ( m_num_readable - m_num_read ); }

    virtual bool rd_rdy() const { return num_available() > 0; }

    // get the data written event

    virtual const sc_event& data_written_event() const { return m_data_written_event; }

    // blocking write
    virtual void write( const T& );

    // non-blocking write
    virtual bool nb_write( const T& );

    // get the number of free spaces

    virtual int num_free() const { return ( m_size - m_num_readable - m_num_written ); }

    virtual bool wr_rdy() const { return num_free() > 0; }

    // get the data read event

    virtual const sc_event& data_read_event() const { return m_data_read_event; }

    // other methods

    operator T () { return read(); }

    fifo<T>& operator = ( const T& a ) { write( a ); return *this; }

    void trace( sc_trace_file* tf ) const;
    virtual void dump( ::std::ostream& = ::std::cout ) const;
    virtual void dump_stat( ::std::ostream& = ::std::cout, int offset=0 ) const;
/*     virtual void print_head( ::std::ostream& = ::std::cout ) const; */

    virtual const char* kind() const { return "fifo"; }

protected:

    virtual void update();


    // support methods

    void init(int, bool, bool);

    void buf_init( int );
    bool buf_write( const T& );
    bool buf_read( T& );

    virtual void update_stats();

protected:

    int m_size;			// size of the buffer
    T*  m_buf;			// the buffer
    int m_free;			// number of free spaces
    int m_ri;			// index of next read
    int m_wi;			// index of next write
    bool statd;          
    bool traced;
    struct { int max_occ; } m_stat;

    sc_port_base* m_reader;	// used for static design rule checking
    sc_port_base* m_writer;	// used for static design rule checking

    int m_num_readable;		// #samples readable
    int m_num_read;		// #samples read during this delta cycle
    int m_num_written;		// #samples written during this delta cycle

    sc_event m_data_read_event;
    sc_event m_data_written_event;

private:

    // disabled
    fifo( const fifo<T>& );
    fifo& operator = ( const fifo<T>& );
};


template <class T>
inline
void
fifo<T>::register_port( sc_port_base& port_, const char* if_typename_ )
{
    std::string nm( if_typename_ );
    if( nm == typeid( fifo_in_if<T> ).name() ) {
	// only one reader can be connected
	if( m_reader != 0 ) {
	    SC_REPORT_ERROR( SC_ID_MORE_THAN_ONE_FIFO_READER_, 0 );
	}
	m_reader = &port_;
    } else {  // nm == typeid( fifo_out_if<T> ).name()
	// only one writer can be connected
	if( m_writer != 0 ) {
	    SC_REPORT_ERROR( SC_ID_MORE_THAN_ONE_FIFO_WRITER_, 0 );
	}
	m_writer = &port_;
    }
}

// blocking read

template <class T>
inline
void
fifo<T>::read( T& val_ )
{
    while( num_available() == 0 ) {
	sc_core::wait( m_data_written_event );
    }
    m_num_read ++;
    buf_read( val_ );
//    if ( trace ) do_trace();
//    if ( stat ) do_stat();
    request_update();
}

template <class T>
inline
T
fifo<T>::read()
{
    T tmp;
    read( tmp );
    return tmp;
}

// non-blocking read

template <class T>
inline
bool
fifo<T>::nb_read( T& val_ )
{
    if( num_available() == 0 ) {
	return false;
    }
    m_num_read ++;
    buf_read( val_ );
    //    if ( trace ) do_trace();
    //    if ( stat ) do_stat();
    request_update();
    return true;
}

// blocking write

template <class T>
inline
void
fifo<T>::write( const T& val_ )
{
    while( num_free() == 0 ) {
	sc_core::wait( m_data_read_event );
    }
    m_num_written ++;
    buf_write( val_ );
//    if ( trace ) do_trace();
//    if ( stat ) do_stat();
    request_update();
}

// non-blocking write

template <class T>
inline
bool
fifo<T>::nb_write( const T& val_ )
{
    if( num_free() == 0 ) {
	return false;
    }
    m_num_written ++;
    buf_write( val_ );
//    if ( trace ) do_trace();
//    if ( stat ) do_stat();
    request_update();
    return true;
}

// Tracing/printing 

template <class T>
inline
void
fifo<T>::trace( sc_trace_file* tf ) const
{
    std::string nm = name();
    sc_trace( tf, m_num_readable, nm+".nr");
#ifndef IO_ARRAY_BUG
    sc_trace( tf, m_buf[m_ri], nm+".hd");
#endif
/*       sc_trace( tf, m_ri, nm + ".ri" ); */
/*       sc_trace( tf, m_wi, nm + ".wi" ); */
}

/* template <class T> */
/* inline */
/* void */
/* fifo<T>::print_head( ::std::ostream& os ) const */
/* { */
/*   if( m_num_readable > 0 ) */
/*     os << m_buf[m_ri]; */
/*   else */
/*     os << "[]"; */
/* } */

template <class T>
inline
void
fifo<T>::dump( ::std::ostream& os ) const
{
  os << "FIFO " << name() << " @ t=" << sc_time_stamp() << " : " << " = (" << num_available() << ")[";
  if( m_free != m_size ) {
    int i = m_ri;
    int j = 0;
    do {
#ifndef IO_ARRAY_BUG
      os << m_buf[i] << " ";
#endif
      i = ( i + 1 ) % m_size;
      j ++;
      } while( i != m_wi );
    }
    os << "]" << ::std::endl;
}

template <class T>
inline
void
fifo<T>::dump_stat( ::std::ostream& os, int offset ) const
{
  os << name() << " fifo_size = " << m_stat.max_occ + offset << ::std::endl;
}

// support methods

template <class T>
inline
void
fifo<T>::update()
{
    if( m_num_read > 0 ) {
	m_data_read_event.notify(SC_ZERO_TIME);
    }

    if( m_num_written > 0 ) {
	m_data_written_event.notify(SC_ZERO_TIME);
    }

    m_num_readable = m_size - m_free;
    m_num_read = 0;
    m_num_written = 0;

    if ( traced ) dump();
    if ( statd ) update_stats();
}

template <class T>
inline
void
fifo<T>::update_stats()
{
    if( m_num_readable > m_stat.max_occ ) m_stat.max_occ = m_num_readable;
}

template <class T>
inline
void
fifo<T>::init( int size_, bool statd_, bool traced_ )
{
    buf_init( size_ );
    m_reader = 0;
    m_writer = 0;
    m_num_readable = 0;
    m_num_read = 0;
    m_num_written = 0;
    statd = statd_;
    traced = traced_;
    m_stat.max_occ = 0;
}

template <class T>
inline
void
fifo<T>::buf_init( int size_ )
{
    if( size_ <= 0 ) {
	SC_REPORT_ERROR( SC_ID_INVALID_FIFO_SIZE_, 0 );
    }
    m_size = size_;
    m_buf = new T[m_size];
    m_free = m_size;
    m_ri = 0;
    m_wi = 0;
}

template <class T>
inline
bool
fifo<T>::buf_write( const T& val_ )
{
    if( m_free == 0 ) {
	return false;
    }
    m_buf[m_wi] = val_;
    m_wi = ( m_wi + 1 ) % m_size;
    m_free --;
    return true;
}

template <class T>
inline
bool
fifo<T>::buf_read( T& val_ )
{
    if( m_free == m_size ) {
	return false;
    }
    val_ = m_buf[m_ri];
    m_ri = ( m_ri + 1 ) % m_size;
    m_free ++;
    return true;
}


// ----------------------------------------------------------------------------

template <class T>
inline
::std::ostream&
operator << ( ::std::ostream& os, const fifo<T>& a )
{
    a.print_head( os );
    return os;
}

#endif
