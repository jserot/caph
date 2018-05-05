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

#ifndef FIFO_IFS_H
#define FIFO_IFS_H


#include "sysc/communication/sc_interface.h"

template <class T> 
class fifo_nonblocking_in_if 
: virtual public sc_interface 
{ 
public: 

    // non-blocking read 
    virtual bool nb_read( T& ) = 0; 

    // get the data written event 
    virtual const sc_event& data_written_event() const = 0; 
}; 

template <class T> 
class fifo_blocking_in_if 
: virtual public sc_interface 
{ 
public: 

    // blocking read 
    virtual void read( T& ) = 0; 
    virtual T read() = 0; 
}; 

// ----------------------------------------------------------------------------
//  CLASS : fifo_in_if<T>
//
//  The fifo<T> input interface class.
// ----------------------------------------------------------------------------

template <class T> 
class fifo_in_if 
: public fifo_nonblocking_in_if<T>, 
  public fifo_blocking_in_if<T> 
{ 
public: 

    // get the number of available samples 
    virtual int num_available() const = 0; 

    virtual T peek() const = 0; 
    virtual bool rd_rdy() const = 0; 

protected: 

    // constructor 

    fifo_in_if() 
        {} 

private: 

    // disabled 
    fifo_in_if( const fifo_in_if<T>& ); 
    fifo_in_if<T>& operator = ( const fifo_in_if<T>& ); 
}; 


// ----------------------------------------------------------------------------
//  CLASS : fifo_nonblocking_out_if<T>
//
//  The fifo<T> nonblocking output interface class.
// ----------------------------------------------------------------------------

template <class T> 
class fifo_nonblocking_out_if 
: virtual public sc_interface 
{ 
public: 

    // non-blocking write 
    virtual bool nb_write( const T& ) = 0; 

    // get the data read event 
    virtual const sc_event& data_read_event() const = 0; 
}; 

// ----------------------------------------------------------------------------
//  CLASS : fifo_blocking_out_if<T>
//
//  The fifo<T> blocking output interface class.
// ----------------------------------------------------------------------------

template <class T> 
class fifo_blocking_out_if 
: virtual public sc_interface 
{ 
public: 

    // blocking write 
    virtual void write( const T& ) = 0; 

}; 

// ----------------------------------------------------------------------------
//  CLASS : fifo_out_if<T>
//
//  The fifo<T> output interface class.
// ----------------------------------------------------------------------------

template <class T> 
class fifo_out_if 
: public fifo_nonblocking_out_if<T>, 
  public fifo_blocking_out_if<T> 
{ 
public: 

    // get the number of free spaces 
    virtual int num_free() const = 0; 
    // Added, JS, Sep 24 2010
    virtual bool wr_rdy() const = 0; 

protected: 

    // constructor 

    fifo_out_if() 
        {} 

private: 

    // disabled 
    fifo_out_if( const fifo_out_if<T>& ); 
    fifo_out_if<T>& operator = ( const fifo_out_if<T>& ); 
}; 

#endif
