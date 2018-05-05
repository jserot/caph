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

#ifndef _split_h
#define _split_h

#include "fifo.h"
#include <systemc.h>

template <class T>
SC_MODULE(split2) { 
  sc_in<bool> clk; 
  sc_port<fifo_in_if<T> > i;
  sc_port<fifo_out_if<T> > o_1;
  sc_port<fifo_out_if<T> > o_2;

  void main(void);

  SC_HAS_PROCESS(split2);
  
 split2(sc_module_name name_, bool trace_=false  ) :
  modname(name_), sc_module(name_), trace(trace_)
  {
    SC_THREAD(main);
    sensitive << clk.pos(); 
  }

  ~split2() { }

  private:
    char *fname;
    bool trace;
    sc_module_name modname;
}; 

template <class T>
void split2<T>::main(void) {
    T d;
    while(1) { 
      wait(); // clk
      if ( i->rd_rdy() && o_1->wr_rdy() && o_2->wr_rdy() ) {
          d = i->read();
          if ( trace ) cout << modname << " read " << d << " at " << sc_time_stamp() << endl; 
          o_1->write(d);
          o_2->write(d);
          if ( trace ) cout << modname << " wrote " << d << " at " << sc_time_stamp() << endl; 
          }
      }
}

template <class T>
SC_MODULE(split3) { 
  sc_in<bool> clk; 
  sc_port<fifo_in_if<T> > i;
  sc_port<fifo_out_if<T> > o_1;
  sc_port<fifo_out_if<T> > o_2;
  sc_port<fifo_out_if<T> > o_3;

  void main(void);

  SC_HAS_PROCESS(split3);
  
 split3(sc_module_name name_, bool trace_=false  ) :
  modname(name_), sc_module(name_), trace(trace_)
  {
    SC_THREAD(main);
    sensitive << clk.pos(); 
  }

  ~split3() { }

  private:
    char *fname;
    bool trace;
    sc_module_name modname;
}; 

template <class T>
void split3<T>::main(void) {
    T d;
    while(1) { 
      wait(); // clk
      if ( i->rd_rdy() && o_1->wr_rdy() && o_2->wr_rdy() && o_3->wr_rdy() ) {
          d = i->read();
          if ( trace ) cout << modname << " read " << d << " at " << sc_time_stamp() << endl; 
          o_1->write(d);
          o_2->write(d);
          o_3->write(d);
          if ( trace ) cout << modname << " wrote " << d << " at " << sc_time_stamp() << endl; 
          }
      }
}

template <class T>
SC_MODULE(split4) { 
  sc_in<bool> clk; 
  sc_port<fifo_in_if<T> > i;
  sc_port<fifo_out_if<T> > o_1;
  sc_port<fifo_out_if<T> > o_2;
  sc_port<fifo_out_if<T> > o_3;
  sc_port<fifo_out_if<T> > o_4;

  void main(void);

  SC_HAS_PROCESS(split4);
  
 split4(sc_module_name name_, bool trace_=false  ) :
  modname(name_), sc_module(name_), trace(trace_)
  {
    SC_THREAD(main);
    sensitive << clk.pos(); 
  }

  ~split4() { }

  private:
    char *fname;
    bool trace;
    sc_module_name modname;
}; 

template <class T>
void split4<T>::main(void) {
    T d;
    while(1) { 
      wait(); // clk
      if ( i->rd_rdy() && o_1->wr_rdy() && o_2->wr_rdy() && o_3->wr_rdy() && o_4->wr_rdy() ) {
          d = i->read();
          if ( trace ) cout << modname << " read " << d << " at " << sc_time_stamp() << endl; 
          o_1->write(d);
          o_2->write(d);
          o_3->write(d);
          o_4->write(d);
          if ( trace ) cout << modname << " wrote " << d << " at " << sc_time_stamp() << endl; 
          }
      }
}

template <class T>
SC_MODULE(split5) { 
  sc_in<bool> clk; 
  sc_port<fifo_in_if<T> > i;
  sc_port<fifo_out_if<T> > o_1;
  sc_port<fifo_out_if<T> > o_2;
  sc_port<fifo_out_if<T> > o_3;
  sc_port<fifo_out_if<T> > o_4;
  sc_port<fifo_out_if<T> > o_5;

  void main(void);

  SC_HAS_PROCESS(split5);
  
 split5(sc_module_name name_, bool trace_=false  ) :
  modname(name_), sc_module(name_), trace(trace_)
  {
    SC_THREAD(main);
    sensitive << clk.pos(); 
  }

  ~split5() { }

  private:
    char *fname;
    bool trace;
    sc_module_name modname;
}; 

template <class T>
void split5<T>::main(void) {
    T d;
    while(1) { 
      wait(); // clk
      if ( i->rd_rdy() && o_1->wr_rdy() && o_2->wr_rdy() && o_3->wr_rdy() && o_4->wr_rdy() && o_5->wr_rdy() ) {
          d = i->read();
          if ( trace ) cout << modname << " read " << d << " at " << sc_time_stamp() << endl; 
          o_1->write(d);
          o_2->write(d);
          o_3->write(d);
          o_4->write(d);
          o_5->write(d);
          if ( trace ) cout << modname << " wrote " << d << " at " << sc_time_stamp() << endl; 
          }
      }
}


template <class T>
SC_MODULE(split6) { 
  sc_in<bool> clk; 
  sc_port<fifo_in_if<T> > i;
  sc_port<fifo_out_if<T> > o_1;
  sc_port<fifo_out_if<T> > o_2;
  sc_port<fifo_out_if<T> > o_3;
  sc_port<fifo_out_if<T> > o_4;
  sc_port<fifo_out_if<T> > o_5;
  sc_port<fifo_out_if<T> > o_6;

  void main(void);

  SC_HAS_PROCESS(split6);
  
 split6(sc_module_name name_, bool trace_=false  ) :
  modname(name_), sc_module(name_), trace(trace_)
  {
    SC_THREAD(main);
    sensitive << clk.pos(); 
  }

  ~split6() { }

  private:
    char *fname;
    bool trace;
    sc_module_name modname;
}; 

template <class T>
void split6<T>::main(void) {
    T d;
    while(1) { 
      wait(); // clk
      if ( i->rd_rdy() && o_1->wr_rdy() && o_2->wr_rdy() && o_3->wr_rdy() && o_4->wr_rdy() && o_5->wr_rdy() && o_6->wr_rdy() ) {
          d = i->read();
          if ( trace ) cout << modname << " read " << d << " at " << sc_time_stamp() << endl; 
          o_1->write(d);
          o_2->write(d);
          o_3->write(d);
          o_4->write(d);
          o_5->write(d);
          o_6->write(d);
          if ( trace ) cout << modname << " wrote " << d << " at " << sc_time_stamp() << endl; 
          }
      }
}
#endif
