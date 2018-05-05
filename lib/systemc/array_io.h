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

#ifndef _array_io_h
#define _array_io_h

#include <fstream>
#include <iostream>
#include <array>

using namespace std;

template <class T, std::size_t N>
istream& operator >> (istream& is, std::array<T,N>& a) {
  char c;
  int i;
  is >> c;
  i = 0;
  c = is.peek();
  while ( is.good() && c != '}' ) {
    is >> a[i];
    if ( is.peek() == ',' ) {
      is >> c;
      }
    i++;;
    c = is.peek();
    }
  return is;
}

template <class T, std::size_t N>
ostream& operator<<(ostream &os, std::array<T,N> const& a)
{
  os << "{";
  if ( N > 0 ) {
    for ( int i=0; i<N-1; i++ ) os << a[i] << ",";
    if ( N>0 ) os << a[N-1];
    }
  os << "}";
  return os;
}

#endif
