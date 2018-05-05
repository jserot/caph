#include <systemc.h>

sc_int<8> mult(sc_int<8> x, sc_int<8> y) 
{
  sc_int<8> res = x*y;
  cout << "** mult(" << x << "," << y << ") -> " << res << endl;
  return res;
}

sc_int<8> incr(sc_int<8> x) 
{
  sc_int<8> res = x+1;
  cout << "** incr(" << x << ") -> " << res << endl;
  return res;
}
