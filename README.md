CAPH 
====

**CAPH** is a domain-specific language for describing and implementing stream-processing applications on
reconfigurable hardware, such as FPGAs. **CAPH** generates VHDL code from high-level descriptions of
signal or image processing applications. **CAPH** relies upon the actor/dataflow model of
computation. Applications are described as networks of purely dataflow actors exchanging tokens
through unidirectional channels and the behavior of each actor is defined as a set of transition
rules using pattern matching.

**CAPH** comes with :

* a _higher-order, purely functional language_ for description of complex dataflow networks
* a _rich type system_ with sized-integers, booleans, floats, fully polymorphic _algebraic data
  types_ and _dependent types_
* automatic _type inference_ and type-checking
* _higher-order actors_ (actors taking functions as parameters)
* _pattern-matching_ based description of actor behavior
* _graphical visualisation_ of dataflow networks
* _code simulation_ with trace facilities
* _SystemC_ back-end for simulation
* _VHDL_ backend, generating  _target-independant_, _ready-to-synthetize_, time and space-efficient VHDL code
* a _foreign-function interface_ (FFI) to use existing SystemC or VHDL code

DOCUMENTATION
-------------

[Project web page](http://caph.univ-bpclermont.fr)

[Tutorial](http://caph.univ-bpclermont.fr/dist/caph-primer.pdf) and [Reference
manual](http://caph.univ-bpclermont.fr/dist/caph-lrm.pdf).

INSTALLATION
------------

##### Using pre-built binaries

Installers for MacOS and Windows are available from the [Project web page](http://caph.univ-bpclermont.fr).

##### From source

Download the source tree from the [Project web page](http://caph.univ-bpclermont.fr) or via
[github](http://github.com/jserot/caph) (`git clone https://github.com/jserot/caph`).

From the root of the source tree :

1. `./configure [options]`  (`./configure --help` for the list of options)
2. `make`
3. `make install`
