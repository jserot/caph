## 2.9.1 (May XX, 2019)

####  LANGUAGE
- support for input or output-less actors (using the `unit` type) (see for ex. `examples/misc/ioless`)


## 2.9.0 (Nov 12, 2018)

#### VHDL
- added option `-vhdl_tb_inline_io` and `vhdl_tb_external_clock`. The former forces the generated
  testbench to use inlined data instead of reading/writing `.bin` files. The latter generates an
  "open" testbench, taking `clock` and `reset` signals as input.
- added variants `cstream_in` and `cstream_out` to `lib/vhdl` to support inlined data in testbenches
- removed `fifo_big.vhd` from `lib/vhdl` because the synthesis of this model as a RAM-based
  component was Quartus-specific. All platform-specific code will be now hosted on 
  the separate repository [caph-contrib](https://github.com/jserot/caph-contrib). More details on 
  this [here](https://github.com/jserot/caph/issues/1).

#### COMPILER
- removed option `-vhdl_quartus` for the reasons listed above
  
#### EXAMPLES
- corrected name of the VHDL annotation file in several examples .proj files

#### DOC
- fixed several typos in the Primer document

#### DISTRIB
- fixed a bug in `man/Makefile` which prevented building the man pages
  from sources on platforms for which the `sed` command did not accept the `-E` extension
- updated the `configure` to automatically detect which version of `sed` to use

## 2.8.5 (May 5, 2018)

#### SYSTEMC backend
- added option `-sc_io_monitor` for dumping start/stop times of IO boxes 
- added option `-sc_fifo_stats_file` to specify the file where to dump FIFO stats when option `-sc_dump_fifo_stats` is asserted (default: "fifo_stats.dat")

#### XDF
- bug fix in `xdf.ml` (`</Connection>` anchor missing) (Thx to Claudio R.)
- added option `-xdf_package` to prefix actor class declarations in the generated `.xdf` file
- if a file "fifo_stats.dat" is present, reads it and generates corresponding annotation in the `.xdf` file
- actor instances are now named `xxx_act` in the `.cal` and `.xdf` files

#### DOCUMENTATION
- fixed some typos
- updated sec 10.9 to reflect changes on the `-sc_dump_fifo_stats` option

#### EXAMPLES
- fixed references to undefined macro MAIN in some project files

#### DISTRIBUTION
- added options `--no-gui`, `--no-doc`, `--qmake` and `--qmake-spec` to `configure`
- reorganized source tree and Makefiles
- now also on [Github](http://github.com/jserot/caph) !

___
## 2.8.4 (Nov 23, 2017)

####  LANGUAGE
- added a ranged integer type; ex : `var s : {0,..,7}]`; local variables with such a type will be identified as potential state variables when dumping box FSMs

####  COMPILER
- added option `-dump_boxes` as a restricted, less verbose alternative to `-dump_senv`
- the `-dump_fsms` options now operates on boxes and not on actors
- added option `-absint` (and `-ai_max_cycles`) to perform abstract interpretation of boxes
- added option `-infer_mocs` to classify boxes according to their actual model of computation (SDF/CSDF/DDF); classification is performed on the basis of both a static analysis of the rule set and abstract interpretation. Results are written to file `xxx_mocs.dat` in `.csv` format.
- added option `-dump_sdf_fifo_sizes` to generate an annotation file containing predicted FIFO sizes from the statically computed rule signatures. For now this only works if _all_ actors can be categorized as SDF.
- removed option `-sc_fifo_stats_offset`; the SystemC backend now produces the true max occupation of each FIFO
- added option `-vhdl_fifo_offset` 
- fixed a bug in the type checker affecting rule pattern variables with int types (thx to C. Rubattu for spotting it)
- added an option `-dif` to export the generated dataflow graphs using the DIF format

####  CAL backend 
- the type `int<n>` is now translated to "int"

####  DOCUMENTATION
- added a chapter in the LRM describing the model of computation (MoC) underpinning Caph programs
- added a section in chapter "Using the compiler" of the LRM on FSM generation

####  LIBRARY
- added actor `sample` to `stream _ops.cph`

####  EXAMPLES
- added a subdirectory `moc` containing examples illustrating the use of MoC-based features
- added an example in `examples/working/cal`showing how to reformulate an actor operating on `int dc` type into an actor operating on raw `int`s

####  DISTRIBUTION
- fixed a bug in the Windows distributed file which incorrectely made it appear as compressed 

___
## 2.8.3 (Jun 2, 2017)

####  DISTRIBUTION
- a binary distribution is now provided for Mac OS platforms as a .DMG package
- the Windows version is now provided as a self-installer (thx to F. Berry for his suggestion and help)

####  GUI
- the GUI frontend has been renamed `Caph`
- subwindows are now resizeable
- the font used for displaying source and result code can be changed using the `Configuration` menu
- the forms for adjusting compiler options have been redesigned

####  COMPILER
- the command line compiler command has been renamed `caphc`

####  EXAMPLES
- all examples now comes with a .cphpro project description file to be run directly from the Caph GUI.
      Testing CAPH is now as simple as
      1. Install 
      2. Launch CAPH
      3. Select project in the example directory
      4. Click on Dot / Sim / Systemc / Vhdl button(s)  !

####  Doc
- a separate Install Guide is provided showing how to use the self-installer under Windows
- the "Caph Primer" tutorial has been completely rewritten; it now includes three distinct and complementary parts : 
      one dedicated to the language itself, one describing the GUI and one describing the command-line 
      interface to the toolset. The section describing the command-line interface has been rewritten to
      show the use of the `caphmake` utility.

___
## 2.8.2 (Apr 26, 2017)

####  COMPILER
- a few minor bug fixes 

####  TOOLS
- `caphy` GUI now included in the distribution (in source form for Unix-like distribution, as a pre-built binary for Windows) 

___
## 2.8.1 (Feb 10, 2017)

####  COMPILER
- XDF/CAL output format for networks (option `-xdf`)
- fixed a bug in the Win32 distribution which prevented the generation of SystemC and VHDL makefiles
- the options `-sc_target_dir` and `-vhdl_target_dir` are now superceded by a general `-target_dir` option
- the syntax for file globbing has been changed and extended to outputs files.
      See Sec 9.5.2 of the LRM and `examples/working/io/fglob[12]`

####  ETC
- added a `xdf` target to `Makefile.core`
- the file `lib/etc/Makefile.core` now only contains platform-specific definitions

####  EXAMPLES
- the source code of the examples  described in the _Caph Primer_ tutorial are now included in 
      the distribution (under `examples/working/primer`)

####  SYSTEMC 
- fixed a bug in the the SystemC library which could cause the last token of a input file to be
      discarded (thx to F. Palumbo for spotting this one !)

####  TOOLS
- `caphmake` utility to automatically generate Makefiles from `.defs` and `.rules` project descriptions
      See section 9.10 of the LRM.
- `pgm2bin` and `bin2pgm` utilities to directly generate `.bin` input files from `.pgm` images (and vice versa)

####  DOC
- updated section 9.10 of the LRM concerning the use of Makefiles (now promoting the use of `caphmake`)
- updated section 9.5.3 and 9.5.4 of the LRM concerning the use of file format converters
- some updates in the _Caph Primer_ tutorial 

####  DISTRIBUTION
- the distribution now only comes in two flavors : a source distribution for Unix-like systems (Unix, Linux,
      MacOS) and a binary distribution for Windows

####  INTERNAL
- recraft for ocaml 4.0 + ocamlbuild

___
## 2.8.0 ( May 27, 2016)

####  COMPILER 
- support of higher order actors (see `examples/working//ho`, `lib/caph/dc.cph` and Sec 2.4.6 of the LRM
- introduced a general, C-like, macro mechanism to pass arguments to programs from the
      command line (`D` option, see Sec. 9.7 of the LRM). As a result the ad-hoc `-arg<n>` options 
      are no longer supported (`-arg1 v1` is now written by `-D arg1=v1`). Examples have been rewritten
      accordingly.
- reimplementation of arrays. 2D and 3D arrays are now fully supported both at the expression and 
      network level

####  EXAMPLES
- added examples for higher order actors in `examples/working/ho`
- examples making use of command line arguments have been rewritten to use the new `-D` compiler option
- added an 3D lookup table in `examples/working/basic`

####  DOC
- added a section on higher order actors (Sec 2.4.6) in the LRM
- added a section describing the `-D option` (Sec 9.7)
- added a paragraph to the program example given at the end of Sec 2 illustrating 
      how to use higher order actors.

___
## 2.7.2 ( Mar 7, 2016)

####  COMPILER
- size attributes: ex `var z: array[n] = [ 0 | i=0 to z'size-1 ]` (see Sec 2.3.7 of the LRM)
- bug fix concerning negative float values and arrays of floats

####  INTERPRETER
- the default channel capacity is now set to 256 (can be changed with option `-chan_cap`)
- option `-dump_channel_stats` now also reports the final (end of run) occupation of channels

####  CAPH LIB
- revised implementations for `conv11{3,4,5}` and `conv21{3,4,5}` actors in `convol.cph` (kernels are now
      specified "from left to right" and as bidimensionnal arrays for 2D convolutions
- neighborhood wiring functions have been moved to a separate file `neigh.cph`
- added 1D neighborhood generators and updated names accordingly `neigh1<w>` for 1D, `neigh2<h><w>` for 2D

####  SYSTEMC
- the default value for FIFO size is now 256 (can be changed with option `-sc_default_fifo_size`)

####  VHDL
- the `-vhdl_quartus` option now also generates a `.xml` file listing the source files and
      the network IO (to used in the future with the GPStudio IDE)
- bug fix for declarations of arrays of size 1

####  ETC
- bug fix in `quartus/mk_quartus_proj` script
- `mkconv` program in `tools` for generating MxN convolution and neighborhood generation actors (see Appendix F of
      the reference manual and examples `conv/mkconv[12]`

####  DOC
- several corrections and typos (thx to M. Sizov for pointing them)
- added a section (Appendix F) in the reference manual on `mkconv` program
- the image processing example in the _CAPH Primer_ has been rewritten to use the predefined convolution 
      actors; a section has been added on FIFO dimensionning and blanking

####  Web SITE
- added convolution example to the code samples section
- added some published papers to the documentation section

___
## 2.7.1 ( Dec 9, 2015)

####  COMPILER
- new higher-order primitives `napp` and `nappi` (see `examples/working/network/{napp,nappi}`)
      and the section on higher-order primitives in the LRM)
- revised type for the `pipe` higher-order primitive 
- bug fix in the typing stage (thx to C. Bourrasset and his CNN deep-learning app for pointing it)

####  SYSTEMC 
- option `-sc_stop_when_idle` to stop simulation when outputs have been inactive for a given time

####  EXAMPLES
- `compose2`, `foldmap`

####  DOC
- updated section on intermediate representation in LRM 

___
## 2.7.0 (Dec 1, 2015)

####  COMPILER
- a prologue is now inserted at the beginning of each file generated by the compiler
- `#if/#else/#endif` directives and `-D` option for conditional compilation (see Sec. 9.7 of LRM
      and `examples/working/misc/ccond`)
- support for 3D arrays (see `examples/working/basic/lut4` and `examples/working/conv/conv_bank`)

####  SYSTEMC
- bug fix in class generation mechanism (when using polymorphic variants, in some cases the compiler
      emitted duplicate class declarations in file `xxx_globals.h`)
- added option `-sc_fifo_stats_offset` to specify an offset to be added to the reported FIFO sizes when
      using the `-sc_dump_fifo_stats` (see Sec 9.8 of the LRM)

####  VHDL
- recrafted VHDL backend; the generated actors now process one token per clock cycle and do not register 
      tokens on their I/O ports; as a result, applications may now operate at full clock speed; the "old" behavior 
      (one token every two clock cycles) can be emulated using the `-istream_clock_period` option.
- added option `-vhdl_istream_skew` to control the timing of input tokens at the test-bench level
- added options `-vhdl_quartus` to generated Altera Quartus II specific file(s) (for now: `xxx_net.qip`)
- removed package body from `xxx_globals.vhd` when empty (this causes a warning under Quartus II)
- bug fix for arrays of integers when the sign-ness/siez of integers is not specified

####  CAPH LIBRARY
- added `conv213`, `conv214] and [conv215] actors to [convol.cph` 
- added a separate package `optimized` containing "custom" versions of specific actors, such as `d1l`

####  SYSTEMC LIBRARY
- removed file `split.h]; the [split<n>` actors are now generated on the fly by the compiler in the
      application-specific file `xxx_splitters.h` (see Sec 9.3 of the LRM)

####  VHDL LIBRARY
- back to two separate models for small and big FIFOs. Customization is provided by options 
      `-vhdl_small_fifo_model`, `-vhdl_big_fifo_model` and `-vhdl_fifo_model_threshold`. Option
      `-vhdl_default_fifo_model` is deprecated (see Sec 9.8 of the LRM). Thx to F. Berry and C. Bourrasset
      for providing and testing optimized versions of the FIFO models
- removed file `split.vhd]; the [split<n>` actors are now generated on the fly by the compiler in the
      application-specific file `xxx_splitters.vhd` (see Sec 9.4 of the LRM)

####  EXAMPLES
- example in `working/misc/implemented` illustrates usage of the "#pragma implemented" directive
- example of 2D-histogramming in `working/app/histo2d`
- examples `working/network/filter_bank2] and [working/basic/lut4` illustrating 3D arrays

####  MISC
- the `vhdl.viewtrace] target in [lib/etc/Makefile.core` now expects a .ghw file (as generated by GHDL
      when using the --wave option); displaying .vcd files is still possible by invoking the `vhdl.viewvcdtrace`
- added a target `vhdl.quartus] in [lib/etc/Makefile.core` to automatize integration of VHDL generated code
      to Quartus II tool (with the help of predefined templates in `lib/etc/quartus`)

___
## 2.6.3 (Oct 1, 2015)

####  COMPILER
- bug fix : negative values on port "init" clause are now accepted
- bug fix : the option `-dump_fifo_stats` is now compatible even when IO ports are present

####  VHDL 
- bug fix in `stream_{in,out}.vhd `
- added option `-vhdl_ostream_period`

####  EXAMPLES
- added missing `#include "dc.cph"` in several `main.cph` files
     
___
## 2.6.2 (Sep 11, 2015)

####  COMPILER
- full support for n-ary type and value constructors. Ex

    ```
    type ($a,$b) pair = Pair of $a * $b
    ... 
    actor foo in (i:(bool, signed<8>) pair, ...)
    ...
    rules
    i:Pair(x,y) -> ...
    ```

    Sec. 2.4.1 of the LRM has been updated accordingly.
    Examples are given in `examples/working/apps/{rl,rle,rld}` and
    `examples/working/typing/{tydecl2,mixedarithm3}`.  
    **Warning**: the code generated by the SystemC backend for n-ary value ctors requires a `c++-11`
    compliant C++ compiler.
    
- the type of actor local variables is no longer limited to ground types (`signed`, `unsigned`, ...) and local enums but
      can be any globally defined type. Pattern-matching on constructed values is extended to local variables. Ex :

    ```
     type $t option = None | Some of $t;
     ...
     actor foo (...)
     var z: signed<8> option = None
     rules 
     ...
     | (..., z:Some x, ...) -> ...
     ...;
    ```

    As a side-effect, "full depth" pattern matching is no longer required, and the following actor, for example,
    is now perfectly legal (this example was listed in KNOWN-BUGS in previous versions) :

    ```
    actor id
      in (a:$t dc)
      out (c:$t dc)
      rules
      | a:x  -> c:x
      ;
    ```

- `foldt` and `pipe` higher-order primitives (see LRM p 50-51 and `examples/working/network/{foldt,pipe}`)
- fixed a bug in the error location mechanism in case of nested source files (`#include`)
- protection against multiple includes
- revised name mangling system for the SystemC and VHDL backends. The generated type and file names are shorter.
      Warning: this may require some changes in local Makefiles (esp those using automatically generated IO type
      converters (`decode_xxx`, `encode_xxx`) for VHDL)

####  CAPH LIBRARY
- the `dc` type is no longer built-in but now explicitely defined in `lib/caph/dc.cph`. For backward 
      compatibility the option `-abbrev_dc_ctor` still provides abbreviations to the corresponding value
      constructors (`'<` for `SoS`, `'>` for `EoS` and `'v` for `Data v`).

####  VHDL LIBRARY
- optimized code for `fifo_fb` in `lib/vhdl` (thx to F. Berry)

####  EXAMPLES
- updated `working/apps/{rl,rle,rld}` to make use of n-ary type and value constructors
- added `fir/fir33sep` and `typing/mixedarithm3` examples

___
## 2.6.1 (Jul 29, 2015)

####  COMPILER
- added support for complex array expressions in rule qualifiers; ex 

    ```
    rules
    | ... -> tab1[tab2[i]]:v
    ```

- added a `foldli` higher-order primitive (see LRM p 49 and `examples/working/network/foldli`)
- global constants are now correctly interpreted when used as type size parameters. For ex, the following program is now accepted :

    ```
    const ntaps = 8 : unsigned<8>;
    ...
    actor fir (...) var z : signed<...> array[ntaps];
    ```

    Before, `ntaps` in the last line was interpreted as a size variable. Thx to M. Pelcat for pointing this.

- fixed a significant bug in the simulator and the SystemC backend which caused execution results
  to depend on the order in which the actions where listed in a rule RHS when an array was both
  read and written in this rule. A typical example was :

    ```
    actor fir (...) in (i:...) out (o:...)
      var z : int<s,m> array[2] = [ v  | i=0 to 1 ]
      rules
      | i:x -> (o:..., z[1]:x, z[0]:z[1])
    ```

    The generated SystemC code for the rule was : `....; z[1] = p_x; z[0] = z[1]; ....`
    which is obviously wrong.
    Rule actions now systematically use a copy of the local variables, bringing back a fully synchronous 
    semantics for rule execution (just as for VHDL backend).
    As a result, the order in which rule actions are written is now irrelevant.
    Thx to C. Bourrasset and M. Pelcat for excavating this.

####  CAPH LIBRARY
- added `stream_ops.cph` for operating on unstructured streams

#### TOOLS
- fixed a bug in `bin2txt` program which caused some signed values to be misinterpretated

####  EXAMPLES
- added `fir13a` and `fir13b`, mono and multi-actor implementations of 1x3 FIR on unstructured streams
- examples in `working` are now sorted by categories, with a short description (`examples/working/Readme`)

___
## 2.6.0 (Jun 18, 2015)

####  COMPILER
- support for generic, signed and unsigned integers (see LRM, section 2.2.1, 2.2.3, 2.3.6, 2.4.5,
  tables 2.2, 2.3 and 2.5, chapter 3). It is now possible to define "sign-generic" actors, i.e. actors
  operating polymorphically on `signed` or `unsigned` quantities. As a result, the standard prelude
  does not contain any longer two versions (signed/unsigned) of each actor operating on integers. 
- support for size-polymorphic variants (see Sec 2.4.1)
- support for (a limited form of) dependant types. See section 2.4.5. It is now possible to define
  actors whose type depends on the value of some of their parameters. A typical example is :

    ```
    actor resize (k:int) in (a:signed<n>) out (signed<k>) ...
    ```

  The size of local variables, including arrays, may also depend on the value of some parameters.
  As a result, several actors in the standard prelude (`dkp`, `dl`, ...) have been rewritten so that they do
  not rely any longer on hardcoded constants (for storing lines in arrays for ex.). 
- type abbreviation now uses the symbol `==` (instead of `=`)

####  SYSTEMC
- the option `sc_use_templates` has been deprecated. The combination of the semantically unsound template 
  systemc of C++ and the buggy SystemC library just created too many problems. The SystemC backend nows supports 
  polymorphism via code replication (just as the VHDL one). 

####  VHDL LIB 
- the 2-bit tags in the VHDL representation of values having `t dc` type has changed :
- the value `Data v` is now encoded `11xxxxxx` (instead of `00xxxxxx`)
- the tag `00` is now used to encode (in `.bin` files) the special value "no data" (for blanking)
- the tags for `SoS` and `EoS` are unchanged
  The related programs (txt2bin and bin2txt) and the module `source.vhd` (in lib/vhdl) have been
  modified accordingly.
    
___
## 2.5.1 (Feb 27, 2015)

####  DISTRIBUTION
- fully revamped organisation, with configure script and install tools (see the file INSTALL.xxx)
- there are now three distinct distributions : source, bytecode and binary (for Win32)

####  COMPILER
- support for port-based I/O (see sec 2.4.3, chap 3 and sec 9.5.1 of the manual and examples/working/portio)
- option `-vhdl_rename_io_wires` to rename the IOs of the top-level network generated by the
  the VHDL backend (this may help plugging it in a pre-existing VHDL project)
- options `-restrict_inputs` and `-restrict_outputs` for using the compiler in web-based context

#### SYSTEMC LIBRARY
- renamed `source.h`, `sink.h`, `source_dc.h` and `sink_dc.h` to `stream_in.h`, `stream_out.h`, `stream_dc_in.h`
  and `stream_out_dc.h` 
- added `port_in.h`, `port_out.h`, `buffer_in.h`, `buffer_out.h`, `io_event.h`

####  VHDL LIBRARY
- renamed `source.vhd` and `sink.vhd` to `stream_in.vhd` and `stream_out.vhd`
- added `port_in.vhd`, `port_out.vhd] and [port_buffer.vhd`
- components `port_buffer` have their output set to 0 by the (asynchronous) `rst` signal

#### UTILS
- `txt2bin` : `-eventf` option for processing event files (to be used as port inputs)
 
___
## 2.5.0 (Dec 2014, JS)
- Internal release

___
## 2.4.1 (Nov 19, 2014)

##### COMPILER
- bug fix: in actors, rule like

    ```
    ... -> (o1: let v = ... in, o2: let v = ..., ...)
    ```

  where the same variable name `v` was used in `let` expressions associated to distinct outputs,
  generated incorrect code
- bug fix: error locations were not reported correctly

##### SYSTEMC backend
- bug fix in the implementation of `lib/systemc/source.h` (file globbing)

##### VHDL backend
- the 2-bit tags in the VHDL representation of values having `'a dc` type has changed :
- the value `Data v` is now encoded `11xxxxxx` (instead of `00xxxxxx`)
- the tag `00` is now used to encode (in `.bin` files) the special value "no data" (for blanking)
- the tags for `SoS` and `EoS` are unchanged
  The related programs (`txt2bin` and `bin2txt`) and the module `source.vhd` (in lib/vhdl) have been
  modified accordingly.
 
___
## 2.4.0 (Nov 5, 2014)
- `#include` directive for textual inclusion of files (see sec 9.6 of the ref man); as a
  consequence, the directory `lib/caph` does not contain a monolithic "standard prelude"
  any longer but a bunch of "standard libraries" which can be included separately in programs
  (see chap 10 of the manual)
- the `-I` option now serves to augment the search path for file inclusion
- support for multi-file I/O (see sec 9.5 of the manual and `examples/working/fglob[12]`)
- added a section on file I/O in chapter 9 of the manual
- added a mechanism for introducing horizontal and vertical "blanking" when generating SystemC and 
   VHDL code (see sec 9.5.4 of the manual and examples/working/conv33_v3)
- the tutorial "Caph primer" is now included in the distribution
- man pages for the `txt2pgm`, ... utility programs (installed in `man` subdir)
- bug fix in the `conv_signed` function of the CAPH VHDL library (the result of converting an
   `unsigned<n>` value to `signed<m>` (with m>n) was wrong if the value exceeded n/2)
- bug fix in file `makefile.systemc`, generated by the SystemC backend (invoking target `xxx.exe` now
   does not rebuild the source files if not necessary)
- bug fix in the VHDL backend : in some circumstances the type declaration for 2D arrays was uncomplete
- the compiler now emits a warning when the size (in bits) of an expression used as an array index
  is too small w.r.t. the indexed array. For ex, compiling the following code :

    ```
    actor foo ...
      var t : unsigned<8> array[4] = [1,2,3,4]
      var j: unsigned<1> = 0
      rules ...
      | ...  -> ...t[j]...
    ```

   will give this warning :

    ```
    Warning: the size (in bits) of this index expression (1) is too small wrt the range of the indexed array (4).
    ```

   The corresponding warnings are not emitted if the `-suppress_cast_warnings` option is asserted.
- size variables are now denoted without a leading "#"
   For ex. one will write a size-generic max function operating on unsigned integers as

    ```
    function max(x,y) = if x > y then x else y : unsigned<s> * unsigned<s> -> unsigned<s>;
    ```

- several bug fixes in the type-checker
- option `-vhdl_trace` is back
- the package generated by the VHDL backend for variant types (in file `xxx_types.vhd` now defines
   nullary constructors as constants and not functions with no argument (the latter seems not to be
   supported correctly by the Altera Quartus 9 synthetiser).
- signness suffixes ("S" or "U") are now optional when this property can be infered from the context. 
  Ex: one now can write 

    ```
    const z = [1,2,1] : signed<8> array[3]
    ```

  instead of 

    ```
    const z = [1S,2S,1S] : signed<8> array[3]
    ```

- some ambiguities in the language grammar definitions has been removed; as a result, in particular,
   negative constants should no longer be enclosed between brackets when used as network-level
   parameters; for ex., one should write

    ```
    net o = foo -1 i
    ```

   instead of

    ```
    net o = foo (-1) i
    ```

   and, even more conveniently,

    ```
    net o = bar [-1,2,-1] i
    ```

   instead of

    ```
    net o = bar [(-1),2,(-1)] i
    ```

- when using the `-dump_fsms` option, rule RHS expressions are now printed as "..." when their size
  exceeds 16 characters (the `Graphviz` application can get confused otherwise)
- added, `lib/caph/convol.cph` a pair of wiring functions `conv2{u,s}33c` for performing _centered_
  3x3 convolution on images
 
___
## 2.3.3 (Oct 25, 2014)
- internal release only
 
___
## 2.3.2 (Oct 1, 2014)
- internal release only
 
___
## 2.3.1 (Aug 26, 2014)
- internal release only
 
___
## 2.3.0 (May 22, 2014)
- support for bidimensional arrays (arrays of arrays, aka 1Dx1D arrays)
- `map` and `foldl` higher-order wiring primitives for describing parallel replication and
   reduction graph patterns (see Sec 2.4.5 of the reference manual); a nice example of how
   bidimensionnal arrays and map higher-order wiring can be combined is given in `examples/working/filter_bank`.
- reformated reference manual (cleaner presentation and a logo !)
- renamed actors `d`, `dp`, `f` and `fp` in the standard prelude
   to `dl`, `dkl`, `fl` and `fkl` resp. to minimize the risk of name clash with user code
 
___
## 2.2.0  (May 8, 2014)
- clarification (and simplication) of the syntax for injecting core-level expressions at the network level.
  Basically, there's now no syntactical distinction between the two levels.
  In particular, the awkward `'..'` syntax for passing parameters to actors has disappeared. 
  For example, an actor `foo` defined as

    ```
     actor foo (k:unsigned<8>) in (a:unsigned<8>) out (o:...) ...
    ```

  will be simply instanciated by writing

    ```
     actor foo (k:unsigned<8>) in (a:unsigned<8>) out (o:...) ...

    ```

  The tuple notation is used when the actor accepts several parameters.
  For example, an actor `foo` defined as

    ```
     actor bar (k1:unsigned<8>,k2:bool) in (a:unsigned<8>) out (o:...) ...
    ```

  will be instanciated by writing

    ```
     net o = bar (2,false) i 
    ```

  This makes instanciation of actors accepting arrays as parameters less confusing.
  For example, an actor `fir` defined as

    ```
     actor fir (kern:unsigned<8> array[3]) in (a:unsigned<8>) out (o:...) ...
    ```

  will now be instanciated by writing

    ```
     net o = fir [1,2,1] i 
    ```

  This modification is documented in Sec 2.4.5 of the reference manual.
  The corresponding sections describing the concrete and abstract
  syntaxes and the static semantics have been updated accordingly.
 
___
## 2.1.0   (Apr 25, 2014)
- revised semantics and syntax for handling arrays in rules. 
  For arrays, pattern matching and updates now only operate on individual items.
  For example, the following rule doubles the value of the second item of array `t` :

    ```
    | (..., t[1]:v, ...) -> (..., t[1]:v*2, ...)
    ```

  Array indexes in this context may be constants, local variables or any arithmetic combination of these.
  A typical example is given is `examples/working/histo[lf]`.
  As a consequence array comprehension expressions, such as

    ```
    | ... -> (..., t:[i+1 | i=0 to 4], ...)
    ```

  are no longer valid in rule RHS.
  This is not a problem in practice since their translation in VHDL was relying on the ability
  to perform multiple concurrent memory updates, not always supported by synthetizers.
  Array comprehension expressions are still supported for initializing arrays (at declaration), however.
  Sec 2.2.2, 2.4.4 and Chap. 3 of the reference manual have been updated according to this modification.
- added a new FIFO model to the VHDL library (credits: F.Berry, francois.berry@univ-bpclermont.fr).
  This model supports automatic switching from a LE to a RAM based implementation.
  The previous models (`fifo_small` and `fifo_big` are still available with the
  `-vhdl_small_fifo_model` and `-vhdl_default_big_fifo_model` options (see Sec 9.5 of the reference manual)
- the compiler now detects redefinitions in type, actor, value and io declarations; it also detects when,
  within an actor, a variable declaration shadows an input, output or parameter. 
- added an option `-dump_fsms` to dump graphical representation of "stateful" actors.
  A "stateful" actor is an actor for which the set of transition rules involves at least one local
   variable having an enumerated type. Ex:

    ```
    actor a (...)
      var s: {S0,S1,...} = S0
    rules
      (s:S0, ...) -> (s:S1, ...)
    ```

   In this case the enumerated variable is interpreted as a "state" variable and the set of transition
   rules is interpreted as describing a state diagram. The graphical representation of this diagram is
   output in `.dot` format (for visualisation with the [Graphviz](http://www.graphviz.org) suite of tools.
   The name of is file is `<actor_name>_fsm.dot`. 
   Illustrative examples are given in `examples/working/suml` and `examples/working/histof`.
- integer constants are now printed without signness indication (S/U) by default. To get this back, 
   use the option `-show_signness`
 
___
## 2.0.3   (Mar 26, 2014)
- bug fixed in the decl of local variables of type array 
- support for arrays in i/o text files (written `{v1,v2,...,vn}`) (see for ex `examples/working/mux_array`)
- added a new generic FIFO model (Magic_FIFO, developed by F. Berry) in `lib/vhdl` 
 
___
## 2.0.2   (Feb 12, 2014)
- the `trace` option of the simulator now produces a detailed report of rule activations, in the form :

    ```
    @<date> rule <pid>.<rule_no> <actor_name>:<rule_desc> selected <info>
    ```

   where 
     - `<date>` is the index of the simulation cycle
     - `<pid>` the process id (obtained with `-dump_denv` option)
     - `<rule_no>` the rule number (starting at one)
     - `<rule_desc>` a readable description of the rule 
     - `<info>` other informations
   The generated report can be used to compute various "timing" informations.

- updated Makefiles for C programs to make them compliant with gcc4 
- bug fix in the type checker with affected let rec defns 
 
___
## 2.0.1   (Jul 11, 2013)
- literal integer constants are now implicitely typed as `unsigned<s>`.
  Leaving the sign-ness of these constants un-specified at the source level
  created unsolvable ambiguities for the type-checker. Signed constants
  can be specified using the "S" suffix (ex: `const k = 12S`) or
  with an explicit type signature (ex: `x+(1:signed<8>)`)
- bug fix in type-checker : type casting does not involve unification;
  For ex: if `x` has type `int<'a>` then `x:int<8>` has type `int<8>` but the type of `x` remains `int<'a>`
- variant type definitions (ex: `type 'a option = None | Some of 'a`).  
   The `dc` type is now just a builtin case for this with syntax-level abbrevs :
     - `'<` for `SoS`
     - `'>` for `EoS`
     - `'v` for `Data v`

  Special syntax (`<con> %tag of ...`) for defining the encoding of constructor tags in the VHDL backend
- option `-vhdl_io_converters` for reading/writing data with dc type from/to files
- option `-sc_abbrev_dc_ctors` when compiling systemc code for reading/writing data to txt files
- support for polymorphic actors and polymorphic global fns :
   Ex : ```actor mux in (e1:$a, e2:$a, c:bool) out (o:$a) ...
        function abs(x) = if x<0S then -x else x : signed<#s> -> signed<#s>``` 
               where `$a` is a _type variable_ and `#s` is a _size variable_
   This is supported by generating templated code by the SystemC backend and by code replication by the VHDL backend
- syntax for source level macro is now `%arg1`, `%arg2`, ... (instead of `$arg1`, ...)
- option `-sc_use_templates` to choose between templated/replicated code for the SystemC code
- reading/writing pgm files is now only supported using external converters (`txt2pgm`, `pgm2txt`)
- renamed options for target code: `-fmt dot` is now `-dot`, `-fmt systemc` is
   now `-systemc`, `-fmt vhdl` is now `-vhdl`
- network level fns can now take core exprs as arguments
- Re-implementation of the type system (using explicit type schemes, see mofl2/v3).
- new syntax for array declarations and updates (see sec 2.2.2 of the LRM) 
- removed support for 2D arrays (they complicate the compiler and are they really useful any way ? )
- local variables are now automatically added to the environment when evaluating guards
- support for mutually recursive definitions (both at the expression and network levels)
 
___
## 1.8.2   (Apr 19, 2013)
- Bug fix : patterns like `(x)` in net declarations were incorrectly parsed as 1-tuples
- Changed the meaning of the back-annotation `fifo_max_occ` for the VHDL backend : setting
   the corresponding value to <n> now instanciates a FIFO of depth
   <n> in the generated VHDL (and not <n+2> as in previous versions). 
 
___
## 1.8.1   (Mar 23, 2013)
- Bug fix in the compiler : splitter boxes were not inserted
   correctly when a box had several outputs themselves connected to several inputs 
 
___
## 1.8.0   (Jan 12, 2013)
- variant syntax for rule declarations in actors. For example, the

    ```
    actor foo in (i:t1) out (o:t2)
    var s:t3
    rules (i, s) -> (o, s)
    | (p1, p2) -> (e1, e2)
    | ...
    ```

   can also be written without a rule schema as 

    ```
    actor foo in (i:t1) out (o:t2) var s:t3
    rules 
    | (i:p1, s:p2) -> (o:e1, s:e2)
    | ...
    ```

   The two forms are here equivalent, but the second one can be more expressive/concise in some situations
   (see ref manual, pp 24--26)
- fixed a bug in the parser which required to write `0-x` (resp. `0.0-.x`) to take
   the opposite of an integer (resp. float)
- the result files generated by the simulator are now added to the `caph.output` file
- updated reference manual
 
___
## 1.7.0   (Jan 5, 2013) New-Year release :-)
- floats can now be supported by the VHDL backend using the fp package provided at http://www.vhdl.org/fphdl.
   **Warning** : this feature is still largely experimental and not enabled by default.
   The INSTALL file describes how to activate it.
   Actual synthesis results will ultimately depends on your synthesis tools.
   A small example is provided in `examples/working/fscale`
- support for recursive definitions at the network level (see `examples/working/{sumlrec,factrec}`
   and the corresponding section in the reference manual)
- two examples illustrating `if..then..else` and `while`
   constructs at the network level (see `examples/working/{cond,while}`)
- updated defn of `conv[d][s]33]` wiring fns so that their outputs can be piped directly to combining fns
- a new example `working/bigfifo` showing how to use the profiling facilities to adjust FIFO sizes
- fixed typing bug affecting cast operations (in `(e:ty)`, the type of `e` and `ty` should _not_ be unified)
- fixed the implementation of big FIFOs in `lib/vhdl`
- updated reference manual
  
___
## 1.6.2  (Dec 17, 2012)  Christmas pre-release :-)
- support for size-generic actors (using templates for the systemc backend) (see `examples/working/{gensz,abs,d1p2,gmux}`)
- support for a `prelude` file : if a file named `prelude.cph` is present in the working directory (or
  in the directory specified with the `-I` option), its contents is read and analysed as if it was
  written at the beginning of the source file
- added a minimal prelude file in `lib/caph` containing "standard" actors and wiring functions (convolutions, ...)
- several examples (`examples/working/{conv*_v1,gdx,fir*,msfl}`) show how to combine size-generic actors with
  higher-order wiring functions to encapsulate reusable dataflow patterns (aka skeletons)
- a new example (`examples/working/dthr`) showing how to deal with "asynchronous" inputs
- the compiler now produces a file `caph.output` listing all the generated files
- fixed a nasty bug in the type checker which caused globally
  defined identifiers to shadow locally defined ones in certain circumstances
- identifiers can no longer end with "'" (this is not supported by the systemc and vhdl backend)
- file Changelog is now is the top directory (instead of src subdir)
- some common definitions in the Makefiles of examples have been lifted up in `Makefile.config`
- option `-make` to automatically generate the dependencies for the systemc and vhdl targets
  (see `examples/working/{scale,harris}`)
 
___
## 1.6.1  (Nov 3, 2012)
- the vhdl backend now makes use of the ieee `numeric_std` package to implement arithmetic and logic operations 
- option `-vhdl_target_dir` (resp. `-vhdl_sysc_dir`) to generate vhdl (resp. systemc) files in a specific directory
- bug fix in `utils/pgm2bin` and `utils/bin2pgm` (could not handle bitwidth > 32 !)
 
___
## 1.5.3 (Sep 30, 2012)
- (re)definition of the bitwise operators : `lnot`, `land`, `lor`, `lxor`, `lnand`, `lnor`, `lxnor`
- `-arg[1-4]` option to pass command-line options to programs (see `examples/working/macro`)
 
___
## 1.5.2 (Sep 11, 2012)
- comprehensive syntax for array initialisation and update Ex:

    ```
    var v : unsigned<8> array[4] = [ i in 0..3 <- i*2 ]
    var t : unsigned<8> array[4,4] = [ i in 0..3, j in 0..3 <- (i+1)*10+j+1 ]
    ```

   and (in rule):

    ```
    ... -> ..., t[in in 4..8 <-t[i]*2], ...
    ```
 
___
## 1.5.1 (Jun 8, 2012)
- global constant and functions can now refer to previously defined ones 
- the same pattern id may now appear in several _distinct_ rules (provided it has the same type). For ex :

    ```
    rules (e1,e2) -> s
    | (x, _) -> x
    | (_, x) -> x
    ```

   But the same id cannot only appear several times in the same rule.
   For ex : 

    ```
    rules (e1,e2) -> s
    | (x, x) -> x
    ```

   is forbidden

- new examples : `conv13`, `conv33`, `median13` and `median33`
 
___
## 1.5 (Jun 13, 2012)
- support for let expressions in rules (see `examples/working/let[12]`)
- by default, the VHDL backend now uses a custom `mul` fonction
 (defined in `lib/vhdl`) to translate the `*` operation. This prevents bound check
 failures at the sim level due to the fact that `std_logic[un]signed."*" `
 produces a result twice as wide as its inputs). Using the
 `std_logic_[un]signed.*` operator is still possible by using the `-vhdl_use_native_mul` option
- code cleanup
 
___
## 1.4 (Jan 11, 2012)
- basic support for guards in rules (see `examples/working/[thr_bis,transpose]`)
 
___
## 1.3 (Nov 24, 2011)
- re-implementation of the typing engine
- support for signed and unsigned sized ints (down to the SystemC and VHDL backends)
 
___
## 1.2 (Jul 19, 2011)
- support for 2D-arrays, including actor parameters, down to SystemC and VHDL backends
- bug fix in VHDL backend : actor vars are now (correctly) declared as signals
  (only locally bound patterns are declared as process variables) 
- (partial) bug fix : negative constants supported ; must be written "(-<int>)" 
- negative int constants are dumped as 2-compl bit vectors by the VHDL backend
 
___
## 1.1 (Jul 9, 2011)
- actor parameters can be (1D) arrays. Supported both by the SystemC and VHDL backends
 
___
## 0.3 (Apr 14, 2011)
- revamp undertaken for syncing with the LRM 1.1 (syntax, type system, ...)
- first public release -> renamed v1.0 on Jun 2011
 
___
## 0.2 (Apr 14, 2011)
- if-exprs are now correctly translated at vhdl-level (only top-level ones were supported in prev versions)
- annotate exprs and patterns with types in interm repr (internal modif, not user-visible ; should ease back-end translation)
 
___
## 0.1 (Mar 15, 2011)
- fifo annotation file
 
___
## 0.0 (Mar 3, 2011)
- cleaning of tree structure
- checking which examples/tests are working and not
- `caph-0.0` is based upon `hal-2.6.3`
- bug fix : stdout as output stream is now correctly handled 
- added support for reading/writing `pgm` (`P2`) files at the interpreter level
- added support for reading/writing `pgm` (`P2`) files to the
   `txt2bin`/`bin2txt` converters in order to support PGM io in VHDL mode

___
## -1.1 (HAL v2.6.3) (Feb 18, 2011)
- support for actors implemented as external code in SystemC and
 VHDL (will help implementing features while the backend is
 improved; is required for "special" actors such as "frame delay").
- added option `-vhdl_num_lib`

___
## -1.2 (HAL v2.6.2) (Feb 18, 2011)
- bug fix : rule order is now preserved in the backends (this is
 crucial when rules are not mutually exclusive) (they were reversed in the previous versions !)
- bug fix : `_` for inputs was (incorrectly) implemented as "read but dont care about value"; it really means "ignore"
- bug fix : `to_data` in `dcflow` package
- external fns are now supported by the SystemC and VHDL backends
 (fns must be provided in file `extfns.[h|cpp]` for SystemC, in package `extfns.vhd` for VHDL)
- `if` expressions are now supported
- added an option `-sc_alias_arrays` (but correct handling of arrays remains to be done...)

___
## -1.3 (HAL v2.6.1) (Dec 10, 2010)
- Major release 
- revised syntax (`const` and `function` declarations, `ndef` for `let`)
- type declarations (abbreviations)
- clarification of the distinction between expression level and
 coordination level expressions. The former are used to give values
 to global constants and functions and in actor rule expressions.
 The latter are used to define the network structure. 
- external fns with support at the interpreter level (dynamic loading)
- revised intermediate representation (in module Interm) : it is now
 a "degenerated" FSM, specifying only a set of transitions on an
 implicit state. Explicit states are introduced in the VHDL backend only. 
- first VHDL backend (preliminary : no support for global or external fns)
- updated SystemC backend
- a few examples now correctly up to VHDL simulation
 
___
## -1.4 (HAL v2.5.1) (Nov 11, 2010)
- revised dynamic semantics (get rid of the separate update step during process execution); this
 affects `dsval.ml`, `dynamic.ml` and `process.ml`
- added an option `-dump_channel_stats` to keep track of maximal channel usage during a simulation run
 
___
## -1.5 (HAL v2.4.3) (Oct 29, 2010)
- by default, the compiler now creates separate outputs at FSM
 level when an output is bound to several wires; the ruleset is
 modified accordingly; this saves the programmer from manually
 declaring "replicating" actors for forking data streams; this
 behavior can be turned off with the `-dont_split_outputs` option
 (but the code produced by the SystemC and VHDL backends may be wrong in this case)
- basic support for external fns in the SystemC backend (see
 `examples/systemc/extfn and examples/systemc/grad`)
- patterns bound in the rules are now renamed to avoid confusion
 with local variables or inputs having the same name
- two extra examples showing how arrays can be used to implement
 delays (see `examples/systemc/d1l and examples/systemc/dkp`)
- support for PGM image files i/o (in `examples/systemc/lib`) (see
 `examples/systemc/pgmio and examples/systemc/grad`)
- special syntax for initializing arrays : ex
   `var x : int array[16] = [ 0 : 16 ]` means initialize array x with 16 `0`s
 
___
## -1.6 (HAL v2.4.2) (Oct 21, 2010)
- the SystemC backend now supports enum types 
- simplify the rules in the FSM when var/out updates can be carried out immediatly
 
___
## -1.7 (HAL v2.4.1) (Oct 18, 2010)
- rule patterns and exprs are now qualified in the static semantics (qualifiers were added in the dynamic sem in v2.3.8)
- FSM interm repr
- SystemC backend
 
___
## -1.8 (HAL v2.3)
- Major recrafting
- Sync with LRM :
- ext fn decls take names of corresp vhdl and c fns
- actor params can take a default value
- actor vars can be initialized when declared (ex: `var s : int = 4`)
- no specific decl any more for state; it is a var with an enum type (ex: `var s : (L1, L2) = L2`)
- simulator based on dynamic semantics
 
___
## -1.9 (HAL v2.2)
- Added optional size to type declarations
- Handling aliases when generating actions on transitions
 
___
## -1.10 (HAL v2.1)
- array type and operations (for implementing line delay)

___
## -1.11 (HAL v2.0)
- (Almost) full syntax  : actors, i/o streams, external fns decls and fgn-like definitions for describing the data-flow graph
- Outputs resulting DFG and FSMs as .dot files
