* It is not allowed for a pattern variable to appear several times with different types in a rule set.
  For example, the following actor does not compile :

    ```actor bug ()
      in (a:signed<8>)
      out (c:bool)
    var s : unsigned<8>
    rules
    | a:v -> c:(v>0)
    | s:v -> c:(s>1)
    ;```

  This is clearly too restrictive (and anyway does not correspond to the natural interpretation of pattern binding).

  OTOH, the same variable pattern can appear several times with the same type.
  For example, the following actor is ok :

    ```actor ok ()
      in (a:signed<8>)
      out (c:bool)
    var s : signed<8>
    rules
    | a:v -> c:(v>0)
    | s:v -> c:(s>1)
    ;```

* The simulator use signed native ints (31 or 63 bits, depending on the platform) for encoding
  `signed<n>` and `unsigned<n>` values.  This may result in discrespancies with results obtained by
  the simulator and the SysC/VHDL backends.

* The support of floats by the VHDL backend is very experimental, relying on VHDL-2008 proposed extensions which are 
  only partially supported by VHDL-93 compilers ans synthetizers (see : http://www.vhdl.org/fphdl)

* The SystemC backend translates `signed<n>` (resp. `unsigned<n>`) values to `sc_int<n>` (resp. `sc_uint<n>`) quantities.
  Unfortunately, in the latter classes, all computations are done using an internal 64 bit representation. This means
  that overflows will not be detected (and this, in turn, may explain some discrepancies between the results obtained
  by the SystemC backend OTOH and the VHDL backend OTOH... This is really a pity, since it significantly limits 
  the advantages of using the `sc_[u]int<n>` but overcoming this limitation would require a complete rewriting of these
  classes...

* Free type/size variables cannot appear in the output signature of an actor. For example, the following program 
  is rejected (1) :

        ```actor foo
         in (a:signed<8>)
        out (c:signed<s>)
        rules
        | a:x -> c:0
        ;
        
        stream i1:signed<8> from "sample.txt";
        stream o1:signed<16> to "result.txt";
        
        net o1 = foo i1;```

  This is because the output of the `foo` box keeps type `int<signed,s>` (`int` in `foo_xxx.h`) 
  although it is bound to a signal having type `int<signed,16>` in `main_net.cpp`.
  The underlying explanation is that the static analysis only propagates type information "downward"; i.e. it
  cannot "refine" the type of the `foo` box to `int<signed,16>` by taking into account the fact that it is
  actually connected to a wire of this type (this kind of unification, performed by `Typing.type_application`,
  only occurs for _inputs_). 
  Fixing this would probaly require a signicant change in the static analysis.
  A pragmatic workaround, meanwhile, is to simply forbid free type/size variables in actor outputs (i.e.
  all type/size vars occuring in the type of an actor output must occur in its input or as a parameter).

  (1) in fact, it typechecks and simulates but fails when generating SystemC and VHDL code

* In certain situations, litteral integer constants used as arguments to value constructors have to be explicitely type-annotated
  for the SystemC and VHDL backends. A typical exemple follows : 

        ```type $t tau = Foo of $t;
        
        actor bar
          in (i:signed<8>)
          out (o:signed<8>)
        var v: signed<8> tau = Foo 0; -- problem here
        rules 
        ... ;```
        
   Here, the VHDL backend will fail to generate the code of the [bar] actor, with the following message :
      `** Not implemented: VHDL translation of value 0.`

  The reason is that in this particular case, the code generator is not able to retrieve the "full" type (signness and size in bits)
  of the argument of the `Foo` value constructor. This bug should be fixed in future
  versions. Meanwhile, the workaround is simply to
  explicitely refine the type of the arguments, i.e., in the previous example, to write the declaration of `v` as :

        `var v: signed<8> tau = Foo ((0:signed<8>))`

* When used as function or actor parameters, array accesses must be put between parens.
  Ex : `net o = foo (t[0]) i ...` instead of `net o = foo t[0] i ...`
  This is required to solve ambiguities in the syntax (which are hard to solve with a LALR(1) parser like CamlYacc).

* The comprehension syntax for n-D arrays with n>1 is fragile and can result in typing errors in certain situations.

* The `fifo_small` implementation provided in lib/vhdl does not work with FIFO sizes < 2.
  The `fifo_big` implementation provided in lib/vhdl does not work with FIFO sizes < 1.
  This explains why the default value for FIFO size offset is set to 2.
  This means that, when using these default implementations, all FIFOs will have a minimum depth of 2. 

* Bounds of integer range types can only be constants or parameter names. Expressions are not allowed.
  Ex :  `actor foo (n: int) ... var s: {1,..,n}` is allowed
    but `actor foo (n: int) ... var s: {0,..,n-1}` is not allowed
