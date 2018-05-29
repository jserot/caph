## ###############################################################################
## This file has been automatically generated with command :
## /Users/jserot/Dev/ml/caph/bin/caphmake
## ###############################################################################

include /Users/jserot/Dev/ml/caph/lib/etc/Makefile.core

-include examples.proj

dot: examples.dot

examples.dot: main.cph $(GEN_CAPH_SRCS)
	$(CAPHC) -I /Users/jserot/Dev/ml/caph/lib/caph -dot $(DOT_OPTS) main.cph

dot.show: examples.dot
	$(GRAPHVIZ) examples.dot

makefiles: sim.makefile systemc.makefile vhdl.makefile

sim.makefile: Makefile.sim
Makefile.sim: main.cph $(GEN_CAPH_SRCS)
	$(CAPHC) -I /Users/jserot/Dev/ml/caph/lib/caph -make -sim $(SIM_OPTS) main.cph
	@echo "Wrote file Makefile.sim"

systemc.makefile: Makefile.systemc
Makefile.systemc: main.cph $(GEN_CAPH_SRCS)
	$(CAPHC) -I /Users/jserot/Dev/ml/caph/lib/caph -make -systemc $(SC_OPTS) main.cph
	@echo "Wrote file Makefile.systemc"

vhdl.makefile: Makefile.vhdl
Makefile.vhdl: main.cph $(GEN_CAPH_SRCS)
	$(CAPHC) -I /Users/jserot/Dev/ml/caph/lib/caph -make -vhdl $(VHDL_OPTS) main.cph
	@echo "Wrote file Makefile.vhdl"

sim.run:
	make -f Makefile.sim run CAPH=/Users/jserot/Dev/ml/caph
sim.show:
	make -f Makefile.sim show CAPH=/Users/jserot/Dev/ml/caph
sim.save:
	make -f Makefile.sim save CAPH=/Users/jserot/Dev/ml/caph
sim.clean:
	make -f Makefile.sim clean CAPH=/Users/jserot/Dev/ml/caph
sim.clobber:
	make -f Makefile.sim clobber CAPH=/Users/jserot/Dev/ml/caph

systemc.code:
	make -f Makefile.systemc code CAPH=/Users/jserot/Dev/ml/caph
systemc.exe:
	make -f Makefile.systemc exe CAPH=/Users/jserot/Dev/ml/caph
systemc.run:
	make -f Makefile.systemc run CAPH=/Users/jserot/Dev/ml/caph
systemc.show:
	make -f Makefile.systemc show CAPH=/Users/jserot/Dev/ml/caph
systemc.save:
	make -f Makefile.systemc save CAPH=/Users/jserot/Dev/ml/caph
systemc.check:
	make -f Makefile.systemc check CAPH=/Users/jserot/Dev/ml/caph
systemc.clean:
	make -f Makefile.systemc clean CAPH=/Users/jserot/Dev/ml/caph
systemc.clobber:
	make -f Makefile.systemc clobber CAPH=/Users/jserot/Dev/ml/caph

vhdl.bin:
	make -f Makefile.vhdl bin CAPH=/Users/jserot/Dev/ml/caph
vhdl.code:
	make -f Makefile.vhdl code CAPH=/Users/jserot/Dev/ml/caph
vhdl.exe:
	make -f Makefile.vhdl exe CAPH=/Users/jserot/Dev/ml/caph
vhdl.run:
	make -f Makefile.vhdl run CAPH=/Users/jserot/Dev/ml/caph
vhdl.show:
	make -f Makefile.vhdl show CAPH=/Users/jserot/Dev/ml/caph
vhdl.save:
	make -f Makefile.vhdl save CAPH=/Users/jserot/Dev/ml/caph
vhdl.check:
	make -f Makefile.vhdl check CAPH=/Users/jserot/Dev/ml/caph
vhdl.viewtrace:
	make -f Makefile.vhdl viewtrace CAPH=/Users/jserot/Dev/ml/caph
vhdl.viewvcdtrace:
	make -f Makefile.vhdl viewvcdtrace CAPH=/Users/jserot/Dev/ml/caph
vhdl.clean:
	make -f Makefile.vhdl clean CAPH=/Users/jserot/Dev/ml/caph
vhdl.clobber:
	make -f Makefile.vhdl clobber CAPH=/Users/jserot/Dev/ml/caph

xdf: main.xdf

main.xdf: main.cph $(GEN_CAPH_SRCS)
	$(CAPHC) -I /Users/jserot/Dev/ml/caph/lib/caph -xdf $(XDF_OPTS) main.cph


dif: main.dif

main.dif: main.cph $(GEN_CAPH_SRCS)
	$(CAPHC) -I /Users/jserot/Dev/ml/caph/lib/caph -dif $(DIF_OPTS) main.cph


clean:
	if [ -e Makefile.sim ]; then make -f Makefile.sim clean CAPH=/Users/jserot/Dev/ml/caph; fi
	if [ -e Makefile.systemc ]; then make -f Makefile.systemc clean CAPH=/Users/jserot/Dev/ml/caph; fi
	if [ -e Makefile.vhdl ]; then make -f Makefile.vhdl clean CAPH=/Users/jserot/Dev/ml/caph; fi
	$(RM) caph.output
	$(RM) examples.dot examples_expanded.dot *_act_*.dot *_mocs.dat
	$(RM) *.xdf *.cal
	$(RM) $(GEN_CAPH_SRCS)

clobber: clean
	if [ -e Makefile.sim ]; then make -f Makefile.sim clobber CAPH=/Users/jserot/Dev/ml/caph; fi
	if [ -e Makefile.systemc ]; then make -f Makefile.systemc clobber CAPH=/Users/jserot/Dev/ml/caph; fi
	if [ -e Makefile.vhdl ]; then make -f Makefile.vhdl clobber CAPH=/Users/jserot/Dev/ml/caph; fi
	$(RM) Makefile.sim
	$(RM) Makefile.systemc
	$(RM) Makefile.vhdl
	$(RM) $(GEN_CAPH_SRCS)
	$(RM) *_sdf_fifo_sizes.dat
	$(RMDIR) dot sim systemc vhdl xdf
	$(RM) *~
