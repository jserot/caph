DIFF=diff #-Nau

all: diff

diff: #FORCE
	echo "Hello"
	/usr/bin/diff location.mli.org ../location.mli > location.mli.diff
#	$(DIFF) location.ml.org ../location.ml > location.ml.diff
#	$(DIFF) parsing.mli.org ../parsing_m.mli > parsing_m.mli.diff
#	$(DIFF) parsing.ml.org ../parsing_m.ml > parsing_m.ml.diff

clean:

realclean: clean
	rm -f *.diff
	rm -f *~

clobber: realclean

#FORCE:
