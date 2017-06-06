#$Header:$
#SUBDIRS= 
#./rhrv/gs/Makefile
.PHONY: clean
clean:
	-rm -f *.aux *.log *.bbl *.blg *.glo \
	*.gls *.idx *.ind *.ilg *.inf \
	*.out *.toc *.tpt *.pdfsync
	@for d in * ; do \
	 if test -d  $${d} ; then \
	 echo directory $${d}; \
	  if (test "$${d}" != CVS); then	  (cd $${d} && $(MAKE) $@;) fi;\
	  fi \
	done

pdf:
	@for d in * ; do \
	 if test -d  $${d} ; then \
	 echo directory $${d}; \
	  if (test "$${d}" != CVS); then	  (cd $${d} && $(MAKE) $@;) fi;\
	  fi \
	done
	

