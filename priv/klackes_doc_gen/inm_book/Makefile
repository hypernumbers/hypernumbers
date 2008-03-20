include ../support/include.mk

F =     ../inm_intro/intro.xml \
	../classes/classes.xml \
	../config_elems/config_elems.xml \
	../netconf_sessions/netconf_sessions.xml \
	../netconf_extensions/netconf_extensions.xml \
	../notifications/notifications.xml

PDF_OUT = ../output/pdf/inm.pdf
HTML_OUT = ../output/html/inm.html

inm:	inm.xml $(PDF_OUT) $(HTML_OUT) all

inm.xml:	$(F) Makefile
	echo > inm.xml;
	for i in $(F); do\
	   cat $$i >> inm.xml; \
	done




# Actually I think joinPDF is available on more platforms - the port of
# pdftk was broken on MacOS and joinPDF just seemed a lot easier...
#   http://www.iis.ee.ic.ac.uk/~g.briscoe/ICL/JoinPDF.html
ifeq ($(shell (test -x /usr/bin/joinPDF || test -x /usr/local/bin/joinPDF) && \
        echo yes),yes)
# Mac clause
PDFCONCAT = joinPDF ../output/pdf/user_guide-$${vsn}.pdf $${d}
else
# linux clause, emerge app-text/pdfjam or apt-get install pdfjam
ifeq ($(shell (test -x /usr/bin/pdfjoin || test -x /usr/local/bin/pdfjoin) && \
        echo yes),yes)
PDFCONCAT = pdfjoin $${d} --outfile ../output/pdf/user_guide-$${vsn}.pdf 
else
# pdftk - which requires gcc java compiler - boring
PDFCONCAT = pdftk $${d} cat output ../output/pdf/user_guide-$${vsn}.pdf
endif
endif



all:
	set -x;\
	d="../output/pdf/inm.pdf `find ../../../man -name '*.pdf' -print | sort -d`"; \
	vsn="`cat ../../../vsn.mk | sed s/INMVSN=//`"; \
	$(PDFCONCAT)

clean: 
	rm -f $(PDF_OUT) $(HTML_OUT) *.out *.out0 *~ inm.xml 2> /dev/null
	rm -f ../output/pdf/user_guide-*.pdf
