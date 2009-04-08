# Hypernumbers make file
# (C) Hypernumbers Ltd 2009
# 
# Written by Gordon Guthrie gordon@hypernumbers.com

# include a copy of the mochiweb include.mk file with all the Erlang
# guff set up
include support/include.mk

all:
	(cd lib/hypernumbers-1.0/src;$(MAKE)   all)
	(cd lib/read_excel-1.0/src;$(MAKE)     all)
 	(cd lib/formula_engine-1.0/src;$(MAKE) all)
 	(cd lib/mochiweb;$(MAKE)               all)

debug:
	(cd lib/hypernumbers-1.0/src;$(MAKE)   debug)
	(cd lib/read_excel-1.0/src;$(MAKE)     debug)
	(cd lib/formula_engine-1.0/src;$(MAKE) debug)
 	(cd lib/mochiweb;$(MAKE)               debug)

clean:
	(cd lib/hypernumbers-1.0/src;$(MAKE)   clean)
	(cd lib/read_excel-1.0/src;$(MAKE)     clean)
	(cd lib/formula_engine-1.0/src;$(MAKE) clean)
 	(cd lib/mochiweb;$(MAKE)               clean)

edoc:
	(cd lib/hypernumbers-1.0/src;$(MAKE)   edoc)
	(cd lib/read_excel-1.0/src;$(MAKE)     edoc)
	(cd lib/formula_engine-1.0/src;$(MAKE) edoc)
 	(cd lib/mochiweb;$(MAKE)               edoc)
