# Hypernumbers make file
# (C) Hypernumbers Ltd 2009
# 
# Written by Gordon Guthrie gordon@hypernumbers.com
all:
	(cd lib/read_excel-1.0;$(MAKE)     all)
	(cd lib/formula_engine-1.0;$(MAKE) all)
	(cd lib/mochiweb;$(MAKE)           all)
	(cd lib/gettext;$(MAKE)            all)
	(cd lib/hypernumbers-1.0;$(MAKE)   all)

debug:
	(cd lib/mochiweb;$(MAKE)           all)
	(cd lib/gettext;$(MAKE)            all)
	(cd lib/read_excel-1.0;$(MAKE)     debug)
	(cd lib/formula_engine-1.0;$(MAKE) debug)
	(cd lib/hypernumbers-1.0;$(MAKE)   debug)

clean:
	(cd lib/read_excel-1.0;$(MAKE)     clean)
	(cd lib/formula_engine-1.0;$(MAKE) clean)
	(cd lib/mochiweb;$(MAKE)           clean)
	(cd lib/gettext;$(MAKE)            clean)
	(cd lib/hypernumbers-1.0;$(MAKE)   clean)

edoc:
	(cd lib/read_excel-1.0;$(MAKE)     edoc)
	(cd lib/formula_engine-1.0;$(MAKE) edoc)
	(cd lib/mochiweb;$(MAKE)           edoc)
	(cd lib/gettext;$(MAKE)            edoc)
	(cd lib/hypernumbers-1.0;$(MAKE)   edoc)
