# hypernumbers-1.0 make file
# (C) Hypernumbers Ltd 2009
# 
# Written by Gordon Guthrie gordon@hypernumbers.com

# include a copy of the mochiweb include.mk file with all the Erlang
# guff set up
include ../support/include.mk

APPLICATION='hypernumbers-1.0'
DOC_OPTS={dir,\"../doc\"}

all: $(EBIN_FILES)

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES)

edoc:
	$(ERL) -noshell -pa ../ebin \
		-eval "edoc:application($(APPLICATION), \".\", [$(DOC_OPTS)])" \
		-s init stop
