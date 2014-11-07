Installation Etc
----------------

This document gives instructions on how to do some basic stuff with Hypernumbers:
* installation
* build
* start
* hypernumbers on the file system
* create a cluster
* add a new function
* create a site-type
* instantiate a new instance of a site-type
* TODO

Installation
------------

Hypernumbers required Erlang R14B04 which is available from:

http://www.erlang.org/download_release/12

Installing on Ubuntu
--------------------

First get all the erlang dependencies with
``sudo apt-get build-dep erlang``
then build Erlang R14B from source

Hypernumbers has 5 dependencies which need to be installed.

On Ubuntu they are:
* ``sudo apt-get install libicu``
* ``sudo apt-get install libicu-dev``
* ``sudo apt-get install ant1.7``
* ``sudo apt-get install openjdk-6-jdk``
* ``sudo apt-get install rake``

icu is the internalisation library for Unicode from IBM:
http://site.icu-project.org/

Installing on Redhat/Centos
---------------------------

Start with Erlang
``sudo yum install gcc glibc-devel make ncurses-devel openssl-devel autoconf``
then build Erlang R14B from source

You need to install icu version 44 manually.

Download ``icu4c-4_4_2-src.tgz`` from http://site.icu-project.org/
``tar -xvf icu4c-4_4_2-src.tgz``
``cd icu/source``
``chmod +x runConfigureICU configure install-sh``
``./runConfigureICU Linux``
``make``
``sudo make install``

Then install the dependencies:
* ``sudo yum install ant``
* ``sudo yum install java-1.7.0-openjdk``
* ``sudo yum install ruby``
* ``gem install rake``

On some older version of CentOS rake won't install and you need to install Ruby by hand:

``sudo yum install gpg``
``gpg2 --keyserver hkp://keys.gnupg.net --recv-keys D39DC0E3``
``\curl -sSL https://get.rvm.io | bash -s stable --ruby``
``rvm install 2.1``
``gem install rake``

Install Hypernumbers
--------------------

Then git clone the source into a working directory.

Setting Up Your Cryptographic Salts
-----------------------------------

Cryptographic salts are defined in the file:
``lib/hypernumbers-1.0/src/salts.erl``

You probably want to edit them.

Build
-----

The build structure predates rebar and is done with the command ``hn``

Typing ``./hn`` will bring up the help

    Commands to run hypernumbers are:
      start [erl_args]
      stop [remote@host.com]
      do refresh|hotswap|restart|migrate [remote@host.com]

    Commands to build hypernumbers are:
      build
      quick
      lexer-parser
      migrate
      clean

    Other commands are:
      debug
      buddy
      jslint [ [] | /dir/hn.*.js]
      no_mnesia

The most important ones are the build commands

* ``./hn build`` does a full build of all dependencies and runs the code to generate Erlang modules from the lexer/parsers, complies the Erlang and hot loads it and then copies js/html assets
* ``./hn lexer-parser`` does a partial build - the lexer/parsers run and then the main application is compiled and hot loaded (dependencies are not) and copies the js/html assets
* ``./hn quick`` just does a quick Erlang compile and hot-load and copies the js/html assets

For details of what copying the js/html assets means please see the section on Hypernumbers In The File System

Starting Hypernumbers
---------------------

Hypernumbers is started detached by typing:
``./hn start``

This creates a batch job called ``shell`` that can be used to attach a shell. To start with a shell simply type:
``./hn start && ./shell``

All the Hypernumbers data is created in a directory ``$HNROOT/var``:

* database tables
* site data
* logs
* ``sys.config``
* etc, etc

The start up checks 3 things:

* is there a ``sys.config`` in the directory ``$HNROOT/var`` - if not copy ``$HNROOT/priv/sys.config.default`` to ``$HNROOT/var``
* is there a ``.erlang.cookie`` in ``~`` - if not create a random cookie
* is there a Hypernumbers site created - if not create a blank one on the domain ``hypernumbers.dev`` at the default port

The default ``sys.config`` has a sane set of configurations

Hypernumbers On The File System
-------------------------------

The code is in an old-fashioned OTP filelayout ``$HNROOT/lib/APP``

The various dependencies have been copied into this structure, for instance:

* bert
* erlsha2
* gettext
* mochiweb
* sgte
* twilio

The main hypernumbers applications are:
* formula_engine-1.0 (spreadsheet formulae and execution engine)
* hypernumbers-1.0 (mostly everything else)
* read_excel-1.0 (reads Excel '97 file formats)
* starling (unicode C port - maintained seperately)
* sysmon-1.0 (not very important)

Inside ``$HNROOT/lib/formula_engine-1.0/priv`` are the various ``.xrl`` and ``.yrl`` files that define the spreadsheet language and some ruby libraries that are used to generate them for multi-linguage spreadsheets (the double-meta)

Inside ``$HNROOT/lib/hypernumbers-1.0/priv/core_install`` are all the website assets to make all Hypernumbers sites work, stuff like:

* html
* css
* js
* minification specifications

These files are copied to all site specific docroots and asset directories in files like ``$HNROOT/var/sites/any.sites.com&1234/``

Each Hypernumbers site has a ``type`` - blank is the commonest one - but there are a number of pre-built types in ``$HNROOT/lib/hypernumbers-1.0/priv/site_types``

On build and update these files are also copied over to the appropriate docroot or asset directory eg ``$HNROOT/var/sites/any.sites.com&1234/``

Clustering Hypernumbers
-----------------------

Hypernumbers can be run as a cluster, but a number of things needs to happen:

* there is only one master node - it runs both the ``hns`` and ``passport`` services - all the other nodes must have these set as ``false`` in their ``sys.config``
* the various URLs (``sync_url``, ``reset_url`` and ``norefer_url``) should point back to a site on the master node
* a common cookie must be on all the nodes in ``.erlang.cookie``

There is a whole world of ``hns`` zones and automatic provisioning

**TODO**

Adding A New Function
---------------------

The spreadsheet functions exposed to the user are just Erlang functions.

There are two major classes of Spreadsheet functions, and they are in ``$HNROOT/lib/formula-engine-1.0/src``:

* ones named ``stdfuns_something.erl`` which are Excel '97 compatible
* ones named ``hnfuns_anotherthing.erl`` which are hypernumbers specific

The modules to be exposed as Spreadsheet functions are listed in the Erlang module ``$HNROOT/lib/formula_engine-1.0/src/fns.erl``

To add a new custom function:

* create a new file ``myfuns_something.erl``
* add that module name to ``fns.erl`` in ``fns:get_modules/0`
* export a function with ``arity/1``
* compile and let it reload
* good to go

Things are a bit more complex than that. Spreadsheet functions receive one argument which is a list of all the arguments that are in the function in the spreadsheet.

So a spreadsheet function ``do_something(1, 2, "a", A1, true)`` be executed in Erlang as ``myfuns:do_something([1, 2, "a", {cellref, ...}, 'TRUE'])``

The first step in any spreadsheet function is to cast the appropriate values to things that the function can operate on. For custom functions the default casts of the module ``typechecks.erl`` should be used. For instance:

   -module(myfns).

   -export([somefun/1]).

   somefun([A, C, C]) ->
      [A1, B1] = typechecks:std_ints([A, B]),
      C1 = typechecks:std_bools([C]),
      ...

Each of these functions takes a list and returns a list. More complex typecasting can be done with the module ``muin_collect.erl`` - look at Excel compatible functions so see how they use it.

A spreadsheet function can return one of two types of things:

* a single return value (string, number, boolean, error)
* a ``#spec_val{}`` record

TODO user defined functions that return a ``#spec_val{}``

Create A Site Type
------------------

When you have built an application in Hypernumbers (say an expenses system) you can save it as a sitetype and deploy multiple instances of it.

The cycle is this:

* create your site by making pages, using templates and the ``=create.button(...)`` function
* from the console save it as a sitetype using the command ``hn_archive:export_as_sitetype("http://some.domain.com:1234", new_type).``
* it will have been saved in ``$HNROOT/lib/hypernumbers-1.0/priv/site_types/new_type``
* to create a new instance use ``hn_setup:site("http://another.domain.com:1234", new_type, []).``
* the last option (here an empty list) can be used to pass in instantiation parameters if the site type has been setup to use them.



TODO
----

The following sections remain to be written:
* setting up your own cryptographic salts
* configuring your own e-mail provider
* setting up deployment zones
