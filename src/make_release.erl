-module(make_release).

-export([
         make_rel/0
         ]).

-define(tmp, "/tmp/make_release/").

make_rel() ->

    % set up some stuff
    Root = code:lib_dir(hypernumbers) ++ "/../../",
    file:set_cwd(Root++"ebin/"),

    % extract the key stuff for the release
    ok = systools:make_tar("hypernumbers"),
    file:make_dir(?tmp ++ ""),

    % copy it over and untar it into a temporary directory
    file:copy(Root++"ebin/hypernumbers.tar.gz",
              ?tmp ++ "hypernumbers.tar.gz"),
    erl_tar:extract(?tmp ++ "hypernumbers.tar.gz",
                    [{cwd, ?tmp}, compressed]),

    % delete the archive (gonnae make a new one with the same name)
    os:cmd("rm " ++ Root ++ "ebin/hypernumbers.tar.gz"),

    % a bit of a tidy up
    ok = file:delete(?tmp ++ "hypernumbers.tar.gz"),

    % clean up some stuff that we don't want to publish
    HNPrivRoot = ?tmp ++ "lib/hypernumbers-1.0/priv/",
    os:cmd("rm -rf " ++ HNPrivRoot ++ "css_generator"),
    os:cmd("rm -rf " ++ HNPrivRoot ++ "fns_generator"),
    os:cmd("rm -rf " ++ HNPrivRoot ++ "upload_test"),
    FEPrivRoot = ?tmp ++ "lib/formula_engine-1.0/priv/",
    os:cmd("rm -rf " ++ FEPrivRoot ++ "*"),
    STRoot = ?tmp ++ "lib/hypernumbers-1.0/priv/site_types/sust_adv/",
    os:cmd("rm -rf " ++ STRoot),

    % delete our salts module - they need their own
    os:cmd("rm " ++ HNPrivRoot ++ "../ebin/salts.beam"),
    SaltSrc = HNPrivRoot ++ "../src/",
    file:make_dir(SaltSrc),
    file:copy(Root ++ "/priv/fns_for_binary/salts.erl.example",
              SaltSrc ++ "salts.erl.example"),

    % we will now add some new stuff
    file:make_dir(?tmp ++ "bin"),
    % now copy in the various critcal bits from the main erlang release
    Components = ["erlc", "escript", "start", "start_erl", "to_erl", "erl",
                  "run_erl", "start.boot", "start_sasl.boot", "typer"],
    [file:copy("/usr/local/lib/erlang/bin/" ++ X,
               ?tmp ++ "bin/" ++ X)
     || X <- Components],
    [file:copy(Root ++ X, ?tmp ++ X) || X <- ["hn", "shell"]],

    % now tar up the release
    file:set_cwd(?tmp ++ ""),
    erl_tar:create("hypernumbers.tar.gz",
                               ["hn", "shell", "./bin", "./lib",
                                "./priv", "./releases"],
                               [compressed]),
    % copy it back into ebin
    file:copy(?tmp ++ "hypernumbers.tar.gz",
              Root ++ "ebin/hypernumbers.tar.gz"),

    % finally clean up
    os:cmd("rm -rf " ++ ?tmp),
    file:set_cwd(Root++"ebin/"),
    ok.

