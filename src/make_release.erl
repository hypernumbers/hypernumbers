-module(make_release).

-export([
         make_rel/0
         ]).

make_rel() ->

    % set up some stuff
    Root = code:lib_dir(hypernumbers) ++ "/../../",
    file:set_cwd(Root++"ebin/"),

    % extract the key stuff for the release
    ok = systools:make_tar("hypernumbers"),
    file:make_dir("/tmp/make_release/"),

    % copy it over and untar it into a temporary directory
    file:copy(Root++"ebin/hypernumbers.tar.gz",
              "/tmp/make_release/hypernumbers.tar.gz"),
    erl_tar:extract("/tmp/make_release/hypernumbers.tar.gz",
                    [{cwd, "/tmp/make_release"}, compressed]),

    % delete the archive (gonnae make a new one with the same name)
    os:cmd("rm " ++ Root ++ "ebin/hypernumbers.tar.gz"),

    % a bit of a tidy up
    ok = file:delete("/tmp/make_release/hypernumbers.tar.gz"),

    % clean up some stuff that we don't want
    HNPrivRoot = "/tmp/make_release/lib/hypernumbers-1.0/priv/",
    os:cmd("rm -rf " ++ HNPrivRoot ++ "css_generator"),
    os:cmd("rm -rf " ++ HNPrivRoot ++ "fns_generator"),
    os:cmd("rm -rf " ++ HNPrivRoot ++ "upload_test"),

    FEPrivRoot = "/tmp/make_release/lib/formula_engine-1.0/priv/",
    os:cmd("rm -rf " ++ FEPrivRoot ++ "*"),

    % we will now add some new stuff
    file:make_dir("/tmp/make_release/bin"),
    % now copy in the various critcal bits from the main erlang release
    Components = ["erlc", "escript", "start", "start_erl", "to_erl", "erl",
                  "run_erl", "start.boot", "start_sasl.boot", "typer"],
    [file:copy("/usr/local/lib/erlang/bin/" ++ X,
               "/tmp/make_release/bin/" ++ X)
     || X <- Components],

    % now tar up the release
    erl_tar:create("/tmp/make_release/hypernumbers.tar.gz",
                               ["/tmp/make_release/bin",
                                "/tmp/make_release/priv",
                                "/tmp/make_release/releases"],
                               [compressed]),
    % copy it back into ebin
    file:copy("/tmp/make_release/hypernumbers.tar.gz",
              Root ++ "ebin/hypernumbers.tar.gz"),

    % finally clean up
    os:cmd("rm -rf /tmp/make_release/"),
    ok.

