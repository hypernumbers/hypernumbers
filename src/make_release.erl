-module(make_release).

-export([
         make_rel/0
         ]).

-define(tmp, "/tmp/make_release/").

make_rel() ->
    io:format("in here...~n"),
    % set up some stuff
    Root = code:lib_dir(hypernumbers) ++ "/../../",
    io:format("Root is ~p~n", [Root]),
    file:set_cwd(Root++"ebin/"),

    io:format("got to here...~n"),
    % extract the key stuff for the release
    ok = systools:make_tar("hypernumbers"),

    io:format("got to 2~n"),
    file:make_dir(?tmp),
    % copy it over and untar it into a temporary directory
    file:copy(Root++"ebin/hypernumbers.tar.gz",
              ?tmp ++ "hypernumbers.tar.gz"),
    erl_tar:extract(?tmp ++ "hypernumbers.tar.gz",
                    [{cwd, ?tmp}, compressed]),
    io:format("got to 3~n"),
    % delete the archive (gonnae make a new one with the same name)
    os:cmd("rm " ++ Root ++ "ebin/hypernumbers.tar.gz"),

    % a bit of a tidy up
    ok = file:delete(?tmp ++ "hypernumbers.tar.gz"),
    io:format("got to 4~n),
    % delete the erlang stuff which is included for a proper
    % release
    os:cmd("rm -rf " ++ ?tmp ++ "/lib/crypto-2.0.2/"),
    os:cmd("rm -rf " ++ ?tmp ++ "/lib/inets-5.5.1/"),
    os:cmd("rm -rf " ++ ?tmp ++ "/lib/kernel-2.14.2/"),
    os:cmd("rm -rf " ++ ?tmp ++ "/lib/mnesia-4.4.16/"),
    os:cmd("rm -rf " ++ ?tmp ++ "/lib/public-key-0.10/"),
    os:cmd("rm -rf " ++ ?tmp ++ "/lib/sasl-2.1.9.2/"),
    os:cmd("rm -rf " ++ ?tmp ++ "/lib/ssl-4.1.1/"),
    os:cmd("rm -rf " ++ ?tmp ++ "/lib/stdlib-1.17.2/"),

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
    io:format("about to copy salts...~n"),
    file:copy(Root ++ "/priv/fns_for_binary/salts.erl.example",
              SaltSrc ++ "salts.erl"),
    % need to compile the salts file
    io:format("about to compile salts...~n"),
    compile:file(SaltSrc ++ "salts.erl", 
         [{outdir, SaltSrc ++ "../ebin"}]),
    io:format("Salts compiled~n"),

    % we will now add some new stuff
    % copy in the various critcal bits from the main erlang release
    %BinRoot = "/usr/local/lib/erlang/bin/",
    %BinFiles = filelib:wildcard(BinRoot ++ "*"),
    %file:make_dir(?tmp ++ "bin/"),
    %[file:copy(X, ?tmp ++ "bin/" ++ file(X)) || X <- BinFiles],
    %[file:copy(Root ++ X, ?tmp ++ X) || X <- ["hn", "shell"]],
    % now add the ebin/ directory
    EbinFiles = ["compile_code.beam","hypernumbers.rel"],
    file:make_dir(?tmp ++ "ebin/"),
    [file:copy(Root ++ "ebin/" ++ X, ?tmp ++ "ebin/" ++ X) || X <- EbinFiles],

    % now add the sys.config.default
    file:make_dir(?tmp ++ "priv"),
    file:make_dir(?tmp ++ "keys"),
    file:copy(Root ++ "priv/sys.config.default", 
       ?tmp ++ "priv/sys.config.default"),

    % now tar up the release
    file:set_cwd(?tmp),
    erl_tar:create("hypernumbers.tar.gz",
                               ["hn", "./ebin", "shell", "./lib", "./priv", 
                                "./releases"],
                               [compressed]),
    % copy it back into ebin
    file:copy(?tmp ++ "hypernumbers.tar.gz",
              Root ++ "ebin/hypernumbers.tar.gz"),

    % finally clean up
    os:cmd("rm -rf " ++ ?tmp),
    file:set_cwd(Root++"ebin/"),
    ok.

%file(X) -> List = string:tokens(X, "/"),
%	   hd(lists:reverse(List)).
 