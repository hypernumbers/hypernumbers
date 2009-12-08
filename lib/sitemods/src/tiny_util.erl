%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie gordon@hypernumbers.com
%%% @copyright (C) 2009, Hypernumbers Ltd
%%% @doc       Library for tiny.hn
%%%
%%% @end
%%% Created :  2 Dec 2009 by Gordon Guthrie
%%%-------------------------------------------------------------------
-module(tiny_util).

%% %% API
%% -export([
%%          get_password/0,
%%          make_subs/0,
%%          get_unallocated_sub/0
%%         ]).

%% %% Debugging API
%% -export([
%%          clear_down_mnesia_DEBUG/0,
%%          hey/0,
%%          hey2/0,
%%          setup/3
%%         ]).

%% -include("password.hrl").
%% -include("tiny.hrl").

%% -define(ALPHABET, [$a, $b, $c, $d, $e, $f, $g, $h, $i, $j,
%%                    $k, $l, $m, $n, $o, $p, $q, $r, $s, $t,
%%                    $u, $v, $w, $x, $y, $z]).
%% -define(SUBS, tiny_sub).
%% -define(PERCENTAGE, $%).

%% get_password() -> 
%%     N1 = gen_server:call(random_srv, {random, int, ?NO_OF_WORDS}),
%%     N2 = gen_server:call(random_srv, {random, int, ?NO_OF_WORDS}),
%%     N3 = gen_server:call(random_srv, {random, int, 99}),
%%     {value, {N1, W1}} = lists:keysearch(N1, 1, ?WORDS),
%%     {value, {N2, W2}} = lists:keysearch(N2, 1, ?WORDS),
%%     W1 ++ "!" ++ W2 ++ integer_to_list(N3).
           
%% make_subs() ->
%%     % List = make_subs1(make_subs1(make_subs1([[]]))),
%%     List = gen_subs(1),
%%     Rand = make_random(List, []),
%%     NewList = lists:keysort(1, Rand),
%%     ok = create_sub_table(),
%%     write(NewList).

%% get_unallocated_sub() ->
%%     Fun = fun() ->
%%                   Head = #tiny_sub{sub = '$1', allocated=false},
%%                   {[H | _T], _Cont}
%%                       = mnesia:select(?SUBS, [{Head, [], ['$1']}], 1, write),
%%                   Rec = #tiny_sub{sub = H, allocated = true},
%%                   ok = mnesia:write(Rec),
%%                   H
%%           end,
%%     mnesia:activity(transaction, Fun).

%% %%
%% %% Internal Functions
%% %%

%% gen_subs(X) -> 
%%     gen_subs(X, [[]]).
%% gen_subs(0, Subs) -> Subs;
%% gen_subs(N, Subs) -> 
%%     Subs2 = [[X | Y] || X <- ?ALPHABET, Y <- Subs],
%%     gen_subs(N-1, Subs2).


%% make_random([], Acc) -> Acc;
%% make_random([H | T], Acc) ->
%%     R = random:uniform(26*26*26),
%%     make_random(T, [{R, H} | Acc]).

%% write([]) -> ok;
%% write([{_K, V} | T]) ->
%%     Fun = fun() ->
%%                   ok = mnesia:write(#tiny_sub{sub = V})
%%           end,
%%     mnesia:activity(transaction, Fun),
%%     write(T).

%% create_sub_table() ->
%%     Tables = mnesia:system_info(tables),
%%     case lists:member(?SUBS, Tables) of
%%         true  -> ok;
%%         false ->  Attrs = [{record_name, ?SUBS},
%%                            {attributes, record_info(fields, ?SUBS)},
%%                            {type, set},
%%                            {index, [allocated]},
%%                            {disc_only_copies, [node()]}],
%%                   {atomic, ok} = mnesia:create_table(?SUBS, Attrs),
%%                   ok
%%     end.

%% get_json_files(List) -> get_json_files1(List, []).

%% get_json_files1([], Acc)      -> Acc;
%% get_json_files1([H | T], Acc) ->
%%     case get_path(H) of
%%         {ok, Path} -> get_json_files1(T, [{H, Path} | Acc]);
%%         {error, _} -> get_json_files1(T, Acc)
%%     end.

%% get_path(String) ->
%%     LongPath = string:tokens(String, "."),
%%     case LongPath of
%%         ["path" | T] ->
%%             case lists:nth(length(T), T) of
%%                 "json" -> {ok, "/" ++ string:join(get_path2(T), "/") ++ "/"};
%%                 _      -> {error, String}
%%             end;
%%         _ -> {error, String}
%%     end.

%% get_path2(["json"]) -> [];
%% get_path2(List)     -> lists:sublist(List, 1, length(List) -1).

%% is_valid_email(Email) ->
%%     EMail_regex = "[a-z0-9!#$%&'*+/=?^_`{|}~-]+"
%%         ++ "(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*"
%%         ++ "@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+"
%%         ++ "(?:[a-zA-Z]{2}|com|org|net|gov|mil"
%%         ++ "|biz|info|mobi|name|aero|jobs|museum)", %" for syntax highighting
%%     case re:run(Email, EMail_regex) of
%%         nomatch    -> not_email;
%%         {match, _} -> {email, Email}
%%     end.

%% get(Key, List) -> lists:keyfind(Key, 1, List).

%% add_host(Config, IP, Port, DomainName) ->
%%     {hosts, Hosts} = get(hosts, Config),
%%     NewHosts = {hosts, lists:merge([{IP, Port, [DomainName]}],
%%                                    lists:sort(Hosts))},
%%     lists:keyreplace(hosts, 1, Config, NewHosts).

%% persist_conf(FileName, Config) ->
%%     NewConfig = make_terms(Config, []),
%%     ok = file:write_file(FileName, NewConfig).

%% make_terms([], Acc)      ->
%%     Header = "%% -*- mode: erlang -*-",
%%     lists:flatten([Header, 10, 10 | lists:reverse(Acc)]);
%% make_terms([H | T], Acc) ->
%%     make_terms(T, [io_lib:fwrite("~p.~n~n", [H]) | Acc]).

%% %%
%% %% Mini-Scripting "Languages"
%% %%

%% run(Script, Details, Fun) ->
%%     case filelib:is_file(Script) of
%%         true  -> {ok, Terms} = file:consult(Script),
%%                  run1(Terms, Details, Fun);
%%         false -> ok
%%     end.

%% run1([], _D, _Fun) ->
%%     ok;
%% run1([[?PERCENTAGE | _T1] | T], D, Fun) ->
%%     run1(T, D, Fun); % ignore comments 
%% run1(["\n" | T], D, Fun) ->
%%     run1(T, D, Fun); % ignore blank lines
%% run1([H | T], D, Fun) ->
%%     Fun(H, D),
%%     run1(T, D, Fun).

%% run_script({Path, '$email'}, D) ->
%%     Expr = D#run_details.email,
%%     run_script2(Path, Expr, D);
%% run_script({Path, '$username'}, D)  ->
%%     Toks = string:tokens(D#run_details.email, "@"), 
%%     run_script2(Path, hd(Toks), D);
%% run_script({Path, '$site'}, D)      ->
%%     Expr = D#run_details.sub ++ "."++D#run_details.domain,
%%     run_script2(Path, Expr, D);
%% run_script({Path, '$subdomain'}, D) ->
%%     Expr = D#run_details.sub,
%%     run_script2(Path, Expr, D);
%% run_script({Path, '$expiry'}, D)    ->
%%     {Date, _Time} = calendar:now_to_datetime(now()),
%%     NewDays = calendar:date_to_gregorian_days(Date) + 31,
%%     NewDate = calendar:gregorian_days_to_date(NewDays),
%%     Expr = "This site will expire on "
%%         ++ dh_date:format("D d M Y", {NewDate, {0, 0, 0}}),
%%     run_script2(Path, Expr, D);
%% run_script({Path, '$password'}, D) ->
%%     Expr = D#run_details.password,
%%     run_script2(Path, Expr, D);
%% run_script({Path, Expr}, D) ->
%%     run_script2(Path, Expr, D).

%% run_script2(Path, Expr, D) ->
%%     Attrs = [{"formula", Expr}],
%%     Site = "http://" ++ D#run_details.sub
%%         ++ "." ++ D#run_details.domain ++ ":"
%%         ++ integer_to_list(D#run_details.port),
%%     RefX = hn_util:parse_url(Site ++ Path),
%%     hn_db_api:write_attributes([{RefX, Attrs}]).

%% run_perms({control, C}, Dt)  ->
%%     {list, L}     = get(list, C),
%%     {page, Pg}    = get(page, C),
%%     {perms, Pm}   = get(perms, C),
%%     {override, O} = get(override, C),
%%     {views, V}    = get(views, C),
%%     Site = "http://" ++ Dt#run_details.sub ++ "."
%%         ++ Dt#run_details.domain ++ ":"
%%         ++ integer_to_list(Dt#run_details.port),
%%     auth_srv:add_controls(Site, L, Pg, Pm, O, V);

%% run_perms({perm, P}, Dt) ->
%%     {list, L}     = get(list, P),
%%     {page, Pg}    = get(page, P),
%%     {perms, Pm}   = get(perms, P),
%%     Site = "http://" ++ Dt#run_details.sub ++ "."
%%         ++ Dt#run_details.domain ++ ":"
%%         ++ integer_to_list(Dt#run_details.port),
%%     auth_srv:add_perm(Site, L, Pg, Pm);

%% run_perms({views, V}, Dt)    ->
%%     {list, L}     = get(list, V),
%%     {page, Pg}    = get(page, V),
%%     {override, O} = get(override, V),
%%     {views, Vw}    = get(views, V),
%%     Site = "http://" ++ Dt#run_details.sub ++ "."
%%         ++ Dt#run_details.domain ++ ":"
%%         ++ integer_to_list(Dt#run_details.port),
%%     auth_srv:add_views(Site, L, Pg, O, Vw);

%% run_perms({default, D}, Dt)  ->
%%     {default, Df} = get(default, D),
%%     {page, Pg}    = get(page, D),
%%     Site = "http://" ++ Dt#run_details.sub ++ "."
%%         ++ Dt#run_details.domain ++ ":"
%%         ++ integer_to_list(Dt#run_details.port),
%%     auth_srv:add_default(Site, Pg, Df).

%% run_users({{user, User}, {group, Groups},
%%            {email, EMail}, {password, Password}}, Dt) ->
%%     User2     = get_user(User, Dt),
%%     EMail2    = get_email(EMail, Dt),
%%     Password2 = get_password(Password, Dt),
%%     run_users2(User2, Groups, EMail2, Password2, Dt).

%% run_users2(User, Groups, _EMail, Password, Dt) ->
%%     Site = "http://" ++ Dt#run_details.sub ++ "."
%%         ++ Dt#run_details.domain ++ ":"
%%         ++ integer_to_list(Dt#run_details.port),
%%     ok = hn_users:create(Site, User, Password),
%%     ok = hn_users:add_groups(Site, User, Groups),
%%     ok.

%% get_user('$username', Dt) -> hd(string:tokens(Dt#run_details.email, "@"));
%% get_user(User, _Dt)       -> User.

%% get_email('$email', Dt) -> Dt#run_details.email;
%% get_email(EMail, _Dt)   -> EMail.

%% get_password('$password', _Dt) -> get_password();
%% get_password(Password, _Dt)    -> Password.

%% %%
%% %% Debugging stuff
%% %%
%% setup(Domain, Port, Type) ->
%%     create_new_site(get_unallocated_sub(), Domain, Port,
%%                     Type, "gordon@hypernumbers.com", get_password()).

%% hey() ->
%%     create_new_site(get_unallocated_sub(), "tiny.dev", 9000,
%%                     "tiny_quiz", "gordon@hypernumbers.com", get_password()).
%% hey2() ->
%%     create_new_site(get_unallocated_sub(), "tiny.dev", 9000,
%%                     "hypernumbers", "gordon@hypernumbers.com", get_password()).

%% clear_down_mnesia_DEBUG() ->
%%     Tables = mnesia:system_info(tables),
%%     delete_table(Tables).

%% delete_table([])      -> ok;
%% delete_table([H | T]) ->
%%     Tab = atom_to_list(H),
%%     case Tab of
%%         "schema"  -> ok;
%%         [49 | _T] -> ok; %49 is ASCII for '1'
%%         _         -> {atomic, ok} = mnesia:delete_table(H)
%%     end,
%%     delete_table(T).
