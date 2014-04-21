%%% @author    Gordon Guthrie <gordon@hypernumbers.dev>
%%% @copyright (C) 2013-2014, Gordon Guthrie
%%% @doc       Broken out post fns for hn_mochi
%%%
%%% @end
%%% Created : 28 Mar 2013 by Gordon Guthrie <gordon@hypernumbers.dev>

%%%-------------------------------------------------------------------
%%%
%%% LICENSE
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Affero General Public License as
%%% published by the Free Software Foundation version 3
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Affero General Public License for more details.
%%%
%%% You should have received a copy of the GNU Affero General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%-------------------------------------------------------------------

-module(hn_post).

-include("hn_mochi.hrl").
-include("spriki.hrl").

-define(E, error_logger:error_msg).
-define(dbapi, new_db_api).
-define(WRATTR, new_db_api:write_attributes).
-define(TOLERANCE, 1.0e-10).

-export([
         post/3
        ]).

-spec post(#refX{}, #qry{}, #env{}) -> any().
%% a post to the phone view - need to check there is a control
%% TODO remove this clause
%% TODO write some posts
post(#refX{obj = {cell, _}} = Ref, #qry{view = ?PHONE},
     #env{accept = json} = Env) ->
    case  ?dbapi:get_phone(Ref) of
        []      ->
            hn_mochi:json(Env, {struct, [{"error", "no phone at this url"}]});
        [Phone] ->
            case hn_twilio_mochi:handle_phone_post_DEPR(Ref, Phone, Env) of
                {ok, 200}    -> hn_mochi:json(Env, "success");
                {error, 401} -> hn_mochi:respond(401, Env);
                _            -> hn_mochi:'500'(Env)
            end
    end;

post(Ref = #refX{path = ["_parse_expression"]} = Ref, _Qry, Env) ->
    [{"expression", Expr}] = Env#env.body,
    % this expr is not going to be run, but you
    % need to compile it in a cell context
    % just firing in A1 - ie {cell, {1, 1}}
    Expr2 = muin:parse_expr_for_gui(Expr),
    hn_mochi:json(Env, {struct, [{"expression", Expr2}]});

post(Ref = #refX{path = ["_forgotten_password"]} = Ref, _Qry,
     Env = #env{uid = Uid}) ->
    ok = status_srv:update_status(Uid, Ref, "forgot password"),
    case passport_running() of
        false -> hn_mochi:'404'(Ref, Env);
        true  ->
            {S, R} = case lists:sort(Env#env.body) of
                         [{"email", E1}] ->
                             {ok, S1} = application:get_env(hypernumbers,
                                                            norefer_url),
                             request_pwd_reset(E1, S1);
                         [{"email", E1}, {"site", S1}] ->
                             request_pwd_reset(E1, S1);
                         [{"email", E1}, {"hash", Hash}, {"newpwd", NPwd}] ->
                             reset_password(E1, NPwd, Hash)
                     end,
            hn_mochi:json(Env,{struct,[{"status",S}, {"response",R}]})
    end;

post(#refX{site = S, path = ["_login"]}, Qry, E) ->
    SortedList = lists:sort(E#env.body),
    [{"email", Email0},{"pass", Pass}, {"remember", _R}] = SortedList,
    Email = string:to_lower(Email0),
    case passport:authenticate(Email, Pass, true) of
        {error, authentication_failed} ->
            hn_mochi:json(E, {struct, [{"response", "error"}]});
        {ok, Uid, Stamp, Age} ->
            Return = case Qry#qry.return of
                         R when R /= undefined -> mochiweb_util:unquote(R);
                         _Else -> hn_util:strip80(S)
                     end,
            {E2, Redir} = hn_mochi:post_login(S, Uid, Stamp, Age, E, Return),
            hn_mochi:json(E2, {struct, [{"redirect", Redir}]})
    end;

%% the purpose of this message is to mark the mochilog so we don't
%% need to do nothing with anything...
post(_Ref, _Qry, Env = #env{body = [{"mark", _Msg}]}) ->
    hn_mochi:json(Env, "success");

%% the purpose of this message is to log javascript errors into mochilog
%% so we don't need to do anything with anything
post(_Ref, _Qry, #env{body = [{"jserr", _Msg}]} = Env) ->
    hn_mochi:json(Env, "success");

post(#refX{obj = {page, "/"}} = R, _Qry,
     #env{body = [{"load_template", {_, [{"name", Nm}]}}], uid = Uid} = Env) ->
    ok = status_srv:update_status(Uid, R, "created page from template " ++ Nm),
    ok = hn_templates:load_template(R, Nm, Uid),
    hn_mochi:json(Env, "success");

post(#refX{obj = {cell, _}} = Ref, _Qry,
     #env{body = [{"drag", {_, [{"range", Rng}]}}], uid = Uid} = Env) ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    ok = ?dbapi:drag_n_drop(Ref,
                            Ref#refX{obj = hn_util:parse_attr(range, Rng)},
                            Uid),
    hn_mochi:json(Env, "success");

post(Ref = #refX{obj = {O, _}}, _Qry,
     Env = #env{body = [{"insert", "before"}], uid = Uid})
  when O == row orelse O == column ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    ok = ?dbapi:insert(Ref, Uid),
    hn_mochi:json(Env, "success");

post(Ref = #refX{obj = {O, _}}, _Qry,
     Env = #env{body = [{"insert", "after"}], uid = Uid})
  when O == row orelse O == column ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    ok = ?dbapi:insert(make_after(Ref), Uid),
    hn_mochi:json(Env, "success");

%% by default cells and ranges displace vertically
post(Ref = #refX{obj = {O, _}}, _Qry,
     Env = #env{body = [{"insert", "before"}], uid = Uid})
  when O == cell orelse O == range ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    ok = ?dbapi:insert(Ref, vertical, Uid),
    hn_mochi:json(Env, "success");

%% by default cells and ranges displace vertically
post(Ref = #refX{obj = {O, _}}, _Qry, Env = #env{body = [{"insert", "after"}],
                                                 uid = Uid})
  when O == cell orelse O == range ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    ok = ?dbapi:insert(make_after(Ref), vertical, Uid),
    hn_mochi:json(Env, "success");

% but you can specify displacement manually - these are 'before' inserts
post(Ref = #refX{obj = {O, _}}, _Qry,
     Env = #env{body = [{"insert", Direction}],
                uid = Uid})
  when (O == cell orelse O == range),
       (Direction == "horizontal" orelse Direction == "vertical") ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    ok = ?dbapi:insert(Ref, list_to_atom(Direction), Uid),
    hn_mochi:json(Env, "success");

post(Ref = #refX{obj = {O, _}}, _Qry,
     Env = #env{body = [{"delete", "all"}], uid = Uid})
  when O == page orelse O == row orelse O == column ->
    Status = case O of
                 page   -> "deleted page";
                 row    -> "deleted row(s)";
                 column -> "deleted column(s)"
             end,
    ok = status_srv:update_status(Uid, Ref, Status),
    ok = ?dbapi:delete(Ref, Uid),
    hn_mochi:json(Env, "success");

post(Ref = #refX{obj = {O, _}}, _Qry,
     Env = #env{body = [{"delete", Direction}],
                uid = Uid})
  when O == cell orelse O == range,
       (Direction == "horizontal" orelse Direction == "vertical") ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    ok = ?dbapi:delete(Ref, list_to_atom(Direction), Uid),
    hn_mochi:json(Env, "success");

post(Ref = #refX{obj = {O, _}}, _Qry,
     Env = #env{body = [{"delete", "all"}],
                uid = Uid})
  when O == cell orelse O == range ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    ok = ?dbapi:delete(Ref, vertical, Uid),
    hn_mochi:json(Env, "success");

%% These three cases could be collapsed into one...
post(#refX{obj = {O, _}} = Ref, _Qry,
     #env{body = [{"copy", {struct, [{"src", Src}]}}], uid = Uid} = Env)
  when O == cell orelse O == range ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    copy(Ref, Src, all, Uid, Env);
post(#refX{obj = {O, _}} = Ref, _Qry,
     #env{body = [{"copystyle", {struct, [{"src", Src}]}}], uid = Uid} = Env)
  when O == cell orelse O == range ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    copy(Ref, Src, style, Uid, Env);
post(#refX{obj = {O, _}} = Ref, _Qry,
     #env{body = [{"copyvalue", {struct, [{"src", Src}]}}], uid = Uid} = Env)
  when O == cell orelse O == range ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    copy(Ref, Src, value, Uid, Env);

%% post(#refX{obj = {O, _}} = Ref, _Qry,
%%      Env = #env{body = [{"borders", {struct, Attrs}}], uid = Uid})
%%   when O == cell orelse O == range ->
%%     Where = from("where", Attrs),
%%     Border = from("border", Attrs),
%%     Border_Style = from("border_style", Attrs),
%%     Border_Color = from("border_color", Attrs),
%%     ok = status_srv:update_status(Uid, Ref, "edited page"),
%%     ok = ?dbapi:set_borders(Ref, Where, Border, Border_Style, Border_Color),
%%     hn_mochi:json(Env, "success");

%% post(Ref = #refX{path = ["_user"]}, _Qry,
%%      _Env = #env{body = [{"set", {struct, [{"language", _Lang}]}}],
%%                  uid = Uid}) ->
%%     ok = status_srv:update_status(Uid, Ref, "changed language"),
%%     throw("can't set language right now");
%% ok = hn_users:update(Site, Uid, "language", Lang),
%% hn_mochi:json(Env, "success");

%% post for inline editable cells
post(#refX{obj = {cell, _}} = Ref, _Qry,
     #env{body = [{"postinline", {struct, [{Type, Val}]}}], uid = Uid} = Env)
  when Type == "formula" orelse
       Type == "clear"   ->
    case {Type, ?dbapi:read_attribute(Ref, "input")} of
        {"formula", [{#xrefX{}, "inline"}]} ->
            % escape the attributes to prevent script injection, etc
            Attrs = [{"formula", hn_util:esc(Val)}],
            ok = ?WRATTR([{Ref, Attrs}], Uid, Uid),
            ok = status_srv:update_status(Uid, Ref, "edited page"),
            hn_mochi:json(Env, "success");
        {"formula", [{#xrefX{}, "inlinecheckbox"}]} ->
            % escape the attributes to prevent script injection, etc
            % checkboxes can only send true/false values - make it so
            Val2 = case hn_util:esc(Val) of
                       "true"  -> "true";
                       "false" -> "false"
                   end,
            Attrs = [{"formula", Val2}],
            ok = ?WRATTR([{Ref, Attrs}], Uid, Uid),
            ok = status_srv:update_status(Uid, Ref, "edited page"),
            hn_mochi:json(Env, "success");
        {"formula", [{#xrefX{}, "inlinerich"}]} ->
            case hn_html_sanitizer:is_sane(Val) of
                {true, Val2} ->
                    Attrs = [{"formula", Val2}],
                    ok = ?WRATTR([{Ref, Attrs}], Uid, Uid),
                    ok = status_srv:update_status(Uid, Ref, "edited page"),
                    hn_mochi:json(Env, "success");
                false ->
                    hn_mochi:respond(403, Env)
            end;
        {"formula", [{#xrefX{}, {"increment", Incr}}]} ->
            OldVal = case new_db_api:read_attribute(Ref, "__rawvalue") of
                         [{_X, OldV}] -> OldV;
                         Other        -> 0
                     end,
            Val2 = tconv:to_num(Val),
            OldVal2 = case tconv:to_num(OldVal) of
                          {error, nan} -> 0;
                          OVal         -> OVal
                      end,
            Diff = abs(Val2 - OldVal2) - Incr,
            if
                Diff < ?TOLERANCE ->
                    Attrs = [{"formula", Val}],
                    ok = ?WRATTR([{Ref, Attrs}], Uid, Uid),
                    ok = status_srv:update_status(Uid, Ref, "edited page"),
                    hn_mochi:json(Env, "success");
                Diff >= ?TOLERANCE ->
                    hn_mochi:respond(403, Env)
            end;
        {"clear", [{#xrefX{}, Type2}]}
         when Type2 == "inline" orelse Type2 == "inlinerich" ->
                % Double check Val should be "contents"
                case Val of
                    "contents" ->
                        ok = status_srv:update_status(Uid, Ref, "edited page"),
                        ok = ?dbapi:clear(Ref, contents, Uid),
                        hn_mochi:json(Env, "success");
                    _ ->
                        hn_mochi:respond(403, Env)
                end;
         {"formula", [{#xrefX{}, {"select", Vs}}]} ->
                % escape the attributes to prevent script injection, etc
                Attrs = [{"formula", hn_util:esc(Val)}],
                [{"formula", V}] = Attrs,
                case lists:member(V, Vs) of
                    true ->
                        ok = ?WRATTR([{Ref, Attrs}], Uid, Uid),
                        ok = status_srv:update_status(Uid, Ref, "edited page"),
                        hn_mochi:json(Env, "success");
                false ->
                        hn_mochi:respond(403, Env)
            end;
        {"formula", [{#xrefX{}, {"dynamic_select", _Src, Vs}}]} ->
            % escape the attributes to prevent script injection, etc
            Attrs = [{"formula", hn_util:esc(Val)}],
            [{"formula", V}] = Attrs,
            case lists:member(V, Vs) of
                true ->
                    ok = ?WRATTR([{Ref, Attrs}], Uid, Uid),
                    hn_mochi:json(Env, "success");
                false ->
                    hn_mochi:respond(403, Env)
            end;
        _ ->
            hn_mochi:respond(403, Env)
    end;

% post forms are only on pages
post(Ref = #refX{site = S, path = P, obj = {page, "/"}} = Ref, _Qry,
     Env = #env{body = [{"postform", {struct, Vals}}], uid = PosterUid}) ->
    [{"results", ResPath}, {"values", {array, Array}}] = Vals,
    Transaction = common,
    Expected = ?dbapi:matching_forms(Ref, Transaction),
    case hn_security:validate_form(Expected, Array) of
        false ->
            error_logger:error_msg("invalid form submission~n""on:       ~p~n"
                                   ++ "Expected: ~p~nGot:      ~p~n",
                                   [Ref, Expected, Array]),
            hn_mochi:respond(403, Env);
        true  ->
            Path = string:tokens(hn_util:abs_path(P, ResPath), "/"),
            Res = Ref#refX{site = S, type = gurl, path = Path,
                           obj = {row, {1, 1}}},
            Array2 = [{R, [L, {"formula", hn_util:esc(V)}]}
                      || {R, [L, {_, V}]} <- Array],
            {ok, Poster} = passport:uid_to_email(PosterUid),
            ok = maybe_send_email(S, P, Expected, Array2, Poster),
            ok = ?dbapi:handle_form_post(Res, Array2, PosterUid),
            ok = status_srv:update_status(PosterUid, Ref, "edited page"),
            hn_mochi:json(Env, "success")
    end;

%% post of factory provisioning - special type of webcontrol
%% first up is a trial
post(#refX{obj = {cell, _}} = Ref, _Qry,
     #env{body = [{"postwebcontrols",
                   {struct, [{"signup", {struct, [{"sitetype", ST}]}}]}}],
          uid = PrevUID} = Env) ->
    SType2 = hn_util:site_type_exists(ST),
    Transaction = factory,
    Email = "dummy_" ++ PrevUID ++ "@vixo.com",
    [Expected] = ?dbapi:matching_forms(Ref, Transaction),
    case hn_security:validate_factory(Expected, SType2, []) of
        true  -> ok = status_srv:update_status(PrevUID, Ref, "commissioned trial site"),
                 hn_mochi:provision_site(Ref, PrevUID, SType2, Email,
                                         [], Env);
        false -> hn_mochi:json(Env, {struct, [{"result", "error"},
                                              {"reason", 401}]})
    end;
post(#refX{obj = {cell, _}} = Ref, _Qry,
     #env{uid = PrevUID,
          body = [{"postwebcontrols", {struct, [{"signup", Act}]}}]} = Env) ->
    {struct, List} = Act,
    {"sitetype", SiteType} = lists:keyfind("sitetype", 1, List),
    {"email", Email} = lists:keyfind("email", 1, List),
    {"data", {struct, Data}} = lists:keyfind("data", 1, List),
    SType2 = hn_util:site_type_exists(SiteType),
    Email2 = string:to_lower(Email),
    case hn_util:valid_email(Email) of
        false ->
            hn_mochi:json(Env, {struct, [{"result", "error"},
                                         {"reason", "invalid_email"}]});
        true ->
            Transaction = factory,
            [Expected] = ?dbapi:matching_forms(Ref, Transaction),
            case hn_security:validate_factory(Expected, SType2, Data) of
                true ->
                    spawn(hn_mochi, provision_site,
                          [Ref, PrevUID, SType2, Email2, Data, Env]),
                    ok = status_srv:update_status(PrevUID, Ref, "commissioned site"),
                    hn_mochi:json(Env, "success");
                false ->
                    hn_mochi:json(Env, {struct, [{"result", "error"},
                                                 {"reason", 401}]})
            end
    end;

% run a web control
post(Ref = #refX{obj = {cell, _}}, _Qry,
     Env = #env{body = [{"postwebcontrols", Actions}],
                uid = Uid}) ->
    {struct, Act} = Actions,
    Status = element(1, hd(Act)),
    {Mod, Fn} = new_db_api:get_callbackD(Ref),
    case Mod:Fn(Ref, Env, Act, Uid) of
        "success"              -> ok = status_srv:update_status(Uid, Ref, Status),
                                  hn_mochi:json(Env, "success");
        {"successresp", Resp1} -> ok = status_srv:update_status(Uid, Ref, Status),
                                  hn_mochi:json(Env, {struct, Resp1});
        {"error",       Error} -> hn_mochi:json(Env, {struct, [{"error", Error}]});
        {"errorresp",   Resp2} -> hn_mochi:json(Env, {struct, Resp2});
        {"failure",     R}     -> ?E("invalid invite_user request ~p~n", [R]),
                                  hn_mochi:json(Env, {struct, [{"failure", R}]});
        {"phone available", P} -> ok = status_srv:update_status(Uid, Ref, Status),
                                  hn_mochi:json(Env, {struct, [P]});
        401                    -> hn_mochi:respond(401, Env);
        403                    -> hn_mochi:respond(403, Env);
        404                    -> hn_mochi:respond(404, Env);
        500                    -> hn_mochi:'500'(Env);
        {redirect, R}          -> hn_mochi:json(Env, {struct, R});
        {ok, died}             -> ok % phone connection has timeout
    end;

% revert a cell to an old value
post(Ref = #refX{obj = {cell, _}} = Ref, _Qry,
     Env = #env{body = [{"revert_to", Revision}],
                uid = Uid}) ->
    case ?dbapi:revert(Ref, Revision, Uid) of
        {ok, ok} ->
            ok = status_srv:update_status(Uid, Ref, "Cell reverted from logs"),
            hn_mochi:json(Env, "success");
        _ ->
            hn_mochi:respond(500, Env)
    end;

%% changes the default view
post(Ref = #refX{site = S, path = P, obj = {O, _}}, _Qry,
     Env = #env{body = [{"default_view", {struct, [{"view", View}]}}],
                uid = Uid})
  when O == page ->
    ok = status_srv:update_status(Uid, Ref, "changed default view"),
    Args = [{"path", P}, {"view", View}],
    case hn_web_admin:rpc(Uid, S, "set_champion", Args) of
        ok         -> hn_mochi:json(Env, {struct, [{"result", "success"}]});
        {error, R} -> ?E("invalid change default view request ~p~n", [R]),
                      hn_mochi:json(Env, {struct, [{"failure", R}]})
    end;

%% sets the view
post(Ref = #refX{site = S, path = P, obj = {O, _}}, _Qry,
     Env = #env{body = [{"set_view", {struct, Args}}], uid = Uid})
  when O == page ->
    ok = status_srv:update_status(Uid, Ref, "set the view"),
    Args2 = [{"path", P} | Args],
    case hn_web_admin:rpc(Uid, S, "set_view", Args2) of
        ok         -> hn_mochi:json(Env, {struct, [{"result", "success"}]});
        {error, R} -> ?E("invalid set view request ~p~n", [R]),
                      hn_mochi:json(Env, {struct, [{"failure", R}]})
    end;

%% saves as template or add groups or usrs
post(Ref = #refX{site = S, path = P, obj = {O, _}}, _Qry,
     Env = #env{body = [{Action, {struct, Args}}], uid = Uid})
  when O == page andalso Action == "save_template";
       O == page andalso Action == "add_group";
       O == page andalso Action == "add_user";
       O == page andalso Action == "invite_user" ->
    Msg = case Action of
              "save_template" -> "save page as template";
              "add_group"     -> "add a group";
              "add_user"      -> "add a user";
              "invite_user"   -> "invite a user"
          end,
    ok = status_srv:update_status(Uid, Ref, Msg),
    Args2 = [{"path", P} | Args],
    case hn_web_admin:rpc(Uid, S, Action, Args2) of
        ok         -> hn_mochi:json(Env, {struct, [{"result", "success"}]});
        {error, R} -> ?E("invalid " ++ Msg ++ " request ~p~n", [R]),
                      hn_mochi:json(Env, {struct, [{"failure", R}]})
    end;

post(#refX{obj = {O, _} = Obj} = Ref, _Qry,
     #env{body = [{"set", {struct, Attr}}], uid = Uid} = Env)
  when O == cell orelse
       O == range ->
    case Attr of
        % TODO : Get Rid of this (for pasting a range of values)
        [{"formula",{array, Vals}}] ->
            case validate_array(Obj, Vals) of
                true  -> ok = status_srv:update_status(Uid, Ref, "edited page"),
                         post_range_values(Ref, Vals, Uid, Uid),
                         hn_mochi:json(Env, "success");
                false -> hn_mochi:'401'(Ref, Env)
            end;
        [{"formula", _Val}] ->
            ok = status_srv:update_status(Uid, Ref, "edited page"),
            ok = ?WRATTR([{Ref, Attr}], Uid, Uid),
            hn_mochi:json(Env, "success");
        [{"input", {struct, [{"select", {array, Array}}]}}] ->
            ok = status_srv:update_status(Uid, Ref, "edited page"),
            NewAttr = [{"input", {"select", Array}}],
            ok = ?WRATTR([{Ref, NewAttr}], Uid, Uid),
            hn_mochi:json(Env, "success");
        [{"input", {struct, [{"dynamic_select", Expression}]}}] ->
            ok = status_srv:update_status(Uid, Ref, "edited page"),
            NewAttr = [{"input", {"dynamic_select", Expression}}],
            ok = ?WRATTR([{Ref, NewAttr}], Uid, Uid),
            hn_mochi:json(Env, "success");
        [{"input", {struct, [{"inputincrement", Increment}]}}] ->
            case tconv:to_num(Increment) of
                {error, nan} ->
                    Resp = {struct, [
                                     {"result", "error"},
                                     {"reason", 401}
                                    ]},
                    hn_mochi:json(Env, Resp);
                NIncr ->
                    ok = status_srv:update_status(Uid, Ref, "edited page"),
                    NewAttr = [{"input", {"increment", NIncr}}],
                    ok = ?WRATTR([{Ref, NewAttr}], Uid, Uid),
                    hn_mochi:json(Env, "success")
            end;
        _Else ->
            ok = status_srv:update_status(Uid, Ref, "edited page"),
            ok = ?WRATTR([{Ref, Attr}], Uid, Uid),
            hn_mochi:json(Env, "success")
    end;

post(#refX{obj = {row, _}} = Ref, _Qry,
     #env{body = [{"set", {struct, [{Type, _}] = Attr}}],
         uid = Uid} = Env)
  when Type == "height" orelse
       Type == "fixedheight" ->
    ok = expand_height(Ref, Attr, Uid, Uid),
    hn_mochi:json(Env, "success");

post(#refX{obj = {column, _}} = Ref, _Qry,
     #env{body = [{"set", {struct, [{"width", _}] = Attr}}],
         uid = Uid} = Env) ->
    ok = expand_width(Ref, Attr, Uid, Uid),
    hn_mochi:json(Env, "success");

post(#refX{obj = {O, _}} = Ref, _Qry, #env{body = [{"set", _}]} = Env)
  when O == row    orelse
       O == column orelse
       O == page ->
    hn_mochi:'401'(Ref, Env);

post(Ref, _Qry, Env = #env{body = [{"set", {array, Array}}], uid = Uid})
  when Array =/= [] ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    List = [X || {struct, [X]} <- Array],
    ok = ?WRATTR([{Ref, List}], Uid, Uid),
    hn_mochi:json(Env, "success");

post(Ref, _Qry, Env = #env{body = [{"clear", What}], uid = Uid})
     when What == "contents" orelse
          What == "style"    orelse
          What == "all" ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    ok = ?dbapi:clear(Ref, list_to_atom(What), Uid),
    hn_mochi:json(Env, "success");

%% This clause is used to clear attributes
post(Ref, _Qry, Env = #env{body = [{"clear", What}], uid = Uid}) ->
    ok = status_srv:update_status(Uid, Ref, "edited page"),
    ok = ?dbapi:clear(Ref, {attributes, [What]}, Uid),
    hn_mochi:json(Env, "success");

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Horizonal API = notify_back_create handler                               %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% post(Ref, _Qry,
%%       Env = #env{body = [{"action", "notify_back_create"}|T]}) ->

%%     %% WARNING this assumes that the list is provided in strict order - should
%%     %% really sort the list before testing for "action"
%%     Biccie   = from("biccie",     T),
%%     Proxy    = from("proxy",      T),
%%     ChildUrl = from("child_url",  T),
%%     PVsJson  = from("parent_vsn", T),
%%     CVsJson  = from("child_vsn",  T),
%%     Stamp    = from("stamp",      T),

%%     #refX{site = Site} = Ref,
%%     ParentX = Ref,
%%     _ParentUrl = hn_util:refX_to_url(ParentX),
%%     ChildX = hn_util:url_to_refX(ChildUrl),

%%     %% there is only 1 parent and 1 child for this action
%%     PVsn = json_util:unjsonify(PVsJson),
%%     CVsn = json_util:unjsonify(CVsJson),
%%     %% #version{page = PP, version = PV} = PVsn,
%%     %% #version{page = CP, version = CV} = CVsn,
%%     Sync1 = ?dbapi:check_page_vsn(Site, PVsn),
%%     Sync2 =  ?dbapi:check_page_vsn(Site, CVsn),
%%     case Sync1 of
%%         synched         -> ok;
%%         unsynched       -> ?dbapi:resync(Site, PVsn);
%%         not_yet_synched -> ok % the child gets the version in this call...
%%     end,
%%     case Sync2 of
%%         synched         -> ok;
%%         unsynched       -> ?dbapi:resync(Site, CVsn);
%%         not_yet_synched -> sync_exit()
%%     end,
%%     {struct, Return} = ?dbapi:register_hn_from_web(ParentX, ChildX,
%%                                                       Proxy, Biccie),
%%     Return2 = lists:append([Return, [{"stamp", Stamp}]]),
%% hn_mochi:json(Env, {struct, Return2});

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Horizonal API = notify_back handler                                      %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% post(Ref, _Qry,
%%       Env = #env{body = [{"action", "notify_back"} |T] = _Json}) ->
%%     Biccie    = from("biccie",     T),
%%     ChildUrl  = from("child_url",  T),
%%     ParentUrl = from("parent_url", T),
%%     Type      = from("type",       T),
%%     PVsJson   = from("parent_vsn", T),
%%     CVsJson   = from("child_vsn",  T),
%%     Stamp     = from("stamp",      T),

%%     %% there is only 1 parent and 1 child here
%%     PVsn = json_util:unjsonify(PVsJson),
%%     CVsn = json_util:unjsonify(CVsJson),
%%     %% #version{page = PP, version = PV} = PVsn,
%%     %% #version{page = CP, version = CV} = CVsn,
%%     ChildX = hn_util:url_to_refX(ChildUrl),
%%     ParentX = hn_util:url_to_refX(ParentUrl),
%%     #refX{site = Site} = Ref,
%%     Sync1 = ?dbapi:check_page_vsn(Site, CVsn),
%%     Sync2 = ?dbapi:check_page_vsn(Site, PVsn),
%%     case Sync1 of
%%         synched ->
%%             ok = ?dbapi:notify_back_from_web(ParentX, ChildX,
%%                                                 Biccie, Type);
%%         unsynched ->
%%             ?dbapi:resync(Site, PVsn);
%%         not_yet_synched ->
%%             ok = ?dbapi:initialise_remote_page_vsn(Site, PVsn)
%%     end,
%%     case Sync2 of
%%         synched -> ok;
%%         unsynched ->
%%             ok = ?dbapi:resync(Site, CVsn);
%%         not_yet_synched ->
%%             ok = ?dbapi:initialise_remote_page_vsn(Site, CVsn)
%%     end,
%%     S = {struct, [{"result", "success"}, {"stamp", Stamp}]},
%%     hn_mochi:json(Env, S);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                          %%%
%%% Horizonal API = notify handler                                           %%%
%%%                                                                          %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% post(Ref, _Qry,
%%       Env = #env{body = [{"action", "notify"} | T] = _Json}) ->
%%     Biccie    = from("biccie",     T),
%%     ParentUrl = from("parent_url", T),
%%     Type      = from("type",       T),
%%     Payload   = from("payload",    T),
%%     PVsJson   = from("parent_vsn", T),
%%     CVsJson   = from("child_vsn",  T),
%%     Stamp     = from("stamp",      T),

%%     ParentX = hn_util:url_to_refX(ParentUrl),
%%     ChildX = Ref,
%%     _ChildUrl = hn_util:refX_to_url(ChildX),

%%     #refX{site = Site} = ChildX,
%%     PVsn = json_util:unjsonify(PVsJson),
%%     CVsn = json_util:unjsonify(CVsJson),
%%     %%#version{page = PP, version = PV} = PVsn,

%%     Sync1 = case Type of
%%                 "insert"    -> ?dbapi:incr_remote_page_vsn(Site, PVsn, Payload);
%%                 "delete"    -> ?dbapi:incr_remote_page_vsn(Site, PVsn, Payload);
%%                 "new_value" -> ?dbapi:check_page_vsn(Site, PVsn)
%%             end,
%%     %% there is one parent and it if is out of synch, then don't process it, ask for a
%%     %% resynch
%%     case Sync1 of
%%         synched ->
%%             ok = ?dbapi:notify_from_web(ParentX, Ref, Type,
%%                                            Payload, Biccie);
%%         unsynched ->
%%             ok = ?dbapi:resync(Site, PVsn);
%%         not_yet_synched ->
%%             sync_exit()
%%     end,
%%     %% there are 1 to many children and if they are out of synch ask for
%%     %% a resynch for each of them
%%     Fun =
%%         fun(X) ->
%%                 Sync2 = ?dbapi:check_page_vsn(Site, X),
%%                 %% #version{page = CP, version = CV} = X,
%%                 case Sync2 of
%%                     synched         -> ok;
%%                     unsynched       -> ok = ?dbapi:resync(Site, X);
%%                     not_yet_synched -> sync_exit()
%%                 end
%%         end,
%%     [Fun(X) || X <- CVsn],
%%     S = {struct, [{"result", "success"}, {"stamp", Stamp}]},
%%     hn_mochi:json(Env, S);

post(#refX{site = Site}, _Qry,
     Env = #env{body = [{"admin", Json}], uid = Uid}) ->
    {struct,[{Fun, {struct, Args}}]} = Json,
    case hn_web_admin:rpc(Uid, Site, Fun, Args) of
        ok         -> hn_mochi:json(Env, {struct, [{"result", "success"}]});
        {error, R} -> ?E("invalid _admin request ~p~n", [R]),
                      hn_mochi:json(Env, {struct, [{"failure", R}]})
    end;

post(#refX{site = S, path = P, obj = {page, "/"}} = RefX, _Qry,
     Env = #env{body = [{"append", Data}], uid = Uid}) ->
    {struct, [{"values", {array, Array}}]} = Data,
                Array2 = [{R, [L, {"formula", hn_util:esc(V)}]}
                          || {R, [L, {_, V}]} <- Array],
    Res = #refX{site = S, path = P, type = gurl, obj = {row,{1, 1}}},
    ok = ?dbapi:handle_form_post(Res, Array2, Uid),
    ok = status_srv:update_status(Uid, RefX, "edited page"),
    hn_mochi:json(Env, "success");

%% post(#refX{site = Site, path = _P} = Ref, _Qry,
%%      Env = #env{body = [{"read_user_fn", Entry}], uid = Uid}) ->
%%     {struct, Args} = Entry,
%%     case hn_web_admin:rpc(Uid, Site, "read_user_fn", Args) of
%%         {ok, Return}	  -> ok = status_srv:update_status(Uid, Ref, "read user fn"),
%%                            hn_mochi:json(Env, Return);
%%         {error, Reason} -> ?E("invalid curie request ~p~n", [Reason]),
%%                            hn_mochi:json(Env, {struct, [{"failure", Reason}]})
%%     end;

%% post(#refX{site = Site, path = _P} = Ref, _Qry,
%%      Env = #env{body = [{"delete_user_fn", Entry}], uid = Uid}) ->
%%     {struct, Args} = Entry,
%%     case hn_web_admin:rpc(Uid, Site, "delete_user_fn", Args) of
%%         {ok, Return}	  -> ok = status_srv:update_status(Uid, Ref, "delete user fn"),
%%                            hn_mochi:json(Env, Return);
%%         {error, Reason} -> ?E("invalid curie request ~p~n", [Reason]),
%%                            hn_mochi:json(Env, {struct, [{"failure", Reason}]})
%%     end;

%% post(#refX{site = Site, path = _P} = Ref, _Qry,
%%      Env = #env{body = [{"write_user_fn", Entry}], uid = Uid}) ->
%%     {struct, Args} = Entry,
%%     case hn_web_admin:rpc(Uid, Site, "write_user_fn", Args) of
%%         {ok, Return}	  -> ok = status_srv:update_status(Uid, Ref, "write user ffn"),
%%                            hn_mochi:json(Env, Return);
%%         {error, Reason} -> ?E("invalid curie request ~p~n", [Reason]),
%%                            hn_mochi:json(Env, {struct, [{"failure", Reason}]})
%%     end;

%~ post(_Ref, _Qry, Env = #env{body= [{"type", "user_defined_read"} | _T] = Json_Entry}) ->
%~ Return = curie:read_user_fn(Json_Entry),
%~ hn_mochi:json(Env, Return);
%~
%~
%~ post(_Ref, _Qry, Env = #env{body= [{"type", "user_defined_delete"} | _T] = Json_Entry}) ->
%~ Return = curie:delete_user_fn(Json_Entry),
%~ hn_mochi:json(Env, Return);

post(Ref, Qry, Env) ->
    error_logger:error_msg("401~n-~p~n-~p~n",[Ref, Qry]),
    hn_mochi:'401'(Ref, Env).

%%------------------------------------------------------------------------------
%%
%% Internal Fns
%%
%%------------------------------------------------------------------------------
passport_running() ->
    {ok, Services} = application:get_env(hypernumbers, services),
    case lists:keysearch(passport, 1, Services) of
        {value, {passport, false}} -> false;
        {value, {passport, true}}  -> true
    end.

request_pwd_reset(Email, Site) ->
    case hn_util:valid_email(Email) of
        false ->
            ?E("invalid e-mail in forgotten password ~p~n",
               [Email]),
            {"failure", "you supplied an invalid e-mail"};
        true ->
            case passport:is_user(Email) of
                false -> {"failure", Email ++ " is not a user"};
                true  -> ok = passport:issue_pwd_reset(Email, Site),
                         {"success", "Password reset sent. "
                          ++ "Please check your e-mail"}
            end
    end.

reset_password(Email, Password, Hash) ->
    case hn_util:valid_email(Email) of
        false ->
            ?E("invalid e-mail in forgotten password ~p~n",
               [Email]),
            {"failure", "you supplied an invalid e-mail"};
        true ->
            case passport:reset_pwd(Email, Password, Hash) of
                {success, Success} ->
                    HRef = "Password reset - <a href=\"" ++ Success
                        ++ "\">continue</a>",
                    {"success", HRef};
                {error, weak_password} ->
                    {"failure", "that password was too weak, try again."};
                {error, invalid_email} ->
                    {"failure", "invalid e-mail address"};
                {error, reset_not_issued} -> % this is the one
                    {"failure", "reset not issued"};
                {error, invalid_reset} ->
                    ?E("invalid reset attempt~nemail: ~p~nhash:~p~n",
                       [{email, Email}, {hash, Hash}]),
                    {"failure", "invalid attempt at resetting"};
                {error, expired_reset} ->
                    {ok, URL} = application:get_env(hypernumbers, reset_url),
                    Msg = "<a href=\"" ++ URL ++"\">try again</a>",
                    {"failure", "that reset has expired, "
                     ++ Msg}
            end
    end.

make_after(#refX{obj = {cell, {X, Y}}} = RefX) ->
    RefX#refX{obj = {cell, {X - 1, Y - 1}}};
make_after(#refX{obj = {range, {X1, Y1, X2, Y2}}} = RefX) ->
    DiffX = X2 - X1 - 1,
    DiffY = Y2 - Y1 - 1,
    RefX#refX{obj = {range, {X1 - DiffX, Y1 - DiffY, X2 - DiffX, Y2 - DiffY}}};
make_after(#refX{obj = {column, {X1, X2}}} = RefX) ->
    DiffX = X2 - X1 + 1,
    RefX#refX{obj = {column, {X1 + DiffX, X2 + DiffX}}};
make_after(#refX{obj = {row, {Y1, Y2}}} = RefX) ->
    DiffY = Y2 - Y1 + 1,
    RefX#refX{obj = {row, {Y1 + DiffY, Y2 + DiffY}}}. %

%% from(Key, List) ->
%%     {value, {Key, Value}} = lists:keysearch(Key, 1, List),
%%     Value.

maybe_send_email(Site, Path, Expected, Array, Poster) ->
    case get_email(Expected) of
        false -> ok;
        Email -> [_, "//" ++ S2, _] = string:tokens(Site, ":"),
                 From = "no-reply@" ++ S2,
                 Subject = "Form on page " ++ hn_util:list_to_path(Path)
                     ++ " has been submitted",
                 Contents = make_contents(Poster, Path, Array),
                 ok = emailer:send_email(Email, "", From, Subject, Contents)
    end.

validate_array({range, {X1, Y1, X2, Y2}}, Vals) ->
    Height = Y2 - Y1 + 1,
    Width  = X2 - X1 + 1,
    case length(Vals) of
        Height -> validate_a2(Vals, Width);
        _     -> false
    end;
validate_array(_, _) ->
    false.

validate_a2([], _) -> true;
validate_a2([{array, Row} | T], Width) ->
    case length(Row) of
        Width -> validate_a2(T, Width);
        _      -> false
    end.

post_range_values(Ref, Values, PAr, VAr) ->
     F = fun({array, Vals}, Acc) ->
                 post_row_values(Ref, Vals, PAr, VAr, Acc), Acc+1
         end,
     lists:foldl(F, 0, Values).

post_row_values(Ref, Values, PAr, VAr, Offset) ->
    #refX{obj = {range,{X1, Y1, _X2, _Y2}}} = Ref,
    F = fun(Val, Acc) ->
                % if you paste in a range with blank cells from Excel you
                % don't want values of "" stuck in because they count as
                % not blank
                % with countblank() so we skip them
                % we don't do this in cell formulae though...
                NRef = Ref#refX{type = url,
                                obj = {cell, {X1 + Acc, Y1 + Offset}}},
                case Val of
                    "" -> ok = ?dbapi:clear(NRef, contents, PAr),
                          Acc + 1;
                    _  -> ok = ?WRATTR([{NRef,
                                               [{"formula", Val}]}],
                                             PAr, VAr),
                          Acc + 1
                end
        end,
    lists:foldl(F, 0, Values).

expand_height(#refX{obj = {row, {Y1, Y1}}} = Ref, Attr, PAr, VAr) ->
    ok = new_db_api:write_attributes([{Ref, Attr}], PAr, VAr);
expand_height(#refX{obj = {row, {Y1, Y2}}} = Ref, Attr, PAr, VAr) ->
    NewRef = Ref#refX{obj = {row, {Y1, Y1}}},
    ok = new_db_api:write_attributes([{NewRef, Attr}], PAr, VAr),
    NewRef2 = Ref#refX{obj = {row, {Y1 + 1, Y2}}},
    expand_height(NewRef2, Attr, PAr, VAr).

expand_width(#refX{obj = {column, {X1, X1}}} = Ref, Attr, PAr, VAr) ->
    ok = new_db_api:write_attributes([{Ref, Attr}], PAr, VAr);
expand_width(#refX{obj = {column, {X1, X2}}} = Ref, Attr, PAr, VAr) ->
    NewRef = Ref#refX{obj = {column, {X1, X1}}},
    ok = new_db_api:write_attributes([{NewRef, Attr}], PAr, VAr),
    NewRef2 = Ref#refX{obj = {column, {X1 + 1, X2}}},
    expand_width(NewRef2, Attr, PAr, VAr).

get_email([]) ->
    false;
get_email([{_, _, _, button, none, Attrs} | T]) ->
    case lists:keyfind("email", 1, Attrs) of
        false            -> get_email(T);
        {"email", Email} -> Email
    end;
get_email([_H | T]) ->
    get_email(T).

make_contents(Poster, Page, Array) ->
    Line1 = "User: " ++ Poster ++ " posted to the form on page "
        ++ Page ++ "\n",
    Lines = get_lines(Array, []),
    Line1 ++ Lines.

get_lines([], Acc) -> string:join(lists:reverse(Acc), "\n");
get_lines([{struct, [{"label", L}, {"formula", F}]} | T], Acc) ->
    Line = io_lib:format("~s - ~s", [L, F]),
    get_lines(T, [Line | Acc]).

copy(Ref, Source, Style, Uid, Env) ->
    SrcRef = hn_util:url_to_refX(Source),
    case SrcRef of
        #refX{obj = {O, _}} when O == cell orelse O == range ->
            ok = ?dbapi:copy_n_paste(SrcRef, Ref, Style, Uid),
            hn_mochi:json(Env, "success");
        _ ->
            hn_mochi:'401'(Ref, Env)
    end.
