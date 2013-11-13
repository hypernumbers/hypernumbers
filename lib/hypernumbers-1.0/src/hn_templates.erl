%%%-------------------------------------------------------------------
%%% File        hn_templates.erl
%%% @author     Gordon Guthrie <gordon@hypernumbers.com>
%%% @doc        This module deals with templates
%%% @copyright  Hypernumbers Ltd
%%% Created     30 Aug 2008 by Gordon Guthrie
%%% @TODO       proper documentation and specifications
%%%             This function has to be totally rewritten for the new
%%%             database
%%%-------------------------------------------------------------------

-module(hn_templates).

-export([
         save_template/3,
         load_template/3,
         load_template/2,
         load_template_if_no_page/2
         ]).

%% -import(format_util,
%%         [clock_12/1,
%%          pad_year/1,
%%          pad_calendar/1,
%%          get_short_day/1,
%%          get_day/1,
%%          get_short_month/1,
%%          get_month/1,
%%          get_last_two/1,
%%          get_dayname/1,
%%          get_short_dayname/1
%%         ]).

-include("spriki.hrl").
-include("hn_mochi.hrl").

%%
%% External API
%%

save_template(#refX{site = S} = RefX, Name, UID)  ->
    TemplatesDir = hn_util:templateroot(S),
    Encoder = mochijson:encoder([{input_encoding, utf8}]),
    FileName = filename:join(TemplatesDir, Name++".json"),
    Date = dh_date:format(".Y.m.d.H.i.s") ++ ".json" ,
    BackupName = filename:join(TemplatesDir ++ "/backups", Name ++ Date),
    % if this is the first save of the template don't try and take a backup
    case filelib:is_file(FileName) of
        true  -> ok = filelib:ensure_dir(BackupName),
                 {ok, _} = file:copy(FileName, BackupName);
        false -> ok
    end,
    Page = Encoder(hn_mochi:page_attrs_for_export(RefX, #env{uid = UID})),
    Data = io_lib:format("~s", [lists:flatten(Page)]),
    ok = filelib:ensure_dir(FileName),
    ok = file:write_file(FileName, Data),
    ok = remoting_reg:notify_site(S).

% race condition not managed by a db transaction here
load_template_if_no_page(#refX{} = RefX, Name) ->
    case new_db_api:does_page_exist(RefX) of
        false -> load_template(RefX, Name);
        true  -> io:format("Not loading ~p on ~p~n",
                           [Name, hn_util:refX_to_url(RefX)]),
                 ok
    end.

load_template(RefX, Name) -> load_template(RefX, Name, nil).

load_template(#refX{site = S, path = P}, Name, Uid) ->
    TemplatesDir = hn_util:templateroot(S),
    URL = S ++ hn_util:list_to_path(P),
    File = TemplatesDir++"/"++Name++".json",
    ok = hn_import:json_file(URL, File, Uid).
