%%% @author    Gordon Guthrie
%%% @copyright (C) 2011, Sustainable Advantage Ltd
%%% @doc       This is the upload code for Sustainable Avantage
%%%
%%% @end
%%% Created :  5 Mar 2011 by gordon@hypernumbers

-module(sust_adv_upload).

-export([
        imserv_type_1/2
       ]).

imserv_type_1(File, Date) ->
    %% check if the date is valid
    Date2 = dh_date:parse(Date),
    io:format("Date2 is ~p~n", [Date2]),
    case filelib:is_file(File) of
        true -> Contents = parse_csv:parse_file(File),
                io:format("Contents is ~p~n", [Contents]),
                ok;
        false -> {error, file_doesnt_exist}
    end.
    
