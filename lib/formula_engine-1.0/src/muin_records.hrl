%%%----------------------------------------------------------------------------
%%% @doc Various records used in Muin.
%%% @author Hasan Veldstra <hasan@hypernumbers.com>
%%%----------------------------------------------------------------------------

%% DateTime record.
%% Date and time fields are in the format expected by functions in the
%% calendar module.
-record(datetime, {date = {0, 0, 0},   % year, month, day
                   time = {0, 0, 0}}). % hour, minute, second
